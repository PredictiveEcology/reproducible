## URL access logging for prepInputs / preProcess.
##
## The option `reproducible.urlLog` selects a sink:
##   NULL / FALSE       -> off (default)
##   <environment>      -> the caller (e.g. SpaDES.core simInitAndSpades) owns
##                         the env and its lifecycle. Records appended to
##                         `env$records`; idempotency via `env$seen`. Caller
##                         decides what to do with the contents on exit
##                         (flush to CSV, attach to a sim object, discard...).
##   TRUE               -> in-memory session log, retrievable via getUrlLog();
##                         per-session dedup.
##   function(record)   -> callback invoked with each record list (no dedup).
##
## Independent of the sink, when a prepInputs/preProcess access flows through
## Cache(), the cacheId is tagged in the cache DB with reproducible.url* tags
## so the URL provenance lives with the cached object.
##
## How `Cache(...prepInputs(url=...)...)` is handled (incl. Cache(Map(...))):
##   - On Cache entry, a transient "url frame" is allocated. prepInputs and
##     preProcess function-head hooks push (fn, url) onto every currently-open
##     url frame. Cache, on save, drains its frame, writes session records
##     with the cacheId, and tags the cacheId. On.exit clears the frame so
##     interrupted evaluations don't leak state.
##   - On Cache hit, no inner code runs. Cache reads existing reproducible.url
##     tags from the DB and emits replay records (cacheHit = TRUE).
##   - Matched-call extraction is kept as a fallback for direct
##     Cache(prepInputs(url="...")) shapes where the inner function may not
##     have been instrumented (e.g. local masks in tests).
##
## Idempotency key is (fn, url, cacheId). Within a scope (env, or session in
## TRUE mode), each (fn,url,cacheId) triple produces one record. The cache-DB
## tags carry their own hitCount counter independently.

.urlLogEnv <- new.env(parent = emptyenv())
.urlLogEnv$records      <- list()
.urlLogEnv$seen         <- character()
.urlLogEnv$frames       <- list()
.urlLogEnv$frameCounter <- 0L

#' URL access log for `prepInputs` / `preProcess`
#'
#' Controlled by `getOption("reproducible.urlLog")`. See the package option
#' documentation for sink types. `getUrlLog()` returns the in-memory records
#' written under the `TRUE` sink mode; `clearUrlLog()` empties them. Records
#' written to an environment sink (the typical SpaDES use case) live on that
#' environment and are not retrievable through these accessors.
#'
#' @return `getUrlLog()` returns a list of record lists. `clearUrlLog()` returns
#'   `NULL` invisibly.
#'
#' @rdname urlLog
#' @export
getUrlLog <- function() .urlLogEnv$records

#' @rdname urlLog
#' @export
clearUrlLog <- function() {
  .urlLogEnv$records <- list()
  .urlLogEnv$seen    <- character()
  invisible()
}

## Build the (fn, url, cacheId) idempotency key. Field separator is \037 (US),
## which never appears in URLs / function names / cacheIds.
.urlLogKey <- function(fn, url, cacheId) {
  paste(fn,
        paste(url, collapse = "\036"),
        if (is.null(cacheId) || !length(cacheId)) "NA" else cacheId,
        sep = "\037")
}

## Build the record list written to the sink.
.urlLogRecord <- function(fn, url, destinationPath, cacheId, cacheHit, via) {
  list(
    time            = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3"),
    fn              = fn,
    url             = as.character(url),
    destinationPath = if (is.null(destinationPath)) NA_character_
                      else as.character(destinationPath),
    cacheId         = if (is.null(cacheId)) NA_character_
                      else as.character(cacheId),
    cacheHit        = cacheHit,
    via             = via
  )
}

## Write one record to whichever sink is active. Applies idempotency.
## Internal helper -- callers must already have validated the sink/url.
.writeSessionRecord <- function(rec) {
  sink <- getOption("reproducible.urlLog", NULL)
  key <- .urlLogKey(rec$fn, rec$url, rec$cacheId)
  if (is.environment(sink)) {
    if (is.null(sink$seen))    sink$seen    <- character()
    if (is.null(sink$records)) sink$records <- list()
    if (!key %in% sink$seen) {
      sink$seen <- c(sink$seen, key)
      sink$records[[length(sink$records) + 1L]] <- rec
    }
  } else if (isTRUE(sink)) {
    if (!key %in% .urlLogEnv$seen) {
      .urlLogEnv$seen <- c(.urlLogEnv$seen, key)
      .urlLogEnv$records[[length(.urlLogEnv$records) + 1L]] <- rec
    }
  } else if (is.function(sink)) {
    try(sink(rec), silent = TRUE)
  }
  invisible()
}

## Dispatcher used by prepInputs/preProcess function-head hooks.
##
## If any Cache url-frames are currently open, the (fn, url) record is pushed
## onto each of them (so the outer Cache(s) will emit/tag the URL on save).
## Otherwise the record is written to the session sink directly with cacheId
## = NA. NULL or empty url is ignored.
.logUrlAccess <- function(fn, url, destinationPath = NULL,
                          cacheId = NA_character_, cacheHit = NA,
                          via = NA_character_) {
  sink <- getOption("reproducible.urlLog", NULL)
  if (is.null(sink) || isFALSE(sink)) return(invisible())
  if (is.null(url)) return(invisible())
  if (is.character(url) && !length(url)) return(invisible())

  ## When prepInputs invokes preProcess, both heads would otherwise log the
  ## same access. Suppress the inner preProcess record.
  if (identical(fn, "preProcess") && .calledFromPrepInputs())
    return(invisible())

  ## Inside a Cache wrapper: defer to Cache by pushing to its frame(s).
  ## Cache will write session records (with the cacheId) and tag the cacheId.
  if (length(.urlLogEnv$frames) > 0L &&
      (is.null(cacheId) || any(is.na(cacheId)))) {
    push <- list(fn = fn, url = url, destinationPath = destinationPath)
    for (id in names(.urlLogEnv$frames)) {
      n <- length(.urlLogEnv$frames[[id]]) + 1L
      .urlLogEnv$frames[[id]][[n]] <- push
    }
    return(invisible())
  }

  ## Bare call (no Cache around): write directly with cacheId = NA.
  rec <- .urlLogRecord(fn, url, destinationPath,
                       cacheId = cacheId, cacheHit = cacheHit,
                       via = if (is.na(via)) fn else via)
  .writeSessionRecord(rec)
}

## TRUE when prepInputs is on the call stack above the current frame.
.calledFromPrepInputs <- function() {
  calls <- sys.calls()
  if (length(calls) < 2L) return(FALSE)
  for (i in seq_len(length(calls) - 1L)) {
    cl <- calls[[i]]
    if (is.call(cl)) {
      nm <- tryCatch(deparse(cl[[1]], nlines = 1L), error = function(e) "")
      if (identical(nm, "prepInputs") ||
          identical(nm, "reproducible::prepInputs"))
        return(TRUE)
    }
  }
  FALSE
}

## ---- Cache url-frame lifecycle -------------------------------------------

## Open a transient slot for one Cache call. Returns the frame id (or NULL if
## logging is off). Caller must arrange for .closeCacheUrlFrame() to fire on
## exit so interrupted Cache calls don't leak slots.
.openCacheUrlFrame <- function() {
  sink <- getOption("reproducible.urlLog", NULL)
  if (is.null(sink) || isFALSE(sink)) return(NULL)
  .urlLogEnv$frameCounter <- .urlLogEnv$frameCounter + 1L
  id <- paste0("f", .urlLogEnv$frameCounter)
  .urlLogEnv$frames[[id]] <- list()
  id
}

.closeCacheUrlFrame <- function(id) {
  if (is.null(id)) return(invisible())
  .urlLogEnv$frames[[id]] <- NULL
  invisible()
}

.takeCacheUrlFrame <- function(id) {
  if (is.null(id)) return(list())
  recs <- .urlLogEnv$frames[[id]]
  if (is.null(recs)) list() else recs
}

## ---- Matched-call extraction (fallback for direct Cache(prepInputs(...))) -

## Walk a call expression for the first inner call whose function name matches.
## Handles bare names and `pkg::name`. Returns list(name, call) or NULL.
##
## Does NOT descend into `function(...)` literals: symbols inside an anonymous
## function's body refer to its formal args, not the calling env, so a hit
## there can't be resolved meaningfully (e.g. `function(url) prepInputs(url=url)`
## binds `url` to the formal, not to a string we can record).
.findCallByName <- function(expr, names) {
  if (is.call(expr)) {
    head <- expr[[1]]
    if (identical(head, as.name("function"))) return(NULL)
    nm <- if (is.name(head)) {
      as.character(head)
    } else if (is.call(head) && length(head) == 3L &&
               identical(head[[1]], as.name("::"))) {
      as.character(head[[3]])
    } else {
      NULL
    }
    if (!is.null(nm) && nm %in% names)
      return(list(name = nm, call = expr))
    for (i in seq_along(expr)) {
      hit <- .findCallByName(expr[[i]], names)
      if (!is.null(hit)) return(hit)
    }
  }
  NULL
}

## Pull the url arg + function name out of a matched Cache() call. Returns
## list(fn, url) or NULL when this isn't a prepInputs/preProcess call.
.urlInfoFromMatchedCall <- function(matchedCall, functionName,
                                    .callingEnv = parent.frame()) {
  fn <- functionName
  mc <- matchedCall
  if (!isTRUE(fn %in% c("prepInputs", "preProcess"))) {
    hit <- .findCallByName(mc, c("prepInputs", "preProcess"))
    if (is.null(hit)) return(NULL)
    fn <- hit$name
    mc <- hit$call
  }
  urlExpr <- mc$url
  if (is.null(urlExpr)) return(NULL)
  url <- tryCatch(eval(urlExpr, envir = .callingEnv), error = function(e) NULL)
  if (is.null(url) || !length(url)) return(NULL)
  if (!is.atomic(url)) return(NULL)
  url <- tryCatch(as.character(url), error = function(e) NULL)
  if (is.null(url) || !length(url) || all(is.na(url) | !nzchar(url))) return(NULL)
  list(fn = fn, url = url)
}

## ---- Cache-DB tag helpers ------------------------------------------------
##
## All three readers route through showCacheFast(cacheId = ...), which already
## handles both the DBI and per-cacheId file backends. Cached the table once
## per call to .maybeRecordUrlForCache via .urlTagsTable() so we don't reload
## three times when both .urlTagsForCacheId and .urlTagValue are needed.

.urlTagsTable <- function(cacheId, cachePath, drv, conn) {
  tryCatch(
    showCacheFast(cacheId = cacheId, cachePath = cachePath,
                  strict = FALSE, drv = drv, conn = conn, verbose = 0),
    error = function(e) NULL)
}

.urlTagsForCacheId <- function(cacheId, cachePath, drv, conn,
                               .tab = .urlTagsTable(cacheId, cachePath, drv, conn)) {
  if (is.null(.tab) || NROW(.tab) == 0L) return(character(0))
  unique(.tab$tagKey[grepl("^reproducible\\.url", .tab$tagKey)])
}

.urlTagValue <- function(cacheId, tagKey, cachePath, drv, conn,
                         .tab = .urlTagsTable(cacheId, cachePath, drv, conn)) {
  if (is.null(.tab) || NROW(.tab) == 0L) return(NA_character_)
  v <- .tab$tagValue[.tab$tagKey == tagKey]
  if (length(v) == 0L) NA_character_ else v[1L]
}

## Read all url/urlFn tag values for replay on cache hit. Returns
## list(urls = character, fn = character(1) or NA).
.readUrlTagsForCacheId <- function(cacheId, cachePath, drv, conn,
                                   .tab = .urlTagsTable(cacheId, cachePath, drv, conn)) {
  empty <- list(urls = character(0), fn = NA_character_)
  if (is.null(.tab) || NROW(.tab) == 0L) return(empty)
  urls <- .tab$tagValue[.tab$tagKey == "reproducible.url"]
  fn   <- .tab$tagValue[.tab$tagKey == "reproducible.urlFn"][1L]
  list(urls = if (length(urls)) as.character(urls) else character(0),
       fn   = if (length(fn) == 0L || is.na(fn)) NA_character_ else as.character(fn))
}

## Persistent provenance tags for a cacheId.
## isHit == FALSE -> first write: url/urlFn/firstSeen/lastSeen=now/hitCount=0
## isHit == TRUE  -> bump lastSeen + hitCount; back-fill url/urlFn/firstSeen
##                   if a pre-existing entry has none.
.persistUrlTags <- function(cacheId, fn, url, cachePath, drv, conn, isHit) {
  if (is.null(cacheId) || !nzchar(cacheId)) return(invisible())
  if (is.null(url) || !length(url)) return(invisible())
  if (is.null(cachePath) || !nzchar(cachePath)) return(invisible())

  now <- as.character(Sys.time())
  tab <- .urlTagsTable(cacheId, cachePath, drv, conn)
  existing <- .urlTagsForCacheId(cacheId, cachePath, drv, conn, .tab = tab)
  hasUrl       <- "reproducible.url"          %in% existing
  hasFn        <- "reproducible.urlFn"        %in% existing
  hasFirstSeen <- "reproducible.urlFirstSeen" %in% existing
  hasLastSeen  <- "reproducible.urlLastSeen"  %in% existing
  hasCount     <- "reproducible.urlHitCount"  %in% existing

  addOne <- function(key, value) {
    try(.addTagsRepo(cacheId = cacheId, cachePath = cachePath,
                     tagKey = key, tagValue = as.character(value),
                     drv = drv, conn = conn), silent = TRUE)
  }
  updateOne <- function(key, value) {
    try(.updateTagsRepo(cacheId = cacheId, cachePath = cachePath,
                        tagKey = key, tagValue = as.character(value),
                        add = TRUE, drv = drv, conn = conn), silent = TRUE)
  }

  if (!hasUrl) for (u in url) addOne("reproducible.url", u)
  if (!hasFn)        addOne("reproducible.urlFn", fn)
  if (!hasFirstSeen) addOne("reproducible.urlFirstSeen", now)
  if (hasLastSeen)   updateOne("reproducible.urlLastSeen", now) else addOne("reproducible.urlLastSeen", now)

  newCount <- if (isHit) {
    cur <- if (hasCount) {
      suppressWarnings(as.integer(.urlTagValue(cacheId, "reproducible.urlHitCount",
                                               cachePath, drv, conn, .tab = tab)))
    } else 0L
    if (is.na(cur)) 1L else cur + 1L
  } else {
    0L
  }
  if (hasCount) updateOne("reproducible.urlHitCount", newCount) else addOne("reproducible.urlHitCount", newCount)
  invisible()
}

## Single dispatcher called from Cache() at hit-return and post-save branches.
##
## On miss (isHit=FALSE): drain this Cache's url frame (pushed by inner
## prepInputs/preProcess), union with matched-call extraction, write one
## session record per (fn,url) with the cacheId, and tag the cacheId.
##
## On hit (isHit=TRUE): read existing reproducible.url* tags from the cache
## DB and replay one session record per url (cacheHit=TRUE). Falls back to
## matched-call extraction if the entry pre-dates this feature.
.maybeRecordUrlForCache <- function(callList, keyFull, cachePaths, drv, conn,
                                    isHit, .callingEnv = parent.frame(),
                                    urlFrameId = NULL) {
  sink <- getOption("reproducible.urlLog", NULL)
  if (is.null(sink) || isFALSE(sink)) return(invisible())

  cacheId <- keyFull$key
  cachePath <- if (length(cachePaths)) cachePaths[[1]] else
    getOption("reproducible.cachePath")

  if (!isHit) {
    framed <- .takeCacheUrlFrame(urlFrameId)
    info   <- .urlInfoFromMatchedCall(callList$new_call,
                                      callList$.functionName,
                                      .callingEnv = .callingEnv)
    if (length(framed) == 0L && is.null(info)) return(invisible())

    ## Collapse framed + matched-call into one (fn, url, destinationPath) list,
    ## deduped on url (a url shouldn't get two records for one cache miss).
    seenUrl <- character(0)
    items <- list()
    addItem <- function(fn, url, dest) {
      for (u in url) {
        if (u %in% seenUrl) next
        seenUrl <<- c(seenUrl, u)
        items[[length(items) + 1L]] <<- list(fn = fn, url = u,
                                             destinationPath = dest)
      }
    }
    for (rec in framed) addItem(rec$fn, rec$url, rec$destinationPath)
    if (!is.null(info)) addItem(info$fn, info$url, NULL)

    for (it in items) {
      rec <- .urlLogRecord(it$fn, it$url,
                           destinationPath = it$destinationPath,
                           cacheId = cacheId, cacheHit = FALSE,
                           via = "Cache")
      .writeSessionRecord(rec)
    }
    ## One tag-write for the whole vector so the `hasUrl` early-exit inside
    ## .persistUrlTags doesn't skip the 2nd, 3rd, ... urls of the same miss.
    fn <- items[[1L]]$fn
    allUrls <- vapply(items, `[[`, character(1), "url")
    try(.persistUrlTags(cacheId, fn, allUrls, cachePath, drv, conn,
                        isHit = FALSE), silent = TRUE)
  } else {
    tags <- .readUrlTagsForCacheId(cacheId, cachePath, drv, conn)
    fn <- if (is.na(tags$fn)) "prepInputs" else tags$fn
    if (length(tags$urls) == 0L) {
      info <- .urlInfoFromMatchedCall(callList$new_call,
                                      callList$.functionName,
                                      .callingEnv = .callingEnv)
      if (is.null(info)) return(invisible())
      tags$urls <- info$url
      fn <- info$fn
    }
    for (u in tags$urls) {
      rec <- .urlLogRecord(fn, u, destinationPath = NULL,
                           cacheId = cacheId, cacheHit = TRUE,
                           via = "Cache-replay")
      .writeSessionRecord(rec)
    }
    try(.persistUrlTags(cacheId, fn, tags$urls, cachePath, drv, conn,
                        isHit = TRUE), silent = TRUE)
  }
  invisible()
}
