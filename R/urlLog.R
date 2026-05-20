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
##     tags from the DB (via showCacheFast + extractFromCache) and emits
##     replay records (cacheHit = TRUE).
##
## Idempotency key is (fn, url, cacheId). Within a scope (env, or session in
## TRUE mode), each (fn,url,cacheId) triple produces one record. The cache-DB
## tags carry their own hitCount counter independently.
##
## Caller-supplied extra columns: for env sinks, set `sink$extra` to a list
## of key-value pairs and they are merged into every record. SpaDES.core uses
## this to attach module/event to each row -- update the slot just before
## dispatching each event. Core columns (time/fn/url/...) always win on key
## collisions.

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

## Build the (fn, url, cacheId, cacheHit) idempotency key. Field separator is
## \037 (US), which never appears in URLs / function names / cacheIds.
##
## cacheHit is part of the key so a cache miss and a subsequent cache hit of
## the same cacheId in the same scope each get one record -- they are distinct
## events ("bytes were fetched" vs. "the cached object was used") and the user
## wants both visible in the log.
.urlLogKey <- function(fn, url, cacheId, cacheHit) {
  paste(fn,
        paste(url, collapse = "\036"),
        if (is.null(cacheId) || !length(cacheId)) "NA" else cacheId,
        if (is.na(cacheHit)) "NA" else if (isTRUE(cacheHit)) "hit" else "miss",
        sep = "\037")
}

## Collapse a possibly-vector char value to a single field, NA if empty.
.scalarOrNA <- function(x) {
  if (is.null(x) || !length(x)) return(NA_character_)
  if (length(x) > 1L) return(paste(as.character(x), collapse = "; "))
  as.character(x)
}

## Normalize a path-like value to an absolute path (no must-work check).
## Returns NA on empty / null. Vector inputs joined with "; ".
.absPathOrNA <- function(x) {
  if (is.null(x) || !length(x) || all(is.na(x) | !nzchar(as.character(x))))
    return(NA_character_)
  paths <- tryCatch(
    normalizePath(as.character(x), mustWork = FALSE, winslash = "/"),
    error = function(e) as.character(x))
  if (length(paths) > 1L) paste(paths, collapse = "; ") else paths
}

## Build the record list written to the sink.
##
## Core columns: time, fn, url, targetFile, archive, alsoExtract,
## destinationPath, cacheId, cacheHit, via. Any extra columns supplied by
## the caller (env sink) via sink$extra are merged in -- core columns take
## precedence so reproducible's own fields can't be overridden.
.urlLogRecord <- function(fn, url,
                          targetFile = NULL, archive = NULL, alsoExtract = NULL,
                          destinationPath = NULL,
                          cacheId, cacheHit, via) {
  core <- list(
    time            = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3"),
    fn              = fn,
    url             = as.character(url),
    targetFile      = .scalarOrNA(targetFile),
    archive         = .scalarOrNA(archive),
    alsoExtract     = .scalarOrNA(alsoExtract),
    destinationPath = .absPathOrNA(destinationPath),
    cacheId         = if (is.null(cacheId)) NA_character_
                      else as.character(cacheId),
    cacheHit        = cacheHit,
    via             = via
  )
  sink <- getOption("reproducible.urlLog", NULL)
  if (is.environment(sink) && is.list(sink$extra) && length(sink$extra)) {
    ## modifyList(extra, core) so core overrides matching keys in extra.
    modifyList(sink$extra, core)
  } else {
    core
  }
}

## Write one record to whichever sink is active. Applies idempotency.
.writeSessionRecord <- function(rec) {
  sink <- getOption("reproducible.urlLog", NULL)
  key <- .urlLogKey(rec$fn, rec$url, rec$cacheId, rec$cacheHit)
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
.logUrlAccess <- function(fn, url,
                          targetFile = NULL, archive = NULL,
                          alsoExtract = NULL, destinationPath = NULL,
                          cacheId = NA_character_, cacheHit = NA,
                          via = NA_character_) {
  sink <- getOption("reproducible.urlLog", NULL)
  if (is.null(sink) || isFALSE(sink)) return(invisible())
  if (is.null(url)) return(invisible())
  if (is.character(url) && !length(url)) return(invisible())

  ## Inside a Cache wrapper: defer to Cache by pushing to its frame(s).
  ## Cache will write session records (with the cacheId) and tag the cacheId.
  if (length(.urlLogEnv$frames) > 0L &&
      (is.null(cacheId) || any(is.na(cacheId)))) {
    push <- list(fn = fn, url = url,
                 targetFile = targetFile, archive = archive,
                 alsoExtract = alsoExtract,
                 destinationPath = destinationPath)
    for (id in names(.urlLogEnv$frames)) {
      n <- length(.urlLogEnv$frames[[id]]) + 1L
      .urlLogEnv$frames[[id]][[n]] <- push
    }
    return(invisible())
  }

  ## Bare call (no Cache around): write directly with cacheId = NA.
  rec <- .urlLogRecord(fn = fn, url = url,
                       targetFile = targetFile, archive = archive,
                       alsoExtract = alsoExtract,
                       destinationPath = destinationPath,
                       cacheId = cacheId, cacheHit = cacheHit,
                       via = if (is.na(via)) fn else via)
  .writeSessionRecord(rec)
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

## ---- Persistent provenance tags on cacheIds ------------------------------

## Persistent provenance tags for a cacheId.
## isHit == FALSE -> first write: url/urlFn/firstSeen/lastSeen=now/hitCount=0
## isHit == TRUE  -> bump lastSeen + hitCount; back-fill url/urlFn/firstSeen
##                   if a pre-existing entry has none.
.persistUrlTags <- function(cacheId, fn, url, cachePath, drv, conn, isHit) {
  if (is.null(cacheId) || !nzchar(cacheId)) return(invisible())
  if (is.null(url) || !length(url)) return(invisible())
  if (is.null(cachePath) || !nzchar(cachePath)) return(invisible())

  now <- as.character(Sys.time())
  sc  <- tryCatch(showCacheFast(cacheId = cacheId, cachePath = cachePath,
                                strict = FALSE, drv = drv, conn = conn,
                                verbose = 0),
                  error = function(e) NULL)
  existing <- if (is.null(sc) || NROW(sc) == 0L) character(0) else
    unique(sc$tagKey[startsWith(sc$tagKey, "reproducible.url")])
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
      suppressWarnings(as.integer(extractFromCache(sc, "reproducible.urlHitCount")[1L]))
    } else 0L
    if (length(cur) == 0L || is.na(cur)) 1L else cur + 1L
  } else {
    0L
  }
  if (hasCount) updateOne("reproducible.urlHitCount", newCount) else addOne("reproducible.urlHitCount", newCount)
  invisible()
}

## Single dispatcher called from Cache() at hit-return and post-save branches.
##
## On miss (isHit=FALSE): drain this Cache's url frame (pushed by inner
## prepInputs/preProcess), write one session record per (fn, url) with the
## cacheId, and tag the cacheId.
##
## On hit (isHit=TRUE): read existing reproducible.url* tags from the cache
## DB via showCacheFast + extractFromCache, and replay one session record
## per url (cacheHit=TRUE).
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
    if (length(framed) == 0L) return(invisible())

    ## Dedup on url within this miss (a url shouldn't get two records for
    ## one cache miss even if multiple inner pushes happened).
    seenUrl <- character(0)
    items <- list()
    for (rec in framed) {
      for (u in rec$url) {
        if (u %in% seenUrl) next
        seenUrl <- c(seenUrl, u)
        items[[length(items) + 1L]] <- list(
          fn              = rec$fn,
          url             = u,
          targetFile      = rec$targetFile,
          archive         = rec$archive,
          alsoExtract     = rec$alsoExtract,
          destinationPath = rec$destinationPath
        )
      }
    }
    if (length(items) == 0L) return(invisible())

    for (it in items) {
      rec <- .urlLogRecord(fn = it$fn, url = it$url,
                           targetFile = it$targetFile,
                           archive = it$archive,
                           alsoExtract = it$alsoExtract,
                           destinationPath = it$destinationPath,
                           cacheId = cacheId, cacheHit = FALSE,
                           via = "Cache")
      .writeSessionRecord(rec)
    }
    ## One tag-write for the whole vector so the `hasUrl` early-exit inside
    ## .persistUrlTags doesn't skip the 2nd, 3rd, ... urls of the same miss.
    try(.persistUrlTags(cacheId, items[[1L]]$fn,
                        vapply(items, `[[`, character(1), "url"),
                        cachePath, drv, conn, isHit = FALSE), silent = TRUE)
  } else {
    sc <- tryCatch(showCacheFast(cacheId = cacheId, cachePath = cachePath,
                                 strict = FALSE, drv = drv, conn = conn,
                                 verbose = 0),
                   error = function(e) NULL)
    if (is.null(sc) || NROW(sc) == 0L) return(invisible())
    urls <- extractFromCache(sc, "reproducible.url")
    if (length(urls) == 0L) return(invisible())
    fnTag <- extractFromCache(sc, "reproducible.urlFn", ifNot = "prepInputs")[1L]
    ## destinationPath for hit replays: prefer the value in the current matched
    ## call (Cache(prepInputs(..., destinationPath = ...)) shape), else fall
    ## back to the option default so the column isn't blank.
    destExpr <- callList$new_call$destinationPath
    dest <- if (!is.null(destExpr)) {
      tryCatch(eval(destExpr, .callingEnv), error = function(e) NULL)
    } else {
      getOption("reproducible.destinationPath", ".")
    }
    for (u in urls) {
      rec <- .urlLogRecord(fn = fnTag, url = u,
                           destinationPath = dest,
                           cacheId = cacheId, cacheHit = TRUE,
                           via = "Cache-replay")
      .writeSessionRecord(rec)
    }
    try(.persistUrlTags(cacheId, fnTag, urls, cachePath, drv, conn,
                        isHit = TRUE), silent = TRUE)
  }
  invisible()
}
