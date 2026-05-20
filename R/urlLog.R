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
## Idempotency key is (fn, url, cacheId), so a given URL access against a given
## cacheId produces one record per scope (= per env, or per session in TRUE
## mode). The cache-DB tags carry their own hitCount counter independently.

.urlLogEnv <- new.env(parent = emptyenv())
.urlLogEnv$records <- list()
.urlLogEnv$seen    <- character()

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

## Session dispatcher. Called from prepInputs/preProcess function heads and
## from Cache hit/miss branches. NULL or empty url is ignored.
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

  rec <- .urlLogRecord(fn, url, destinationPath, cacheId, cacheHit, via)
  key <- .urlLogKey(fn, url, cacheId)

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
  ## Belt-and-braces: even with the function-literal skip above, an eval can
  ## still land on something un-coercible (e.g. a same-named function in the
  ## calling env). Refuse anything that isn't atomic-character-shaped.
  if (!is.atomic(url)) return(NULL)
  url <- tryCatch(as.character(url), error = function(e) NULL)
  if (is.null(url) || !length(url) || all(is.na(url) | !nzchar(url))) return(NULL)
  list(fn = fn, url = url)
}

## Read/write helpers against the cache DB. DBI-only; no-op otherwise.
.urlTagsForCacheId <- function(cacheId, cachePath, drv, conn) {
  if (!useDBI()) return(character(0))
  ownConn <- is.null(conn)
  if (ownConn) {
    conn <- tryCatch(dbConnectAll(drv, cachePath = cachePath, create = FALSE),
                     error = function(e) NULL)
    if (is.null(conn)) return(character(0))
    on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)
  }
  tab <- CacheDBTableName(cachePath, drv)
  qry <- paste0("SELECT DISTINCT \"tagKey\" FROM \"", tab,
                "\" WHERE \"cacheId\" = '", cacheId,
                "' AND \"tagKey\" LIKE 'reproducible.url%'")
  rs <- tryCatch(DBI::dbGetQuery(conn, qry), error = function(e) NULL)
  if (is.null(rs) || NROW(rs) == 0L) return(character(0))
  rs$tagKey
}

.urlTagValue <- function(cacheId, tagKey, cachePath, drv, conn) {
  if (!useDBI()) return(NA_character_)
  ownConn <- is.null(conn)
  if (ownConn) {
    conn <- tryCatch(dbConnectAll(drv, cachePath = cachePath, create = FALSE),
                     error = function(e) NULL)
    if (is.null(conn)) return(NA_character_)
    on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)
  }
  tab <- CacheDBTableName(cachePath, drv)
  qry <- paste0("SELECT \"tagValue\" FROM \"", tab,
                "\" WHERE \"cacheId\" = '", cacheId,
                "' AND \"tagKey\" = '", tagKey, "' LIMIT 1")
  rs <- tryCatch(DBI::dbGetQuery(conn, qry), error = function(e) NULL)
  if (is.null(rs) || NROW(rs) == 0L) return(NA_character_)
  rs$tagValue[1L]
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
  existing <- .urlTagsForCacheId(cacheId, cachePath, drv, conn)
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
                                               cachePath, drv, conn)))
    } else 0L
    if (is.na(cur)) 1L else cur + 1L
  } else {
    0L
  }
  if (hasCount) updateOne("reproducible.urlHitCount", newCount) else addOne("reproducible.urlHitCount", newCount)
  invisible()
}

## Single dispatcher called from Cache() at hit-return and post-save branches.
## Writes both the session record (deduped) and the persistent cacheId tags.
.maybeRecordUrlForCache <- function(callList, keyFull, cachePaths, drv, conn,
                                    isHit, .callingEnv = parent.frame()) {
  sink <- getOption("reproducible.urlLog", NULL)
  haveSink <- !(is.null(sink) || isFALSE(sink))
  info <- .urlInfoFromMatchedCall(callList$new_call, callList$.functionName,
                                  .callingEnv = .callingEnv)
  if (is.null(info)) return(invisible())

  cacheId <- keyFull$key
  if (haveSink) {
    destExpr <- callList$new_call$destinationPath
    dest <- if (is.null(destExpr)) NA_character_ else
      tryCatch(eval(destExpr, .callingEnv), error = function(e) NA_character_)
    .logUrlAccess(fn = info$fn, url = info$url, destinationPath = dest,
                  cacheId = cacheId, cacheHit = isHit, via = "Cache")
  }

  ## Persistent cacheId tags fire whenever the sink is configured; gate is the
  ## same option so users opt into the whole feature with one switch.
  if (haveSink) {
    cachePath <- if (length(cachePaths)) cachePaths[[1]] else
      getOption("reproducible.cachePath")
    try(.persistUrlTags(cacheId, info$fn, info$url, cachePath, drv, conn,
                        isHit = isHit), silent = TRUE)
  }
  invisible()
}
