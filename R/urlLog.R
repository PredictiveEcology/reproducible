## URL access logging for prepInputs / preProcess.
##
## The option `reproducible.urlLog` has three modes:
##   FALSE              -> fully off (kill switch).
##   NULL (default)     -> "tags only": prepInputs/preProcess accesses that
##                         flow through Cache() tag the cacheId in the cache DB
##                         with reproducible.url* tags (permanent, on disk,
##                         queryable via showCache). No in-memory session log.
##   <environment>      -> tags + a session log the caller owns (e.g.
##                         SpaDES.core simInitAndSpades). Records appended to
##                         `env$records`; idempotency via `env$seen`. Caller
##                         decides what to do with the contents on exit.
##   TRUE               -> tags + in-memory session log via prepInputsLog().
##   function(record)   -> tags + callback invoked with each record (no dedup).
##
## So persistent cacheId provenance accrues by default (cheap, disk-only); the
## live session log is opt-in. The cacheId tags also let a later cache hit be
## replayed into the session log even though no inner code runs on a hit.
##
## How `Cache(...prepInputs(url=...)...)` is handled (incl. Cache(Map(...))):
##   - On Cache entry, a transient "url frame" is allocated. prepInputs and
##     preProcess function-head hooks push (fn, url) onto every currently-open
##     url frame. Cache, on save, drains its frame, writes session records
##     with the cacheId, and tags the cacheId. On.exit clears the frame so
##     interrupted evaluations don't leak state.
##   - On Cache hit, no inner code runs. Cache reads existing reproducible.url
##     tags from the DB (via showCacheFast + extractFromCache) and emits
##     replay records so the access is still visible in the run's log.
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
.urlLogEnv$announced    <- FALSE   # one-time "how to view" hint per session

## One-time, per-session hint pointing users at the log. Fires the first time
## a URL access is recorded (any mode except FALSE), respects verbosity.
.maybeAnnounceUrlLog <- function() {
  if (isTRUE(.urlLogEnv$announced)) return(invisible())
  .urlLogEnv$announced <- TRUE
  messagePreProcess(
    "Recording data-source URLs from prepInputs/preProcess. View with ",
    "prepInputsLog() or showCache(userTags = 'reproducible.url'); ",
    "disable with options(reproducible.urlLog = FALSE).",
    verboseLevel = 1
  )
  invisible()
}

#' URL access log for `prepInputs` / `preProcess`
#'
#' Controlled by `getOption("reproducible.urlLog")`. See the package option
#' documentation for modes. `prepInputsLog()` returns the package-level
#' in-memory records, which are populated in the default (`NULL`) and `TRUE`
#' modes; `clearUrlLog()` empties them. Records written to an environment or
#' function sink live there instead and are not retrievable through these
#' accessors. Set the option to `FALSE` to disable logging entirely.
#'
#' @return `prepInputsLog()` returns a list of record lists. `clearUrlLog()`
#'   returns `NULL` invisibly.
#'
#' @rdname prepInputsLog
#' @export
#'
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true")
#' # Every prepInputs()/preProcess() call that resolves a url records it, so a
#' # completed run can report exactly which data sources it pulled.
#' clearUrlLog()
#' prepInputsLog() # empty at the start of a session
#'
#' # After any prepInputs()/preProcess() work, the log holds one record per url;
#' # rbindlist() turns it into a table for reporting.
#' # data.table::rbindlist(prepInputsLog())
prepInputsLog <- function() .urlLogEnv$records

#' @rdname prepInputsLog
#' @export
clearUrlLog <- function() {
  .urlLogEnv$records <- list()
  .urlLogEnv$seen    <- character()
  invisible()
}

## Build the (fn, url, cacheId) idempotency key. Field separator is \037 (US),
## which never appears in URLs / function names / cacheIds. Whether the access
## was a miss or a hit is internal cache mechanics, not part of the access
## identity, so it isn't keyed here.
.urlLogKey <- function(fn, url, cacheId) {
  paste(fn,
        paste(url, collapse = "\036"),
        if (is.null(cacheId) || !length(cacheId)) "NA" else cacheId,
        sep = "\037")
}

## Collapse a possibly-vector char value to a single field, NA if empty.
.scalarOrNA <- function(x) {
  if (is.null(x) || !length(x)) return(NA_character_)
  if (length(x) > 1L) return(paste(as.character(x), collapse = "; "))
  as.character(x)
}

## Normalize a path-like value using the package's normPath() (handles NAs,
## empty strings, ./-prefix, absolute-path forcing on *nix). NA on empty/null.
.absPathOrNA <- function(x) {
  if (is.null(x) || !length(x) || all(is.na(x) | !nzchar(as.character(x))))
    return(NA_character_)
  paths <- tryCatch(normPath(as.character(x)),
                    error = function(e) as.character(x))
  if (length(paths) > 1L) paste(paths, collapse = "; ") else paths
}

## Build the record list written to the sink.
##
## Core columns: time, fn, url, targetFile, archive, alsoExtract,
## destinationPath, cacheId. Any extra columns supplied by the caller (env
## sink) via sink$extra are merged in -- core columns take precedence so
## reproducible's own fields can't be overridden.
.urlLogRecord <- function(fn, url,
                          targetFile = NULL, archive = NULL, alsoExtract = NULL,
                          destinationPath = NULL,
                          cacheId = NA_character_) {
  core <- list(
    time            = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3"),
    fn              = fn,
    url             = as.character(url),
    targetFile      = .scalarOrNA(targetFile),
    archive         = .scalarOrNA(archive),
    alsoExtract     = .scalarOrNA(alsoExtract),
    destinationPath = .absPathOrNA(destinationPath),
    cacheId         = if (is.null(cacheId)) NA_character_
                      else as.character(cacheId)
  )
  sink <- getOption("reproducible.urlLog", NULL)
  if (is.environment(sink) && is.list(sink$extra) && length(sink$extra)) {
    ## modifyList(extra, core) so core overrides matching keys in extra.
    modifyList(sink$extra, core)
  } else {
    core
  }
}

## TRUE only when the user explicitly set the option to FALSE (kill switch).
.urlLogOff <- function() isFALSE(getOption("reproducible.urlLog", NULL))

## Write one record to whichever sink is active. Applies idempotency.
## NULL (default) and TRUE both route to the package-level in-memory log so
## prepInputsLog() captures accesses (incl. bare prepInputs with no Cache) out of
## the box; an environment or function sink overrides that destination.
.writeSessionRecord <- function(rec) {
  sink <- getOption("reproducible.urlLog", NULL)
  if (isFALSE(sink)) return(invisible())
  key <- .urlLogKey(rec$fn, rec$url, rec$cacheId)
  if (is.environment(sink)) {
    if (is.null(sink$seen))    sink$seen    <- character()
    if (is.null(sink$records)) sink$records <- list()
    if (!key %in% sink$seen) {
      sink$seen <- c(sink$seen, key)
      sink$records[[length(sink$records) + 1L]] <- rec
    }
  } else if (is.function(sink)) {
    try(sink(rec), silent = TRUE)
  } else {
    ## NULL (default) or TRUE -> package-level in-memory log.
    if (!key %in% .urlLogEnv$seen) {
      .urlLogEnv$seen <- c(.urlLogEnv$seen, key)
      .urlLogEnv$records[[length(.urlLogEnv$records) + 1L]] <- rec
    }
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
                          cacheId = NA_character_) {
  if (.urlLogOff()) return(invisible())
  if (is.null(url)) return(invisible())
  if (is.character(url) && !length(url)) return(invisible())
  .maybeAnnounceUrlLog()

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
                       cacheId = cacheId)
  .writeSessionRecord(rec)
}

## ---- Cache url-frame lifecycle -------------------------------------------

## Open a transient slot for one Cache call. Returns the frame id (or NULL if
## logging is off). Caller must arrange for .closeCacheUrlFrame() to fire on
## exit so interrupted Cache calls don't leak slots.
.openCacheUrlFrame <- function() {
  if (.urlLogOff()) return(NULL)
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

## ---- Matched-call URL extraction (legacy cache-hit recovery) -------------

## Walk a call expression for the first inner call whose function name matches.
## Handles bare names and `pkg::name`. Does NOT descend into `function(...)`
## literals (symbols there are formal args, not values we can resolve, e.g.
## `function(url) prepInputs(url = url)`). Returns list(name, call) or NULL.
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

## Recover (fn, url) from a Cache()'s matched call when the wrapped function is
## prepInputs/preProcess. Used on a cache HIT of a legacy entry that has no
## reproducible.url* tags (the URL is only hashed in the cache DB, so the
## current call expression is the only place a literal URL exists). Returns
## NULL for opaque wrappers (Cache(userFn())) where the URL isn't in the call.
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
  if (is.null(url) || !length(url) || !is.atomic(url)) return(NULL)
  url <- tryCatch(as.character(url), error = function(e) NULL)
  if (is.null(url) || all(is.na(url) | !nzchar(url))) return(NULL)
  list(fn = fn, url = url)
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
## per url. fromCache distinction isn't tracked at the record level (the
## DB tag hitCount captures access frequency separately).
.maybeRecordUrlForCache <- function(callList, keyFull, cachePaths, drv, conn,
                                    isHit, .callingEnv = parent.frame(),
                                    urlFrameId = NULL) {
  if (.urlLogOff()) return(invisible())

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
                           cacheId = cacheId)
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
    urls  <- if (is.null(sc) || NROW(sc) == 0L) character(0)
             else extractFromCache(sc, "reproducible.url")
    fnTag <- if (length(urls) == 0L) NA_character_
             else extractFromCache(sc, "reproducible.urlFn",
                                   ifNot = "prepInputs")[1L]

    ## Option 1 recovery: a legacy entry (created before this feature) has no
    ## reproducible.url* tags, and the URL is only hashed in the cache DB. The
    ## one place a literal URL still exists on a hit is the current matched
    ## call -- extract it for the direct Cache(prepInputs(url=...)) shape.
    ## Then .persistUrlTags below back-fills the tags so the entry self-heals.
    if (length(urls) == 0L) {
      info <- .urlInfoFromMatchedCall(callList$new_call, callList$.functionName,
                                      .callingEnv = .callingEnv)
      if (is.null(info)) return(invisible())
      urls  <- info$url
      fnTag <- info$fn
    }
    ## Pure cache-hit replay -- inner code didn't run, so .logUrlAccess didn't
    ## fire; announce here (once per session) now that a URL is confirmed.
    .maybeAnnounceUrlLog()

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
                           cacheId = cacheId)
      .writeSessionRecord(rec)
    }
    try(.persistUrlTags(cacheId, fnTag, urls, cachePath, drv, conn,
                        isHit = TRUE), silent = TRUE)
  }
  invisible()
}

#' Re-check downloaded files against their remote sources
#'
#' Walks the `.hash` sidecars written by [preProcess()] under `path`, asks each
#' recorded URL whether the file has changed, and removes the sidecars of those
#' that have. Nothing is downloaded here: the next ordinary
#' [preProcess()]/[prepInputs()] call re-fetches exactly the files whose
#' sidecar was removed, and leaves the rest alone.
#'
#' @details
#' This exists so that re-checking is something a user *does*, occasionally and
#' deliberately, rather than a mode that gets switched on and forgotten.
#' `options(reproducible.checkRemoteHash = TRUE)` is sticky — set once in a
#' project's setup and every run pays for it forever — whereas this is a single
#' action with no lasting state.
#'
#' How each file is checked depends on what its sidecar holds:
#' an ETag is revalidated with a conditional `If-None-Match` request (one
#' round-trip, no download); otherwise the recorded digest is compared with the
#' hash the server currently advertises. Files whose remote cannot be reached
#' are reported as `"unreachable"` and left untouched.
#'
#' Sidecars written before the URL was recorded have nothing to check against
#' and are reported as `"noURL"`.
#'
#' @param path Directory to walk. Typically `inputPath(sim)` or
#'   `outputPath(sim)`.
#' @param recursive Logical; recurse into subdirectories. Default `TRUE`.
#' @param dryRun Logical; if `TRUE`, report what would happen without removing
#'   any sidecar. Default `FALSE`.
#' @param verbose Numeric verbosity.
#'
#' @return Invisibly, a `data.frame` with one row per sidecar: `file`, `url`,
#'   `status` (one of `"unchanged"`, `"changed"`, `"unreachable"`, `"noURL"`)
#'   and `sidecar`.
#'
#' @export
#' @examples
#' \donttest{
#' # after a run, ask which inputs have changed upstream
#' revalidateRemotes(tempdir(), dryRun = TRUE)
#' }
revalidateRemotes <- function(path = ".", recursive = TRUE, dryRun = FALSE,
                              verbose = getOption("reproducible.verbose")) {
  sidecars <- dir(path, pattern = "\\.hash$", all.files = TRUE,
                  full.names = TRUE, recursive = recursive)

  empty <- data.frame(file = character(), url = character(),
                      status = character(), sidecar = character(),
                      stringsAsFactors = FALSE)
  if (!length(sidecars)) return(invisible(empty))

  rows <- lapply(sidecars, function(sc) {
    parsed <- .parseRemoteHashFile(sc)
    mkRow <- function(status, url = NA_character_)
      data.frame(file = basename(sc), url = url, status = status,
                 sidecar = sc, stringsAsFactors = FALSE)

    if (is.null(parsed) || is.null(parsed$url) || !nzchar(parsed$url))
      return(mkRow("noURL"))

    url <- parsed$url
    status <- if (!is.null(parsed$etag) && nzchar(parsed$etag)) {
      reval <- .remoteEtagRevalidate(url, parsed$etag)
      if (isTRUE(reval$unchanged)) "unchanged"
      else if (isFALSE(reval$unchanged)) "changed"
      else "unreachable"
    } else {
      meta <- tryCatch(getRemoteMetadata(url = url), error = function(e) NULL)
      if (is.null(meta) || is.null(meta$remoteHash)) "unreachable"
      else if (identical(tolower(as.character(meta$remoteHash)),
                         tolower(as.character(parsed$hash)))) "unchanged"
      else "changed"
    }

    if (identical(status, "changed") && !isTRUE(dryRun)) unlink(sc)
    mkRow(status, url)
  })

  out <- do.call(rbind, rows)
  nChanged <- sum(out$status == "changed")
  messagePreProcess(
    "revalidateRemotes: ", NROW(out), " sidecar(s); ", nChanged, " changed",
    if (nChanged && !isTRUE(dryRun)) " (sidecars removed; next call re-downloads)"
    else if (nChanged) " (dryRun: nothing removed)" else "",
    verbose = verbose
  )
  invisible(out)
}
