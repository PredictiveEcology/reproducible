## URL access logging for prepInputs / preProcess.
##
## The option `reproducible.urlLog` selects where accesses are recorded:
##   FALSE              -> fully off (kill switch): no records, no tags.
##   NULL (default)     -> in-memory session log (retrieved with prepInputsLog())
##                         + cacheId tags. A bare prepInputs/preProcess (no Cache)
##                         is recorded straight to the log with cacheId = NA
##                         (.logUrlAccess()); one that flows through Cache() is
##                         recorded with its cacheId and also tags the cacheId in
##                         the cache DB with reproducible.url* tags (permanent, on
##                         disk, queryable via showCache).
##   TRUE               -> the same as NULL.
##   <environment>      -> tags + a session log the caller owns (e.g.
##                         SpaDES.core simInitAndSpades). Records appended to
##                         `env$records`; idempotency via `env$seen`. Caller
##                         decides what to do with the contents on exit.
##   function(record)   -> tags + callback invoked with each record (no dedup).
##
## So both the persistent cacheId provenance and the in-memory session log accrue
## by default. The cacheId tags also let a later cache hit be replayed into the
## session log even though no inner code runs on a hit.
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
## Idempotency key is (fn, url, cacheId). Within a scope (env, or the
## in-memory log in the default and TRUE modes), each (fn,url,cacheId) triple produces one record. The cache-DB
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
.urlLogEnv$pid          <- NA_integer_ # the process that owns this state; set in .onLoad
.urlLogEnv$parentPid    <- NA_integer_ # set in a forked child: where its records are sent
.urlLogEnv$spoolCounter <- 0L

## ---- Forked children (parallel::mclapply, makeForkCluster, future multicore) ----
##
## A forked child has a copy of .urlLogEnv: what it records, and what it pushes onto
## the url frames of the parent's open Cache() calls, would die with it. So a child
## also writes each record into a spool folder of its parent (tempdir() is shared
## with a fork), and the parent reads that folder whenever it next records, drains a
## frame, or is asked for prepInputsLog(). Frame ids carry the pid of the process
## that opened them, so a child knows which frames are its own. Separate R sessions
## (PSOCK clusters, callr) are not forks; their records stay in their own session.
.urlLogForkCheck <- function() {
  me <- Sys.getpid()
  if (is.na(.urlLogEnv$pid)) {
    .urlLogEnv$pid <- me
  } else if (.urlLogEnv$pid != me) {
    .urlLogEnv$parentPid <- .urlLogEnv$pid
    .urlLogEnv$pid <- me
  }
  invisible()
}

.urlSpoolDir <- function(pid) file.path(tempdir(), paste0("reproducible-urlLog-", pid))

.urlSpool <- function(pid, item) {
  d <- .urlSpoolDir(pid)
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
  repeat {
    .urlLogEnv$spoolCounter <- .urlLogEnv$spoolCounter + 1L
    f <- file.path(d, sprintf("%d-%08d.rds", Sys.getpid(), .urlLogEnv$spoolCounter))
    if (!file.exists(f)) break
  }
  tmp <- paste0(f, ".tmp")
  try({
    saveRDS(item, tmp, compress = FALSE)
    file.rename(tmp, f)
  }, silent = TRUE)
  invisible()
}

## Read what forked children of this process recorded, oldest first.
.urlIngestSpool <- function() {
  .urlLogForkCheck()
  d <- .urlSpoolDir(Sys.getpid())
  if (!dir.exists(d)) return(invisible())
  fs <- dir(d, pattern = "[.]rds$", full.names = TRUE)
  if (!length(fs)) return(invisible())
  fs <- fs[order(file.mtime(fs), fs)]
  for (f in fs) {
    item <- tryCatch(readRDS(f), error = function(e) NULL)
    unlink(f)
    if (is.null(item)) next
    if (identical(item$type, "push")) {
      open <- intersect(item$frames, names(.urlLogEnv$frames))
      if (length(open)) .pushToFrames(item$push, open) else .writeSessionRecord(item$rec)
    } else {
      .writeSessionRecord(item$rec)
    }
  }
  invisible()
}

.frameOwner <- function(ids) as.integer(sub("^f([0-9]+)_.*$", "\\1", ids))

## Push one access onto open url frames: in memory for this process's own frames,
## through the spool for frames a parent process opened. `rec` is the record the
## parent writes if those frames have closed by the time it reads the spool.
.pushToFrames <- function(push, ids, rec = NULL) {
  if (!length(ids)) return(invisible())
  owner <- .frameOwner(ids)
  me <- Sys.getpid()
  for (id in ids[owner == me]) {
    n <- length(.urlLogEnv$frames[[id]]) + 1L
    .urlLogEnv$frames[[id]][[n]] <- push
  }
  for (pid in unique(owner[owner != me])) {
    if (is.null(rec)) rec <- .recordFromPush(push, cacheId = NA_character_)
    .urlSpool(pid, list(type = "push", frames = ids[owner == pid], push = push, rec = rec))
  }
  invisible()
}

.recordFromPush <- function(push, cacheId) {
  .urlLogRecord(fn = push$fn, url = push$url, targetFile = push$targetFile,
                archive = push$archive, alsoExtract = push$alsoExtract,
                destinationPath = push$destinationPath, cacheId = cacheId)
}

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

#' The download ledger: a record of the data a run read from outside
#'
#' reproducible keeps a ledger of the web addresses (URLs) and Google Drive files that
#' data were read from, so that a finished run can say where its inputs came from.
#' `prepInputsLog()` returns the ledger of the current R session, `clearUrlLog()` empties it,
#' [urlLog()] reads the ledger kept in a cache, and `logUrlAccess()` adds an access that
#' did not go through `prepInputs()`.
#'
#' @section What is recorded, and when:
#' One record is written each time one of these is called with a URL:
#' \itemize{
#'   \item [prepInputs()] and [preProcess()] (`fn` is `"prepInputs"` or `"preProcess"`);
#'   \item a `dlFun` given to them with no `url` (`fn` ends in `"(dlFun)"`; the `url`
#'     column holds `"dlFun: "` and the `dlFun` expression);
#'   \item the listing of a directory-index URL that `preProcess()` expands into files
#'     (`fn` is `"prepInputs (directory index)"`);
#'   \item [CacheGeo()] and [CacheGeoWrite()], for the Google Drive ledger file they read or
#'     write;
#'   \item `logUrlAccess()`, called by any code that fetches data by other means.
#' }
#' A record has the columns `time`, `fn`, `url`, `targetFile`, `archive`, `alsoExtract`,
#' `destinationPath` and `cacheId` (`NA` outside [Cache()]). Within one scope a repeated
#' `fn`, `url` and `cacheId` gives one record, not several. A URL is recorded when it is
#' used, before the download starts, so a download that fails is recorded too.
#'
#' @section Inside `Cache()`:
#' A record made while a [Cache()] call is evaluating is held until the call has
#' finished. Cache then writes the record with that call's `cacheId` and stores the
#' URLs as tags on the cache entry: `reproducible.url`, `reproducible.urlFn`,
#' `reproducible.urlFirstSeen`, `reproducible.urlLastSeen` and `reproducible.urlHitCount`.
#' The tags stay in the cache, so [urlLog()] and `showCache(userTags = "reproducible.url")`
#' report them later. On a cache hit nothing is evaluated, so no download is made, but the
#' tags are read and the same records (each url with its own `fn`, `targetFile`, `archive`,
#' `alsoExtract` and `destinationPath`) are written again to the ledger, and `hitCount` is
#' increased. A `Cache()` call nested inside another passes its urls to the outer one
#' whether it was computed or a hit, so the outer entry is tagged with every url read
#' beneath it. If the cached function fails, the urls it read are still written, with
#' `cacheId` `NA`.
#'
#' @section The option `reproducible.urlLog`:
#' Where records go is set by `options(reproducible.urlLog = )`:
#' \describe{
#'   \item{`NULL` (default) or `TRUE`}{the in-memory list read by `prepInputsLog()`, plus the
#'     cache tags above.}
#'   \item{`FALSE`}{recording is off: no records, no tags.}
#'   \item{an environment}{records are appended to `env$records` (and keys to `env$seen`);
#'     anything in the list `env$extra` is added to each record as extra columns. The
#'     caller owns the environment.}
#'   \item{a function}{called with each record (a list); there is no de-duplication.}
#' }
#' With an environment or a function, records still go to the session ledger too, so
#' `prepInputsLog()` always holds every access of the session.
#' SpaDES.core uses the environment form: with `options(spades.urlLog = TRUE)` (the
#' default) each `simList` carries its own ledger, with `module` and `event` columns,
#' read with `urlLog(sim)`. `spades.urlLog = FALSE` turns that off. `prepInputsLog()` is
#' the ledger of the session; `urlLog(sim)` is the ledger of one run.
#'
#' @section Parallel code:
#' A forked child (`parallel::mclapply()`, a fork cluster, `future`'s multicore plan) sends
#' its records to the process that forked it, which adds them to its own ledger, and to the
#' `Cache()` call open around the fork. A separate R session (a PSOCK cluster, `callr`) has
#' its own ledger; read it there with `prepInputsLog()`.
#'
#' @section Why a ledger can miss an access:
#' The ledger holds only what was passed to `prepInputs()`, `preProcess()` or
#' `logUrlAccess()`. Code that downloads another way (`download.file()`,
#' `googledrive::drive_download()`, a package that reads a web service) is not seen unless it
#' calls `logUrlAccess()`. A cache hit replays the tags on its entry, so an entry made
#' without them is silent on a hit: one made with `reproducible.urlLog = FALSE`, one made
#' before URL tags existed, and one made before reproducible 3.2.1.9089 around an inner
#' `Cache()` hit (such an entry lacks the inner urls). For a legacy entry of the direct
#' form `Cache(prepInputs(url = ...))` the url is recovered from the call and the tags are
#' added. Clearing such entries, or one run with `useCache = "overwrite"`, rebuilds their
#' tags. With `reproducible.urlLog = FALSE` nothing is recorded.
#'
#' @section Cloud caches:
#' Reading or writing a `Cache(useCloud = TRUE)` entry on Google Drive is not recorded in the
#' ledger; the URLs tagged on an entry are those of the `prepInputs()` calls that made it.
#' Such a cache folder shared "Anyone with the link" is read without a Google sign-in (see
#' `useCloud` in [Cache()]).
#'
#' @section Recording a download made by other means:
#' A package or module author who fetches data without `prepInputs()` calls
#' `logUrlAccess()` with the address, once per access, just before the download. Inside
#' `Cache()` that tags the cache entry and is replayed on a hit, exactly as for
#' `prepInputs()`. See the example.
#'
#' @param url Character. The address (or addresses) read: a URL, a Google Drive link, or
#'   any identifier of the remote source. `NULL` or empty is ignored.
#' @param fn Character. The name recorded in the `fn` column. The default is the name of
#'   the function that called `logUrlAccess()`.
#' @param targetFile,archive,alsoExtract,destinationPath Optional. Recorded as in
#'   [prepInputs()], when they apply to the download.
#'
#' @return `prepInputsLog()` returns a list of record lists. `clearUrlLog()` and
#'   `logUrlAccess()` return `NULL` invisibly.
#'
#' @seealso [urlLog()] for the ledger kept in a cache, [reproducibleOptions()] for the
#'   option, [prepInputs()], [preProcess()], [Cache()].
#'
#' @rdname prepInputsLog
#' @export
#'
#' @examples
#' clearUrlLog()
#' prepInputsLog() # empty at the start of a session
#'
#' # A function that downloads by its own means says so with logUrlAccess().
#' getMyTable <- function(url = "https://example.com/table.csv") {
#'   logUrlAccess(url)      # fn is "getMyTable"
#'   # download.file(url, tempfile()) ... here
#'   data.frame(x = 1)
#' }
#' getMyTable()
#' data.table::rbindlist(prepInputsLog())[, c("fn", "url")]
#'
#' # Inside Cache() the url is also stored on the cache entry, and recorded again on a hit.
#' \donttest{
#' op <- options(reproducible.cachePath = tempfile())
#' Cache(getMyTable())
#' urlLog(which = c("url", "caller"))
#' options(op)
#' }
#' clearUrlLog()
prepInputsLog <- function() {
  .urlIngestSpool()
  .urlLogEnv$records
}

#' @rdname prepInputsLog
#' @export
clearUrlLog <- function() {
  .urlLogEnv$records <- list()
  .urlLogEnv$seen    <- character()
  invisible()
}

#' @rdname prepInputsLog
#' @export
logUrlAccess <- function(url, fn = NULL, targetFile = NULL, archive = NULL,
                         alsoExtract = NULL, destinationPath = NULL) {
  if (is.null(fn)) {
    caller <- sys.call(-1L)
    fn <- if (is.null(caller)) "logUrlAccess" else
      paste(deparse(caller[[1L]], width.cutoff = 500L), collapse = "")
  }
  .logUrlAccess(fn = fn, url = url, targetFile = targetFile, archive = archive,
                alsoExtract = alsoExtract, destinationPath = destinationPath)
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

## Write one record to the active sink and to the session log. Applies idempotency.
.writeSessionRecord <- function(rec) {
  sink <- getOption("reproducible.urlLog", NULL)
  if (isFALSE(sink)) return(invisible())
  .urlLogForkCheck()
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
  }
  ## The session log gets every record whatever the sink, so prepInputsLog() is
  ## complete for the session (an environment sink, e.g. SpaDES.core's, is one run's).
  if (!key %in% .urlLogEnv$seen) {
    .urlLogEnv$seen <- c(.urlLogEnv$seen, key)
    .urlLogEnv$records[[length(.urlLogEnv$records) + 1L]] <- rec
    if (!is.na(.urlLogEnv$parentPid))
      .urlSpool(.urlLogEnv$parentPid, list(type = "session", rec = rec))
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
  .urlIngestSpool()
  if (length(.urlLogEnv$frames) > 0L &&
      (is.null(cacheId) || any(is.na(cacheId)))) {
    push <- list(fn = fn, url = url,
                 targetFile = targetFile, archive = archive,
                 alsoExtract = alsoExtract,
                 destinationPath = destinationPath)
    .pushToFrames(push, names(.urlLogEnv$frames))
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
  .urlIngestSpool()
  .urlLogEnv$frameCounter <- .urlLogEnv$frameCounter + 1L
  id <- paste0("f", Sys.getpid(), "_", .urlLogEnv$frameCounter)
  .urlLogEnv$frames[[id]] <- list()
  id
}

## A frame still holding accesses when it closes belongs to a Cache() call that
## did not finish (its function or the save failed): the accesses happened, so
## they are written with cacheId = NA.
.closeCacheUrlFrame <- function(id) {
  if (is.null(id)) return(invisible())
  left <- .takeCacheUrlFrame(id)
  .urlLogEnv$frames[[id]] <- NULL
  for (push in left) .writeSessionRecord(.recordFromPush(push, cacheId = NA_character_))
  invisible()
}

.takeCacheUrlFrame <- function(id) {
  if (is.null(id)) return(list())
  .urlIngestSpool()
  recs <- .urlLogEnv$frames[[id]]
  if (!is.null(recs)) .urlLogEnv$frames[[id]] <- list()
  if (is.null(recs)) list() else recs
}

## One cache tag per url holding the whole record, so a hit replays each url with
## its own fn, targetFile, archive, alsoExtract and destinationPath. Fields are
## separated by \037; the trailing "." keeps strsplit() from dropping empty fields.
.urlRecordFields <- c("fn", "url", "targetFile", "archive", "alsoExtract", "destinationPath")

.encodeUrlRecord <- function(rec) {
  vals <- vapply(.urlRecordFields, function(f) {
    v <- if (f == "destinationPath") .absPathOrNA(rec[[f]]) else .scalarOrNA(rec[[f]])
    if (is.na(v)) "" else v
  }, character(1))
  paste(c(vals, "."), collapse = "\037")
}

.decodeUrlRecords <- function(x) {
  lapply(strsplit(x, "\037", fixed = TRUE), function(v) {
    v <- v[seq_along(.urlRecordFields)]
    v[is.na(v) | !nzchar(v)] <- NA_character_
    as.list(setNames(v, .urlRecordFields))
  })
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
.persistUrlTags <- function(cacheId, fn, url, cachePath, drv, conn, isHit, records = list()) {
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
  hasRecord    <- "reproducible.urlRecord"    %in% existing

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
  if (!hasRecord) for (r in records) addOne("reproducible.urlRecord", .encodeUrlRecord(r))
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

## Replay the url tags of one cache entry, as on a Cache() hit: one session record per
## url, each also pushed onto the frames of enclosing Cache() calls other than
## `urlFrameId`, so an outer entry computed around this hit is tagged as if the entry's
## code had run. Returns the replayed records (list(), if the entry has no url tags).
## SpaDES.core calls this for the events a cacheChaining jump restores without a Cache() call.
.replayUrlTags <- function(cacheId, cachePath, drv = getDrv(getOption("reproducible.drv", NULL)),
                           conn = getOption("reproducible.conn", NULL), urlFrameId = NULL) {
  if (.urlLogOff()) return(invisible(list()))
  sc <- tryCatch(showCacheFast(cacheId = cacheId, cachePath = cachePath,
                               strict = FALSE, drv = drv, conn = conn, verbose = 0),
                 error = function(e) NULL)
  if (is.null(sc) || NROW(sc) == 0L) return(invisible(list()))
  tagged <- extractFromCache(sc, "reproducible.urlRecord")
  items <- if (length(tagged)) {
    .decodeUrlRecords(tagged) # each url with its own fields
  } else {
    ## entries written before the reproducible.urlRecord tag
    urls <- extractFromCache(sc, "reproducible.url")
    fn <- extractFromCache(sc, "reproducible.urlFn", ifNot = "prepInputs")[1L]
    lapply(urls, function(u) list(fn = fn, url = u))
  }
  .replayItems(items, cacheId, urlFrameId)
  invisible(items)
}

.replayItems <- function(items, cacheId, urlFrameId = NULL) {
  if (!length(items)) return(invisible())
  ## Pure cache-hit replay -- inner code didn't run, so .logUrlAccess didn't
  ## fire; announce here (once per session) now that a URL is confirmed.
  .maybeAnnounceUrlLog()
  outer <- setdiff(names(.urlLogEnv$frames), urlFrameId)
  for (it in items) {
    .writeSessionRecord(.recordFromPush(it, cacheId = cacheId))
    .pushToFrames(it, outer)
  }
  invisible()
}

## Single dispatcher called from Cache() at hit-return and post-save branches.
##
## On miss (isHit=FALSE): drain this Cache's url frame (pushed by inner
## prepInputs/preProcess), write one session record per (fn, url) with the
## cacheId, and tag the cacheId.
##
## On hit (isHit=TRUE): read the reproducible.urlRecord (or, for older entries,
## reproducible.url*) tags from the cache DB, replay one session record per url,
## and push each onto the frames of enclosing Cache() calls, so an outer entry
## computed around an inner hit is tagged as if the inner call had run.
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

    for (it in items) .writeSessionRecord(.recordFromPush(it, cacheId = cacheId))
    ## One tag-write for the whole vector so the `hasUrl` early-exit inside
    ## .persistUrlTags doesn't skip the 2nd, 3rd, ... urls of the same miss.
    try(.persistUrlTags(cacheId, items[[1L]]$fn,
                        vapply(items, `[[`, character(1), "url"),
                        cachePath, drv, conn, isHit = FALSE, records = items), silent = TRUE)
  } else {
    items <- .replayUrlTags(cacheId, cachePath, drv = drv, conn = conn,
                            urlFrameId = urlFrameId)
    if (!length(items)) {
      ## Option 1 recovery: a legacy entry (created before this feature) has no
      ## reproducible.url* tags, and the URL is only hashed in the cache DB. The
      ## one place a literal URL still exists on a hit is the current matched
      ## call -- extract it for the direct Cache(prepInputs(url=...)) shape.
      ## Then .persistUrlTags below back-fills the tags so the entry self-heals.
      info <- .urlInfoFromMatchedCall(callList$new_call, callList$.functionName,
                                      .callingEnv = .callingEnv)
      if (is.null(info)) return(invisible())
      ## destinationPath: the value in the current matched call
      ## (Cache(prepInputs(..., destinationPath = ...)) shape), else the option.
      destExpr <- callList$new_call$destinationPath
      dest <- if (!is.null(destExpr)) {
        tryCatch(eval(destExpr, .callingEnv), error = function(e) NULL)
      } else {
        getOption("reproducible.destinationPath", ".")
      }
      items <- lapply(info$url, function(u) list(fn = info$fn, url = u, destinationPath = dest))
      .replayItems(items, cacheId, urlFrameId)
    }
    try(.persistUrlTags(cacheId, items[[1L]]$fn, vapply(items, `[[`, character(1), "url"),
                        cachePath, drv, conn, isHit = TRUE), silent = TRUE)
  }
  invisible()
}

#' Check downloaded files against their remote sources
#'
#' Walks the `.hash` sidecars written by [preProcess()] under `path`, asks each
#' recorded URL whether the file has changed, and reports what it found. The
#' check always happens; `redownload` decides what to do about anything that
#' has changed.
#'
#' @details
#' This exists so that re-checking is something a user *does*, occasionally and
#' deliberately, rather than a mode that gets switched on and forgotten.
#' `options(reproducible.checkRemoteHash = TRUE)` is sticky — set once in a
#' project's setup and every run pays for it forever — whereas this is a single
#' action with no lasting state.
#'
#' How each file is checked depends on what its sidecar holds: an ETag is
#' revalidated with a conditional `If-None-Match` request (one round-trip, no
#' download); otherwise the recorded digest is compared with the hash the server
#' currently advertises. Files whose remote cannot be reached are reported as
#' `"unreachable"` and left alone, as are sidecars written before the URL was
#' recorded (`"noURL"`).
#'
#' @param path Directory to walk. Typically `inputPath(sim)` or
#'   `outputPath(sim)`.
#' @param redownload What to do with files whose remote has changed. Defaults
#'   to `"no"`, so a bare call reports without touching anything.
#'   \describe{
#'     \item{`"no"`}{report only; change nothing.}
#'     \item{`"immediate"`}{fetch them now.}
#'     \item{`"nextPreProcess"`}{remove their sidecars, so the next ordinary
#'       [preProcess()] call fetches them. Nothing is downloaded here.}
#'   }
#'   Partial matches work, e.g. `"imm"` and `"next"`. (`"n"` alone is ambiguous
#'   between `"no"` and `"nextPreProcess"`.)
#' @param recursive Logical; recurse into subdirectories. Default `TRUE`.
#' @param verbose Numeric verbosity.
#'
#' @return Invisibly, a `data.frame` with one row per sidecar: `file`, `url`,
#'   `status` (`"unchanged"`, `"changed"`, `"unreachable"` or `"noURL"`),
#'   `action` (`"none"`, `"sidecarRemoved"`, `"redownloaded"` or
#'   `"redownloadFailed"`) and `sidecar`.
#'
#' @export
#' @rdname preProcessCheckURLs
#'
#' @examples
#' \donttest{
#' # what has changed upstream, without touching anything (the default)
#' preProcessCheckURLs(tempdir())
#'
#' # ... then act on it; partial matches are fine
#' preProcessCheckURLs(tempdir(), redownload = "next")   # refetch on next preProcess()
#' preProcessCheckURLs(tempdir(), redownload = "imm")    # refetch now
#' }
preProcessCheckURLs <- function(path = ".",
                                redownload = c("no", "immediate", "nextPreProcess"),
                                recursive = TRUE,
                                verbose = getOption("reproducible.verbose")) {
  redownload <- match.arg(redownload)

  sidecars <- dir(path, pattern = "\\.hash$", all.files = TRUE,
                  full.names = TRUE, recursive = recursive)

  empty <- data.frame(file = character(), url = character(),
                      status = character(), action = character(),
                      sidecar = character(), stringsAsFactors = FALSE)
  if (!length(sidecars)) return(invisible(empty))

  rows <- lapply(sidecars, function(sc) {
    parsed <- .parseRemoteHashFile(sc)
    mkRow <- function(status, action = "none", url = NA_character_)
      data.frame(file = basename(sc), url = url, status = status,
                 action = action, sidecar = sc, stringsAsFactors = FALSE)

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

    if (!identical(status, "changed")) return(mkRow(status, "none", url))

    action <- switch(
      redownload,
      no = "none",
      nextPreProcess = { unlink(sc); "sidecarRemoved" },
      immediate = {
        # drop the stale sidecar first so preProcess() cannot trust it, then
        # fetch; preProcess() rewrites the sidecar from the new response
        unlink(sc)
        ok <- tryCatch({
          preProcess(url = url, destinationPath = dirname(sc), verbose = verbose - 1L)
          TRUE
        }, error = function(e) FALSE)
        if (isTRUE(ok)) "redownloaded" else "redownloadFailed"
      }
    )
    mkRow(status, action, url)
  })

  out <- do.call(rbind, rows)
  nChanged <- sum(out$status == "changed")
  messagePreProcess(
    "preProcessCheckURLs: ", NROW(out), " sidecar(s) checked; ", nChanged, " changed",
    switch(redownload,
           no = " (redownload = 'no': nothing changed on disk)",
           nextPreProcess = if (nChanged) " (sidecars removed; next preProcess() re-downloads)" else "",
           immediate = if (nChanged) " (re-downloaded now)" else ""),
    verbose = verbose
  )
  invisible(out)
}

## ---- urlLog(): accessor for the url ledger kept as Cache tags -------------

## which -> where it is read from. "url", "caller", "firstSeen", "lastSeen" and
## "hitCount" are the reproducible.url* tags; "function", "module" and "event"
## are the Cache tags of the same entry (SpaDES.core writes "module" and
## "eventType"); "cacheId" and "createdDate" are columns of the cache table.
.urlLogTagKeys <- c(url = "reproducible.url", caller = "reproducible.urlFn",
                    firstSeen = "reproducible.urlFirstSeen",
                    lastSeen = "reproducible.urlLastSeen",
                    hitCount = "reproducible.urlHitCount",
                    `function` = "function", module = "module", event = "eventType")
.urlLogWhich <- c("cacheId", "function", "module", "event", "url", "caller",
                  "targetFile", "archive", "alsoExtract", "destinationPath",
                  "firstSeen", "lastSeen", "hitCount", "createdDate")
.urlLogRecordCols <- c("targetFile", "archive", "alsoExtract", "destinationPath")

#' The URL ledger kept in a cache
#'
#' `Cache()` tags every entry that came from `prepInputs()`/`preProcess()` with the
#' url(s) it downloaded from (see the `urlLog` option in [reproducibleOptions()]).
#' `urlLog()` reads those tags into one table, one row per cache entry and url.
#' It reads only the `reproducible.url*` tags and the named tags of those entries, so
#' argument digests that merely contain "url" never appear.
#'
#' `urlLog()` is an S3 generic. Other packages add methods for their own objects, for
#' example SpaDES.core for a `simList`; every method returns the same column names.
#'
#' @param x A cache path (character); defaults to `getOption("reproducible.cachePath")`
#'   (`NULL` means the same). Methods for other classes are provided by
#'   the packages that own them.
#' @param which Character vector of columns to return, in the order given. Allowed
#'   values: `"cacheId"`, `"function"` (the function that was cached), `"module"` and
#'   `"event"` (the SpaDES module and event type, `NA` outside SpaDES), `"url"`,
#'   `"caller"` (`prepInputs` or `preProcess`), `"targetFile"`, `"archive"`, `"alsoExtract"`
#'   and `"destinationPath"` (as given to the call that read the url; `NA` for entries
#'   made before these were stored), `"firstSeen"`, `"lastSeen"`,
#'   `"hitCount"` (times the entry was reused) and `"createdDate"`.
#' @param ... Passed to methods; unused by the default method.
#'
#' @return A `data.table` with the columns in `which`, `NA` where a value is absent,
#'   sorted by `lastSeen`, most recent first. A cache with no logged urls gives a
#'   zero-row table with the same columns.
#'
#' @seealso [prepInputsLog()] for the per-session log, [showCache()].
#' @export
#' @rdname urlLog
#' @examples
#' tmp <- tempfile()
#' urlLog(tmp, which = c("url", "function", "lastSeen"))  # empty until prepInputs() has run in Cache()
urlLog <- function(x = getOption("reproducible.cachePath"), ...) UseMethod("urlLog")

#' @export
#' @rdname urlLog
urlLog.default <- function(x = getOption("reproducible.cachePath"), which = c("function", "module", "url"), ...) {
  which <- match.arg(which, .urlLogWhich, several.ok = TRUE)
  if (is.null(x)) x <- getOption("reproducible.cachePath") # an explicit NULL still means the default
  if (!is.character(x))
    stop("urlLog() has no method for an object of class ", class(x)[1])
  x <- x[1]

  empty <- function() {
    out <- setNames(rep(list(character()), length(which)), which)
    setDT(out)
  }
  ## Only entries that carry a reproducible.url tag; userTags is a regexp, so
  ## keep exact tagKey matches.
  urlTags <- showCache(x, userTags = "reproducible.url", verbose = FALSE)
  urlTags <- urlTags[urlTags$tagKey == "reproducible.url"]
  if (NROW(urlTags) == 0L) return(empty())
  ids <- unique(urlTags$cacheId)

  tags <- showCache(x, cacheId = ids, verbose = FALSE)
  tags <- tags[tags$tagKey %in% c(.urlLogTagKeys, "reproducible.urlRecord")]

  ## one value per (cacheId, key), the most recent if repeated; url keeps them all
  pick <- function(key) {
    d <- tags[tags$tagKey == key, c("cacheId", "tagValue")]
    d[!duplicated(d$cacheId, fromLast = TRUE)]
  }
  out <- urlTags[, c("cacheId", "tagValue", "createdDate")]
  setnames(out, "tagValue", "url")
  out <- unique(out, by = c("cacheId", "url"))
  for (nm in intersect(which, names(.urlLogTagKeys)[-1])) {
    d <- pick(.urlLogTagKeys[[nm]])
    out[[nm]] <- d$tagValue[match(out$cacheId, d$cacheId)]
  }
  ## Per-url fields, and a per-url caller, from the reproducible.urlRecord tags
  recTags <- tags[tags$tagKey == "reproducible.urlRecord", c("cacheId", "tagValue")]
  if (any(c("caller", .urlLogRecordCols) %in% which)) {
    for (nm in intersect(which, .urlLogRecordCols)) out[[nm]] <- NA_character_
    if (NROW(recTags)) {
      recs <- rbindlist(.decodeUrlRecords(recTags$tagValue))
      recs$cacheId <- recTags$cacheId
      m <- match(paste(out$cacheId, out$url), paste(recs$cacheId, recs$url))
      has <- !is.na(m)
      for (nm in intersect(which, .urlLogRecordCols)) out[[nm]][has] <- recs[[nm]][m[has]]
      if ("caller" %in% which) out$caller[has] <- recs$fn[m[has]]
    }
  }
  if ("hitCount" %in% which) out$hitCount <- suppressWarnings(as.integer(out$hitCount))
  for (nm in intersect(which, c("firstSeen", "lastSeen")))
    out[[nm]] <- as.POSIXct(out[[nm]], tz = "")
  ls <- if ("lastSeen" %in% names(out)) out$lastSeen else
    as.POSIXct(pick("reproducible.urlLastSeen")$tagValue[
      match(out$cacheId, pick("reproducible.urlLastSeen")$cacheId)], tz = "")
  out <- out[order(ls, decreasing = TRUE, na.last = TRUE)]
  out[, which, with = FALSE]
}
