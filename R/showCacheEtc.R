utils::globalVariables(c(
  "..onCol"
))

#' @param x A simList or a directory containing a valid Cache repository. Note:
#'   For compatibility with `Cache` argument, `cachePath` can also be
#'   used instead of `x`, though `x` will take precedence.
#' @param after A time (POSIX, character understandable by data.table).
#'                  Objects cached after this time will be shown or deleted.
#' @param before A time (POSIX, character understandable by data.table).
#'                   Objects cached before this time will be shown or deleted.
#' @param fun An optional character vector describing the function name to extract.
#'   Only functions with this/these functions will be returned.
#' @param cacheId An optional character vector describing the `cacheId`s to extract.
#'   Only entries with this/these `cacheId`s will be returned. If `useDBI(FALSE)`,
#'   this will also be dramatically faster than using `userTags`, for a large
#'   cache.
#'
#' @param ask Logical. If `FALSE`, then it will not ask to confirm deletions using
#'            `clearCache` or `keepCache`. Default is `TRUE`
#' @param ... Other arguments. Can be in the form of `tagKey = tagValue`, such as,
#'            `class = "numeric"` to find all entries that are numerics in the cache.
#'            Note: the special cases of `cacheId` and `fun` have their own
#'            named arguments in these functions.
#'            Also can be `regexp = xx`, where `xx` is `TRUE` if the user
#'            is passing a regular expression.
#'            Otherwise, `userTags` will need to be exact matches. Default is
#'            missing, which is the same as `TRUE`. If there are errors due
#'            to regular expression problem, try `FALSE`. For `cc`, it is
#'            passed to `clearCache`, e.g., `ask`, `userTags`. For `showCache`,
#'            it can also be `sorted = FALSE` to return the object unsorted.
#' @param userTags Character vector. If used, this will be used in place of the
#'                 `after` and `before`.
#'                 Specifying one or more `userTag` here will clear all
#'                 objects that match those tags.
#'                 Matching is via regular expression, meaning partial matches
#'                 will work unless strict beginning (`^`) and end (`$`) of string
#'                 characters are used.
#'                 Matching will be against any of the 3 columns returned by `showCache()`,
#'                 i.e., `artifact`, `tagValue` or `tagName`.
#'                 Also, if `length(userTags) > 1`, then matching is by `and`.
#'                 For `or` matching, use `|` in a single character string.
#'                 See examples.
#' @param useCloud Logical. If `TRUE`, then every object that is deleted locally will
#'    also be deleted in the `cloudFolderID`, if it is non-`NULL`
#'
#' @inheritParams Cache
#'
#' @details
#' If neither `after` or `before` are provided, nor `userTags`,
#' then all objects will be removed.
#' If both `after` and `before` are specified, then all objects between
#' `after` and `before` will be deleted.
#' If `userTags` is used, this will override `after` or `before`.
#'
#' @return Will clear all objects (or those that match `userTags`, or those
#' between `after` or `before`) from the repository located in
#' `cachePath`.
#' Invisibly returns a `data.table` of the removed items.
#'
#' @note If the cache is larger than 10MB, and clearCache is used, there will be
#' a message and a pause, if interactive, to prevent accidentally deleting of a
#' large cache repository.
#'
#' @export
#' @importFrom data.table setindex
#' @importFrom methods setGeneric setMethod
#' @importFrom utils object.size
#' @name showCache
#' @aliases clearCache
#' @rdname viewCache
#'
#' @examples
#' data.table::setDTthreads(2)
#'
#' tmpDir <- file.path(tempdir(), "reproducible_examples", "Cache")
#' try(clearCache(tmpDir, ask = FALSE), silent = TRUE) # just to make sure it is clear
#'
#' # Basic use
#' ranNumsA <- Cache(rnorm, 10, 16, cachePath = tmpDir)
#'
#' # All same
#' ranNumsB <- Cache(rnorm, 10, 16, cachePath = tmpDir) # recovers cached copy
#' ranNumsD <- Cache(quote(rnorm(n = 10, 16)), cachePath = tmpDir) # recovers cached copy
#'
#' # Any minor change makes it different
#' ranNumsE <- Cache(rnorm, 10, 6, cachePath = tmpDir) # different
#'
#' ## Example 1: basic cache use with tags
#' ranNumsA <- Cache(rnorm, 4, cachePath = tmpDir, userTags = "objectName:a")
#' ranNumsB <- Cache(runif, 4, cachePath = tmpDir, userTags = "objectName:b")
#' ranNumsC <- Cache(runif, 40, cachePath = tmpDir, userTags = "objectName:b")
#'
#' showCache(tmpDir, userTags = c("objectName"))
#' showCache(tmpDir, userTags = c("^a$")) # regular expression ... "a" exactly
#'
#' # Fine control of cache elements -- pick out only the large runif object, and remove it
#' cache1 <- showCache(tmpDir, userTags = c("runif")) # show only cached objects made during runif
#' toRemove <- cache1[tagKey == "object.size"][as.numeric(tagValue) > 700]$cacheId
#' clearCache(tmpDir, userTags = toRemove, ask = FALSE)
#' cacheAfter <- showCache(tmpDir, userTags = c("runif")) # Only the small one is left
#'
setGeneric("clearCache", function(x, userTags = character(), after = NULL, before = NULL,
                                  fun = NULL, cacheId = NULL,
                                  ask = getOption("reproducible.ask"),
                                  useCloud = FALSE,
                                  cloudFolderID = getOption("reproducible.cloudFolderID", NULL),
                                  drv = getDrv(getOption("reproducible.drv", NULL)),
                                  conn = getOption("reproducible.conn", NULL),
                                  verbose = getOption("reproducible.verbose"),
                                  ...) {
  standardGeneric("clearCache")
})

#' @export
#' @rdname viewCache
setMethod(
  "clearCache",
  definition = function(x, userTags, after = NULL, before = NULL,
                        fun = NULL, cacheId = NULL,
                        ask, useCloud = FALSE,
                        cloudFolderID = getOption("reproducible.cloudFolderID", NULL),
                        drv = getDrv(getOption("reproducible.drv", NULL)),
                        conn = getOption("reproducible.conn", NULL),
                        verbose = getOption("reproducible.verbose"),
                        ...) {
    # isn't clearing the raster backed file
    if (missing(x)) {
      x <- if (!is.null(list(...)$cachePath)) {
        messageCache("x not specified, but cachePath is; using ", list(...)$cachePath, verbose = verbose)
        list(...)$cachePath
      } else {
        messageCache("x not specified; using ", getOption("reproducible.cachePath")[1], verbose = verbose)
        x <- getOption("reproducible.cachePath")[1]
      }
    }

    dots <- list(...)
    hasNoOther <- (length(dots)) == 0 | is.null(dots[!names(dots) %in% sortedOrRegexp])

    # Check if no args -- faster to delete all then make new empty repo for large repos
    clearWholeCache <- all(missing(userTags), is.null(after), is.null(before),
                           is.null(fun), is.null(cacheId), isTRUE(hasNoOther))

    if (isTRUEorForce(useCloud) || !clearWholeCache) {
      if (isTRUEorForce(useCloud)) {
        .requireNamespace("googledrive", stopOnFALSE = TRUE, messageStart = "to use google drive files")
      }

      # browser(expr = exists("._clearCache_2"))
      # if (missing(after)) after <- NA # "1970-01-01"
      # if (missing(before)) before <- NA # Sys.time() + 1e5

      args <- append(
        list(x = x, after = after, before = before, userTags = userTags,
             fun = fun, cacheId = cacheId, sorted = FALSE, verbose = verbose),
        list(...)
      )

      objsDT <- do.call(showCache, args = args, quote = TRUE)
      if (isTRUE(useCloud) && NROW(objsDT) > 0 || identical(useCloud, "force")) {
        cacheIds <- unique(objsDT[[.cacheTableHashColName()]])
        fns <- Filenames(objsDT)
        rmFromCloudFolder(cloudFolderID, x, cacheIds, otherFiles = fns, verbose = verbose)
      }
    }

    # browser(expr = exists("rrrr"))
    # if (useDBI()) {
    if (!CacheIsACache(x, drv = drv, conn = conn)) {
      return(.emptyCacheTable)
    }
    # }

    if (clearWholeCache) {
      if (isInteractive()) {
        if (isTRUE(ask)) {
          cacheSize <- sum(file.size(dir(x, full.names = TRUE, recursive = TRUE)))
          class(cacheSize) <- "object_size"
          formattedCacheSize <- format(cacheSize, "auto")
          messageQuestion(
            "Your current cache size is ", formattedCacheSize, ".\n",
            " Are you sure you would like to delete it all? Y or N"
          )
          rl <- readline()
          if (!identical(toupper(rl), "Y")) {
            messageCache("Aborting clearCache", verbose = verbose)
            return(invisible())
          }
        }
      }

      unlink(CacheStorageDir(x), recursive = TRUE)

      if (useDBI()) {
        unlink(file.path(x, "rasters"), recursive = TRUE)
        unlink(CacheDBFile(x, drv = drv, conn = conn), recursive = TRUE, force = TRUE)
      }

      checkPath(x, create = TRUE)
      # if (useDBI()) {
      createCache(x, drv = drv, force = TRUE)
      # }
      if (isTRUE(getOption("reproducible.useMemoise"))) {
        objsInMemEnv <- ls(memoiseEnv(x))
        if (length(objsInMemEnv)) {
          rm(list = objsInMemEnv, envir = memoiseEnv(x))
        }
      }
      # memoise::forget(.loadFromLocalRepoMem)
      return(invisible())
    }

    if (isInteractive()) {
      objSz <- objsDT[tagKey == "object.size"][[.cacheTableTagColName()]]
      objSizes <- if ("NA" %in% objSz) NA else as.numeric(objSz)
      cacheSize <- sum(objSizes) / 4
    }

    if (NROW(objsDT)) {
      filesToRemove1 <- objsDT[grepl(pattern = "cacheRaster", tagKey)][[.cacheTableTagColName()]]
      filesToRemove2 <- objsDT[grepl(pattern = "origFilename|filesToLoad|filenamesInCache", tagKey)][[.cacheTableTagColName()]]
      filesToRemove2 <- normPath(file.path(CacheStorageDir(x), basename(filesToRemove2)))
      filesToRemove3 <- dir(CacheStorageDir(x), full.names = TRUE)

      # Way faster to gsub for the cacheId, rather than greps
      cacheIdsOfTheseFilenames <- gsub("^.*/([0-9a-zA-Z]+)\\..*$", "\\1", filesToRemove3)
      cacheIdsToRm <- unique(objsDT[["cacheId"]])
      indicesToRm <- which(cacheIdsOfTheseFilenames %in% cacheIdsToRm)
      filesToRemove4 <- filesToRemove3[indicesToRm]

      # grep can only handle so many -- do in groups
      # filesToRemove3 <- grep(paste(cacheIdsToRm, collapse = "|"), filesToRemove3, value = TRUE)
      # maxNumForGrep <- 20
      # sequen <- seq(cacheIdsToRm)
      # if (length(cacheIdsToRm) > maxNumForGrep) {
      #   groups <- cut(sequen, breaks = ceiling(length(cacheIdsToRm) / maxNumForGrep))
      #   groups <- split(sequen, groups)
      # } else {
      #   groups <- list(sequen)
      # }
      #
      # filesToRemove4 <- lapply(groups, function(g) {
      #   cis <- cacheIdsToRm[sequen[g]] |> unlist()
      #   grep(paste(cis, collapse = "|"), filesToRemove3, value = TRUE)
      # }) |> unlist() |> unname()

      filesToRemove <- unique(c(filesToRemove1, filesToRemove2, filesToRemove4))
      # filebackedInRepo <- objsDT[grepl(pattern = "fromDisk", tagKey) &
      #                           grepl(pattern = "TRUE", get(.cacheTableTagColName()))]
      #
      # rastersInRepo <- objsDT[grepl(pattern = "class", tagKey) &
      #                           grepl(pattern = "Raster", get(.cacheTableTagColName()))]
      # listsInRepo <-  objsDT[grepl(pattern = "class", tagKey) &
      #                          grepl(pattern = "list", get(.cacheTableTagColName()))]
      # hasARaster <- all(!is.na(rastersInRepo[[.cacheTableHashColName()]])) && NROW(rastersInRepo) > 0 # nolint
      # hasAList <- all(!is.na(rastersInRepo[[.cacheTableHashColName()]])) && NROW(listsInRepo) > 0 # nolint

      if (NROW(filesToRemove)) {
        # fileBackedRastersInRepo <- filebackedInRepo[[.cacheTableHashColName()]]# [rasterObjSizes < 1e5]
        # if (NROW(fileBackedRastersInRepo)) {
        filesToRemove <- unlist(filesToRemove)
        if (isInteractive()) {
          dirLs <- dir(unique(dirname(filesToRemove)), full.names = TRUE)
          filesToRemove <- dirLs[which(basename(dirLs)  %in% basename(filesToRemove))]
          # dirLs <- unlist(lapply(basename(filesToRemove), grep, dirLs, value = TRUE))
          # filesToRemove <- unique(c(filesToRemove, dirLs))
          cacheSize <- sum(file.size(filesToRemove))
        }
        # }
      }

      if (isInteractive() && isTRUE(!is.na(cacheSize))) {
        class(cacheSize) <- "object_size"
        formattedCacheSize <- format(cacheSize, "auto")
        if (isTRUE(ask)) {
          messageQuestion(
            "Your size of your selected objects (including file-backed objects) is ",
            formattedCacheSize, ".\n",
            " Are you sure you would like to delete it all? Y or N"
          )
          rl <- readline()
          if (!identical(toupper(rl), "Y")) {
            messageCache("Aborting clearCache", verbose = verbose)
            return(invisible())
          }
        }
      }

      # remove file-backed files
      if (NROW(filesToRemove) > 0) {
        unlink(filesToRemove)
      }

      objToGet <- unique(objsDT[[.cacheTableHashColName()]])
      if (useDBI()) {
        if (is.null(conn)) {
          conn <- dbConnectAll(drv, cachePath = x, create = FALSE)
          on.exit({
            DBI::dbDisconnect(conn)
          })
        }
      }
      rmFromCache(x, objToGet, conn = conn, drv = drv, verbose = verbose) # many = TRUE)
      if (isTRUE(getOption("reproducible.useMemoise"))) {
        exist <- vapply(objToGet, exists, envir = memoiseEnv(x), FUN.VALUE = logical(1))
        if (isTRUE(any(exist))) {
          suppressWarnings(rm(list = objToGet[exist], envir = memoiseEnv(x)))#.pkgEnv[[x]]))
        }
      }
    }
    try(setindex(objsDT, NULL), silent = TRUE)
    return(invisible(objsDT))
  }
)

#' @details
#' `cc(secs)` is just a shortcut for `clearCache(repo = currentRepo, after = secs)`,
#' i.e., to remove any cache entries touched in the last `secs` seconds. Since, `secs`
#' can be missing, this is also be a shorthand for "remove most recent entry from
#' the cache".
#'
#' @param secs Currently 3 options: the number of seconds to pass to `clearCache(after = secs)`,
#'     a `POSIXct` time e.g., from `Sys.time()`, or missing. If missing,
#'             the default, then it will delete the most recent entry in the Cache.
#'
#' @export
#' @rdname viewCache
#'
#' @examples
#' data.table::setDTthreads(2)
#' tmpDir <- file.path(tempdir(), "reproducible_examples", "Cache")
#' try(clearCache(tmpDir, ask = FALSE), silent = TRUE) # just to make sure it is clear
#'
#' Cache(rnorm, 1, cachePath = tmpDir)
#' thisTime <- Sys.time()
#' Cache(rnorm, 2, cachePath = tmpDir)
#' Cache(rnorm, 3, cachePath = tmpDir)
#' Cache(rnorm, 4, cachePath = tmpDir)
#' showCache(x = tmpDir) # shows all 4 entries
#' cc(ask = FALSE, x = tmpDir)
#' showCache(x = tmpDir) # most recent is gone
#' cc(thisTime, ask = FALSE, x = tmpDir)
#' showCache(x = tmpDir) # all those after thisTime gone, i.e., only 1 left
#' cc(ask = FALSE, x = tmpDir) # Cache is
#' cc(ask = FALSE, x = tmpDir) # Cache is already empty
cc <- function(secs, ..., verbose = getOption("reproducible.verbose")) {
  if (missing(secs)) {
    messageCache("No time provided; removing the most recent entry to the Cache",
                 verbose = verbose
    )
    suppressMessages({
      theCache <- reproducible::showCache(...)
    })
    if (NROW(theCache) > 0) {
      accessed <- data.table::setkey(theCache[tagKey == "accessed"], tagValue)
      clearCache(userTags = tail(accessed, 1)[[.cacheTableHashColName()]], ...)
    } else {
      messageCache("Cache already empty", verbose = verbose)
    }
  } else {
    if (is(secs, "POSIXct")) {
      reproducible::clearCache(after = secs, ...)
    } else {
      reproducible::clearCache(after = Sys.time() - secs, ...)
    }
  }
}

#' Examining and modifying the cache
#'
#' These are convenience wrappers around `DBI` package functions.
#' They allow the user a bit of control over what is being cached.
#'
#' \describe{
#'   \item{`clearCache`}{remove items from the cache based on their
#'                            `userTag` or `times` values.}
#'   \item{`keepCache`}{remove all cached items *except* those based on
#'                           certain `userTags` or `times` values.}
#'   \item{`showCache`}{display the contents of the cache.}
#' }
#'
#' @details
#' By default the return of `showCache` is sorted by `cacheId`. For convenience,
#' a user can optionally have it unsorted (passing `sorted = FALSE`),
#' which may be noticeably faster when
#' the cache is large (`> 1e4` entries).
#'
#' @inheritParams clearCache
#' @inheritParams Cache
#' @export
#' @importFrom data.table data.table set setkeyv
#' @rdname viewCache
#' @name showCache
#' @seealso [mergeCache()]. Many more examples in [Cache()].
#'
setGeneric("showCache", function(x, userTags = character(), after = NULL, before = NULL,
                                 fun = NULL, cacheId = NULL,
                                 # cacheSaveFormat = getOption("reproducible.cacheSaveFormat"),
                                 drv = getDrv(getOption("reproducible.drv", NULL)),
                                 conn = getOption("reproducible.conn", NULL),
                                 verbose = getOption("reproducible.verbose"),
                                 ...) {
  standardGeneric("showCache")
})

#' @export
#' @rdname viewCache
setMethod(
  "showCache",
  definition = function(x, userTags, after = NULL, before = NULL, fun = NULL,
                        cacheId = NULL, # cacheSaveFormat = getOption("reproducible.cacheSaveFormat"),
                        drv, conn, ...) {
    # browser(expr = exists("rrrr"))
    if (missing(x)) {
      messageCache("x not specified; using ", getOption("reproducible.cachePath")[1], verbose = verbose)
      x <- getOption("reproducible.cachePath")[1]
    }
    # browser(expr = exists("jjjj"))
    # if (useDBI()) {
    afterNA <- FALSE
    if (is.null(after)) {
      afterNA <- TRUE
      after <- NA
    }
    # "1970-01-01"
    beforeNA <- FALSE
    if (is.null(before)) {
      beforeNA <- TRUE
      before <- NA
    } # Sys.time() + 1e5
    # } else {
    #   if (is.null(after)) after <- "1970-01-01"
    #   if (is.null(before)) before <- Sys.time() + 1e5
    # }

    # not seeing userTags
    # Clear the futures that are resolved
    .onLinux <- .Platform$OS.type == "unix" && unname(Sys.info()["sysname"]) == "Linux" &&
      !isFALSE(getOption("reproducible.futurePlan"))
    if (.onLinux) {
      if (exists("futureEnv", envir = .reproEnv)) {
        hasFuture <- .requireNamespace("future",
                                       messageStart = "To use reproducible.futurePlan, "
        )
        if (hasFuture) {
          checkFutures(verbose)
        }
      }
    }

    if (!useDBI()) {
      pkgEnv <- memoiseEnv(cachePath = x)
      if (!exists("shownCache", envir = pkgEnv))
        pkgEnv[["shownCache"]] <- new.env()
      if (!exists(x, envir = pkgEnv[["shownCache"]]))
        pkgEnv[["shownCache"]][[x]] <- new.env()

      ## Note: do NOT lazy-spawn from here. The async fork itself runs
      ## showCache(); spawning again from inside the fork would recurse
      ## (the fork's pkgEnv has no job entry yet because spawn_showCache_async
      ## assigns the job AFTER mcparallel returns). Cache() is the lazy-spawn
      ## site; direct showCache() callers can use prepopulateCacheAsync()
      ## explicitly if they want a warm fork.

      # Non-blocking poll: if the async pre-load job has already finished,
      # harvest it; otherwise proceed synchronously.  Never block here — for
      # large caches the fork can take minutes, and blocking would defeat the
      # purpose of having the incremental-update mechanism below.
      collect_showCache_async(x, wait = FALSE, timeout = 0)
      scEnv <- pkgEnv[["shownCache"]][[x]]

      # periodically, a cache entry is corrupt; this while, tryCatch will remove the corrupt file and restart
      objsDT <- list()
      while(is(objsDT, "list")) {
        # filOutside <- character()
        objsDT <- tryCatch2(
          if (!is.null(cacheId)) {
            objsDT <- rbindlist(fill = TRUE, lapply(cacheId, function(fil) {
              showCacheFast(fil, cachePath = x,
                            drv = drv, conn = conn)
            }))
          } else {
            dd <- dir(CacheStorageDir(x),
                      pattern = paste(CacheDBFileSingleExt(cacheSaveFormat = .cacheSaveFormats), collapse = "|"),
                      full.names = TRUE
            )
            lapplyFun <- lapply
            curFileInfo <- file.info(dd) |> setDT(keep.rownames = "filename")

            # Compare only on stable, content-relevant columns.
            # Joining on all file.info columns (including atime) caused every
            # file to appear "new" on every call because loadFile() updates atime.
            stableCols <- c("filename", "mtime", "size")

            if (is.null(scEnv$FileInfo)) {
              newOnes <- curFileInfo
            } else {
              newOnes  <- curFileInfo[!scEnv$FileInfo,  on = stableCols]
              removeOnes <- scEnv$FileInfo[!curFileInfo, on = "filename"]
              if (NROW(removeOnes)) {
                scEnv$FileInfo <- scEnv$FileInfo[!removeOnes, on = "filename"]
                cis <- filePathSansExt(filePathSansExt(basename(removeOnes$filename)))
                scEnv$sc <- scEnv$sc[!cacheId %in% cis]
              }
            }
            dd <- newOnes[["filename"]]
            ddOrig <- dd
            # If a file's mtime changed (e.g. a cache hit rewrote the accessed
            # tag), it appears in newOnes even though it still exists.  Without
            # this purge, the old entry and the freshly-loaded one would both be
            # present in scEnv$sc, causing duplicate cacheIds and a cartesian
            # join error in the userTags filter below.
            if (!is.null(scEnv$sc) && length(dd) > 0) {
              cisToRemove <- filePathSansExt(filePathSansExt(basename(dd)))
              scEnv$sc <- scEnv$sc[!cacheId %in% cisToRemove]
            }
            scEnv$FileInfo <- curFileInfo

            keepDoing <- TRUE
            while(keepDoing) {
              allFilesLoaded <- lapplyFun(dd, function(fil) {

                ## Wrap loadFile in try(): readRDS / qs_read can throw on
                ## corrupt or wrong-format files (e.g. "unknown input format"
                ## from readRDS when the .rds extension lies). The recovery
                ## branch below expects out to be a try-error, so without
                ## the wrap the error escapes the loop and the whole
                ## showCache() call.
                out <- try(loadFile(fil,
                                    cachePath = x, # in case it needs swapCacheFormat
                                    drv = drv, conn = conn, verbose = verbose),
                           silent = TRUE)
                if (is(out, "try-error")) {
                  cacheId <- gsub(paste0(CacheDBFileSingleExt()), "",
                                  basename(fil))

                  fileEx <- fileExt(fil)
                  fileExs <- setdiff(.cacheSaveFormats, fileEx)
                  for (fe in fileExs) {
                    out <- try(loadFile(fil, format = fe,
                                        cacheId = cacheId, cachePath = x, # in case it needs swapCacheFormat
                                        drv = drv, conn = conn, verbose = verbose),
                               silent = TRUE)
                    if (!is(out, "try-error")) {
                      if (identical(getOption("reproducible.cacheSaveFormat"), .qsFormat))
                        optForUndo <- options("reproducible.qsFormat" = .qsFormat)
                      on.exit(options(optForUndo), add = TRUE)
                      saveFilesInCacheFolder(out, fts = fil, cachePath = x,
                                             cacheId = cacheId)

                      return(out)
                    }
                  }
                  # browser()
                  filesToRm <- dir(dirname(fil), pattern = cacheId, full.names = TRUE)
                  messageCache("The database file was corrupt; deleting Cache entry for ", cacheId,
                               verbose = getOption("reproducible.verbose"))
                  unlink(filesToRm)
                  # Return NULL (not the try-error) so rbindlist(fill = TRUE)
                  # below silently skips this entry. Letting the try-error
                  # escape made rbindlist error with "Item N is not a list",
                  # the catch handler didn't successfully retry, and the
                  # whole showCache() call returned NULL — which broke
                  # test-showCacheCorruptFile.R's NROW(out) > 0 invariant
                  # (valid sibling entries should still be returned).
                  return(NULL)
                }
                out
              })
              allFilesLoaded <- Filter(Negate(is.null), allFilesLoaded)


              ret <- tryCatch(

                rbindlist(fill = TRUE, allFilesLoaded), error = function(err) {
                if (any(grepl("Item .+ is not a", err$message))) {
                  toDel <- gsub("Item ([[:digit:]]+) of.+", "\\1", err$message) |> as.numeric()
                  unlink(dd[toDel])
                  dd <- dd[-toDel]
                }
                # browser()
                if (any(grepl("use qs::qread", err$message))) {
                  swapCacheFileFormat()
                }
                # next
              })
              #if (is(ret, "try-error"))
              #  browser()
              keepDoing <- FALSE
            }

            if (!is.null(scEnv$sc)) {
              ret <- rbindlist(list(scEnv$sc, ret))
            }
            scEnv$sc <- ret
            ret

          }# , error = function(e) {
          #   cacheId <- gsub(paste0(CacheDBFileSingleExt(), "|", cacheSaveFormat), "",
          #                   basename(file))
          #   filesToRm <- dir(dirname(file), pattern = cacheId, full.names = TRUE)
          #   messageCache("The database file was corrupt; deleting Cache entry for ", cacheId,
          #                   verbose = getOption("reproducible.verbose"))
          #   unlink(filesToRm)
          # }
        )
      }
      if (NROW(objsDT) == 0) {
        return(.emptyCacheTable)
      }
    } else {
      if (is.null(conn)) {
        conn <- dbConnectAll(drv, cachePath = x, create = FALSE)
        if (is.null(conn)) {
          return(.emptyCacheTable)
        }
        on.exit(DBI::dbDisconnect(conn), add = TRUE)
      }
      if (!CacheIsACache(x, drv = drv, conn = conn)) {
        return(.emptyCacheTable)
      }

      dbTabNam <- CacheDBTableName(x, drv = drv)
      # tab <- dbReadTable(conn, dbTabNam)
      res <- retry(retries = 250, exponentialDecayBase = 1.01, quote(
        DBI::dbSendQuery(conn, paste0("SELECT * FROM \"", dbTabNam, "\""))
      ))
      tab <- DBI::dbFetch(res)
      DBI::dbClearResult(res)
      if (is(tab, "try-error")) {
        objsDT <- .emptyCacheTable
      } else {
        objsDT <- setDT(tab)
      }
      if (!is.null(fun)) {

      }
    }

    onCol <- "cacheId"
    if (!is.null(cacheId)) {
      cacheIds <- cacheId
      objsDT <- objsDT[unique(objsDT[cacheId %in% cacheIds, ..onCol]), on = onCol]
    }
    if (!is.null(fun)) {
      objsDT <- objsDT[objsDT[tagKey %in% "function" & tagValue %in% fun, ..onCol], on = onCol]
    }
    dots <- list(...)

    dots <- dots[!names(dots) %in% sortedOrRegexp]
    if (length(dots)) {
      names(dots) <- gsub("^Function$", "function", names(dots)) # in case user uses Function instead of "function"
      Map(nam = names(dots), val = dots, function(nam, val) {
        objsDT <<- objsDT[objsDT[tagKey %in% nam & tagValue %in% val, ..onCol], on = onCol]
      })

    }
    sorted <- !isFALSE(list(...)$sorted) # NULL and TRUE are sorted
    if (isTRUE(sorted) && NROW(objsDT)) {
      data.table::setorderv(objsDT, onCol)
    }
    # }

    if (NROW(objsDT) > 0) {
      if (!afterNA || !beforeNA) {
        objsDT3 <- objsDT[tagKey == "accessed"]
        if (!beforeNA) {
          objsDT3 <- objsDT3[(tagValue <= before)]
        }
        if (!afterNA) {
          objsDT3 <- objsDT3[(tagValue >= after)]
        }
        objsDT <- objsDT[objsDT[[.cacheTableHashColName()]] %in%
                           unique(objsDT3[[.cacheTableHashColName()]])] # faster than data.table join
      }
      if (length(userTags) > 0) {
        if (isTRUE(list(...)$regexp) | is.null(list(...)$regexp)) {
          objsDTs <- list()
          for (ut in userTags) {
            if (grepl(":", ut)) {
              ut <- paste(strsplit(ut, ":")[[1]], collapse = "|")
            }
            objsDT2 <- objsDT[
              grepl(get(.cacheTableTagColName()), pattern = ut) |
                grepl(tagKey, pattern = ut) |
                grepl(get(.cacheTableHashColName()), pattern = ut)
            ]
            setkeyv(objsDT2, .cacheTableHashColName())
            shortDT <- unique(objsDT2, by = .cacheTableHashColName())[, get(.cacheTableHashColName())]
            # }
            objsDT <- if (NROW(shortDT)) objsDT[shortDT, on = .cacheTableHashColName()] else objsDT[0] # merge each userTags
          }
        } else {
          # if (useDBI()) {
          objsDT2 <- objsDT[cacheId %in% userTags | tagKey %in% userTags | tagValue %in% userTags]
          setkeyv(objsDT2, onCol)
          shortDT <- unique(objsDT2, by = onCol)[, cacheId]
          objsDT <- if (NROW(shortDT)) objsDT[shortDT, on = .cacheTableHashColName()] else objsDT[0] # merge each userTags
          # } else {
          #   objsDT2 <- objsDT[artifact %in% userTags | tagKey %in% userTags | tagValue %in% userTags]
          #   setkeyv(objsDT2, "artifact")
          #   shortDT <- unique(objsDT2, by = "artifact")[, artifact]
          #   objsDT <- if (NROW(shortDT)) objsDT[shortDT, on = .cacheTableHashColName()] else objsDT[0] # merge each userTags
          # }
        }
      }
    }
    .message$CacheSize(x,
                       artifacts = unique(objsDT[[.cacheTableHashColName()]]),
                       cacheTable = objsDT, verbose = verbose
    )
    return(objsDT)
  }
)

#' @rdname viewCache
setGeneric("keepCache", function(x, userTags = character(), after = NULL, before = NULL,
                                 ask = getOption("reproducible.ask"),
                                 drv = getDrv(getOption("reproducible.drv", NULL)),
                                 conn = getOption("reproducible.conn", NULL),
                                 verbose = getOption("reproducible.verbose"),
                                 ...) {
  standardGeneric("keepCache")
})

#' @export
#' @rdname viewCache
setMethod(
  "keepCache",
  definition = function(x, userTags, after, before, ask, drv, conn,
                        verbose = getOption("reproducible.verbose"),
                        ...) {
    if (missing(x)) {
      messageCache("x not specified; using ", getOption("reproducible.cachePath")[1], verbose = verbose)
      x <- getOption("reproducible.cachePath")[1]
    }
    args <- append(list(x = x, after = after, before = before, userTags = userTags),
                   modifyList(list(...), list(verbose = FALSE)))

    objsDTAll <- suppressMessages(showCache(x, verbose = FALSE, sorted = FALSE))
    objsDT <- do.call(showCache, args = args)
    keep <- unique(objsDT[[.cacheTableHashColName()]])

    eliminate <- unique(objsDTAll[[.cacheTableHashColName()]][
      !(objsDTAll[[.cacheTableHashColName()]] %in% keep)
    ])

    if (length(eliminate)) {
      clearCache(x, cacheId = eliminate, verbose = FALSE, regexp = FALSE, ask = ask)
    } else {
      messageCache("Nothing to remove; keeping all")
    }
    return(objsDT)
  }
)

#' Merge two cache repositories together
#'
#' \if{html}{\figure{lifecycle-experimental.svg}{options: alt="experimental"}}
#'
#' All the `cacheFrom` artifacts will be put into `cacheTo`
#' repository. All `userTags` will be copied verbatim, including
#' `accessed`, with 1 exception: `date` will be the
#' current `Sys.time()` at the time of merging. The
#' `createdDate` column will be similarly the current time
#' of merging.
#'
#' @param cacheTo The cache repository (character string of the file path)
#'                that will become larger, i.e., merge into this
#' @param cacheFrom The cache repository (character string of the file path)
#'                  from which all objects will be taken and copied from
#' @param drvTo The database driver for the `cacheTo`.
#' @param drvFrom The database driver for the `cacheFrom`
#' @param connTo The connection for the `cacheTo`. If not provided, then
#'   a new one will be made from `drvTo` and `cacheTo`
#' @param connFrom The database for the `cacheFrom`. If not provided, then
#'   a new one will be made from `drvFrom` and `cacheFrom`
#'
#' @return The character string of the path of `cacheTo`, i.e., not the
#' objects themselves.
#' @inheritParams Cache
setGeneric("mergeCache", function(cacheTo, cacheFrom,
                                  drvTo = getDrv(getOption("reproducible.drv", NULL)),
                                  drvFrom = getDrv(getOption("reproducible.drv", NULL)),
                                  connTo = NULL, connFrom = NULL,
                                  verbose = getOption("reproducible.verbose")) {
  standardGeneric("mergeCache")
})

#' @export
#' @rdname mergeCache
setMethod(
  "mergeCache",
  definition = function(cacheTo, cacheFrom, drvTo, drvFrom, connTo, connFrom,
                        verbose = getOption("reproducible.verbose")) {

    if (useDBI()) {
      if (is.null(connTo)) {
        connTo <- dbConnectAll(drvTo, cachePath = cacheTo)
        on.exit(DBI::dbDisconnect(connTo), add = TRUE)
      }

      if (is.null(connFrom)) {
        connFrom <- dbConnectAll(drvFrom, cachePath = cacheFrom)
        on.exit(DBI::dbDisconnect(connFrom), add = TRUE)
      }
    }

    suppressMessages({
      cacheFromList <- showCache(cacheFrom, drv = drvFrom, conn = connFrom, sorted = FALSE)
    })
    suppressMessages({
      cacheToList <- showCache(cacheTo, drv = drvTo, conn = connTo, sorted = FALSE)
    })

    artifacts <- unique(cacheFromList[[.cacheTableHashColName()]])
    objectList <- lapply(artifacts, function(artifact) {
      # browser(expr = exists("gggg"))

      if (!(artifact %in% cacheToList[[.cacheTableHashColName()]])) {
        outputToSave <- # if (useDBI()) {
          try(loadFromCache(
            cachePath = cacheFrom, fullCacheTableForObj = cacheToList,
            cacheId = artifact, verbose = verbose
          ))
        if (is(outputToSave, "try-error")) {
          messageCache("Continuing to load others", verbose = verbose)
          outputToSave <- NULL
        }

        ## Save it
        userTags <- cacheFromList[artifact, on = .cacheTableHashColName()][
          !tagKey %in% c("format", "name", "date"), list(tagKey, tagValue)
        ]
        outputToSave <- .wrap(outputToSave, cachePath = cacheTo, drv = drvTo, conn = connTo)
        output <- saveToCache(cacheTo,
                              userTags = userTags, obj = outputToSave, cacheId = artifact,
                              drv = drvTo, conn = connTo
        ) # nolint
        messageCache(artifact, " copied", verbose = verbose)
        outputToSave
      } else {
        messageCache("Skipping ", artifact, "; already in ", cacheTo, verbose = verbose)
      }
    })

    .message$CacheSize(cacheTo, cacheTable = showCache(cacheTo, sorted = FALSE), verbose = verbose)

    return(invisible(cacheTo))
  }
)


#' @keywords internal
#' @inheritParams Cache
checkFutures <- function(verbose = getOption("reproducible.verbose")) {
  # This takes a long time -- can't use it if
  resol1 <- FALSE
  count <- 0
  lsFutureEnv <- ls(.reproEnv$futureEnv)

  anyFutureWrite <- length(lsFutureEnv)

  if (anyFutureWrite > 0) {
    # objsInReproEnv <- ls(.reproEnv)
    # objsInReproEnv <- grep("^future|cloudCheckSums", objsInReproEnv, value = TRUE)
    while (any(!resol1)) {
      count <- count + 1
      # numSleeps <<- numSleeps+1
      if (count > 1) {
        Sys.sleep(0.001)
        if (count > 1e3) {
          messageCache("Future is not resolved after 1 second of waiting. Allowing to proceed.",
                       verbose = verbose
          )
          break
        }
      }
      resol <- future::resolved(.reproEnv$futureEnv)
      resol1 <- resol[!startsWith(names(resol), "cloudCheckSums")]
    }
    if (length(resol) > 0) {
      .reproEnv$futureEnv[[lsFutureEnv]] <- NULL
    }
  }
}

useDBI <- function(set = NULL, verbose = getOption("reproducible.verbose"), default = TRUE) {
  canSwitch <- TRUE
  if (!is.null(set)) {
    if (isTRUE(set)) {
      canSwitch <- .requireNamespace("RSQLite", stopOnFALSE = FALSE) &&
        .requireNamespace("DBI", stopOnFALSE = FALSE)
    }
    if (isTRUE(canSwitch)) {
      options("reproducible.useDBI" = set)
    }
  }
  ud <- getOption("reproducible.useDBI", default)
  if (isTRUE(ud)) {
    drv <- getOption("reproducible.drv")
    if (is.null(drv)) {
      canSwitch <- .requireNamespace("RSQLite", stopOnFALSE = FALSE) &&
        .requireNamespace("DBI", stopOnFALSE = FALSE)
      if (isFALSE(canSwitch)) {
        options("reproducible.useDBI" = FALSE)
        ud <- getOption("reproducible.useDBI")
      }
    }
  }

  if (isFALSE(canSwitch)) {
    messageColoured("User has requested to use DBI as the backend, but DBI and/or RSQLite not ",
                    "installed.",
                    verboseLevel = 1, verbose = verbose
    )
  }
  if (!is.null(set)) {
    messSet <- if (isTRUE(ud)) {
      "Using DBI backend."
    } else {
      "Using non-DBI backend."
    }
    if (verbose > -1) {
      messageColoured(messSet, verboseLevel = 0, verbose = verbose)
    }
  }

  ud
}

#' @inheritParams Cache
rmFromCloudFolder <- function(cloudFolderID, x, cacheIds, otherFiles,
                              verbose = getOption("reproducible.verbose")) {
  if (is.null(cloudFolderID)) {
    cloudFolderID <- checkAndMakeCloudFolderID(cloudFolderID, cachePath = x)
  }

  whEmpty <- !nzchar(otherFiles)
  if (any(whEmpty)) {
    otherFiles <- otherFiles[!whEmpty]
  }
  grepToSrch <- c(cacheIds, otherFiles)
  gdriveLs <- driveLs(cloudFolderID, pattern = paste(grepToSrch, collapse = "|"))
  isInCloud <- lapply(grepToSrch, function(ci) startsWith(prefix = ci, gdriveLs$name))
  isInCloud <- Reduce(rbind, isInCloud)
  if (!is.null(dim(isInCloud))) {
    isInCloud <- apply(isInCloud, 2, any)
  }

  if (any(isInCloud)) {
    toDelete <- gdriveLs[isInCloud, ]
  }
  if (any(isInCloud)) {
    retry(quote(googledrive::drive_rm(toDelete)))
  }

  return(invisible())
}


isTRUEorForce <- function(cond) {
  isTRUE(cond) || identical(cond, "force")
}

showCacheFast <- function(cacheId, cachePath = getOption("reproducible.cachePath"),
                          dtFile, strict = TRUE, # cacheSaveFormat = getOption("reproducible.cacheSaveFormat"),
                          drv, conn, verbose = getOption("reproducible.verbose")) {

  if (missing(dtFile)) {
    # dtFile <- CacheDBFileSingle(cachePath, cacheId, cacheSaveFormat = "check")
    dtFile <- CacheDBFileSingle(cachePath, cacheId, cacheSaveFormat = "check")
    # dtFile <- dir(CacheStorageDir(cachePath), full.names = TRUE,
    #               pattern = paste0(cacheId, "\\", suffixMultipleDBFiles()))
  }
  fe <- file.exists(dtFile)
  sc <- NULL
  if (fe || isFALSE(strict)) {
    dtFile <- if (any(fe)) dtFile[fe][1] else character()
    if (length(dtFile)) {
      sc <- loadFile(dtFile,
                     cacheId = cacheId, cachePath = cachePath, # in case it needs swapCacheFormat
                     drv = drv, conn = conn, verbose = verbose) # , cacheSaveFormat = cacheSaveFormat)
    } else {
      sc <- showCache(cachePath, userTags = cacheId, drv = drv, conn = conn, verbose = FALSE)[cacheId %in% cacheId]
    }
  }
  sc[]
}

sortedOrRegexp <- c("sorted", "regexp", "ask")




# mcparallel is fork-based and not available on Windows
# pkgEnv <- reproducible:::pkgEnv()  # internal environment for package objects [3](https://rdrr.io/cran/reproducible/man/pkgEnv.html)
## Idempotent, all-guards-applied wrapper used by Cache() and showCache() to
## kick off a background showCache scan for `x` the first time the path is
## touched in a session. Cheap (~10us) when a job already exists -- safe to
## call from hot paths.
##
## Skipped silently on Windows (no fork), when parallel isn't available, or
## when `x` is NULL/non-character.
.maybeSpawnShowCacheAsync <- function(x = getOption("reproducible.cachePath")) {
  if (.Platform$OS.type == "windows") return(invisible(NULL))
  if (is.null(x) || !is.character(x) || !nzchar(x[[1L]])) return(invisible(NULL))
  if (!requireNamespace("parallel", quietly = TRUE)) return(invisible(NULL))
  ## spawn_showCache_async is itself idempotent via its overwrite=FALSE guard
  spawn_showCache_async(x[[1L]], silent = TRUE, overwrite = FALSE)
}

#' Pre-populate the in-memory `showCache` cache for a given `cachePath`
#'
#' Forks a background process that runs `showCache()` against `cachePath`;
#' subsequent `showCache()` / `Cache()->showSimilar()` calls in the same R
#' session can then harvest the result instead of re-scanning the cache
#' directory synchronously. Useful for very large caches (tens of thousands
#' of entries) where the cold first scan can take a minute or more.
#'
#' Idempotent: a second call with the same `cachePath` reuses the existing
#' job. Skipped silently on Windows (forking-based) and when the `parallel`
#' package isn't available.
#'
#' This helper is called automatically the first time `Cache()` or
#' `showCache()` is invoked against a given `cachePath`, so most users do
#' not need to call it explicitly. It is exported for workflows that want
#' to kick off the spawn early (e.g. inside `setupProject()`) so the fork
#' has more wall-clock time to complete before the first manual
#' `showCache()` call.
#'
#' @param cachePath A character path. Defaults to
#'   `getOption("reproducible.cachePath")`.
#' @return Invisibly returns the spawn job handle, or `NULL` if the spawn
#'   was skipped.
#' @export
prepopulateCacheAsync <- function(cachePath = getOption("reproducible.cachePath")) {
  invisible(.maybeSpawnShowCacheAsync(cachePath))
}

spawn_showCache_async <- function(
    x = getOption("reproducible.cachePath"),
    silent = TRUE,
    overwrite = FALSE
) {
  if (.Platform$OS.type == "windows") {
    return(NULL)
  }

  # Internal env for package state
  pkgEnv <- memoiseEnv(cachePath = x)
  if (!exists("shownCache", envir = pkgEnv))
    pkgEnv[["shownCache"]] <- new.env()
  if (!exists(x, envir = pkgEnv[["shownCache"]]))
    pkgEnv[["shownCache"]][[x]] <- new.env()

  if (is.null(pkgEnv[["shownCache"]]$shownCache_jobs)) pkgEnv[["shownCache"]]$shownCache_jobs <- new.env(parent = emptyenv())

  # If job exists and not overwriting, reuse it
  if (!overwrite && exists(x, envir = pkgEnv[["shownCache"]]$shownCache_jobs, inherits = FALSE)) {
    return(invisible(get(x, envir = pkgEnv[["shownCache"]]$shownCache_jobs, inherits = FALSE)))
  }

  # Capture the function objects in the *parent*.
  # This avoids any namespace loading inside the fork.
  ns <- asNamespace("reproducible")
  showCache_fun  <- get("showCache", envir = ns)
  memoiseEnv_fun <- get("memoiseEnv", envir = ns)

  # Build fork expression with injected function objects
  expr <- substitute({
    data.table::setDTthreads(1L)
    options(reproducible.nThreads = 1L)
    
    # Run slow call (side effects stay in child; we return the memoised object)
    SHOWCACHE(cp, drv = NULL, conn = NULL)
    
    # Harvest the memoised object created by showCache
    pkgEnv_child <- MEMOISEENV(cachePath = cp)
    sc <- pkgEnv_child[["shownCache"]][[cp]]

    # mcparallel/mccollect: NULL should not be returned (reserved as error signal) [1](https://www.r-bloggers.com/2023/06/dofuture-a-better-foreach-parallelization-operator-than-dopar/)
    if (is.null(sc)) {
      structure(list(error = "shownCache was NULL in child", cachePath = cp),
                class = "shownCache_error")
    } else {
      sc
    }
  }, list(
    cp = x,
    SHOWCACHE = showCache_fun,
    MEMOISEENV = memoiseEnv_fun
  ))

  job <- parallel::mcparallel(expr, name = paste0("showCache:", x), silent = silent)  # [1](https://www.r-bloggers.com/2023/06/dofuture-a-better-foreach-parallelization-operator-than-dopar/)
  assign(x, job, envir = pkgEnv[["shownCache"]]$shownCache_jobs)

  invisible(job)
}


collect_showCache_async <- function(
    x = getOption("reproducible.cachePath"),
    wait = FALSE,
    timeout = 10
) {
  if (.Platform$OS.type == "windows") {
    return(NULL)
    # stop("parallel::mccollect is not available on Windows (forking backend).")
  }

  pkgEnv <- memoiseEnv(cachePath = x)
  if (!exists("shownCache", envir = pkgEnv))
    pkgEnv[["shownCache"]] <- new.env()
  if (!exists(x, envir = pkgEnv[["shownCache"]]))
    pkgEnv[["shownCache"]][[x]] <- new.env()
  if (is.null(pkgEnv[["shownCache"]]$shownCache_jobs) || !exists(x, envir = pkgEnv[["shownCache"]]$shownCache_jobs, inherits = FALSE)) {
    return(invisible(NULL))  # nothing spawned for this cachePath
  }

  job <- get(x, envir = pkgEnv[["shownCache"]]$shownCache_jobs, inherits = FALSE)

  
  # The warning occurs if the pid has already be deleted e.g., manually
  suppressWarnings(
    # Poll or wait for results
    res_list <- parallel::mccollect(job, wait = wait, timeout = timeout)  # collect async results [1](https://www.rdocumentation.org/packages/parallel/versions/3.4.1/topics/mcparallel)[2](https://stat.ethz.ch/R-manual/R-devel/library/parallel/html/mcparallel.html)
  )

  # If still running, mccollect returns NULL (per docs)
  if (is.null(res_list)) {
    return(invisible(NULL))
  }

  # Extract result (single job => first element)
  sc <- res_list[[1]]

  # If child reported an internal NULL issue, propagate as an error
  if (inherits(sc, "shownCache_error")) {
    return(invisible(NULL))
  }

  # Install recovered shownCache object into main session memoiseEnv location.
  # The synchronous showCache() path expects pkgEnv[["shownCache"]][[x]] to be
  # an environment with $FileInfo and $sc slots; previously this assigned `sc`
  # directly at that key, replacing the env with an atomic data.table and then
  # crashing the next sync call at `is.null(scEnv$FileInfo)` with
  # "$ operator is invalid for atomic vectors".
  .installAsyncShownCache(memoiseEnv(cachePath = x), x, sc)

  # Clear job handle after successful install
  rm(list = x, envir = pkgEnv[["shownCache"]]$shownCache_jobs)

  invisible(sc)
}

## Install an async-collected showCache result into the per-cachePath env so
## the synchronous showCache() path can read it via `scEnv$sc` /
## `scEnv$FileInfo`. The async child harvests the inner env from its own
## pkgEnv (`pkgEnv_child[["shownCache"]][[cp]]`) and returns it, so the
## value we receive is *itself* the per-cachePath env -- not a data.table.
## Copy its bindings into the parent's env. If we receive a raw
## data.frame/data.table/list (older child shapes), store it at $sc.
## Anything else is treated as no-op (the env is left empty, which causes
## the sync path to fall through to its full disk scan).
.installAsyncShownCache <- function(pkgEnv_main, x, sc) {
  if (!is.environment(pkgEnv_main[["shownCache"]]))
    pkgEnv_main[["shownCache"]] <- new.env(parent = emptyenv())
  if (!is.environment(pkgEnv_main[["shownCache"]][[x]]))
    pkgEnv_main[["shownCache"]][[x]] <- new.env(parent = emptyenv())
  innerEnv <- pkgEnv_main[["shownCache"]][[x]]
  if (is.environment(sc)) {
    for (nm in ls(sc, all.names = TRUE)) {
      assign(nm, get(nm, envir = sc, inherits = FALSE), envir = innerEnv)
    }
  } else if (is.data.frame(sc) || data.table::is.data.table(sc) || is.list(sc)) {
    innerEnv$sc <- sc
  }
  invisible(NULL)
}
