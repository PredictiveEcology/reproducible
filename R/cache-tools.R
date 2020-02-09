#' @param x A simList or a directory containing a valid archivist repository. Note:
#'   For compatibility with \code{Cache} argument, \code{cacheRepo} can also be
#'   used instead of \code{x}, though \code{x} will take precedence.
#' @param after A time (POSIX, character understandable by data.table).
#'                  Objects cached after this time will be shown or deleted.
#' @param before A time (POSIX, character understandable by data.table).
#'                   Objects cached before this time will be shown or deleted.
#' @param ask Logical. If \code{FALSE}, then it will not ask to confirm deletions using
#'            \code{clearCache} or \code{keepCache}. Default is \code{TRUE}
#' @param ... Other arguments. Currently, \code{regexp}, a logical, can be provided.
#'            This must be \code{TRUE} if the use is passing a regular expression.
#'            Otherwise, \code{userTags} will need to be exact matches. Default is
#'            missing, which is the same as \code{TRUE}. If there are errors due
#'            to regular expression problem, try \code{FALSE}. For \code{cc}, it is
#'            passed to \code{clearCache}, e.g., \code{ask}, \code{userTags}
#' @param userTags Character vector. If used, this will be used in place of the
#'                 \code{after} and \code{before}.
#'                 Specifying one or more \code{userTag} here will clear all
#'                 objects that match those tags.
#'                 Matching is via regular expression, meaning partial matches
#'                 will work unless strict beginning (^) and end ($) of string
#'                 characters are used.
#'                 Matching will be against any of the 3 columns returned by \code{showCache()},
#'                 i.e., \code{artifact}, \code{tagValue} or \code{tagName}.
#'                 Also, length \code{userTags} > 1, then matching is by `and`.
#'                 For `or` matching, use \code{|} in a single character string.
#'                 See examples.
#' @param useCloud Logical. If \code{TRUE}, then every object that is deleted locally will
#'    also be deleted in the \code{cloudFolderID}, if it is non-\code{NULL}
#'
#' @inheritParams Cache
#'
#' @details
#' If neither \code{after} or \code{before} are provided, nor \code{userTags},
#' then all objects will be removed.
#' If both \code{after} and \code{before} are specified, then all objects between
#' \code{after} and \code{before} will be deleted.
#' If \code{userTags} is used, this will override \code{after} or \code{before}.
#'
#' @return Will clear all objects (or those that match \code{userTags}, or those
#' between \code{after} or \code{before}) from the repository located at
#' \code{cachePath} of the sim object, if \code{sim} is provided, or located in
#' \code{cacheRepo}.
#' Invisibly returns a \code{data.table} of the removed items.
#'
#' @note If the cache is larger than 10MB, and clearCache is used, there will be
#' a message and a pause, if interactive, to prevent accidentally deleting of a
#' large cache repository.
#'
#' @export
#' @importFrom data.table setindex
#' @importFrom methods setGeneric setMethod
#' @importFrom utils object.size
#' @rdname viewCache
#'
#' @examples
#' library(raster)
#'
#' tmpDir <- file.path(tempdir(), "reproducible_examples", "Cache")
#' try(clearCache(tmpDir, ask = FALSE), silent = TRUE) # just to make sure it is clear
#'
#' # Basic use
#' ranNumsA <- Cache(rnorm, 10, 16, cacheRepo = tmpDir)
#'
#' # All same
#' ranNumsB <- Cache(rnorm, 10, 16, cacheRepo = tmpDir) # recovers cached copy
#' ranNumsC <- Cache(cacheRepo = tmpDir) %C% rnorm(10, 16)  # recovers cached copy
#' ranNumsD <- Cache(quote(rnorm(n = 10, 16)), cacheRepo = tmpDir) # recovers cached copy
#'
#' # Any minor change makes it different
#' ranNumsE <- Cache(cacheRepo = tmpDir) %C% rnorm(10, 6)# different
#'
#' ## Example 1: basic cache use with tags
#' ranNumsA <- Cache(rnorm, 4, cacheRepo = tmpDir, userTags = "objectName:a")
#' ranNumsB <- Cache(runif, 4, cacheRepo = tmpDir, userTags = "objectName:b")
#' ranNumsC <- Cache(runif, 40, cacheRepo = tmpDir, userTags = "objectName:b")
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
                                  ask = getOption("reproducible.ask"),
                                  useCloud = FALSE, cloudFolderID = NULL,
                                  drv = getOption("reproducible.drv", RSQLite::SQLite()),
                                  conn = getOption("reproducible.conn", NULL), ...) {
  standardGeneric("clearCache")
})

#' @export
#' @rdname viewCache
setMethod(
  "clearCache",
  definition = function(x, userTags, after = NULL, before = NULL, ask, useCloud = FALSE,
                        cloudFolderID = getOption("reproducible.cloudFolderID", NULL),
                        drv = getOption("reproducible.drv", RSQLite::SQLite()),
                        conn = getOption("reproducible.conn", NULL), ...) {
    # isn't clearing the raster bacekd file
    browser(expr = exists("ssss"))

    if (missing(x)) {
      x <- if (!is.null(list(...)$cacheRepo)) {
        message("x not specified, but cacheRepo is; using ", list(...)$cacheRepo)
        list(...)$cacheRepo
      } else  {
        message("x not specified; using ", getOption("reproducible.cachePath")[1])
        x <- getOption("reproducible.cachePath")[1]
      }
    }

    # Check if no args -- faster to delete all then make new empty repo for large repos
    clearWholeCache <- all(missing(userTags), is.null(after), is.null(before))

    if (useCloud || !clearWholeCache) {
      browser(expr = exists("jjjj"))
      # if (missing(after)) after <- NA # "1970-01-01"
      # if (missing(before)) before <- NA # Sys.time() + 1e5

      args <- append(list(x = x, after = after, before = before, userTags = userTags),
                     list(...))

      objsDT <- do.call(showCache, args = args, quote = TRUE)
      if (useCloud) {
        browser(expr = exists("kkkk"))
        if (is.null(cloudFolderID)) {
          stop("If using 'useCloud', 'cloudFolderID' must be provided. ",
               "If you don't know what should be used, try getOption('reproducible.cloudFolderID')")
        }
        if (useDBI()) {
          cacheIds <- unique(objsDT[[.cacheTableHashColName()]])
        } else {
          cacheIds <- objsDT[tagKey == "cacheId", tagValue]
        }
        gdriveLs <- drive_ls(path = as_id(cloudFolderID), pattern = paste(cacheIds, collapse = "|"))
        filenamesToRm <- basename2(CacheStoredFile(x, cacheIds))
        # filenamesToRm <- paste0(cacheIds, ".rda")
        isInCloud <- gdriveLs$name %in% filenamesToRm
        message("From Cloud:")
        drive_rm(as_id(gdriveLs$id[isInCloud]))
      }
    }

    browser(expr = exists("rrrr"))
    if (useDBI()) {
      if (!CacheIsACache(x, drv = drv, conn = conn))
        return(invisible(.emptyCacheTable))
    }

    if (clearWholeCache) {
      if (isInteractive()) {
        if (isTRUE(ask)) {
          cacheSize <- sum(file.size(dir(x, full.names = TRUE, recursive = TRUE)))
          class(cacheSize) <- "object_size"
          formattedCacheSize <- format(cacheSize, "auto")
          message("Your current cache size is ", formattedCacheSize, ".\n",
                  " Are you sure you would like to delete it all? Y or N")
          rl <- readline()
          if (!identical(toupper(rl), "Y")) {
            message("Aborting clearCache")
            return(invisible())
          }
        }

      }
      unlink(CacheStorageDir(x), recursive = TRUE)
      unlink(file.path(x, "rasters"), recursive = TRUE)
      unlink(CacheDBFile(x, drv = drv, conn = conn), recursive = TRUE, force = TRUE)

      checkPath(x, create = TRUE)
      if (useDBI()) {
        createCache(x, drv = drv, force = TRUE)
      } else {
        archivist::createLocalRepo(x)
      }
      memoise::forget(.loadFromLocalRepoMem)
      return(invisible())
    }

    if (isInteractive()) {
      objSizes <- as.numeric(objsDT[tagKey == "object.size"][[.cacheTableTagColName()]])
      cacheSize <- sum(objSizes) / 4
    }

    if (NROW(objsDT)) {
      rastersInRepo <- objsDT[grepl(pattern = "class", tagKey) &
                                grepl(pattern = "Raster", get(.cacheTableTagColName()))]
      hasARaster <- all(!is.na(rastersInRepo[[.cacheTableHashColName()]])) && NROW(rastersInRepo) > 0 # nolint

      if (hasARaster) {
        rasterObjSizes <- as.numeric(objsDT[get(.cacheTableHashColName()) %in%
                                              rastersInRepo[[.cacheTableHashColName()]] &
                                              tagKey == "object.size"]$tagValue)
        fileBackedRastersInRepo <- rastersInRepo[[.cacheTableHashColName()]][rasterObjSizes < 1e5]
      filesToRemove <- lapply(fileBackedRastersInRepo, function(ras) {
          if (useDBI()) {
            r <- loadFromCache(x, ras)
          } else {
            r <- suppressWarnings(archivist::loadFromLocalRepo(ras, repoDir = x, value = TRUE))
          }
          tryCatch(filename(r), error = function(e) NULL)
        })

        if (length(filesToRemove)) {
          filesToRemove <- unlist(filesToRemove)
          #filesToRemove <- file_path_sans_ext(filesToRemove)#gsub(filesToRemove, pattern = "(\\.).*$", replacement = "\\1*")
          if (isInteractive()) {
            dirLs <- dir(unique(dirname(filesToRemove)), full.names = TRUE)
            dirLs <- unlist(lapply(basename(filesToRemove), grep, dirLs, value = TRUE) )
            cacheSize <- sum(cacheSize, file.size(dirLs))
          }
        }
      }

      if (isInteractive()) {
          class(cacheSize) <- "object_size"
          formattedCacheSize <- format(cacheSize, "auto")
          if (isTRUE(ask)) {
            message("Your size of your selected objects is ", formattedCacheSize, ".\n",
                    " Are you sure you would like to delete it all? Y or N")
            rl <- readline()
            if (!identical(toupper(rl), "Y")) {
              message("Aborting clearCache")
              return(invisible())
            }
          }
      }

      # remove file-backed files
      if (all(!is.na(rastersInRepo$cacheId)) && NROW(rastersInRepo) > 0) {
        unlink(filesToRemove)
      }

      objToGet <- unique(objsDT[[.cacheTableHashColName()]])
      if (useDBI()) {
        if (is.null(conn)) {
          conn <- dbConnectAll(drv, cachePath = x, create = FALSE)
          on.exit({dbDisconnect(conn)})
        }
        rmFromCache(x, objToGet, conn = conn, drv = drv)# many = TRUE)
        browser(expr = exists("rmFC"))
      } else {
        suppressWarnings(archivist::rmFromLocalRepo(objToGet, x, many = TRUE))
      }
    }
    memoise::forget(.loadFromLocalRepoMem)
    try(setindex(objsDT, NULL), silent = TRUE)
    return(invisible(objsDT))
})

#' @details
#' \code{cc(secs)} is just a shortcut for \code{clearCache(repo = Paths$cachePath, after = secs)},
#' i.e., to remove any cache entries touched in the last \code{secs} seconds.
#'
#' @param secs Currently 3 options: the number of seconds to pass to \code{clearCache(after = secs)},
#'     a \code{POSIXct} time e.g., from \code{Sys.time()}, or missing. If missing,
#'             the default, then it will delete the most recent entry in the Cache.
#'
#' @export
#' @rdname viewCache
#'
#' @examples
#' tmpDir <- file.path(tempdir(), "reproducible_examples", "Cache")
#' try(clearCache(tmpDir, ask = FALSE), silent = TRUE) # just to make sure it is clear
#'
#' Cache(rnorm, 1, cacheRepo = tmpDir)
#' thisTime <- Sys.time()
#' Cache(rnorm, 2, cacheRepo = tmpDir)
#' Cache(rnorm, 3, cacheRepo = tmpDir)
#' Cache(rnorm, 4, cacheRepo = tmpDir)
#' showCache(x = tmpDir) # shows all 4 entries
#' cc(ask = FALSE, x = tmpDir)
#' showCache(x = tmpDir) # most recent is gone
#' cc(thisTime, ask = FALSE, x = tmpDir)
#' showCache(x = tmpDir) # all those after thisTime gone, i.e., only 1 left
#' cc(ask = FALSE, x = tmpDir) # Cache is
#' cc(ask = FALSE, x = tmpDir) # Cache is already empty
cc <- function(secs, ...) {
  browser(expr = exists("jjjj"))
  if (missing(secs)) {
    message("No time provided; removing the most recent entry to the Cache")
    suppressMessages({theCache <- reproducible::showCache(...)})
    if (NROW(theCache) > 0) {
      accessed <- data.table::setkey(theCache[tagKey == "accessed"], tagValue)
      clearCache(userTags = tail(accessed, 1)[[.cacheTableHashColName()]], ...)
    } else {
      message("Cache already empty")
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
#' These are convenience wrappers around \code{DBI}
#' (formerly \code{archivist}) package functions.
#' They allow the user a bit of control over what is being cached.
#'
#' \describe{
#'   \item{\code{clearCache}}{remove items from the cache based on their
#'                            \code{userTag} or \code{times} values.}
#'   \item{\code{keepCache}}{remove all cached items \emph{except} those based on
#'                           certain \code{userTags} or \code{times} values.}
#'   \item{\code{showCache}}{display the contents of the cache.}
#' }
#'
#' @inheritParams clearCache
#'
#' @export
#' @importFrom DBI dbSendQuery dbFetch dbClearResult
#' @importFrom data.table data.table set setkeyv
#' @rdname viewCache
#' @seealso \code{\link{mergeCache}}, \code{archivist::splitTagsLocal}. Many more examples
#' in \code{\link{Cache}}
#'
setGeneric("showCache", function(x, userTags = character(), after = NULL, before = NULL,
                                 drv = getOption("reproducible.drv", RSQLite::SQLite()),
                                 conn = getOption("reproducible.conn", NULL), ...) {
  standardGeneric("showCache")
})

#' @export
#' @rdname viewCache
setMethod(
  "showCache",
  definition = function(x, userTags, after = NULL, before = NULL, drv,
                        conn, ...) {
    browser(expr = exists("rrrr"))
    if (missing(x)) {
      message("x not specified; using ", getOption("reproducible.cachePath")[1])
      x <- getOption("reproducible.cachePath")[1]
    }
    browser(expr = exists("jjjj"))
    if (useDBI()) {
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
    } else {
      if (is.null(after)) after <- "1970-01-01"
      if (is.null(before)) before <- Sys.time() + 1e5
    }
    # if (is(x, "simList")) x <- x@paths$cachePath

    # not seeing userTags
    # Clear the futures that are resolved
    .onLinux <- .Platform$OS.type == "unix" && unname(Sys.info()["sysname"]) == "Linux" &&
      !isFALSE(getOption("reproducible.futurePlan"))
    if (.onLinux) {
      if (exists("futureEnv", envir = .reproEnv))
        if (suppressWarnings(requireNamespace("future", quietly = TRUE, warn.conflicts = FALSE))) {
          checkFutures()
        }
    }

    if (useDBI()) {
      if (is.null(conn)) {
        conn <- dbConnectAll(drv, cachePath = x, create = FALSE)
        if (is.null(conn)) {
          return(invisible(.emptyCacheTable))
        }
        on.exit({
          dbDisconnect(conn)
        })
      }

      dbTabNam <- CacheDBTableName(x, drv = drv)
      # tab <- dbReadTable(conn, dbTabNam)
      res <- retry(retries = 250, exponentialDecayBase = 1.01, quote(
        dbSendQuery(conn, paste0("SELECT * FROM \"", dbTabNam, "\""))))
      tab <- dbFetch(res)
      dbClearResult(res)
      if (is(tab, "try-error"))
        objsDT <- .emptyCacheTable
      else
        objsDT <- setDT(tab)
      #setkeyv(objsDT, "cacheId")
    } else {
      objsDT <- archivist::showLocalRepo(x) %>% data.table()
      #setkeyv(objsDT, "md5hash")
    }

    if (NROW(objsDT) > 0) {
      if (useDBI()) {
        if (!afterNA || !beforeNA) {
          objsDT3 <- objsDT[tagKey == "accessed"]
          if (!beforeNA)
            objsDT3 <- objsDT3[(tagValue <= before)]
          if ( !afterNA)
            objsDT3 <- objsDT3[(tagValue >= after)]
          # objsDT3 <- objsDT3[!duplicated(cacheId)]
          browser(expr = exists("zzzz"))
          # objsDT <- objsDT[cacheId %in% objsDT3$cacheId]
          objsDT <- objsDT[objsDT$cacheId %in% unique(objsDT3$cacheId)] # faster than data.table join
        }
      } else {
        objsDT <- data.table(archivist::splitTagsLocal(x), key = "artifact")
        objsDT3 <- objsDT[tagKey == "accessed"][(tagValue <= before) &
                                                  (tagValue >= after)][!duplicated(artifact)]
        objsDT <- objsDT[artifact %in% objsDT3[[.cacheTableHashColName()]]]
      }
      if (length(userTags) > 0) {
        if (isTRUE(list(...)$regexp) | is.null(list(...)$regexp)) {
          objsDTs <- list()
          for (ut in userTags) {
              objsDT2 <- objsDT[
                grepl(get(.cacheTableTagColName()), pattern = ut) |
                  grepl(tagKey, pattern = ut) |
                  grepl(get(.cacheTableHashColName()), pattern = ut)]
              setkeyv(objsDT2, .cacheTableHashColName())
              shortDT <- unique(objsDT2, by = .cacheTableHashColName())[, get(.cacheTableHashColName())]
            #}
            objsDT <- if (NROW(shortDT)) objsDT[shortDT, on = .cacheTableHashColName()] else objsDT[0] # merge each userTags
          }
        } else {
          if (useDBI()) {
            objsDT2 <- objsDT[cacheId %in% userTags | tagKey %in% userTags | tagValue %in% userTags]
            setkeyv(objsDT2, "cacheId")
            shortDT <- unique(objsDT2, by = "cacheId")[, cacheId]
            objsDT <- if (NROW(shortDT)) objsDT[shortDT, on = .cacheTableHashColName()] else objsDT[0] # merge each userTags
          } else {
            objsDT2 <- objsDT[artifact %in% userTags | tagKey %in% userTags | tagValue %in% userTags]
            setkeyv(objsDT2, "artifact")
            shortDT <- unique(objsDT2, by = "artifact")[, artifact]
            objsDT <- if (NROW(shortDT)) objsDT[shortDT, on = .cacheTableHashColName()] else objsDT[0] # merge each userTags
          }
        }
      }
    }
    verboseMessaging <- TRUE
    if (!is.null(list(...)$verboseMessaging)) {
      if (!isTRUE(list(...)$verboseMessaging)) {
        verboseMessaging <- FALSE
      }
    }
    if (verboseMessaging)
      .messageCacheSize(x, artifacts = unique(objsDT[[.cacheTableHashColName()]]),
                        cacheTable = objsDT)
    objsDT
})

#' @rdname viewCache
setGeneric("keepCache", function(x, userTags = character(), after = NULL, before = NULL,
                                 ask  = getOption("reproducible.ask"),
                                 drv = getOption("reproducible.drv", RSQLite::SQLite()),
                                 conn = getOption("reproducible.conn", NULL), ...) {
  standardGeneric("keepCache")
})

#' @export
#' @rdname viewCache
setMethod(
  "keepCache",
  definition = function(x, userTags, after, before, ask, drv, conn, ...) {
    if (missing(x)) {
      message("x not specified; using ", getOption("reproducible.cachePath")[1])
      x <- getOption("reproducible.cachePath")[1]
    }
    # if (missing(after)) after <- NA # "1970-01-01"
    # if (missing(before)) before <- NA # Sys.time() + 1e5
    # if (is(x, "simList")) x <- x@paths$cachePath

    args <- append(list(x = x, after = after, before = before, userTags = userTags), list(...))

    objsDTAll <- suppressMessages(showCache(x, verboseMessaging = FALSE))
    objsDT <- do.call(showCache, args = args)
    keep <- unique(objsDT[[.cacheTableHashColName()]])
    eliminate <- unique(objsDTAll[[.cacheTableHashColName()]][
      !(objsDTAll[[.cacheTableHashColName()]] %in% keep)])


    if (length(eliminate)) {
      #eliminate <- paste(eliminate, collapse = "|") ## TODO: remove
      clearCache(x, eliminate, verboseMessaging = FALSE, regexp = FALSE, ask = ask)
    }
    return(objsDT)
})

#' Merge two cache repositories together
#'
#' All the \code{cacheFrom} artifacts will be put into \code{cacheTo}
#' repository. All \code{userTags} will be copied verbatim, including
#' \code{accessed}, with 1 exception: \code{date} will be the
#' current \code{Sys.time()} at the time of merging. The
#' \code{createdDate} column will be similarly the current time
#' of merging.
#'
#' @param cacheTo The cache repository (character string of the file path)
#'                that will become larger, i.e., merge into this
#' @param cacheFrom The cache repository (character string of the file path)
#'                  from which all objects will be taken and copied from
#' @param drvTo The database driver for the \code{cacheTo}.
#' @param drvFrom The database driver for the \code{cacheFrom}
#' @param connTo The connection for the \code{cacheTo}. If not provided, then
#'   a new one will be made from \code{drvTo} and \code{cacheTo}
#' @param connFrom The database for the \code{cacheFrom}. If not provided, then
#'   a new one will be made from \code{drvFrom} and \code{cacheFrom}
#'
#' @details
#' This is still experimental
#'
#' @return The character string of the path of \code{cacheTo}, i.e., not the
#' objects themselves.
#'
#' @rdname mergeCache
setGeneric("mergeCache", function(cacheTo, cacheFrom,
                                  drvTo = getOption("reproducible.drv", RSQLite::SQLite()),
                                  drvFrom = getOption("reproducible.drv", RSQLite::SQLite()),
                                  connTo = NULL, connFrom = NULL) {
  standardGeneric("mergeCache")
})

#' @export
#' @rdname mergeCache
setMethod(
  "mergeCache",
  definition = function(cacheTo, cacheFrom, drvTo, drvFrom, connTo, connFrom) {
    if (is.null(connTo)) {
      connTo <- dbConnectAll(drvTo, cachePath = cacheTo)
      on.exit(dbDisconnect(connTo), add = TRUE)
    }

    if (is.null(connFrom)) {
      connFrom <- dbConnectAll(drvFrom, cachePath = cacheFrom)
      on.exit(dbDisconnect(connFrom), add = TRUE)
    }

    suppressMessages({
      cacheFromList <- showCache(cacheFrom, drv = drvFrom, connFrom = connFrom)
    })
    suppressMessages({
      cacheToList <- showCache(cacheTo, drv = drvTo, connTo = connTo)
    })
    browser(expr = exists("kkkk"))

    artifacts <- unique(cacheFromList[[.cacheTableHashColName()]])
    objectList <- lapply(artifacts, function(artifact) {
      browser(expr = exists("gggg"))
      if (!(artifact %in% cacheToList[[.cacheTableHashColName()]])) {
        browser(expr = exists("gggg"))
        outputToSave <- if (useDBI()) {
          try(loadFromCache(cacheFrom, artifact))
        } else {
          try(archivist::loadFromLocalRepo(artifact, repoDir = cacheFrom, value = TRUE))
        }
        if (is(outputToSave, "try-error")) {
          message("Continuing to load others")
          outputToSave <- NULL
        }

        ## Save it
        userTags <- cacheFromList[artifact, on = .cacheTableHashColName()][
          !tagKey %in% c("format", "name", "date", "cacheId"), list(tagKey, tagValue)]
        if (useDBI()) {
          output <- saveToCache(cacheTo, userTags = userTags, obj = outputToSave, cacheId = artifact) # nolint
        } else {
          written <- FALSE
          if (is(outputToSave, "Raster")) {
            outputToSave <- .prepareFileBackedRaster(outputToSave, repoDir = cacheTo)
          }
          userTags <- c(paste0(userTags$tagKey, ":", userTags$tagValue))
          while (!written) {
            saved <- suppressWarnings(try(
              archivist::saveToLocalRepo(outputToSave, repoDir = cacheTo,
                              artifactName = NULL,
                              archiveData = FALSE, archiveSessionInfo = FALSE,
                              archiveMiniature = FALSE, rememberName = FALSE,
                              silent = TRUE, userTags = userTags),
              silent = TRUE
            ))
            # This is for simultaneous write conflicts. SQLite on Windows can't handle them.
            written <- if (is(saved, "try-error")) {
              Sys.sleep(0.05)
              FALSE
            } else {
              TRUE
            }
          }
        }
        message(artifact, " copied")
        outputToSave
      } else {
        message("Skipping ", artifact, "; already in ", cacheTo)
      }
    })
    .messageCacheSize(cacheTo, cacheTable = showCache(cacheTo))

    return(invisible(cacheTo))
})

#' @keywords internal
.messageCacheSize <- function(x, artifacts = NULL, cacheTable) {
  browser(expr = exists("ffff"))

  tagCol <- "tagValue"
  if (missing(cacheTable)) {
    if (useDBI()) {
      a <- showCache(x, verboseMessaging = FALSE)
    } else {
      a <- showLocalRepo2(x)
      tagCol <- "tag"
    }

  } else {
    a <- cacheTable
  }
  cn <- if (any(colnames(a) %in% "tag")) "tag" else "tagKey"
  b <- a[a[[cn]] == "object.size",]
  if (any(colnames(a) %in% "tag")) {
    fsTotal <- sum(as.numeric(unlist(lapply(strsplit(b[[cn]], split = ":"), function(x) x[[2]])))) / 4
  } else {
    fsTotal <- sum(as.numeric(b[[.cacheTableTagColName()]])) / 4
  }
  fsTotalRasters <- sum(file.size(dir(file.path(x, "rasters"), full.names = TRUE, recursive = TRUE)))
  fsTotal <- fsTotal + fsTotalRasters
  class(fsTotal) <- "object_size"
  preMessage1 <- "  Total (including Rasters): "

  b <- a[a[[.cacheTableHashColName()]] %in% artifacts &
           (a[[cn]] %in% "object.size"),]
  if (cn == "tag") {
    fs <- sum(as.numeric(unlist(lapply(strsplit(b[[cn]], split = ":"), function(x) x[[2]])))) / 4
  } else {
    fs <- sum(as.numeric(b[[.cacheTableTagColName()]])) / 4
  }

  class(fs) <- "object_size"
  preMessage <- "  Selected objects (not including Rasters): "

  message("Cache size: ")
  message(preMessage1, format(fsTotal, "auto"))
  message(preMessage, format(fs, "auto"))
}

#' @keywords internal
checkFutures <- function() {
  # This takes a long time -- can't use it if
  resol1 <- FALSE
  count <- 0
  lsFutureEnv <- ls(.reproEnv$futureEnv)

  anyFutureWrite <- length(lsFutureEnv)

  if (anyFutureWrite > 0) {
    #objsInReproEnv <- ls(.reproEnv)
    #objsInReproEnv <- grep("^future|cloudCheckSums", objsInReproEnv, value = TRUE)
    while (any(!resol1)) {
      count <- count + 1
      #numSleeps <<- numSleeps+1
      if (count > 1 ) {
        Sys.sleep(0.001)
        if (count > 1e3) {
          message("Future is not resolved after 1 second of waiting. Allowing to proceed.")
          break
        }
      }
      resol <- future::resolved(.reproEnv$futureEnv)
      resol1 <- resol[!startsWith(names(resol), "cloudCheckSums")]
    }
    browser(expr = exists("aaaa"))
    if (length(resol) > 0)
      .reproEnv$futureEnv[[lsFutureEnv]] <- NULL
  }
}

#' Cache helpers
#'
#' A few helpers to get specific things from the cache repository
#' @rdname cache-helpers
#' @inheritParams Cache
#' @export
#' @param shownCache Primary way of supplying \code{cacheRepo}; the data.table obj
#'   resulting from \code{showCache}, i.e., it will override \code{cacheRepo}.
#'   If this and \code{cacheRepo} are missing, then it will default to
#'   \code{getOption('reproducible.cachePath')}
#' @param cacheId A character vector of cacheId values to use in the cache
#' @param concatenated Logical. If \code{TRUE}, the returned \code{userTags} will
#'   be concatenated \code{tagKey:tagValue}.
getUserTags <- function(cacheRepo, shownCache, cacheId, concatenated = TRUE) {
  stop("This function is deprecated")
  if (missing(shownCache)) {
    if (missing(cacheRepo)) {
      cacheRepos <- .checkCacheRepo(create = TRUE)
      cacheRepo <- cacheRepos[[1]]
    }
    suppressMessages({
      shownCache <- showCache(cacheRepo)
    })
  }
  arts <- getArtifact(shownCache = shownCache, cacheId = cacheId)
  if (!missing(cacheId)) {
    shownCache <- shownCache[artifact %in% arts]
  }

  userTags <- shownCache[!tagKey %in% c("format", "name", "class", "date", "cacheId"),
                         list(tagKey, tagValue)]
  if (concatenated)
    userTags <- c(paste0(userTags$tagKey, ":", userTags$tagValue))
  userTags
}


#' @param artifact Character vector of artifact values in the
#'   \code{artifact} column of \code{showCache}
#'
#' @return \code{getCacheId} returns the \code{cacheId} values for 1 or more artifacts in the cache.
#'
#' @export
#' @rdname cache-helpers
getCacheId <- function(cacheRepo, shownCache, artifact) {
  if (missing(shownCache)) {
    if (missing(cacheRepo)) {
      cacheRepos <- .checkCacheRepo(create = TRUE)
      cacheRepo <- cacheRepos[[1]]
    }
    suppressMessages({
      shownCache <- showCache(cacheRepo)
    })
  }
  if (!missing(artifact)) {
    artifacts <- artifact
    shownCache <- shownCache[artifact %in% artifacts]
  }
  shownCache[tagKey == "cacheId"]$tagValue
}

#' @return
#' \code{getArtifact} returns the \code{artifact} value for 1 or more
#' entries in the cache, by \code{cacheId}.
#'
#' @export
#' @rdname cache-helpers
getArtifact <- function(cacheRepo, shownCache, cacheId) {
  stop("This function is deprecated")
  if (missing(shownCache)) {
    if (missing(cacheRepo)) {
      cacheRepos <- .checkCacheRepo(create = TRUE)
      cacheRepo <- cacheRepos[[1]]
    }
    suppressMessages({
      shownCache <- showCache(cacheRepo)
    })
  }
  if (!missing(cacheId)) {
    shownCache <- shownCache[tagValue %in% cacheId]
  }
  shownCache[tagKey == "cacheId", artifact]
}

useDBI <- function() {
  ud <- getOption("reproducible.useDBI", TRUE)
  rn <- requireNamespace("archivist", quietly = TRUE)
  if (!ud && !rn)
    message("Trying to not use DBI as database engine, but archivist is not ",
            "installed. Please install.packages('archivist') or ",
            "set options('reproducible.useDBI' = TRUE)")
  ud || !rn
}
