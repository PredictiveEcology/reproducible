#' @param x A simList or a directory containing a valid Cache repository. Note:
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
#' @inheritParams Cache
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
#' ranNumsD <- Cache(quote(rnorm(n = 10, 16)), cacheRepo = tmpDir) # recovers cached copy
#'
#' # Any minor change makes it different
#' ranNumsE <- Cache(rnorm, 10, 6, cacheRepo = tmpDir) # different
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
                                  useCloud = FALSE,
                                  cloudFolderID = NULL,
                                  drv = getOption("reproducible.drv"),
                                  conn = getOption("reproducible.conn", NULL),
                                  verbose = getOption("reproducible.verbose"), ...) {
  standardGeneric("clearCache")
})

#' @export
#' @rdname viewCache
setMethod(
  "clearCache",
  definition = function(x, userTags, after = NULL, before = NULL, ask, useCloud = FALSE,
                        cloudFolderID = NULL,
                        drv = getOption("reproducible.drv"),
                        conn = getOption("reproducible.conn", NULL),
                        verbose = getOption("reproducible.verbose"), ...) {
    # isn't clearing the raster bacekd file
    # browser(expr = exists("._clearCache_1"))

    if (missing(x)) {
      x <- if (!is.null(list(...)$cacheRepo)) {
        messageCache("x not specified, but cacheRepo is; using ", list(...)$cacheRepo,
                     verbose = verbose)
        list(...)$cacheRepo
      } else  {
        messageCache("x not specified; using ", getOption("reproducible.cachePath")[1],
                     verbose = verbose)
        x <- getOption("reproducible.cachePath")[1]
      }
    }

    # Check if no args -- faster to delete all then make new empty repo for large repos
    clearWholeCache <- all(isTRUE(length(userTags) == 0), is.null(after), is.null(before))

    if (isTRUEorForce(useCloud) || !clearWholeCache) {

      args <- append(list(x = x, after = after, before = before, userTags = userTags,
                          conn = conn, drv = drv),
                     list(...))

      objsDT <- do.call(showCache, args = args, quote = TRUE)
      if (isTRUE(useCloud) && NROW(objsDT) > 0 || identical(useCloud, "force")) {
        if (!requireNamespace("googledrive")) stop(requireNamespaceMsg("googledrive", "to use google drive files"))
        if (exists("aaa")) browser()
        cloudFolderID <- cloudFolderID(cloudFolderID)
        # browser(expr = exists("._clearCache_3"))
        cacheIds <- unique(objsDT[[.cacheTableHashColName()]])
        if (identical(useCloud, "force")) {
          gdriveLs <- cloudDriveLs(cloudFolderID, pattern = userTags)
          cacheIds <- c(cacheIds, gsub("\\..*$", "", gdriveLs$name))
        }
        cloudRemove(cloudFolderID, x, cacheIds, cacheDT = objsDT)
        cloudRmFromDBFile(objsDT = objsDT, cloudFolderID = cloudFolderID, cacheIds = cacheIds,
                          gdriveLs = NULL, drv = drv, conn = conn)

      }

    }

    returnEmptyCache <- FALSE
    # browser(expr = exists("rrrr"))
    if (!CacheIsACache(x, drv = drv, conn = conn, allowNoStorageFolder = TRUE, allowTBMismatch = TRUE))
      returnEmptyCache <- TRUE

    if (clearWholeCache && !returnEmptyCache) {
      if (isInteractive()) {
        if (isTRUE(ask)) {
          cacheSize <- sum(file.size(dir(x, full.names = TRUE, recursive = TRUE)))
          class(cacheSize) <- "object_size"
          formattedCacheSize <- format(cacheSize, "auto")
          messageQuestion("Your current cache size is ", formattedCacheSize, ".\n",
                  " Are you sure you would like to delete it all? Y or N")
          rl <- readline()
          if (!identical(toupper(rl), "Y")) {
            messageCache("Aborting clearCache", verbose = verbose)
            return(invisible())
          }
        }

      }
      unlink(CacheStorageDirs(x), recursive = TRUE)
      unlink(CacheDBFile(x, drv = drv, conn = conn), recursive = TRUE, force = TRUE)

      checkPath(x, create = TRUE)
      createCache(x, drv = drv, force = TRUE)
      if (isTRUE(getOption("reproducible.useMemoise"))) {
        if (exists(x, envir = .pkgEnv))
          rm(list = x, envir = .pkgEnv)
      }
      # memoise::forget(.loadFromLocalRepoMem)
      returnEmptyCache <- TRUE
    }
    if (isTRUE(returnEmptyCache))
      return(.emptyCacheTable)

    if (NROW(objsDT)) {
      rmFromCache(x, objsDT, ask = ask, verbose = verbose, conn = conn, drv = drv)# many = TRUE)

    }
    try(setindex(objsDT, NULL), silent = TRUE)
    setkeyv(objsDT, colnames(objsDT))
    return(objsDT)
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
#' @inheritParams Cache
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
cc <- function(secs,
               verbose = getOption("reproducible.verbose"), ...) {
  # browser(expr = exists("jjjj"))
  if (missing(secs)) {
    messageCache("No time provided; removing the most recent entry to the Cache",
                 verbose = verbose)
    suppressMessages({theCache <- reproducible::showCache(...)})
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
#' These are convenience wrappers around \code{DBI} package functions.
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
#' @importFrom data.table data.table set setkeyv
#' @rdname viewCache
#' @seealso \code{\link{mergeCache}}. Many more examples
#' in \code{\link{Cache}}.
#'
setGeneric("showCache", function(x, userTags = character(), after = NULL, before = NULL,
                                 drv = getOption("reproducible.drv"),
                                 conn = getOption("reproducible.conn", NULL),
                                 verbose = getOption("reproducible.verbose"), ...) {
  standardGeneric("showCache")
})

#' @export
#' @rdname viewCache
setMethod(
  "showCache",
  definition = function(x, userTags, after = NULL, before = NULL, drv, conn, ...) {
    # browser(expr = exists("rrrr"))
    dots <- list(...)
    if (missing(x)) {
      messageCache("x not specified; using ", getOption("reproducible.cachePath")[1],
                   verbose = verbose)
      x <- getOption("reproducible.cachePath")[1]
    }
    after <- toNA(after)
    afterNA <- is.na(after)
    before <- toNA(before)
    beforeNA <- is.na(before)

    # Clear the futures that are resolved
    useFuture <- !isFALSE(getOption("reproducible.futurePlan"))
    if (useFuture) conn <- checkFutures(x, drv, conn)

    if (is.null(conn)) {
      conn <- dbConnectAll(drv, cachePath = x, create = FALSE)
      if (is.null(conn)) {
        return(.emptyCacheTable)
      }
      on.exit({
        dbDisconnectAll(conn, shutdown = TRUE)
      }, add = TRUE)
    }
    # This next "is.null(dots$dbTabName)" may be a work around for an internal
    #   usage from cloud caching -- where dbTabNam is supplied
    dbTabNam <- dots$dbTabNam
    if (!CacheIsACache(x, drv = drv, conn = conn,
                       allowNoStorageFolder = isTRUE(list(...)$allowNoStorageFolder),
                       allowTBMismatch = isTRUE(list(...)$allowTBMismatch)))
      return(.emptyCacheTable)

    objsDT <- showCacheAll(x, drv, conn, dbTabNam)

    if (NROW(objsDT) > 0) {
      objsDT <- afterBefore(objsDT, afterNA, after, beforeNA, before)
      if (length(userTags) > 0) {
        if (isTRUE(dots$regexp) | is.null(dots$regexp)) {
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
          objsDT2 <- objsDT[cacheId %in% userTags | tagKey %in% userTags | tagValue %in% userTags]
          setkeyv(objsDT2, "cacheId")
          shortDT <- unique(objsDT2, by = "cacheId")[, cacheId]
          objsDT <- if (NROW(shortDT)) objsDT[shortDT, on = .cacheTableHashColName()] else objsDT[0] # merge each userTags

        }
      }
    }
    verboseMessaging <- TRUE
    if (!is.null(dots$verboseMessaging)) {
      if (!isTRUE(dots$verboseMessaging)) {
        verboseMessaging <- FALSE
      }
    }
    if (verboseMessaging)
      .messageCacheSize(x, artifacts = unique(objsDT[[.cacheTableHashColName()]]),
                        cacheTable = objsDT, verbose = verbose)
    setkeyv(objsDT, colnames(objsDT))
    objsDT[]
})

#' @rdname viewCache
setGeneric("keepCache", function(x, userTags = character(), after = NULL, before = NULL,
                                 ask  = getOption("reproducible.ask"),
                                 useCloud = FALSE,
                                 cloudFolderID = getOption("reproducible.cloudFolderID", NULL),
                                 drv = getOption("reproducible.drv"),
                                 conn = getOption("reproducible.conn", NULL), ...) {
  standardGeneric("keepCache")
})

#' @export
#' @rdname viewCache
setMethod(
  "keepCache",
  definition = function(x, userTags, after, before, ask, useCloud, cloudFolderID,
                        drv, conn,
                        verbose = getOption("reproducible.verbose"), ...) {
    dots <- list(...)
    if (missing(x)) {
      if (!is.null(dots$cacheRepo)) {
        messageCache("x not specified but cacheRepo is; using cacheRepo",
                     verbose = verbose)
        x <- dots$cacheRepo
      } else {
        messageCache("x not specified; using ", getOption("reproducible.cachePath")[1],
                     verbose = verbose)
        x <- getOption("reproducible.cachePath")[1]
      }
    }

    args <- append(list(x = x, after = after, before = before, userTags = userTags), dots)

    objsDTAll <- suppressMessages(showCache(x, verboseMessaging = FALSE))
    objsDT <- suppressMessages(do.call(showCache, args = args))
    keep <- unique(objsDT[[.cacheTableHashColName()]])
    eliminate <- unique(objsDTAll[[.cacheTableHashColName()]][
      !(objsDTAll[[.cacheTableHashColName()]] %in% keep)])

    if (length(eliminate)) {
      clearCache(x, eliminate, verboseMessaging = FALSE, regexp = FALSE, ask = ask,
                 useCloud = useCloud, cloudFolderID = cloudFolderID)
    }
    setkeyv(objsDT, colnames(objsDT))
    return(objsDT)
})

#' Merge two cache repositories together
#'
#' \if{html}{\figure{lifecycle-experimental.svg}{options: alt="experimental"}}
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
#' @return The character string of the path of \code{cacheTo}, i.e., not the
#' objects themselves.
#'
#' @inheritParams Cache
#' @rdname mergeCache
setGeneric("mergeCache", function(cacheTo, cacheFrom,
                                  drvTo = getOption("reproducible.drv"),
                                  drvFrom = getOption("reproducible.drv"),
                                  verbose = getOption("reproducible.verbose", 1),
                                  connTo = NULL, connFrom = NULL) {
  standardGeneric("mergeCache")
})

#' @export
#' @rdname mergeCache
setMethod(
  "mergeCache",
  definition = function(cacheTo, cacheFrom, drvTo, drvFrom,
                        verbose = getOption("reproducible.verbose", 1),
                        connTo, connFrom
                        ) {
    if (is.null(connTo)) {
      connTo <- dbConnectAll(drvTo, cachePath = cacheTo)
      on.exit(dbDisconnectAll(connTo, shutdown = TRUE), add = TRUE)
    }

    if (is.null(connFrom)) {
      connFrom <- dbConnectAll(drvFrom, cachePath = cacheFrom)
      on.exit(dbDisconnectAll(connFrom, shutdown = TRUE), add = TRUE)
    }

    suppressMessages({
      cacheFromList <- showCache(cacheFrom, drv = drvFrom, connFrom = connFrom)
    })
    suppressMessages({
      cacheToList <- showCache(cacheTo, drv = drvTo, connTo = connTo)
    })
    # browser(expr = exists("kkkk"))

    artifacts <- unique(cacheFromList[[.cacheTableHashColName()]])
    objectList <- lapply(artifacts, function(artifact) {
      # browser(expr = exists("gggg"))
      if (!(artifact %in% cacheToList[[.cacheTableHashColName()]])) {
        # browser(expr = exists("gggg"))
        outputToSave <- try(loadFromCache(cacheFrom, artifact))

        if (is(outputToSave, "try-error")) {
          messageCache("Continuing to load others", verbose = verbose)
          outputToSave <- NULL
        }

        ## Save it
        userTags <- cacheFromList[artifact, on = .cacheTableHashColName()][
          !tagKey %in% c("format", "name", "date", "cacheId"), list(tagKey, tagValue)]
        output <- saveToCache(cacheTo, userTags = userTags, obj = outputToSave, cacheId = artifact) # nolint
        messageCache(artifact, " copied", verbose = verbose)
        outputToSave
      } else {
        messageCache("Skipping ", artifact, "; already in ", cacheTo, verbose = verbose)
      }
    })
    .messageCacheSize(cacheTo, cacheTable = showCache(cacheTo), verbose = verbose)

    return(invisible(cacheTo))
})

#' @keywords internal
.messageCacheSize <- function(x, artifacts = NULL, cacheTable,
                              verbose = getOption("reproducible.verbose")) {
  # browser(expr = exists("ffff"))
  CacheFiles <- dir(unique(CacheStorageDirs(x)), full.names = TRUE)
  fsTotal <- sum(file.size(CacheFiles))

  class(fsTotal) <- "object_size"
  preMessage1 <- "  Total (including auxiliary files -- e.g., .tif, .grd): "

  messageCache("Cache size: ", verbose = verbose)
  messageCache(preMessage1, format(fsTotal, "auto"), verbose = verbose)
}

#' @keywords internal
checkFutures <- function(cacheRepo, drv, conn) {
  if (exists("futureEnv", envir = .reproEnv)) {
    if (!is.null(conn)) {
      dbDisconnectAll(conn, shutdown = TRUE)
    }
    resol1 <- FALSE
    count <- 0
    lsFutureEnv <- ls(.reproEnv$futureEnv)

    anyFutureWrite <- length(lsFutureEnv)

    if (anyFutureWrite > 0) {
      while (any(!resol1)) {
        if (count == 0) messageCache("Waiting for a previous future `saveToCache` to complete ")
        if (count %% 20 == 0) messageCache("\b.")
        count <- count + 1
        if (count > 1 ) {
          Sys.sleep(0.001)
          if (count > 3e3) {
            messageCache("Future is not resolved after many seconds of waiting. Allowing to proceed.")
            break
          }
        }
        resol <- future::resolved(.reproEnv$futureEnv)
        resol1 <- resol[!startsWith(names(resol), "cloudCheckSums")]
      }
      if (length(resol[resol]) > 0) {
        rm(list = names(resol)[resol], envir = .reproEnv$futureEnv)
      }

    }
  }
  if (!is.null(conn)) conn <- dbConnectAll(drv, cachePath = cacheRepo)
  return(conn)
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



isTRUEorForce <- function(cond) {
  isTRUE(cond) || identical(cond, "force")
}


showCacheAll <- function(x, drv, conn, dbTabNam = NULL) {
  if (useSQL(conn)) {
    if (missing(x))
      x <- dirname(conn@dbname)
    if (is.null(dbTabNam))
      dbTabNam <- CacheDBTableName(x, drv = drv)
    res <- retry(retries = 250, exponentialDecayBase = 1.01, quote(
      DBI::dbSendQuery(conn, paste0("SELECT * FROM \"", dbTabNam, "\""))))
    tab <- DBI::dbFetch(res)
    DBI::dbClearResult(res)
  } else {
    objName <- objNameFromConn(conn)
    tab <- readFilebasedConn(objName, conn)
    tab <- finalizeDTtoWrite(conn = conn, dt = tab, objName = objName)
  }
  if (is(tab, "try-error")) {
    objsDT <- setDF(.emptyCacheTable)
  } else {
    objsDT <- setDT(tab)
  }
  objsDT
}

toNA <- function(x) {
  # afterNA <- FALSE
  if (is.null(x)) {
    #afterNA <- TRUE
    x <- NA
  }
  x
}

afterBefore <- function(objsDT, afterNA, after, beforeNA, before) {
  if (!afterNA || !beforeNA) {
    objsDT3 <- objsDT[tagKey == "accessed"]
    if (!beforeNA)
      objsDT3 <- objsDT3[(tagValue <= before)]
    if ( !afterNA)
      objsDT3 <- objsDT3[(tagValue >= after)]
    objsDT <- objsDT[objsDT[[.cacheTableHashColName()]] %in%
                       unique(objsDT3[[.cacheTableHashColName()]])] # faster than data.table join
  }
  objsDT
}

CacheDTFilesAll <- function(CacheDT, cacheRepo, format = getOption("reproducible.cacheSaveFormat", "rds")) {
  mainFiles <- CacheStoredFile(cacheRepo, unique(CacheDT$cacheId), format = format)
  wh <- CacheDT[["tagKey"]] %in% c(userTag_CacheRasterRelPath, userTag_SideEffectRelPath)
  auxFiles <- CacheDT[[.cacheTableTagColName()]][wh]
  if (NROW(auxFiles)) {
    auxFilesAbsPath <- file.path(cacheRepo, auxFiles)
    mainFiles <- c(mainFiles, auxFilesAbsPath)
  }
  mainFiles
}

