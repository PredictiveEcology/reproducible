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
#' between `after` or `before`) from the repository located at
#' `cachePath` of the sim object, if `sim` is provided, or located in
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
             fun = fun, cacheId = cacheId, sorted = FALSE),
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
      return(invisible(.emptyCacheTable))
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
        if (exists(x, envir = .pkgEnv)) {
          rm(list = x, envir = .pkgEnv)
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
      filesToRemove <- objsDT[grepl(pattern = "cacheRaster", tagKey)][[.cacheTableTagColName()]]
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
          dirLs <- unlist(lapply(basename(filesToRemove), grep, dirLs, value = TRUE))
          cacheSize <- sum(cacheSize, file.size(dirLs))
        }
        # }
      }

      if (isInteractive() && isTRUE(!is.na(cacheSize))) {
        class(cacheSize) <- "object_size"
        formattedCacheSize <- format(cacheSize, "auto")
        if (isTRUE(ask)) {
          messageQuestion(
            "Your size of your selected objects is ", formattedCacheSize, ".\n",
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
      rmFromCache(x, objToGet, conn = conn, drv = drv) # many = TRUE)
      if (isTRUE(getOption("reproducible.useMemoise"))) {
        if (exists(x, envir = .pkgEnv)) {
          suppressWarnings(rm(list = objToGet, envir = .pkgEnv[[x]]))
        }
      }

      # browser(expr = exists("rmFC"))
      # }
    }
    # memoise::forget(.loadFromLocalRepoMem)
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
                                 drv = getDrv(getOption("reproducible.drv", NULL)),
                                 conn = getOption("reproducible.conn", NULL),
                                 verbose = getOption("reproducible.verbose"), ...) {
  standardGeneric("showCache")
})

#' @export
#' @rdname viewCache
setMethod(
  "showCache",
  definition = function(x, userTags, after = NULL, before = NULL, fun = NULL,
                        cacheId = NULL, drv, conn, ...) {
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
      if (!is.null(cacheId)) {
        objsDT <- rbindlist(lapply(cacheId, showCacheFast, cachePath = x))
      } else {
        objsDT <- rbindlist(lapply(
          dir(CacheStorageDir(x),
              pattern = CacheDBFileSingleExt(),
              full.names = TRUE
          ),
          loadFile
        ))
      }
      if (NROW(objsDT) == 0) {
        return(invisible(.emptyCacheTable))
      }
    } else {
      if (is.null(conn)) {
        conn <- dbConnectAll(drv, cachePath = x, create = FALSE)
        if (is.null(conn)) {
          return(invisible(.emptyCacheTable))
        }
        on.exit(DBI::dbDisconnect(conn), add = TRUE)
      }
      if (!CacheIsACache(x, drv = drv, conn = conn)) {
        return(invisible(.emptyCacheTable))
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
      # if (useDBI()) {
      if (!afterNA || !beforeNA) {
        objsDT3 <- objsDT[tagKey == "accessed"]
        if (!beforeNA) {
          objsDT3 <- objsDT3[(tagValue <= before)]
        }
        if (!afterNA) {
          objsDT3 <- objsDT3[(tagValue >= after)]
        }
        # objsDT3 <- objsDT3[!duplicated(cacheId)]
        # browser(expr = exists("zzzz"))
        # objsDT <- objsDT[cacheId %in% objsDT3$cacheId]
        objsDT <- objsDT[objsDT[[.cacheTableHashColName()]] %in%
          unique(objsDT3[[.cacheTableHashColName()]])] # faster than data.table join
      }
      # }
      if (length(userTags) > 0) {
        if (isTRUE(list(...)$regexp) | is.null(list(...)$regexp)) {
          objsDTs <- list()
          for (ut in userTags) {
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
    args <- append(list(x = x, after = after, before = before, userTags = userTags), list(...))

    objsDTAll <- suppressMessages(showCache(x, verbose = FALSE, sorted = FALSE))
    objsDT <- do.call(showCache, args = args)
    keep <- unique(objsDT[[.cacheTableHashColName()]])
    eliminate <- unique(objsDTAll[[.cacheTableHashColName()]][
      !(objsDTAll[[.cacheTableHashColName()]] %in% keep)
    ])

    if (length(eliminate)) {
      clearCache(x, eliminate, verbose = FALSE, regexp = FALSE, ask = ask)
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
          !tagKey %in% c("format", "name", "date", "cacheId"), list(tagKey, tagValue)
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
    messageColoured(messSet, verboseLevel = 0, verbose = verbose)
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

showCacheFast <- function(cacheId, cachePath = getOption("reproducible.cachePath")) {
  fileexists <- dir(CacheStorageDir(cachePath), full.names = TRUE,
                    pattern = paste0(cacheId, "\\.dbFile"))
  if (length(fileexists)) {
    sc <- loadFile(fileexists)
  } else {
    sc <- showCache(userTags = cacheId, verbose = FALSE)[cacheId %in% cacheId]
  }
  sc[]
}

sortedOrRegexp <- c("sorted", "regexp", "ask")
