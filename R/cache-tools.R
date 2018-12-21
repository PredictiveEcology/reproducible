#' @param x A simList or a directory containing a valid archivist repository
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
#'            to regular expression problem, try \code{FALSE}.
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
#'
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
#' @importFrom archivist rmFromLocalRepo searchInLocalRepo
#' @importFrom data.table setindex
#' @importFrom methods setGeneric setMethod
#' @importFrom utils object.size
#' @rdname viewCache
#'
#' @examples
#' library(raster)
#' try(detach("package:magrittr", unload = TRUE), silent = TRUE) # magrittr,
#'                                     #if loaded, gives an error below
#'
#' tmpDir <- file.path(tempdir(), "reproducible_examples", "Cache")
#' try(clearCache(tmpDir), silent = TRUE) # just to make sure it is clear
#'
#' # Basic use
#' ranNumsA <- Cache(rnorm, 10, 16, cacheRepo = tmpDir)
#'
#' # All same
#' ranNumsB <- Cache(rnorm, 10, 16, cacheRepo = tmpDir) # recovers cached copy
#' ranNumsC <- rnorm(10, 16) %>% Cache(cacheRepo = tmpDir) # recovers cached copy
#' ranNumsD <- Cache(quote(rnorm(n = 10, 16)), cacheRepo = tmpDir) # recovers cached copy
#'
#' # Any minor change makes it different
#' ranNumsE <- rnorm(10, 6) %>% Cache(cacheRepo = tmpDir) # different
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
#' toRemove <- cache1[tagKey=="object.size"][as.numeric(tagValue) > 700]$artifact
#' clearCache(tmpDir, userTags = toRemove)
#' cacheAfter <- showCache(tmpDir, userTags = c("runif")) # Only the small one is left
#'
setGeneric("clearCache", function(x, userTags = character(), after, before,
                                  ask = getOption("reproducible.ask"), ...) {
  standardGeneric("clearCache")
})

#' @export
#' @importFrom archivist createLocalRepo
#' @rdname viewCache
setMethod(
  "clearCache",
  definition = function(x, userTags, after, before, ask, ...) {
    if (missing(x)) {
      message("x not specified; using ", getOption("reproducible.cachePath"))
      x <- getOption("reproducible.cachePath")
    }
    if (is(x, "simList")) x <- x@paths$cachePath

    # Check if no args -- faster to delete all then make new empty repo for large repos
    if (all(missing(userTags), missing(after), missing(before))) {
      if (isInteractive()) {
        cacheSize <- sum(file.size(dir(x, full.names = TRUE, recursive = TRUE)))
        class(cacheSize) <- "object_size"
        formattedCacheSize <- format(cacheSize, "auto")

        if (isTRUE(ask)) {
          if (isInteractive()) {
            message("Your current cache size is ", formattedCacheSize, ".\n",
                    " Are you sure you would like to delete it all? Y or N")
            rl <- readline()
            if (!identical(toupper(rl), "Y")) {
              message("Aborting clearCache")
              return(invisible())
            }
          }
        }
      }
      unlink(file.path(x, "gallery"), recursive = TRUE)
      unlink(file.path(x, "rasters"), recursive = TRUE)
      unlink(file.path(x, "backpack.db"))
      checkPath(x, create = TRUE)
      createLocalRepo(x)
      memoise::forget(.loadFromLocalRepoMem)
      return(invisible())
    }

    if (missing(after)) after <- "1970-01-01"
    if (missing(before)) before <- Sys.time() + 1e5

    args <- append(list(x = x, after = after, before = before, userTags = userTags),
                   list(...))

    objsDT <- do.call(showCache, args = args)

    if (isInteractive()) {
      objSizes <- as.numeric(objsDT[tagKey == "object.size"]$tagValue)
      cacheSize <- sum(objSizes) / 4
      #rdaFiles <- file.path(x, "gallery", paste0(unique(objsDT$artifact), ".rda"))
      #cacheSize <- sum(file.size(rdaFiles))
    }

    if (NROW(objsDT)) {
      rastersInRepo <- objsDT[grepl(pattern = "class", tagKey) &
                                grepl(pattern = "Raster", tagValue)] # only Rasters* class
      if (all(!is.na(rastersInRepo$artifact)) && NROW(rastersInRepo) > 0) {
        rasterObjSizes <- as.numeric(objsDT[artifact %in% rastersInRepo$artifact & tagKey == "object.size"]$tagValue)
        fileBackedRastersInRepo <- rastersInRepo$artifact[rasterObjSizes < 1e5]
        filesToRemove <- lapply(fileBackedRastersInRepo, function(ras) {
          r <- suppressWarnings(loadFromLocalRepo(ras, repoDir = x, value = TRUE))
          tryCatch(filename(r), error = function(e) NULL)
        })

        if (length(filesToRemove)) {
          filesToRemove <- gsub(filesToRemove, pattern = "(\\.).*$", replacement = "\\1*")
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
          if (isInteractive()) {
            message("Your size of your selected objects is ", formattedCacheSize, ".\n",
                    " Are you sure you would like to delete it all? Y or N")
            rl <- readline()
            if (!identical(toupper(rl), "Y")) {
              message("Aborting clearCache")
              return(invisible())
            }
          }
        }
      }

      if (all(!is.na(rastersInRepo$artifact)) && NROW(rastersInRepo) > 0) {
        unlink(filesToRemove)
      }

      suppressWarnings(rmFromLocalRepo(unique(objsDT$artifact), x, many = TRUE))
    }
    memoise::forget(.loadFromLocalRepoMem)
    try(setindex(objsDT, NULL), silent = TRUE)
    return(invisible(objsDT))
})

#' @rdname viewCache
#' @export
#' @param secs The number of seconds to pass to \code{clearCache(after = secs)}. If missing,
#'             the default, then it will delete the most recent entry in the Cache
#' @param ... Passed to \code{clearCache}, e.g., \code{ask}, \code{userTags}
#'
#' @details
#' \code{cc(secs)} is just a shortcut for \code{clearCache(repo = Paths$cachePath, after = secs)},
#' i.e., to remove any cache entries touched in the last \code{secs} seconds.
#' @examples
#' Cache(rnorm, 1)
#' Cache(rnorm, 2)
#' Cache(rnorm, 3)
#' showCache() # shows all 3 entries
#' cc(ask = FALSE)
#' showCache()
cc <- function (secs, ...) {
  if (missing(secs)) {
    message("No time provided; removing the most recent entry to the Cache")
    suppressMessages(theCache <- reproducible::showCache())
    accessed <- data.table::setkey(theCache[ tagKey == "accessed"], tagValue)
    clearCache(userTags = tail(accessed, 1)$tagValue, ...)
  } else {
    reproducible::clearCache(after = Sys.time() - secs, ...)
  }

}


#' Examining and modifying the cache
#'
#' These are convenience wrappers around \code{archivist} package functions.
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
#' @importFrom archivist splitTagsLocal
#' @importFrom data.table data.table set setkeyv
#' @rdname viewCache
#' @seealso \code{\link{mergeCache}}, \code{\link[archivist]{splitTagsLocal}}. Many more examples
#' in \code{\link{Cache}}
#'
setGeneric("showCache", function(x, userTags = character(), after, before, ...) {
  standardGeneric("showCache")
})

#' @export
#' @rdname viewCache
setMethod(
  "showCache",
  definition = function(x, userTags, after, before, ...) {
    if (missing(x)) {
      message("x not specified; using ", getOption("reproducible.cachePath"))
      x <- getOption("reproducible.cachePath")
    }
    if (missing(after)) after <- "1970-01-01"
    if (missing(before)) before <- Sys.time() + 1e5
    if (is(x, "simList")) x <- x@paths$cachePath

    # Clear the futures that are resolved
    .onLinux <- .Platform$OS.type == "unix" && unname(Sys.info()["sysname"]) == "Linux"
    if (.onLinux) {
      if (suppressWarnings(requireNamespace("future", quietly = TRUE, warn.conflicts = FALSE)))
        checkFutures()
    }

    objsDT <- showLocalRepo(x) %>% data.table()
    setkeyv(objsDT, "md5hash")
    if (NROW(objsDT) > 0) {
      objsDT <- data.table(splitTagsLocal(x), key = "artifact")
      objsDT3 <- objsDT[tagKey == "accessed"][(tagValue <= before) &
                                                (tagValue >= after)][!duplicated(artifact)]
      objsDT <- objsDT[artifact %in% objsDT3$artifact]
      if (length(userTags) > 0) {
        if (isTRUE(list(...)$regexp) | is.null(list(...)$regexp)) {
          for (ut in userTags) {
            #objsDT$artifact %in% ut
            objsDT2 <- objsDT[
              grepl(tagValue, pattern = ut) |
                grepl(tagKey, pattern = ut) |
                grepl(artifact, pattern = ut)]
            setkeyv(objsDT2, "artifact")
            shortDT <- unique(objsDT2, by = "artifact")[, artifact]
            objsDT <- if (NROW(shortDT)) objsDT[shortDT] else objsDT[0] # merge each userTags
          }
        } else {
          objsDT2 <- objsDT[artifact %in% userTags | tagKey %in% userTags | tagValue %in% userTags]
          setkeyv(objsDT2, "artifact")
          shortDT <- unique(objsDT2, by = "artifact")[, artifact]
          objsDT <- if (NROW(shortDT)) objsDT[shortDT] else objsDT[0] # merge each userTags
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
      .messageCacheSize(x, artifacts = unique(objsDT$artifact))
    objsDT
})

#' @rdname viewCache
setGeneric("keepCache", function(x, userTags = character(), after, before,
                                 ask  = getOption("reproducible.ask"), ...) {
  standardGeneric("keepCache")
})

#' @export
#' @rdname viewCache
setMethod(
  "keepCache",
  definition = function(x, userTags, after, before, ask, ...) {
    if (missing(x)) {
      message("x not specified; using ", getOption("reproducible.cachePath"))
      x <- getOption("reproducible.cachePath")
    }
    if (missing(after)) after <- "1970-01-01"
    if (missing(before)) before <- Sys.time() + 1e5
    if (is(x, "simList")) x <- x@paths$cachePath

    args <- append(list(x = x, after = after, before = before, userTags = userTags),
                   list(...))

    objsDTAll <- suppressMessages(showCache(x))
    objsDT <- do.call(showCache, args = args)
    keep <- unique(objsDT$artifact)
    eliminate <- unique(objsDTAll$artifact[!(objsDTAll$artifact %in% keep)])

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
#'
#' @details
#' This is still experimental
#'
#' @return The character string of the path of \code{cacheTo}, i.e., not the
#' objects themselves.
#'
#' @rdname mergeCache
setGeneric("mergeCache", function(cacheTo, cacheFrom) {
  standardGeneric("mergeCache")
})

#' @export
#' @rdname mergeCache
setMethod(
  "mergeCache",
  definition = function(cacheTo, cacheFrom) {
    suppressMessages(cacheFromList <- showCache(cacheFrom))
    suppressMessages(cacheToList <- showCache(cacheTo))

    artifacts <- unique(cacheFromList$artifact)
    objectList <- lapply(artifacts, function(artifact) {
      if (!(artifact %in% cacheToList$artifact)) {
        outputToSave <- try(loadFromLocalRepo(artifact, repoDir = cacheFrom, value = TRUE))
        if (is(outputToSave, "try-error")) {
          message("Continuing to load others")
          outputToSave <- NULL
        }

        ## Save it
        written <- FALSE
        if (is(outputToSave, "Raster")) {
          outputToSave <- .prepareFileBackedRaster(outputToSave, repoDir = cacheTo)
        }
        userTags <- cacheFromList[artifact][!tagKey %in% c("format", "name", "class", "date", "cacheId"),
                                            list(tagKey, tagValue)]
        userTags <- c(paste0(userTags$tagKey, ":", userTags$tagValue))
        while (!written) {
          saved <- suppressWarnings(try(
            saveToLocalRepo(outputToSave, repoDir = cacheTo,
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
        message(artifact, " copied")
        outputToSave
      } else {
        message("Skipping ", artifact, "; already in ", cacheTo)
      }
    })
    .messageCacheSize(cacheTo)

    return(invisible(cacheTo))
})

#' @keywords internal
.messageCacheSize <- function(x, artifacts = NULL) {
  a <- showLocalRepo2(x);
  b <- a[startsWith(a$tag, "object.size"),]
  fsTotal <- sum(as.numeric(unlist(lapply(strsplit(b$tag, split = ":"), function(x) x[[2]])))) / 4
  fsTotalRasters <- sum(file.size(dir(file.path(x, "rasters"), full.names = TRUE, recursive = TRUE)))
  fsTotal <- fsTotal + fsTotalRasters
  class(fsTotal) <- "object_size"
  preMessage1 <- "  Total (including Rasters): "

  b <- a[a$artifact %in% artifacts & startsWith(a$tag, "object.size"),]
  fs <- sum(as.numeric(unlist(lapply(strsplit(b$tag, split = ":"), function(x) x[[2]])))) / 4

  class(fs) <- "object_size"
  preMessage <- "  Selected objects (not including Rasters): "

  message("Cache size: ")
  message(preMessage1, format(fsTotal, "auto"))
  message(preMessage, format(fs, "auto"))
}


checkFutures <- function() {
  # This takes a long time -- can't use it if
  resol <- future::resolved(.reproEnv)

  while (any(!resol)) {
    #numSleeps <<- numSleeps+1
    Sys.sleep(0.001)
    resol <- future::resolved(.reproEnv)
  }
  rm(list = names(resol)[resol], envir = .reproEnv)
}
