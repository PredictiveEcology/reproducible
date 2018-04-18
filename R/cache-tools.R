#' @param x A simList or a directory containing a valid archivist repository
#' @param after A time (POSIX, character understandable by data.table).
#'                  Objects cached after this time will be shown or deleted.
#' @param before A time (POSIX, character understandable by data.table).
#'                   Objects cached before this time will be shown or deleted.
#' @param ... Other arguments. Currently unused.
#'
#' If neither \code{after} or \code{before} are provided, nor \code{userTags},
#'  then all objects will be removed.
#' If both \code{after} and \code{before} are specified, then all objects between \code{after} and
#' \code{before} will be deleted.
#' If \code{userTags} is used, this will override \code{after} or \code{before}.
#'
#' @return Will clear all (or that match \code{userTags}, or between \code{after} or \code{before})
#' objects from the repository located at \code{cachePath} of the sim object,
#' if \code{sim} is provided, or located in \code{cacheRepo}. Also returns a data.table invisibly
#' of the removed items.
#'
#' @note If the cache is larger than 10MB, and clearCache is used, there will be a message
#' and a pause, if interactive, to prevent accidentally deleting of a large cache repository.
#'
#'
#' @export
#' @importFrom archivist rmFromLocalRepo searchInLocalRepo
#' @importFrom methods setGeneric setMethod
#' @param userTags Character vector. If used, this will be used in place of the \code{after} and
#'                 \code{before}. Specifying one or more \code{userTag} here will
#'                 clear all objects that
#'                 match those tags. Matching is via regular expresssion, meaning
#'                 partial matches
#'                 will work unless strict beginning (^) and end ($) of string
#'                 characters are used. Matching
#'                 will be against any of the 3 columns returned by \code{showCache()},
#'                 i.e., artifact, tagValue or tagName. Also, length \code{userTags} > 1,
#'                 then matching is by `and`. For `or` matching, use | in a single character
#'                 string. See examples.
#'
#' @rdname viewCache
#'
#' @example inst/examples/example_Cache.R
#'
setGeneric("clearCache", function(x, userTags = character(), after, before, ...) {
  standardGeneric("clearCache")
})

#' @export
#' @rdname viewCache
#' @importFrom archivist createLocalRepo
setMethod(
  "clearCache",
  definition = function(x, userTags, after, before, ...) {
    if (missing(x)) {
      message("x not specified; using ", getOption("spades.cachePath"))
      x <- getOption("spades.cachePath")
    }
    if (is(x, "simList")) x <- x@paths$cachePath

    # Check if no args -- faster to delete all then make new empty repo for large repos
    if (all(missing(userTags), missing(after), missing(before))) {
      if (interactive()) {
        cacheSize <- sum(file.size(dir(x, full.names = TRUE, recursive = TRUE)))
        class(cacheSize) <- "object_size"
        formattedCacheSize <- format(cacheSize, "auto")

        if (cacheSize > 1e7) {
          message("Your current cache size is ", formattedCacheSize,
                  " Are you sure you would like to delete it all? Y or N")
          rl <- readline()
          if (!identical(toupper(rl), "Y")) {
            message("Aborting clearCache")
            return(invisible())
          }
        }
      }
      unlink(file.path(x, "gallery"), recursive = TRUE)
      unlink(file.path(x, "rasters"), recursive = TRUE)
      unlink(file.path(x, "backpack.db"))
      checkPath(x, create = TRUE)
      createLocalRepo(x)
      memoise::forget(loadFromLocalRepoMem)
      return(invisible())
    }

    if (missing(after)) after <- "1970-01-01"
    if (missing(before)) before <- Sys.time() + 1e5

    args <- append(list(x = x, after = after, before = before, userTags = userTags),
                   list(...))

    objsDT <- do.call(showCache, args = args)

    if (interactive()) {
      rdaFiles <- file.path(x, "gallery", paste0(unique(objsDT$artifact), ".rda"))
      cacheSize <- sum(file.size(rdaFiles))
    }

    if (NROW(objsDT)) {
      rastersInRepo <- objsDT[grepl(pattern = "class", tagKey) & grepl(pattern = "Raster", tagValue)] # only Rasters* class
      if (all(!is.na(rastersInRepo$artifact)) && NROW(rastersInRepo)>0) {
        suppressWarnings(rasters <- lapply(rastersInRepo$artifact, function(ras) {
          loadFromLocalRepo(ras, repoDir = x, value = TRUE)
        }))
        filesToRemove <- tryCatch(unlist(lapply(rasters, function(x) filename(x))), 
                                  error = function(x) NULL)
        if (!is.null(filesToRemove)) {
          filesToRemove <- gsub(filesToRemove, pattern = ".{1}$", replacement = "*")
          if (interactive()) {
            dirLs <- dir(dirname(filesToRemove), full.names = TRUE)
            dirLs <- unlist(lapply(basename(filesToRemove), grep, dirLs, value = TRUE) )
            cacheSize <- sum(cacheSize, file.size(dirLs))
          }
        }

      }

      if (interactive()) {
        class(cacheSize) <- "object_size"
        formattedCacheSize <- format(cacheSize, "auto")
        if (cacheSize > 1e7) {
          message("Your current cache size is ", formattedCacheSize,
                  " Are you sure you would like to delete it all? Y or N")
          rl <- readline()
          if (!identical(rl, "Y")) {
            message("Aborting clearCache")
            return(invisible())
          }
        }
      }

      if (all(!is.na(rastersInRepo$artifact)) && NROW(rastersInRepo)>0) {
        unlink(filesToRemove)
      }

      # memoise
      # fn <- unique(objsDT[tagKey == "memoisedFunction", tagValue])
      # forgetResults <- lapply(fn, function(f)
      #   forget(.memoisedCacheFuns[[f]])
      # )

      suppressWarnings(rmFromLocalRepo(unique(objsDT$artifact), x, many = TRUE))
    }
    memoise::forget(loadFromLocalRepoMem)
    return(invisible(objsDT))
})

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
#' @seealso \code{\link{mergeCache}}, \code{\link[archivist]{splitTagsLocal}}.
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
      message("x not specified; using ", getOption("spades.cachePath"))
      x <- getOption("spades.cachePath")
    }
    if (missing(after)) after <- "1970-01-01"
    if (missing(before)) before <- Sys.time() + 1e5
    if (is(x, "simList")) x <- x@paths$cachePath

    objsDT <- showLocalRepo(x) %>% data.table()
    setkeyv(objsDT, "md5hash")
    if (NROW(objsDT) > 0) {
      objsDT <- data.table(splitTagsLocal(x), key = "artifact")
      objsDT3 <- objsDT[tagKey == "accessed"][(tagValue <= before) &
                                                (tagValue >= after)][!duplicated(artifact)]
      objsDT <- objsDT[artifact %in% objsDT3$artifact]
      if (length(userTags) > 0) {
        for (ut in userTags) {
          objsDT2 <- objsDT[
            grepl(tagValue, pattern = ut)   |
              grepl(tagKey, pattern = ut) |
              grepl(artifact, pattern = ut)]
          setkeyv(objsDT2, "artifact")
          shortDT <- unique(objsDT2, by = "artifact")[, artifact]
          objsDT <- if (NROW(shortDT)) objsDT[shortDT] else objsDT[0] # merge each userTags
        }
      }
    }
    .messageCacheSize(x)
    objsDT
})

#' @rdname viewCache
setGeneric("keepCache", function(x, userTags = character(), after, before, ...) {
  standardGeneric("keepCache")
})

#' @export
#' @rdname viewCache
setMethod(
  "keepCache",
  definition = function(x, userTags, after, before, ...) {
    if (missing(x)) {
      message("x not specified; using ", getOption("spades.cachePath"))
      x <- getOption("spades.cachePath")
    }
    if (missing(after)) after <- "1970-01-01"
    if (missing(before)) before <- Sys.time() + 1e5
    if (is(x, "simList")) x <- x@paths$cachePath

    args <- append(list(x = x, after = after, before = before, userTags = userTags),
                   list(...))

    objsDTAll <- showCache(x)
    objsDT <- do.call(showCache, args = args)
    keep <- unique(objsDT$artifact)
    eliminate <- unique(objsDTAll$artifact[!(objsDTAll$artifact %in% keep)])

    if (length(eliminate)) {
      eliminate <- paste(eliminate, collapse = "|")
      clearCache(x, eliminate)
    }
    return(objsDT)
})


##############################################################
##############################################################
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
    objectList <- lapply(artifacts, loadFromLocalRepo,
                         repoDir = cacheFrom, value = TRUE)
    mapply(outputToSave = objectList, artifact = artifacts,
           function(outputToSave, artifact) {
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
                                 artifactName = "Cache",
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
    )
    .messageCacheSize(cacheTo)

    return(invisible(cacheTo))
  })


#' @keywords internal
.messageCacheSize <- function(x) {
  fs <- sum(file.size(dir(x, full.names = TRUE, recursive = TRUE)))
  class(fs) <- "object_size"
  message("Total Cache size: ",
          format(fs, "auto"))
}
