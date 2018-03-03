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
setMethod(
  "clearCache",
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

    objsDT <- do.call(showCache, args = args)

    if (NROW(objsDT)) {
      rastersInRepo <- objsDT[grep(tagValue, pattern = "Raster")]
      if (all(!is.na(rastersInRepo$artifact))) {
        suppressWarnings(rasters <- lapply(rastersInRepo$artifact, function(ras) {
          loadFromLocalRepo(ras, repoDir = x, value = TRUE)
        }))
        filesToRemove <- unlist(lapply(rasters, function(x) filename(x)))
        filesToRemove <- gsub(filesToRemove, pattern = ".{1}$", replacement = "*")
        unlink(filesToRemove)
      }

      # memoise
      # fn <- unique(objsDT[tagKey == "memoisedFunction", tagValue])
      # forgetResults <- lapply(fn, function(f)
      #   forget(.memoisedCacheFuns[[f]])
      # )

      suppressWarnings(rmFromLocalRepo(unique(objsDT$artifact), x, many = TRUE))
    }
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
#' @seealso \code{\link[archivist]{splitTagsLocal}}.
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
