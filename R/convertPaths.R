#' Change the absolute path of a file
#'
#' \code{convertPaths} is simply a wrapper around \code{gsub} for changing the
#' first part of a path.
#' \code{convertRasterPaths} is useful for changing the path to a file-backed
#' raster (e.g., after copying the file to a new location).
#'
#' @param x             For \code{convertPaths}, a character vector of file paths.
#'                      For \code{convertRasterPaths}, a disk-backed \code{RasterLayer}
#'                      object, or a list of such rasters.
#' @param patterns      Character vector containing a pattern to match (see \code{?gsub}).
#' @param replacements  Character vector of the same length of \code{patterns}
#'                      containing replacement text (see \code{?gsub}).
#'
#' @author Eliot McIntire and Alex Chubaty
#' @export
#' @rdname convertPaths
#'
#' @examples
#' filenames <- c("/home/user1/Documents/file.txt", "/Users/user1/Documents/file.txt")
#' oldPaths <- dirname(filenames)
#' newPaths <- c("/home/user2/Desktop", "/Users/user2/Desktop")
#' convertPaths(filenames, oldPaths, newPaths)
#'
#' r1 <- raster::raster(system.file("external/test.grd", package = "raster"))
#' r2 <- raster::raster(system.file("external/rlogo.grd", package = "raster"))
#' rasters <- list(r1, r2)
#' oldPaths <- system.file("external", package = "raster")
#' newPaths <- file.path("~/rasters")
#' rasters <- convertRasterPaths(rasters, oldPaths, newPaths)
#' lapply(rasters, raster::filename)
#'
convertPaths <- function(x, patterns, replacements) {
  stopifnot(is(x, "character"))
  stopifnot(length(patterns) == length(replacements))
  patterns <- normPath(patterns)
  replacements <- normPath(replacements)
  x <- normPath(x)
  for (i in seq_along(patterns)) {
    x <- gsub(x = x, pattern = patterns[i], replacement = replacements[i])
  }
  normPath(x)
}

#' @author Eliot McIntire and Alex Chubaty
#' @export
#' @importFrom raster filename raster
#' @rdname convertPaths
convertRasterPaths <- function(x, patterns, replacements) {
  if (is.list(x)) {
    x <- lapply(x, convertRasterPaths, patterns, replacements)
  } else if (!is.null(x)) {
    if (is.character(x)) {
      if (length(x) > 1) {
        x <- lapply(x, convertRasterPaths, patterns, replacements)
      } else {
        x <- raster(x)
      }
    }

    x@file@name <- convertPaths(filename(x), patterns, replacements)
  }
  x # handles null case
}

#' Return the filename(s) from a \code{Raster*} object
#'
#' This is mostly just a wrapper around \code{filename} from the \pkg{raster} package, except that
#' instead of returning an empty string for a \code{RasterStack} object, it will return a vector of
#' length >1 for \code{RasterStack}.
#'
#' @param obj A \code{Raster*} object (i.e., \code{RasterLayer}, \code{RasterStack}, \code{RasterBrick})
#' @param allowMultiple Logical. If \code{TRUE}, the default, then all relevant
#'   filenames will be returned, i.e., in cases such as \code{.grd} where multiple files
#'   are required. If \code{FALSE}, then only the first file will be returned,
#'   e.g., \code{filename.grd}, in the case of default Raster format in R.
#'
#' @author Eliot McIntire
#' @export
#' @rdname Filenames
setGeneric("Filenames", function(obj, allowMultiple = TRUE) {
  standardGeneric("Filenames")
})

#' @export
#' @rdname Filenames
setMethod(
  "Filenames",
  signature = "ANY",
  definition = function(obj, allowMultiple) {
    NULL
})

#' @export
#' @rdname Filenames
setMethod(
  "Filenames",
  signature = "Raster",
  definition = function(obj, allowMultiple = TRUE) {
    fn <- filename(obj)
    if (exists("._Filenames_1")) browser()
    if (length(fn) == 0)
      fn <- ""
    # browser(expr = exists("._Filenames_1"))
    if (isTRUE(allowMultiple))
      if (endsWith(fn, suffix = "grd"))
        fn <- c(fn, gsub("grd$", "gri", fn))
    normPath(fn)
})

#' @export
#' @rdname Filenames
setMethod(
  "Filenames",
  signature = "RasterStack",
  definition = function(obj, allowMultiple = TRUE) {
    fn <- unlist(lapply(seq_along(names(obj)), function(index)
      Filenames(obj[[index]], allowMultiple = allowMultiple)))

    dups <- duplicated(fn)
    if (any(dups)) {
      theNames <- names(fn)
      fn <- fn[!dups]
      names(fn) <- theNames[!dups]
    }

    return(fn)
})

#' @export
#' @rdname Filenames
setMethod(
  "Filenames",
  signature = "environment",
  definition = function(obj, allowMultiple = TRUE) {
    rasterFilename <- Filenames(as.list(obj), allowMultiple = allowMultiple)
    # rastersLogicalList <- isOrHasRaster(obj)
    # rastersLogical <- vapply(rastersLogicalList, function(x) any(unlist(x)), logical(1))
    # rastersLogicalLong <- unlist(rastersLogicalList)
    # rasterFilename <- NULL
    # if (any(rastersLogical)) {
    #   rasterNames <- names(rastersLogical)[rastersLogical]
    #   if (!is.null(rasterNames)) {
    #     no <- names(obj)
    #     names(no) <- no
    #     ## TODO: sapply is not type-safe; use vapply
    #     nestedOnes <- lapply(no, function(rn) grep(paste0("^", rn, "\\."), rasterNames, value = TRUE))
    #     nestedOnes1 <- nestedOnes[sapply(nestedOnes, function(x) length(x) > 0)]
    #     nonNested <- nestedOnes[sapply(nestedOnes, function(x) length(x) == 0)]
    #     nonNestedRasterNames <- rasterNames[rasterNames %in% names(nonNested)]
    #     diskBacked <- sapply(mget(nonNestedRasterNames, envir = obj), fromDisk)
    #
    #     names(rasterNames) <- rasterNames
    #     rasterFilename <- if (sum(diskBacked) > 0) {
    #       browser()
    #       lapply(mget(rasterNames[diskBacked], envir = obj), Filenames,
    #              allowMultiple = allowMultiple)
    #     } else {
    #       NULL
    #     }
    #     if (length(nestedOnes1) > 0) {
    #       rasterFilename2 <- sapply(mget(names(nestedOnes1), envir = obj), Filenames,
    #                                 allowMultiple = allowMultiple)
    #       rasterFilename <- c(rasterFilename, rasterFilename2)
    #     }
    #   }
    # }
    rasterFilenameDups <- lapply(rasterFilename, duplicated)

    if (any(unlist(rasterFilenameDups))) {
      rasterFilename <- rasterFilename[!unlist(rasterFilenameDups)]
    }
    return(rasterFilename)
})

#' @export
#' @rdname Filenames
setMethod(
  "Filenames",
  signature = "list",
  definition = function(obj, allowMultiple = TRUE) {
    ## convert a list to an environment -- this is to align it with a simList and environment
    if (is.null(names(obj))) {
      names(obj) <- as.character(seq(obj))
    }
    unlist(lapply(obj, function(o) Filenames(o, allowMultiple = allowMultiple)))
    # Filenames(as.environment(obj), allowMultiple = allowMultiple)
})
