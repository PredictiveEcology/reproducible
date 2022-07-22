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
#' @rdname convertPaths
convertRasterPaths <- function(x, patterns, replacements) {
  if (is.list(x)) {
    x <- lapply(x, convertRasterPaths, patterns, replacements)
  } else if (!is.null(x)) {
    .requireNamespace("raster", stopOnFALSE = TRUE)
    if (is.character(x)) {
      if (length(x) > 1) {
        x <- lapply(x, convertRasterPaths, patterns, replacements)
      } else {
        x <- raster::raster(x)
      }
    }

    x@file@name <- convertPaths(Filenames(x, allowMultiple = FALSE), patterns, replacements)
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
Filenames <- function(obj, allowMultiple = TRUE) {
  UseMethod("Filenames")
}

#' @export
Filenames.default <- function(obj, allowMultiple = TRUE) {
  fns <- if (any(inherits(obj, "SpatVector"), inherits(obj, "SpatRaster"))) {
    if (!requireNamespace("terra") )
      stop("Please install terra package")
    terra::sources(obj)
  } else if (inherits(obj, "RasterStack")) {
    FilenameRasterStack(obj, allowMultiple = allowMultiple)
  } else if (inherits(obj, "Raster")) {
    FilenameRaster(obj, allowMultiple = allowMultiple)
  } else {
    NULL
  }
  fns
}

FilenameRaster <- function(obj, allowMultiple = TRUE) {
    fn <- raster::filename(obj)
    if (length(fn) == 0)
      fn <- ""
    if (isTRUE(allowMultiple))
      if (endsWith(fn, suffix = "grd"))
        fn <- c(fn, gsub("grd$", "gri", fn))
    normPath(fn)
}

FilenameRasterStack <- function(obj, allowMultiple = TRUE) {
    fn <- unlist(lapply(seq_along(names(obj)), function(index)
      Filenames(obj[[index]], allowMultiple = allowMultiple)))

    dups <- duplicated(fn)
    if (any(dups)) {
      theNames <- names(fn)
      fn <- fn[!dups]
      names(fn) <- theNames[!dups]
    }

    return(fn)
}

#' @export
Filenames.environment <- function(obj, allowMultiple = TRUE) {
    rasterFilename <- Filenames(as.list(obj), allowMultiple = allowMultiple)
    rasterFilenameDups <- lapply(rasterFilename, duplicated)

    if (any(unlist(rasterFilenameDups))) {
      rasterFilename <- rasterFilename[!unlist(rasterFilenameDups)]
    }
    return(rasterFilename)
}

#' @export
Filenames.list <- function(obj, allowMultiple = TRUE) {
    ## convert a list to an environment -- this is to align it with a simList and environment
    if (is.null(names(obj))) {
      names(obj) <- as.character(seq(obj))
    }
    unlist(lapply(obj, function(o) Filenames(o, allowMultiple = allowMultiple)))
}
