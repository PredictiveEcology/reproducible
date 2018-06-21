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
        x <- raster()
      }
    }

    x@file@name <- convertPaths(filename(x), patterns, replacements)
  }
  x # handles null case
}
