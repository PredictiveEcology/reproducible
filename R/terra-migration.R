#' Get min or maximum value of a (Spat)Raster
#'
#' During the transition from raster to terra, some functions are not drop in
#' replacements, such as `minValue` and `maxValue` became `terra::minmax`. This
#' helper allows one function to be used, which calls the correct max or min
#' function, depending on whether the object is a `Raster` or `SpatRaster`.
#'
#' @param x A `Raster` or `SpatRaster` object.
#'
#' @return
#' A vector (not matrix as in `terra::minmax`) with the minimum or maximum
#' value on the `Raster` or `SpatRaster`, one value per layer.
#'
#' @export
#' @rdname terra-migration
#'
#' @examples
#' if (requireNamespace("terra", quietly = TRUE)) {
#'   ras <- terra::rast(terra::ext(0, 10, 0, 10), vals = 1:100)
#'   maxFn(ras)
#'   minFn(ras)
#' }
minFn <- function(x) {
  minmaxFn(x, "min")
}

#' @export
#' @rdname terra-migration
maxFn <- function(x) {
  minmaxFn(x, "max")
}

#' @importFrom utils head tail
minmaxFn <- function(x, which = "max") {
  out <- NULL
  if (is(x, "Raster")) {
    .requireNamespace("raster", stopOnFALSE = TRUE)
    fn <- get(paste0(which, "Value"), envir = asNamespace("raster"))
    out <- fn(x)
  } else {
    .requireNamespace("terra", stopOnFALSE = TRUE)
    fn <- ifelse(identical(which, "max"), "tail", "head")
    fn <- getFromNamespace(fn, ns = "utils")
    if (!terra::hasMinMax(x))
      x <- terra::setMinMax(x)
    out <- fn(terra::minmax(x), 1)[1, ]
  }
  if (is.null(out)) {
    stop("To use maxFn or minFn, you need either terra or raster package installed")
  }

  out
}

#' @export
#' @param ... Passed to the functions in `raster` or `terra`, as needed.
#' @rdname terra-migration
dataType2 <- function(x, ...) {
  if (is(x, "Raster")) {
    raster::dataType(x)
  } else {
    terra::datatype(x, ...)
  }
}

#' @export
#' @rdname terra-migration
nlayers2 <- function(x) {
  if (is(x, "Raster")) {
    raster::nlayers(x)
  } else {
    terra::nlyr(x)
  }
}


#' @export
#' @rdname terra-migration
values2 <- function(x, ...) {
  if (is(x, "Raster")) {
    raster::values(x, ...)
  } else {
    if (.isSpatRaster(x)) {
      terra::values(x, ..., mat = nlayers2(x) > 1)
    } else {
      terra::values(x, ...)
    }
  }
}
