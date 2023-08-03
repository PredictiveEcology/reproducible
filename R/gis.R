#' Faster operations on rasters (DEPRECATED because `terra::mask` is fast)
#'
#' Deprecated. Use [maskTo()].
#'
#' @param x  A `Raster*` object.
#'
#' @param y  A `SpatialPolygons` object. If it is not in the same projection
#'           as `x`, it will be reprojected on the fly to that of `x`
#'
#' @param cores An `integer*` or `'AUTO'`. This will be used if gdalwarp is
#'           triggered. `'AUTO'` will calculate 90% of the total
#'           number of cores in the system, while an integer or rounded
#'           float will be passed as the exact number of cores to be used.
#' @param skipDeprecastedMsg Logical. If `TRUE`, then the message about this function
#'           being deprecated will be suppressed.
#'
#' @param useGDAL Deprecated. Logical or `"force"`. This is defunct; internals now can use
#'     `terra` if `options("reproducible.useTerra" = TRUE)`, which is not (yet) the default.
#'
#' @param ... Currently unused.
#'
#' @inheritParams Cache
#'
#' @inheritParams projectInputs
#'
#' @return A `Raster*` object, masked (i.e., smaller extent and/or several pixels converted to NA)
#'
#' @author Eliot McIntire
#' @export
#'
fastMask <- function(x, y, cores = NULL, useGDAL = FALSE,
                     verbose = getOption("reproducible.verbose", 1), ...,
                     skipDeprecastedMsg = FALSE) {
  if (!skipDeprecastedMsg) {
    .Deprecated("mask", "terra", "fastMask is deprecated; using maskTo and terra")
  }
  touches <- list(...)$touches
  maskTo(from = x, maskTo = y, touches = isTRUE(touches))
}

bigRastersTmpFolder <- function() checkPath(tempdir2(sub = "bigRasters"), create = TRUE)

bigRastersTmpFile <- function() file.path(bigRastersTmpFolder(), "bigRasInput.tif")

checkColors <- function(x) {
  origColors <- .getColors(x)
  origMaxValue <- maxFn(x)
  origMinValue <- minFn(x)
  list(origColors = origColors[[1]], origMinValue = origMinValue, origMaxValue = origMaxValue)
}

rebuildColors <- function(x, origColors) {
  if (isTRUE(all(origColors$origMinValue != minFn(x)) || all(origColors$origMaxValue != maxFn(x)) ||
    !identical(.getColors(x)[[1]], origColors$origColors))) {
    colorSequences <- unlist(lapply(seq_along(origColors$origMinValue), function(ind) {
      origColors$origMinValue[ind]:origColors$origMaxValue[ind]
    }))
    if (isTRUE(length(origColors$origColors) == length(colorSequences))) {
      newSeq <- minFn(x):maxFn(x)
      oldSeq <- origColors$origMinValue:origColors$origMaxValue
      whFromOld <- match(newSeq, oldSeq)
      x@legend@colortable <- origColors$origColors[whFromOld]
    }
  }
  x
}

.getColors <- function(object) {
  if (is(object, "SpatRaster")) {
    cols <- terra::coltab(object)
  } else {
    nams <- names(object)
    cols <- lapply(nams, function(x) {
      as.character(object[[x]]@legend@colortable)
    })
    names(cols) <- nams
  }
  return(cols)
}
