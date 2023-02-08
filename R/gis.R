#' Faster operations on rasters (DEPRECATED as `terra::mask` is fast)
#'
#' This alternative to `raster::mask` is included here.
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
#'   being deprecated will be suppressed.
#'
#' @param ... Currently unused.
#'
#' @return A `Raster*` object, masked (i.e., smaller extent and/or
#'         several pixels converted to NA)
#'
#' @author Eliot McIntire
#' @export
#' @inheritParams Cache
#' @inheritParams projectInputs.Raster
#' @importFrom raster crop crs extract mask nlayers raster stack tmpDir
#' @importFrom raster xmin xmax ymin ymax fromDisk setMinMax
#' @importFrom sp SpatialPolygonsDataFrame spTransform
#'
fastMask <- function(x, y, cores = NULL, useGDAL = getOption("reproducible.useGDAL", FALSE),
                     verbose = getOption("reproducible.verbose", 1), ..., skipDeprecastedMsg = FALSE) {
  if (!skipDeprecastedMsg)
    .Deprecated("mask", "terra", "fastMask is deprecated; using maskTo and terra")
  touches <- list(...)$touches
  maskTo(from = x, maskTo = y, touches = isTRUE(touches))
}

#' @importFrom raster tmpDir
bigRastersTmpFolder <- function() checkPath(tempdir2(sub = "bigRasters"), create = TRUE)

bigRastersTmpFile <- function() file.path(bigRastersTmpFolder(), "bigRasInput.tif")


attemptGDAL <- function(x, useGDAL = getOption("reproducible.useGDAL", FALSE),
                        verbose = getOption("reproducible.verbose", 1)) {
  attemptGDAL <- FALSE
  if (is(x, "Raster")) {
    crsIsNA <- is.na(.crs(x))
    cpim <- canProcessInMemory(x, 3)
    isTRUEuseGDAL <- isTRUE(useGDAL)
    forceGDAL <- identical(useGDAL, "force")
    shouldUseGDAL <- (!cpim && isTRUEuseGDAL || forceGDAL)
    attemptGDAL <- if (shouldUseGDAL && !crsIsNA) {
      TRUE
    } else {
      if (crsIsNA && shouldUseGDAL)
        messagePrepInputs("      Can't use GDAL because crs is NA", verbose = verbose)
      if (cpim && isTRUEuseGDAL)
        messagePrepInputs("      useGDAL is TRUE, but problem is small enough for RAM; skipping GDAL; ",
                          "useGDAL = 'force' to override", verbose = verbose)

      FALSE
    }
  }
  attemptGDAL
}

maskWithRasterNAs <- function(x, y) {
  origColors <- checkColors(x)
  if (canProcessInMemory(x, 3) && fromDisk(x))
    x[] <- x[]
  x <- rebuildColors(x, origColors)
  m <- which(is.na(y[]))
  x[m] <- NA
  x
}

#' @importFrom raster maxValue minValue
checkColors <- function(x) {
  origColors <- .getColors(x)
  origMaxValue <- maxValue(x)
  origMinValue <- minValue(x)
  list(origColors = origColors[[1]], origMinValue = origMinValue, origMaxValue = origMaxValue)
}

rebuildColors <- function(x, origColors) {
  if (isTRUE(all(origColors$origMinValue != minValue(x)) || all(origColors$origMaxValue != maxValue(x)) ||
             !identical(.getColors(x)[[1]], origColors$origColors))) {
    colorSequences <- unlist(lapply(seq(length(origColors$origMinValue)), function(ind) {
      origColors$origMinValue[ind]:origColors$origMaxValue[ind]
    }))
    if (isTRUE(length(origColors$origColors) == length(colorSequences))) {
      newSeq <- minValue(x):maxValue(x)
      oldSeq <- origColors$origMinValue:origColors$origMaxValue
      whFromOld <- match(newSeq, oldSeq)
      x@legend@colortable <- origColors$origColors[whFromOld]
    }
  }
  x
}


.getColors <- function(object) {
  nams <- names(object)
  cols <- lapply(nams, function(x) {
    as.character(object[[x]]@legend@colortable)
  })
  names(cols) <- nams
  return(cols)
}

