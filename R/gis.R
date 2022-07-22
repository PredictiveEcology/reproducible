#' Faster operations on rasters (DEPRECATED as \code{terra::mask} is fast)
#'
#' This alternative to \code{raster::mask} is included here.
#'
#' @param x  A \code{Raster*} object.
#'
#' @param y  A \code{SpatialPolygons} object. If it is not in the same projection
#'           as \code{x}, it will be reprojected on the fly to that of \code{x}
#'
#' @param cores An \code{integer*} or \code{'AUTO'}. This will be used if gdalwarp is
#'           triggered. \code{'AUTO'} will calculate 90% of the total
#'           number of cores in the system, while an integer or rounded
#'           float will be passed as the exact number of cores to be used.
#' @param skipDeprecastedMsg Logical. If \code{TRUE}, then the message about this function
#'   being deprecated will be suppressed.
#'
#' @param ... Currently unused.
#'
#' @return A \code{Raster*} object, masked (i.e., smaller extent and/or
#'         several pixels converted to NA)
#'
#' @author Eliot McIntire
#' @export
#' @inheritParams Cache
#'
#' @examples
#'
#' # This function is mostly superfluous as terra::mask is fast
#' library(sp)
#' library(raster)
#'
#' Sr1 <- Polygon(cbind(c(2, 4, 4, 0.9, 2), c(2, 3, 5, 4, 2)))
#' Sr2 <- Polygon(cbind(c(5, 4, 2, 5), c(2, 3, 2, 2)))
#' Sr3 <- Polygon(cbind(c(4, 4, 5, 10, 4), c(5, 3, 2, 5, 5)))
#'
#' Srs1 <- Polygons(list(Sr1), "s1")
#' Srs2 <- Polygons(list(Sr2), "s2")
#' Srs3 <- Polygons(list(Sr3), "s3")
#' shp <- SpatialPolygons(list(Srs1, Srs2, Srs3), 1:3)
#' d <- data.frame(vals = 1:3, other = letters[3:1], stringsAsFactors = FALSE)
#' row.names(d) <- names(shp)
#' shp <- SpatialPolygonsDataFrame(shp, data = d)
#' poly <- list()
#' poly[[1]] <- raster(raster::extent(shp), vals = 0, res = c(1, 1))
#' poly[[2]] <- raster(raster::extent(shp), vals = 1, res = c(1, 1))
#' origStack <- stack(poly)
#'
#' # during transition from raster to terra, the following requires terra to run
#' if (requireNamespace("terra", silent = TRUE)) {
#'   newStack1 <- mask(x= terra::rast(origStack), mask = terra::vect(sf::st_as_sf(shp)))
#'   newStack2 <- fastMask(x = origStack, y = sf::st_as_sf(shp))
#'
#' # test all equal
#'   all.equal(newStack1, newStack2)
#'
#'   newStack1 <- stack(newStack1)
#'   newStack2 <- stack(newStack2)
#'
#'  if (interactive()) {
#'   plot(newStack2[[1]])
#'   plot(shp, add = TRUE)
#'  }
#'
#' }
#'
fastMask <- function(x, y, cores = NULL, # useGDAL = getOption("reproducible.useGDAL", TRUE),
                     verbose = getOption("reproducible.verbose", 1), ..., skipDeprecastedMsg = FALSE) {
  if (!skipDeprecastedMsg)
    .Deprecated("mask", "terra", "fastMask is deprecasted; using maskTo and terra")
  touches <- list(...)$touches
  maskTo(from = x, maskTo = y, touches = isTRUE(touches))
}

bigRastersTmpFolder <- function() checkPath(Require::tempdir2(sub = "bigRasters"), create = TRUE)

bigRastersTmpFile <- function() file.path(bigRastersTmpFolder(), "bigRasInput.tif")

dealWithCores <- function(cores) {
  # browser(expr = exists("._dealWithCores_1"))
  if (is.null(cores) || cores == "AUTO") {
    if (requireNamespace("parallel", quietly = TRUE)) {
      cores <- as.integer(parallel::detectCores() * 0.9)
    } else {
      cores <- 1L
    }
  } else {
    if (!is.integer(cores)) {
      if (is.character(cores) | is.logical(cores)) {
        stop("'cores' needs to be passed as numeric or 'AUTO'")
      } else {
        cores <- as.integer(cores)
      }
    }
  }

}

attemptGDAL <- function(x, useGDAL = getOption("reproducible.useGDAL", TRUE),
                        verbose = getOption("reproducible.verbose", 1)) {
  attemptGDAL <- FALSE
  if (is(x, "Raster")) {
    # browser(expr = exists("._attemptGDAL_1"))
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


checkColors <- function(x) {
  .requireNamespace("raster", stopOnFALSE = TRUE)
  origColors <- .getColors(x)
  origMaxValue <- raster::maxValue(x)
  origMinValue <- raster::minValue(x)
  list(origColors = origColors[[1]], origMinValue = origMinValue, origMaxValue = origMaxValue)
}

rebuildColors <- function(x, origColors) {
  if (isTRUE(all(origColors$origMinValue != raster::minValue(x)) ||
             all(origColors$origMaxValue != raster::maxValue(x)) ||
             !identical(.getColors(x)[[1]], origColors$origColors))) {
    colorSequences <- unlist(lapply(seq(length(origColors$origMinValue)), function(ind) {
      origColors$origMinValue[ind]:origColors$origMaxValue[ind]
    }))
    if (isTRUE(length(origColors$origColors) == length(colorSequences))) {
      newSeq <- raster::minValue(x):raster::maxValue(x)
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


similarExtents <- function(ext1, ext2, closeEnough) {
  if (is(ext1, "SpatExtent")) {
    out <- ((ext1@ptr$vector - ext2@ptr$vector) < closeEnough)
  } else {
    out <- sapply(slotNames(ext1), function(sn) abs(slot(ext1, sn) - slot(ext2, sn)) < closeEnough)
  }
  all(out)
}
