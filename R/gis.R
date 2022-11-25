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
#' @inheritParams projectInputs.Raster
#' @importFrom raster crop crs extract mask nlayers raster stack tmpDir
#' @importFrom raster xmin xmax ymin ymax fromDisk setMinMax
#' @importFrom sp SpatialPolygonsDataFrame spTransform
#'
fastMask <- function(x, y, cores = NULL, useGDAL = getOption("reproducible.useGDAL", TRUE),
                     verbose = getOption("reproducible.verbose", 1), ..., skipDeprecastedMsg = FALSE) {
  if (!skipDeprecastedMsg)
    .Deprecated("mask", "terra", "fastMask is deprecated; using maskTo and terra")
  touches <- list(...)$touches
  maskTo(from = x, maskTo = y, touches = isTRUE(touches))
#   if (!identical(.crs(y), .crs(x))) {
#     if (!is(y, "sf")) {
#       y <- spTransform(x = y, CRSobj = .crs(x))
#     } else {
#       .requireNamespace("sf", stopOnFALSE = TRUE)
#       y <- sf::st_transform(x = y, crs = .crs(x))
#     }
#   }
#
#   if (is(y, "SpatialPolygons")) {
#     if (!is(y, "SpatialPolygonsDataFrame")) {
#       y <- SpatialPolygonsDataFrame(Sr = y, data = data.frame(ID = seq(length(y))),
#                                     match.ID = FALSE)
#     }
#   }
#
#   # need to double check that gdal executable exists before going down this path
#   attemptGDAL <- attemptGDAL(x, useGDAL, verbose = verbose)
#
#   # browser(expr = exists("._fastMask_2"))
#
#   if (is(x, "RasterLayer") && requireNamespace("sf", quietly = TRUE) &&
#       requireNamespace("fasterize", quietly = TRUE)) {
#     messagePrepInputs("fastMask is using sf and fasterize")
#
#
#      if (attemptGDAL) {
#        message("GDAL is deprecated in fastMask")
#      }
# #       # call gdal
# #       messagePrepInputs("fastMask is using gdalwarp")
# #
# #       # rasters need to go to same directory that can be unlinked at end without losing other temp files
# #       tmpRasPath <- checkPath(bigRastersTmpFolder(), create = TRUE)
# #       tempSrcRaster <- bigRastersTmpFile()
# #       tempDstRaster <- file.path(tmpRasPath, paste0(x@data@names,"_mask", ".tif"))
# #
# #       # GDAL will to a reprojection without an explicit crop
# #       cropExtent <- extent(x)
# #       te <- paste(c(cropExtent[1], cropExtent[3],
# #                     cropExtent[2], cropExtent[4]))
# #       # cropExtentRounded <- roundToRes(cropExtent, x)
# #       # the raster could be in memory if it wasn't reprojected
# #       if (inMemory(x)) {
# #         dType <- assessDataType(x, type = "writeRaster")
# #         dTypeGDAL <- assessDataType(x, type = "GDAL")
# #         x <- writeRaster(x, filename = tempSrcRaster, datatype = dType, overwrite = TRUE)
# #         gc()
# #       } else {
# #         tempSrcRaster <- x@file@name #Keep original raster.
# #         dTypeGDAL <- assessDataType(raster(tempSrcRaster), type = "GDAL")
# #       }
# #
# #       ## GDAL requires file path to cutline - write to disk
# #       tempSrcShape <- normPath(file.path(tempfile(tmpdir = raster::tmpDir()), ".shp", fsep = ""))
# #       ysf <- sf::st_as_sf(y)
# #       sf::st_write(ysf, tempSrcShape)
# #       tr <- res(x)
# #
# #       cores <- dealWithCores(cores)
# #       prll <- paste0("-wo NUM_THREADS=", cores, " ")
# #       srcCRS <- as.character(.crs(raster::raster(tempSrcRaster)))
# #       targCRS <- srcCRS
# #
# #       gdalUtilities::gdalwarp(srcfile = tempSrcRaster, dstfile = tempDstRaster,
# #                               s_srs = srcCRS, t_srs = targCRS,
# #                               cutline = tempSrcShape, crop_to_cutline = FALSE,
# #                               srcnodata = NA, dstnodata = NA, tr = tr,
# #                               te = te, ot = dTypeGDAL, multi = TRUE, wo = prll, overwrite = TRUE)
# #
# #       x <- raster(tempDstRaster)
# #       x <- setMinMaxIfNeeded(x)
# #     } else {
#       # Eliot removed this because fasterize::fasterize will handle cases where x[[1]] is too big
#       #extentY <- extent(y)
#       #resX <- res(x) * 2 # allow a fuzzy interpretation -- the cropInputs here won't make it perfect anyway
#       # if ( (xmin(x) + resX[1]) < xmin(extentY) && (xmax(x) - resX[1]) > xmax(extentY) &&
#       #      (ymin(x) + resX[2]) < ymin(extentY) && (ymax(x) - resX[2]) > ymax(extentY) )
#       #   x <- cropInputs(x, y)
#       if (!is(y, "sf")) {
#         y <- sf::st_as_sf(y)
#       }
#       yRas <- fasterize::fasterize(y, raster = x[[1]], field = NULL)
#       x <- maskWithRasterNAs(x = x, y = yRas)
#       # if (canProcessInMemory(x, 3) && fromDisk(x))
#       #   x[] <- x[]
#       # m <- which(is.na(y[]))
#       # x[m] <- NA
#
#       if (nlayers(x) > 1) {
#         raster::stack(x)
#       } else {
#         x
#       }
#     # }
#   } else {
#     if (is(x, "RasterStack") || is(x, "RasterBrick")) {
#       messagePrepInputs(" because fastMask doesn't have a specific method ",
#               "for these RasterStack or RasterBrick yet")
#     } else {
#       messagePrepInputs("This may be slow in large cases. ",
#               "To use sf and GDAL instead, see ",
#               "https://github.com/r-spatial/sf to install GDAL ",
#               "on your system. Then, 'install.packages(\"sf\")",
#               "; install.packages(\"fasterize\")')")
#     }
#     isRasterStack <- is(x, "RasterStack")
#     isRasterBrick <- is(x, "RasterBrick")
#     if (requireNamespace("terra") && getOption("reproducible.useTerra", FALSE)) {
#
#       messagePrepInputs("      Using terra::mask for masking")
#       if (!is(x, "SpatRaster"))
#         x <- terra::rast(x)
#       if (!is(y, "SpatVector")) {
#         if (!is(y, "sf"))
#           y <- sf::st_as_sf(y)
#         y <- terra::vect(y)
#       }
#
#       x <- terra::mask(x, y, touches = FALSE) # that was previous default in raster::mask
#       x <- if (isRasterStack) {
#         raster::stack(x)
#       } else if (isRasterBrick) {
#         raster::brick(x)
#       } else { raster::raster(x)}
#     } else {
#       messagePrepInputs("This function is using raster::mask")
#       x <- raster::mask(x, y)
#       if (isRasterStack) x <- raster::stack(x)
#       x
#     }
#
#   }
}

#' @importFrom raster tmpDir
bigRastersTmpFolder <- function() checkPath(tempdir2(sub = "bigRasters"), create = TRUE)

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

