#' Check the GDAL version in use
#'
#' @return \code{numeric_version}
#'
#' @author Alex Chubaty and Eliot McIntire
#' @export
#' @importFrom magrittr %>%
getGDALVersion <-  function() {
  if (.requireNamespace("rgdal")) {
    vers <- tryCatch(rgdal::getGDALVersionInfo(), error = function(e) NA_real_)
    if (!is.na(vers)) {
      vers <- strsplit(vers, split = ",")[[1]][1] %>%
        strsplit(., split = " ") %>%
        `[[`(1) %>%
        `[`(2) %>%
        as.numeric_version(.)
    }
  } else {
    vers <- '0.0.0'
  }
  return(vers)
}

#' Check whether the system has a minimum version of GDAL available
#'
#' @param version  The minimum GDAL version to check for.
#'
#' @return Logical.
#'
#' @author Eliot McIntire and Alex Chubaty
#' @export
#' @examples
#'
#' \dontrun{
#'   checkGDALVersion("2.0")
#' }
checkGDALVersion <- function(version) {
  if (missing(version)) stop("Minimum version not specified.")

  if (!is.na(getGDALVersion())) {
    if (getGDALVersion() >= version) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    return(FALSE)
  }
}

#' Faster operations on rasters
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
#' @examples
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
#' # original mask function in raster
#' newStack1 <- mask(x= origStack, mask = shp)
#' newStack2 <- fastMask(x = origStack, y = shp)
#'
#' # test all equal
#' all.equal(newStack1, newStack2)
#'
#' newStack1 <- stack(newStack1)
#' newStack2 <- stack(newStack2)
#'
#' if (interactive()) {
#'   plot(newStack2[[1]])
#'   plot(shp, add = TRUE)
#' }
#'
fastMask <- function(x, y, cores = NULL, useGDAL = getOption("reproducible.useGDAL", TRUE),
                     verbose = getOption("reproducible.verbose", 1), ...) {
  if (!identical(.crs(y), .crs(x))) {
    if (!is(y, "sf")) {
      y <- spTransform(x = y, CRSobj = .crs(x))
    } else {
      .requireNamespace("sf", stopOnFALSE = TRUE)
      y <- sf::st_transform(x = y, crs = .crs(x))
    }
  }

  if (is(y, "SpatialPolygons")) {
    if (!is(y, "SpatialPolygonsDataFrame")) {
      y <- SpatialPolygonsDataFrame(Sr = y, data = data.frame(ID = seq(length(y))),
                                    match.ID = FALSE)
    }
  }

  # need to double check that gdal executable exists before going down this path
  attemptGDAL <- attemptGDAL(x, useGDAL, verbose = verbose)

  # browser(expr = exists("._fastMask_2"))

  if (is(x, "RasterLayer") && requireNamespace("sf", quietly = TRUE) &&
      requireNamespace("fasterize", quietly = TRUE)) {
    messagePrepInputs("fastMask is using sf and fasterize")


    if (attemptGDAL) {
      # call gdal
      messagePrepInputs("fastMask is using gdalwarp")

      # rasters need to go to same directory that can be unlinked at end without losing other temp files
      tmpRasPath <- checkPath(bigRastersTmpFolder(), create = TRUE)
      tempSrcRaster <- bigRastersTmpFile()
      tempDstRaster <- file.path(tmpRasPath, paste0(x@data@names,"_mask", ".tif"))

      # GDAL will to a reprojection without an explicit crop
      cropExtent <- extent(x)
      # cropExtentRounded <- roundToRes(cropExtent, x)
      # the raster could be in memory if it wasn't reprojected
      if (inMemory(x)) {
        dType <- assessDataType(x, type = "writeRaster")
        dTypeGDAL <- assessDataType(x, type = "GDAL")
        x <- writeRaster(x, filename = tempSrcRaster, datatype = dType, overwrite = TRUE)
        gc()
      } else {
        tempSrcRaster <- x@file@name #Keep original raster.
        dTypeGDAL <- assessDataType(raster(tempSrcRaster), type = "GDAL")
      }

      ## GDAL requires file path to cutline - write to disk
      tempSrcShape <- normPath(file.path(tempfile(tmpdir = raster::tmpDir()), ".shp", fsep = ""))
      ysf <- sf::st_as_sf(y)
      sf::st_write(ysf, tempSrcShape)
      tr <- res(x)

      if (isWindows()) {
        messagePrepInputs("Using gdal at ", getOption("gdalUtils_gdalPath")[[1]]$path)
        exe <- ".exe"
      } else {
        exe <- ""
      }
      # dType <- assessDataType(raster(tempSrcRaster), type = "GDAL")
      cores <- dealWithCores(cores)
      prll <- paste0("-wo NUM_THREADS=", cores, " ")
      srcCRS <- as.character(.crs(raster::raster(tempSrcRaster)))
      targCRS <- srcCRS
      system(
        paste0(paste0(getOption("gdalUtils_gdalPath")[[1]]$path, "gdalwarp", exe, " "),
               "-s_srs \"", srcCRS, "\"",
               " -t_srs \"", targCRS, "\"",
               " -multi ", prll,
               "-ot ",
               dTypeGDAL, " ",
               # "-crop_to_cutline ", # crop to cutline is wrong here, it will realign raster to new origin
               "-cutline ",  "\"", tempSrcShape,"\"", " ",
               " -overwrite ",
               "-tr ", paste(tr, collapse = " "), " ",
               "-te ", paste(c(cropExtent[1], cropExtent[3], # having this here is like crop to cutline
                               cropExtent[2], cropExtent[4]), # but without cutting pixels off
                             collapse = " "), " ",
               "\"", tempSrcRaster, "\"", " ",
               "\"", tempDstRaster, "\""),
        wait = TRUE, intern = TRUE, ignore.stderr = TRUE)
      x <- raster(tempDstRaster)
      x <- setMinMaxIfNeeded(x)
    } else {
      # Eliot removed this because fasterize::fasterize will handle cases where x[[1]] is too big
      #extentY <- extent(y)
      #resX <- res(x) * 2 # allow a fuzzy interpretation -- the cropInputs here won't make it perfect anyway
      # if ( (xmin(x) + resX[1]) < xmin(extentY) && (xmax(x) - resX[1]) > xmax(extentY) &&
      #      (ymin(x) + resX[2]) < ymin(extentY) && (ymax(x) - resX[2]) > ymax(extentY) )
      #   x <- cropInputs(x, y)
      if (!is(y, "sf")) {
        y <- fasterize::fasterize(sf::st_as_sf(y), raster = x[[1]], field = NULL)
      }
      x <- maskWithRasterNAs(x = x, y = y)
      # if (canProcessInMemory(x, 3) && fromDisk(x))
      #   x[] <- x[]
      # m <- which(is.na(y[]))
      # x[m] <- NA

      if (nlayers(x) > 1) {
        raster::stack(x)
      } else {
        x
      }
    }
  } else {
    messagePrepInputs("This function is using raster::mask")
    if (is(x, "RasterStack") || is(x, "RasterBrick")) {
      messagePrepInputs(" because fastMask doesn't have a specific method ",
              "for these RasterStack or RasterBrick yet")
    } else {
      messagePrepInputs("This may be slow in large cases. ",
              "To use sf and GDAL instead, see ",
              "https://github.com/r-spatial/sf to install GDAL ",
              "on your system. Then, 'install.packages(\"sf\")",
              "; install.packages(\"fasterize\")')")
    }
    if (is(x, "RasterStack")) {
      raster::stack(raster::mask(x, y))
    } else {
      raster::mask(x, y)
    }
  }
}

#' @importFrom raster tmpDir
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

findGDAL <- function() {
  attemptGDAL <- FALSE
  if (.requireNamespace("gdalUtils")) {
    gdalPath <- NULL
    attemptGDAL <- TRUE
    if (isWindows()) {
      # Handle all QGIS possibilities
      a <- dir("C:/", pattern = "Progra", full.names = TRUE)
      a <- grep("Program Files", a, value = TRUE)
      a <- unlist(lapply(a, dir, pattern = "QGIS", full.name = TRUE))
      a <- unlist(lapply(a, dir, pattern = "bin", full.name = TRUE))


      possibleWindowsPaths <- c(a, "C:/OSGeo4W64/bin",
                                "C:/GuidosToolbox/QGIS/bin",
                                "C:/GuidosToolbox/guidos_progs/FWTools_win/bin",
                                "C:/Program Files (x86)/Quantum GIS Wroclaw/bin",
                                "C:/Program Files/GDAL",
                                "C:/Program Files (x86)/GDAL")
      messagePrepInputs("Searching for gdal installation")
      gdalInfoExists <- file.exists(file.path(possibleWindowsPaths, "gdalinfo.exe"))
      if (any(gdalInfoExists))
        gdalPath <- possibleWindowsPaths[gdalInfoExists]
    }
    b <- try(setGDALInst(gdalPath), silent = TRUE)

    if (is.null(getOption("gdalUtils_gdalPath"))) # if it doesn't find gdal installed
      attemptGDAL <- FALSE
    attemptGDAL
  }
  invisible(attemptGDAL)
}

setGDALInst <- function(gdalPath) {
  if (isWindows())
    setTimeLimit(20, transient = TRUE)
  out <- gdalUtils::gdal_setInstallation(gdalPath)
  return(out)
}

attemptGDAL <- function(x, useGDAL = getOption("reproducible.useGDAL", TRUE),
                        verbose = getOption("reproducible.verbose", 1)) {
  attemptGDAL <- FALSE
  if (is(x, "Raster")) {
    if (requireNamespace("gdalUtils", quietly = TRUE)) {
      # browser(expr = exists("._attemptGDAL_1"))
      crsIsNA <- is.na(.crs(x))
      cpim <- canProcessInMemory(x, 3)
      isTRUEuseGDAL <- isTRUE(useGDAL)
      forceGDAL <- identical(useGDAL, "force")
      shouldUseGDAL <- (!cpim && isTRUEuseGDAL || forceGDAL)
      attemptGDAL <- if (shouldUseGDAL && !crsIsNA) {
        findGDAL()
      } else {
        if (crsIsNA && shouldUseGDAL)
          messagePrepInputs("      Can't use GDAL because crs is NA", verbose = verbose)
        if (cpim && isTRUEuseGDAL)
          messagePrepInputs("      useGDAL is TRUE, but problem is small enough for RAM; skipping GDAL; ",
                            "useGDAL = 'force' to override", verbose = verbose)

        FALSE
      }
    } else {
      messagePrepInputs("To use gdal, you need to install gdalUtils; install.packages('gdalUtils')", verbose = verbose)
      attemptGDAL <- FALSE
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
    if (isTRUE(length(origColors$origColors) == length(origColors$origMinValue:origColors$origMaxValue))) {
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

