#' Check the GDAL version in use
#'
#' @return \code{numeric_version}
#'
#' @author Alex Chubaty and Eliot McIntire
#' @export
#' @importFrom magrittr %>%
#' @importFrom rgdal getGDALVersionInfo
#'
getGDALVersion <-  function() {
  vers <- tryCatch(getGDALVersionInfo(), error = function(e) NA_real_)
  if (!is.na(vers)) {
    vers <- strsplit(vers, split = ",")[[1]][1] %>%
      strsplit(., split = " ") %>%
      `[[`(1) %>%
      `[`(2) %>%
      as.numeric_version(.)
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
#'
#' @return A \code{Raster*} object, masked (i.e., smaller extent and/or
#'         several pixels converted to NA)
#'
#' @author Eliot McIntire
#' @export
#' @inheritParams projectInputs.Raster
#' @importFrom fasterize fasterize
#' @importFrom parallel detectCores
#' @importFrom raster crop crs extract mask nlayers raster stack tmpDir
#' @importFrom raster xmin xmax ymin ymax fromDisk
#' @importFrom sf st_as_sf st_write
#' @importFrom sp SpatialPolygonsDataFrame spTransform
#'
#' @examples
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
#' newStack1 <- mask(origStack, mask = shp)
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
fastMask <- function(x, y, cores = NULL, useGDAL = getOption("reproducible.useGDAL", TRUE)) {
  if (is(x, "RasterLayer") && requireNamespace("sf") && requireNamespace("fasterize")) {
    message("fastMask is using sf and fasterize")

    if (!identical(crs(y), crs(x))) {
      if (!is(y, "sf")) {
        y <- spTransform(x = y, CRSobj = crs(x))
      } else {
        y <- st_transform(x = y, crs = crs(x))
      }
    }

    if (is(y, "SpatialPolygons")) {
      if (!is(y, "SpatialPolygonsDataFrame")) {
        y <- SpatialPolygonsDataFrame(Sr = y, data = data.frame(ID = seq(length(y))),
                                      match.ID = FALSE)
      }
    }

    if (!raster::canProcessInMemory(x, n = 3) && isTRUE(useGDAL)) {
     # call gdal
      message("fastMask is using gdalwarp")

      # rasters need to go to same directory that can be unlinked at end without losing other temp files
      tmpRasPath <- checkPath(file.path(raster::tmpDir(), "bigRasters"), create = TRUE)
      tempSrcRaster <- file.path(tmpRasPath, "bigRasInput.tif")
      tempDstRaster <- file.path(tmpRasPath, paste0(x@data@names,"_mask", ".tif"))

      # the raster could be in memory if it wasn't reprojected
      if (inMemory(x)) {
        dType <- assessDataType(raster(x), type = "writeRaster")
        x <- writeRaster(x, filename = tempSrcRaster, datatype = dType, overwrite = TRUE)
        gc()
      } else {
        tempSrcRaster <- x@file@name #Keep original raster.
      }

      ## GDAL requires file path to cutline - write to disk
      tempSrcShape <- file.path(tempfile(tmpdir = raster::tmpDir()), ".shp", fsep = "")
      ysf <- sf::st_as_sf(y)
      sf::st_write(ysf, tempSrcShape)
      tr <- res(x)

      gdalPath <- NULL
      if (isWindows()) {
        possibleWindowsPaths <- c("C:/PROGRA~1/QGIS3~1.0/bin/", "C:/OSGeo4W64/bin",
                                  "C:/GuidosToolbox/QGIS/bin",
                                  "C:/GuidosToolbox/guidos_progs/FWTools_win/bin",
                                  "C:/Program Files (x86)/QGIS 3.6/bin",
                                  "C:/Program Files (x86)/Quantum GIS Wroclaw/bin",
                                  "C:/Program Files/GDAL",
                                  "C:/Program Files (x86)/GDAL",
                                  "C:/Program Files (x86)/QGIS 2.18/bin")
        message("Searching for gdal installation")
        gdalInfoExists <- file.exists(file.path(possibleWindowsPaths, "gdalinfo.exe"))
        if (any(gdalInfoExists))
          gdalPath <- possibleWindowsPaths[gdalInfoExists]
      }
      gdalUtils::gdal_setInstallation(gdalPath)
      if (isWindows()) {
        message("Using gdal at ", getOption("gdalUtils_gdalPath")[[1]]$path)
        exe <- ".exe"
      } else {
        exe <- ""
      }
      dType <- assessDataType(raster(tempSrcRaster), type = "GDAL")
      if (is.null(cores) || cores == "AUTO") {
        cores <- as.integer(parallel::detectCores() * 0.9)
        prll <- paste0("-wo NUM_THREADS=", cores, " ")
      } else {
        if (!is.integer(cores)) {
          if (is.character(cores) | is.logical(cores)) {
            stop("'cores' needs to be passed as numeric or 'AUTO'")
          } else {
            prll <- paste0("-wo NUM_THREADS=", as.integer(cores), " ")
          }
        } else {
          prll <- paste0("-wo NUM_THREADS=", cores, " ")
        }
      }
      system(
        paste0(paste0(getOption("gdalUtils_gdalPath")[[1]]$path, "gdalwarp", exe, " "),
               " -multi ", prll,
               "-ot ",
               dType, " ",
               "-crop_to_cutline ",
               "-cutline ",  "\"", tempSrcShape,"\"", " ",
               " -overwrite ",
               "-tr ", paste(tr, collapse = " "), " ",
               "\"", tempSrcRaster, "\"", " ",
               "\"", tempDstRaster, "\""),
        wait = TRUE, intern = TRUE, ignore.stderr = TRUE)
      x <- raster(tempDstRaster)
    } else {
      extentY <- extent(y)
      resX <- res(x) * 2 # allow a fuzzy interpretation -- the cropInputs here won't make it perfect anyway
      if ( (xmin(x) + resX[1]) < xmin(extentY) && (xmax(x) - resX[1]) > xmax(extentY) &&
           (ymin(x) + resX[2]) < ymin(extentY) && (ymax(x) - resX[2]) > ymax(extentY) )
        x <- cropInputs(x, y)
      if (!is(y, "sf")) {
        y <- fasterize::fasterize(sf::st_as_sf(y), raster = x[[1]], field = NULL)
      }
      if (canProcessInMemory(x, 3) && fromDisk(x))
        x[] <- x[]
      m <- is.na(y[])
      x[m] <- NA

      if (nlayers(x) > 1) {
        raster::stack(x)
      } else {
        x
      }
    }
  } else {
    message("This function is using raster::mask")
    if (is(x, "RasterStack") || is(x, "RasterBrick")) {
      message(" because fastMask doesn't have a specific method ",
              "for these classes yet")
    } else {
      message("This may be slow in large cases. ",
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
