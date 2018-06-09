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
#'
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
#' @param x        A \code{Raster*} object.
#'
#' @param y  A \code{SpatialPolygons} object. If it is not in the same projection
#'           as \code{x}, it will be reprojected on the fly to that of \code{x}
#'
#' @return A \code{Raster*} object, masked (i.e., smaller extent and/or
#'         several pixels converted to NA)
#'
#' @author Eliot Mcintire
#' @export
#' @importFrom raster crop extract mask nlayers raster stack crs
#' @importFrom sp spTransform SpatialPolygonsDataFrame
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
fastMask <- function(x, y) {
  if (requireNamespace("sf") && requireNamespace("fasterize")) {
    message("fastMask is using sf and fasterize")

    if (!identical(crs(y), crs(x))) {
      y <- spTransform(x = y, CRSobj = crs(x))
    }

    if (!is(y, "SpatialPolygonsDataFrame")) {
      y <- SpatialPolygonsDataFrame(Sr = y, data = data.frame(ID = seq(length(y))),
                                    match.ID = FALSE)
    }

    numericfield <- names(y)[which(unlist(lapply(names(y), function(x) {
      is.numeric(y[[x]])
    })))[1]]
    a <- fasterize::fasterize(sf::st_as_sf(y), raster = x[[1]], field = numericfield)
    m <- is.na(a[])
    x[m] <- NA

    if (nlayers(x) > 1) {
      raster::stack(x)
    } else {
      x
    }
  } else {
    message("This function is using the much slower raster::mask. ",
            "See https://github.com/r-spatial/sf to install gdal ",
            "on your system. Then, 'install.packages(\"sf\")",
            "; install.packages(\"fasterize\")')")
    raster::mask(x, y)
  }
}

#' fastCrop
#'
#' This function is a wrapper around \code{velox::VeloxRaster_crop}.
#'
#' @param x Raster to crop
#'
#' @inheritParams raster::crop
#'
#' @export
#' @importFrom raster crop extent
#' @importFrom velox velox
#' @seealso \code{velox::VeloxRaster_crop}
#'
fastCrop <- function(x, y, ...) {
  # velox package is much faster than raster package for rasterize function,
  # but not as fast as gdal_rasterize for large polygons
  a <- crop(x, y)
  v1 <- velox::velox(x)
  if (is(y, "Raster")) y <- extent(y)
  v1$crop(y)
  if (length(names(x)) > 1) {
    a <- v1$as.RasterStack()
  } else {
    a <- v1$as.RasterLayer(band = 1)
  }
  a
}

#' Rasterize polygons quickly
#'
#' Uses either \code{velox} package or, if \code{GDAL} (> 2.0)  is installed and
#' accessible by \code{rgdal::getGDALVersionInfo} and the version,  then it will
#' default to \code{gdalUtils::gdal_rasterize}.
#' This default will be overridden for "small" rasters (fewer than 2e+6 cells),
#' as \code{velox} is faster in those cases.
#' The user can specify whether to use \code{GDAL} with the \code{useGDAL} argument.
#' \code{fastRasterize} will try to keep the object in memory or on disk,
#' depending on whether the input raster was on disk.
#'
#' @param polygon   A \code{SpatialPolygons} object.
#'
#' @param ras       A \code{RasterLayer} object.
#'
#' @param field     The field to use from \code{polygon}.
#'
#' @param filename  Character string giving the filename. Note: if \code{filename}
#'                  is supplied, only the basename of the file is used, and the
#'                  the output raster will be saved using \code{.tif} format.
#'
#' @param useGDAL   Logical. If missing (default): GDAL will be used if version >2
#'                  is available and \code{ras} is larger than 2e+6 pixels.
#'
#' @param datatype  Passed to raster object and disk-format.
#'                  See \code{\link[raster]{dataType}}.
#'
#' @return A \code{Raster*} object.
#'
#' @author Eliot McIntire
#' @export
#' @importClassesFrom velox VeloxRaster
#' @importFrom velox velox
#' @importFrom gdalUtils gdal_rasterize
#' @importFrom raster 'dataType<-' extension fromDisk inMemory setMinMax shapefile
#' @importFrom raster raster res tmpDir xmax xmin ymax ymin
#' @importClassesFrom velox VeloxRaster
#' @importFrom velox velox
#' @rdname fastRasterize
#'
#' @examples
#'\dontrun{
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
#' # rasterize
#' shpRas1 <- rasterize(shp, origStack, field = "vals")
#' shpRas2 <- fastRasterize(shp, origStack, field = "vals", useGDAL = FALSE, datatype = "FLT4S")
#' shpRas3 <- fastRasterize(shp, origStack, field = "vals", useGDAL = TRUE, datatype = "FLT4S")
#' shpRas4 <- fastRasterize(shp, origStack, field = "vals", useGDAL = TRUE, datatype = "FLT4S",
#'                          filename = "newMap")
#' }
fastRasterize <- function(polygon, ras, field, filename, useGDAL, datatype) {
  minGDALVersion <- 2

  keepInMemory <- inMemory(ras) & missing(filename)

  if (missing(useGDAL)) {
    if (ncell(ras) > 2e6) {
      useGDAL <- checkGDALVersion(minGDALVersion)
    } else {
      useGDAL <- FALSE
    }
  } else {
    if (isTRUE(useGDAL)) {
      if (!is.na(getGDALVersion())) {
        if (getGDALVersion() < minGDALVersion) {
          warning("Outdated GDAL version detected. ",
                  "A recent version (>2) recommended for best performance. ",
                  "Using velox package instead.")
          useGDAL <- FALSE
        } else {
          useGDAL <- TRUE
        }
      } else {
        stop("No suitable GDAL version detected. Please specify 'useGDAL = FALSE'.")
      }
    }
  }
  rstFilename <- if (!missing(filename)) {
    extension(basename(filename), ".tif")
  } else {
    tempfile(fileext = ".tif")
  }

  # These are taken from ?dataType from the raster package and http://www.gdal.org/frmt_gtiff.html
  types <- data.frame(
    rasterTypes = c("INT1S", "INT1U", "INT2S", "INT2U", "INT4S", "INT4U", "FLT4S", "FLT4U"),
    gdalTypes = c("16Int", "U16Int", "16Int", "U16Int", "32Int", "U32Int", "32Float", "32Float"),
    stringsAsFactors = FALSE)

  if (missing(field)) field <- names(polygon)

  needFactor <- if (missing(field) | length(field) > 1) {
    TRUE
  } else if (length(field) == 1) {
    is.factor(polygon[[field]]) | is.character(polygon[[field]])
  }

  valsGDAL <- c(16, 32, 64)
  valsRast <- data.frame(vals = c(2 ^ 8, 2 ^ 16, 2 ^ 32, 2 ^ 128), labels = c(1, 2, 4, 8))
  intflt <- c("INT", "FLT")

  if (needFactor) {
    int <- 1
    rang <- c(1, NROW(polygon))
    neg <- 2
  } else {
    int <- if (is.integer(polygon[[field]])) 1 else 2
    rang <- range(polygon[[field]])
    neg <- if (rang[1] < 0 & int == 1) 1 else 2
  }

  if (missing(datatype)) {
    if (useGDAL) {
      maxV <- which.min(diff(rang) < (2 ^ valsGDAL))
      intflt <- c("Int", "Float")
      datatypeGdal <- paste0("U"[neg], intflt[int], valsGDAL[maxV])
    }
  } else {
    datatypeGdal <- types$gdalTypes[types$rasterTypes %in% datatype]
  }

  if (needFactor) {
    attrNames <- names(polygon)
    if (!("ID" %in% attrNames)) {
      polygon$ID <- 1:NROW(polygon) # nolint
    }
  }

  fieldTmp <- if (needFactor) "ID" else field

  if (useGDAL) {
    tmpShpFilename <- tempfile(fileext = ".shp")

    # write the polygon object to disk as a shapefile -- can't handle NAs without showing a warning
    suppressWarnings(shapefile(polygon, filename = tmpShpFilename))

    # Run rasterize from gdal
    gdalUtils::gdal_rasterize(a = fieldTmp, tr = res(ras), a_nodata = NA_integer_,
                              tmpShpFilename,
                              te = c(xmin(ras), ymin(ras), xmax(ras), ymax(ras)),
                              rstFilename, ot = datatypeGdal)
    a <- raster(rstFilename)
  } else {
    # velox package is much faster than raster package for rasterize function,
    # but not as fast as gdal_rasterize for large polygons
    v1 <- velox(ras)
    v1$rasterize(polygon, field = fieldTmp, background = NA_integer_)
    a <- v1$as.RasterLayer(band = 1)
  }

  if (needFactor) {
    whichID <- names(polygon) %in% "ID"
    whichOther <- names(polygon) %in% field
    levels(a) <- data.frame(as.data.frame(polygon[, whichID]),
                            as.data.frame(polygon[, whichOther]))
    names(a) <- "layer"
  } else {
    names(a) <- field
  }

  if (isTRUE(tryCatch(minValue(a), warning = function(e) TRUE)))
    a <- setMinMax(a)

  hasNA <- if (anyNA(a[])) 1 else 0
  maxV <- pmax(pmax(min(which(diff(rang) < (valsRast$vals))), 1 + hasNA), (int == 2) * 4)
  if (missing(datatype)) {
    datatype <- paste0(intflt[int], valsRast$labels[maxV], c("U", "S")[neg])
  }

  dataType(a) <- datatype
  if (keepInMemory & fromDisk(a)) a[] <- getValues(a)
  if (!keepInMemory & inMemory(a)) {
    a <- writeRaster(a, filename = file.path(tmpDir(), rstFilename),
                     overwrite = TRUE, datatype = datatype) # need NA value
  }

  return(a)
}
