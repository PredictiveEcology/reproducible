test_that("GDAL doesn't work (part 3)", {
  if (requireNamespace("gdalUtils", quietly = TRUE)) {
    suppressWarnings(gdalUtils::gdal_setInstallation())
  }

  if (is.null(getOption("gdalUtils_gdalPath")))
    skip("no GDAL installation found")

  if (requireNamespace("rgeos")) {
    testInitOut <- testInit(c("raster", "sf", "rgeos"), opts = list(
      "rasterTmpDir" = tempdir2(rndstr(1,6)),
      "reproducible.inputPaths" = NULL,
      "reproducible.overwrite" = TRUE,
      'reproducible.useGDAL' = TRUE)
    )
    on.exit({
      testOnExit(testInitOut)
    }, add = TRUE)

    options("reproducible.cachePath" = tmpdir)


    #test GDAL
    coords <- structure(c(-122.98, -116.1, -99.2, -106, -122.98, 59.9, 65.73, 63.58, 54.79, 59.9),
                        .Dim = c(5L, 2L))
    coords2 <- structure(c(-115.98, -116.1, -99.2, -106, -122.98, 59.9, 65.73, 63.58, 54.79, 59.9),
                         .Dim = c(5L, 2L))
    Sr1 <- Polygon(coords)
    Srs1 <- Polygons(list(Sr1), "s1")
    StudyArea <- SpatialPolygons(list(Srs1), 1L)
    crs(StudyArea) <- crsToUse


    nonLatLongProj <- paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
                            "+x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs")
    nc <- sf::st_as_sf(StudyArea)#system.file("shape/nc.shp", package="sf"))
    nc1 <- sf::st_transform(nc, nonLatLongProj)
    #ncSmall <- sf::st_buffer(nc1, dist = -10000)
    ncSmall <- sf::st_buffer(nc1, dist = -2000)



   rasterBig <- raster(extent(nc), vals = as.integer(runif(n = 1056, min = 0, max = 10)),
                       res = c(0.5, 0.5),
                       crs = crs(nc))
   rasterSmall <- raster(extent(ncSmall), vals = 1, res = c(10000, 10000), crs = crs(ncSmall))
   #The extent of a negatively buffered SpatialPolygonsDataFrame doesn't change
   rasterSmall <- rasterize(ncSmall, rasterSmall)
   out <- postProcess(rasterBig,
                      studyArea = ncSmall,
                      rasterToMatch = rasterSmall,
                      useGDAL = 'force')
   expect_true(compareCRS(out, rasterSmall))

  }
})
