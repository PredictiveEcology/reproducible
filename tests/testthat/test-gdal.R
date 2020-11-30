test_that("GDAL doesn't work (part 3)", {
  hasGDAL <- findGDAL()
  if (!isTRUE(hasGDAL))
    skip("no GDAL installation found")

  #if (requireNamespace("rgeos")) {
  #testInitOut <- testInit(c("raster", "sf", "rgeos"), tmpFileExt = c(".grd", ".tif"),
  testInitOut <- testInit(c("raster", "sf"), tmpFileExt = c(".grd", ".tif"),
                                                  opts = list(
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
  ccc <- testthat::capture_output(
    out <- postProcess(rasterBig,
                       studyArea = ncSmall,
                       rasterToMatch = rasterSmall,
                       useGDAL = 'force')
  )
  expect_true(compareCRS(out, rasterSmall))

  out2 <- cropReprojMaskWGDAL(rasterBig, useSAcrs = FALSE,
                              rasterToMatch = rasterSmall, dots = list(),
                              cores = 1)
  expect_true(raster::compareRaster(out2, rasterSmall))

  ccc <- testthat::capture_output(
    out3 <- cropReprojMaskWGDAL(rasterBig, ncSmall, useSAcrs = FALSE,
                                dots = list(),
                                cores = 1)
  )
  expect_true(raster::compareCRS(out3, rasterBig))

  ccc <- testthat::capture_output(
    expect_error(out3a <- cropReprojMaskWGDAL(rasterBig, ncSmall, useSAcrs = TRUE,
                                              dots = list(),
                                              cores = 1), regexp = "Cannot set useSAcrs to TRUE")
  )
  ncSmallCRSNonLatLong <- sf::st_transform(ncSmall, crs = st_crs(rasterSmall))
  ccc <- testthat::capture_output(
    expect_error(out3b <- cropReprojMaskWGDAL(rasterBig, ncSmallCRSNonLatLong, useSAcrs = TRUE,
                                              dots = list(),
                                              cores = 1), regexp = "Cannot set useSAcrs to TRUE")
  )

  rasterBigOnDisk <- writeRaster(rasterBig, file = tmpfile[2], overwrite = TRUE)
  out4 <- cropReprojMaskWGDAL(rasterBigOnDisk, rasterToMatch = rasterSmall,
                              dots = list(),
                              cores = 1)
  expect_true(raster::compareRaster(out4, rasterSmall))

  #  }
})
