test_that("prepInputs doesn't work (part 1)", {

  testInitOut <- testInit(c("raster", "sf"), opts = list(
    "rasterTmpDir" = file.path(tempdir(), "raster"),
    "reproducible.inputPaths" = NULL,
    "reproducible.overwrite" = TRUE)
  )
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  options("reproducible.cachePath" = tmpdir)

  # Add a study area to Crop and Mask to
  # Create a "study area"
  coords <- structure(c(-122.98, -116.1, -99.2, -106, -122.98, 59.9, 65.73, 63.58, 54.79, 59.9),
                      .Dim = c(5L, 2L))
  Sr1 <- Polygon(coords)
  Srs1 <- Polygons(list(Sr1), "s1")
  StudyArea <- SpatialPolygons(list(Srs1), 1L)
  crs(StudyArea) <- "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

  nonLatLongProj <- "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"
  #dPath <- file.path(tmpdir, "ecozones")
  nc <- st_as_sf(StudyArea)#system.file("shape/nc.shp", package="sf"))
  nc1 <- st_transform(nc, nonLatLongProj)
  ncSmall <- st_buffer(nc1, dist = -10000)
  b <- postProcess(nc1, studyArea = ncSmall, filename2 = NULL)
  expect_true(is(b, "sf"))
  expect_true(identical(extent(b), extent(ncSmall)))
  expect_true(st_area(b) < st_area(nc1))

  r <- raster(nc1, res = 1000)
  rSmall <- raster(ncSmall, res = 1000)

  # now raster with sf
  r1 <- fasterize(nc1, r)
  r2 <- postProcess(r1, studyArea = ncSmall, filename2 = NULL)
  expect_true(is(r2, "RasterLayer"))
  expect_true(ncell(r2) < ncell(r1))
  expect_true( (xmin(extent(ncSmall)) - xmin(r2)) < res(r2)[1] * 2)
  expect_true( (ymin(extent(ncSmall)) - ymin(r2)) < res(r2)[2] * 2)
  expect_true( (ymax(extent(ncSmall)) - ymax(r2)) > -(res(r2)[2] * 2))
  expect_true( (xmax(extent(ncSmall)) - xmax(r2)) > -(res(r2)[2] * 2))


})
