test_that("prepInputs doesn't work (part 3)", {
  # if (requireNamespace("rgeos")) {
  testInitOut <- testInit(c("raster", "sf"), tmpFileExt = c(".tif", ".tif"),
                          opts = list(
    #  testInitOut <- testInit(c("raster", "sf", "rgeos"), opts = list(
    "rasterTmpDir" = tempdir2(rndstr(1,6)),
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
  coords2 <- structure(c(-115.98, -116.1, -99.2, -106, -122.98, 59.9, 65.73, 63.58, 54.79, 59.9),
                       .Dim = c(5L, 2L))
  Sr1 <- Polygon(coords)
  Srs1 <- Polygons(list(Sr1), "s1")
  StudyArea <- SpatialPolygons(list(Srs1), 1L)
  crs(StudyArea) <- crsToUse

  Sr1 <- Polygon(coords2)
  Srs1 <- Polygons(list(Sr1), "s1")
  StudyArea2 <- SpatialPolygons(list(Srs1), 1L)
  crs(StudyArea2) <- crsToUse

  nonLatLongProj <- paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
                          "+x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs")
  #dPath <- file.path(tmpdir, "ecozones")
  nc <- sf::st_as_sf(StudyArea)#system.file("shape/nc.shp", package="sf"))
  nc1 <- sf::st_transform(nc, nonLatLongProj)
  #ncSmall <- sf::st_buffer(nc1, dist = -10000)
  ncSmall <- sf::st_as_sf(StudyArea2)
  ncSmall <- sf::st_transform(ncSmall, nonLatLongProj)
  ncSmall <- sf::st_buffer(ncSmall, dist = -10000)
  b <- postProcess(nc1, studyArea = ncSmall, filename2 = NULL)
  expect_true(is(b, "sf"))
  expect_true(identical(extent(b), extent(ncSmall)))
  expect_true(sf::st_area(b) < sf::st_area(nc1))

  r <- suppressWarnings(raster(nc1, res = 1000)) # TODO: temporary until raster crs fixes
  # rSmall <- suppressWarnings(raster(ncSmall, res = 1000)) # TODO: temporary until raster crs fixes

  rB <- suppressWarnings(raster(nc1, res = 4000)) # TODO: temporary until raster crs fixes
  rSmall <- suppressWarnings(raster(ncSmall, res = 4000)) # TODO: temporary until raster crs fixes

  # Tests with RasterBrick
  r2 <- r1 <- rB
  r1[] <- runif(ncell(rB))
  r2[] <- runif(ncell(rB))

  b <- raster::brick(r1, r2)
  b1 <- postProcess(b, studyArea = ncSmall, useCache = FALSE)
  expect_is(b1, "RasterBrick")

  s <- raster::stack(r1, r2)
  s1 <- postProcess(s, studyArea = ncSmall, useCache = FALSE)
  expect_is(s1, "RasterStack")
  expect_equivalent(s1, b1)


  b <- writeRaster(b, filename = tmpfile[1], overwrite = TRUE)
  b1 <- postProcess(b, studyArea = ncSmall, useCache = FALSE, filename2 = tmpfile[2], overwrite = TRUE)
  expect_is(b1, "RasterBrick")


  s <- raster::stack(writeRaster(s, filename = tmpfile[1], overwrite = TRUE))
  s1 <- postProcess(s, studyArea = ncSmall, useCache = FALSE, filename2 = tmpfile[2], overwrite = TRUE)
  expect_is(s1, "RasterStack")

  # Test datatype setting
  dt1 <- "INT2U"
  s <- raster::stack(writeRaster(s, filename = tmpfile[2], overwrite = TRUE))
  s1 <- postProcess(s, studyArea = ncSmall, useCache = FALSE, filename2 = tmpfile[1], overwrite = TRUE,
                    datatype = dt1)
  expect_true(identical(dataType(s1), rep(dt1,nlayers(s))))

  # Test datatype setting
  dt1 <- c("INT2U", "INT4U")
  s <- raster::stack(writeRaster(s, filename = tmpfile[1], overwrite = TRUE))
  warns <- capture_warnings(s1 <- postProcess(s, studyArea = ncSmall, useCache = FALSE, filename2 = tmpfile[2], overwrite = TRUE,
                    datatype = dt1))
  expect_true(any(grepl("can only be length", warns)))

  dt1 <- "INT4U"
  b <- writeRaster(b, filename = tmpfile[2], overwrite = TRUE)
  b1 <- postProcess(b, studyArea = ncSmall, useCache = FALSE, filename2 = tmpfile[1], overwrite = TRUE,
                    datatype = dt1)
  expect_true(identical(dataType(b1), dt1))

  # now raster with sf ## TODO: temporarily skip these tests due to fasterize not being updated yet for crs changes
  if (requireNamespace("fasterize")) {
    r1 <- fasterize::fasterize(nc1, r)
    r2 <- postProcess(r1, studyArea = ncSmall, filename2 = NULL)
    expect_true(is(r2, "RasterLayer"))
    expect_true(ncell(r2) < ncell(r1))
    expect_true((xmin(extent(ncSmall)) - xmin(r2)) < res(r2)[1] * 2)
    expect_true((ymin(extent(ncSmall)) - ymin(r2)) < res(r2)[2] * 2)
    expect_true((ymax(extent(ncSmall)) - ymax(r2)) > -(res(r2)[2] * 2))
    expect_true((xmax(extent(ncSmall)) - xmax(r2)) > -(res(r2)[2] * 2))

    # postProcess
    expect_true(identical(1, postProcess(1)))
    expect_true(identical(list(1, 1), postProcess(list(1, 1))))
    expect_error(postProcess(nc1, rasterToMatch = r))
    nc2 <- postProcess(nc1, studyArea = as(ncSmall, "Spatial"))
    expect_true(identical(st_area(nc2), st_area(ncSmall)))

    # cropInputs
    expect_true(identical(1, cropInputs(1)))
    nonLatLongProj2 <- paste("+proj=lcc +lat_1=51 +lat_2=77 +lat_0=0 +lon_0=-95",
                             "+x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs")
    nc3 <- spTransform(as(nc1, "Spatial"), CRSobj = CRS(nonLatLongProj2))
    nc4 <- cropInputs(nc3, studyArea = ncSmall)
    ncSmall2 <- spTransform(as(ncSmall, "Spatial"), CRSobj = CRS(nonLatLongProj2))
    expect_true(isTRUE(all.equal(extent(nc4), extent(ncSmall2))))

    mess <- capture_error({
      nc4 <- cropInputs(nc3, studyArea = 1)
    })
    expect_true(grepl("studyArea does not have a crs", mess))

    ncSmallShifted <- ncSmall + 10000000
    ncSmallShifted <- st_as_sf(ncSmallShifted)
    st_crs(ncSmallShifted) <- st_crs(ncSmall)
    mess <- capture_error(cropInputs(as(ncSmall, "Spatial"), studyArea = ncSmallShifted))
    expect_true(any(grepl("polygons do not intersect", mess)))
    # expect_true(any(grepl("with no data", mess)))

    #LINEARRING Example
    if (requireNamespace("rgeos", quietly = TRUE)) {
      p6 = rgeos::readWKT("POLYGON ((0 60, 0 0, 60 0, 60 20, 100 20, 60 20, 60 60, 0 60))")
      mess <- capture_messages({
        p6a <- fixErrors(p6, verbose = 2)
      })
      expect_true(any(grepl("Found errors", mess)))
      expect_true(any(grepl("Some or all of the errors fixed", mess)))
    }

    # cropInputs.sf
    nc3 <- st_transform(nc1, crs = CRS(nonLatLongProj2))
    nc4 <- cropInputs(nc3, studyArea = ncSmall)
    ncSmall2 <- st_transform(ncSmall, crs = CRS(nonLatLongProj2))
    expect_true(isTRUE(all.equal(extent(nc4), extent(ncSmall2))))

    # studyArea as spatial object
    nc5 <- cropInputs(nc3, studyArea = as(ncSmall, "Spatial"))
    ncSmall2 <- st_transform(ncSmall, crs = CRS(nonLatLongProj2))
    expect_true(isTRUE(all.equal(extent(nc5), extent(ncSmall2))))
    expect_true(isTRUE(all.equal(extent(nc5), extent(nc4))))

    # studyArea pass through
    nc5 <- cropInputs(nc3, studyArea = 1)
    expect_identical(nc5, nc3)

    # rasterToMatch
    nc5 <- cropInputs(nc3, rasterToMatch = r)
    nc5Extent_r <- st_transform(nc5, crs = crs(r))
    expect_true(isTRUE(abs(xmin(r) - xmin(as(nc5Extent_r, "Spatial"))) < res(r)[1]))
    expect_true(isTRUE(abs(ymin(r) - ymin(as(nc5Extent_r, "Spatial"))) < res(r)[1]))
    expect_true(isTRUE(abs(xmax(r) - xmax(as(nc5Extent_r, "Spatial"))) < res(r)[1]))
    expect_true(isTRUE(abs(ymax(r) - ymax(as(nc5Extent_r, "Spatial"))) < res(r)[1]))

    ncSmallShifted <- ncSmall + 10000000
    ncSmallShifted <- st_as_sf(ncSmallShifted)
    st_crs(ncSmallShifted) <- st_crs(ncSmall)
    mess <- capture_messages(cropInputs(ncSmall, studyArea = ncSmallShifted))
    expect_true(any(grepl("polygons do not intersect", mess)))

    # LINEARRING Example
    if (requireNamespace("rgeos", quietly = TRUE)) {
      p6 = rgeos::readWKT("POLYGON ((0 60, 0 0, 60 0, 60 20, 100 20, 60 20, 60 60, 0 60))")
      mess <- capture_messages({
        p6a <- fixErrors(st_as_sf(p6))
      })
      expect_true(any(grepl("Found errors", mess)))
      expect_true(any(grepl("Some or all of the errors fixed", mess)))
    }
    # projectInputs pass through
    nc5 <- projectInputs(x = 1)
    expect_identical(nc5, 1)

    # expect_error(determineFilename(inputFilePath = "a"))
    # expect_error(determineFilename(postProcessedFilename = "a"))
    # expect_error(determineFilename(targetFilePath = "a"))
  }

})

test_that("writeOutputs with non-matching filename2", {
  testInitOut <- testInit(c("raster"), tmpFileExt = c(".grd", ".tif"))
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  r <- raster(extent(0,10,0,10), vals = rnorm(100))
  r <- writeRaster(r, file = tmpfile[1], overwrite = TRUE)
  warn <- capture_warnings({
    r1 <- writeOutputs(r, filename2 = tmpfile[2])
  })
  expect_true(any(grepl("filename2 file type", warn)))
  r2 <- raster(filename(r1))
  vals1 <- r2[]
  vals2 <- r1[]
  vals3 <- r[]
  expect_true(identical(vals1, vals2))
  expect_true(identical(vals1, vals3))
  expect_false(identical(normPath(filename(r)), normPath(filename(r1))))
  expect_true(identical(normPath(filename(r2)), normPath(filename(r1))))
})

test_that("new gdalwarp all in one with grd with factor", {
  skip_on_cran()
  hasGDAL <- findGDAL()
  if (!isTRUE(hasGDAL))
    skip("no GDAL installation found")

  testInitOut <- testInit(c("raster"), tmpFileExt = c(".grd", ".tif"))
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  coords <- structure(c(-122.98, -116.1, -99.2, -106, -122.98, 59.9, 65.73, 63.58, 54.79, 59.9),
                      .Dim = c(5L, 2L))
  Sr1 <- Polygon(coords)
  Srs1 <- Polygons(list(Sr1), "s1")
  StudyArea <- SpatialPolygons(list(Srs1), 1L)
  crs(StudyArea) <- crsToUse

  r <- raster(extent(-130, -110, 55, 65), res = 1)
  crs(r) <- crsToUse
  r[] <- sample(0:10, size = ncell(r), replace = TRUE)
  df <- data.frame(ID = 0:10, label = LETTERS[1:11])
  levels(r) <- df

  # Check that the file-backed location is NOT in the cacheRepo for 1st or 2nd time.
  #  This invokes the new dealWithRastersOnRecovery fn
  rr <- postProcess(r, destinationPath = tmpdir,
                    studyArea = StudyArea, useCache = TRUE, useGDAL = "force", filename2 = TRUE)
  rr1 <- Cache(postProcess, r, destinationPath = tmpdir,
               studyArea = StudyArea, useCache = TRUE, useGDAL = "force", cacheRepo = tmpCache, filename2 = TRUE)
  rr3 <- Cache(postProcess, r, destinationPath = tmpdir,
               studyArea = StudyArea, useCache = TRUE, useGDAL = "force", cacheRepo = tmpCache, filename2 = TRUE)
  expect_true(identical(dirname(Filenames(rr)), dirname(Filenames(rr1))))
  expect_true(identical(Filenames(rr1), Filenames(rr3)))
  expect_true(!identical(dirname(Filenames(rr)), tmpCache)) # used to be that the recovery path was Cache file

  expect_true(identical(raster::levels(rr), raster::levels(r)))
  expect_true(sum(abs(raster::extent(rr)[1:4] - raster::extent(StudyArea)[1:4])) < 1)
  expect_true(sum(abs(raster::extent(r)[1:4] - raster::extent(StudyArea)[1:4])) > 1)
  rr2 <- postProcess(r, studyArea = StudyArea, useCache = FALSE, useGDAL = "force", filename2 = "out.grd")
  expect_true(identical(raster::levels(rr2), raster::levels(r)))
  expect_true(sum(abs(raster::extent(rr2)[1:4] - raster::extent(StudyArea)[1:4])) < 1)
  expect_true(!identical(filename(rr), filename(rr2)))

  expect_true(grepl(".tif$", filename(rr))) # now (Aug 12, 2020) does not exist on disk after gdalwarp -- because no filename2
  expect_true(grepl(".grd$", filename(rr2)))

  # Try to move cache -- see if rasters in Cache recover
  tmpCache2 <- file.path(dirname(tmpCache), "testCache2")
  files <- dir(tmpCache, full.names = TRUE, recursive = TRUE)
  newFiles <- gsub(basename(tmpCache), basename(tmpCache2), files)
  nnn <- lapply(dirname(newFiles), checkPath, create = TRUE)
  mmm <- Map(ff = files, nf = newFiles, function(ff, nf) file.link(ff, nf))
  unlink(files)
  expect_true(all(file.exists(newFiles)))
  expect_warning(regexp = "being updated automatically",
                 rr4 <- Cache(postProcess, r, destinationPath = tmpdir,
               studyArea = StudyArea, useCache = TRUE, useGDAL = "force",
               cacheRepo = tmpCache2, filename2 = TRUE))
  expect_identical(rr3, rr4)

})

test_that("cropInputs crops too closely when input projections are different", {
  skip_on_cran()

  testInitOut <- testInit("raster", opts = list(
    "rasterTmpDir" = tempdir2(rndstr(1,6)),
    "reproducible.overwrite" = TRUE,
    "reproducible.inputPaths" = NULL
  ), needGoogle = TRUE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  ext <- new("Extent",
             xmin = -3229772.32501426,
             xmax = 3680227.67498574,
             ymin = -365833.605586135,
             ymax = 3454166.39441387)
  x <- raster(ext,
              crs = paste("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0",
                          "+a=6370997 +b=6370997 +units=m +no_defs"),
              res = c(10000, 10000))
  x <- setValues(x, 1)

  RTMext <- new("Extent",
                xmin = -1613500.00000023,
                xmax = -882500.000000228,
                ymin = 7904500,
                ymax = 9236000)
  RTM <- raster(RTMext,
                crs = paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0",
                            "+ellps=GRS80 +units=m +no_defs"),
                res = c(250, 250))
  RTM <- setValues(RTM, 2)
  out <- postProcess(x = x, rasterToMatch = RTM, filename2 = NULL)
  expect_null(out[is.na(out) & !is.na(RTM)])
})

test_that("maskInputs errors when x is Lat-Long", {
  skip_on_cran()

  testInitOut <- testInit("raster", opts = list(
    "rasterTmpDir" = tempdir2(rndstr(1,6)),
    "reproducible.overwrite" = TRUE,
    "reproducible.inputPaths" = NULL
  ), needGoogle = TRUE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  i <- 0
  roads <- list()
  clearCache()

  smallSA <- new("Extent", xmin = -117.580383168455, xmax = -117.43120279669,
                 ymin = 61.0576330401172, ymax = 61.0937107807574)
  crs <- new("CRS", projargs = "+proj=longlat +ellps=GRS80 +no_defs")
  smallSA <- as(smallSA, "SpatialPolygons");
  crs(smallSA) <- crs

  for (ii in c(TRUE, FALSE)) {
    i <- i + 1
    options(reproducible.polygonShortcut = ii)
    suppressWarningsSpecific(falseWarnings = "attribute variables",
                             roads[[i]] <- Cache(prepInputs, targetFile = "miniRoad.shp",
                        alsoExtract = "similar",
                        url = "https://drive.google.com/file/d/1Z6ueq8yXtUPuPWoUcC7_l2p0_Uem34CC/view?usp=sharing",
                        studyArea = smallSA,
                        destinationPath = tmpdir,
                        filename2 = "miniRoads"))
    clearCache()
    roads[[i + 2]] <- Cache(prepInputs, targetFile = "miniRoad.shp",
                        alsoExtract = "similar",
                        url = "https://drive.google.com/file/d/1Z6ueq8yXtUPuPWoUcC7_l2p0_Uem34CC/view?usp=sharing",
                        # studyArea = smallSA,
                        destinationPath = tmpdir,
                        filename2 = "miniRoads")
    clearCache()
    attr(roads[[i]], "tags") <- NULL
  }
  expect_true(all.equal(roads[[1]], roads[[2]]))
  expect_true(compareRaster(raster(extent(roads[[1]])), raster(extent(smallSA))))
  expect_error(compareRaster(raster(extent(roads[[3]])), raster(extent(smallSA))))
  expect_true(extent(roads[[3]]) > extent(roads[[1]]))

})
