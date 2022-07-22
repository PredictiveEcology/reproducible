test_that("postProcess doesn't work (part 1)", {
  testInitOut <- testInit(c("raster", "sf"), tmpFileExt = c(".grd", ".grd", ".tif", ".tif"),
                          opts = list(
                            "rasterTmpDir" = tempdir2(rndstr(1,6)),
                            "reproducible.inputPaths" = NULL,
                            "reproducible.overwrite" = TRUE)
  )
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  options("reproducible.cachePath" = tmpdir)

  for (TorF in c(FALSE, TRUE)) {
    opts <- options("reproducible.useTerra" = TorF)

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
    expect_equal(extent(b), extent(ncSmall))
    expect_true(sf::st_area(b) < sf::st_area(nc1))

    r <- suppressWarnings(raster(nc1, res = 1000)) # TODO: temporary until raster crs fixes
    # rSmall <- suppressWarnings(raster(ncSmall, res = 1000)) # TODO: temporary until raster crs fixes

    rB <- suppressWarnings(raster(nc1, res = 4000)) # TODO: temporary until raster crs fixes
    rSmall <- suppressWarnings(raster(ncSmall, res = 4000)) # TODO: temporary until raster crs fixes

    # Tests with RasterBrick
    r2 <- r1 <- rB
    r1[] <- runif(ncell(rB)) * 100000
    r2[] <- runif(ncell(rB)) * 100000

    b <- raster::brick(r1, r2)
    b1 <- postProcess(b, studyArea = ncSmall, useCache = FALSE)
    expect_true(inherits(b1, "RasterBrick"))

    ss <- raster::stack(r1, r2)
    s1 <- postProcess(ss, studyArea = ncSmall, useCache = FALSE)
    expect_true(inherits(s1, "RasterStack"))
    expect_equal(s1[], b1[], ignore_attr = TRUE)
    # expect_equivalent(s1, b1) # deprecated in testthat

    file.remove(tmpfile[3])
    b <- .writeRaster(terra::rast(b), filename = tmpfile[3], overwrite = TRUE) # changes it to SpatRaster
    b1 <- postProcess(b, studyArea = ncSmall, useCache = FALSE,
                      filename2 = tmpfile[2], overwrite = TRUE)
    expect_true(inherits(b1, "SpatRaster"))

    ss <- .writeRaster(terra::rast(ss), filename = tmpfile[4], overwrite = TRUE)
    s1 <- postProcess(ss, studyArea = ncSmall, useCache = FALSE, filename2 = tmpfile[3], overwrite = TRUE)
    expect_true(inherits(raster::stack(terra::rast(s1)), "RasterStack"))
    expect_true(identical(Filenames(s1, allowMultiple = FALSE), tmpfile[3]))
    # it was masked
    expect_true(sum(values(s1), na.rm = TRUE) < (0.8 * sum(values(ss), na.rm = TRUE)))

    # Test datatype setting
    dt1 <- "INT2U"
    ss <- .writeRaster(ss, filename = tmpfile[3], overwrite = TRUE)
    s1 <- postProcess(ss, studyArea = ncSmall, useCache = FALSE, filename2 = tmpfile[4], overwrite = TRUE,
                      datatype = dt1)
    ss1A_FileSize <- file.size(Filenames(s1))
    expect_true(max(abs(range(terra::minmax(s1)) - c(0, 65534))) < 10) # because INT -- all values became 0 to 9

    # Test datatype setting
    dt1 <- "INT4U"
    ss <- .writeRaster(ss, filename = tmpfile[4], overwrite = TRUE)
    b1 <- postProcess(ss, studyArea = ncSmall, useCache = FALSE, filename2 = tmpfile[3], overwrite = TRUE,
                      datatype = dt1)
    bb1A_FileSize <- file.size(Filenames(b1))
    expect_true(max(abs(range(terra::minmax(b1)) - c(0, 99999))) < 10)
    expect_true(bb1A_FileSize > ss1A_FileSize)

    # now raster with sf ## TODO: temporarily skip these tests due to fasterize not being updated yet for crs changes
    if (requireNamespace("fasterize")) {
      r1 <- fasterize::fasterize(nc1, r)
      r2 <- postProcess(r1, studyArea = ncSmall, filename2 = NULL)
      expect_true(is(r2, "RasterLayer"))
      expect_true(ncell(r2) < ncell(r1))
      expect_true(similarExtents(extent(ncSmall), extent(r2), res(r2)[1] * 2.5))
      # expect_true((xmin(extent(ncSmall)) - xmin(r2)) < (res(r2)[1] * 2))
      # expect_true((ymin(extent(ncSmall)) - ymin(r2)) < (res(r2)[2] * 2.5))
      # expect_true((ymax(extent(ncSmall)) - ymax(r2)) > -(res(r2)[2] * 2.5))
      # expect_true((xmax(extent(ncSmall)) - xmax(r2)) > -(res(r2)[2] * 2))

      # postProcess
      expect_true(identical(1, postProcess(1)))
      expect_true(identical(list(1, 1), postProcess(list(1, 1))))
      expect_error(postProcess(nc1, rasterToMatch = r))
      nc2 <- postProcess(nc1, studyArea = as(ncSmall, "Spatial"))
      expect_equal(st_area(nc2), st_area(ncSmall))

      # cropInputs
      expect_true(identical(1, cropInputs(1)))
      nonLatLongProj2 <- paste("+proj=lcc +lat_1=51 +lat_2=77 +lat_0=0 +lon_0=-95",
                               "+x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs")
      nc3 <- suppressWarningsSpecific(falseWarnings = "Discarded datum Unknown based",
                                      terra::project(terra::vect(nc1), nonLatLongProj2))#CRSobj = CRS(nonLatLongProj2)))

      nc4 <- cropInputs(nc3, studyArea = ncSmall)
      # nc4 <- cropTo(nc3, ncSmall)
      ncSmall2 <- terra::project(terra::vect(ncSmall), nonLatLongProj2)
      ee <- terra::ext(nc4)

      expect_true(isTRUE(similarExtents(ee, terra::ext(ncSmall2), closeEnough = terra::res(r)[1])))

      # pass through
      nc4 <- cropInputs(nc3, studyArea = 1)
      expect_true(identical(nc4, nc3))

      ncSmallShifted <- ncSmall + 10000000
      ncSmallShifted <- st_as_sf(ncSmallShifted)
      st_crs(ncSmallShifted) <- st_crs(ncSmall)
      mess <- cropTo(ncSmall, cropTo = ncSmallShifted)
      expect_true(NROW(mess) == 0) # didn't overlap -- used to be error
      # expect_true(any(grepl("with no data", mess)))

      # cropInputs.sf
      nc3 <- st_transform(nc1, crs = CRS(nonLatLongProj2))
      nc4 <- cropTo(nc3, cropTo = ncSmall)
      # nc4 <- cropInputs(nc3, studyArea = ncSmall)
      ncSmall2 <- st_transform(ncSmall, crs = CRS(nonLatLongProj2))
      expect_true(isTRUE(similarExtents(extent(nc4), extent(ncSmall2), closeEnough = res(r)[1])))
      # expect_true(isTRUE(all.equal(extent(nc4), extent(ncSmall2))))

      # studyArea as spatial object
      nc5 <- cropTo(nc3, cropTo = as(ncSmall, "Spatial"))
      # nc5 <- cropInputs(nc3, studyArea = as(ncSmall, "Spatial"))
      ncSmall2 <- st_transform(ncSmall, crs = CRS(nonLatLongProj2))
      expect_true(similarExtents(extent(nc5), extent(ncSmall2), res(r)[1]))
      expect_true(isTRUE(all.equal(extent(nc5), extent(nc4))))

      # studyArea pass through
      nc5 <- cropTo(nc3, cropTo = 1)
      expect_identical(nc5, nc3)

      # rasterToMatch
      browser()

      nc5 <- cropTo(nc3, cropTo = r)
      nc5_b <- projectTo(nc5, projectTo = r)
      nc5Extent_r <- st_transform(nc5, crs = crs(r))
      expect_true(similarExtents(extent(r), extent(nc5Extent_r), res(r)[1]))

      ncSmallShifted <- ncSmall + 10000000
      ncSmallShifted <- st_as_sf(ncSmallShifted)
      st_crs(ncSmallShifted) <- st_crs(ncSmall)
      mess <- capture_messages(out <- cropTo(ncSmall, cropTo = ncSmallShifted))
      # expect_true(any(grepl("polygons do not intersect", mess)))
      expect_true(NROW(out) == 0) # polygons do not intersect

      # LINEARRING Example
      p6 = terra::vect("POLYGON ((0 60, 0 0, 60 0, 60 20, 100 20, 60 20, 60 60, 0 60))")
      p6a <- fixErrorsTerra(p6)
      expect_true(terra::is.valid(p6a))
      expect_false(terra::is.valid(p6))
      # projectInputs pass through
      nc5 <- expect_error(projectInputs(x = 1))
      # expect_identical(nc5, 1)
    }
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
  skip_on_ci()
  skip_if_not(requireNamespace("sf", quietly = TRUE))

  testInitOut <- testInit("raster", opts = list(
    "rasterTmpDir" = tempdir2(rndstr(1,6)),
    "reproducible.overwrite" = TRUE,
    "reproducible.inputPaths" = NULL
  ), needGoogle = TRUE)
  skip_if(!Require::internetExists())
  skip_if_no_token()
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
    noisyOutput <- capture.output(
      suppressWarningsSpecific(falseWarnings = "attribute variables|st_buffer does not correctly buffer longitude",
                               roads[[i]] <- prepInputs(targetFile = "miniRoad.shp",
                                                        alsoExtract = "similar",
                                                        url = "https://drive.google.com/file/d/1Z6ueq8yXtUPuPWoUcC7_l2p0_Uem34CC",
                                                        studyArea = smallSA,
                                                        useCache = FALSE,
                                                        fun = "sf::st_read",
                                                        destinationPath = tmpdir,
                                                        filename2 = "miniRoads"))
    )
    # clearCache()
    noisyOutput <- capture.output(
      roads[[i + 2]] <- prepInputs(targetFile = "miniRoad.shp",
                                   alsoExtract = "similar",
                                   url = "https://drive.google.com/file/d/1Z6ueq8yXtUPuPWoUcC7_l2p0_Uem34CC",
                                   # studyArea = smallSA,
                                   useCache = FALSE,
                                   fun = "sf::st_read",
                                   destinationPath = tmpdir,
                                   filename2 = "miniRoads")
    )
    # clearCache()
    attr(roads[[i]], "tags") <- NULL
  }

  # There are floating point issues with 32 bit vs 64 bit approaches. The following fails:
  # expect_true(all.equal(roads[[1]], roads[[2]], check.attributes = FALSE))

  diffs <- sum(abs(unlist(lapply(sf::st_geometry(roads[[1]]), as.numeric)) -
                     unlist(lapply(sf::st_geometry(roads[[2]]), as.numeric))))
  expect_true(diffs < 0.0001)
  expect_true(all.equal(roads[[3]], roads[[4]], check.attributes = FALSE))
  expect_true(compareRaster(raster(extent(roads[[1]])), raster(extent(smallSA))))
  expect_error(compareRaster(raster(extent(roads[[3]])), raster(extent(smallSA))))
  expect_true(extent(roads[[3]]) > extent(roads[[1]]))

})


test_that("prepInputs doesn't work (part 3)", {
  skip_if(!Require::internetExists())
  if (interactive()) {
    testInitOut <- testInit(opts = list(reproducible.useTerra = TRUE))
    on.exit({
      testOnExit(testInitOut)
    }, add = TRUE)

    # Tati'ss reprex
    wd <- checkPath(file.path(getwd(), "reprex"), create = TRUE)
    ranges <- prepInputs(url = "https://drive.google.com/file/d/1AfGfRjaDsdq3JqcsidGRo3N66OUjRJnn",
                         destinationPath = wd,
                         fun = "sf::st_read")
    LCC05 <- prepInputs(url = "https://drive.google.com/file/d/1g9jr0VrQxqxGjZ4ckF6ZkSMP-zuYzHQC",
                        targetFile = "LCC2005_V1_4a.tif",
                        studyArea = ranges,
                        destinationPath = wd)
    sumNonNAs <- sum(!is.na(!LCC05[]))

    # These are suitably vague that they will capture the mask if it gets it right
    expect_true(sumNonNAs < 37193550)
    expect_true(sumNonNAs > 37192540)
  }}
)

