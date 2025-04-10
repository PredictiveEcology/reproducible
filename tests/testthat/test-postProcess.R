test_that("prepInputs doesn't work (part 3)", {
  skip_on_cran() # too long
  skip_if(getRversion() < "4.1" && isWindows()) # old Windows is failing; not going to fix tests for those
  skip_if_not_installed("sf")
  testInit(c("terra", "sf"),
    tmpFileExt = c(".tif", ".tif", ".tif"),
    opts = list(
      "rasterTmpDir" = tempdir2(rndstr(1, 6)),
      "reproducible.inputPaths" = NULL,
      "reproducible.overwrite" = TRUE,
      "rgdal_show_exportToProj4_warnings" = "none"
    ) # https://gis.stackexchange.com/questions/390945/importing-raster-files-warning-and-extracting-covariates-error-with-raster-and
  )

  withr::local_options("reproducible.cachePath" = tmpdir)

  # Add a study area to Crop and Mask to
  # Create a "study area"

  coords <- structure(c(-122.98, -116.1, -99.2, -106, -122.98, 59.9, 65.73, 63.58, 54.79, 59.9),
    .Dim = c(5L, 2L)
  )
  coords2 <- structure(c(-115.98, -116.1, -99.2, -106, -122.98, 59.9, 65.73, 63.58, 54.79, 59.9),
    .Dim = c(5L, 2L)
  )

  StudyArea <- terra::vect(coords, "polygons")
  terra::crs(StudyArea) <- crsToUse
  StudyArea2 <- terra::vect(coords2, "polygons")
  terra::crs(StudyArea2) <- crsToUse

  # coords <- structure(c(-122.98, -116.1, -99.2, -106, -122.98, 59.9, 65.73, 63.58, 54.79, 59.9),
  #                     .Dim = c(5L, 2L))
  # coords2 <- structure(c(-115.98, -116.1, -99.2, -106, -122.98, 59.9, 65.73, 63.58, 54.79, 59.9),
  #                      .Dim = c(5L, 2L))
  # Sr1 <- Polygon(coords)
  # Srs1 <- Polygons(list(Sr1), "s1")
  # StudyArea <- SpatialPolygons(list(Srs1), 1L)
  # crs(StudyArea) <- crsToUse
  #
  # Sr1 <- Polygon(coords2)
  # Srs1 <- Polygons(list(Sr1), "s1")
  # StudyArea2 <- SpatialPolygons(list(Srs1), 1L)
  # crs(StudyArea2) <- crsToUse

  nonLatLongProj <- paste(
    "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
    "+x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"
  )
  nc <- sf::st_as_sf(StudyArea) # system.file("shape/nc.shp", package="sf"))
  nc1 <- sf::st_transform(nc, nonLatLongProj)
  ncSmall <- sf::st_as_sf(StudyArea2)
  ncSmall <- sf::st_transform(ncSmall, nonLatLongProj)
  ncSmall <- sf::st_buffer(ncSmall, dist = -10000)
  b <- postProcess(nc1, studyArea = ncSmall, filename2 = NULL)
  expect_true(is(b, "sf"))
  expect_equal(terra::ext(b), terra::ext(ncSmall))
  expect_true(sf::st_area(b) < sf::st_area(nc1))

  r <- suppressWarnings(terra::rast(nc1, resolution = 1000)) # TODO: temporary until raster crs fixes

  rB <- suppressWarnings(terra::rast(nc1, resolution = 4000)) # TODO: temporary until raster crs fixes
  rSmall <- suppressWarnings(terra::rast(ncSmall, resolution = 4000)) # TODO: temporary until raster crs fixes

  # Tests with RasterBrick
  r2 <- r1 <- rB
  r1[] <- runif(terra::ncell(rB))
  r2[] <- runif(terra::ncell(rB))

  b <- c(r1, r2)
  terra::crs(b) <- sf::st_crs(ncSmall)$input
  b1 <- postProcess(b, studyArea = ncSmall, useCache = FALSE)
  expect_true(inherits(b1, "SpatRaster"))

  s <- c(r1, r2)
  crs(s) <- crs(nonLatLongProj)
  s1 <- postProcess(s, studyArea = ncSmall, useCache = FALSE)
  expect_true(inherits(s1, "SpatRaster"))
  expect_equal(s1[], b1[], ignore_attr = TRUE)

  b <- writeRaster(b, filename = tmpfile[1], overwrite = TRUE)
  b1 <- postProcess(b, studyArea = ncSmall, useCache = FALSE, writeTo = tmpfile[2], overwrite = TRUE)
  expect_true(inherits(b1, "SpatRaster"))

  s1 <- postProcess(s, studyArea = ncSmall, useCache = FALSE, writeTo = tmpfile[2], overwrite = TRUE)
  expect_true(inherits(s1, "SpatRaster"))

  # Test datatype setting
  dt1 <- "INT2U"
  s <- writeRaster(s, filename = tmpfile[2], overwrite = TRUE)
  s1 <- postProcess(s,
    studyArea = ncSmall, useCache = FALSE, writeTo = tmpfile[1], overwrite = TRUE,
    datatype = dt1
  )
  expect_identical(terra::datatype(s1), rep(dt1, terra::nlyr(s)))

  # Test datatype setting
  dt1 <- c("INT2U", "INT4U")
  s <- writeRaster(s, filename = tmpfile[1], overwrite = TRUE)
  warns <- capture_error({
    s1 <- postProcess(s,
      studyArea = ncSmall, useCache = FALSE, writeTo = tmpfile[2], overwrite = TRUE,
      datatype = dt1
    )
  })
  expect_true(any(grepl("Expecting a single", warns)))

  dt1 <- "INT4U"
  b <- writeRaster(b, filename = tmpfile[2], overwrite = TRUE)
  b1 <- postProcess(b,
    studyArea = ncSmall, useCache = FALSE, writeTo = tmpfile[1], overwrite = TRUE,
    datatype = dt1
  )
  expect_identical(terra::datatype(b1), rep(dt1, terra::nlyr(b1)))

  # now raster with sf
  skip_if_not_installed("terra")
  r1 <- terra::rasterize(terra::vect(nc1), r)
  r2 <- postProcess(r1, studyArea = ncSmall, filename2 = NULL)
  expect_true(is(r2, "SpatRaster"))
  expect_true(terra::ncell(r2) < terra::ncell(r1))
  expect_true((terra::xmin(terra::ext(ncSmall)) - terra::xmin(r2)) < terra::res(r2)[1] * 2)
  expect_true((terra::ymin(terra::ext(ncSmall)) - terra::ymin(r2)) < terra::res(r2)[2] * 2)
  expect_true((terra::ymax(terra::ext(ncSmall)) - terra::ymax(r2)) > -(terra::res(r2)[2] * 2))
  expect_true((terra::xmax(terra::ext(ncSmall)) - terra::xmax(r2)) > -(terra::res(r2)[2] * 2))

  # postProcess
  expect_error(postProcess(1, to = r2), regexp = "from must be a")
  expect_error(postProcess(list(1, 1), to = r2), regexp = "from must be a")

  nc2 <- postProcess(nc1, studyArea = as(ncSmall, "sf"))
  expect_equal(st_area(nc2), st_area(ncSmall))

  # cropInputs
  expect_true(identical(1, cropInputs(1)))
  nonLatLongProj2 <- paste(
    "+proj=lcc +lat_1=51 +lat_2=77 +lat_0=0 +lon_0=-95",
    "+x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"
  )
  nc3 <- suppressWarningsSpecific(
    {
      sf::st_transform(nc1, CRSobj = nonLatLongProj2)
    },
    falseWarnings = "Discarded datum Unknown based on GRS80 ellipsoid in Proj4 definition|PROJ support is provided by the sf and terra packages among others"
  )
  nc4 <- cropInputs(nc3, studyArea = ncSmall)
  ncSmall2 <- suppressWarningsSpecific(
    {
      sf::st_transform(ncSmall, CRSobj = nonLatLongProj2)
    },
    falseWarnings = "Discarded datum Unknown based on GRS80 ellipsoid in Proj4 definition|PROJ support is provided by the sf and terra packages among others"
  )
  expect_true(isTRUE(all.equal(terra::ext(nc4), terra::ext(ncSmall2))))

  mess <- capture_error({
    nc4 <- cropInputs(nc3, studyArea = 1)
  })
  expect_s3_class(mess, "simpleError")

  ncSmallShifted <- ncSmall + 10000000
  ncSmallShifted <- st_as_sf(ncSmallShifted)
  st_crs(ncSmallShifted) <- st_crs(ncSmall)
  mess <- capture_messages(
    cropInputs(ncSmall, studyArea = ncSmallShifted)
  )
  expect_true(any(grepl("overlap", mess)))

  # cropInputs.sf
  nc3 <- st_transform(nc1, crs = nonLatLongProj2)
  nc4 <- cropInputs(nc3, studyArea = ncSmall)
  ncSmall2 <- st_transform(ncSmall, crs = nonLatLongProj2)
  expect_true(isTRUE(all.equal(terra::ext(nc4), terra::ext(ncSmall2))))

  # studyArea as spatial object
  nc5 <- cropInputs(nc3, studyArea = ncSmall)
  ncSmall2 <- st_transform(ncSmall, crs = nonLatLongProj2)
  expect_true(isTRUE(all.equal(terra::ext(nc5), terra::ext(ncSmall2))))
  expect_true(isTRUE(all.equal(terra::ext(nc5), terra::ext(nc4))))


  # rasterToMatch
  nc5 <- cropTo(nc3, cropTo = r)
  nc5Extent_r <- st_transform(nc5, crs = crs(r))
  expect_true(isTRUE(abs(terra::xmin(r) - sf::st_bbox(nc5Extent_r)["xmin"]) < terra::res(r)[1]))
  expect_true(isTRUE(abs(terra::ymin(r) - sf::st_bbox(nc5Extent_r)["ymin"]) < terra::res(r)[1]))
  expect_true(isTRUE(abs(terra::xmax(r) - sf::st_bbox(nc5Extent_r)["xmax"]) < terra::res(r)[1]))
  expect_true(isTRUE(abs(terra::ymax(r) - sf::st_bbox(nc5Extent_r)["ymax"]) < terra::res(r)[1]))

  ncSmallShifted <- ncSmall + 10000000
  ncSmallShifted <- st_as_sf(ncSmallShifted)
  st_crs(ncSmallShifted) <- st_crs(ncSmall)
  expect_message(
    regexp = "results in no data",
    aaa <- cropInputs(ncSmall, studyArea = ncSmallShifted)
  )
  expect_s3_class(aaa, "sf")
  # expect_true(NROW(out11) == 0)

  # LINEARRING Example
  p6 <- terra::vect("POLYGON ((0 60, 0 0, 60 0, 60 20, 100 20, 60 20, 60 60, 0 60))")
  p6a <- fixErrorsIn(p6)
  expect_true(terra::is.valid(p6a))
  expect_false(terra::is.valid(p6))
  # projectInputs pass through
  expect_error(projectInputs(x = 1), "argument .+ is missing")

  #using deprecated filename2 arg
  expect_error(cropInputs(ncSmall, studyArea = ncSmallShifted, filename2 = "use_WriteTo_Instead.shp"))

})

test_that("writeOutputs with non-matching writeTo", {
  testInit(c("terra"), tmpFileExt = c(".grd", ".tif"))

  r <- terra::rast(terra::ext(0, 10, 0, 10), vals = rnorm(100))
  r <- terra::writeRaster(r, filename = tmpfile[1], overwrite = TRUE)
  r[] <- r[]
  warn <- capture_warnings({
    r1 <- writeOutputs(r, writeTo = tmpfile[2])
  })
  r2 <- terra::rast(Filenames(r1))
  vals1 <- r2[]
  vals2 <- r1[]
  vals3 <- r[]
  expect_true(identical(vals1, vals2))
  expect_true(identical(vals1, vals3))
  expect_false(identical(normPath(Filenames(r)), normPath(Filenames(r1))))
  expect_true(identical(normPath(Filenames(r2)), normPath(Filenames(r1))))
})

test_that("cropInputs crops too closely when input projections are different", {
  skip_on_cran()
  # skip_on_ci() ## TODO: why is this failing on GHA but not locally??? (2022-11-04)

  testInit("terra", opts = list(
    "rasterTmpDir" = tempdir2(rndstr(1, 6)),
    "reproducible.overwrite" = TRUE,
    "reproducible.inputPaths" = NULL,
    "rgdal_show_exportToProj4_warnings" = "none" # https://gis.stackexchange.com/questions/390945/importing-raster-files-warning-and-extracting-covariates-error-with-raster-and
  ), needGoogleDriveAuth = TRUE)

  ext2 <- terra::ext(c(
    xmin = -3229772.32501426,
    xmax = 3680227.67498574,
    ymin = -365833.605586135,
    ymax = 3454166.39441387
  ))
  x <- terra::rast(ext2,
    crs = paste(
      "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0",
      "+a=6370997 +b=6370997 +units=m +no_defs"
    ),
    resolution = c(10000, 10000)
  )
  x <- terra::setValues(x, 1)

  RTMext <- terra::ext(c(
    xmin = -1613500.00000023,
    xmax = -882500.000000228,
    ymin = 7904500,
    ymax = 9236000
  ))
  RTM <- terra::rast(RTMext,
    crs = paste(
      "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0",
      "+ellps=GRS80 +units=m +no_defs"
    ),
    resolution = c(250, 250)
  )
  RTM <- setValues(RTM, 2)
  out <- postProcess(x = x, rasterToMatch = RTM, filename2 = NULL)
  expect_true(nrow(out[is.na(out[]) & is.na(RTM[])]) == 0)
})

test_that("maskInputs errors when x is Lat-Long", {
  skip_on_cran()
  skip_on_ci()
  testInit("sf", opts = list(
    "rasterTmpDir" = tempdir2(rndstr(1, 6)),
    "reproducible.overwrite" = TRUE,
    "reproducible.inputPaths" = NULL
  ), needGoogleDriveAuth = TRUE)

  i <- 0
  roads <- list()
  clearCache()

  smallSA <- terra::ext(c(
    -117.580383168455, -117.43120279669,
    61.0576330401172, 61.0937107807574
  ))

  x <- terra::rast(smallSA,
    crs = "+proj=longlat +ellps=GRS80 +no_defs",
    resolution = c(0.001, 0.001)
  )
  suppressWarnings(smallSA <- terra::vect(terra::ext(x), "polygons"))
  terra::crs(smallSA) <- terra::crs(x)

  noisyOutput <- capture.output(
    suppressWarningsSpecific(
      falseWarnings = "attribute variables|st_buffer does not correctly buffer longitude",
      roads1 <- prepInputs(
        targetFile = "miniRoad.shp",
        alsoExtract = "similar",
        url = "https://drive.google.com/file/d/1Z6ueq8yXtUPuPWoUcC7_l2p0_Uem34CC",
        studyArea = smallSA,
        useCache = FALSE,
        fun = "sf::st_read",
        destinationPath = tmpdir,
        writeTo = "miniRoad.shp"
      )
    )
  )
  # clearCache()
  noisyOutput <- capture.output(
    roads2 <- prepInputs(
      targetFile = "miniRoad.shp",
      alsoExtract = "similar",
      url = "https://drive.google.com/file/d/1Z6ueq8yXtUPuPWoUcC7_l2p0_Uem34CC",
      # studyArea = smallSA,
      useCache = FALSE,
      fun = "sf::st_read",
      destinationPath = tmpdir,
      filename2 = "miniRoads"
    )
  )
  attr(roads1, "tags") <- NULL

  # There are floating point issues with 32 bit vs 64 bit approaches. The following fails:
  # expect_true(all.equal(roads[[1]], roads[[2]], check.attributes = FALSE))

  # diffs <- sum(abs(unlist(lapply(sf::st_geometry(roads[[1]]), as.numeric)) -
  #                    unlist(lapply(sf::st_geometry(roads[[2]]), as.numeric))))
  # expect_true(diffs < 0.0001)
  expect_true(terra::compareGeom(terra::rast(terra::ext(roads1)), terra::rast(terra::ext(smallSA))))
  expect_error(terra::compareGeom(terra::rast(terra::ext(roads2)), terra::rast(terra::ext(smallSA))))
  expect_true(terra::ext(roads2) > terra::ext(roads1))
})

# test_that("prepInputs doesn't work (part 3)", {
#   skip("The Google Drive url is dead")
#   if (interactive()) {
#     testInit(needGoogleDriveAuth = TRUE)
#
#     # Tati's reprex
#     # tmpdir <- "/mnt/d/temp/Cache"
#     wd <- checkPath(file.path(tmpdir, "reprex"), create = TRUE)
#     ranges <- prepInputs(
#       url = "https://drive.google.com/file/d/1AfGfRjaDsdq3JqcsidGRo3N66OUjRJnn",
#       destinationPath = wd,
#       fun = "terra::vect"
#     )
#     LCC05 <- prepInputs(
#       url = "https://drive.google.com/file/d/1g9jr0VrQxqxGjZ4ckF6ZkSMP-zuYzHQC",
#       targetFile = "LCC2005_V1_4a.tif",
#       studyArea = ranges,
#       fun = "terra::rast",
#       destinationPath = wd
#     )
#     sumNonNAs <- sum(!is.na(!LCC05[]))
#
#     # These are suitably vague that they will capture the mask if it gets it right
#     expect_true(sumNonNAs < 38000000)
#     expect_true(sumNonNAs > 37000000)
#   }
# })
