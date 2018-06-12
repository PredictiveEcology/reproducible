test_that("prepInputs doesn't work", {
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  testthat::skip_on_appveyor()

  tmpdir <- file.path(tempdir(), paste(collapse = "", sample(LETTERS, 5)))
  checkPath(tmpdir, create = TRUE)
  cwd <- getwd()
  setwd(tmpdir)

  on.exit({
    setwd(cwd)
    unlink(tmpdir, recursive = TRUE)
  }, add = TRUE)

  dPath <- file.path(tempdir(), "ecozones")
  mess <- capture_messages(
    shpEcozone <- prepInputs(destinationPath = dPath,
                             url = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip")
  )
  expect_true(any(grepl(mess, pattern = "ecozone_shp.zip")))
  expect_true(any(grepl(mess, pattern = "Appending")))
  expect_true(any(grepl(mess, pattern = "Finished")))
  expect_true(is(shpEcozone, "SpatialPolygons"))

  # Robust to partial file deletions:
  unlink(dir(dPath, full.names = TRUE)[1:3])
  expect_error(raster::shapefile(file.path(dPath, "ecozone_shp.zip")))
  rm(shpEcozone)
  shpEcozone1 <- prepInputs(destinationPath = dPath,
                            url = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip")
  expect_true(is(shpEcozone1, "SpatialPolygons"))
  unlink(dPath, recursive = TRUE)

  # Once this is done, can be more precise in operational code:
  #  specify targetFile, alsoExtract, and fun, wrap with Cache
  ecozoneFilename <- file.path(dPath, "ecozones.shp")
  ecozoneFiles <- c(
    "ecozones.dbf",
    "ecozones.prj",
    "ecozones.sbn",
    "ecozones.sbx",
    "ecozones.shp",
    "ecozones.shx"
  )
  shpEcozone2 <- prepInputs(
    targetFile = ecozoneFilename,
    url = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip",
    alsoExtract = ecozoneFiles,
    fun = "shapefile",
    destinationPath = dPath
  )
  expect_true(is(shpEcozone2, "SpatialPolygons"))
  expect(identical(shpEcozone1, shpEcozone2))
  unlink(dPath, recursive = TRUE)

  # Add a study area to Crop and Mask to
  # Create a "study area"
  library(sp)
  library(raster)
  coords <- structure(c(-122.98, -116.1, -99.2, -106, -122.98,
                        59.9, 65.73, 63.58, 54.79, 59.9),
                      .Dim = c(5L, 2L))
  Sr1 <- Polygon(coords)
  Srs1 <- Polygons(list(Sr1), "s1")
  StudyArea <- SpatialPolygons(list(Srs1), 1L)
  crs(StudyArea) <- "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

  #  specify targetFile, alsoExtract, and fun, wrap with Cache
  ecozoneFilename <- file.path(dPath, "ecozones.shp")
  # Note, you don't need to "alsoExtract" the archive... if the archive is not there, but the
  #   targetFile is there, it will not redownload the archive.
  ecozoneFiles <- c(
    "ecozones.dbf",
    "ecozones.prj",
    "ecozones.sbn",
    "ecozones.sbx",
    "ecozones.shp",
    "ecozones.shx"
  )
  shpEcozoneSm <- Cache(
    prepInputs,
    url = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip",
    targetFile = reproducible::asPath(ecozoneFilename),
    alsoExtract = reproducible::asPath(ecozoneFiles),
    studyArea = StudyArea,
    fun = "shapefile",
    destinationPath = dPath,
    filename2 = "EcozoneFile.shp"
  ) # passed to determineFilename
  expect_true(is(shpEcozoneSm, "SpatialPolygons"))
  expect_identical(extent(shpEcozoneSm), extent(StudyArea))

  #plot(shpEcozone)
  #plot(shpEcozoneSm, add = TRUE, col = "red")
  unlink(dPath)

  # Big Raster, with crop and mask to Study Area - no reprojecting (lossy) of raster,
  #   but the StudyArea does get reprojected, need to use rasterToMatch
  dPath <- file.path(tempdir(), "LCC")
  lcc2005Filename <- file.path(dPath, "LCC2005_V1_4a.tif")
  url <- file.path("ftp://ftp.ccrs.nrcan.gc.ca/ad/NLCCLandCover",
                   "LandcoverCanada2005_250m/LandCoverOfCanada2005_V1_4.zip")

  # messages received below may help for filling in more arguments in the subsequent call
  LCC2005 <- prepInputs(
    url = url,
    destinationPath = asPath(dPath),
    studyArea = StudyArea
  )
  expect_is(LCC2005, "Raster")
  StudyAreaCRSLCC2005 <- spTransform(StudyArea, crs(LCC2005))
  expect_identical(extent(LCC2005)[1:4],
                   round(extent(StudyAreaCRSLCC2005)[1:4] / 250, 0) * 250)

  # if wrapped with Cache, will be fast second time, very fast 3rd time (via memoised copy)
  LCC2005_2 <- Cache(
    prepInputs,
    url = url,
    targetFile = lcc2005Filename,
    archive = asPath("LandCoverOfCanada2005_V1_4.zip"),
    destinationPath = asPath(dPath),
    studyArea = StudyArea
  )
  expect_is(LCC2005_2, "Raster")
  expect_equivalent(LCC2005, LCC2005_2)

  setwd(tempdir())
  if (interactive()) {
    # need authentication for this
    test <- prepInputs(
      targetFile = "FMA_Boundary_Updated.shp",
      url = "https://drive.google.com/file/d/1nTFOcrdMf1hIsxd_yNCSTr8RrYNHHwuc/view?usp=sharing",
      destinationPath = "data/FMA"
    )
    expect_is(test, "SpatialPolygons")
  }
})
