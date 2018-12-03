test_that("prepInputs doesn't work", {
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  testthat::skip_on_appveyor()

  testInitOut <- testInit("raster", opts = list(
    "reproducible.cachePath" = .reproducibleTempCacheDir,
    "reproducible.inputPaths" = NULL,
    "reproducible.overwrite" = TRUE)
  )
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  # Add a study area to Crop and Mask to
  # Create a "study area"
  coords <- structure(c(-122.98, -116.1, -99.2, -106, -122.98,
                        59.9, 65.73, 63.58, 54.79, 59.9),
                      .Dim = c(5L, 2L))
  Sr1 <- Polygon(coords)
  Srs1 <- Polygons(list(Sr1), "s1")
  StudyArea <- SpatialPolygons(list(Srs1), 1L)
  crs(StudyArea) <- "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

  dPath <- file.path(tmpdir, "ecozones")

  #######################################
  ### url  ######
  #######################################
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

  #######################################
  ### url, targetFile, alsoExtract ######
  #######################################
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
  expect_equivalent(shpEcozone1, shpEcozone2) # different attribute newCache


  #######################################
  ### url, targetFile, alsoExtract -- with Cache ######
  #######################################
  # specify targetFile, alsoExtract, and fun, wrap with Cache
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

  unlink(dirname(ecozoneFilename), recursive = TRUE)
  # Test useCache = FALSE -- doesn't error and has no "loading from cache" or "loading from memoised"
  mess <- capture_messages(shpEcozoneSm <- Cache(
    prepInputs,
    url = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip",
    targetFile = reproducible::asPath(ecozoneFilename),
    alsoExtract = reproducible::asPath(ecozoneFiles),
    studyArea = StudyArea,
    fun = "shapefile",
    destinationPath = dPath,
    filename2 = "EcozoneFile.shp",
    useCache = FALSE
  ))
  expect_false(all(grepl("loading", mess)))

  # Test useCache -- doesn't error and loads from cache
  mess <- capture_messages(shpEcozoneSm <- Cache(
    prepInputs,
    url = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip",
    targetFile = reproducible::asPath(ecozoneFilename),
    alsoExtract = reproducible::asPath(ecozoneFiles),
    studyArea = StudyArea,
    fun = "shapefile",
    destinationPath = dPath,
    filename2 = "EcozoneFile.shp",
    useCache = TRUE
  ))
  expect_true(any(grepl("loading", mess)))

  # Big Raster, with crop and mask to Study Area - no reprojecting (lossy) of raster,
  #   but the StudyArea does get reprojected, need to use rasterToMatch
  lcc2005Filename <- file.path(dPath, "LCC2005_V1_4a.tif")
  url <- file.path("ftp://ftp.ccrs.nrcan.gc.ca/ad/NLCCLandCover",
                   "LandcoverCanada2005_250m/LandCoverOfCanada2005_V1_4.zip")

  #######################################
  ### url                          ######
  #######################################
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

  #######################################
  ### url, targetFile, archive     ######
  #######################################
  # if wrapped with Cache, will be fast second time, very fast 3rd time (via memoised copy)
  LCC2005_2 <- Cache(
    prepInputs,
    url = url,
    targetFile = lcc2005Filename,
    archive = asPath("LandCoverOfCanada2005_V1_4.zip"),
    destinationPath = asPath(dPath),
    studyArea = StudyArea
  )

  # Test the no allow overwrite if two functions (here postProcess and prepInputs)
  #  return same file-backed raster
  reproducible::clearCache(userTags = "prepInputs", ask = FALSE)
  # previously, this would cause an error because prepInputs file is gone b/c of previous
  #  line, but postProcess is still in a Cache recovery situation, to same file, which is
  #  not there. Now should be no error.
  mess <- capture_messages(LCC2005_2 <- Cache(
    prepInputs,
    url = url,
    targetFile = lcc2005Filename,
    archive = asPath("LandCoverOfCanada2005_V1_4.zip"),
    destinationPath = asPath(dPath),
    studyArea = StudyArea
  ))
  expect_true(isTRUE(any(grepl(pattern = "Loading", mess))))

  ##
  expect_is(LCC2005_2, "Raster")
  expect_equivalent(LCC2005, LCC2005_2)

  ######################################
  ##  archive     ######
  ######################################
  # don't pass url -- use local copy of archive only - use purge = TRUE to rm checksums file, rewrite it here
  shpEcozone <- prepInputs(destinationPath = dPath,
                           archive = file.path(dPath, "ecozone_shp.zip"), purge = TRUE)
  expect_true(is(shpEcozone, "SpatialPolygons"))

  #######################################
  ### archive, alsoExtract char    ######
  #######################################
  shpEcozone <- prepInputs(destinationPath = dPath,
                           archive = file.path(dPath, "ecozone_shp.zip"),
                           alsoExtract = c("ecozones.dbf", "ecozones.prj",
                                           "ecozones.sbn", "ecozones.sbx", "ecozones.shp", "ecozones.shx"))
  expect_true(is(shpEcozone, "SpatialPolygons"))

  rm(shpEcozone)
  expect_false(exists("shpEcozone", inherits = FALSE))

  #######################################
  ### url, alsoExtract, archive    ######
  #######################################
  # try again with url - should *not* download, even though checksums came from the
  #   prepInputs that had locally generated -- confirming that checksums with a manually copied file will work
  #   instead of forcing prepInputs to get the file.
  shpEcozone <- prepInputs(destinationPath = dPath,
                           url = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip",
                           archive = file.path(dPath, "ecozone_shp.zip"),
                           alsoExtract = c("ecozones.dbf", "ecozones.prj",
                                           "ecozones.sbn", "ecozones.sbx", "ecozones.shp", "ecozones.shx"))
  expect_true(is(shpEcozone, "SpatialPolygons"))

  lcc2005Filename <- file.path(dPath, "LCC2005_V1_4a.tif")
  url <- file.path("ftp://ftp.ccrs.nrcan.gc.ca/ad/NLCCLandCover",
                   "LandcoverCanada2005_250m/LandCoverOfCanada2005_V1_4.zip")

  #######################################
  ### archive                     ######
  #######################################
  # only archive -- i.e., skip download, but do extract and postProcess
  rm(LCC2005)
  mess <- capture_messages(LCC2005 <- prepInputs(
    archive = "LandCoverOfCanada2005_V1_4.zip",
    destinationPath = asPath(dPath),
    studyArea = StudyArea,
    purge = TRUE
  ))
  expect_true(any(grepl("From:LandCoverOfCanada2005_V1_4.zip", mess)))
  expect_true(is(LCC2005, "Raster"))

  #######################################
  ### archive                      ######
  #######################################
  rm(LCC2005)
  mess <- capture_messages(LCC2005 <- prepInputs(
    archive = "LandCoverOfCanada2005_V1_4.zip",
    destinationPath = asPath(dPath),
    studyArea = StudyArea
  ))
  expect_true(any(grepl("No targetFile supplied. Extracting all files from archive", mess)))
  expect_true(is(LCC2005, "Raster"))

  #######################################
  ### targetFile                   ######
  #######################################
  # only targetFile -- i.e., skip download, extract ... but do postProcess
  rm(LCC2005)
  mess <- capture_messages(LCC2005 <- prepInputs(
    targetFile = lcc2005Filename,
    destinationPath = asPath(dPath),
    studyArea = StudyArea,
    purge = TRUE
  ))
  expect_false(any(grepl("extract", mess))) # nothing that talks about extracting ...
  #which means no extractFromArchive or even skipping extract

  expect_true(is(LCC2005, "Raster"))
  StudyAreaCRSLCC2005 <- spTransform(StudyArea, crs(LCC2005))
  # crop and mask worked:
  expect_identical(extent(LCC2005)[1:4],
                   round(extent(StudyAreaCRSLCC2005)[1:4] / 250, 0) * 250)

})

test_that("interactive prepInputs", {
  testInitOut <- testInit("raster", opts = list("reproducible.overwrite" = TRUE,
                                                "reproducible.inputPaths" = NULL),
                          needGoogle = TRUE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  if (isInteractive()) {
    #######################################
    ### url                          ######
    #######################################
    #tmpdir <- "data/FMA"
    #checkPath(tmpdir, create = TRUE)
    warns <- capture_warnings(test <- prepInputs(
      url = "https://drive.google.com/file/d/1nTFOcrdMf1hIsxd_yNCSTr8RrYNHHwuc/view?usp=sharing",
      destinationPath = tmpdir
    ))
    files <- dir(tmpdir, pattern = "FMA_Boundary")
    expect_true(length(files) == 9)
    expect_is(test, "SpatialPolygons")

    #######################################
    ### url, targetFile              ######
    #######################################
    # need authentication for this
    #tmpdir <- "data/FMA"
    #checkPath(tmpdir, create = TRUE)
    warns <- capture_warnings(test <- prepInputs(
      targetFile = "FMA_Boundary_Updated.shp",
      url = "https://drive.google.com/file/d/1nTFOcrdMf1hIsxd_yNCSTr8RrYNHHwuc/view?usp=sharing",
      destinationPath = tmpdir
    ))
    # There is a meaningless warning for this unit test -- ignore it :
    # In rgdal::readOGR(dirname(x), fn, stringsAsFactors = stringsAsFactors,  :
    #                  Z-dimension discarded
    expect_is(test, "SpatialPolygons")

    # From Bird/Tati project
    testOnExit(testInitOut)
    testInitOut <- testInit("raster", opts = list("reproducible.overwrite" = TRUE,
                                                  "reproducible.inputPaths" = NULL),
                            needGoogle = TRUE)
    birdSpecies <- c("BBWA", "YRWA")
    urls <- c("https://drive.google.com/open?id=1CmzYNpxwWr82PoRSbHWG8yg2cC3hncfb",
              "https://drive.google.com/open?id=11Hxk0CcwJsoAnUgfrwbJhXBJNM5Xbd9e")

    #######################################
    ### url, targetFile, archive     ######
    #######################################
    outsideModule <- Map(x = birdSpecies, url = urls,
                         MoreArgs = list(tmpdir = tmpdir),
                         function(x, url, tmpdir) {
                           ras <- prepInputs(
                             targetFile = paste0(x, "_currmean.asc"),
                             archive = paste0(x, "_current.zip"),
                             fun = "raster::raster",
                             url = url,
                             destinationPath = tmpdir,
                             overwrite = TRUE
                           )
                         })
    expect_is(outsideModule[[1]], "Raster")
    expect_is(outsideModule[[2]], "Raster")
    expect_is(crs(outsideModule[[2]]), "CRS")
    expect_is(crs(outsideModule[[1]]), "CRS")
    expect_false(identical(outsideModule[[1]], outsideModule[[2]]))

    # remove the .prj files -- test "similar"
    #######################################
    ### url, targetFile, archive, alsoExtract similar ######
    #######################################
    file.remove(grep(pattern = "asc|zip|CHECK",
                     invert = TRUE, value = TRUE,
                     dir(tmpdir, full.names = TRUE)[!R.utils::isDirectory(dir(tmpdir))]))

    outsideModule <- Map(x = birdSpecies, url = urls,
                         MoreArgs = list(tmpdir = tmpdir),
                         function(x, url, tmpdir, purge) {
                           ras <- prepInputs(
                             targetFile = paste0(x, "_currmean.asc"),
                             archive = paste0(x, "_current.zip"),
                             url = url,
                             fun = "raster::raster",
                             alsoExtract = "similar",
                             destinationPath = tmpdir,
                             overwrite = TRUE
                           )
                         })
    expect_is(outsideModule[[1]], "Raster")
    expect_is(outsideModule[[2]], "Raster")
    expect_is(crs(outsideModule[[2]]), "CRS")
    expect_is(crs(outsideModule[[1]]), "CRS")
    expect_true(!is.na(crs(outsideModule[[1]])))
    expect_false(identical(outsideModule[[1]], outsideModule[[2]]))

    # remove the .prj files -- test "similar"
    file.remove(grep(pattern = "asc|zip|CHECK",
                     invert = TRUE, value = TRUE,
                     dir(tmpdir, full.names = TRUE)[!R.utils::isDirectory(dir(tmpdir))]))

    #######################################
    ### url, targetFile, archive, alsoExtract NA ######
    #######################################
    # because alsoExtract is NA ... no other files are unzipped, so no .prj and so no CRS
    outsideModule <- Map(x = birdSpecies, url = urls,
                         MoreArgs = list(tmpdir = tmpdir),
                         function(x, url, tmpdir, purge) {
                           ras <- prepInputs(
                             targetFile = paste0(x, "_currmean.asc"),
                             archive = paste0(x, "_current.zip"),
                             url = url,
                             fun = "raster::raster",
                             alsoExtract = NA,
                             destinationPath = tmpdir,
                             overwrite = TRUE
                           )
                         })
    expect_is(outsideModule[[1]], "Raster")
    expect_is(outsideModule[[2]], "Raster")
    expect_is(crs(outsideModule[[1]]), "CRS")
    expect_true(is.na(crs(outsideModule[[1]])))
    expect_false(identical(outsideModule[[1]], outsideModule[[2]]))
  }
})

test_that("preProcess doesn't work", {
  testInitOut <- testInit("raster", opts = list("reproducible.overwrite" = TRUE,
                                                "reproducible.inputPaths" = NULL),
                          needGoogle = TRUE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  testthat::skip_on_cran()
  testthat::skip_on_travis()
  testthat::skip_on_appveyor()

  testthat::skip_if_not(isInteractive())

  # Note urlShapefiles1Zip, urlShapefilesZip, and urlTif1 are in helper-allEqual.R

  ###############################################################
  ##### url                                                 #####
  ###############################################################
  mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    url = urlTif1,
    destinationPath = tmpdir
  )))
  runTest("1_2_5_6_7_10_13", "Raster", 1, mess, expectedMess = expectedMessage, filePattern = "DEM", tmpdir = tmpdir, test = test)

  # 2nd time # no targetFile, but since url is simple, can guess correctly
  mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    url = urlTif1,
    destinationPath = tmpdir
  )))
  runTest("1_2_5_6_8_10", "Raster", 1, mess, expectedMess = expectedMessage, filePattern = "DEM", tmpdir = tmpdir, test = test)
  unlink(dir(tmpdir, full.names = TRUE))

  # url is an archive on googledrive -- can get file.info from remote -- so can do checksums
  mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    url = urlShapefiles1Zip,
    destinationPath = tmpdir
  )))

  runTest("1_2_3_4_5_6_7_10_12_13", "SpatialPolygons", 5, mess, expectedMess = expectedMessage, filePattern = "Shapefile", tmpdir = tmpdir, test = test)

  # 2nd time # can checksums
  mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    url = urlShapefiles1Zip,
    destinationPath = tmpdir
  )))
  runTest("1_2_5_6_8_9_10_12", "SpatialPolygons", 5, mess, expectedMess = expectedMessage, filePattern = "Shapefile", tmpdir = tmpdir, test = test)
  unlink(dir(tmpdir, full.names = TRUE))

  ################################################################
  ###### url, targetFile                                     #####
  ################################################################
  mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    url = urlTif1,
    targetFile = basename(urlTif1),
    destinationPath = tmpdir
  )))
  runTest("1_2_5_6_7_13", "Raster", 1, mess, expectedMess = expectedMessage, filePattern = "DEM", tmpdir = tmpdir, test = test)

  # 2nd time # can checksums
  mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    url = urlTif1,
    targetFile = basename(urlTif1),
    destinationPath = tmpdir
  )))
  runTest("1_2_5_6_8", "Raster", 1, mess, expectedMess = expectedMessage, filePattern = "DEM", tmpdir = tmpdir, test = test)
  unlink(dir(tmpdir, full.names = TRUE))

  # url is an archive on googledrive --
  mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    url = urlShapefiles1Zip,
    targetFile = "Shapefile1.shp",
    destinationPath = tmpdir
  )))
  runTest("1_2_3_4_5_6_7_13", "SpatialPolygons", 5, mess, expectedMess = expectedMessage, filePattern = "Shapefile", tmpdir = tmpdir, test = test)

  # 2nd time # can checksums
  mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    url = urlShapefiles1Zip,
    targetFile = "Shapefile1.shp",
    destinationPath = tmpdir
  )))
  runTest("1_2_5_6_8_9", "SpatialPolygons", 5, mess, expectedMess = expectedMessage, filePattern = "Shapefile", tmpdir = tmpdir, test = test)
  unlink(dir(tmpdir, full.names = TRUE))


  ################################################################
  ###### url, alsoExtract                                    #####
  ################################################################
  mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    url = urlTif1,
    alsoExtract = "DEM.tif",
    destinationPath = tmpdir
  )))
  runTest("1_2_5_6_7_10_13", "Raster", 1, mess, expectedMess = expectedMessage, filePattern = "DEM", tmpdir = tmpdir, test = test)

  # 2nd time # can use checksums, even though don't have targetFile, b/c simple url
  mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    url = urlTif1,
    alsoExtract = "DEM.tif",
    destinationPath = tmpdir
  )))
  runTest("1_2_5_6_8_10", "Raster", 1, mess, expectedMess = expectedMessage, filePattern = "DEM", tmpdir = tmpdir, test = test)
  unlink(dir(tmpdir, full.names = TRUE))

  # url is an archive on googledrive --
  mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    url = urlShapefiles1Zip,
    alsoExtract = c("Shapefile1.dbf", "Shapefile1.prj", "Shapefile1.shp", "Shapefile1.shx"),
    destinationPath = tmpdir
  )))
  runTest("1_2_3_4_5_6_7_10_13", "SpatialPolygons", 5, mess, expectedMess = expectedMessage, filePattern = "Shapefile", tmpdir = tmpdir, test = test)

  # 2nd time # can't checksums because no targetfile
  mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    url = urlShapefiles1Zip,
    alsoExtract = c("Shapefile1.dbf", "Shapefile1.prj", "Shapefile1.shp", "Shapefile1.shx"),
    destinationPath = tmpdir
  )))
  runTest("1_2_5_6_8_9_10", "SpatialPolygons", 5, mess, expectedMess = expectedMessage, filePattern = "Shapefile", tmpdir = tmpdir, test = test)
  unlink(dir(tmpdir, full.names = TRUE))

  ################################################################
  ###### url, archive                                        #####
  ################################################################
  # url is an archive on googledrive -- here, zip has 2 Shapefile filesets -- Shapefile1* and Shapefile2*
  #   should extract all
  mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    url = urlShapefilesZip,
    archive = "Shapefiles1.zip",
    destinationPath = tmpdir
  )))
  runTest("1_2_3_4_5_6_7_10_12_13", "SpatialPolygons", 9, mess, expectedMess = expectedMessage, filePattern = "Shapefile", tmpdir = tmpdir, test = test)

  # 2nd time # can checksums
  mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    url = urlShapefilesZip,
    archive = "Shapefiles1.zip",
    destinationPath = tmpdir
  )))
  runTest("1_2_5_6_8_9_10_12", "SpatialPolygons", 9, mess, expectedMess = expectedMessage, filePattern = "Shapefile", tmpdir = tmpdir, test = test)
  unlink(dir(tmpdir, full.names = TRUE))

  ################################################################
  ###### url, archive, targetFile                            #####
  ################################################################
  # url is an archive on googledrive --
  mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    url = urlShapefiles1Zip,
    archive = "Shapefiles1.zip",
    targetFile = "Shapefile1.shp",
    destinationPath = tmpdir
  )))
  runTest("1_2_3_4_5_6_7_13", "SpatialPolygons", 5, mess, expectedMess = expectedMessage, filePattern = "Shapefile", tmpdir = tmpdir, test = test)

  # 2nd time # can checksums
  mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    url = urlShapefiles1Zip,
    archive = "Shapefiles1.zip",
    targetFile = "Shapefile1.shp",
    destinationPath = tmpdir
  )))
  runTest("1_2_5_6_8_9", "SpatialPolygons", 5, mess, expectedMess = expectedMessage, filePattern = "Shapefile", tmpdir = tmpdir, test = test)
  unlink(dir(tmpdir, full.names = TRUE))

  ################################################################
  ###### url, targetFile, alsoExtract                        #####
  ################################################################
  # url is an archive on googledrive --
  mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    url = urlShapefilesZip,
    targetFile = "Shapefile1.shp",
    alsoExtract = c("Shapefile1.dbf", "Shapefile1.prj", "Shapefile1.shx"),
    destinationPath = tmpdir
  )))
  runTest("1_2_3_4_5_6_7_13", "SpatialPolygons", 5, mess, expectedMess = expectedMessage, filePattern = "Shapefile", tmpdir = tmpdir, test = test)

  # 2nd time # can checksums
  mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    url = urlShapefilesZip,
    targetFile = "Shapefile1.shp",
    alsoExtract = c("Shapefile1.dbf", "Shapefile1.prj", "Shapefile1.shx"),
    destinationPath = tmpdir
  )))
  runTest("1_2_5_6_8", "SpatialPolygons", 5, mess, expectedMess = expectedMessage, filePattern = "Shapefile", tmpdir = tmpdir, test = test)
  unlink(dir(tmpdir, full.names = TRUE))


  mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    url = urlShapefilesZip,
    targetFile = "Shapefile1.shp",
    alsoExtract = c("similar"),
    destinationPath = tmpdir
  )))
  runTest("1_2_3_4_5_6_7_13", "SpatialPolygons", 5, mess, expectedMess = expectedMessage, filePattern = "Shapefile", tmpdir = tmpdir, test = test)
  mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    url = urlTif1,
    targetFile = "DEM.tif",
    alsoExtract = c("DEM.tif"),
    destinationPath = tmpdir
  )))
  runTest("1_2_5_6_7_13", "Raster", 1, mess, expectedMess = expectedMessage, filePattern = "DEM", tmpdir = tmpdir, test = test)
  unlink(dir(tmpdir, full.names = TRUE))

  ################################################################
  ##### url, archive, alsoExtract               #####
  ###############################################################
  # url is an archive on googledrive --
  mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    url = urlShapefilesZip,
    archive = "Shapefiles1.zip",
    alsoExtract = "similar",
    destinationPath = tmpdir
  )))
  runTest("1_2_3_4_5_6_7_10_12_13", "SpatialPolygons", 9, mess, expectedMess = expectedMessage, filePattern = "Shapefile", tmpdir = tmpdir, test = test)

  # 2nd time # can checksums
  mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    url = urlShapefilesZip,
    archive = "Shapefiles1.zip",
    alsoExtract = "similar",
    destinationPath = tmpdir
  )))
  runTest("1_2_5_6_8_9_10_12", "SpatialPolygons", 9, mess, expectedMess = expectedMessage, filePattern = "Shapefile", tmpdir = tmpdir, test = test)


  unlink(dir(tmpdir, full.names = TRUE))
  expect_error(mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    url = urlShapefilesZip,
    archive = "Shapefiles1.zip",
    alsoExtract = c("Shapefile1.dbf", "Shapefile1.prj", "Shapefile1.shx"),
    destinationPath = tmpdir
  ))))

  unlink(dir(tmpdir, full.names = TRUE))

  # ################################################################
  ###### url, targetFile, alsoExtract               #####
  ################################################################
  # url is an archive on googledrive --
  mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    url = urlShapefilesZip,
    alsoExtract = "similar",
    targetFile = "Shapefile1.shp",
    destinationPath = tmpdir
  )))
  runTest("1_2_3_4_5_6_7_13", "SpatialPolygons", 5, mess, expectedMess = expectedMessage,
          filePattern = "Shapefile", tmpdir = tmpdir, test = test)
  mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    url = urlShapefilesZip,
    alsoExtract = "similar",
    targetFile = "Shapefile1.shp",
    destinationPath = tmpdir
  )))
  runTest("1_2_5_6_8_9", "SpatialPolygons", 5, mess, expectedMess = expectedMessage, filePattern = "Shapefile", tmpdir = tmpdir, test = test)
  unlink(dir(tmpdir, full.names = TRUE))

  # 2nd time # can checksums
  mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    url = urlShapefilesZip,
    alsoExtract = c("Shapefile1.dbf", "Shapefile1.prj", "Shapefile1.shx"),
    targetFile = "Shapefile1.shp",
    destinationPath = tmpdir
  )))
  runTest("1_2_3_4_5_6_7_13", "SpatialPolygons", 5, mess, expectedMess = expectedMessage, filePattern = "Shapefile", tmpdir = tmpdir, test = test)

  # 2nd time # can checksums
  mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    url = urlShapefilesZip,
    alsoExtract = c("Shapefile1.dbf", "Shapefile1.prj", "Shapefile1.shx"),
    targetFile = "Shapefile1.shp",
    destinationPath = tmpdir
  )))
  runTest("1_2_5_6_8", "SpatialPolygons", 5, mess, expectedMess = expectedMessage, filePattern = "Shapefile", tmpdir = tmpdir, test = test)
  unlink(dir(tmpdir, full.names = TRUE))

  ################################################################
  ###### url, archive, targetFile, alsoExtract               #####
  ################################################################
  # url is an archive on googledrive --
  mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    url = urlShapefilesZip,
    archive = "Shapefiles1.zip",
    alsoExtract = "similar",
    targetFile = "Shapefile1.shp",
    destinationPath = tmpdir
  )))
  runTest("1_2_3_4_5_6_7_13", "SpatialPolygons", 5, mess, expectedMess = expectedMessage, filePattern = "Shapefile", tmpdir = tmpdir, test = test)

  # 2nd time # can checksums
  mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    url = urlShapefilesZip,
    archive = "Shapefiles1.zip",
    alsoExtract = "similar",
    targetFile = "Shapefile1.shp",
    destinationPath = tmpdir
  )))
  runTest("1_2_5_6_8_9", "SpatialPolygons", 5, mess, expectedMess = expectedMessage, filePattern = "Shapefile", tmpdir = tmpdir, test = test)

  ################################################################
  ###### archive                                             #####
  ################################################################
  # archive exists locally
  # remove all non archive files
  file.remove(grep(dir(tmpdir, full.names = TRUE)[!R.utils::isDirectory(dir(tmpdir))], pattern = "\\.zip", invert = TRUE, value = TRUE))
  mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    archive = "Shapefiles1.zip",
    destinationPath = tmpdir
  )))
  runTest("1_2_4_5_6_9_10_12_13", "SpatialPolygons", 9, mess, expectedMess = expectedMessage, filePattern = "Shapefile", tmpdir = tmpdir, test = test)

  # 2nd time # can checksums
  mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    archive = "Shapefiles1.zip",
    destinationPath = tmpdir
  )))
  runTest("1_2_5_6_9_10_12", "SpatialPolygons", 9, mess, expectedMess = expectedMessage, filePattern = "Shapefile", tmpdir = tmpdir, test = test)

  ################################################################
  ###### archive, targetFile                                 #####
  ################################################################
  # archive exists locally
  # remove all non archive files
  file.remove(grep(dir(tmpdir, full.names = TRUE)[!R.utils::isDirectory(dir(tmpdir))], pattern = "\\.zip", invert = TRUE, value = TRUE))
  mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    archive = "Shapefiles1.zip",
    targetFile = "Shapefile1.shp",
    destinationPath = tmpdir
  )))
  runTest("1_2_4_5_6_9_13", "SpatialPolygons", 9, mess, expectedMess = expectedMessage, filePattern = "Shapefile", tmpdir = tmpdir, test = test)

  # 2nd time # can checksums
  mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    archive = "Shapefiles1.zip",
    targetFile = "Shapefile1.shp",
    destinationPath = tmpdir
  )))
  runTest("1_2_5_6_9", "SpatialPolygons", 9, mess, expectedMess = expectedMessage, filePattern = "Shapefile", tmpdir = tmpdir, test = test)

  ################################################################
  ###### archive, targetFile, alsoExtract                    #####
  ################################################################
  # archive exists locally
  # remove all non archive files
  file.remove(grep(dir(tmpdir, full.names = TRUE)[!R.utils::isDirectory(dir(tmpdir))],
                   pattern = "\\.zip", invert = TRUE, value = TRUE))
  mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    archive = "Shapefiles1.zip",
    targetFile = "Shapefile1.shp",
    alsoExtract = c("Shapefile1.dbf", "Shapefile1.prj", "Shapefile1.shp", "Shapefile1.shx"),
    destinationPath = tmpdir
  )))
  runTest("1_2_4_5_6_9_13", "SpatialPolygons", 5, mess, expectedMess = expectedMessage, filePattern = "Shapefile", tmpdir = tmpdir, test = test)

  # 2nd time # can checksums
  mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    archive = "Shapefiles1.zip",
    targetFile = "Shapefile1.shp",
    alsoExtract = c("Shapefile1.dbf", "Shapefile1.prj", "Shapefile1.shp", "Shapefile1.shx"),
    destinationPath = tmpdir
  )))
  runTest("1_2_5_6_9", "SpatialPolygons", 5, mess, expectedMess = expectedMessage, filePattern = "Shapefile", tmpdir = tmpdir, test = test)

  file.remove(grep(dir(tmpdir, full.names = TRUE)[!R.utils::isDirectory(dir(tmpdir))],
                   pattern = "\\.zip", invert = TRUE, value = TRUE))
  file.remove(grep(dir(tmpdir, full.names = TRUE)[!R.utils::isDirectory(dir(tmpdir))],
                   pattern = "CHECKSUMS.txt", value = TRUE))
  mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    archive = "Shapefiles1.zip",
    targetFile = "Shapefile1.shp",
    alsoExtract = "similar",
    destinationPath = tmpdir
  )))
  runTest("1_2_4_5_6_9_13", "SpatialPolygons", 5, mess, expectedMess = expectedMessage, filePattern = "Shapefile", tmpdir = tmpdir, test = test)

  # 2nd time # can checksums
  mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    archive = "Shapefiles1.zip",
    targetFile = "Shapefile1.shp",
    alsoExtract = c("similar"),
    destinationPath = tmpdir
  )))
  runTest("1_2_5_6_9", "SpatialPolygons", 5, mess, expectedMess = expectedMessage, filePattern = "Shapefile", tmpdir = tmpdir, test = test)

  ################################################################
  ###### targetFile                                          #####
  ################################################################
  file.remove(grep(dir(tmpdir, full.names = TRUE), pattern = "CHECKSUMS.txt", value = TRUE))
  mess <- capture_messages(warns <- capture_warnings(
    test <- prepInputs(
      targetFile = "Shapefile1.shp",
      destinationPath = tmpdir
    )
  ))
  runTest("1_2_5_6", "SpatialPolygons", 5, mess, expectedMess = expectedMessage, filePattern = "Shapefile", tmpdir = tmpdir, test = test)
  mess <- capture_messages(warns <- capture_warnings(
    test <- prepInputs(
      targetFile = "Shapefile1.shp",
      destinationPath = tmpdir
    )
  ))
  runTest("1_2_5_6", "SpatialPolygons", 5, mess, expectedMess = expectedMessage, filePattern = "Shapefile", tmpdir = tmpdir, test = test)

  ################################################################
  ###### targetFile, alsoExtract                             #####
  ################################################################
  file.remove(grep(dir(tmpdir, full.names = TRUE)[!R.utils::isDirectory(dir(tmpdir))],
                   pattern = "CHECKSUMS.txt", value = TRUE))
  mess <- capture_messages(warns <- capture_warnings(
    test <- prepInputs(
      targetFile = "Shapefile1.shp",
      alsoExtract = c("Shapefile1.dbf", "Shapefile1.prj", "Shapefile1.shp", "Shapefile1.shx"),
      destinationPath = tmpdir
    )
  ))
  runTest("1_2_5_6", "SpatialPolygons", 5, mess, expectedMess = expectedMessage, filePattern = "Shapefile", tmpdir = tmpdir, test = test)
  mess <- capture_messages(warns <- capture_warnings(
    test <- prepInputs(
      targetFile = "Shapefile1.shp",
      alsoExtract = c("Shapefile1.dbf", "Shapefile1.prj", "Shapefile1.shp", "Shapefile1.shx"),
      destinationPath = tmpdir
    )
  ))
  runTest("1_2_5_6", "SpatialPolygons", 5, mess, expectedMess = expectedMessage, filePattern = "Shapefile", tmpdir = tmpdir, test = test)

  ################################################################
  ###### alsoExtract -- will fail b/c no information         #####
  ###############################################################
  file.remove(grep(dir(tmpdir, full.names = TRUE)[!R.utils::isDirectory(dir(tmpdir))],
                   pattern = "CHECKSUMS.txt", value = TRUE))
  expect_error(mess <- capture_messages(warns <- capture_warnings(
    test <- prepInputs(
      alsoExtract = c("Shapefile1.dbf", "Shapefile1.prj", "Shapefile1.shp", "Shapefile1.shx"),
      destinationPath = tmpdir
    )
  )))

  ################################################################
  ###### archive, alsoExtract                                #####
  ################################################################
  # archive exists locally
  # remove all non archive files
  file.remove(grep(dir(tmpdir, full.names = TRUE)[!R.utils::isDirectory(dir(tmpdir))],
                   pattern = "\\.zip", invert = TRUE, value = TRUE))
  file.remove(grep(dir(tmpdir, full.names = TRUE)[!R.utils::isDirectory(dir(tmpdir))],
                   pattern = "CHECKSUMS.txt", value = TRUE))
  mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    archive = "Shapefiles1.zip",
    alsoExtract = c("Shapefile1.dbf", "Shapefile1.prj", "Shapefile1.shp", "Shapefile1.shx"),
    destinationPath = tmpdir
  )))
  runTest("1_2_4_5_6_9_10_13", "SpatialPolygons", 5, mess, expectedMess = expectedMessage, filePattern = "Shapefile", tmpdir = tmpdir, test = test)

  # 2nd time # can checksums
  mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    archive = "Shapefiles1.zip",
    alsoExtract = c("Shapefile1.dbf", "Shapefile1.prj", "Shapefile1.shp", "Shapefile1.shx"),
    destinationPath = tmpdir
  )))
  runTest("1_2_5_6_9_10", "SpatialPolygons", 5, mess, expectedMess = expectedMessage, filePattern = "Shapefile", tmpdir = tmpdir, test = test)

  # Try without .shp -- fail
  file.remove(grep(dir(tmpdir, full.names = TRUE)[!R.utils::isDirectory(dir(tmpdir))],
                   pattern = "\\.zip", invert = TRUE, value = TRUE))
  expect_error(mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    archive = "Shapefiles1.zip",
    alsoExtract = c("Shapefile1.dbf", "Shapefile1.prj", "Shapefile1.shx"),
    destinationPath = tmpdir
  ))))

  file.remove(grep(dir(tmpdir, full.names = TRUE)[!R.utils::isDirectory(dir(tmpdir))],
                   pattern = "\\.zip", invert = TRUE, value = TRUE))
  file.remove(grep(dir(tmpdir, full.names = TRUE)[!R.utils::isDirectory(dir(tmpdir))],
                   pattern = "CHECKSUMS.txt", value = TRUE))
  mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    archive = "Shapefiles1.zip",
    targetFile = "Shapefile1.shp",
    alsoExtract = "similar",
    destinationPath = tmpdir
  )))
  runTest("1_2_4_5_6_9_13", "SpatialPolygons", 5, mess, expectedMess = expectedMessage, filePattern = "Shapefile", tmpdir = tmpdir, test = test)

  # 2nd time # can checksums
  mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    archive = "Shapefiles1.zip",
    targetFile = "Shapefile1.shp",
    alsoExtract = c("similar"),
    destinationPath = tmpdir
  )))
  runTest("1_2_5_6_9", "SpatialPolygons", 5, mess, expectedMess = expectedMessage, filePattern = "Shapefile", tmpdir = tmpdir, test = test)

})

test_that("prepInputs doesn't work", {
  skip_on_cran()

  if (getRversion() > "3.3.0") {
    testInitOut <- testInit("raster", opts = list("reproducible.overwrite" = TRUE,
                                                  "reproducible.inputPaths" = NULL),
                            needGoogle = TRUE)
    on.exit({
      testOnExit(testInitOut)
    }, add = TRUE)
    mess1 <- capture_messages({
      test1 <- prepInputs(#targetFile = "GADM_2.8_LUX_adm0.rds", # looks like GADM has changed their API
                          targetFile = targetFileLuxRDS,
                          #destinationPath = ".",
                          dlFun = getDataFn, name = "GADM", country = "LUX", level = 0,
                          #dlFun = "raster::getData", name = "GADM", country = "LUX", level = 0,
                          path = tmpdir)
    })
    mess2 <- capture_messages({
      test2 <- prepInputs(targetFile = targetFileLuxRDS,
                          dlFun = getDataFn, name = "GADM", country = "LUX", level = 0,
                          path = tmpdir)
    })
    runTest("1_2_5_6_13", "SpatialPolygonsDataFrame", 1, mess1, expectedMess = expectedMessage,
            filePattern = targetFileLuxRDS, tmpdir = tmpdir, test = test1)
    runTest("1_2_5_6_8", "SpatialPolygonsDataFrame", 1, mess2, expectedMess = expectedMessage,
            filePattern = targetFileLuxRDS, tmpdir = tmpdir, test = test1)

    # Add a study area to Crop and Mask to
    # Create a "study area"
    coords <- structure(c(6, 6.1, 6.2, 6.15, 6, 49.5, 49.7, 49.8, 49.6, 49.5),
                        .Dim = c(5L, 2L))
    Sr1 <- Polygon(coords)
    Srs1 <- Polygons(list(Sr1), "s1")
    StudyArea <- SpatialPolygons(list(Srs1), 1L)
    crs(StudyArea) <- "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

    mess2 <- capture_messages(warn <- capture_warnings(test3 <- prepInputs(targetFile = targetFileLuxRDS,
                                                                           dlFun = getDataFn, name = "GADM", country = "LUX", level = 0,
                                                                           path = tmpdir, studyArea = StudyArea)))
    runTest("1_2_5_6_8", "SpatialPolygonsDataFrame", 1, mess2, expectedMess = expectedMessage,
            filePattern = targetFileLuxRDS, tmpdir = tmpdir,
            test = test3)

    testOnExit(testInitOut)
    testInitOut <- testInit("raster", opts = list("reproducible.inputPaths" = NULL,
                                                  "reproducible.overwrite" = TRUE),
                            needGoogle = TRUE)
    mess2 <- capture_messages(warn <- capture_warnings(test3 <- prepInputs(
      targetFile = targetFileLuxRDS,
      dlFun = getDataFn, name = "GADM", country = "LUX", level = 0,
      path = tmpdir, studyArea = StudyArea)))
    runTest("1_2_5_6_13", "SpatialPolygonsDataFrame", 1, mess2, expectedMess = expectedMessage,
            filePattern = targetFileLuxRDS, tmpdir = tmpdir,
            test = test3)

    runTest("1_2_3_4", "SpatialPolygonsDataFrame", 1, mess2,
            expectedMess = expectedMessagePostProcess,
            filePattern = targetFileLuxRDS, tmpdir = tmpdir, test = test3)

    testOnExit(testInitOut)
    testInitOut <- testInit("raster", opts = list("reproducible.overwrite" = TRUE,
                                                  "reproducible.inputPaths" = NULL),
                            needGoogle = TRUE)
    mess1 <- capture_messages(test <- prepInputs(
      targetFile = "DEM.tif",
      url = urlTif1,
      destinationPath = tmpdir,
      useCache = TRUE
    ))
    runTest("1_2_5_6_7_13", "Raster", 1, mess1, expectedMess = expectedMessage, filePattern = "DEM", tmpdir = tmpdir,
            test = test)

    if (interactive()){
      testOnExit(testInitOut)
      testInitOut <- testInit("raster", opts = list("reproducible.inputPaths" = NULL,
                                                    "reproducible.overwrite" = TRUE
                                                    ),
                              needGoogle = TRUE)
      opts <- options("reproducible.cachePath" = tmpCache)
      on.exit({options(opts)}, add = TRUE)
      googledrive::drive_auth_config(active = TRUE)
      mess2 <- capture_messages(warn <- capture_warnings(test3 <- prepInputs(
        url = "https://drive.google.com/file/d/1zkdGyqkssmx14B9wotOqlK7iQt3aOSHC/view?usp=sharing",
        studyArea = StudyArea,
        fun = "base::readRDS")))
      runTest("1_2_3_4", "SpatialPolygonsDataFrame", 1, mess2,
              expectedMess = expectedMessagePostProcess,
              filePattern = "GADM_2.8_LUX_adm0.rds$", tmpdir = tmpdir, test = test3)
    }

  }
})

test_that("load rdata in prepInputs", {
  testInitOut <- testInit("raster", opts = list("reproducible.overwrite" = TRUE,
                                                "reproducible.inputPaths" = NULL),
                          needGoogle = TRUE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  a <- 1
  b <- 2
  save(a, b, file = tmpfile)
  aa <- prepInputs(tmpfile, fun = "base::load")
  expect_true(identical(aa, list(a = a, b = b)))

  d <- new.env()
  aa <- prepInputs(tmpfile, fun = "base::load", envir = d)
  expect_false(identical(aa, list(a = a, b = b))) # not in aa, because loaded to d
  expect_true(identical(as.list(d), list(a = a, b = b)))
})

test_that("assessDataType doesn't work", {
  testInitOut <- testInit("raster", opts = list("reproducible.overwrite" = TRUE,
                                                "reproducible.inputPaths" = NULL),
                          needGoogle = TRUE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  ## LOG1S
  ras <- raster(ncol = 10, nrow = 10)
  ras[] <- c(0, NaN, rep(c(0,1),49))
  expect_true(assessDataType(ras) == "LOG1S")

  ras <- raster(ncol = 10, nrow = 10)
  ras[] <- rep(c(0,1),50)
  expect_true(assessDataType(ras) == "LOG1S")

  ras[] <- rep(c(TRUE,FALSE),50)
  expect_true(assessDataType(ras) == "LOG1S")

  ras[] <- c(NA, NA, rep(c(0,1),49))
  expect_true(assessDataType(ras) == "LOG1S")

  ## INT1S
  ras[] <- -1:98
  expect_true(assessDataType(ras) == "INT1S")

  ras[] <- c(NA, -1:97)
  expect_true(assessDataType(ras) == "INT1S")

  ## INT1U
  ras <- raster(ncol = 10, nrow = 10)
  ras[] <- 1:100
  expect_true(assessDataType(ras) == "INT1U")

  ras[] <- c(NA, 2:100)
  expect_true(assessDataType(ras) == "INT1U")

  ## INT2U
  ras <- raster(ncol = 10, nrow = 10)
  ras[] <- round(runif(100, min = 64000, max = 65000))
  expect_true(assessDataType(ras) == "INT2U")

  ## INT2S
  ras <- raster(ncol = 10, nrow = 10)
  ras[] <- round(runif(100, min = -32767, max = 32767))
  expect_true(assessDataType(ras) == "INT2S")

  ras[54] <- NA
  expect_true(assessDataType(ras) == "INT2S")

  ## INT4U
  ras <- raster(ncol = 10, nrow = 10)
  ras[] <- round(runif(100, min = 0, max = 500000000))
  expect_true(assessDataType(ras) == "INT4U")

  ras[14] <- NA
  expect_true(assessDataType(ras) == "INT4U")

  ## INT4S
  ras <- raster(ncol = 10, nrow = 10)
  ras[] <- round(runif(100, min = -200000000, max = 200000000))
  expect_true(assessDataType(ras) == "INT4S")

  ras[14] <- NA
  expect_true(assessDataType(ras) == "INT4S")

  ## FLT4S
  ras <- raster(ncol = 10, nrow = 10)
  ras[] <- runif(100, min = -10, max = 87)
  expect_true(assessDataType(ras) == "FLT4S")

  ras <- raster(ncol = 10, nrow = 10)
  ras[] <- round(runif(100, min = -3.4e+26, max = 3.4e+28))
  expect_true(assessDataType(ras) == "FLT4S")

  ras <- raster(ncol = 10, nrow = 10)
  ras[] <- round(runif(100, min = 3.4e+26, max = 3.4e+28))
  expect_true(assessDataType(ras) == "FLT4S")

  ras <- raster(ncol = 10, nrow = 10)
  ras[] <- round(runif(100, min = -3.4e+26, max = -1))
  expect_true(assessDataType(ras) == "FLT4S")

  ## FLT8S
  ras <- raster(ncol = 10, nrow = 10)
  ras[] <- round(runif(100, min = -1.7e+30, max = 1.7e+308))
  expect_true(assessDataType(ras) == "FLT8S")

  ras <- raster(ncol = 10, nrow = 10)
  ras[] <- round(runif(100, min = 1.7e+30, max = 1.7e+308))
  expect_true(assessDataType(ras) == "FLT8S")

  ras <- raster(ncol = 10, nrow = 10)
  ras[] <- round(runif(100, min = -1.7e+308, max = -1))
  expect_true(assessDataType(ras) == "FLT8S")

  ras <- raster(ncol = 10, nrow = 10)
  ras[] <- c(-Inf, 1, rep(c(0,1),49))
  expect_true(assessDataType(ras) == "FLT8S")


  ras <- raster(ncol = 10, nrow = 10)
  ras[] <- c(Inf, 1, rep(c(0,1),49))
  expect_true(assessDataType(ras) == "FLT8S")
})

test_that("assessDataType doesn't work for GDAL", {
  testInitOut <- testInit("raster", opts = list("reproducible.overwrite" = TRUE,
                                                "reproducible.inputPaths" = NULL),
                          needGoogle = TRUE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  ## Float32
  ras <- raster(ncol = 10, nrow = 10)
  ras[] <- runif(100, 0, 1)
  expect_true(assessDataType(ras, type = "GDAL") == "Float32")

  ## UInt16
  ras[] <- c(201:300)
  expect_true(assessDataType(ras, type = "GDAL") == "UInt16")

  ##Byte
  ras[] <- 1:100
  expect_true(assessDataType(ras, type = "GDAL") == "Byte")
})

test_that("lightweight tests for code coverage", {
  testthat::skip_on_cran()

  testInitOut <- testInit("raster", opts = list("reproducible.overwrite" = TRUE,
                                                "reproducible.inputPaths" = NULL),
                          needGoogle = TRUE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  url <- "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip"

  checkPath(tmpdir, create = TRUE)
  checkSums <- .emptyChecksumsResult
  checkSumFilePath <- file.path(tmpdir, "CHECKSUMS.txt")

  downloadFile(url = url, neededFiles = "ecozones.shp", checkSums = checkSums,
               archive = "ecozone_shp.zip", needChecksums = TRUE, quick = FALSE,
               destinationPath = tmpdir, checksumFile = checkSumFilePath)
  expect_true(file.exists(dir(tmpdir, pattern = "ecozone", full.names = TRUE)))


  # have local copy
  unzip("ecozone_shp.zip", exdir = tmpdir)
  file.copy(dir(file.path(tmpdir, "Ecozones"), full.names = TRUE), tmpdir)
  checkSums <- Checksums(path = tmpdir, write = TRUE)
  #dir(tmpdir)

  aMess <- capture_messages(downloadFile(url = url, neededFiles = "ecozones.shp", checkSums = checkSums,
                                         targetFile = "ecozones.shp",
                                         archive = NULL, needChecksums = TRUE, quick = FALSE,
                                         destinationPath = file.path(tmpdir, "Ecozones"),
                                         checksumFile = file.path(tmpdir, "CHECKSUMS.txt")))

  expect_true(any(grepl("Skipping download", aMess)))

  # Test when wrong archive exists, wrong checkSums
  #checkSums <- Checksums(path = tmpdir)
  file.remove(file.path(tmpdir, "ecozone_shp.zip"))
  file.remove(dir(file.path(tmpdir), pattern = "ecozones", full.names = TRUE))
  file.create(file.path(tmpdir, "ecozone_shp.zip"))
  checkSums <- Checksums(path = tmpdir, write = TRUE)
  file.remove(file.path(tmpdir, "ecozone_shp.zip"))
  checkSums <- Checksums(path = tmpdir)

  expect_error(downloadFile(url = url,
                            neededFiles = c("ecozones.dbf", "ecozones.prj", "ecozones.sbn", "ecozones.sbx",
                                            "ecozones.shp", "ecozones.shx"),
                            checkSums = checkSums,
                            targetFile = "ecozones.shp",
                            archive = "ecozone_shp.zip", needChecksums = TRUE, quick = FALSE,
                            destinationPath = tmpdir, checksumFile = checkSumFilePath))

  ## postProcess.default
  b <- 1
  a <- postProcess(b)
  expect_true(identical(a, b))

  ## postProcess.list
  b <- list(1,1)
  a <- postProcess(b)
  expect_true(identical(a, b))

  ras <- raster(extent(0,10,0,10), res = 1, vals = 1:100)
  crs(ras) <- "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

  expect_error(postProcess(ras, studyArea = 1), "The 'studyArea")
  expect_error(postProcess(ras, rasterToMatch = 1), "The 'rasterToMatch")
  mess <- capture_messages(postProcess(ras, inputFilePath = "test"))
  expect_true(all(grepl("inputFilePath is being deprecated", mess)))

  mess <- capture_messages(postProcess(ras, targetFilePath = "test"))
  expect_true(all(grepl("targetFilePath is being deprecated", mess)))

  ## cropInputs.default
  b <- 1
  a <- cropInputs(b)
  expect_true(identical(a, b))

  ras2 <- raster(extent(0,5,0,5), res = 1, vals = 1:25)
  crs(ras2) <- "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  a <- cropInputs(ras, extentToMatch = extent(ras2), extentCRS = crs(ras2))
  expect_is(a, "RasterLayer")

  ras4 <- raster(extent(6,10,6,10), res = 1, vals = 1:16)
  sp4 <- as(raster::extent(ras4), "SpatialPolygons")
  crs(sp4) <- "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

  expect_error(cropInputs(ras2, studyArea = sp4), "extents do not overlap")

  ras3 <- raster(extent(0,5,0,5), res = 1, vals = 1:25)
  crs(ras3) <- "+init=epsg:4326 +proj=longlat +datum=NAD83 +no_defs +ellps=WGS84 +towgs84=0,0,0"

  ################################################
  # Different crs
  # Because studyArea is a Raster, then it doesn't work correctly
  a <- cropInputs(ras2, studyArea = ras3)
  expect_is(a, "RasterLayer")
  expect_true(identical(crs(a), crs(ras2)))

  # Now rasterToMatch used -- internally reprojects it to x
  a <- cropInputs(ras2, rasterToMatch = ras3)
  expect_is(a, "RasterLayer")
  expect_true(identical(crs(a), crs(ras2)))

  ## fixErrors.default
  b <- 1
  a <- fixErrors(b)
  expect_true(identical(a, b))

  ## projectInputs.Raster
  a <- projectInputs(ras2, rasterToMatch = ras3, method = "ngb")
  expect_is(a, "RasterLayer")
  expect_true(identical(crs(a), crs(ras3)))

  a <- projectInputs(ras2, targetCRS = crs(ras3), rasterToMatch = ras3, method = "ngb")
  expect_is(a, "RasterLayer")
  expect_true(identical(crs(a), crs(ras3)))

  #warns if bilinear is passed for reprojecting integer
  expect_warning(projectInputs(ras2, rasterToMatch = ras3, method = "bilinear"))

  #Works with no rasterToMatch
  a <- projectInputs(ras2, targetCRS = crs(ras3), method = "ngb")
  expect_true(identical(crs(a), crs(ras3)))

  # sp::CRS("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"))
})

test_that("lightweight tests 2 for code coverage", {
  testthat::skip_on_cran()

  testInitOut <- testInit("raster", opts = list("reproducible.overwrite" = TRUE,
                                                "reproducible.inputPaths" = NULL),
                          needGoogle = TRUE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  theZipFile <- tempfile(fileext = ".zip")
  theZipFile2 <- tempfile(fileext = ".zip")
  theZipFile3 <- tempfile(fileext = ".zip")
  theZipName <- file.path(tmpdir, "hi.zip")
  theZapFile <- tempfile(fileext = ".zap")
  theRDSFile <- tempfile(fileext = ".rds")
  a <- 1
  saveRDS(a, file = theRDSFile)
  origWD <- setwd(dirname(theRDSFile))
  zip(zipfile = theZipFile, files = basename(theRDSFile))
  zip(zipfile = theZipFile2, files = basename(theZipFile))
  zip(zipfile = theZipFile3, files = basename(theZipFile2))
  setwd(origWD)
  expect_error(extractFromArchive(theZapFile), "Archives of type zap are not currently supported")

  expect_error(extractFromArchive(theZipName), "No archive exists with filename")

  extractFromArchive(theZipFile, neededFiles = character())

  csfp <- file.path(tmpdir, "CHECKSUMS.txt")
  data.table::fwrite(.emptyChecksumsFileContent, file = csfp, sep = "\t")

  # check Checksums fn
  a <- extractFromArchive(theZipFile, neededFiles = character(), checkSumFilePath = csfp,
                          destinationPath = tmpdir)
  expect_true(file.exists(a$filesExtracted))
  # check Checksums fn

  expect_error(suppressWarnings(extractFromArchive(theZipFile,
                                                   neededFiles = character(),
                                                   checkSumFilePath = theRDSFile,
                                                   destinationPath = tmpdir)),
               "checkSumFilePath is not a CHECKSUMS.txt")

  # Doubley nested zips -- extract inner, inner
  a <- extractFromArchive(c(theZipFile2, theZipFile), neededFiles = character(), checkSumFilePath = csfp,
                          destinationPath = tmpdir)
  expect_true(isTRUE(all(file.exists(a$filesExtracted))))

  # triply
  a <- extractFromArchive(theZipFile3, neededFiles = theRDSFile, checkSumFilePath = csfp,
                          destinationPath = tmpdir)
  expect_true(length(a$extractedArchives) == 3)
  expect_true(length(a$filesExtracted) == 3)
  expect_true(all(basename(a$filesExtracted) %in% basename(c(theZipFile, theZipFile2, theRDSFile))))
  expect_true(all(basename(a$extractedArchives) %in% basename(c(theZipFile, theZipFile2, theZipFile3))))

  allZipsAndRDS <- c(theZipFile, theZipFile2, theZipFile3, theRDSFile)
  Checksums(tmpdir, write = TRUE, files = allZipsAndRDS, overwrite = TRUE)
  a <- extractFromArchive(theZipFile3, neededFiles = theRDSFile, checkSumFilePath = csfp,
                          destinationPath = tmpdir, checkSums = Checksums(tmpdir, files = allZipsAndRDS))
})

test_that("options inputPaths", {
  skip_on_cran()

  testInitOut <- testInit("raster",
                          opts = list("reproducible.inputPaths" = NULL,
                                      "reproducible.inputPathsRecursive" = FALSE))
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  theFile <- targetFileLuxRDS

  if (getRversion() <= "3.3.0")  skip("Doesn't work on R 3.3.0") # Not sure why this fails on 3.3.0
  options("reproducible.inputPaths" = NULL)
  options("reproducible.inputPathsRecursive" = FALSE)

  mess1 <- capture_messages({
    test1 <- prepInputs(targetFile = theFile,
                        destinationPath = tmpdir,
                        dlFun = getDataFn, name = "GADM", country = "LUX", level = 0,
                        path = tmpdir)
  })

  # Use inputPaths -- should do a link to tmpCache (the destinationPath)
  options("reproducible.inputPaths" = tmpdir)
  options("reproducible.inputPathsRecursive" = FALSE)
  mess1 <- capture_messages({
    test1 <- prepInputs(targetFile = theFile,
                        destinationPath = tmpCache,
                        dlFun = getDataFn, name = "GADM", country = "LUX", level = 0,
                        path = tmpCache)
  })
  expect_true(sum(grepl(paste0("Hardlinked version of file created at: ", tmpCache), mess1)) == 1)

  # Now two folders - file not in destinationPath, not in 1st inputPaths, but yes 2nd
  #   should hardlink from 2nd IP to destinationPath, make sure CHECKSUMS.txt is correct in both
  options("reproducible.inputPaths" = c(tmpdir, tmpCache))
  file.remove(file.path(tmpdir, theFile))
  tmpdir3 <- file.path(tmpCache, "test")
  mess1 <- capture_messages({
    test1 <- prepInputs(targetFile = theFile,
                        destinationPath = tmpdir3,
                        dlFun = getDataFn, name = "GADM", country = "LUX", level = 0,
                        path = tmpdir3)
  })
  expect_true(sum(grepl(paste0("Hardlinked version of file created at: ", tmpdir3), mess1)) == 1)

  #  should copy from 2nd directory (tmpCache) because it is removed in the lower
  #  tmpdir directory & has a CHECKSUMS.txt
  options("reproducible.inputPaths" = tmpdir)
  options("reproducible.inputPathsRecursive" = TRUE)
  file.remove(file.path(tmpCache, theFile))
  tmpdir1 <- file.path(tmpCache, "test1")
  mess1 <- capture_messages({
    test1 <- prepInputs(targetFile = theFile,
                        destinationPath = tmpdir1,
                        dlFun = getDataFn, name = "GADM", country = "LUX", level = 0,
                        path = tmpdir1)
  })
  expect_true(sum(grepl(paste0("Hardlinked version of file created at: ", file.path(tmpdir1, theFile)), mess1)) == 1)
  expect_true(sum(grepl(paste0("which points to ", file.path(tmpdir3, theFile)), mess1)) == 1)
  expect_true(sum(basename(dir(file.path(tmpdir), recursive = TRUE)) %in% theFile) == 2)


  ####
  # Try download to inputPath, intercepting the destination, creating a link
  testOnExit(testInitOut)
  testInitOut <- testInit("raster",
                          opts = list("reproducible.inputPaths" = NULL,
                                      "reproducible.inputPathsRecursive" = FALSE))
  options("reproducible.inputPaths" = tmpdir)
  tmpdir2 <- file.path(tmpdir, rndstr(1,5))
  mess1 <- capture_messages({
    test1 <- prepInputs(targetFile = theFile,
                        destinationPath = tmpdir2,
                        dlFun = getDataFn, name = "GADM", country = "LUX", level = 0,
                        path = tmpCache)
  })
  expect_true(sum(grepl("Hardlinked version of file created", mess1)) == 1)

  # Have file in inputPath, not in destinationPath
  unlink(file.path(tmpdir2, theFile))
  expect_false(file.exists(file.path(tmpdir2, theFile))) # FALSE -- confirm previous line
  expect_true(file.exists(file.path(tmpdir, theFile))) # TRUE b/c is in getOption('reproducible.inputPaths')
  tmpdir2 <- file.path(tmpdir, rndstr(1, 5))
  mess1 <- capture_messages({
    test1 <- prepInputs(targetFile = theFile,
                        destinationPath = tmpdir2,
                        dlFun = getDataFn, name = "GADM", country = "LUX", level = 0,
                        path = tmpCache)
  })
  expect_true(sum(grepl("Hardlinked version of file created", mess1)) == 1) # used a linked version
  expect_true(sum(grepl(basename(tmpdir2), mess1)) == 1) # it is now in tmpdir2, i.e., the destinationPath

  # Have file in destinationPath, not in inputPath
  unlink(file.path(tmpdir, theFile))
  expect_false(file.exists(file.path(tmpdir, theFile))) # FALSE -- confirm previous line
  expect_true(file.exists(file.path(tmpdir2, theFile))) # TRUE b/c is in getOption('reproducible.inputPaths')
  mess1 <- capture_messages({
    test1 <- prepInputs(targetFile = theFile,
                        destinationPath = tmpdir2,
                        dlFun = getDataFn, name = "GADM", country = "LUX", level = 0,
                        path = tmpCache)
  })
  expect_true(sum(grepl("Hardlinked version of file created", mess1)) == 1) # used a linked version
  expect_true(sum(grepl(basename(tmpdir2), mess1)) == 1) # it is now in tmpdir2, i.e., the destinationPath
})

test_that("writeOutputs saves factor rasters with .grd class to preserve levels", {
  skip_on_cran()

  testInitOut <- testInit("raster", opts = list("reproducible.overwrite" = TRUE,
                                                "reproducible.inputPaths" = NULL),
                          needGoogle = TRUE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  a <- raster(extent(0, 2, 0, 2), res = 1, vals = c(1, 1, 2, 2))
  levels(a) <- data.frame(ID = 1:2, Factor = c("This", "That"))
  tifTmp <- tempfile(fileext = ".tif")
  file.create(tifTmp)
  tifTmp <- normPath(tifTmp)

  b1 <- writeRaster(a, filename = tifTmp, overwrite = TRUE)
  expect_warning(b1a <- writeOutputs(a, filename2 = tifTmp))
  expect_false(identical(b1, b1a))
  expect_true(identical(as.integer(b1[]), b1a[]))

  expect_true(identical(normPath(filename(b1)), normPath(tifTmp)))
  expect_true(identical(normPath(filename(b1a)), normPath(gsub(tifTmp, pattern = "tif", replacement = "grd"))))
})

test_that("rasters aren't properly resampled", {
  testInitOut <- testInit("raster", opts = list("reproducible.overwrite" = TRUE,
                                                "reproducible.inputPaths" = NULL),
                          needGoogle = TRUE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  a <- raster(extent(0, 20, 0, 20), res = 1, vals = 1:400)
  b <- raster(extent(0, 20, 0, 20), res = c(2,2), vals = 1:100)
  crs(a) <- "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  crs(b) <- "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

  tiftemp1 <- tempfile(fileext = ".tif")
  writeRaster(a, filename = tiftemp1)

  tiftemp2 <- tempfile(fileext = ".tif")
  writeRaster(b, filename = tiftemp2)

  out <- prepInputs(targetFile = tiftemp1, rasterToMatch = raster(tiftemp2),
                    destinationPath = tempdir(), useCache = FALSE)
  expect_true(dataType(out) == "INT2U")

  out2 <- prepInputs(targetFile = tiftemp1, rasterToMatch = raster(tiftemp2),
                    destinationPath = tempdir(), method = "bilinear", filename2 = tempfile(fileext = ".tif"))
  expect_true(dataType(out2) == "FLT4S")

  c <- raster(extent(0, 20, 0, 20), res = 1, vals = runif(400, 0, 1))
  crs(c) <- "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  tiftemp3 <- tempfile(fileext = ".tif")
  writeRaster(c, filename = tiftemp3)

  out3 <- prepInputs(targetFile = tiftemp3, rasterToMatch = raster(tiftemp2),
                     destinationPath = tempdir(), filename2 = tempfile(fileext = ".tif"))
  expect_true(dataType(out3) == "FLT4S")
})

test_that("System call gdal works", {
  skip_on_cran()

  testInitOut <- testInit("raster")
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  ras <- raster(extent(0, 10, 0, 10), res = 1, vals = 1:100)
  crs(ras) <- "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  ras <- writeRaster(ras, filename = tempfile(), format = "GTiff")

  ras2 <- raster(extent(0,8,0,8), res = 1, vals = 1:64)
  crs(ras2) <- "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

  raster::rasterOptions(todisk = TRUE) #to trigger GDAL

  test1 <- prepInputs(targetFile = ras@file@name, destinationPath = tempdir(),
                      rasterToMatch = ras2, useCache = FALSE)
  expect_true(file.exists(test1@file@name)) #exists on disk after gdalwarp
  expect_true(dataType(test1) == "INT1U") #properly resampled

  ras <- raster::setValues(ras, values = runif(n = ncell(ras), min = 1, max = 2))
  ras <- writeRaster(ras, filename = tempfile(), format = "GTiff")
  test2 <- prepInputs(targetFile = ras@file@name,
                      destinationPath = tempdir(),
                      rasterToMatch = ras2, useCache = FALSE, method = "bilinear")
  expect_true(dataType(test2) == "FLT4S")

  on.exit(raster::rasterOptions(todisk = FALSE))
})

test_that("System call gdal will make the rasters match for rasterStack", {

  skip_on_cran()

  testInitOut <- testInit("raster")
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  ras <- raster(extent(0, 4, 0, 4), res = 2, vals = 1:4)
  crs(ras) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

  #next line generates intermittent error: In .Internal(gc(verbose, reset, full)) :
  #closing unused connection 3 (C:/Temp/RtmpU5EOTS/raster/r_tmp_2018-12-03_143339_14468_30160.gri)
  ras1 <- suppressWarnings(raster::projectRaster(from = ras, crs = "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=49 +lon_0=-95 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0", method = "ngb"))

  ras1 <- writeRaster(ras, filename = tempfile(), format = "GTiff")

  ras2 <- raster(extent(0,8,0,8), res = 1, vals = 1:64)
  crs(ras2) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

  raster::rasterOptions(todisk = TRUE) #to trigger GDAL

  test1 <- prepInputs(targetFile = ras1@file@name, destinationPath = tempdir(),
                      rasterToMatch = ras2, useCache = FALSE, method = 'ngb')

  expect_true(file.exists(test1@file@name)) #exists on disk after gdalwarp
  expect_true(dataType(test1) == "INT1U")
  expect_identical(raster::res(ras2), raster::res(test1))
  expect_identical(raster::extent(ras2), raster::extent(test1))
  expect_identical(raster::crs(ras2), raster::crs(test1))

  on.exit(raster::rasterOptions(todisk = FALSE))
})
