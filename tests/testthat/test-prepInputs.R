test_that("prepInputs doesn't work", {
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  testthat::skip_on_appveyor()

  testInitOut <- testInit("raster")
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  #######################################
  ### url  ######
  #######################################
  dPath <- file.path(tmpdir, "ecozones")
  mess <- capture_messages(
    shpEcozone <- prepInputs(destinationPath = dPath,
                             url = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip")
  )
  expect_true(any(grepl(mess, pattern = "ecozone_shp.zip")))
  expect_true(any(grepl(mess, pattern = "Writing")))
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

  # Add a study area to Crop and Mask to
  # Create a "study area"
  coords <- structure(c(-122.98, -116.1, -99.2, -106, -122.98,
                        59.9, 65.73, 63.58, 54.79, 59.9),
                      .Dim = c(5L, 2L))
  Sr1 <- Polygon(coords)
  Srs1 <- Polygons(list(Sr1), "s1")
  StudyArea <- SpatialPolygons(list(Srs1), 1L)
  crs(StudyArea) <- "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"


  #######################################
  ### url, targetFile, alsoExtract -- with Cache ######
  #######################################
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
  clearCache(userTags = "prepInputs", ask = FALSE)
  # previously, this would cause an error because prepInputs file is gone b/c of previous
  #  line, but postProcess is still in a Cache recovery situation, to same file, which is
  #  not there. Now should be no error
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

  #######################################
  ###  archive     ######
  #######################################
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
  LCC2005 <- prepInputs(
    archive = "LandCoverOfCanada2005_V1_4.zip",
    destinationPath = asPath(dPath),
    studyArea = StudyArea,
    purge = TRUE
  )
  expect_false(any(grepl("From:LandCoverOfCanada2005_V1_4.zip", mess)))
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
  expect_true(any(grepl("Skipping extractFromArchive", mess)))
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

  skip(message = "Untested deeper tests")
  file.remove(dir(dPath, full.names = TRUE, recursive = TRUE))
  polyMatrix <- matrix(c(-121.85, 53.04), ncol = 2)
  template <- prepInputs(url = "ftp://ftp.ccrs.nrcan.gc.ca/ad/NLCCLandCover/LandcoverCanada2005_250m/LandCoverOfCanada2005_V1_4.zip",
                         destinationPath = dPath)


  # fix StudyArea
  x <- postProcess(x = StudyArea, destinationPath = dPath) # Not using rasterToMatch: works

  ## TODO: remove SpaDES.shiny dependency
  #studyArea2 <- SpaDES.tools::randomPolygon(x = polyMatrix, hectares = 1000)
  #studyArea2 <- spTransform(studyArea2, crs(LCC2005))
  #rgeos::gArea(studyArea2)
  #rgeos::gArea(StudyArea)

  ## rasterToMatch: doesn't work; doesn't finish nor returns error.
  #postProcess(x = studyArea, targetFilePath = dataPath(sim),
  #            destinationPath = dataPath(sim), rasterToMatch = template)
})

test_that("interactive prepInputs", {
  testInitOut <- testInit("raster")
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  if (interactive()) {
    #setwd(tempdir())
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
    birdSpecies <- c("BBWA", "YRWA")
    urls <- c("https://drive.google.com/open?id=1CmzYNpxwWr82PoRSbHWG8yg2cC3hncfb",
              "https://drive.google.com/open?id=11Hxk0CcwJsoAnUgfrwbJhXBJNM5Xbd9e")

    #######################################
    ### url, targetFile, archive     ######
    #######################################
    testOnExit(testInitOut)
    testInitOut <- testInit("raster")
    outsideModule <- Map(x = birdSpecies, url = urls,
                         MoreArgs = list(tmpdir = tmpdir),
                         function(x, url, tmpdir) {
                           ras <- prepInputs(
                             targetFile = paste0(x, "_currmean.asc"),
                             archive = paste0(x, "_current.zip"),
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
  testInitOut <- testInit("raster")
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  testthat::skip_on_cran()
  testthat::skip_on_travis()
  testthat::skip_on_appveyor()

  testthat::skip_if_not(interactive())

  urlTif1 <- "https://raw.githubusercontent.com/PredictiveEcology/quickPlot/master/inst/maps/DEM.tif"
  urlShapefiles1Zip <- "https://drive.google.com/file/d/1Bk4SPz8rx8zziIlg2Yp9ELZmdNZytLqb/view?usp=sharing"
  urlShapefilesZip <- "https://drive.google.com/file/d/1z1x0oI5jUDJQosOXacI8xbzbR15HFi0W/view?usp=sharing"

  expectedMessageRaw <- c("Running preP", "Preparing:", "File downloaded",
                          "From:Shapefile", "Checking local", "Finished checking",
                          "Downloading", "Skipping download", "Skipping extractFrom",
                          "targetFile was not.*Trying raster",
                          "Writing checksums.*you can specify targetFile",
                          "No targetFile supplied, so can't use")
  expectedMessage <- paste0(collapse = "|", expectedMessageRaw)

  runTest <- function(prod, class, numFiles, mess, expectedMess = expectedMessage, filePattern) {
    files <- dir(tmpdir, pattern = filePattern, full.names = TRUE)
    expect_true(length(files) == numFiles)
    expect_is(test, class)
    message(mess)
    print(hasMessageNum <-
            paste(collapse = "_", which(unlist(
              lapply(strsplit(expectedMessage, "\\|")[[1]], function(m)
                any(grepl(m, mess)))
            ))))

    isOK <- hasMessageNum == prod
    if (!isOK) {
      expe <- as.numeric(strsplit(prod, split = "_")[[1]])
      getting <- as.numeric(strsplit(hasMessageNum, split = "_")[[1]])

      expectedMessVec <- strsplit(expectedMessage, split = "\\|")[[1]]
      message("expecting, but didn't get ", expectedMessVec[setdiff(expe, getting)])
      message("got, but didn't expect ", paste(collapse = ", ", expectedMessVec[setdiff(getting, expe)]))
    }
    expect_true(isOK) #
  }

  ################################################################
  ###### url                                                 #####
  ################################################################
  mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    url = urlTif1,
    destinationPath = tmpdir
  )))
  runTest("1_2_5_6_7_10_11", "Raster", 1, mess, filePattern = "DEM")

  # 2nd time # no targetFile, so can't checksums
  mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    url = urlTif1,
    destinationPath = tmpdir
  )))
  runTest("1_2_5_6_7_10", "Raster", 1, mess, filePattern = "DEM")
  unlink(dir(tmpdir, full.names = TRUE))

  # url is an archive on googledrive -- can get file.info from remote -- so can do checksums
  mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    url = urlShapefiles1Zip,
    destinationPath = tmpdir
  )))
  runTest("1_2_3_4_5_6_7_10_11", "SpatialPolygons", 5, mess, filePattern = "Shapefile")

  # 2nd time # can checksums
  mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    url = urlShapefiles1Zip,
    destinationPath = tmpdir
  )))
  runTest("1_2_5_6_8_9_10", "SpatialPolygons", 5, mess, filePattern = "Shapefile")
  unlink(dir(tmpdir, full.names = TRUE))

  ################################################################
  ###### url, targetFile                                     #####
  ################################################################
  mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    url = urlTif1,
    targetFile = basename(urlTif1),
    destinationPath = tmpdir
  )))
  runTest("1_2_5_6_7_11", "Raster", 1, mess, filePattern = "DEM")

  # 2nd time # can checksums
  mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    url = urlTif1,
    targetFile = basename(urlTif1),
    destinationPath = tmpdir
  )))
  runTest("1_2_5_6_8", "Raster", 1, mess, filePattern = "DEM")
  unlink(dir(tmpdir, full.names = TRUE))

  # url is an archive on googledrive --
  mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    url = urlShapefiles1Zip,
    targetFile = "Shapefile1.shp",
    destinationPath = tmpdir
  )))
  runTest("1_2_3_4_5_6_7_11", "SpatialPolygons", 5, mess, filePattern = "Shapefile")

  # 2nd time # can checksums
  mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    url = urlShapefiles1Zip,
    targetFile = "Shapefile1.shp",
    destinationPath = tmpdir
  )))
  runTest("1_2_5_6_8_9", "SpatialPolygons", 5, mess, filePattern = "Shapefile")
  unlink(dir(tmpdir, full.names = TRUE))


  ################################################################
  ###### url, alsoExtract                                    #####
  ################################################################
  mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    url = urlTif1,
    alsoExtract = "DEM.tif",
    destinationPath = tmpdir
  )))
  runTest("1_2_5_6_7_10_11", "Raster", 1, mess, filePattern = "DEM")

  # 2nd time # can't use checksums because don't have targetFile
  mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    url = urlTif1,
    alsoExtract = "DEM.tif",
    destinationPath = tmpdir
  )))
  runTest("1_2_5_6_7_10", "Raster", 1, mess, filePattern = "DEM")
  unlink(dir(tmpdir, full.names = TRUE))

  # url is an archive on googledrive --
  mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    url = urlShapefiles1Zip,
    alsoExtract = c("Shapefile1.dbf", "Shapefile1.prj", "Shapefile1.shp", "Shapefile1.shx"),
    destinationPath = tmpdir
  )))
  runTest("1_2_3_4_5_6_7_10_11", "SpatialPolygons", 5, mess, filePattern = "Shapefile")

  # 2nd time # can't checksums because no targetfile
  mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    url = urlShapefiles1Zip,
    alsoExtract = c("Shapefile1.dbf", "Shapefile1.prj", "Shapefile1.shp", "Shapefile1.shx"),
    destinationPath = tmpdir
  )))
  runTest("1_2_3_5_6_7_9_10", "SpatialPolygons", 5, mess, filePattern = "Shapefile")
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
  runTest("1_2_3_4_5_6_7_10_11", "SpatialPolygons", 9, mess, filePattern = "Shapefile")

  # 2nd time # can checksums
  mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    url = urlShapefilesZip,
    archive = "Shapefiles1.zip",
    destinationPath = tmpdir
  )))
  runTest("1_2_5_6_8_9_10", "SpatialPolygons", 9, mess, filePattern = "Shapefile")
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
  runTest("1_2_3_4_5_6_7_11", "SpatialPolygons", 5, mess, filePattern = "Shapefile")

  # 2nd time # can checksums
  mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    url = urlShapefiles1Zip,
    archive = "Shapefiles1.zip",
    targetFile = "Shapefile1.shp",
    destinationPath = tmpdir
  )))
  runTest("1_2_5_6_8_9", "SpatialPolygons", 5, mess, filePattern = "Shapefile")
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
  runTest("1_2_3_4_5_6_7_11", "SpatialPolygons", 5, mess, filePattern = "Shapefile")

  # 2nd time # can checksums
  mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    url = urlShapefilesZip,
    targetFile = "Shapefile1.shp",
    alsoExtract = c("Shapefile1.dbf", "Shapefile1.prj", "Shapefile1.shx"),
    destinationPath = tmpdir
  )))
  runTest("1_2_5_6_8_9", "SpatialPolygons", 5, mess, filePattern = "Shapefile")
  unlink(dir(tmpdir, full.names = TRUE))
  mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    url = urlShapefilesZip,
    targetFile = "Shapefile1.shp",
    alsoExtract = c("similar"),
    destinationPath = tmpdir
  )))
  runTest("1_2_3_4_5_6_7_11", "SpatialPolygons", 5, mess, filePattern = "Shapefile")
  mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    url = urlTif1,
    targetFile = "DEM.tif",
    alsoExtract = c("DEM.tif"),
    destinationPath = tmpdir
  )))
  runTest("1_2_5_6_7", "Raster", 1, mess, filePattern = "DEM")
  unlink(dir(tmpdir, full.names = TRUE))

  ################################################################
  ###### url, archive, alsoExtract               #####
  ################################################################
  # url is an archive on googledrive --
  mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    url = urlShapefilesZip,
    archive = "Shapefiles1.zip",
    alsoExtract = "similar",
    destinationPath = tmpdir
  )))
  runTest("1_2_3_4_5_6_7_10_11_12", "SpatialPolygons", 9, mess, filePattern = "Shapefile")

  # 2nd time # can checksums
  mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    url = urlShapefilesZip,
    archive = "Shapefiles1.zip",
    alsoExtract = "similar",
    destinationPath = tmpdir
  )))
  runTest("1_2_5_6_8_9_10_12", "SpatialPolygons", 9, mess, filePattern = "Shapefile")

  unlink(dir(tmpdir, full.names = TRUE))
  expect_error(mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    url = urlShapefilesZip,
    archive = "Shapefiles1.zip",
    alsoExtract = c("Shapefile1.dbf", "Shapefile1.prj", "Shapefile1.shx"),
    destinationPath = tmpdir
  ))))

  unlink(dir(tmpdir, full.names = TRUE))

  ################################################################
  ###### url, targetFile, alsoExtract               #####
  ################################################################
  # url is an archive on googledrive --
  mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    url = urlShapefilesZip,
    alsoExtract = "similar",
    targetFile = "Shapefile1.shp",
    destinationPath = tmpdir
  )))
  runTest("1_2_3_4_5_6_7_11", "SpatialPolygons", 5, mess, filePattern = "Shapefile")
  mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    url = urlShapefilesZip,
    alsoExtract = "similar",
    targetFile = "Shapefile1.shp",
    destinationPath = tmpdir
  )))
  runTest("1_2_5_6_8_9", "SpatialPolygons", 5, mess, filePattern = "Shapefile")
  unlink(dir(tmpdir, full.names = TRUE))

  # 2nd time # can checksums
  mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    url = urlShapefilesZip,
    alsoExtract = c("Shapefile1.dbf", "Shapefile1.prj", "Shapefile1.shx"),
    targetFile = "Shapefile1.shp",
    destinationPath = tmpdir
  )))
  runTest("1_2_3_4_5_6_7_11", "SpatialPolygons", 5, mess, filePattern = "Shapefile")

  # 2nd time # can checksums
  mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    url = urlShapefilesZip,
    alsoExtract = c("Shapefile1.dbf", "Shapefile1.prj", "Shapefile1.shx"),
    targetFile = "Shapefile1.shp",
    destinationPath = tmpdir
  )))
  runTest("1_2_5_6_8_9", "SpatialPolygons", 5, mess, filePattern = "Shapefile")
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
  runTest("1_2_3_4_5_6_7_11", "SpatialPolygons", 5, mess, filePattern = "Shapefile")

  # 2nd time # can checksums
  mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    url = urlShapefilesZip,
    archive = "Shapefiles1.zip",
    alsoExtract = "similar",
    targetFile = "Shapefile1.shp",
    destinationPath = tmpdir
  )))
  runTest("1_2_5_6_8_9", "SpatialPolygons", 5, mess, filePattern = "Shapefile")
  #unlink(dir(tmpdir, full.names = TRUE))

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
  runTest("1_2_4_5_6_10_11", "SpatialPolygons", 9, mess, filePattern = "Shapefile")

  # 2nd time # can checksums
  mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    archive = "Shapefiles1.zip",
    destinationPath = tmpdir
  )))
  runTest("1_2_5_6_9_10", "SpatialPolygons", 9, mess, filePattern = "Shapefile")

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
  runTest("1_2_4_5_6_11", "SpatialPolygons", 9, mess, filePattern = "Shapefile")

  # 2nd time # can checksums
  mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    archive = "Shapefiles1.zip",
    targetFile = "Shapefile1.shp",
    destinationPath = tmpdir
  )))
  runTest("1_2_5_6_9", "SpatialPolygons", 9, mess, filePattern = "Shapefile")

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
  runTest("1_2_4_5_6_11", "SpatialPolygons", 5, mess, filePattern = "Shapefile")

  # 2nd time # can checksums
  mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    archive = "Shapefiles1.zip",
    targetFile = "Shapefile1.shp",
    alsoExtract = c("Shapefile1.dbf", "Shapefile1.prj", "Shapefile1.shp", "Shapefile1.shx"),
    destinationPath = tmpdir
  )))
  runTest("1_2_5_6_9", "SpatialPolygons", 5, mess, filePattern = "Shapefile")

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
  runTest("1_2_4_5_6_11", "SpatialPolygons", 5, mess, filePattern = "Shapefile")

  # 2nd time # can checksums
  mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    archive = "Shapefiles1.zip",
    targetFile = "Shapefile1.shp",
    alsoExtract = c("similar"),
    destinationPath = tmpdir
  )))
  runTest("1_2_5_6_9", "SpatialPolygons", 5, mess, filePattern = "Shapefile")

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
  runTest("1_2_5_6_11", "SpatialPolygons", 5, mess, filePattern = "Shapefile")
  mess <- capture_messages(warns <- capture_warnings(
      test <- prepInputs(
        targetFile = "Shapefile1.shp",
        destinationPath = tmpdir
      )
    ))
  runTest("1_2_5_6", "SpatialPolygons", 5, mess, filePattern = "Shapefile")

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
  runTest("1_2_5_6_11", "SpatialPolygons", 5, mess, filePattern = "Shapefile")
  mess <- capture_messages(warns <- capture_warnings(
      test <- prepInputs(
        targetFile = "Shapefile1.shp",
        alsoExtract = c("Shapefile1.dbf", "Shapefile1.prj", "Shapefile1.shp", "Shapefile1.shx"),
        destinationPath = tmpdir
      )
    ))
  runTest("1_2_5_6", "SpatialPolygons", 5, mess, filePattern = "Shapefile")

  ################################################################
  ###### alsoExtract                                         #####
  ################################################################
  file.remove(grep(dir(tmpdir, full.names = TRUE)[!R.utils::isDirectory(dir(tmpdir))],
                   pattern = "CHECKSUMS.txt", value = TRUE))
  mess <- capture_messages(warns <- capture_warnings(
      test <- prepInputs(
        alsoExtract = c("Shapefile1.dbf", "Shapefile1.prj", "Shapefile1.shp", "Shapefile1.shx"),
        destinationPath = tmpdir
      )
    ))
  runTest("1_2_5_6_10_11", "SpatialPolygons", 5, mess, filePattern = "Shapefile")
  mess <- capture_messages(warns <- capture_warnings(
      test <- prepInputs(
        alsoExtract = c("Shapefile1.dbf", "Shapefile1.prj", "Shapefile1.shp", "Shapefile1.shx"),
        destinationPath = tmpdir
      )
    ))
  runTest("1_2_5_6_10", "SpatialPolygons", 5, mess, filePattern = "Shapefile")

  file.remove(grep(dir(tmpdir, full.names = TRUE)[!R.utils::isDirectory(dir(tmpdir))],
                   pattern = "CHECKSUMS.txt", value = TRUE))
  expect_error(mess <-
    capture_messages(warns <- capture_warnings(
      test <- prepInputs(
        alsoExtract = c("Shapefile1.dbf", "Shapefile1.prj", "Shapefile1.shx"),
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
  runTest("1_2_4_5_6_10_11", "SpatialPolygons", 5, mess, filePattern = "Shapefile")

  # 2nd time # can checksums
  mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    archive = "Shapefiles1.zip",
    alsoExtract = c("Shapefile1.dbf", "Shapefile1.prj", "Shapefile1.shp", "Shapefile1.shx"),
    destinationPath = tmpdir
  )))
  runTest("1_2_5_6_9_10", "SpatialPolygons", 5, mess, filePattern = "Shapefile")

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
  runTest("1_2_4_5_6_11", "SpatialPolygons", 5, mess, filePattern = "Shapefile")

  # 2nd time # can checksums
  mess <- capture_messages(warns <- capture_warnings(test <- prepInputs(
    archive = "Shapefiles1.zip",
    targetFile = "Shapefile1.shp",
    alsoExtract = c("similar"),
    destinationPath = tmpdir
  )))
  runTest("1_2_5_6_9", "SpatialPolygons", 5, mess, filePattern = "Shapefile")

})

test_that("prepInputs doesn't work", {
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  testthat::skip_on_appveyor()

  testInitOut <- testInit("raster")
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  urlTif1 <- "https://raw.githubusercontent.com/PredictiveEcology/quickPlot/master/inst/maps/DEM.tif"
  test <- prepInputs(
    targetFile = "DEM.tif",
    url = urlTif1,
    destinationPath = tmpdir,
    useCache = TRUE
  )
})

test_that("assessDataType doesn't work", {
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
