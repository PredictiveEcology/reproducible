test_that("prepInputs doesn't work (part 1)", {
  skip_on_cran()
  skip_on_ci()

  testInit("terra", opts = list(
    "rasterTmpDir" = tempdir2(rndstr(1,6)),
    "reproducible.inputPaths" = NULL,
    "reproducible.overwrite" = TRUE,
    reproducible.showSimilar = TRUE), needInternet = TRUE
  )

  options("reproducible.cachePath" = tmpdir)

  # Add a study area to Crop and Mask to
  # Create a "study area"
  coords <- structure(c(-122.98, -116.1, -99.2, -106, -122.98, 59.9, 65.73, 63.58, 54.79, 59.9),
                      .Dim = c(5L, 2L))
  StudyArea <- terra::vect(coords, "polygons")
  terra::crs(StudyArea) <- crsToUse

  dPath <- file.path(tmpdir, "ecozones")

  ### url
  url <- "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip"

  noisyOutput <- capture.output(
    mess <- capture_messages(
      shpEcozone <- prepInputs(destinationPath = dPath, url = url)
    )
  )
  expect_true(any(grepl(mess, pattern = "ecozone_shp.zip")))
  expect_true(any(grepl(mess, pattern = "Appending")))
  expect_true(any(grepl(mess, pattern = "Finished")))
  expect_true(is(shpEcozone, vectorType()))

  # Robust to partial file deletions:
  unlink(dir(dPath, full.names = TRUE)[1:3])
  expect_error(terra::vect(file.path(dPath, "ecozone_shp.zip")))
  rm(shpEcozone)
  noisyOutput <- capture.output(
    shpEcozone1 <- prepInputs(destinationPath = dPath, url = url)
  )
  expect_true(is(shpEcozone1, vectorType()))
  unlink(dPath, recursive = TRUE)


  ### url, targetFile, alsoExtract ######g
  # Once this is done, can be more precise in operational code:
  #  specify targetFile, alsoExtract, and fun, wrap with Cache
  ecozoneFilename <- file.path(dPath, "Ecozones/ecozones.shp")
  ecozoneFiles <- c(
    "ecozones.dbf",
    "ecozones.prj",
    "ecozones.sbn",
    "ecozones.sbx",
    "ecozones.shp",
    "ecozones.shx"
  )
  noisyOutput <- capture.output(
    shpEcozone2 <- prepInputs(
      targetFile = ecozoneFilename,
      url = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip",
      alsoExtract = ecozoneFiles,
      destinationPath = dPath
    )
  )

  if (.requireNamespace("sf")) {
    expect_true(is(shpEcozone2, "sf"))
    testObj <- if (!is(shpEcozone1, "sf")) as(shpEcozone1, "sf") else shpEcozone1
  }

  # As of Jan 2022 -- these objects are very different; character encoding of accents, numbers interpretted as character
  # expect_equivalent(testObj, shpEcozone2) # different attribute newCache

  ### url, targetFile, alsoExtract -- with Cache
  # specify targetFile, alsoExtract, and fun, wrap with Cache -- it is wrong b/c no subfolder
  ecozoneFilename <- file.path(dPath, "ecozones.shp")
  # Note, you don't need to "alsoExtract" the archive... if the archive is not there, but the
  #   targetFile is there, it will not redownload the archive.

  unlink(dirname(ecozoneFilename), recursive = TRUE)
  # Test useCache = FALSE -- doesn't error and has no "loading from cache" or "loading from memoised"
  #aaaa <<- 1
  #on.exit(rm(aaaa, envir = .GlobalEnv))
  noisyOutput <- capture.output(
    warn <- suppressWarningsSpecific(
      falseWarnings = "attribute variables are assumed to be spatially constant", {
        mess <- capture_messages(
          shpEcozoneSm <- Cache(
            prepInputs,
            url = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip",
            targetFile = reproducible::asPath(ecozoneFilename),
            alsoExtract = reproducible::asPath(ecozoneFiles),
            studyArea = StudyArea,
            destinationPath = dPath,
            filename2 = "EcozoneFile.shp",
            useCache = FALSE
          )
        )
      })
  )
  expect_false(all(grepl("loading", mess)))

  # Test useCache -- doesn't error and loads from cache
  mess <- capture_messages(
    warn <- suppressWarningsSpecific(
      falseWarnings = "attribute variables are assumed to be spatially constant", {
        shpEcozoneSm <- Cache(
          prepInputs(
            url = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip",
            targetFile = reproducible::asPath(ecozoneFilename),
            alsoExtract = reproducible::asPath(ecozoneFiles),
            studyArea = StudyArea,
            destinationPath = dPath,
            filename2 = "EcozoneFile.shp",
            useCache = TRUE # with useTerra = TRUE, this is only for loading, not postProcess
          ))
      })
  )

  expect_true(any(grepl("loaded", mess)))

  ##  archive
  ## don't pass url -- use local copy of archive only
  ## use purge = TRUE to rm checksums file, rewrite it here
  noisyOutput <- capture.output(
    shpEcozone <- prepInputs(destinationPath = dPath,
                             archive = file.path(dPath, "ecozone_shp.zip"), purge = TRUE)
  )
  expect_true(is(shpEcozone, vectorType()))

  ### archive, alsoExtract char
  shpEcozone <- prepInputs(destinationPath = dPath,
                           archive = file.path(dPath, "ecozone_shp.zip"),
                           alsoExtract = c("ecozones.dbf", "ecozones.prj", "ecozones.sbn",
                                           "ecozones.sbx", "ecozones.shp", "ecozones.shx"))
  expect_true(is(shpEcozone, vectorType()))

  rm(shpEcozone)
  expect_false(exists("shpEcozone", inherits = FALSE))

  ### url, alsoExtract, archive
  # try again with url - should *not* download, even though checksums came from the
  #   prepInputs that had locally generated -- confirming that checksums with a manually copied file will work
  #   instead of forcing prepInputs to get the file.
  shpEcozone <- prepInputs(destinationPath = dPath,
                           url = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip",
                           archive = file.path(dPath, "ecozone_shp.zip"),
                           alsoExtract = c("ecozones.dbf", "ecozones.prj", "ecozones.sbn",
                                           "ecozones.sbx", "ecozones.shp", "ecozones.shx"))
  expect_true(is(shpEcozone, vectorType()))


})

test_that("interactive prepInputs", {
  skip_on_cran()
  skip_on_ci()
  testInit("terra",
                          opts = list(
                            "rasterTmpDir" = tempdir2(rndstr(1,6)),
                            "reproducible.overwrite" = TRUE,
                            "reproducible.inputPaths" = NULL
                          ),
                          needGoogleDriveAuth = TRUE)

  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  # skip_if_not(isInteractive())
  #######################################
  ### url
  #tmpdir <- "data/FMA"
  #checkPath(tmpdir, create = TRUE)

  noisyOutput <- capture.output(
    warns <- capture_warnings(
      test <- prepInputs(
        url = "https://drive.google.com/file/d/1BNsUiMqENJa0I8gzhO68K307ySPHbdGk/view?usp=sharing",
        destinationPath = tmpdir
      )
    )
  )
  files <- dir(tmpdir, pattern = "FMA_Boundary")
  expect_true(length(files) == 9)
  expect_true(inherits(test, vectorType()))

  #######################################
  ### url, targetFile
  # need authentication for this
  #tmpdir <- "data/FMA"
  #checkPath(tmpdir, create = TRUE)
  noisyOutput <- capture.output(
    warns <- capture_warnings(
      test <- prepInputs(
        targetFile = "FMA_Boundary_Updated.shp",
        url = "https://drive.google.com/file/d/1BNsUiMqENJa0I8gzhO68K307ySPHbdGk",
        destinationPath = tmpdir
      )
    )
  )
  # There is a meaningless warning for this unit test -- ignore it :
  # In rgdal::readOGR(dirname(x), fn, stringsAsFactors = stringsAsFactors,  :
  #                  Z-dimension discarded
  expect_true(inherits(test, vectorType()))

  # From Bird/Tati project
  testOnExit(testInitOut)
  testInit("terra", opts = list("reproducible.overwrite" = TRUE,
                                               "reproducible.inputPaths" = NULL),
                          needGoogleDriveAuth = TRUE)
  birdSpecies <- c("BBWA", "YRWA")
  urls <- c("https://drive.google.com/open?id=1CmzYNpxwWr82PoRSbHWG8yg2cC3hncfb",
            "https://drive.google.com/open?id=11Hxk0CcwJsoAnUgfrwbJhXBJNM5Xbd9e")

  #######################################
  ### url, targetFile, archive
  outsideModule <- Map(x = birdSpecies, url = urls,
                       MoreArgs = list(tmpdir = tmpdir),
                       function(x, url, tmpdir) {
                         ras <- prepInputs(
                           targetFile = paste0(x, "_currmean.asc"),
                           archive = paste0(x, "_current.zip"),
                           # fun = "raster::raster",
                           url = url,
                           destinationPath = tmpdir,
                           overwrite = TRUE
                         )
                       })
  expect_true(inherits(outsideModule[[1]], rasterType()))
  expect_true(inherits(outsideModule[[2]], rasterType()))
  # expect_true(inherits(terra::crs(outsideModule[[2]]), "CRS"))
  # expect_true(inherits(crs(outsideModule[[1]]), "CRS"))
  expect_false(identical(outsideModule[[1]], outsideModule[[2]]))

  # remove the .prj files -- test "similar"
  #######################################
  ### url, targetFile, archive, alsoExtract similar
  file.remove(grep(pattern = "asc|zip|CHECK",
                   invert = TRUE, value = TRUE,
                   dir(tmpdir, full.names = TRUE)[!isDirectory(dir(tmpdir))]))

  outsideModule <- Map(x = birdSpecies, url = urls,
                       MoreArgs = list(tmpdir = tmpdir),
                       function(x, url, tmpdir, purge) {
                         ras <- prepInputs(
                           targetFile = paste0(x, "_currmean.asc"),
                           archive = paste0(x, "_current.zip"),
                           url = url,
                           # fun = "raster::raster",
                           alsoExtract = "similar",
                           destinationPath = tmpdir,
                           overwrite = TRUE
                         )
                       })
  expect_true(inherits(outsideModule[[1]], rasterType()))
  expect_true(inherits(outsideModule[[2]], rasterType()))
  # expect_true(inherits(crs(outsideModule[[2]]), "CRS"))
  # expect_true(inherits(crs(outsideModule[[1]]), "CRS"))
  expect_true(!is.na(crs(outsideModule[[1]])))
  expect_false(identical(outsideModule[[1]], outsideModule[[2]]))

  # remove the .prj files -- test "similar"
  file.remove(grep(pattern = "asc|zip|CHECK",
                   invert = TRUE, value = TRUE,
                   dir(tmpdir, full.names = TRUE)[!isDirectory(dir(tmpdir))]))

  #######################################
  ### url, targetFile, archive, alsoExtract NA
  # because alsoExtract is NA ... no other files are unzipped, so no .prj and so no CRS
  outsideModule <- Map(x = birdSpecies, url = urls,
                       MoreArgs = list(tmpdir = tmpdir),
                       function(x, url, tmpdir, purge) {
                         ras <- prepInputs(
                           targetFile = paste0(x, "_currmean.asc"),
                           archive = paste0(x, "_current.zip"),
                           url = url,
                           alsoExtract = NULL,
                           destinationPath = tmpdir,
                           overwrite = TRUE
                         )
                       })
  expect_true(inherits(outsideModule[[1]], rasterType()))
  expect_true(inherits(outsideModule[[2]], rasterType()))
  expect_false(identical(terra::crs(outsideModule[[1]]), "")) # now with subfolders & all files, has crs
  expect_false(identical(outsideModule[[1]], outsideModule[[2]]))

})

test_that("preProcess doesn't work", {
  skip_on_cran()
  skip_on_ci()
  testInit("terra", opts = list(
    "reproducible.overwrite" = TRUE,
    "reproducible.inputPaths" = NULL
  ),
  needGoogleDriveAuth = TRUE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  skip_if_not(isInteractive())
  cls <- rasterType()
  # cls <- .fileExtsKnown()[.fileExtsKnown()[, "extension"] == "tif", "type"]

  # Note urlShapefiles1Zip, urlShapefilesZip, and urlTif1 are in helper-allEqual.R

  # # # # # Comment
  ##### url
  # # # # # Comment
  noisyOutput <- capture.output( # the sf::st_read
    mess <- capture_messages(
      warns <- capture_warnings(
        test <- prepInputs(url = urlTif1, destinationPath = tmpdir)
      )
    )
  )
  runTest("1_2_5_6_7_10_13", cls, 1, mess, expectedMess = expectedMessage,
          filePattern = "DEM", tmpdir = tmpdir, test = test)

  # 2nd time # no targetFile, but since url is simple, can guess correctly
  mess <- capture_messages(
    warns <- capture_warnings(
      test <- prepInputs(url = urlTif1, destinationPath = tmpdir)
    )
  )
  runTest("1_2_5_6_8_10", cls, 1, mess, expectedMess = expectedMessage,
          filePattern = "DEM", tmpdir = tmpdir, test = test)
  unlink(dir(tmpdir, full.names = TRUE))

  # url is an archive on googledrive -- can get file.info from remote -- so can do checksums
  noisyOutput <- capture.output(
    mess <- capture_messages(
      warns <- capture_warnings(
        test <- prepInputs(url = urlShapefiles1Zip, destinationPath = tmpdir)
      )
    )
  )

  runTest("1_2_4_5_6_7_10_12_13", vectorType(), 5, mess,
          expectedMess = expectedMessage,
          filePattern = "Shapefile",  # the file name is actually Shapefile1...
          tmpdir = tmpdir, test = test)

  # 2nd time # can checksums
  noisyOutput <- capture.output(
    mess <- capture_messages(
      warns <- capture_warnings(
        test <- prepInputs( url = urlShapefiles1Zip, destinationPath = tmpdir)
      )
    )
  )
  runTest("1_2_5_6_8_9_10_12", vectorType(), 5, mess, expectedMess = expectedMessage,
          filePattern = "Shapefile", tmpdir = tmpdir, test = test)
  unlink(dir(tmpdir, full.names = TRUE))

  # # # # # Comment
  ###### url, targetFile
  # # # # # Comment
  noisyOutput <- capture.output(
    mess <- capture_messages(
      warns <- capture_warnings(
        test <- prepInputs(url = urlTif1, targetFile = basename(urlTif1), destinationPath = tmpdir)
      )
    )
  )
  runTest("1_2_5_6_7_13", cls, 1, mess, expectedMess = expectedMessage,
          filePattern = "DEM", tmpdir = tmpdir, test = test)

  # 2nd time # can checksums
  mess <- capture_messages(
    warns <- capture_warnings(
      test <- prepInputs(url = urlTif1, targetFile = basename(urlTif1), destinationPath = tmpdir)
    )
  )
  runTest("1_2_5_6_8", cls, 1, mess, expectedMess = expectedMessage,
          filePattern = "DEM", tmpdir = tmpdir, test = test)
  unlink(dir(tmpdir, full.names = TRUE))

  # url is an archive on googledrive --
  noisyOutput <- capture.output(
    mess <- capture_messages(
      warns <- capture_warnings(
        test <- prepInputs(url = urlShapefiles1Zip, targetFile = "Shapefile1.shp",
                           destinationPath = tmpdir)
      )
    )
  )
  runTest("1_2_4_5_6_7_13", vectorType(), 5, mess, expectedMess = expectedMessage,
          filePattern = "Shapefile", tmpdir = tmpdir, test = test)

  ## 2nd time; can checksums
  noisyOutput <- capture.output(
    mess <- capture_messages(
      warns <- capture_warnings(
        test <- prepInputs(url = urlShapefiles1Zip, targetFile = "Shapefile1.shp",
                           destinationPath = tmpdir)
      )
    )
  )
  runTest("1_2_5_6_8_9", vectorType(), 5, mess, expectedMess = expectedMessage,
          filePattern = "Shapefile", tmpdir = tmpdir, test = test)
  unlink(dir(tmpdir, full.names = TRUE))

  # # # # # Comment
  ###### url, alsoExtract
  # # # # # Comment
  noisyOutput <- capture.output(
    mess <- capture_messages(
      warns <- capture_warnings(
        test <- prepInputs(url = urlTif1, alsoExtract = "DEM.tif", destinationPath = tmpdir)
      )
    )
  )
  runTest("1_2_5_6_7_10_13", cls, 1, mess, expectedMess = expectedMessage,
          filePattern = "DEM", tmpdir = tmpdir, test = test)

  # 2nd time # can use checksums, even though don't have targetFile, b/c simple url
  mess <- capture_messages(
    warns <- capture_warnings(
      test <- prepInputs(
        url = urlTif1,
        alsoExtract = "DEM.tif",
        destinationPath = tmpdir
      )
    )
  )
  runTest("1_2_5_6_8_10", cls, 1, mess, expectedMess = expectedMessage,
          filePattern = "DEM", tmpdir = tmpdir, test = test)
  unlink(dir(tmpdir, full.names = TRUE))

  # url is an archive on googledrive --
  noisyOutput <- capture.output(
    mess <- capture_messages(
      warns <- capture_warnings(
        test <- prepInputs(
          url = urlShapefiles1Zip,
          alsoExtract = c("Shapefile1.dbf", "Shapefile1.prj", "Shapefile1.shp", "Shapefile1.shx"),
          destinationPath = tmpdir
        )
      )
    )
  )
  runTest("1_2_4_5_6_7_10_13", vectorType(), 5, mess, expectedMess = expectedMessage,
          filePattern = "Shapefile", tmpdir = tmpdir, test = test)

  # 2nd time # can't checksums because no targetfile
  noisyOutput <- capture.output(
    mess <- capture_messages(
      warns <- capture_warnings(
        test <- prepInputs(
          url = urlShapefiles1Zip,
          alsoExtract = c("Shapefile1.dbf", "Shapefile1.prj", "Shapefile1.shp", "Shapefile1.shx"),
          destinationPath = tmpdir
        )
      )
    )
  )
  runTest("1_2_5_6_8_9_10", vectorType(), 5, mess, expectedMess = expectedMessage,
          filePattern = "Shapefile", tmpdir = tmpdir, test = test)
  unlink(dir(tmpdir, full.names = TRUE))

  # # # # # Comment
  ###### url, archive
  # # # # # Comment
  # url is an archive on googledrive -- here, zip has 2 Shapefile filesets -- Shapefile1* and Shapefile2*
  #   should extract all
  noisyOutput <- capture.output(
    mess <- capture_messages(
      warns <- capture_warnings(
        test <- prepInputs(
          url = urlShapefilesZip,
          archive = "Shapefiles1.zip",
          destinationPath = tmpdir
        )
      )
    )
  )
  runTest("1_2_4_5_6_7_10_12_13", vectorType(), 9, mess, expectedMess = expectedMessage,
          filePattern = "Shapefile", tmpdir = tmpdir, test = test)

  # 2nd time # can checksums
  mess <- capture_messages(
    warns <- capture_warnings(
      test <- prepInputs(
        url = urlShapefilesZip,
        archive = "Shapefiles1.zip",
        destinationPath = tmpdir
      )
    )
  )
  runTest("1_2_5_6_8_9_10_12", vectorType(), 9, mess, expectedMess = expectedMessage,
          filePattern = "Shapefile", tmpdir = tmpdir, test = test)
  unlink(dir(tmpdir, full.names = TRUE))

  # # # # # Comment
  ###### url, archive, targetFile
  # # # # # Comment
  # url is an archive on googledrive --
  noisyOutput <- capture.output(
    mess <- capture_messages(
      warns <- capture_warnings(
        test <- prepInputs(
          url = urlShapefiles1Zip,
          archive = "Shapefiles1.zip",
          targetFile = "Shapefile1.shp",
          destinationPath = tmpdir
        )
      )
    )
  )
  runTest("1_2_4_5_6_7_13", vectorType(), 5, mess, expectedMess = expectedMessage,
          filePattern = "Shapefile", tmpdir = tmpdir, test = test)

  # 2nd time # can checksums
  noisyOutput <- capture.output(
    mess <- capture_messages(
      warns <- capture_warnings(
        test <- prepInputs(
          url = urlShapefiles1Zip,
          archive = "Shapefiles1.zip",
          targetFile = "Shapefile1.shp",
          destinationPath = tmpdir
        )
      )
    )
  )
  runTest("1_2_5_6_8_9", vectorType(), 5, mess, expectedMess = expectedMessage,
          filePattern = "Shapefile", tmpdir = tmpdir, test = test)
  unlink(dir(tmpdir, full.names = TRUE))

  # # # # # Comment
  ###### url, targetFile, alsoExtract                        #####
  # # # # # Comment
  # url is an archive on googledrive --
  mess <- capture_messages(
    warns <- capture_warnings(
      test <- prepInputs(
        url = urlShapefilesZip,
        targetFile = "Shapefile1.shp",
        alsoExtract = c("Shapefile1.dbf", "Shapefile1.prj", "Shapefile1.shx"),
        destinationPath = tmpdir
      )
    )
  )
  runTest("1_2_4_5_6_7_13", vectorType(), 5, mess, expectedMess = expectedMessage,
          filePattern = "Shapefile", tmpdir = tmpdir, test = test)

  # 2nd time # can checksums
  mess <- capture_messages(
    warns <- capture_warnings(
      test <- prepInputs(
        url = urlShapefilesZip,
        targetFile = "Shapefile1.shp",
        alsoExtract = c("Shapefile1.dbf", "Shapefile1.prj", "Shapefile1.shx"),
        destinationPath = tmpdir
      )
    )
  )
  runTest("1_2_5_6_8_9", vectorType(), 5, mess, expectedMess = expectedMessage,
          filePattern = "Shapefile", tmpdir = tmpdir, test = test)
  unlink(dir(tmpdir, full.names = TRUE))


  mess <- capture_messages(
    warns <- capture_warnings(
      test <- prepInputs(
        url = urlShapefilesZip,
        targetFile = "Shapefile1.shp",
        alsoExtract = c("similar"),
        destinationPath = tmpdir
      )
    )
  )
  runTest("1_2_4_5_6_7_13", vectorType(), 5, mess, expectedMess = expectedMessage,
          filePattern = "Shapefile", tmpdir = tmpdir, test = test)
  noisyOutput <- capture.output(
    mess <- capture_messages(
      warns <- capture_warnings(
        test <- prepInputs(
          url = urlTif1,
          targetFile = "DEM.tif",
          alsoExtract = c("DEM.tif"),
          destinationPath = tmpdir
        )
      )
    )
  )
  runTest("1_2_5_6_7_13", cls, 1, mess, expectedMess = expectedMessage,
          filePattern = "DEM", tmpdir = tmpdir, test = test)
  unlink(dir(tmpdir, full.names = TRUE))

  # # # # # Comment
  ##### url, archive, alsoExtract               #####
  # # # # # Comment
  # url is an archive on googledrive --
  mess <- capture_messages(
    warns <- capture_warnings(
      test <- prepInputs(
        url = urlShapefilesZip,
        archive = "Shapefiles1.zip",
        alsoExtract = "similar",
        destinationPath = tmpdir
      )
    )
  )
  runTest("1_2_4_5_6_7_10_12_13", vectorType(), 9, mess, expectedMess = expectedMessage,
          filePattern = "Shapefile", tmpdir = tmpdir, test = test)

  # 2nd time # can checksums
  mess <- capture_messages(
    warns <- capture_warnings(
      test <- prepInputs(
        url = urlShapefilesZip,
        archive = "Shapefiles1.zip",
        alsoExtract = "similar",
        destinationPath = tmpdir
      )
    )
  )
  runTest("1_2_5_6_8_9_10_12", vectorType(), 9, mess, expectedMess = expectedMessage,
          filePattern = "Shapefile", tmpdir = tmpdir, test = test)

  unlink(dir(tmpdir, full.names = TRUE))
  expect_error(
    mess <- capture_messages(
      warns <- capture_warnings(
        test <- prepInputs(
          url = urlShapefilesZip,
          archive = "Shapefiles1.zip",
          alsoExtract = c("Shapefile1.dbf", "Shapefile1.prj", "Shapefile1.shx"),
          destinationPath = tmpdir
        )
      )
    )
  )

  unlink(dir(tmpdir, full.names = TRUE))

  # # # # # # Comment
  ###### url, targetFile, alsoExtract               #####
  # # # # # Comment
  # url is an archive on googledrive --
  mess <- capture_messages(
    warns <- capture_warnings(
      test <- prepInputs(
        url = urlShapefilesZip,
        alsoExtract = "similar",
        targetFile = "Shapefile1.shp",
        destinationPath = tmpdir
      )
    )
  )
  runTest("1_2_4_5_6_7_13", vectorType(), 5, mess, expectedMess = expectedMessage,
          filePattern = "Shapefile", tmpdir = tmpdir, test = test)
  mess <- capture_messages(
    warns <- capture_warnings(
      test <- prepInputs(
        url = urlShapefilesZip,
        alsoExtract = "similar",
        targetFile = "Shapefile1.shp",
        destinationPath = tmpdir
      )
    )
  )
  runTest("1_2_5_6_8_9", vectorType(), 5, mess, expectedMess = expectedMessage,
          filePattern = "Shapefile", tmpdir = tmpdir, test = test)
  unlink(dir(tmpdir, full.names = TRUE))

  # 2nd time # can checksums
  mess <- capture_messages(
    warns <- capture_warnings(
      test <- prepInputs(
        url = urlShapefilesZip,
        alsoExtract = c("Shapefile1.dbf", "Shapefile1.prj", "Shapefile1.shx"),
        targetFile = "Shapefile1.shp",
        destinationPath = tmpdir
      )
    )
  )
  runTest("1_2_4_5_6_7_13", vectorType(), 5, mess, expectedMess = expectedMessage,
          filePattern = "Shapefile", tmpdir = tmpdir, test = test)

  # 2nd time # can checksums
  mess <- capture_messages(
    warns <- capture_warnings(
      test <- prepInputs(
        url = urlShapefilesZip,
        alsoExtract = c("Shapefile1.dbf", "Shapefile1.prj", "Shapefile1.shx"),
        targetFile = "Shapefile1.shp",
        destinationPath = tmpdir
      )
    )
  )
  runTest("1_2_5_6_8_9", vectorType(), 5, mess, expectedMess = expectedMessage,
          filePattern = "Shapefile", tmpdir = tmpdir, test = test)
  unlink(dir(tmpdir, full.names = TRUE))

  # # # # # Comment
  ###### url, archive, targetFile, alsoExtract               #####
  # # # # # Comment
  # url is an archive on googledrive --
  mess <- capture_messages(
    warns <- capture_warnings(
      test <- prepInputs(
        url = urlShapefilesZip,
        archive = "Shapefiles1.zip",
        alsoExtract = "similar",
        targetFile = "Shapefile1.shp",
        destinationPath = tmpdir
      )
    )
  )
  runTest("1_2_4_5_6_7_13", vectorType(), 5, mess, expectedMess = expectedMessage,
          filePattern = "Shapefile", tmpdir = tmpdir, test = test)

  # 2nd time # can checksums
  mess <- capture_messages(
    warns <- capture_warnings(
      test <- prepInputs(
        url = urlShapefilesZip,
        archive = "Shapefiles1.zip",
        alsoExtract = "similar",
        targetFile = "Shapefile1.shp",
        destinationPath = tmpdir
      )
    )
  )
  runTest("1_2_5_6_8_9", vectorType(), 5, mess, expectedMess = expectedMessage,
          filePattern = "Shapefile", tmpdir = tmpdir, test = test)

  # # # # # Comment
  ###### archive
  # # # # # Comment
  # archive exists locally
  # remove all non archive files
  file.remove(grep(dir(tmpdir, full.names = TRUE)[!isDirectory(dir(tmpdir))], pattern = "\\.zip", invert = TRUE, value = TRUE))
  mess <- capture_messages(
    warns <- capture_warnings(
      test <- prepInputs(
        archive = "Shapefiles1.zip",
        destinationPath = tmpdir
      )
    )
  )
  runTest("1_2_4_5_6_9_10_12_13", vectorType(), 9, mess, expectedMess = expectedMessage,
          filePattern = "Shapefile", tmpdir = tmpdir, test = test)

  # 2nd time # can checksums
  mess <- capture_messages(
    warns <- capture_warnings(
      test <- prepInputs(
        archive = "Shapefiles1.zip",
        destinationPath = tmpdir
      )
    )
  )
  runTest("1_2_5_6_9_10_12", vectorType(), 9, mess, expectedMess = expectedMessage,
          filePattern = "Shapefile", tmpdir = tmpdir, test = test)

  # # # # # Comment
  ###### archive, targetFile
  # # # # # Comment
  # archive exists locally
  # remove all non archive files
  file.remove(grep(dir(tmpdir, full.names = TRUE)[!isDirectory(dir(tmpdir))],
                   pattern = "\\.zip", invert = TRUE, value = TRUE))
  mess <- capture_messages(
    warns <- capture_warnings(
      test <- prepInputs(
        archive = "Shapefiles1.zip",
        targetFile = "Shapefile1.shp",
        destinationPath = tmpdir
      )
    )
  )
  runTest("1_2_4_5_6_9_13", vectorType(), 9, mess, expectedMess = expectedMessage,
          filePattern = "Shapefile", tmpdir = tmpdir, test = test)

  # 2nd time # can checksums
  mess <- capture_messages(
    warns <- capture_warnings(
      test <- prepInputs(
        archive = "Shapefiles1.zip",
        targetFile = "Shapefile1.shp",
        destinationPath = tmpdir
      )
    )
  )
  runTest("1_2_5_6_9", vectorType(), 9, mess, expectedMess = expectedMessage,
          filePattern = "Shapefile", tmpdir = tmpdir, test = test)

  # # # # # Comment
  ###### archive, targetFile, alsoExtract                    #####
  # # # # # Comment
  # archive exists locally
  # remove all non archive files
  file.remove(grep(dir(tmpdir, full.names = TRUE)[!isDirectory(dir(tmpdir))],
                   pattern = "\\.zip", invert = TRUE, value = TRUE))
  mess <- capture_messages(
    warns <- capture_warnings(
      test <- prepInputs(
        archive = "Shapefiles1.zip",
        targetFile = "Shapefile1.shp",
        alsoExtract = c("Shapefile1.dbf", "Shapefile1.prj", "Shapefile1.shp", "Shapefile1.shx"),
        destinationPath = tmpdir
      )
    )
  )
  runTest("1_2_4_5_6_9_13", vectorType(), 5, mess, expectedMess = expectedMessage,
          filePattern = "Shapefile", tmpdir = tmpdir, test = test)

  # 2nd time # can checksums
  mess <- capture_messages(
    warns <- capture_warnings(
      test <- prepInputs(
        archive = "Shapefiles1.zip",
        targetFile = "Shapefile1.shp",
        alsoExtract = c("Shapefile1.dbf", "Shapefile1.prj", "Shapefile1.shp", "Shapefile1.shx"),
        destinationPath = tmpdir
      )
    )
  )
  runTest("1_2_5_6_9", vectorType(), 5, mess, expectedMess = expectedMessage,
          filePattern = "Shapefile", tmpdir = tmpdir, test = test)

  file.remove(grep(dir(tmpdir, full.names = TRUE)[!isDirectory(dir(tmpdir))],
                   pattern = "\\.zip", invert = TRUE, value = TRUE))
  file.remove(grep(dir(tmpdir, full.names = TRUE)[!isDirectory(dir(tmpdir))],
                   pattern = "CHECKSUMS.txt", value = TRUE))
  mess <- capture_messages(
    warns <- capture_warnings(
      test <- prepInputs(
        archive = "Shapefiles1.zip",
        targetFile = "Shapefile1.shp",
        alsoExtract = "similar",
        destinationPath = tmpdir
      )
    )
  )
  runTest("1_2_4_5_6_9_13", vectorType(), 5, mess, expectedMess = expectedMessage,
          filePattern = "Shapefile", tmpdir = tmpdir, test = test)

  # 2nd time # can checksums
  mess <- capture_messages(
    warns <- capture_warnings(
      test <- prepInputs(
        archive = "Shapefiles1.zip",
        targetFile = "Shapefile1.shp",
        alsoExtract = c("similar"),
        destinationPath = tmpdir
      )
    )
  )
  runTest("1_2_5_6_9", vectorType(), 5, mess, expectedMess = expectedMessage,
          filePattern = "Shapefile", tmpdir = tmpdir, test = test)

  # # # # # Comment
  ###### targetFile
  # # # # # Comment
  file.remove(grep(dir(tmpdir, full.names = TRUE), pattern = "CHECKSUMS.txt", value = TRUE))
  mess <- capture_messages(
    warns <- capture_warnings(
      test <- prepInputs(targetFile = "Shapefile1.shp", destinationPath = tmpdir)
    )
  )
  runTest("1_2_5_6", vectorType(), 5, mess, expectedMess = expectedMessage,
          filePattern = "Shapefile", tmpdir = tmpdir, test = test)
  mess <- capture_messages(
    warns <- capture_warnings(
      test <- prepInputs(targetFile = "Shapefile1.shp", destinationPath = tmpdir)
    )
  )
  runTest("1_2_5_6", vectorType(), 5, mess, expectedMess = expectedMessage,
          filePattern = "Shapefile", tmpdir = tmpdir, test = test)

  # # # # # Comment
  ###### targetFile, alsoExtract
  # # # # # Comment
  file.remove(grep(dir(tmpdir, full.names = TRUE)[!isDirectory(dir(tmpdir))],
                   pattern = "CHECKSUMS.txt", value = TRUE))
  mess <- capture_messages(
    warns <- capture_warnings(
      test <- prepInputs(
        targetFile = "Shapefile1.shp",
        alsoExtract = c("Shapefile1.dbf", "Shapefile1.prj", "Shapefile1.shp", "Shapefile1.shx"),
        destinationPath = tmpdir
      )
    )
  )
  runTest("1_2_5_6", vectorType(), 5, mess, expectedMess = expectedMessage,
          filePattern = "Shapefile", tmpdir = tmpdir, test = test)
  mess <- capture_messages(
    warns <- capture_warnings(
      test <- prepInputs(
        targetFile = "Shapefile1.shp",
        alsoExtract = c("Shapefile1.dbf", "Shapefile1.prj", "Shapefile1.shp", "Shapefile1.shx"),
        destinationPath = tmpdir
      )
    )
  )
  runTest("1_2_5_6", vectorType(), 5, mess, expectedMess = expectedMessage,
          filePattern = "Shapefile", tmpdir = tmpdir, test = test)

  # # # # # Comment
  ###### alsoExtract -- previously failed b/c no information; now ok-- .guessAtTargetAndFun #####
  # # # # # Comment
  file.remove(grep(dir(tmpdir, full.names = TRUE)[!isDirectory(dir(tmpdir))],
                   pattern = "CHECKSUMS.txt", value = TRUE))
  mess <- capture_messages(
    warns <- capture_warnings(
      test <- prepInputs(
        alsoExtract = c("Shapefile1.dbf", "Shapefile1.prj", "Shapefile1.shp", "Shapefile1.shx"),
        destinationPath = tmpdir
      )
    )
  )

  # # # # # Comment
  ###### archive, alsoExtract
  # # # # # Comment
  # archive exists locally
  # remove all non archive files
  file.remove(grep(dir(tmpdir, full.names = TRUE)[!isDirectory(dir(tmpdir))],
                   pattern = "\\.zip", invert = TRUE, value = TRUE))
  file.remove(grep(dir(tmpdir, full.names = TRUE)[!isDirectory(dir(tmpdir))],
                   pattern = "CHECKSUMS.txt", value = TRUE))
  mess <- capture_messages(
    warns <- capture_warnings(
      test <- prepInputs(
        archive = "Shapefiles1.zip",
        alsoExtract = c("Shapefile1.dbf", "Shapefile1.prj", "Shapefile1.shp", "Shapefile1.shx"),
        destinationPath = tmpdir
      )
    )
  )
  runTest("1_2_4_5_6_9_10_13", vectorType(), 5, mess, expectedMess = expectedMessage,
          filePattern = "Shapefile", tmpdir = tmpdir, test = test)

  # 2nd time # can checksums
  mess <- capture_messages(
    warns <- capture_warnings(
      test <- prepInputs(
        archive = "Shapefiles1.zip",
        alsoExtract = c("Shapefile1.dbf", "Shapefile1.prj", "Shapefile1.shp", "Shapefile1.shx"),
        destinationPath = tmpdir
      )
    )
  )
  runTest("1_2_5_6_9_10", vectorType(), 5, mess, expectedMess = expectedMessage,
          filePattern = "Shapefile", tmpdir = tmpdir, test = test)

  # Try without .shp -- fail
  file.remove(grep(dir(tmpdir, full.names = TRUE)[!isDirectory(dir(tmpdir))],
                   pattern = "\\.zip", invert = TRUE, value = TRUE))
  expect_error(
    mess <- capture_messages(
      warns <- capture_warnings(
        test <- prepInputs(
          archive = "Shapefiles1.zip",
          alsoExtract = c("Shapefile1.dbf", "Shapefile1.prj", "Shapefile1.shx"),
          destinationPath = tmpdir
        )
      )
    )
  )

  file.remove(grep(dir(tmpdir, full.names = TRUE)[!isDirectory(dir(tmpdir))],
                   pattern = "\\.zip", invert = TRUE, value = TRUE))
  file.remove(grep(dir(tmpdir, full.names = TRUE)[!isDirectory(dir(tmpdir))],
                   pattern = "CHECKSUMS.txt", value = TRUE))
  mess <- capture_messages(
    warns <- capture_warnings(
      test <- prepInputs(
        archive = "Shapefiles1.zip",
        targetFile = "Shapefile1.shp",
        alsoExtract = "similar",
        destinationPath = tmpdir
      )
    )
  )
  runTest("1_2_4_5_6_9_13", vectorType(), 5, mess, expectedMess = expectedMessage,
          filePattern = "Shapefile", tmpdir = tmpdir, test = test)

  # 2nd time # can checksums
  mess <- capture_messages(
    warns <- capture_warnings(
      test <- prepInputs(
        archive = "Shapefiles1.zip",
        targetFile = "Shapefile1.shp",
        alsoExtract = c("similar"),
        destinationPath = tmpdir
      )
    )
  )
  runTest("1_2_5_6_9", vectorType(), 5, mess, expectedMess = expectedMessage,
          filePattern = "Shapefile", tmpdir = tmpdir, test = test)
})

test_that("prepInputs when fun = NA", {
  skip_on_cran()
  skip_if_not(getRversion() > "3.3.0")

  testInit(c("sf", "terra"), opts = list(
    "rasterTmpDir" = tempdir2(rndstr(1,6)),
    "reproducible.overwrite" = TRUE,
    reproducible.interactiveOnDownloadFail = FALSE,
    "reproducible.inputPaths" = NULL
  ), needGoogleDriveAuth = TRUE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  coords <- structure(c(6, 6.1, 6.2, 6.15, 6, 49.5, 49.7, 49.8, 49.6, 49.5), .Dim = c(5L, 2L))
  StudyArea <- terra::vect(coords, "polygons")
  terra::crs(StudyArea) <- crsToUse


  noisyOutput <- capture.output(type = "message", {
    mess1 <- capture_messages(
      test1 <- try(silent = TRUE, {
        prepInputs(
          fun = NA,
          dlFun = getDataFn, name = "GADM", country = "LUX", level = 0,
          path = tmpdir)
      })
    )
  })
  if (!is(test1, "try-error")) {
    expect_true(is(test1, "SpatVector"))
    # test quoted version of `dlFun`
    noisyOutput3 <- capture.output(type = "message", {
      mess3 <- capture_messages(
        test3 <- prepInputs(
          fun = NA,
          dlFun = quote(getDataFn(name = "GADM", country = "LUX", level = 0, path = tmpdir)),
          destinationPath = tmpdir)
      )
    })
    expect_true(is(test3, "SpatVector"))

    if (.requireNamespace("sf")) {
      noisyOutput6 <- capture.output(type = "message", {
        mess6 <- capture_messages(
          test6 <- prepInputs(
            # targetFile = targetFileLuxRDS,
            dlFun = quote({
              out <- getDataFn(name = "GADM", country = "LUX", level = 0, path = tmpdir)
              sf::st_as_sf(out)
            }),
            tmpdir = tmpdir
          )
        )
      })
      expect_is(test6, "sf")
    }

  }
})

test_that("load rdata in prepInputs", {
  testInit("terra", tmpFileExt = "rda",
                          opts = list(
                            "reproducible.overwrite" = TRUE,
                            "reproducible.inputPaths" = NULL
                          ), needGoogleDriveAuth = TRUE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  a <- 1
  b <- 2
  save(a, b, file = tmpfile)
  aa <- prepInputs(tmpfile, fun = "base::load")
  expect_true(identical(aa, list(a = a, b = b)))

  d <- new.env(parent = emptyenv())
  aa <- prepInputs(tmpfile, fun = "base::load", envir = d)
  expect_false(identical(aa, list(a = a, b = b))) # not in aa, because loaded to d
  expect_true(identical(as.list(d), list(a = a, b = b)))
})

test_that("assessDataType doesn't work", {
  testInit("terra", opts = list(
    "reproducible.overwrite" = TRUE,
    "reproducible.inputPaths" = NULL
  ), needGoogleDriveAuth = TRUE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  ## LOG1S
  ras <- terra::rast(ncol = 10, nrow = 10)
  ras[] <- c(0, NaN, rep(c(0,1),49))
  expect_true(assessDataType(ras) == "LOG1S")

  ras <- terra::rast(ncol = 10, nrow = 10)
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
  ras <- terra::rast(ncol = 10, nrow = 10)
  ras[] <- 1:100
  expect_true(assessDataType(ras) == "INT1U")

  ras[] <- c(NA, 2:100)
  expect_true(assessDataType(ras) == "INT1U")

  ## INT2U
  ras <- terra::rast(ncol = 10, nrow = 10)
  ras[] <- round(runif(100, min = 64000, max = 65000))
  expect_true(assessDataType(ras) == "INT2U")

  ## INT2S
  ras <- terra::rast(ncol = 10, nrow = 10)
  ras[] <- round(runif(100, min = -32767, max = 32767))
  expect_true(assessDataType(ras) == "INT2S")

  ras[54] <- NA
  expect_true(assessDataType(ras) == "INT2S")

  ## INT4U
  ras <- terra::rast(ncol = 10, nrow = 10)
  ras[] <- round(runif(100, min = 0, max = 500000000))
  expect_true(assessDataType(ras) == "INT4U")

  ras[14] <- NA
  expect_true(assessDataType(ras) == "INT4U")

  ## INT4S
  ras <- terra::rast(ncol = 10, nrow = 10)
  ras[] <- round(runif(100, min = -200000000, max = 200000000))
  expect_true(assessDataType(ras) == "INT4S")

  ras[14] <- NA
  expect_true(assessDataType(ras) == "INT4S")

  ## FLT4S
  ras <- terra::rast(ncol = 10, nrow = 10)
  ras[] <- runif(100, min = -10, max = 87)
  expect_true(assessDataType(ras) == "FLT4S")

  ras <- terra::rast(ncol = 10, nrow = 10)
  ras[] <- round(runif(100, min = -3.4e+26, max = 3.4e+28))
  expect_true(assessDataType(ras) == "FLT4S")

  ras <- terra::rast(ncol = 10, nrow = 10)
  ras[] <- round(runif(100, min = 3.4e+26, max = 3.4e+28))
  expect_true(assessDataType(ras) == "FLT4S")

  ras <- terra::rast(ncol = 10, nrow = 10)
  ras[] <- round(runif(100, min = -3.4e+26, max = -1))
  expect_true(assessDataType(ras) == "FLT4S")

  ## FLT8S
  ras <- terra::rast(ncol = 10, nrow = 10)
  ras[] <- round(runif(100, min = -1.7e+30, max = 1.7e+308))
  expect_true(assessDataType(ras) == "FLT8S")

  ras <- terra::rast(ncol = 10, nrow = 10)
  ras[] <- round(runif(100, min = 1.7e+30, max = 1.7e+308))
  expect_true(assessDataType(ras) == "FLT8S")

  ras <- terra::rast(ncol = 10, nrow = 10)
  ras[] <- round(runif(100, min = -1.7e+308, max = -1))
  expect_true(assessDataType(ras) == "FLT8S")

  ras <- terra::rast(ncol = 10, nrow = 10)
  ras[] <- c(-Inf, 1, rep(c(0,1),49))
  expect_true(assessDataType(ras) == "FLT8S")

  ras <- terra::rast(ncol = 10, nrow = 10)
  ras[] <- c(Inf, 1, rep(c(0,1),49))
  expect_true(assessDataType(ras) == "FLT8S")
})

test_that("lightweight tests for code coverage", {
  skip_on_cran()
  testInit(c("sf", "terra"), opts = list("reproducible.overwrite" = TRUE,
                                                        "reproducible.inputPaths" = NULL),
                          needGoogleDriveAuth = TRUE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  url <- "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip"

  checkPath(tmpdir, create = TRUE)
  checkSums <- .emptyChecksumsResult
  checkSumFilePath <- file.path(tmpdir, "CHECKSUMS.txt")

  noisyOutput <- capture.output(
    downloadFile(url = url, neededFiles = "ecozones.shp", checkSums = checkSums,
                 archive = "ecozone_shp.zip", needChecksums = TRUE, quick = FALSE,
                 destinationPath = tmpdir, checksumFile = checkSumFilePath)
  )
  expect_true(file.exists(dir(tmpdir, pattern = "ecozone", full.names = TRUE)))


  # have local copy
  unzip("ecozone_shp.zip", exdir = tmpdir)
  file.copy(dir(file.path(tmpdir, "Ecozones"), full.names = TRUE), tmpdir)
  checkSums <- Checksums(path = tmpdir, write = TRUE)

  aMess <- capture_messages(
    downloadFile(url = url, neededFiles = "ecozones.shp", checkSums = checkSums,
                 targetFile = "ecozones.shp",
                 archive = NULL, needChecksums = TRUE, quick = FALSE,
                 destinationPath = file.path(tmpdir, "Ecozones"),
                 checksumFile = file.path(tmpdir, "CHECKSUMS.txt"))
  )

  if (!isMac()) {
    expect_true(any(grepl("Skipping download", aMess))) ## 2023-05-08: fails on macOS
  }

  filesForShp <- dir(file.path(tmpdir), pattern = "ecozones", full.names = TRUE)
  file.copy(filesForShp, tmpCache)
  # Need these in a test further down -- mostly just need the CRS
  filesForShp2 <- dir(file.path(tmpCache), pattern = "ecozones", full.names = TRUE)
  if (.requireNamespace("sf")) {
    noisyOutput <- capture.output(
      shpFile <- sf::st_read(grep(filesForShp2, pattern = "\\.shp", value = TRUE))
    )
  }
  # Test when wrong archive exists, wrong checkSums
  file.remove(file.path(tmpdir, "ecozone_shp.zip"))
  file.remove(filesForShp)
  file.create(file.path(tmpdir, "ecozone_shp.zip"))
  checkSums <- Checksums(path = tmpdir, write = TRUE)
  file.remove(file.path(tmpdir, "ecozone_shp.zip"))
  checkSums <- Checksums(path = tmpdir)

  noisyOutput <- capture.output(
    out <- try(silent = TRUE,
      downloadFile(url = url,
                   neededFiles = c("ecozones.dbf", "ecozones.prj", "ecozones.sbn", "ecozones.sbx",
                                   "ecozones.shp", "ecozones.shx"),
                   checkSums = checkSums,
                   targetFile = "ecozones.shp",
                   archive = "ecozone_shp.zip", needChecksums = TRUE, quick = FALSE,
                   destinationPath = tmpdir, checksumFile = checkSumFilePath)
    )
  )

  ## 2023-05-08: does not error on macOS
  isErr <- is(out, "try-error")
  #if (isMac()) expect_false(isErr) else
  expect_true(isErr)

  ## postProcess.default
  b <- 1
  expect_error(a <- postProcess(b), "from must be a")

  ## postProcess.list
  b <- list(1,1)
  expect_error(a <- postProcess(b), "from must be a")

  ras <- terra::rast(terra::ext(0,10,0,10), res = 1, vals = 1:100)
  terra::crs(ras) <- crsToUse

  expect_error(postProcess(ras, studyArea = 1), .msgGrep$anySpatialClass)
  expect_error(postProcess(ras, rasterToMatch = 1), .msgGrep$anySpatialClass)


  ## cropInputs.default
  b <- 1
  a <- cropInputs(b)
  expect_true(identical(a, b))

  ras2 <- terra::rast(terra::ext(0,5,0,5), res = 1, vals = 1:25)
  terra::crs(ras2) <- crsToUse
  a <- cropInputs(ras, extentToMatch = terra::ext(ras2), extentCRS = terra::crs(ras2))
  expect_true(inherits(a, "SpatRaster"))

  ras4 <- terra::rast(terra::ext(7,11,7,11), res = 1, vals = 1:16)
  sp4 <- terra::vect(terra::ext(ras4))
  terra::crs(sp4) <- crsToUse
  #sp4 <- sf::st_as_sfc(sf::st_bbox(ras4))
  #sf::st_crs(sp4) <- crsToUse

  grepMessHere <- "extents do not overlap"
  expect_error(cropInputs(ras2, studyArea = sp4), grepMessHere)

  ras3 <- terra::rast(terra::ext(0,5,0,5), res = 1, vals = 1:25)
  terra::crs(ras3) <- crsToUse

  ################################################
  # Different crs
  # Because studyArea is a Raster, then it doesn't work correctly
  a <- cropInputs(ras2, studyArea = ras3)
  expect_true(inherits(a, "SpatRaster"))
  expect_true(identical(terra::crs(a), terra::crs(ras2)))

  # Now rasterToMatch used -- internally reprojects it to x
  a <- cropInputs(ras2, rasterToMatch = ras3)
  expect_true(inherits(a, "SpatRaster"))
  expect_true(identical(terra::crs(a), terra::crs(ras2)))

  ## fixErrors.default
  b <- 1
  a <- fixErrors(b)
  expect_true(identical(a, b))

  ## projectInputs.Raster
  a <- projectInputs(ras2, rasterToMatch = ras3, method = "near")
  expect_true(inherits(a, "SpatRaster"))
  expect_true(identical(terra::crs(a), terra::crs(ras3)))

  a <- projectInputs(ras2, targetCRS = terra::crs(ras3), rasterToMatch = ras3, method = "near")
  expect_true(inherits(a, "SpatRaster"))
  expect_true(identical(terra::crs(a), terra::crs(ras3)))

  #warns if bilinear is passed for reprojecting integer
  if (.requireNamespace("sf")) {
    expect_warning(projectInputs(ras2, targetCRS = terra::crs(shpFile), method = "bilinear"))
  }

  #Works with no rasterToMatch
  a <- projectInputs(ras2, targetCRS = crs(ras3), method = "near")
  expect_true(identical(crs(a), crs(ras3)))

  # }
  # sp::CRS("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"))
})

test_that("lightweight tests 2 for code coverage", {
  skip_on_cran()

  testInit("terra", opts = list("reproducible.overwrite" = TRUE,
                                               "reproducible.inputPaths" = NULL),
                          needGoogleDriveAuth = TRUE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  theZipFile <- tempfile(tmpdir = tmpdir, fileext = ".zip")
  theZipFile2 <- tempfile(tmpdir = tmpdir, fileext = ".zip")
  theZipFile3 <- tempfile(tmpdir = tmpdir, fileext = ".zip")
  theZipName <- file.path(tmpdir, "hi.zip")
  theZapFile <- tempfile(tmpdir = tmpdir, fileext = ".zap")
  theRDSFile <- tempfile(tmpdir = tmpdir, fileext = ".rds")
  a <- 1
  saveRDS(a, file = theRDSFile)
  origWD <- setwd(dirname(theRDSFile))
  noisyOutput <- capture_output(zip(zipfile = theZipFile, files = basename(theRDSFile)))
  noisyOutput <- capture.output(zip(zipfile = theZipFile2, files = basename(theZipFile)))
  noisyOutput <- capture.output(zip(zipfile = theZipFile3, files = basename(theZipFile2)))
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
                          destinationPath = tmpdir, .tempPath = tempdir2())
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
  if (!requireNamespace("geodata", quietly = TRUE)) skip("Need geodata package")
  if (getRversion() <= "4.1.3") skip("geodata::gadm seems to time out on R <= 4.1.3")
  testInit(c("terra", "geodata"),
                          opts = list("reproducible.inputPaths" = NULL,
                                      "reproducible.inputPathsRecursive" = FALSE),
           needInternet = TRUE)

  f <- formals3(prepInputs)
  getDataFn <- getDataFn # not exported from reproducible; can access here, not in the dlFun

  if (getRversion() <= "3.3.0")  skip("Doesn't work on R 3.3.0") # Not sure why this fails on 3.3.0
  options("reproducible.inputPaths" = NULL)
  options("reproducible.inputPathsRecursive" = FALSE)

  noisyOutput <- capture.output(
    noisyOutput <- capture.output(type = "message", {
      mess1 <- capture_messages(
        test0 <- try(getDataFn(path = tmpdir, country = "LUX"), silent = TRUE)
      )}))
  useGADM <- !is(test0, "try-error")

  if (useGADM)
    noisyOutput <- capture.output(
      noisyOutput <- capture.output(type = "message", {
        mess1 <- capture_messages(
          test1 <- try(prepInputs(destinationPath = tmpdir,
                                  #url = if (!useGADM) url2 else f$url,
                                  #targetFile = if (useGADM) theFile else f$targetFile,
                                  dlFun = getDataFn,
                                  name = "GADM",
                                  country = "LUX",
                                  level = 0,
                                  path = tmpdir))
        )
      })
    )

  theFile <- if (useGADM) {
    targetFileLuxRDS
  } else {
    "rasterTest.tif"
  }
  url2 <- "https://github.com/tati-micheletti/host/raw/master/data/rasterTest.tif"

  noisyOutput <- capture.output(
    noisyOutput <- capture.output(type = "message", {
      mess1 <- capture_messages(
        test1 <- try(prepInputs(destinationPath = tmpdir,
                                url = if (!useGADM) url2 else f$url,
                                targetFile = if (useGADM) theFile else f$targetFile,
                                dlFun = if (useGADM) getDataFn else NULL,
                                name = if (useGADM) "GADM" else NULL,
                                country = if (useGADM) "LUX" else NULL,
                                level = if (useGADM) 0 else NULL,
                                path = if (useGADM) tmpdir else NULL))
      )
    })
  )
  # Use inputPaths -- should do a link to tmpCache (the destinationPath)
  options("reproducible.inputPaths" = tmpdir)
  options("reproducible.inputPathsRecursive" = FALSE)
  dlFun1 <- if (useGADM) getDataFn else NULL
  noisyOutput <- capture.output(
    mess1 <- capture_messages(
      test1 <- prepInputs(url = if (!useGADM) url2 else f$url,
                          targetFile = if (useGADM) theFile else f$targetFile,
                          dlFun = dlFun1,
                          name = if (useGADM) "GADM" else NULL,
                          country = if (useGADM) "LUX" else NULL,
                          level = if (useGADM) 0 else NULL,
                          path = if (useGADM) tmpdir else NULL,
                          destinationPath = tmpCache,
                          getDataFn = dlFun1)
    )
  )
  expect_true(sum(grepl(paste0("Hardlinked", ".*:"), mess1)) == 1)

  # Now two folders - file not in destinationPath, not in 1st inputPaths, but yes 2nd
  #   should hardlink from 2nd IP to destinationPath, make sure CHECKSUMS.txt is correct in both
  options("reproducible.inputPaths" = c(tmpdir, tmpCache))
  file.remove(file.path(tmpdir, theFile))
  tmpdir3 <- file.path(tmpCache, "test")
  noisyOutput <- capture.output(
    mess1 <- capture_messages(
      test1 <- prepInputs(url = if (!useGADM) url2 else f$url,
                          targetFile = if (useGADM) theFile else f$targetFile,
                          dlFun = if (useGADM) getDataFn else NULL,
                          name = if (useGADM) "GADM" else NULL,
                          country = if (useGADM) "LUX" else NULL,
                          level = if (useGADM) 0 else NULL,
                          path = if (useGADM) tmpdir else NULL,
                          destinationPath = tmpdir3)
    )
  )
  expect_true(sum(grepl(paste0(hardlinkMessagePrefixForGrep, ":\n", tmpdir3), mess1)) == 1)


  # THIS NEXT ONE DOESN"T PASS ON GA on WINDOWS, skip it
  #  should copy from 2nd directory (tmpCache) because it is removed in the lower
  #  tmpdir directory & has a CHECKSUMS.txt
  if (!isTRUE(as.logical(Sys.getenv("CI")))) { #(!testthat:::on_ci()) { # can't use the :::
    options("reproducible.inputPaths" = tmpdir)
    options("reproducible.inputPathsRecursive" = TRUE)
    file.remove(file.path(tmpCache, theFile))
    tmpdir1 <- file.path(tmpCache, "test1")
    noisyOutput <- capture.output(
      mess1 <- capture_messages(
        test1 <- prepInputs(url = if (!useGADM) url2 else f$url,
                            targetFile = if (useGADM) theFile else f$targetFile,
                            dlFun = if (useGADM) getDataFn else NULL,
                            name = if (useGADM) "GADM" else NULL,
                            country = if (useGADM) "LUX" else NULL,
                            level = if (useGADM) 0 else NULL,
                            path = if (useGADM) tmpdir else NULL,
                            destinationPath = tmpdir1)
      )
    )
    expect_true(sum(grepl(paste0(hardlinkMessagePrefixForGrep, ":\n", file.path(tmpdir1, theFile)), mess1)) == 1)
    expect_true(sum(grepl(paste0("",whPointsToMessForGrep,"\n", file.path(tmpdir, theFile)), mess1)) == 1)
    expect_true(sum(basename(dir(file.path(tmpdir), recursive = TRUE)) %in% theFile) == 3)

  }
  ## Try download to inputPath, intercepting the destination, creating a link
  testOnExit(testInitOut)
  testInit("terra",
                          opts = list("reproducible.inputPaths" = NULL,
                                      "reproducible.inputPathsRecursive" = FALSE))
  options("reproducible.inputPaths" = tmpdir)
  tmpdir2 <- file.path(tmpdir, rndstr(1,5))
  noisyOutput <- capture.output(
    noisyOutput <- capture.output(type = "message", {
      mess1 <- capture_messages(
        test1 <- prepInputs(url = if (!useGADM) url2 else f$url,
                            targetFile = if (useGADM) theFile else f$targetFile,
                            dlFun = if (useGADM) getDataFn else NULL,
                            name = if (useGADM) "GADM" else NULL,
                            country = if (useGADM) "LUX" else NULL,
                            level = if (useGADM) 0 else NULL,
                            path = if (useGADM) tmpdir else NULL,
                            destinationPath = tmpdir2)
      )
    })
  )

  # Must remove the link that happens during downloading to a .tempPath
  test10 <- grep(hardlinkMessagePrefixForGrep, mess1, value = TRUE)
  test10 <- grep(tmpdir2, test10, invert = TRUE, value = TRUE)
  expect_true(length(test10) == (1 - useGADM)) #

  # Have file in inputPath, not in destinationPath
  unlink(file.path(tmpdir2, theFile))
  expect_false(file.exists(file.path(tmpdir2, theFile))) # FALSE -- confirm previous line
  expect_true(file.exists(file.path(tmpdir, theFile))) # TRUE b/c is in getOption('reproducible.inputPaths')
  tmpdir2 <- file.path(tmpdir, rndstr(1, 5))
  noisyOutput <- capture.output(
    mess1 <- capture_messages(
      test1 <- prepInputs(url = if (!useGADM) url2 else f$url,
                          targetFile = if (useGADM) theFile else f$targetFile,
                          dlFun = if (useGADM) getDataFn else NULL,
                          name = if (useGADM) "GADM" else NULL,
                          country = if (useGADM) "LUX" else NULL,
                          level = if (useGADM) 0 else NULL,
                          path = if (useGADM) tmpdir else NULL,
                          destinationPath = tmpdir2)
    )
  )
  expect_true(sum(grepl(hardlinkMessagePrefixForGrep, mess1)) == 1) # used a linked version
  expect_true(sum(grepl(paste0("Hardlinked.*",basename(tmpdir2)), mess1)) == 1) # it is now in tmpdir2, i.e., the destinationPath

  # Have file in destinationPath, not in inputPath
  unlink(file.path(tmpdir, theFile))
  expect_false(file.exists(file.path(tmpdir, theFile))) # FALSE -- confirm previous line
  expect_true(file.exists(file.path(tmpdir2, theFile))) # TRUE b/c is in getOption('reproducible.inputPaths')
  noisyOutput <- capture.output(
    mess1 <- capture_messages(
      test1 <- prepInputs(url = if (!useGADM) url2 else f$url,
                          targetFile = if (useGADM) theFile else f$targetFile,
                          dlFun = if (useGADM) getDataFn else NULL,
                          name = if (useGADM) "GADM" else NULL,
                          country = if (useGADM) "LUX" else NULL,
                          level = if (useGADM) 0 else NULL,
                          path = if (useGADM) tmpdir else NULL,
                          destinationPath = tmpdir2)
    )
  )
  # expect_true(sum(grepl(hardlinkMessagePrefixForGrep, mess1)) == 1) # used a linked version
  # expect_true(sum(grepl(paste0("Hardlinked.*",basename(tmpdir2)), mess1)) == 1) # it is now in tmpdir2, i.e., the destinationPath

  ## Try with inputPaths == destinationPath
  unlink(file.path(tmpdir, theFile))
  unlink(file.path(tmpdir2, theFile))
  expect_false(file.exists(file.path(tmpdir, theFile))) # FALSE -- confirm previous line
  expect_false(file.exists(file.path(tmpdir2, theFile))) # TRUE b/c is in getOption('reproducible.inputPaths')
  options("reproducible.inputPaths" = tmpdir)
  noisyOutput <- capture.output(
    noisyOutput <- capture.output(type = "message", {
      mess1 <- capture_messages(
        test1 <- prepInputs(url = if (!useGADM) url2 else f$url,
                            targetFile = if (useGADM) theFile else f$targetFile,
                            dlFun = if (useGADM) getDataFn else NULL,
                            name = if (useGADM) "GADM" else NULL,
                            country = if (useGADM) "LUX" else NULL,
                            level = if (useGADM) 0 else NULL,
                            path = if (useGADM) tmpdir else NULL,
                            destinationPath = tmpdir)
      )
    })
  )
  objType <- if (useGADM) vectorType() else rasterType()
  expect_true(is(test1, objType) || is(test1, "SpatVector"))
  test11 <- grep(hardlinkMessagePrefixForGrep, mess1, value = TRUE)
  test11 <- grep(tmpdir, test11, invert = TRUE)
  expect_true(length(test11) == 0) # no link made b/c identical dir
  expect_true(sum(grepl(paste0("Hardlinked.*",basename(tmpdir2)), mess1)) == 0) # no link made b/c identical dir
})

test_that("writeOutputs saves factor rasters with .grd class to preserve levels", {
  skip_on_cran()

  testInit("terra", opts = list("reproducible.overwrite" = TRUE,
                                               "reproducible.inputPaths" = NULL),
                          needGoogleDriveAuth = TRUE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  a <- terra::rast(terra::ext(0, 2, 0, 2), res = 1, vals = c(1, 1, 2, 2))
  levels(a) <- data.frame(ID = 1:2, Factor = c("This", "That"))
  tifTmp <- tempfile(tmpdir = tmpdir, fileext = ".tif")
  file.create(tifTmp)
  tifTmp <- normPath(tifTmp)

  b1 <- suppressWarnings(terra::writeRaster(a, filename = tifTmp, overwrite = TRUE)) # the GDAL>6 issue
  b1a <- writeOutputs(a, filename2 = tifTmp)
  expect_false(identical(b1, b1a))
  expect_true(all.equal(b1[], b1a[]))

  expect_true(identical(normPath(Filenames(b1)), normPath(tifTmp)))
})

test_that("rasters aren't properly resampled", {
  skip_on_cran()

  testInit("terra", opts = list("reproducible.overwrite" = TRUE,
                                               "reproducible.inputPaths" = NULL),
                          needGoogleDriveAuth = TRUE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  a <- terra::rast(terra::ext(0, 20, 0, 20), res = 2, vals = as.integer(1:100*4))
  b <- terra::rast(terra::ext(0, 30, 0, 30), res = c(3,3), vals = 1L:100L)
  crs(a) <- crsToUse
  crs(b) <- crsToUse

  tiftemp1 <- normPath(tempfile(tmpdir = tmpdir, fileext = ".tif"))
  tiftemp2 <- normPath(tempfile(tmpdir = tmpdir, fileext = ".tif"))

  suppressWarnings({
    a <- terra::writeRaster(a, filename = tiftemp1, datatype = "INT2U")
    b <- terra::writeRaster(b, filename = tiftemp2, datatype = "INT2U")
  }) ## TODO: temporary GDAL>6


  # Test bilinear --> but keeps integer if it is integer
  suppressWarnings(
    out2 <- prepInputs(targetFile = tiftemp1, rasterToMatch = terra::rast(tiftemp2),
                       destinationPath = dirname(tiftemp1), method = "bilinear",
                       datatype = "INT2S",
                       filename2 = tempfile(tmpdir = tmpdir, fileext = ".tif"))
  ) # about "raster layer has integer values"

  if (getRversion() >= "4.1" || !isWindows())  {
    expect_true(dataType2(out2) %in% c("INT2S")) # because of "bilinear", it can become negative

    rrr1 <- terra::rast(terra::ext(0, 20, 0, 20), res = 1, vals = runif(400, 0, 1))
    terra::crs(rrr1) <- crsToUse
    tiftemp3 <- tempfile(tmpdir = tmpdir, fileext = ".tif")
    tiftemp4 <- tempfile(tmpdir = tmpdir, fileext = ".tif")
    suppressWarningsSpecific(terra::writeRaster(rrr1, filename = tiftemp3), proj6Warn)

    out3 <- prepInputs(targetFile = tiftemp3, rasterToMatch = terra::rast(tiftemp2),
                       destinationPath = dirname(tiftemp3),
                       filename2 = tempfile(tmpdir = tmpdir, fileext = ".tif"))
    expect_true(dataType2(out3) == "FLT4S")

    # Test for raster::stack
    rasStack <- c(terra::rast(tiftemp3), terra::rast(tiftemp3))
    rasStack[] <- rasStack[]
    rasStack[131][1] <- 1.5
    tiftemp4 <- tempfile(tmpdir = tmpdir, fileext = ".tif")

    rasStack <- terra::writeRaster(rasStack, filename = tiftemp4)
    rm(rasStack)
    out3 <- prepInputs(targetFile = tiftemp4, rasterToMatch = terra::rast(tiftemp2),
                       destinationPath = dirname(tiftemp3),
                       filename2 = tempfile(tmpdir = tmpdir, fileext = ".tif"))
    expect_true(is(out3, rasterType()))
    expect_true(identical(length(Filenames(out3)), 1L))

    if (.requireNamespace("raster")) {
      rasterStackFn <- "raster::stack"
      out4 <- prepInputs(targetFile = tiftemp4, rasterToMatch = terra::rast(tiftemp2),
                         destinationPath = dirname(tiftemp3),
                         fun = rasterStackFn,
                         filename2 = c(tempfile(tmpdir = tmpdir, fileext = ".grd"),
                                       tempfile(tmpdir = tmpdir, fileext = ".grd"))
      )
      expect_true(is(out4, rasterType(nlayers = nlayers2(out4), rasterRead = rasterStackFn)))
      expect_true(identical(length(Filenames(out4, allowMultiple = TRUE)), 4L))


      # Test for raster::stack with 3 layers, different types of writeRaster file ext
      rasStack <- c(terra::rast(tiftemp3), terra::rast(tiftemp3), terra::rast(tiftemp3))
      rasStack[] <- rasStack[]
      rasStack[131][1] <- 1.5
      rasStack[131][2] <- 2.5
      tiftemp5 <- tempfile(tmpdir = tmpdir, fileext = ".tif")

      rasStack <- writeRaster(rasStack, filename = tiftemp5)
      rm(rasStack)
      out5 <- prepInputs(targetFile = tiftemp5, rasterToMatch = terra::rast(tiftemp2),
                         destinationPath = dirname(tiftemp3),
                         fun = rasterStackFn,
                         filename2 = c(tempfile(tmpdir = tmpdir, fileext = ".grd"),
                                       tempfile(tmpdir = tmpdir, fileext = ".grd"),
                                       tempfile(tmpdir = tmpdir, fileext = ".tif")
                         ))
      expect_true(is(out5, "RasterStack"))
      expect_true(identical(length(Filenames(out5, allowMultiple = TRUE)), 5L))


      out4 <- prepInputs(targetFile = tiftemp4, rasterToMatch = terra::rast(tiftemp2),
                         destinationPath = dirname(tiftemp3),
                         fun = rasterStackFn,
                         filename2 = c(tempfile(tmpdir = tmpdir, fileext = ".grd"),
                                       tempfile(tmpdir = tmpdir, fileext = ".grd")))
      expect_true(is(out4, rasterType(nlayers2(out4), rasterStackFn)))
      expect_true(identical(length(Filenames(out4)), 4L))
    }

  }
})

