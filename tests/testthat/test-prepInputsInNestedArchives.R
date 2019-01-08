test_that("prepInputs in a simple one double nested zip file, passing only destinationPath and url", {
  skip_on_cran()
  testInitOut <- testInit("raster", needGoogle = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  url <- "https://github.com/tati-micheletti/host/raw/master/data/rasterNested.zip"
  testZip <- reproducible::prepInputs(url = url, destinationPath = tempdir())
  expect_true(exists("testZip"))
})

test_that("prepInputs in a simple one double nested zip file, passing targetFile", {
  skip_on_cran()
  testInitOut <- testInit("raster", needGoogle = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  url <- "https://github.com/tati-micheletti/host/raw/master/data/rasterNested.zip"
  testZip2 <- reproducible::prepInputs(url = url,
                             targetFile = "rasterTest.tif",
                             destinationPath = tempdir())
  expect_true(exists("testZip2"))
})

test_that("prepInputs in a two files double nested zip file, passing only destinationPath and url", {
  skip_on_cran()
  testInitOut <- testInit("raster", needGoogle = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  url <- "https://github.com/tati-micheletti/host/raw/master/data/multiFilesOutter.zip"
  testZip3 <- reproducible::prepInputs(url = url, destinationPath = tempdir())
  expect_true(exists("testZip3"))
})

test_that("prepInputs in a two files double nested zip file, passing targetFile", {
  skip_on_cran()
  testInitOut <- testInit("raster", needGoogle = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  url <- "https://github.com/tati-micheletti/host/raw/master/data/multiFilesOutter.zip"
  testZip4 <- reproducible::prepInputs(url = url,
                                       targetFile = "rasterTest.tif",
                                       destinationPath = tempdir())
  expect_true(exists("testZip4"))
})

test_that(
  paste0(
    "prepInputs in a two files double nested zip file, with the wanted file in",
    "the second layer, and a shapefile in the first, not specifying the targetFile"
  ), {
  skip_on_cran()
  testInitOut <-
    testInit("raster", needGoogle = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  # Gets the first file it finds, the shapefile; warns the user about it
  url <- "https://github.com/tati-micheletti/host/raw/master/data/multiFilesMultiLevels.zip"
  testZip6 <- reproducible::prepInputs(url = url, destinationPath = tempdir())
  expect_true(exists("testZip6"))
  expect_is(testZip6, "SpatialPolygonsDataFrame")
})

test_that(
  paste0("prepInputs in a two files double nested zip file, with the wanted file in",
         "the second layer, and a shapefile in the first, specifying the targetFile"), {
  skip_on_cran()
  testInitOut <- testInit("raster", needGoogle = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  url <- "https://github.com/tati-micheletti/host/raw/master/data/multiFilesMultiLevels.zip"
  testZip7 <- reproducible::prepInputs(url = url,
                                       targetFile = "rasterTest.tif",
                                       destinationPath = tempdir())
  expect_true(exists("testZip7"))
  expect_is(testZip7, "RasterLayer")
})

test_that(paste("prepInputs in a two files double nested rar file,",
                "with the wanted file in the second layer, not specifying the targetFile"), {
 skip_on_cran()
  testInitOut <- testInit("raster", needGoogle = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  hasUnrar <- .unrarExists()
  if (is.null(hasUnrar)) {
    expect_error(testRar <- reproducible::prepInputs(
      url = "https://github.com/tati-micheletti/host/raw/master/data/nestedRarTxtFiles.rar",
      destinationPath = tempdir()))
  } else {
    testRar <- reproducible::prepInputs(
      url = "https://github.com/tati-micheletti/host/raw/master/data/nestedRarTxtFiles.rar",
      destinationPath = tempdir())
    expect_true(exists("testRar"))
    expect_is(testRar, "RasterLayer")
  }
})

test_that(
  paste0(
    "prepInputs in a two files double nested rar file, with the wanted file in",
    "the second layer, specifying the targetFile"), {
  skip_on_cran()
  testInitOut <- testInit("raster", needGoogle = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  hasUnrar <- .unrarExists()
  url <- "https://github.com/tati-micheletti/host/raw/master/data/nestedRarTxtFiles.rar"
  if (is.null(hasUnrar)) {
    expect_error(testRar2 <- reproducible::prepInputs(url = url,
                                                      targetFile = "rasterTOtestRAR.tif",
                                                      destinationPath = tempdir()))
  } else {
    testRar2 <- reproducible::prepInputs(url = url,
                                         targetFile = "rasterTOtestRAR.tif",
                                         destinationPath = tempdir())
    expect_true(exists("testRar2"))
    expect_is(testRar2, "RasterLayer")
  }
})

test_that(
  paste0(
    "prepInputs in a two files double nested rar file, with the wanted file in",
    "the second layer, not specifying the targetFile, passing the main archive"),
  {
    skip_on_cran()
    testInitOut <- testInit("raster", needGoogle = FALSE)
    on.exit({
      testOnExit(testInitOut)
    }, add = TRUE)

    hasUnrar <- .unrarExists()
    url <- "https://github.com/tati-micheletti/host/raw/master/data/nestedRarTxtFiles.rar"
    if (is.null(hasUnrar)) {
      expect_error(testRar3 <- reproducible::prepInputs(url = url,
                                                        archive = "nestedRarTxtFiles.rar",
                                                        targetFile = "rasterTOtestRAR.tif",
                                                        destinationPath = tempdir()))
    } else {
      testRar3 <- reproducible::prepInputs(url = url,
                                           archive = "nestedRarTxtFiles.rar",
                                           targetFile = "rasterTOtestRAR.tif",
                                           destinationPath = tempdir())
      expect_true(exists("testRar3"))
      expect_is(testRar3, "RasterLayer")
    }
})

test_that("prepInputs works with nested rar file inside internal rar folder", {
  skip_on_cran()
  skip_on_travis() # temporary. Test passing locally
  skip_on_appveyor() # temporary. Test passing locally
  #browser()
  testInitOut <- testInit("raster", needGoogle = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  hasUnrar <- .unrarExists()
  url <- "https://github.com/tati-micheletti/host/raw/master/data/testRasterNested.rar"
  if (is.null(hasUnrar)) {
    expect_error(testRar4 <- reproducible::prepInputs(url = url,
                                                      targetFile = "rasterTest.tif",
                                                      destinationPath = tempdir(),
                                                      useCache = FALSE))
  } else {
    testRar4 <- reproducible::prepInputs(url = url,
                                         targetFile = "rasterTest.tif",
                                         destinationPath = tempdir(),
                                         useCache = FALSE)
    expect_true(exists("testRar4"))
    expect_is(testRar4, "RasterLayer")
  }
})
