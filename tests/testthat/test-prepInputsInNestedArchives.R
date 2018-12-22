test_that("prepInputs in a simple one double nested zip file, passing only destinationPath and url",
          {
            skip_on_cran()
            testInitOut <- testInit("raster", needGoogle = FALSE)
            on.exit({
              testOnExit(testInitOut)
            }, add = TRUE)

            testZip <-
              reproducible::prepInputs(url = "https://github.com/tati-micheletti/host/raw/master/data/rasterNested.zip",
                                       destinationPath = tempdir())
            expect_true(exists("testZip"))
          })

test_that("prepInputs in a simple one double nested zip file, passing targetFile",
          {
            skip_on_cran()
            testInitOut <- testInit("raster", needGoogle = FALSE)
            on.exit({
              testOnExit(testInitOut)
            }, add = TRUE)

            testZip2 <-
              reproducible::prepInputs(url = "https://github.com/tati-micheletti/host/raw/master/data/rasterNested.zip",
                                       targetFile = "rasterTest.tif",
                                       destinationPath = tempdir())
            expect_true(exists("testZip2"))
          })

test_that("prepInputs in a two files double nested zip file, passing only destinationPath and url",
          {
            skip_on_cran()
            testInitOut <- testInit("raster", needGoogle = FALSE)
            on.exit({
              testOnExit(testInitOut)
            }, add = TRUE)

            testZip3 <-
              reproducible::prepInputs(url = "https://github.com/tati-micheletti/host/raw/master/data/multiFilesOutter.zip",
                                       destinationPath = tempdir())
            expect_true(exists("testZip3"))
          })

test_that("prepInputs in a two files double nested zip file, passing targetFile",
          {
            skip_on_cran()
            testInitOut <- testInit("raster", needGoogle = FALSE)
            on.exit({
              testOnExit(testInitOut)
            }, add = TRUE)

            testZip4 <-
              reproducible::prepInputs(url = "https://github.com/tati-micheletti/host/raw/master/data/multiFilesOutter.zip",
                                       targetFile = "rasterTest.tif",
                                       destinationPath = tempdir())
            expect_true(exists("testZip4"))
          })

test_that(
  paste0(
    "prepInputs in a two files double nested zip file, with the wanted file in",
    "the second layer, and a shapefile in the first, not specifying the targetFile"
  ),
  {
    skip_on_cran()
    testInitOut <-
      testInit("raster", needGoogle = FALSE)
    on.exit({
      testOnExit(testInitOut)
    }, add = TRUE)

    # Gets the first file it finds, the shapefile; warns the user about it
    testZip6 <-
      reproducible::prepInputs(url = "https://github.com/tati-micheletti/host/raw/master/data/multiFilesMultiLevels.zip",
                               destinationPath = tempdir())
    expect_true(exists("testZip6"))
    expect_is(testZip6, "SpatialPolygonsDataFrame")
  }
)

test_that(
  paste0(
    "prepInputs in a two files double nested zip file, with the wanted file in",
    "the second layer, and a shapefile in the first, specifying the targetFile"), {
      skip_on_cran()
      testInitOut <-
        testInit("raster", needGoogle = FALSE)
      on.exit({
        testOnExit(testInitOut)
      }, add = TRUE)

      testZip7 <-
        reproducible::prepInputs(url = "https://github.com/tati-micheletti/host/raw/master/data/multiFilesMultiLevels.zip",
                                 targetFile = "rasterTest.tif",
                                 destinationPath = tempdir())
      expect_true(exists("testZip7"))
      expect_is(testZip7, "RasterLayer")
    }
  )

test_that(
  paste0(
    "prepInputs in a two files double nested rar file, with the wanted file in",
    "the second layer, not specifying the targetFile"), {
      skip_on_cran()
      testInitOut <-
        testInit("raster", needGoogle = FALSE)
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
  }
)

test_that(
  paste0(
    "prepInputs in a two files double nested rar file, with the wanted file in",
    "the second layer, specifying the targetFile"),
  {
    skip_on_cran()
    testInitOut <-
      testInit("raster", needGoogle = FALSE)
    on.exit({
      testOnExit(testInitOut)
    }, add = TRUE)

    hasUnrar <- .unrarExists()
    if (is.null(hasUnrar)) {
      expect_error(testRar2 <-
                     reproducible::prepInputs(url = "https://github.com/tati-micheletti/host/raw/master/data/nestedRarTxtFiles.rar",
                                              targetFile = "rasterTOtestRAR.tif" ,
                                              destinationPath = tempdir()))
    } else {
      testRar2 <-
        reproducible::prepInputs(url = "https://github.com/tati-micheletti/host/raw/master/data/nestedRarTxtFiles.rar",
                                 targetFile = "rasterTOtestRAR.tif" ,
                                 destinationPath = tempdir())
      expect_true(exists("testRar2"))
      expect_is(testRar2, "RasterLayer")
    }
  }
)

test_that(
  paste0(
    "prepInputs in a two files double nested rar file, with the wanted file in",
    "the second layer, not specifying the targetFile, passing the main archive"),
  {
    skip_on_cran()
    testInitOut <-
      testInit("raster", needGoogle = FALSE)
    on.exit({
      testOnExit(testInitOut)
    }, add = TRUE)

    hasUnrar <- .unrarExists()
    if (is.null(hasUnrar)) {
      expect_error(testRar3 <-
                         reproducible::prepInputs(
                           url = "https://github.com/tati-micheletti/host/raw/master/data/nestedRarTxtFiles.rar",
                           archive = "nestedRarTxtFiles.rar",
                           targetFile = "rasterTOtestRAR.tif",
                           destinationPath = tempdir()))
    } else {
      testRar3 <- reproducible::prepInputs(
          url = "https://github.com/tati-micheletti/host/raw/master/data/nestedRarTxtFiles.rar",
          archive = "nestedRarTxtFiles.rar",
          targetFile = "rasterTOtestRAR.tif",
          destinationPath = tempdir())
      expect_true(exists("testRar3"))
      expect_is(testRar3, "RasterLayer")
    }
  }
)
