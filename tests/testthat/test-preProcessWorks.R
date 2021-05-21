test_that("preProcess works for .tar files", {
  skip_on_cran()
  testInitOut <- testInit("raster", needGoogle = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  url <- theRasterTestTar
  noisyOutput <- capture.output(
    ras <- reproducible::preProcess(url = url, destinationPath = tmpdir)
  )
  testthat::expect_is(object = ras, class = "list")
  testthat::expect_true(file.exists(ras$targetFilePath))
})

test_that("preProcess works for .zip when provided only url and destinationPath", {
  skip_on_cran()
  testInitOut <- testInit("raster", needGoogle = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  url <- theRasterTestZip
  noisyOutput <- capture.output(
    ras <- reproducible::preProcess(url = url, destinationPath = tmpdir)
  )
  testthat::expect_is(object = ras, class = "list")
  testthat::expect_true(file.exists(ras$targetFilePath))
})

test_that("preProcess works with only url", {
  skip_on_cran()
  testInitOut <- testInit("raster", needGoogle = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  url <- theRasterTestZip
  noisyOutput <- capture.output(
    ras <- reproducible::preProcess(url = url)
  )
  testthat::expect_is(object = ras, class = "list")
  testthat::expect_true(file.exists(ras$targetFilePath))
})

test_that("preProcess works when provides only archive", {
  skip_on_cran()
  testInitOut <- testInit("raster", needGoogle = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  url <- theRasterTestZip
  noisyOutput <- capture.output(
    pre <- reproducible::preProcess(url = url, destinationPath = tmpdir)
  )
  testthat::expect_is(object = pre, class = "list")
  noisyOutput <- capture.output(
    ras <- reproducible::preProcess(archive = file.path(pre$destinationPath,
                                                        list.files(pre$destinationPath)[
                                                          grepl(x = list.files(pre$destinationPath),
                                                                pattern = ".zip|.tar")]))
  )
  testthat::expect_is(object = ras, class = "list")
})

test_that("preProcess works when provides archive and destinationPath", {
  skip_on_cran()
  testInitOut <- testInit("raster", needGoogle = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  url <- theRasterTestZip
  noisyOutput <- capture.output(
    pre <- reproducible::preProcess(url = url, destinationPath = tmpdir)
  )
  testthat::expect_is(object = pre, class = "list")
  noisyOutput <- capture.output(
    ras <- reproducible::preProcess(archive = file.path(pre$destinationPath,
                                                        list.files(pre$destinationPath)[
                                                          grepl(x = list.files(pre$destinationPath),
                                                                pattern = ".zip|.tar")]),
                                    destinationPath = tmpdir)
  )
  testthat::expect_is(object = ras, class = "list")
})

test_that("preProcess works when provides only targetFile", {
  skip_on_cran()
  testInitOut <- testInit("raster", needGoogle = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  url <- theRasterTestZip
  noisyOutput <- capture.output(
    pre <- reproducible::preProcess(url = url, destinationPath = tmpdir)
  )
  testthat::expect_is(object = pre, class = "list")
  noisyOutput <- capture.output(
    ras <- reproducible::preProcess(targetFile = pre$targetFilePath)
  )
  testthat::expect_is(object = ras, class = "list")
})

test_that("preProcess works when provides targetfile and destinationPath", {
  skip_on_cran()
  testInitOut <- testInit("raster", needGoogle = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  url <- theRasterTestZip
  noisyOutput <- capture.output(
    pre <- reproducible::preProcess(url = url, destinationPath = tmpdir)
  )
  testthat::expect_is(object = pre, class = "list")
  noisyOutput <- capture.output(
    ras <- reproducible::preProcess(targetFile = pre$targetFilePath,
                                    destinationPath = tmpdir)
  )
  testthat::expect_is(object = ras, class = "list")
})

test_that("preProcess works when provides url, archive, targetfile and destinationPath", {
  skip_on_cran()
  testInitOut <- testInit("raster", needGoogle = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  url <- theRasterTestZip
  noisyOutput <- capture.output(
    ras <- reproducible::preProcess(url = url, targetFile = theRasterTestFilename(suff = "tif"),
                                    archive = theRasterTestFilename(suff = "zip"),
                                    destinationPath = tmpdir)
  )
  testthat::expect_is(object = ras, class = "list")
  testthat::expect_true(file.exists(ras$targetFilePath))
})

test_that("preProcess works when provides url, targetfile and destinationPath", {
  skip_on_cran()
  testInitOut <- testInit("raster", needGoogle = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  url <- theRasterTestZip
  noisyOutput <- capture.output(
    ras <- reproducible::preProcess(url = url, targetFile = theRasterTestFilename(suff = "tif"),
                                    destinationPath = tmpdir)
  )
  testthat::expect_is(object = ras, class = "list")
  testthat::expect_true(file.exists(ras$targetFilePath))
})

test_that("preProcess works when provides url and destinationPath for a .rar file", {
  skip_on_cran()
  testInitOut <- testInit("raster", needGoogle = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  extractSystemCallPath <- try(.testForArchiveExtract(), silent = TRUE)
  url <- theRasterTestRar

  if (!is(extractSystemCallPath, "try-error"))
    if (is.null(extractSystemCallPath)) {
      noisyOutput <- capture.output(
        expect_error({
          ras <- reproducible::preProcess(url = url, destinationPath = tmpdir)
        })
      )
    } else {
      noisyOutput <- capture.output(
        ras <- reproducible::preProcess(url = url, destinationPath = tmpdir)
      )
      testthat::expect_is(object = ras, class = "list")
      testthat::expect_true(file.exists(ras$targetFilePath))
    }
})

test_that("preProcess works when provides url, targetfile and destinationPath for a .rar file", {
  skip_on_cran()
  testInitOut <- testInit("raster", needGoogle = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  extractSystemCallPath <- try(.testForArchiveExtract(), silent = TRUE)
  url <- theRasterTestRar

  if (!is(extractSystemCallPath, "try-error"))
    if (is.null(extractSystemCallPath)) {
      noisyOutput <- capture.output(
        expect_error({
          ras <- reproducible::preProcess(url = url, targetFile = theRasterTestFilename(suff = "tif"),
                                          destinationPath = tmpdir)
        })
      )
    } else {
      wd <- getwd()
      noisyOutput <- capture.output(
        ras <- reproducible::preProcess(url = url, targetFile = theRasterTestFilename(suff = "tif"),
                                        destinationPath = tmpdir)
      )
      testthat::expect_is(object = ras, class = "list")
      testthat::expect_true(file.exists(ras$targetFilePath))
      expect_equal(wd, getwd()) # Test that working directory is restored after unrar call
    }
})

test_that("preProcess works when provides url, archive and destinationPath for a .rar file", {
  skip_on_cran()
  testInitOut <- testInit("raster", needGoogle = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  extractSystemCallPath <- try(.testForArchiveExtract(), silent = TRUE)
  url <- theRasterTestRar
  rasterTestRarFilename <- theRasterTestFilename(suff = "rar")

  if (!is(extractSystemCallPath, "try-error"))
    if (is.null(extractSystemCallPath)) {
      noisyOutput <- capture.output(
        expect_error({
          ras <- reproducible::preProcess(url = url, archive = rasterTestRarFilename, destinationPath = tmpdir)
        })
      )
    } else {
      noisyOutput <- capture.output(
        ras <- reproducible::preProcess(url = url, archive = rasterTestRarFilename, destinationPath = tmpdir)
      )
      testthat::expect_is(object = ras, class = "list")
      testthat::expect_true(file.exists(ras$targetFilePath))
    }
})

test_that("preProcess works, but gives a warning when supplying cacheTags", {
  skip_on_cran()
  testInitOut <- testInit("raster", needGoogle = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  url <- theRasterTestZip
  noisyOutput <- capture.output(
    testthat::expect_message({
      ras <- reproducible::preProcess(url = url, destinationPath = tmpdir, cacheTags = "objectName::ras")
    })
  )
  testthat::expect_is(object = ras, class = "list")
  testthat::expect_true(file.exists(ras$targetFilePath))
})

test_that("preProcess works, but gives a warning when supplying postProcessedFilename", {
  skip_on_cran()
  testInitOut <- testInit("raster", needGoogle = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  url <- theRasterTestZip
  noisyOutput <- capture.output(
    testthat::expect_message({
      ras <- reproducible::preProcess(url = url, destinationPath = tmpdir,
                                      postProcessedFilename = "ras.tif")
    })
  )
  testthat::expect_is(object = ras, class = "list")
  testthat::expect_true(file.exists(ras$targetFilePath))
})

test_that("preProcess works, but gives a warning when supplying rasterInterpMethod", {
  skip_on_cran()
  testInitOut <- testInit("raster", needGoogle = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  url <- theRasterTestZip
  noisyOutput <- capture.output(
    testthat::expect_message({
      ras <- reproducible::preProcess(url = url, destinationPath = tmpdir, rasterInterpMethod = "ngb")
    })
  )
  testthat::expect_is(object = ras, class = "list")
  testthat::expect_true(file.exists(ras$targetFilePath))
})

test_that("preProcess works, but gives a warning when supplying rasterDatatype", {
  skip_on_cran()
  testInitOut <- testInit("raster", needGoogle = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  url <- theRasterTestZip
  noisyOutput <- capture.output(
    testthat::expect_message({
      ras <- reproducible::preProcess(url = url, destinationPath = tmpdir, rasterDatatype = "INT1U")
    })
  )
  testthat::expect_is(object = ras, class = "list")
  testthat::expect_true(file.exists(ras$targetFilePath))
})

test_that("preProcess works, but gives a warning when supplying pkg", {
  skip_on_cran()
  testInitOut <- testInit("raster", needGoogle = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  url <- theRasterTestZip
  noisyOutput <- capture.output(
    testthat::expect_message({
      ras <- reproducible::preProcess(url = url, destinationPath = tmpdir, pkg = "utils", fun = "unzip")
    })
  )
  testthat::expect_is(object = ras, class = "list")
  testthat::expect_true(file.exists(ras$targetFilePath))
})

test_that("message when files from archive are already present", {
  skip_on_cran()
  testInitOut <- testInit("raster", needGoogle = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  url <- "https://github.com/tati-micheletti/host/raw/master/data/rasterTest.zip"
  noisyOutput <- capture.output(
    ccc <- testthat::capture_output(
      ras <- reproducible::preProcess(url = url,
                                      targetFile = "rasterTest.tif",
                                      destinationPath = tmpdir)
    )
  )
  noisyOutput <- capture.output(
    testthat::expect_message({
      ras <- reproducible::preProcess(archive = "rasterTest.zip", destinationPath = tmpdir)
    })
  )
})

test_that("message when file is a shapefile", {
  skip_on_cran()
  testInitOut <- testInit("raster", needGoogle = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  url <- "https://github.com/tati-micheletti/host/raw/master/data/shapefileTest.zip"
  noisyOutput <- capture.output(
    ccc <- testthat::capture_output(
      testthat::expect_message({
        ras <- reproducible::preProcess(url = url, destinationPath = tmpdir)
      })
    )
  )
})

test_that("message when doesn't know the targetFile extension", {
  skip_on_cran()
  testInitOut <- testInit("raster", needGoogle = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  url <- "https://github.com/tati-micheletti/host/raw/master/data/unknownTargetFile.zip"
  noisyOutput <- capture.output(
    ccc <- testthat::capture_output(
      testthat::expect_message(regexp = "does not unambiguously specify", {
        ras <- reproducible::preProcess(url = url, destinationPath = tmpdir)
      })
    )
  )
})

test_that("When supplying two files without archive, when archive and files have different names", {
  skip_on_cran()
  testInitOut <- testInit("raster", needGoogle = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  url <- "https://github.com/tati-micheletti/host/raw/master/data/twoKnownFiles.zip"
  noisyOutput <- capture.output(
    ccc <- testthat::capture_output(
      testthat::expect_error({
        ras <- reproducible::preProcess(url = url,
                                        targetFile = c("rasterTest.tif", "shapefileTest.shp"),
                                        destinationPath = tmpdir)
      })
    )
  )
})

test_that("message when archive has two known files (raster and shapefile)", {
  skip_on_cran()
  testInitOut <- testInit("raster", needGoogle = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  url <- "https://github.com/tati-micheletti/host/raw/master/data/knownFiles.zip"
  noisyOutput <- capture.output(
    ccc <- testthat::capture_output(
      testthat::expect_error({
        ras <- reproducible::preProcess(url = url,
                                        archive = "knownFiles.zip",
                                        targetFile = c("knownFiles.tif", "knownFiles.shp"),
                                        destinationPath = tmpdir)
      })
    )
  )
})

test_that("message when extracting a file that is already present", {
  skip_on_cran()
  testInitOut <- testInit("raster", needGoogle = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  url1 <- "https://github.com/tati-micheletti/host/raw/master/data/rasterTest.zip"
  noisyOutput <- capture.output({
    ccc <- testthat::capture_output({
      ras <- reproducible::preProcess(url = url1,
                                      targetFile = "rasterTest.tif",
                                      destinationPath = tmpdir)
    })
  })
  url2 <- "https://github.com/tati-micheletti/host/raw/master/data/shapefileTest.zip"
  noisyOutput <- capture.output({
    ccc <- testthat::capture_output({
      shp <- reproducible::preProcess(url = url2, destinationPath = tmpdir)
    })
  })
  url3 <- "https://github.com/tati-micheletti/host/raw/master/data/knownFiles.zip"
  noisyOutput <- capture.output({
    ccc <- testthat::capture_output({
      testthat::expect_message({
        fl <- reproducible::preProcess(url = url3, destinationPath = tmpdir)
      })
    })
  })
})

test_that("Test to fix issue #101 prepInputs on raster from disk", {
  if (interactive()) {
    testInitOut <- testInit("raster", needGoogle = TRUE)
    on.exit({
      testOnExit(testInitOut)
    }, add = TRUE)
    smallRT <- prepInputs(url = "https://drive.google.com/open?id=1WhL-DxrByCbzAj8A7eRx3Y1FVujtGmtN")
    a <- raster::extent(smallRT)
    a <- raster::extend(a, -3.5e5) # make it small
    test <- raster(a, res = 250, vals = 1)
    crs(test) <- crs(smallRT)
    a <- postProcess(x = test, rasterToMatch = smallRT, maskWithRTM = TRUE)
    expect_true(is(a, "RasterLayer"))
  }
})

test_that("Test of using future and progress indicator for lrg files on Google Drive", {
  if (interactive()) {
    if (requireNamespace("future")) {
      testInitOut <- testInit(c("raster", "future"), needGoogle = TRUE, opts = list("reproducible.futurePlan" = "multiprocess"))
      on.exit({
        testOnExit(testInitOut)
      }, add = TRUE)
      #future::plan("multiprocess")
      noisyOutput <- capture.output(
        ccc <- testthat::capture_output(
          smallRT <- preProcess(url = "https://drive.google.com/open?id=1WhL-DxrByCbzAj8A7eRx3Y1FVujtGmtN")
        )
      )
      expect_true(is(smallRT, "list"))
    }
  }
})

