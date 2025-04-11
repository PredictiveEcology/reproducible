test_that("prepInputs in a simple one double nested zip file, passing only destinationPath and url", {
  skip_on_cran()
  testInit("terra", needInternet = TRUE)

  url <- "https://github.com/tati-micheletti/host/raw/master/data/rasterNested.zip"
  noisyOutput <- capture.output({
    testZip <- reproducible::prepInputs(url = url, destinationPath = tmpdir)
  })
  expect_true(exists("testZip"))
})

test_that("prepInputs in a simple one double nested zip file, passing targetFile", {
  skip_on_cran()
  testInit("terra", needInternet = TRUE)

  url <- "https://github.com/tati-micheletti/host/raw/master/data/rasterNested.zip"
  noisyOutput <- capture.output({
    testZip2 <- reproducible::prepInputs(
      url = url,
      targetFile = "rasterTest.tif",
      destinationPath = tmpdir
    )
  })
  expect_true(exists("testZip2"))
})

test_that("prepInputs in a two files double nested zip file, passing only destinationPath and url", {
  skip_on_cran()
  testInit("terra", needInternet = TRUE)

  url <- "https://github.com/tati-micheletti/host/raw/master/data/multiFilesOutter.zip"
  noisyOutput <- capture.output({
    testZip3 <- reproducible::prepInputs(url = url, destinationPath = tmpdir)
  })
  expect_true(exists("testZip3"))
})

test_that("prepInputs in a two files double nested zip file, passing targetFile", {
  skip_on_cran()
  testInit("terra", needInternet = TRUE)

  url <- "https://github.com/tati-micheletti/host/raw/master/data/multiFilesOutter.zip"
  noisyOutput <- capture.output({
    testZip4 <- reproducible::prepInputs(
      url = url,
      targetFile = "rasterTest.tif",
      destinationPath = tmpdir
    )
  })
  expect_true(exists("testZip4"))
})

test_that(paste0(
    "prepInputs in a two files double nested zip file, with the wanted file in",
    "the second layer, and a shapefile in the first, not specifying the targetFile"
  ),
  {
    skip_on_cran()
    testInit("terra", needInternet = TRUE)

    # Gets the first file it finds, the shapefile; warns the user about it
    url <- "https://github.com/tati-micheletti/host/raw/master/data/multiFilesMultiLevels.zip"
    # The warning is about the .prj file missing, which is not relevant here -
    #   Capture it and do nothing with it
    noisyOutput <- capture.output({
      warn <- capture_warnings({
        testZip6 <- reproducible::prepInputs(url = url, destinationPath = tmpdir)
      })
    })
    expect_true(exists("testZip6"))
    expect_is(testZip6, vectorType())
  }
)

test_that(
  paste0(
    "prepInputs in a two files double nested zip file, with the wanted file in",
    "the second layer, and a shapefile in the first, specifying the targetFile"
  ),
  {
    skip_on_cran()
    testInit("terra", needInternet = TRUE)

    url <- "https://github.com/tati-micheletti/host/raw/master/data/multiFilesMultiLevels.zip"
    noisyOutput <- capture.output({
      testZip7 <- reproducible::prepInputs(
        url = url,
        targetFile = "rasterTest.tif",
        destinationPath = tmpdir
      )
    })
    expect_true(exists("testZip7"))
    expect_is(testZip7, rasterType())
  }
)

test_that(
  paste(
    "prepInputs in a two files double nested rar file,",
    "with the wanted file in the second layer, not specifying the targetFile"
  ),
  {
    skip_on_cran()
    skip_if(isMac() && isCI())

    testInit("terra", needInternet = TRUE)

    # extractSystemCallPath <- .archiveExtractBinary()
    #
    # if (is.null(extractSystemCallPath)) {
    #   noisyOutput <- capture.output({
    #     warn <- capture_warnings({
    #       expect_error({
    #         testRar <- reproducible::prepInputs(
    #           url = "https://github.com/tati-micheletti/host/raw/master/data/nestedRarTxtFiles.rar",
    #           destinationPath = tmpdir
    #         )
    #       })
    #     })
    #   })
    # } else {

    if (isWindows() && getRversion() < "4.3")
      skip("archive pkg on Windows 4.2.3 fails on rar")
    noisyOutput <- capture.output({
      testRar <- reproducible::prepInputs(
        url = "https://github.com/tati-micheletti/host/raw/master/data/nestedRarTxtFiles.rar",
        destinationPath = tmpdir
      )
    })
    expect_true(exists("testRar"))
    expect_is(testRar, rasterType())
    # }
  }
)

test_that(
  paste0(
    "prepInputs in a two files double nested rar file, with the wanted file in",
    " the second layer, specifying the targetFile"
  ),
  {
    skip_on_cran()
    skip_if(isMac() && isCI())

    testInit("terra", needInternet = TRUE)
    url <- "https://github.com/tati-micheletti/host/raw/master/data/nestedRarTxtFiles.rar"

    # extractSystemCallPath <- .archiveExtractBinary()
    # if (is.null(extractSystemCallPath)) {
    #   noisyOutput <- capture.output(
    #     expect_error({
    #       testRar2 <- reproducible::prepInputs(
    #         url = url,
    #         targetFile = "rasterTOtestRAR.tif",
    #         destinationPath = tmpdir
    #       )
    #     })
    #   )
    # } else {
    if (isWindows() && getRversion() < "4.3")
      skip("archive pkg on Windows 4.2.3 fails on rar")
    noisyOutput <- capture.output({
      testRar2 <- reproducible::prepInputs(
        url = url,
        targetFile = "rasterTOtestRAR.tif",
        destinationPath = tmpdir
      )
    })
    expect_true(exists("testRar2"))
    expect_is(testRar2, rasterType())
    # }
  }
)

test_that(paste0(
  "prepInputs in a two files double nested rar file, with the wanted file in",
  "the second layer, not specifying the targetFile, passing the main archive"
), {
  skip_on_cran()
  skip_if(isMac() && isCI())
  testInit("terra", needInternet = TRUE)

  url <- "https://github.com/tati-micheletti/host/raw/master/data/nestedRarTxtFiles.rar"

  # extractSystemCallPath <- .archiveExtractBinary()
  # if (is.null(extractSystemCallPath)) {
  #   noisyOutput <- capture.output(
  #     expect_error({
  #       testRar3 <- reproducible::prepInputs(
  #         url = url,
  #         archive = "nestedRarTxtFiles.rar",
  #         targetFile = "rasterTOtestRAR.tif",
  #         destinationPath = tmpdir
  #       )
  #     })
  #   )
  # } else {
  if (isWindows() && getRversion() < "4.3")
    skip("archive pkg on Windows 4.2.3 fails on rar")

  noisyOutput <- capture.output({
    testRar3 <- reproducible::prepInputs(
      url = url,
      archive = "nestedRarTxtFiles.rar",
      targetFile = "rasterTOtestRAR.tif",
      destinationPath = tmpdir
    )
  })
  expect_true(exists("testRar3"))
  expect_is(testRar3, rasterType())
  # }
})

test_that("prepInputs works with nested rar file inside internal rar folder", {
  skip_on_cran()
  skip_on_ci() ## TODO: skip for now b/c need additional unrar tools
  skip_on_os("mac") ## TODO: deal with unrar for macOS #266

  testInit("terra", needInternet = TRUE)

  url <- "https://github.com/tati-micheletti/host/raw/master/data/testRasterNested.rar"
  # extractSystemCallPath <- .archiveExtractBinary()
  # if (is.null(extractSystemCallPath)) {
  #   noisyOutput <- capture.output(
  #     expect_error({
  #       testRar4 <- reproducible::prepInputs(
  #         url = url,
  #         targetFile = "rasterTest.tif",
  #         destinationPath = tmpdir,
  #         useCache = FALSE
  #       )
  #     })
  #   )
  # } else {
  noisyOutput <- capture.output({
    testRar4 <- reproducible::prepInputs(
      url = url,
      targetFile = "rasterTest.tif",
      destinationPath = tmpdir,
      useCache = FALSE
    )
  })
  expect_true(exists("testRar4"))
  expect_is(testRar4, rasterType())
  # }
})
