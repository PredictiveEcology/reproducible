test_that("prepInputs works with NULL archive + file without extension, but originally a .zip", {
  skip_on_cran()
  testInit("terra", needInternet = TRUE)
  skip_on_transient_http({
    noisyOutput <- capture.output({
      testthat::expect_message({
        ras <- reproducible::prepInputs(
          url = "https://github.com/PredictiveEcology/reproducible/releases/download/v3.1.1/unknownExtension",
          alsoExtract = "similar",
          destinationPath = tempdir2(rndstr(1, 6))
        )
      })
    })
  })
  testthat::expect_is(object = ras, class = rasterType())
})

test_that("prepInputs WORKS if the file is not originally a .zip, but archive is provided (only extension matters)", {
  skip_on_cran()
  testInit("terra", needInternet = TRUE)
  skip_on_transient_http({
    noisyOutput <- capture.output(
      testthat::expect_message({
        ras <- reproducible::prepInputs(
          url = "https://github.com/PredictiveEcology/reproducible/releases/download/v3.1.1/unknownTAR",
          alsoExtract = "similar", archive = "unknownTAR.tar",
          destinationPath = tempdir2(rndstr(1, 6))
        )
      })
    )
  })
  testthat::expect_is(object = ras, class = rasterType())
})

test_that("prepInputs WORKS if passing archive .zip", {
  skip_on_cran()
  testInit("terra", needInternet = TRUE)
  clearCache(x = normalizePath(getOption("reproducible.cachePath"), winslash = "/"))
  skip_on_transient_http({
    noisyOutput <- capture.output(
      testthat::expect_message({
        ras <- reproducible::prepInputs(
          url = "https://github.com/PredictiveEcology/reproducible/releases/download/v3.1.1/unknownExtension",
          archive = "unknownExtension.zip",
          alsoExtract = "similar", destinationPath = tempdir2(rndstr(1, 6))
        )
      })
    )
  })
  testthat::expect_is(object = ras, class = rasterType())
})

test_that("prepInputs WORKS passing just targetFile that is NOT an archive", {
  skip_on_cran()
  testInit("terra", needInternet = TRUE)
  skip_on_transient_http({
    noisyOutput <- capture.output({
      testthat::expect_message({
        ras <- reproducible::prepInputs(
          url = "https://github.com/PredictiveEcology/reproducible/releases/download/v3.1.1/unknownTIF",
          alsoExtract = "similar", targetFile = "unknownTIF.tif",
          destinationPath = tempdir2(rndstr(1, 6))
        )
      })
    })
  })
  testthat::expect_is(object = ras, class = rasterType())
})

test_that("prepInputs WORKS passing archive + targetFile", {
  skip_on_cran()
  testInit("terra", needInternet = TRUE)
  clearCache(x = normalizePath(getOption("reproducible.cachePath"), winslash = "/"))
  skip_on_transient_http({
    noisyOutput <- capture.output({
      testthat::expect_message({
        ras <- reproducible::prepInputs(
          url = "https://github.com/PredictiveEcology/reproducible/releases/download/v3.1.1/unknownExtension",
          archive = "unknownExtension.zip", targetFile = "rasterTest.tif",
          alsoExtract = "similar",
          destinationPath = tempdir2(rndstr(1, 6))
        )
      })
    })
  })
  testthat::expect_is(object = ras, class = rasterType())
})
