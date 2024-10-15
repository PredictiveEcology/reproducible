test_that("preProcess fails if user provides non-existing file", {
  skip_on_cran()
  testInit("terra", opts = list(reproducible.inputPaths = NULL), verbose = 2)
  testthat::with_mock(
    `isInteractive` = function() {
      FALSE
    },
    {
      errMsg <- testthat::capture_error({
        co <- capture.output({
          co <- capture.output(
            type = "message", {
            reproducible::preProcess(
              url = "https://github.com/tati-micheletti/host/raw/master/data/rasterTest",
              destinationPath = tmpdir
            )
          })
        })
      })
    },
    .env = "reproducible"
  )
  expect_true(grepl("manual download", errMsg))
  expect_true(grepl("appendChecksumsTable", errMsg))

  optsOrig <- options(reproducible.interactiveOnDownloadFail = FALSE)
  co <- capture.output(type = "message", {
    co2 <- capture.output({
      errMsg <- testthat::capture_error({
        reproducible::preProcess(
          url = "https://github.com/tati-micheletti/host/raw/master/data/rasterTest",
          destinationPath = tmpdir
        )
      })
    })
  })

  expect_true(grepl("manual download", errMsg))
  expect_true(grepl("appendChecksumsTable", errMsg))
  options(optsOrig)

  testthat::with_mock(
    `isInteractive` = function() {
      TRUE
    },
    `.readline` = function(prompt) {
      "n"
    },
    {
      co <- capture.output({
        co <- capture.output(type = "message", {
          mess <- testthat::capture_messages({
            errMsg <- testthat::capture_error({
              reproducible::preProcess(
                url = "https://github.com/tati-micheletti/host/raw/master/data/rasterTest",
                destinationPath = tmpdir
              )
            })
          })
        })
      })
    },
    .env = "reproducible"
  )
  expect_true(sum(grepl("Download failed", errMsg)) == 1)

  optsOrig <- options("reproducible.interactiveOnDownloadFail" = TRUE)
  testthat::with_mock(
    `isInteractive` = function() {
      TRUE
    },
    `.readline` = function(prompt) {
      theFile <- file.path(tmpdir, "rasterTestAA")
      write.table(theFile, file = theFile)
      zipFilename <- file.path(tmpdir, "rasterTest")
      origDir <- setwd(dirname(theFile))
      on.exit(setwd(origDir), add = TRUE)
      zip(zipfile = zipFilename, files = basename2(theFile), flags = "-q")
      zipFilenameWithDotZip <- dir(tmpdir, pattern = "\\.zip", full.names = TRUE)
      file.rename(from = zipFilenameWithDotZip, to = zipFilename)
      "y"
    },
    {
      co <- capture.output(type = "message", {
        co <- capture.output({
          mess <- testthat::capture_messages({
            errMsg <- testthat::capture_error({
              reproducible::preProcess(
                url = "https://github.com/tati-micheletti/host/raw/master/data/rasterTest",
                destinationPath = tmpdir
              )
            })
          })
        })
      })
    },
    .env = "reproducible"
  )
  expect_true(sum(grepl("manual download", mess)) == 1)
  expect_true(sum(grepl("To prevent", mess)) == 1)
  expect_true(file.exists(file.path(tmpdir, "rasterTest.zip")))
  cs <- read.table(file.path(tmpdir, "CHECKSUMS.txt"), header = TRUE)
  expect_true(NROW(cs) == 2 || NROW(cs) == 3) # TODO this may be detecting a bug == on GA it is 2, locally it is 3
  expect_true(all(grepl("rasterTest", cs$file)))
  options(optsOrig)
})

test_that("preProcess fails if user provides a non .zip/.tar as archive", {
  skip_on_cran()
  testInit("terra", needInternet = TRUE)
  co <- capture.output({
    co <- capture.output(type = "message", {
      pre <- reproducible::preProcess(url = theRasterTestZip, destinationPath = tmpdir)
    })
  })
  testthat::expect_is(object = pre, class = "list")
  testthat::expect_error({
    ras <- reproducible::preProcess(archive = pre$targetFilePath)
  })
})

test_that("preProcess fails if user provides non-existing file", {
  skip_on_cran()
  testInit("terra", needInternet = TRUE)
  co <- capture.output({
    co <- capture.output(type = "message", {
      pre <- reproducible::preProcess(url = theRasterTestZip, destinationPath = tmpdir)
    })
  })
  testthat::expect_is(object = pre, class = "list")
  testthat::expect_error({
    ras <- reproducible::preProcess(archive = "fileDoesNotExist.zip")
  })
})

test_that("preProcess fails if user provides a directory as a targetFile", {
  skip_on_cran()
  testInit("terra", needInternet = TRUE)
  co <- capture.output({
    co <- capture.output(type = "message", {
      pre <- reproducible::preProcess(url = theRasterTestZip, destinationPath = tmpdir)
    })
  })
  testthat::expect_is(object = pre, class = "list")
  testthat::expect_error({
    ras <- reproducible::preProcess(targetFile = tmpdir)
  })
})

## 2022-11-03 this no longer fails on Ubuntu 20.04
# test_that("preProcess fails if the .rar file is defective", {
#   skip_on_cran()
#   testInit("raster")
#   co <- capture.output({
#     co <- capture.output(type = "message", {
#       testthat::expect_error({
#         ras <- preProcess(url = theRasterTestRar, destinationPath = tmpdir)
#       })
#     })
#   })
# })



test_that("preProcess fails if relative destPath not '.'", {
  # this fails on CRAN version 2.0.2
  skip_on_cran()
  skip_on_ci()
  testInit("googledrive", needGoogleDriveAuth = TRUE)
  dPath <- basename(tmpdir)
  out <- try(prepInputs(
    url = "https://drive.google.com/file/d/1u7o2BzPZ2Bo7hNcC8nEctNpDmp7ce84m",
    fun = "data.table::fread",
    destinationPath = dPath,
    purge = 1,
    filename2 = "userGcM3.csv", overwrite = TRUE
  ))
  expect_s3_class(out, "data.table")
})
