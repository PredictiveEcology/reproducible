test_that("preProcess fails if user provides non-existing file", {
  skip_on_cran()
  testInit("terra", opts = list(reproducible.inputPaths = NULL,
                                reproducible.interactiveOnDownloadFail = TRUE), verbose = 2)
  testthat::with_mocked_bindings(
    isInteractive = function() {
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
    }#,
    #.env = "reproducible"
  )
  expect_true(grepl("manual download", errMsg))
  expect_true(grepl("appendChecksumsTable", errMsg))

  withr::local_options(reproducible.interactiveOnDownloadFail = FALSE)

  .downloadErrorFn = function(xxxx) {
    tryCatch(stop(xxxx), httr2_http_404 = function(cnd) NULL,
             error = function(xxxx) xxxx,
             silent = TRUE) # httr2 has a unique error; need to silence it
    try(stop(xxxx), silent = TRUE)
  }
  testthat::with_mocked_bindings(
      .downloadErrorFn = .downloadErrorFn,
      isInteractive = function() {
        FALSE
      },
      {
      errMsg <- testthat::capture_error({
        preProcess(
          url = "https://github.com/tati-micheletti/host/raw/master/data/rasterTest",
          destinationPath = tmpdir
        )
      })
    }
    #.env = "reproducible"
  )
  expect_true(grepl("manual download", errMsg))
  expect_true(grepl("appendChecksumsTable", errMsg))
  withr::deferred_run()

  # This sub-block exercises the interactive-prompt branch of dlErrorHandling
  # (`isInteractive() && getOption("reproducible.interactiveOnDownloadFail")`).
  # Line 27 left the option at FALSE for the preceding sub-block — re-enable
  # it here so the AND-gate actually fires the prompt path, otherwise the
  # mocked `.readline` is never reached and the error message lacks the
  # "Download failed" tail this assertion checks for.
  withr::local_options(reproducible.interactiveOnDownloadFail = TRUE)
  testthat::with_mocked_bindings(
    isInteractive = function() {
      TRUE
    },
    .readline = function(prompt) {
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
    }#,
    #.env = "reproducible"
  )
  expect_true(sum(grepl("Download failed", errMsg)) == 1)

  url <- "https://github.com/tati-micheletti/host/raw/master/data/rasterTest"
  withr::local_options("reproducible.interactiveOnDownloadFail" = TRUE)
  zipFilename <- tempfile2(fileext = ".zip")
  testthat::with_mocked_bindings(
    .downloadErrorFn = .downloadErrorFn,
    isInteractive = function() {
      TRUE
    },
    .readline = function(prompt) {
      theFile <- file.path(tmpdir, "rasterTestAA")
      write.table(theFile, file = theFile)
      origDir <- setwd(dirname(theFile))
      on.exit(setwd(origDir), add = TRUE)
      zip(zipfile = zipFilename, files = basename2(theFile), flags = "-q")
      file.copy(zipFilename, file.path(tmpdir, basename(url)))
      "y"
    },
    {
      co <- capture.output(type = "message", {
        co <- capture.output({
          mess <- testthat::capture_messages({
            errMsg <- testthat::capture_error({
              reproducible::preProcess(
                fun = NA,
                url = url,
                destinationPath = tmpdir
              )
            })
          })
        })
      })
    }#,
    # .env = "reproducible"
  )
  expect_true(sum(grepl("manual.+download", mess)) == 1) # manual download may be broken by \n
  expect_true(sum(grepl("To prevent", mess)) == 1)
  expect_true(file.exists(zipFilename))
  cs <- fread(file.path(tmpdir, "CHECKSUMS.txt"), header = TRUE)
  expect_true(NROW(cs) == 2 || NROW(cs) == 3) # TODO this may be detecting a bug == on GA it is 2, locally it is 3
  expect_true(all(grepl("rasterTest", cs$file)))
  withr::deferred_run()
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
  oo <- capture.output(type = "message",
  testthat::expect_error({
    ras <- reproducible::preProcess(archive = pre$targetFilePath)
  })
  )
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

test_that("preProcess does not load when fun cannot be guessed", {
  skip_on_cran()
  testInit("terra", needInternet = TRUE)
  co <- capture.output({
    co <- capture.output(type = "message", {
      pre <- reproducible::preProcess(url = theRasterTestZip, destinationPath = tmpdir)
    })
  })
  testthat::expect_is(object = pre, class = "list")
  # Passing a directory as targetFile: no recognised extension means fun
  # cannot be guessed. preProcess does not load the object, so this should
  # succeed quietly with funChar = NULL rather than erroring.
  co <- capture.output({
    co <- capture.output(type = "message", {
      ras <- reproducible::preProcess(targetFile = tmpdir)
    })
  })
  expect_null(ras$funChar)
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
