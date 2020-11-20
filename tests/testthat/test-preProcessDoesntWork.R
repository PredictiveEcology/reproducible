test_that("preProcess fails if user provides non-existing file", {
  skip_on_cran()
  testInitOut <- testInit("raster", needGoogle = FALSE, opts = list(reproducible.inputPaths = NULL))
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  testthat::with_mock(
    `reproducible:::isInteractive` = function() {FALSE},
    {
    errMsg <- testthat::capture_error(reproducible::preProcess(
      url = "https://github.com/tati-micheletti/host/raw/master/data/rasterTest",
      destinationPath = tmpdir
    ))
    })
  expect_true(grepl("try manual", errMsg))
  expect_true(grepl("appendChecksumsTable", errMsg))

  optsOrig <- options('reproducible.interactiveOnDownloadFail' = FALSE)
  errMsg <- testthat::capture_error(reproducible::preProcess(
    url = "https://github.com/tati-micheletti/host/raw/master/data/rasterTest",
    destinationPath = tmpdir
  ))
  expect_true(grepl("try manual", errMsg))
  expect_true(grepl("appendChecksumsTable", errMsg))
  options(optsOrig)

  testthat::with_mock(
    `reproducible:::isInteractive` = function() {TRUE},
    `reproducible:::.readline` = function(prompt) {"n"},
    {
      mess <- testthat::capture_messages(
        errMsg <- testthat::capture_error(reproducible::preProcess(
        url = "https://github.com/tati-micheletti/host/raw/master/data/rasterTest",
        destinationPath = tmpdir
      )))
    })
  expect_true(sum(grepl("manual download", mess)) == 1)
  expect_true(sum(grepl("To prevent", mess)) == 1)

  testthat::with_mock(
    `reproducible:::isInteractive` = function() {TRUE},
    `reproducible:::.readline` = function(prompt) {
      file.create(file.path(tmpdir, "rasterTest.zip"))
      "y"
      },
    {
      mess <- testthat::capture_messages(
        errMsg <- testthat::capture_error(reproducible::preProcess(
          url = "https://github.com/tati-micheletti/host/raw/master/data/rasterTest",
          destinationPath = tmpdir
        )))
    })
  expect_true(sum(grepl("manual download", mess)) == 1)
  expect_true(sum(grepl("To prevent", mess)) == 1)
  expect_true(file.exists(file.path(tmpdir, "rasterTest")))
  cs <- read.table(file.path(tmpdir, "CHECKSUMS.txt"), header = T)
  expect_true(NROW(cs) == 1)
  expect_true(identical(cs$file, "rasterTest"))


})

test_that("preProcess fails if user provides a non .zip/.tar as archive", {
  skip_on_cran()
  testInitOut <- testInit("raster", needGoogle = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  pre <- reproducible::preProcess(
    url = "https://github.com/tati-micheletti/host/raw/master/data/rasterTest.zip",
    destinationPath = tmpdir
  )
  testthat::expect_is(object = pre, class = "list")
  testthat::expect_error({
    ras <- reproducible::preProcess(archive = pre$targetFilePath)
  })
})

test_that("preProcess fails if user provides non-existing file", {
  skip_on_cran()
  testInitOut <- testInit("raster", needGoogle = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  pre <- reproducible::preProcess(
    url = "https://github.com/tati-micheletti/host/raw/master/data/rasterTest.zip",
    destinationPath = tmpdir
  )
  testthat::expect_is(object = pre, class = "list")
  testthat::expect_error({
    ras <- reproducible::preProcess(archive = "fileDoesNotExist.zip")
  })
})

test_that("preProcess fails if user provides a directory as a targetFile", {
  skip_on_cran()
  testInitOut <- testInit("raster", needGoogle = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  pre <- reproducible::preProcess(
    url = "https://github.com/tati-micheletti/host/raw/master/data/rasterTest.zip",
    destinationPath = tmpdir
  )
  testthat::expect_is(object = pre, class = "list")
  testthat::expect_error({
    ras <- reproducible::preProcess(targetFile = tmpdir)
  })
})

test_that("preProcess fails if the .rar file is defective", {
  skip_on_cran()
  testInitOut <- testInit("raster", needGoogle = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  testthat::expect_error({
    ras <- preProcess(url = "https://github.com/tati-micheletti/host/blob/master/data/rasterTest.rar",
                      destinationPath = tmpdir)
  })
})
