test_that("preProcess fails if user provides a non .zip/.tar as archive", {
  testInitOut <- testInit("raster", needGoogle = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  pre <- reproducible::preProcess(
    url = "https://github.com/tati-micheletti/host/raw/master/data/rasterTest.zip",
    destinationPath = tmpdir)
  testthat::expect_is(object = pre, class = "list")
  testthat::expect_error(ras <- reproducible::preProcess(archive = pre$targetFilePath))
})

test_that("preProcess fails if user provides non-existing file", {
  testInitOut <- testInit("raster", needGoogle = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  pre <- reproducible::preProcess(
    url = "https://github.com/tati-micheletti/host/raw/master/data/rasterTest.zip",
    destinationPath = tmpdir)
  testthat::expect_is(object = pre, class = "list")
  testthat::expect_error(ras <- reproducible::preProcess(archive = "fileDoesNotExist.zip"))
})

test_that("preProcess fails if user provides a directory as a targetFile", {
  testInitOut <- testInit("raster", needGoogle = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  pre <- reproducible::preProcess(
    url = "https://github.com/tati-micheletti/host/raw/master/data/rasterTest.zip",
    destinationPath = tmpdir)
  testthat::expect_is(object = pre, class = "list")
  testthat::expect_error(ras <- reproducible::preProcess(targetFile = tmpdir))
})
