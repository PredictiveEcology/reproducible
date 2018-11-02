test_that("preProcess fails if user provides a non .zip/.tar as archive", {
  testInitOut <- testInit("raster", needGoogle = TRUE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  pre <- reproducible::preProcess(url = "https://drive.google.com/open?id=1cGFQlfe719nrPiN8HepmgB8vy7NRL8dR",
                                  destinationPath = tmpdir)
  testthat::expect_is(object = pre, class = "list")
  testthat::expect_error(ras <- reproducible::preProcess(archive = pre$targetFilePath))
})

test_that("preProcess fails if user provides non-existing file", {
  testInitOut <- testInit("raster", needGoogle = TRUE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  pre <- reproducible::preProcess(url = "https://drive.google.com/open?id=1cGFQlfe719nrPiN8HepmgB8vy7NRL8dR",
                                  destinationPath = tmpdir)
  testthat::expect_is(object = pre, class = "list")
  testthat::expect_error(ras <- reproducible::preProcess(archive = "fileDoesNotExist.zip"))
})

test_that("preProcess fails if user provides a directory as a targetFile", {
  testInitOut <- testInit("raster", needGoogle = TRUE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  pre <- reproducible::preProcess(url = "https://drive.google.com/open?id=1cGFQlfe719nrPiN8HepmgB8vy7NRL8dR",
                                  destinationPath = tmpdir)
  testthat::expect_is(object = pre, class = "list")
  testthat::expect_error(ras <- reproducible::preProcess(targetFile = tmpdir))
})
