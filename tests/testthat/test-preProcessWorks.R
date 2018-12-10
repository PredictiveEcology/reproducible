test_that("preProcess works for .tar files", {
  testInitOut <- testInit("raster", needGoogle = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  ras <- reproducible::preProcess(url = "https://github.com/tati-micheletti/host/raw/master/data/rasterTest.tar",
                                  destinationPath = tmpdir)
  testthat::expect_is(object = ras, class = "list")
  testthat::expect_true(file.exists(ras$targetFilePath))
})

test_that("preProcess works for .zip when provided only url and destinationPath", {
  testInitOut <- testInit("raster", needGoogle = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  ras <- reproducible::preProcess(url = "https://github.com/tati-micheletti/host/raw/master/data/rasterTest.zip",
                                  destinationPath = tmpdir)
  testthat::expect_is(object = ras, class = "list")
  testthat::expect_true(file.exists(ras$targetFilePath))
})

test_that("preProcess works with only url", {
  testInitOut <- testInit("raster", needGoogle = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  ras <- reproducible::preProcess(url = "https://github.com/tati-micheletti/host/raw/master/data/rasterTest.zip")
  testthat::expect_is(object = ras, class = "list")
  testthat::expect_true(file.exists(ras$targetFilePath))
})

test_that("preProcess works when provides only archive", {
  testInitOut <- testInit("raster", needGoogle = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  pre <- reproducible::preProcess(url = "https://github.com/tati-micheletti/host/raw/master/data/rasterTest.zip",
                                  destinationPath = tmpdir)
  testthat::expect_is(object = pre, class = "list")
  ras <- reproducible::preProcess(archive = file.path(pre$destinationPath,
                                                      list.files(pre$destinationPath)[
                                                        grepl(x = list.files(pre$destinationPath),
                                                              pattern = ".zip|.tar")]))
  testthat::expect_is(object = ras, class = "list")
})

test_that("preProcess works when provides archive and destinationPath", {
  testInitOut <- testInit("raster", needGoogle = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  pre <- reproducible::preProcess(url = "https://github.com/tati-micheletti/host/raw/master/data/rasterTest.zip",
                                  destinationPath = tmpdir)
  testthat::expect_is(object = pre, class = "list")
  ras <- reproducible::preProcess(archive = file.path(pre$destinationPath,
                                                      list.files(pre$destinationPath)[
                                                        grepl(x = list.files(pre$destinationPath),
                                                              pattern = ".zip|.tar")]),
                                  destinationPath = tmpdir)
  testthat::expect_is(object = ras, class = "list")
})

test_that("preProcess works when provides only targetFile", {
  testInitOut <- testInit("raster", needGoogle = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  pre <- reproducible::preProcess(url = "https://github.com/tati-micheletti/host/raw/master/data/rasterTest.zip",
                                  destinationPath = tmpdir)
  testthat::expect_is(object = pre, class = "list")
  ras <- reproducible::preProcess(targetFile = pre$targetFilePath)
  testthat::expect_is(object = ras, class = "list")
})
test_that("preProcess works when provides targetfile and destinationPath", {
  testInitOut <- testInit("raster", needGoogle = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  pre <- reproducible::preProcess(url = "https://github.com/tati-micheletti/host/raw/master/data/rasterTest.zip",
                                  destinationPath = tmpdir)
  testthat::expect_is(object = pre, class = "list")
  ras <- reproducible::preProcess(targetFile = pre$targetFilePath,
                                  destinationPath = tmpdir)
  testthat::expect_is(object = ras, class = "list")
})

test_that("preProcess works when provides url, archive, targetfile and destinationPath", {
  testInitOut <- testInit("raster", needGoogle = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  ras <- reproducible::preProcess("https://github.com/tati-micheletti/host/raw/master/data/rasterTest.zip",
                                  targetFile = "rasterTest.tif",
                                  archive = "rasterTest.zip",
                                  destinationPath = tmpdir)
  testthat::expect_is(object = ras, class = "list")
  testthat::expect_true(file.exists(ras$targetFilePath))
})
test_that("preProcess works when provides url, targetfile and destinationPath", {
  testInitOut <- testInit("raster", needGoogle = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  ras <- reproducible::preProcess("https://github.com/tati-micheletti/host/raw/master/data/rasterTest.zip",
                                  targetFile = "rasterTest.tif",
                                  destinationPath = tmpdir)
  testthat::expect_is(object = ras, class = "list")
  testthat::expect_true(file.exists(ras$targetFilePath))
})
