test_that("preProcess works for .tar files", {
  skip_on_cran()
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
  skip_on_cran()
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
  skip_on_cran()
  testInitOut <- testInit("raster", needGoogle = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  ras <- reproducible::preProcess(url = "https://github.com/tati-micheletti/host/raw/master/data/rasterTest.zip")
  testthat::expect_is(object = ras, class = "list")
  testthat::expect_true(file.exists(ras$targetFilePath))
})

test_that("preProcess works when provides only archive", {
  skip_on_cran()
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
  skip_on_cran()
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
  skip_on_cran()
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
  skip_on_cran()
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
  skip_on_cran()
  testInitOut <- testInit("raster", needGoogle = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  ras <- reproducible::preProcess(url = "https://github.com/tati-micheletti/host/raw/master/data/rasterTest.zip",
                                  targetFile = "rasterTest.tif",
                                  archive = "rasterTest.zip",
                                  destinationPath = tmpdir)
  testthat::expect_is(object = ras, class = "list")
  testthat::expect_true(file.exists(ras$targetFilePath))
})

test_that("preProcess works when provides url, targetfile and destinationPath", {
  skip_on_cran()
  testInitOut <- testInit("raster", needGoogle = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  ras <- reproducible::preProcess(url = "https://github.com/tati-micheletti/host/raw/master/data/rasterTest.zip",
                                  targetFile = "rasterTest.tif",
                                  destinationPath = tmpdir)
  testthat::expect_is(object = ras, class = "list")
  testthat::expect_true(file.exists(ras$targetFilePath))
})
test_that("preProcess works when provides url and destinationPath for a .rar file", {
  skip_on_cran()
  testInitOut <- testInit("raster", needGoogle = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  hasUnrar <- .unrarExists()
  if (is.null(hasUnrar)) {
    expect_error(ras <- reproducible::preProcess(url = "https://github.com/tati-micheletti/host/raw/master/data/rasterTest.rar",
                                                 destinationPath = tmpdir))
  } else {
    ras <- reproducible::preProcess(url = "https://github.com/tati-micheletti/host/raw/master/data/rasterTest.rar",
                                    destinationPath = tmpdir)
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
  hasUnrar <- .unrarExists()
  if (is.null(hasUnrar)) {
    expect_error( ras <- reproducible::preProcess(url = "https://github.com/tati-micheletti/host/raw/master/data/rasterTest.rar",
                                                   targetFile = "rasterTest.tif",
                                                   destinationPath = tmpdir))
  } else {
    ras <- reproducible::preProcess(url = "https://github.com/tati-micheletti/host/raw/master/data/rasterTest.rar",
                                    targetFile = "rasterTest.tif",
                                    destinationPath = tmpdir)
    testthat::expect_is(object = ras, class = "list")
    testthat::expect_true(file.exists(ras$targetFilePath))
  }
})

test_that("preProcess works when provides url, archive and destinationPath for a .rar file", {
  skip_on_cran()
  testInitOut <- testInit("raster", needGoogle = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  hasUnrar <- .unrarExists()
  if (is.null(hasUnrar)) {
    expect_error(ras <- reproducible::preProcess(url = "https://github.com/tati-micheletti/host/raw/master/data/rasterTest.rar",
                                                   archive = "rasterTest.rar",
                                                   destinationPath = tmpdir))
  } else {
    ras <- reproducible::preProcess(url = "https://github.com/tati-micheletti/host/raw/master/data/rasterTest.rar",
                                    archive = "rasterTest.rar",
                                    destinationPath = tmpdir)
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
  testthat::expect_message(ras <- reproducible::preProcess(url = "https://github.com/tati-micheletti/host/raw/master/data/rasterTest.zip",
                                  destinationPath = tmpdir, cacheTags = "objectName::ras"))
  testthat::expect_is(object = ras, class = "list")
  testthat::expect_true(file.exists(ras$targetFilePath))
})

test_that("preProcess works, but gives a warning when supplying postProcessedFilename", {
  skip_on_cran()
  testInitOut <- testInit("raster", needGoogle = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  testthat::expect_message(ras <- reproducible::preProcess(url = "https://github.com/tati-micheletti/host/raw/master/data/rasterTest.zip",
                                                           destinationPath = tmpdir, postProcessedFilename = "ras.tif"))
  testthat::expect_is(object = ras, class = "list")
  testthat::expect_true(file.exists(ras$targetFilePath))
})

test_that("preProcess works, but gives a warning when supplying rasterInterpMethod", {
  skip_on_cran()
  testInitOut <- testInit("raster", needGoogle = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  testthat::expect_message(ras <- reproducible::preProcess(url = "https://github.com/tati-micheletti/host/raw/master/data/rasterTest.zip",
                                                           destinationPath = tmpdir, rasterInterpMethod = "ngb"))
  testthat::expect_is(object = ras, class = "list")
  testthat::expect_true(file.exists(ras$targetFilePath))
})

test_that("preProcess works, but gives a warning when supplying rasterDatatype", {
  skip_on_cran()
  testInitOut <- testInit("raster", needGoogle = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  testthat::expect_message(ras <- reproducible::preProcess(url = "https://github.com/tati-micheletti/host/raw/master/data/rasterTest.zip",
                                                           destinationPath = tmpdir, rasterDatatype = "INT1U"))
  testthat::expect_is(object = ras, class = "list")
  testthat::expect_true(file.exists(ras$targetFilePath))
})

test_that("preProcess works, but gives a warning when supplying pkg", {
  skip_on_cran()
  testInitOut <- testInit("raster", needGoogle = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  testthat::expect_message(ras <- reproducible::preProcess(url = "https://github.com/tati-micheletti/host/raw/master/data/rasterTest.zip",
                                                           destinationPath = tmpdir, pkg = "utils", fun = "unzip"))
  testthat::expect_is(object = ras, class = "list")
  testthat::expect_true(file.exists(ras$targetFilePath))
})

