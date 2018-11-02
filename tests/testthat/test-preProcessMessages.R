test_that("preProcess returns messages for deprecated arguments", {
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  testthat::skip_on_appveyor()
  testInitOut <- testInit("raster", needGoogle = TRUE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  expect_message(pre <- reproducible::preProcess(url = "https://drive.google.com/open?id=1cGFQlfe719nrPiN8HepmgB8vy7NRL8dR",
                                                 destinationPath = tmpdir,
                                                 cacheTags = "Test"))
  expect_null(pre$dots$cacheTags)
  expect_is(pre$dots$userTags, "character")
  expect_message(pre <- reproducible::preProcess(url = "https://drive.google.com/open?id=1cGFQlfe719nrPiN8HepmgB8vy7NRL8dR",
                                                 destinationPath = tmpdir,
                                                 postProcessedFilename = "Test"))
  expect_null(pre$dots$postProcessedFilename)
  expect_is(pre$dots$filename2, "character")
  expect_message(pre <- reproducible::preProcess(url = "https://drive.google.com/open?id=1cGFQlfe719nrPiN8HepmgB8vy7NRL8dR",
                                                 destinationPath = tmpdir,
                                                 writeCropped = "Test"))
  expect_null(pre$dots$writeCropped)
  expect_is(pre$dots$filename2, "character")
  expect_message(pre <- reproducible::preProcess(url = "https://drive.google.com/open?id=1cGFQlfe719nrPiN8HepmgB8vy7NRL8dR",
                                                 destinationPath = tmpdir,
                                                 rasterInterpMethod = "Test"))
  expect_null(pre$dots$rasterInterpMethod)
  expect_is(pre$dots$method, "character")
  expect_message(pre <- reproducible::preProcess(url = "https://drive.google.com/open?id=1cGFQlfe719nrPiN8HepmgB8vy7NRL8dR",
                                                 destinationPath = tmpdir,
                                                 rasterDatatype = "Test"))
  expect_null(pre$dots$rasterDatatype)
  expect_is(pre$dots$datatype, "character")
  expect_message(pre <- reproducible::preProcess(url = "https://drive.google.com/open?id=1cGFQlfe719nrPiN8HepmgB8vy7NRL8dR",
                                                 destinationPath = tmpdir,
                                                 pkg = "raster", fun = "crop"))
  expect_null(pre$dots$pkg)
  expect_is(pre$fun, "standardGeneric")
})
