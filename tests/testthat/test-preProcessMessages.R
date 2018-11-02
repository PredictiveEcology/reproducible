test_that("preProcess fails if user provides a non .zip/.tar as archive", {
  testInitOut <- testInit("raster", needGoogle = TRUE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  expect_message(pre <- reproducible::preProcess(url = "https://drive.google.com/open?id=1cGFQlfe719nrPiN8HepmgB8vy7NRL8dR",
                                  destinationPath = tmpdir,
                                  cacheTags = "Test"))
  expect_message(pre <- reproducible::preProcess(url = "https://drive.google.com/open?id=1cGFQlfe719nrPiN8HepmgB8vy7NRL8dR",
                                                 destinationPath = tmpdir,
                                                 cacheTags = "Test"))
  expect_message(pre <- reproducible::preProcess(url = "https://drive.google.com/open?id=1cGFQlfe719nrPiN8HepmgB8vy7NRL8dR",
                                                 destinationPath = tmpdir,
                                                 postProcessedFilename = "Test"))
  expect_message(pre <- reproducible::preProcess(url = "https://drive.google.com/open?id=1cGFQlfe719nrPiN8HepmgB8vy7NRL8dR",
                                                 destinationPath = tmpdir,
                                                 writeCropped = "Test"))
  expect_message(pre <- reproducible::preProcess(url = "https://drive.google.com/open?id=1cGFQlfe719nrPiN8HepmgB8vy7NRL8dR",
                                                 destinationPath = tmpdir,
                                                 rasterInterpMethod = "Test"))
  expect_message(pre <- reproducible::preProcess(url = "https://drive.google.com/open?id=1cGFQlfe719nrPiN8HepmgB8vy7NRL8dR",
                                                 destinationPath = tmpdir,
                                                 rasterDatatype = "Test"))
  expect_message(pre <- reproducible::preProcess(url = "https://drive.google.com/open?id=1cGFQlfe719nrPiN8HepmgB8vy7NRL8dR",
                                                 destinationPath = tmpdir,
                                                 pkg = "raster", fun = "crop"))
})
