test_that("preProcess works in different situations", {
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  testthat::skip_on_appveyor()
  testInitOut <- testInit("raster", needGoogle = TRUE,
                          opts = list("reproducible.overwrite" = TRUE))
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  # User provides only url (GDrive)
  ras <- reproducible::preProcess(url = "https://drive.google.com/open?id=1cGFQlfe719nrPiN8HepmgB8vy7NRL8dR",
                                  destinationPath = tmpdir)
  testthat::expect_is(object = ras, class = "list")

  # User provides only url and destinationPath
  ras <- reproducible::preProcess(url = "https://drive.google.com/open?id=1cGFQlfe719nrPiN8HepmgB8vy7NRL8dR")
  testthat::expect_is(object = ras, class = "list")

  # User provides only archive
  pre <- reproducible::preProcess(url = "https://drive.google.com/open?id=1cGFQlfe719nrPiN8HepmgB8vy7NRL8dR",
                                  destinationPath = tmpdir)
  testthat::expect_is(object = pre, class = "list")
  ras <- reproducible::preProcess(archive = file.path(pre$destinationPath,
                                                      list.files(pre$destinationPath)[
                                                        grepl(x = list.files(pre$destinationPath),
                                                              pattern = ".zip|.tar")]))
  testthat::expect_is(object = ras, class = "list")
  # User provides only archive and destinationPath
  ras <- reproducible::preProcess(archive = file.path(pre$destinationPath,
                                                      list.files(pre$destinationPath)[
                                                        grepl(x = list.files(pre$destinationPath),
                                                              pattern = ".zip|.tar")]),
                                  destinationPath = tmpdir)
  testthat::expect_is(object = ras, class = "list")

  # User provides only targetFile
  ras <- reproducible::preProcess(targetFile = pre$targetFilePath)
  testthat::expect_is(object = ras, class = "list")

  # User provides only targetFile and destinationPath
  ras <- reproducible::preProcess(targetFile = pre$targetFilePath,
                                  destinationPath = tmpdir)
  testthat::expect_is(object = ras, class = "list")

  # User provides a .zip file as a targetFile
  ras <- reproducible::preProcess(targetFile = file.path(pre$destinationPath,
                                                         list.files(pre$destinationPath)[
                                                           grepl(x = list.files(pre$destinationPath),
                                                                 pattern = ".zip|.tar")]))
  testthat::expect_is(object = ras, class = "list")
})
