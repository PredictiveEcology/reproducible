test_that("dlGeneric works", {
  skip_on_cran()
  skip_if_not_installed("httr")
  testInit(needInternet = TRUE)
  url <- "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip"
  noisyOutput <- capture.output({
    res <- skip_on_transient_http(dlGeneric(url, tempdir2(rndstr(1, 6))))
  })
  expect_true(file.exists(res$destFile))
  unlink(res$destFile)
})

test_that("prepInputs reads Google Drive spreadsheets", {
  skip_on_cran()
  skip_if_not_installed("googledrive")
  skip_if_service_account_releaseVer_NotLinux()
  testInit(needGoogleDriveAuth = TRUE)
  userDist <- skip_on_transient_http(prepInputs(
    url = "https://docs.google.com/spreadsheets/d/1fOikb83aOuLlFYIn6pjmC7Jydjcy77TH",
    targetFile = "userDist.csv",
    destinationPath = tempdir(),
    type = "spreadsheet",
    fun = "data.table::fread"
  ))
  expect_true(is(userDist, "data.table"))
})
