test_that("dlGeneric works", {
  if (interactive()) {
    skip_if_not_installed("httr")
    testInit(needInternet = TRUE)
    url <- "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip"
    noisyOutput <- capture.output({
      res <- dlGeneric(url, tempdir2(rndstr(1, 6)))
    })
    expect_true(file.exists(res$destFile))
    unlink(res$destFile)

    skip_if_not_installed("googledrive")
    skip_if_no_token()
    userDist <- prepInputs(
      url = "https://docs.google.com/spreadsheets/d/1fOikb83aOuLlFYIn6pjmC7Jydjcy77TH", ##
      targetFile = "userDist.csv", # <---------------------------------------- specify targeFile
      destinationPath = tempdir(),
      type = "spreadsheet",
      fun = "data.table::fread"
    )
    expect_true(is(userDist, "data.table"))
  }
})
