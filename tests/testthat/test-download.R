test_that("dlGeneric works", {
  if (interactive()) {
    url <- "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip"
    noisyOutput <- capture.output({
      res <- dlGeneric(url, FALSE, tempdir2(rndstr(1,6)))
    })
    expect_true(file.exists(res$destFile))
    unlink(res$destFile)



    userDist <- prepInputs(
      url = "https://docs.google.com/spreadsheets/d/1fOikb83aOuLlFYIn6pjmC7Jydjcy77TH", ##
      targetFile = "userDist.csv", # <---------------------------------------------------------------- specify targeFile
      destinationPath = tempdir(),
      type = "spreadsheet",
      fun = "data.table::fread"
    )
    expect_true(is(userDist, "data.table"))
  }
})
