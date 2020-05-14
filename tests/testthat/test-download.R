test_that("dlGeneric works", {
  if (interactive()) {
    url <- "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip"
    res <- dlGeneric(url, FALSE, .tempPath = tempdir2(rndstr(1,6)))
    expect_true(file.exists(res$destFile))
    unlink(res$destFile)
  }
})
