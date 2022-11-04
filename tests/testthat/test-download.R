test_that("dlGeneric works", {
  if (interactive()) {
    url <- "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip"
    noisyOutput <- capture.output({
      res <- dlGeneric(url, FALSE, tempdir2(rndstr(1,6)))
    })
    expect_true(file.exists(res$destFile))
    unlink(res$destFile)
  }
})
