test_that("dlGeneric works", {
  skip_on_cran()

  url <- "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip"
  res <- dlGeneric(url, FALSE)
  expect_true(file.exists(res$destFile))
  unlink(res$destFile)
})
