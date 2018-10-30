test_that("dlGeneric works", {
  skip_on_cran()

  url <- "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip"

  #destFile <- file.path(tempdir(), basename(url))
  #expect_silent(
    #download.file(url, destfile = destFile, method = "auto", mode = "wb")
  #) ## TODO: why is httr generating warnings??

  res <- dlGeneric(url, FALSE)
  expect_true(file.exists(res$destFile))
  unlink(destFile)
})
