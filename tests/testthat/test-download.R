test_that("dlGeneric works", {

  url <- file.path("ftp://ftp.ccrs.nrcan.gc.ca/ad/NLCCLandCover",
                   "LandcoverCanada2005_250m/LandCoverOfCanada2005_V1_4.zip")

  destFile <- file.path(tempdir(), basename(url))

  expect_silent(
    #download.file(url, destfile = destFile, method = "auto", mode = "wb")
    dlGeneric(url, FALSE)
  )

  expect_true(file.exists(destFile))
  unlink(destFile)
})
