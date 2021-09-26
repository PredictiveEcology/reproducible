test_that("prepInputs correctly unzips large files", {
  skip_on_cran()

  skip_on_ci()

  skip_if_not(isInteractive(), "tests extracting large files should be run manually")

  ## based on #145. extracted file is ~30 GB so this takes a long time to test!

  testInitOut <- testInit("raster")
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  url <- "https://opendata.nfis.org/downloads/forest_change/CA_forest_harvest_mask_year_1985_2015.zip"
  ff <- prepInputs(url = url,
                   targetFile = "CA_harvest_year_1985_2015.tif",
                   destinationPath = asPath(tmpdir),
                   fun = "raster::raster",
                   userTags = c("objectName:forestHarvestMask", "goal:posthocGIS"))
  fout <- file.path(tmpdir, "CA_harvest_year_1985_2015.tif")
  expect_true(identical(normPath(filename(ff)), fout))
  expect_true(file.info(fout)[["size"]] > 28 * 1024^3)
})
