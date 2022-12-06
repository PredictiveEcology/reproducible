test_that("prepInputs correctly unzips large files", {
  skip_on_cran()

  skip_on_ci()

  skip_if_not(isInteractive(), "tests extracting large files should be run manually")

  ## based on #145. extracted file is ~30 GB so this takes a long time to test!
  testInitOut <- testInit("raster")
  tmpdir <- "/mnt/d/temp" # need a drive that is large enough
  if (!"emcintir" %in% Sys.info()["user"] || (!dir.exists(tmpdir)))
    skip("This requires a lot of drive space")
  targFile <- "CA_harvest_year_1985_2015.tif"
  on.exit({
    unlink(file.path(tmpdir, targFile), recursive = TRUE)
    testOnExit(testInitOut)
  }, add = TRUE)

  browser()
  url <- "https://opendata.nfis.org/downloads/forest_change/CA_forest_harvest_mask_year_1985_2015.zip"
  options(reproducible.tempPath = file.path(tmpdir, "ttt"))
  ff <- prepInputs(url = url,
                   targetFile = targFile,
                   destinationPath = asPath(tmpdir),
                   fun = "terra::rast",
                   userTags = c("objectName:forestHarvestMask", "goal:posthocGIS"))
  fout <- file.path(tmpdir, "CA_harvest_year_1985_2015.tif")
  expect_true(identical(normPath(Filenames(ff)), fout))
  expect_true(file.info(fout)[["size"]] > 28 * 1024^3)
})
