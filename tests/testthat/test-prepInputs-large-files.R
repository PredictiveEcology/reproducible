test_that("prepInputs correctly unzips large files", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not(isInteractive(), "tests extracting large files should be run manually")

  ## based on #145. extracted file is ~30 GB so this takes a long time to test!
  testInitOut <- testInit("terra")
  # tmpdir <- "/mnt/d/temp" # need a drive that is large enough
  # if (!"emcintir" %in% Sys.info()["user"] || (!dir.exists(tmpdir)))
  #   skip("This requires a lot of drive space")
  targFile <- "CA_harvest_year_1985_2015.tif"
  on.exit({
    unlink(file.path(tmpdir, targFile), recursive = TRUE)
    testOnExit(testInitOut)
  }, add = TRUE)

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


test_that("Issue 181 geodatabase file", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not(isInteractive(), "test #2: extracting large files should be run manually")

  ## based on #145. extracted file is ~30 GB so this takes a long time to test!
  testInitOut <- testInit(c("terra", "googledrive"), needGoogleDriveAuth = TRUE)
  rstLCC <- Cache(prepInputs,
                  targetFile = "EOSD_Mosaic.gdb",
                  archive = "EOSD_2000_2007_combined.zip",
                  alsoExtract = "similar",
                  url = "https://drive.google.com/file/d/1p66_P6dNdlrvAF3Mp99Xz9Bdz2lvfaQ7",
                  destinationPath = tmpdir,
                  filename2 = NULL,
                  fun = NA,
                  userTags = c("outFun:Cache",
                               "step:prepEOSD"))
  expect_true(is(sf::st_read(rstLCC$targetFilePath, layer = "EOSD_Mosaic_BWC_range_clip", quiet = TRUE), "sf"))
})
