test_that("prepInputsUrlTiles", {
  skip_on_cran()
  ## Deliberately NOT skip_on_ci(): skip_if_not_releaseVer_Linux() below confines
  ## this to the single Linux/release leg, so it runs on CI without racing
  ## against the other legs on the same Drive folder.
  ##
  ## Coverage runs are excluded, though: skip_if_not_releaseVer_Linux() bypasses
  ## itself under covr, and this test forks and does a Drive round-trip, which is
  ## exactly what coverage runs should not be carrying.
  skip_if(isTRUE(requireNamespace("covr", quietly = TRUE) && covr::in_covr()),
          "excluded from coverage runs")

  testInit(needGoogleDriveAuth = TRUE,
           c("terra", "googledrive"),

           opts = list(
             rasterTmpDir = tempdir2(rndstr(1, 6)),
             reproducible.inputPaths = NULL,
             reproducible.overwrite = TRUE,
             reproducible.useMemoise = FALSE,
             reproducible.rasterRead = "terra::rast"
           ),
           needInternet = TRUE
  )

  withr::local_options(reproducible.cachePath = tmpdir,
                       reproducible.inputPath = tmpdir,
                       mc.cores = 2L)# used by tiles
  outerDriveFolder <- "1KuBraAYnBpyxl3Nf0udc05fQlTPds2xY"
  skip_if_not_releaseVer_Linux()



  urlForTiles <- try(googledrive::drive_ls(googledrive::as_id(outerDriveFolder)))
  urlForTiles <- googledrive::drive_mkdir(
    name = basename(tempfile(pattern = "urlForTiles_")),
    path = googledrive::as_id(outerDriveFolder))
  urlForTiles <- urlForTiles$id
  withr::local_options(reproducible.prepInputsUrlTiles = urlForTiles)
  fn <- "reproducible_testUrlTiles_test4Tiles.tif"
  ext <- c(xmin = 307000, xmax = 363000, ymin = 1306000, ymax = 1394000)
  ext <- unname(ext)
  b <- terra::rast(terra::ext(ext, xy = FALSE), resolution = 100, vals = 1)
  terra::crs(b) <- "epsg:3978"
  extLrg <- terra::extend(b, 1e2)
  terra::crs(extLrg) <- "epsg:3978"
  ## NUM_THREADS=1: a default write allocates a GDAL thread pool sized to the
  ##   core count that is never released, which would make the tiling fork()
  ##   unsafe and send it down the serial fallback instead of the path we test
  extLrg <- terra::writeRaster(x = extLrg, filename = fn, overwrite = TRUE,
                               gdal = "NUM_THREADS=1")
  d <- googledrive::drive_upload(fn, path = googledrive::as_id(urlForTiles))

  ## wrapped to keep the (very chatty) download/tiling output out of the test log
  capture_messages(
    warns <- capture_warnings(
      a1 <- prepInputs(url = d$id, to = b, doUploads = TRUE, numTiles = c(2,2))
    )
  )
  expect_is(a1, "SpatRaster")
  withr::local_options(reproducible.prepInputsUrlTiles = NULL)
  capture_messages(a2 <- prepInputs(url = d$id, to = b, useCache = FALSE))
  a1b <- .wrap(a1)
  a2b <- .wrap(a2)
  expect_is(a2, "SpatRaster")
  expect_is(a1, "SpatRaster")

  testthat::expect_equivalent(a1, a2)

  # if (FALSE) {
  withr::local_options(reproducible.prepInputsUrlTiles = urlForTiles)
  fn <- "reproducible_testUrlTiles_test1Tile.tif"
  ext <- c(ymin = 1306000, ymax = 1394000, xmin = 307000, xmax = 363000)
  ext <- unname(ext)
  b <- terra::rast(terra::ext(ext, xy = FALSE), resolution = 100, vals = 1)
  terra::crs(b) <- "epsg:3978"
  extLrg <- terra::extend(b, 1e2)
  terra::crs(extLrg) <- "epsg:3978"
  ## NUM_THREADS=1: a default write allocates a GDAL thread pool sized to the
  ##   core count that is never released, which would make the tiling fork()
  ##   unsafe and send it down the serial fallback instead of the path we test
  extLrg <- terra::writeRaster(x = extLrg, filename = fn, overwrite = TRUE,
                               gdal = "NUM_THREADS=1")
  d <- googledrive::drive_upload(fn, path = googledrive::as_id(urlForTiles))
  b1 <- prepInputs(url = d$id, to = b, doUploads = TRUE, numTiles = c(2,2))
  withr::local_options(reproducible.prepInputsUrlTiles = NULL)
  b2 <- prepInputs(url = d$id, to = b)
  testthat::expect_equivalent(b1, b2)
  gls <- googledrive::drive_ls(urlForTiles)

  # clean up
  # googledrive::drive_rm(gls)
  googledrive::drive_rm(urlForTiles)

  # }
})
