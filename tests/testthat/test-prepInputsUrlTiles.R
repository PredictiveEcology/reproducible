test_that("prepInputsUrlTiles", {
  skip_on_cran()
  skip_on_ci()

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
  skip_if_service_account_releaseVer_NotLinux()



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
  extLrg <- terra::writeRaster(x = extLrg, filename = fn, overwrite = TRUE)
  d <- googledrive::drive_upload(fn, path = googledrive::as_id(urlForTiles))

  mess1 <- capture_messages(
    warns <- capture_warnings(
      a1 <- prepInputs(url = d$id, to = b, doUploads = TRUE, numTiles = c(2,2))
    )
  )
  expect_is(a1, "SpatRaster")
  withr::local_options(reproducible.prepInputsUrlTiles = NULL)
  mess2 <- capture_messages(a2 <- prepInputs(url = d$id, to = b, useCache = FALSE))
  cat(mess2, file = "~/tmp/mess2.txt")
  cat(mess1, file = "~/tmp/mess1.txt")
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
  extLrg <- terra::writeRaster(x = extLrg, filename = fn, overwrite = TRUE)
  d <- googledrive::drive_upload(fn, path = googledrive::as_id(urlForTiles))
  b1 <- prepInputs(url = d$id, to = b, doUploads = TRUE, numTiles = c(2,2))
  withr::local_options(reproducible.prepInputsUrlTiles = NULL)
  b2 <- prepInputs(url = d$id, to = b)
  testthat::expect_equivalent(b1, b2)
  save(a1, a2, b1, b2, file = "~/tmp/objs.rda")
  gls <- googledrive::drive_ls(urlForTiles)

  # clean up
  # googledrive::drive_rm(gls)
  googledrive::drive_rm(urlForTiles)

  # }
})
