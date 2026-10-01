test_that("postProcessTo(writeTo =, gdal =) passes creation options to the written file", {
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")
  r <- terra::rast(nrows = 120, ncols = 160, nlyrs = 4, xmin = 0, xmax = 160, ymin = 0, ymax = 120,
                   crs = "EPSG:3857")
  set.seed(2)
  terra::values(r) <- round(stats::rnorm(terra::ncell(r) * 4, 50, 10), 2)
  names(r) <- paste0("year", 2001:2004)
  to <- terra::rast(terra::ext(20, 120, 10, 90), resolution = 0.5, crs = "EPSG:3857")
  mask <- terra::as.polygons(terra::ext(30, 100, 20, 80), crs = "EPSG:3857")

  fDefault <- withr::local_tempfile(fileext = ".tif")
  fGdal <- withr::local_tempfile(fileext = ".tif")
  fThreads <- withr::local_tempfile(fileext = ".tif")
  d <- postProcessTo(r, to = to, maskTo = mask, writeTo = fDefault, useCache = FALSE, overwrite = TRUE)
  g <- postProcessTo(r, to = to, maskTo = mask, writeTo = fGdal, useCache = FALSE, overwrite = TRUE,
                     gdal = c("INTERLEAVE=BAND", "TILED=YES", "BLOCKXSIZE=64", "BLOCKYSIZE=64"))
  postProcessTo(r, to = to, maskTo = mask, writeTo = fThreads, useCache = FALSE, overwrite = TRUE,
                gdal = c("TILED=YES", "NUM_THREADS=2"))

  infoD <- sf::gdal_utils("info", fDefault, quiet = TRUE)
  infoG <- sf::gdal_utils("info", fGdal, quiet = TRUE)
  expect_match(infoG, "INTERLEAVE=BAND")
  expect_match(infoG, "Block=64x64")
  expect_no_match(infoD, "Block=64x64")
  expect_match(infoD, "INTERLEAVE=PIXEL") ## default behaviour unchanged

  expect_equal(terra::values(terra::rast(fGdal)), terra::values(terra::rast(fDefault)), tolerance = 0)
  expect_equal(terra::values(terra::rast(fThreads)), terra::values(terra::rast(fDefault)),
               tolerance = 0)
  expect_identical(names(terra::rast(fGdal)), names(terra::rast(fDefault)))
})
