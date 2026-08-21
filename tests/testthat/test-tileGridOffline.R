## Coverage for makeTileGridFromGADMcode()'s server-outage fallback
## (R/downloadTileAndUpload.R).
##
## The tile grid is normally derived from a GADM boundary fetched by
## geodata::gadm(). When that returns nothing -- "most likely geodata server is
## down", per the comment -- the code substitutes a hardcoded extent so tiling
## still works. That fallback had no test, and it is exactly the path that runs
## when a build breaks in the field.
##
## Mocking gadm() to return NULL reaches it deterministically AND keeps this
## test free of the multi-megabyte GADM download that the real path performs on
## every cold cache.
##
## No network, no Drive.

test_that("makeTileGridFromGADMcode falls back to a fixed extent when gadm returns NULL", {
  skip_if_not_installed("terra")
  skip_if_not_installed("geodata")
  testInit("terra")

  out <- testthat::with_mocked_bindings(
    makeTileGridFromGADMcode("CAN", numTiles = c(2, 2), crs = "EPSG:3347"),
    gadm = function(...) NULL, .package = "geodata")

  ## The contract callers rely on: a usable grid, not an error.
  expect_type(out, "list")
  expect_true(all(c("tileGrid", "numTiles", "area") %in% names(out)))
  expect_s4_class(out$tileGrid, "SpatVector")

  ## numTiles is honoured, so the grid actually has the requested shape.
  expect_identical(out$numTiles, c(2, 2))
  expect_equal(nrow(out$tileGrid), 4)
})

test_that("makeTileGridFromGADMcode fallback also handles the 'NULL' string", {
  skip_if_not_installed("terra")
  skip_if_not_installed("geodata")
  testInit("terra")

  ## Cache() can hand back the literal string "NULL" rather than NULL, which is
  ## why the guard tests for both. Same fallback either way.
  out <- testthat::with_mocked_bindings(
    makeTileGridFromGADMcode("CAN", numTiles = c(2, 2), crs = "EPSG:3347"),
    gadm = function(...) "NULL", .package = "geodata")

  expect_s4_class(out$tileGrid, "SpatVector")
  expect_equal(nrow(out$tileGrid), 4)
})
