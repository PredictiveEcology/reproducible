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

  ## the fallback warns, because it silently changes which area gets tiled
  expect_warning(
    out <- testthat::with_mocked_bindings(
      makeTileGridFromGADMcode("CAN", numTiles = c(2, 2), crs = "EPSG:3347"),
      gadm = function(...) NULL, .package = "geodata"),
    .message$gadmFallbackTxt, fixed = TRUE)

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
  expect_warning(
    out <- testthat::with_mocked_bindings(
      makeTileGridFromGADMcode("CAN", numTiles = c(2, 2), crs = "EPSG:3347"),
      gadm = function(...) "NULL", .package = "geodata"),
    .message$gadmFallbackTxt, fixed = TRUE)

  expect_s4_class(out$tileGrid, "SpatVector")
  expect_equal(nrow(out$tileGrid), 4)
})

test_that("a gadm() error also falls back, with a warning, rather than propagating", {
  skip_if_not_installed("terra")
  skip_if_not_installed("geodata")
  testInit("terra")

  ## The case that broke CI: no geodata path configured, so gadm() errors
  ## outright rather than returning NULL.
  expect_warning(
    out <- testthat::with_mocked_bindings(
      makeTileGridFromGADMcode("CAN", numTiles = c(2, 2), crs = "EPSG:3347"),
      gadm = function(...) stop("you need to provide a path"),
      .package = "geodata"),
    .message$gadmFallbackTxt, fixed = TRUE)

  expect_s4_class(out$tileGrid, "SpatVector")
  expect_equal(nrow(out$tileGrid), 4)
})

test_that(".gadmPath prefers persistence over tempdir", {
  skip_if_not_installed("geodata")

  ## with a shared-downloads location set, downloads should land there, not in
  ## the session temp default
  shared <- file.path(tempdir(), paste0("shared", sample(1e6, 1)))
  withr::local_options(reproducible.destinationPathShared = shared,
                       reproducible.inputPaths = NULL)
  p <- testthat::with_mocked_bindings(
    reproducible:::.gadmPath(),
    geodata_path = function(...) "", .package = "geodata")
  expect_true(startsWith(normalizePath(p, mustWork = FALSE),
                         normalizePath(shared, mustWork = FALSE)))
})
