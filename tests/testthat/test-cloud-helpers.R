test_that("isID accepts 32 and 33 character strings", {
  expect_true(reproducible:::isID(strrep("a", 32L)))
  expect_true(reproducible:::isID(strrep("a", 33L)))
})

test_that("isID rejects strings outside the 32-33 length window", {
  expect_false(reproducible:::isID(strrep("a", 31L)))
  expect_false(reproducible:::isID(strrep("a", 34L)))
  expect_false(reproducible:::isID(""))
})

test_that("isOrHasRaster identifies a SpatRaster", {
  skip_if_not_installed("terra")
  r <- terra::rast(terra::ext(0, 1, 0, 1), vals = 1)
  expect_true(isTRUE(reproducible:::isOrHasRaster(r)))
})

test_that("isOrHasRaster returns FALSE for non-raster atomic objects", {
  expect_false(isTRUE(reproducible:::isOrHasRaster(1L)))
  expect_false(isTRUE(reproducible:::isOrHasRaster("foo")))
  expect_false(isTRUE(reproducible:::isOrHasRaster(data.frame(a = 1))))
})

test_that("isOrHasRaster recurses into lists and environments", {
  skip_if_not_installed("terra")
  r <- terra::rast(terra::ext(0, 1, 0, 1), vals = 1)
  ll <- list(notRaster = 1, alsoNot = "x", inner = list(ras = r))
  out <- reproducible:::isOrHasRaster(ll)
  expect_true(any(unlist(out)))

  env <- new.env(parent = emptyenv())
  env$x <- 1
  env$r <- r
  outE <- reproducible:::isOrHasRaster(env)
  expect_true(any(unlist(outE)))
})
