## Coverage for untested pieces of R/postProcess.R.
##
## assessDataType() picks the smallest GDAL datatype that can hold a raster's
## values -- getting it wrong silently truncates or inflates written files, so
## the branches are worth pinning by value range. The rest are small helpers
## that had no coverage at all.
##
## No network, no Drive.

mkRas <- function(vals) {
  terra::rast(nrows = 4, ncols = 4, xmin = 0, xmax = 4, ymin = 0, ymax = 4,
              crs = "EPSG:4326", vals = vals)
}

test_that("assessDataType picks a datatype from the value range", {
  skip_if_not_installed("terra")
  testInit("terra")

  ## Small non-negative integers fit an unsigned byte.
  expect_identical(assessDataType(mkRas(rep(1:4, 4))), "INT1U")

  ## Negatives force a signed type.
  expect_identical(assessDataType(mkRas(rep(-5:-2, 4))), "INT1S")

  ## Non-integers need a float.
  expect_identical(assessDataType(mkRas(seq(0.5, 8, length.out = 16))), "FLT4S")

  ## Only 0/1 present -> logical, the narrowest of all.
  expect_identical(assessDataType(mkRas(rep(c(0, 1), 8))), "LOG1S")

  ## Values beyond a byte or short need a 32-bit integer.
  expect_identical(assessDataType(mkRas(rep(c(1e6, 2e6), 8))), "INT4U")
})

test_that("sampRand draws the requested number of cells", {
  skip_if_not_installed("terra")
  testInit("terra")

  r <- mkRas(rep(1:4, 4))
  s <- sampRand(r, size = 3)

  ## Returns a bare vector of values, not a data.frame -- the terra branch
  ## subsets [, 1] precisely so both raster and terra backends agree.
  expect_length(s, 3L)
  expect_false(is.data.frame(s))
  expect_true(all(s %in% terra::values(r)))
})

test_that("maskInputs.default masks to studyArea, or to rasterToMatch when asked", {
  skip_if_not_installed("terra")
  testInit("terra")

  r <- mkRas(rep(1:4, 4))
  poly <- terra::as.polygons(terra::ext(1, 3, 1, 3), crs = terra::crs(r))

  out <- maskInputs(r, studyArea = poly, verbose = 0)
  expect_s4_class(out, "SpatRaster")
  ## Masking introduces NAs outside the polygon; the input had none.
  expect_true(sum(is.na(terra::values(out))) > sum(is.na(terra::values(r))))

  ## maskWithRTM switches the mask source from studyArea to rasterToMatch.
  rtm <- mkRas(c(rep(1, 8), rep(NA, 8)))
  out2 <- maskInputs(r, studyArea = poly, rasterToMatch = rtm,
                     maskWithRTM = TRUE, verbose = 0)
  expect_s4_class(out2, "SpatRaster")
})

test_that("setMinMaxIfNeeded returns a raster with usable min/max", {
  skip_if_not_installed("terra")
  testInit("terra")

  r <- mkRas(rep(1:4, 4))
  out <- setMinMaxIfNeeded(r)

  expect_s4_class(out, "SpatRaster")
  expect_false(is.na(suppressWarnings(minFn(out))))
  expect_false(is.na(suppressWarnings(maxFn(out))))
  expect_equal(suppressWarnings(minFn(out)), 1)
  expect_equal(suppressWarnings(maxFn(out)), 4)
})
