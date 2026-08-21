## Coverage for untested helpers in R/cache-helpers.R.
##
## Deliberately does NOT cover .digestRasterLayer() or asPath(NULL): both appear
## to be broken rather than merely untested, and a test asserting their current
## behaviour would cement the bug. See the PR description.

test_that("isUpdated reports whether Cache produced a new or changed result", {
  testInit()

  ## Cache marks a fresh computation with newCache ...
  expect_true(isUpdated(structure(1, .Cache = list(newCache = TRUE))))
  ## ... and a recovered-but-modified one with a non-empty `changed`.
  expect_true(isUpdated(structure(1, .Cache = list(changed = "somethingChanged"))))

  ## Neither -> FALSE, i.e. a plain cache hit.
  expect_false(isUpdated(structure(1, .Cache = list(newCache = FALSE))))
  expect_false(isUpdated(structure(1, .Cache = list(changed = character()))))

  ## An object that never went through Cache has no .Cache attribute at all;
  ## must be FALSE rather than an error, since callers apply it generally.
  expect_false(isUpdated(1))
  expect_false(isUpdated(NULL))
})

test_that("wrapSpatVector/unwrapSpatVector round-trip a SpatVector", {
  skip_if_not_installed("terra")
  testInit("terra")

  v <- terra::vect(cbind(c(0, 1, 1), c(0, 0, 1)), type = "polygons", crs = "EPSG:4326")
  terra::values(v) <- data.frame(a = 1, b = "x")

  w <- wrapSpatVector(v)
  ## Wrapped form is a plain list-ish object that can be serialised into the
  ## cache; a live SpatVector cannot, because it holds an external pointer.
  expect_s3_class(w, "PackedSpatVector2")

  u <- unwrapSpatVector(w)
  expect_s4_class(u, "SpatVector")

  ## Geometry, attributes and CRS all survive.
  expect_identical(terra::geomtype(u), terra::geomtype(v))
  expect_identical(terra::values(u)$a, terra::values(v)$a)
  expect_identical(terra::values(u)$b, terra::values(v)$b)
  expect_identical(terra::crs(u), terra::crs(v))
  expect_equal(as.vector(terra::ext(u)), as.vector(terra::ext(v)), tolerance = 1e-9)
})

test_that("unwrapSpatVector handles an empty geometry", {
  skip_if_not_installed("terra")
  testInit("terra")

  ## The empty case is special-cased to "points": terra::vect() cannot infer a
  ## geometry type from nothing, so a type must be supplied or it errors.
  v <- terra::vect(cbind(numeric(0), numeric(0)), type = "points", crs = "EPSG:4326")
  w <- wrapSpatVector(v)
  expect_equal(NROW(w$geometry), 0L)

  u <- unwrapSpatVector(w)
  expect_s4_class(u, "SpatVector")
  expect_equal(nrow(u), 0L)
})

test_that("list2envAttempts fills a normal environment and returns NULL", {
  testInit()

  e <- new.env(parent = emptyenv())
  out <- list2envAttempts(list(a = 1, b = "two"), e)

  ## NULL return means "the environment was filled in place" -- the caller keeps
  ## using its own environment. A non-NULL return signals the fallback path,
  ## where a new environment had to be built instead.
  expect_null(out)
  expect_setequal(ls(e), c("a", "b"))
  expect_identical(e$a, 1)
  expect_identical(e$b, "two")
})

test_that("robustDigest works for raster objects", {
  skip_if_not_installed("raster")
  testInit("raster")

  r <- raster::raster(raster::extent(0, 2, 0, 2), resolution = 1, vals = 1:4)
  rf <- raster::writeRaster(r, file.path(tmpdir, "r.grd"), overwrite = TRUE)

  ## .digestRasterLayer wrapped its file-slot list in asPath(), which has
  ## methods for character and NULL only -- so this errored with "no applicable
  ## method" for EVERY raster, in memory or file-backed, taking Cache() with it.
  d1 <- .robustDigest(r)
  d2 <- .robustDigest(rf)
  expect_type(d1, "character")
  expect_type(d2, "character")

  ## Deterministic: the same object digests the same way twice. Without this a
  ## cache would never hit.
  expect_identical(.robustDigest(r), d1)

  ## Discriminating: different values must not collide, or the cache would
  ## return the wrong object.
  rDiff <- raster::raster(raster::extent(0, 2, 0, 2), resolution = 1, vals = c(9, 9, 9, 9))
  expect_false(identical(.robustDigest(rDiff), d1))

  ## RasterStack takes the multi-layer branch.
  expect_type(.robustDigest(raster::stack(r, rDiff)), "character")
})

test_that("Cache round-trips a raster argument and a raster return value", {
  skip_if_not_installed("raster")
  testInit("raster")

  r <- raster::raster(raster::extent(0, 2, 0, 2), resolution = 1, vals = 1:4)

  ## A raster passed IN must digest, so the call can be cached at all.
  f <- function(x) raster::cellStats(x, "sum")
  first <- Cache(f, r, cachePath = tmpCache)
  second <- Cache(f, r, cachePath = tmpCache)
  expect_equal(as.numeric(first), 10)
  ## Second call is a cache hit and agrees; it carries .Cache attributes, so
  ## compare values rather than whole objects.
  expect_equal(as.numeric(second), as.numeric(first))

  ## A raster returned OUT must survive wrap/unwrap through the cache.
  g <- function() raster::raster(raster::extent(0, 2, 0, 2), resolution = 1, vals = 1:4)
  out1 <- Cache(g, cachePath = tmpCache)
  out2 <- Cache(g, cachePath = tmpCache)
  expect_s4_class(out1, "RasterLayer")
  expect_s4_class(out2, "RasterLayer")
  expect_equal(raster::values(out2), raster::values(out1))
})
