## unwrapSpatRaster() restores a file-backed SpatRaster from the cache. It used to unlink the
## original file BEFORE computing the destination, which broke remapFilenames(): with no resolvable
## anchor, that function decides whether the original location is still usable by testing
## `is_absolute_path(x) && file.exists(x)` -- on the very files the unlink had just removed. The test
## always failed, so the destination fell back to `file.path(cachePath, basename(x))`, dropping both
## the original directory and the cacheId. Two cached calls whose rasters happened to share a
## basename then overwrote each other in the cache root, and one silently returned the other's data.

test_that("unwrapSpatRaster restores file-backed rasters to their original path", {
  skip_if_not_installed("terra")
  testInit("terra", opts = list(
    "reproducible.showSimilar" = FALSE,
    "reproducible.useMemoise" = FALSE
  ))
  withr::local_options(reproducible.cachePath = tmpdir)

  ## deliberately OUTSIDE the cachePath: the fallback that this test guards against only engages
  ## for a raster whose location has no resolvable anchor relative to the cache.
  dp <- withr::local_tempdir("mapsElsewhere")

  mk <- function(val) {
    terra::writeRaster(
      terra::rast(nrows = 10, ncols = 10, vals = val),
      file.path(dp, "layer.tif"), overwrite = TRUE
    )
  }

  onMiss <- Cache(mk(111), .functionName = "mkOne")
  expect_equal(terra::values(onMiss)[1], 111)

  onHit <- Cache(mk(111), .functionName = "mkOne")
  expect_equal(terra::values(onHit)[1], 111)
  ## restored where it was produced, not dumped in the cache root
  expect_identical(normPath(terra::sources(onHit)), normPath(file.path(dp, "layer.tif")))
  expect_true(file.exists(file.path(dp, "layer.tif")))
  expect_false(file.exists(file.path(tmpdir, "layer.tif"))) ## not dumped in the cache root
})

test_that("two cached calls sharing a raster basename do not collide", {
  skip_if_not_installed("terra")
  testInit("terra", opts = list(
    "reproducible.showSimilar" = FALSE,
    "reproducible.useMemoise" = FALSE
  ))
  withr::local_options(reproducible.cachePath = tmpdir)

  ## both OUTSIDE the cachePath, and unrelated to each other
  dpA <- withr::local_tempdir("elsewhereA")
  dpB <- withr::local_tempdir("elsewhereB")

  ## same basename, different directories, different values, different cacheIds
  mk <- function(val, dp) {
    list(terra::writeRaster(
      terra::rast(nrows = 10, ncols = 10, vals = val),
      file.path(dp, "1.tif"), overwrite = TRUE
    ))
  }

  a1 <- Cache(mk(111, dpA), .functionName = "A")
  b1 <- Cache(mk(222, dpB), .functionName = "B")
  expect_equal(terra::values(a1[[1]])[1], 111)
  expect_equal(terra::values(b1[[1]])[1], 222)

  ## the restore path is where they used to clobber one another
  a2 <- Cache(mk(111, dpA), .functionName = "A")
  b2 <- Cache(mk(222, dpB), .functionName = "B")
  expect_equal(terra::values(a2[[1]])[1], 111)
  expect_equal(terra::values(b2[[1]])[1], 222)
  expect_false(identical(terra::sources(a2[[1]]), terra::sources(b2[[1]])))

  ## and A stays A after B has also been restored
  a3 <- Cache(mk(111, dpA), .functionName = "A")
  expect_equal(terra::values(a3[[1]])[1], 111)
})

test_that(".wrap(copyFiles = TRUE) makes a file-backed SpatRaster self-contained in cachePath", {
  skip_if_not_installed("terra")
  testInit("terra")

  src <- withr::local_tempdir("srcDir")
  dest <- withr::local_tempdir("destDir")
  tif <- file.path(src, "layer.tif")
  terra::writeRaster(terra::rast(nrows = 3, ncols = 2, vals = 1:6), tif)
  rr <- list(r = terra::rast(tif), n = 1)

  w <- .wrap(rr, filebackedPath = dest, copyFiles = TRUE)
  rds <- file.path(dest, "x.rds")
  saveRDS(w, rds)
  unlink(src, recursive = TRUE) ## original is gone

  back <- .unwrap(readRDS(rds), filebackedPath = dest)
  expect_equal(terra::values(back$r)[, 1], 1:6)
  expect_identical(back$n, 1)

  ## default does not copy anything
  dest2 <- withr::local_tempdir("destDir2")
  dir.create(src)
  terra::writeRaster(terra::rast(nrows = 3, ncols = 2, vals = 1:6), tif)
  .wrap(terra::rast(tif), filebackedPath = dest2)
  expect_false(dir.exists(file.path(dest2, "cacheOutputs")))
})

test_that(".wrap/.unwrap: `filebackedPath` is the argument; `cachePath` still works with a message", {
  skip_if_not_installed("terra")
  testInit("terra")

  src <- withr::local_tempdir("srcDir")
  dest <- withr::local_tempdir("destDir")
  tif <- file.path(src, "layer.tif")
  terra::writeRaster(terra::rast(nrows = 3, ncols = 2, vals = 1:6), tif)
  r <- terra::rast(tif)

  expect_no_message(w <- .wrap(r, filebackedPath = dest, copyFiles = TRUE))
  unlink(src, recursive = TRUE)
  expect_no_message(back <- .unwrap(w, filebackedPath = dest))
  expect_equal(terra::values(back)[, 1], 1:6)

  ## old name: same result, silently (deprecation message to come once downstream packages use the new name)
  dir.create(src)
  terra::writeRaster(terra::rast(nrows = 3, ncols = 2, vals = 1:6), tif)
  expect_no_message(w2 <- .wrap(terra::rast(tif), cachePath = dest, copyFiles = TRUE))
  unlink(src, recursive = TRUE)
  expect_no_message(back2 <- .unwrap(w2, cachePath = dest))
  expect_equal(terra::values(back2)[, 1], 1:6)

  ## list and environment methods too
  expect_no_message(.unwrap(.wrap(list(a = 1), cachePath = dest), cachePath = dest))
})
