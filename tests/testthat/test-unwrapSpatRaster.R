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

test_that("restore still separates calls when the original files are gone", {
  skip_if_not_installed("terra")
  testInit("terra", opts = list(
    "reproducible.showSimilar" = FALSE,
    "reproducible.useMemoise" = FALSE
  ))
  withr::local_options(reproducible.cachePath = tmpdir)

  dpA <- withr::local_tempdir("goneA")
  dpB <- withr::local_tempdir("goneB")

  mk <- function(val, dp) {
    list(terra::writeRaster(
      terra::rast(nrows = 10, ncols = 10, vals = val),
      file.path(dp, "1.tif"), overwrite = TRUE
    ))
  }

  Cache(mk(111, dpA), .functionName = "A")
  Cache(mk(222, dpB), .functionName = "B")

  ## Intermediates are routinely deleted while the cache is kept. With the originals gone the
  ## restore cannot put them back where they came from and must fall back to the cache -- which
  ## is exactly where a bare basename would make two cached calls share one file.
  unlink(c(file.path(dpA, "1.tif"), file.path(dpB, "1.tif")))

  a <- Cache(mk(111, dpA), .functionName = "A")
  b <- Cache(mk(222, dpB), .functionName = "B")
  expect_equal(terra::values(a[[1]])[1], 111)
  expect_equal(terra::values(b[[1]])[1], 222)
  expect_false(identical(terra::sources(a[[1]]), terra::sources(b[[1]])))
})
