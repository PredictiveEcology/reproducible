## On a cache hit, unwrapSpatRaster() restores a file-backed raster to the path it was produced
## at when that path still exists (test-unwrapSpatRaster.R). For a raster produced in a temp
## directory that is not this session's tempdir() -- terra's tempdir on a scratch disk shared by
## many worker processes, or another R session's Rtmp* -- that path is named for the process
## that made it, and the live object dies with the next clean-up of that process's temp files
## (fireSense fits, 2026-09-12: "[project] cannot create dataset from source"). Such a raster
## must come back under the cache instead.

test_that("a raster produced in a foreign terra tempdir is restored under the cache, not there", {
  skip_if_not_installed("terra")
  testInit("terra", opts = list(
    "reproducible.showSimilar" = FALSE,
    "reproducible.useMemoise" = FALSE
  ))
  withr::local_options(reproducible.cachePath = tmpdir)

  ## a terra tempdir OUTSIDE this session's tempdir(), as a project sets for a shared scratch disk
  terraTmp <- file.path(dirname(tempdir()), paste0("terraScratch_", rndstr(1, 8)))
  dir.create(terraTmp)
  withr::defer(unlink(terraTmp, recursive = TRUE))
  oldTmp <- terra::terraOptions(print = FALSE)$tempdir
  terra::terraOptions(tempdir = terraTmp)
  withr::defer(terra::terraOptions(tempdir = oldTmp))

  ## what a worker's pipeline leaves behind: a temp file named for that worker's pid
  producedAt <- file.path(terraTmp, "spat_0123456789ab_424242_abcdefghijk.tif")
  mk <- function(val) {
    terra::writeRaster(terra::rast(nrows = 10, ncols = 10, vals = val), producedAt, overwrite = TRUE)
  }

  onMiss <- Cache(mk(111), .functionName = "mkScratch")
  expect_equal(terra::values(onMiss)[1], 111)

  onHit <- Cache(mk(111), .functionName = "mkScratch")
  expect_equal(terra::values(onHit)[1], 111)
  expect_false(startsWith(normPath(terra::sources(onHit)), normPath(terraTmp)))
  expect_true(startsWith(normPath(terra::sources(onHit)), normPath(tmpdir)))

  ## the decisive property: the producing process's temp file goes away, the object survives
  unlink(producedAt)
  expect_equal(terra::values(onHit)[1], 111)
})

test_that(".isForeignTempPath: own tempdir is not foreign; other Rtmp and terra tempdir are", {
  skip_if_not_installed("terra")
  own <- file.path(tempdir(), "x.tif")
  otherRtmp <- file.path(dirname(tempdir()), "RtmpABC123", "x.tif")
  expect_false(.isForeignTempPath(own))
  expect_true(.isForeignTempPath(otherRtmp))
  expect_false(.isForeignTempPath("/data/project/inputs/x.tif"))
  expect_identical(.isForeignTempPath(c(own, otherRtmp)), c(FALSE, TRUE))
})
