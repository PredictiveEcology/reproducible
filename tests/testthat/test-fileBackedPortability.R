# Portability of file-backed objects (e.g. SpatRaster) across machines/users.
#
# A file-backed raster embeds an *absolute* path to its backing .tif. When a
# cache entry is shared (e.g. cloud cache) and retrieved by another user, that
# absolute path (e.g. /home/<producer>/.../inputs/x.tif) does not exist on the
# receiver. The fix stores the backing file's location *relative* to a named,
# machine-independent anchor (reproducible.fileBackedAnchors, e.g. SpaDES
# paths(sim)) and rebuilds it under the receiver's own anchor on load. When no
# anchor resolves, the object is made self-contained under the receiver's cache
# rather than resurrecting the producer's path.

## Relocate a whole cache (DB + storage dir) from one cachePath to another, as a
##   cloud download effectively does.
relocateCache <- function(fromCache, toCache) {
  dir.create(toCache, recursive = TRUE, showWarnings = FALSE)
  file.copy(list.files(fromCache, full.names = TRUE), toCache, recursive = TRUE)
  fromStore <- reproducible:::CacheStorageDir(fromCache)
  toStore <- reproducible:::CacheStorageDir(toCache)
  dir.create(toStore, recursive = TRUE, showWarnings = FALSE)
  file.copy(list.files(fromStore, full.names = TRUE), toStore, recursive = TRUE)
}

test_that("file-backed raster is restored under the receiver's anchor", {
  skip_if_not_installed("terra")
  testInit("terra")
  cId <- "abcabcabcabcabcabcabcabcabcabc01"

  ## ---- producer ----
  prodIn <- normPath(file.path(tmpdir, "producer", "inputs"))
  prodCache <- normPath(file.path(tmpdir, "producer", "cache"))
  dir.create(prodIn, recursive = TRUE, showWarnings = FALSE)
  tif <- file.path(prodIn, "rstLCC.tif")
  terra::writeRaster(terra::rast(nrows = 8, ncols = 8, vals = 1:64), tif, overwrite = TRUE)
  rr <- terra::rast(tif)

  withr::local_options(reproducible.fileBackedAnchors = list(
    inputPath = prodIn, cachePath = prodCache
  ))
  # NB: the cache lookup keys on the function's name; producer and receiver must
  #   use the same symbol (`f`) so the relocated entry is found by `cacheId`.
  f <- function() rr
  Cache(f, cachePath = prodCache, cacheId = cId)

  ## ---- transport: relocate cache, delete producer entirely ----
  consIn <- normPath(file.path(tmpdir, "consumer", "inputs"))
  consCache <- normPath(file.path(tmpdir, "consumer", "cache"))
  dir.create(consIn, recursive = TRUE, showWarnings = FALSE)
  relocateCache(prodCache, consCache)
  unlink(file.path(tmpdir, "producer"), recursive = TRUE)

  ## ---- receiver: different inputPath, producer paths gone ----
  withr::local_options(reproducible.fileBackedAnchors = list(
    inputPath = consIn, cachePath = consCache
  ))
  f <- function() stop("must not recompute")
  out <- Cache(f, cachePath = consCache, cacheId = cId)

  expect_true(inherits(out, "SpatRaster"))
  expect_identical(as.numeric(out[64][1, 1]), 64)
  # restored under the RECEIVER's inputPath, never the producer's tree
  expect_true(startsWith(normPath(terra::sources(out)), consIn))
  expect_false(dir.exists(file.path(tmpdir, "producer")))
})

test_that("file-backed raster with an unresolved anchor falls back to the cache", {
  skip_if_not_installed("terra")
  testInit("terra")
  cId <- "abcabcabcabcabcabcabcabcabcabc02"

  ## producer stores relative to a named anchor the receiver will NOT have
  prodScratch <- normPath(file.path(tmpdir, "producer", "scratch"))
  prodCache <- normPath(file.path(tmpdir, "producer", "cache"))
  dir.create(prodScratch, recursive = TRUE, showWarnings = FALSE)
  tif <- file.path(prodScratch, "rstLCC.tif")
  terra::writeRaster(terra::rast(nrows = 8, ncols = 8, vals = 1:64), tif, overwrite = TRUE)
  rr <- terra::rast(tif)

  withr::local_options(reproducible.fileBackedAnchors = list(
    scratchPath = prodScratch, cachePath = prodCache
  ))
  f <- function() rr
  Cache(f, cachePath = prodCache, cacheId = cId)

  consCache <- normPath(file.path(tmpdir, "consumer", "cache"))
  relocateCache(prodCache, consCache)
  unlink(file.path(tmpdir, "producer"), recursive = TRUE)

  ## receiver has no scratchPath anchor -> must not crash, must be self-contained
  withr::local_options(reproducible.fileBackedAnchors = list(cachePath = consCache))
  f <- function() stop("must not recompute")
  out <- Cache(f, cachePath = consCache, cacheId = cId)

  expect_true(inherits(out, "SpatRaster"))
  expect_identical(as.numeric(out[64][1, 1]), 64)
  expect_true(startsWith(normPath(terra::sources(out)), consCache))
  expect_false(dir.exists(file.path(tmpdir, "producer")))
})
