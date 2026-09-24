## clearCache() must remove every file of an entry. File-backed copies are stored as `<cacheId>_<name>`
## (filenameInCacheWPrefix()), but since the speedup in 3e5b888b clearCache() recognised an entry's files only
## as `<cacheId>.<ext>`. Entries that also tag their files (a plain Cache() call) were still cleaned through the
## tags; entries that do not -- file-backed objects inside a cached SpaDES simList -- left their rasters behind
## (FireSense, 2026-09-24: 22 rasters of two cleared .inputObjects entries).

test_that("clearCache(cacheId = ) removes the entry's file-backed rasters", {
  skip_if_not_installed("terra")
  testInit("terra")
  tmpCache <- file.path(tmpdir, "cacheCC")
  mk <- function(v) {
    f <- file.path(tmpdir, paste0("fb_", v, "_4.2.1.tif"))
    terra::writeRaster(terra::rast(nrows = 5, ncols = 5, vals = v), f, overwrite = TRUE)
    terra::rast(f)
  }
  a <- Cache(mk, 1, cachePath = tmpCache)
  b <- Cache(mk, 2, cachePath = tmpCache)
  ids <- unique(showCache(tmpCache, verbose = -2)$cacheId)
  expect_length(ids, 2L)
  storage <- CacheStorageDir(tmpCache)
  ## a file-backed copy the entry does not tag, as in a cached simList
  untagged <- file.path(storage, paste0(ids[1], "_rstLCC1985_4.2.1.tif"))
  file.copy(dir(storage, pattern = paste0("^", ids[1], "_fb"), full.names = TRUE)[1], untagged)
  expect_true(file.exists(untagged))

  clearCache(tmpCache, cacheId = ids[1], ask = FALSE, verbose = -2)
  expect_length(dir(storage, pattern = paste0("^", ids[1])), 0L)                # nothing of the entry left
  expect_true(length(dir(storage, pattern = paste0("^", ids[2], "_"))) > 0)    # the other entry untouched
})
