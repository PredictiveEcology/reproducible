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

## clearCache(cacheId = ) used to list the whole storage folder (45 s for 1.6M files), under the entry lock of
## Cache(useCache = "overwrite"). It now lists only the files of the ids; the same files must go, no others.
test_that("clearCache(cacheId = ) removes exactly the files of those ids, for few and for many ids", {
  testInit()
  tmpCache <- file.path(tmpdir, "cacheIds")
  nIds <- reproducible:::.maxIdsForPatternListing + 5L
  idFn <- function(v) v
  for (i in seq_len(nIds + 1L)) Cache(idFn, i, cachePath = tmpCache, verbose = -2)
  storage <- CacheStorageDir(tmpCache)
  ids <- unique(showCache(tmpCache, verbose = -2)$cacheId)
  expect_length(ids, nIds + 1L)
  ## files of an entry: `<id>.<ext>` (made by Cache) and `<id>_<name>` (file-backed copies);
  ## decoys that start with, or merely contain, an id must stay
  for (id in ids) file.create(file.path(storage, paste0(id, "_copy.tif")))
  decoys <- file.path(storage, c(paste0("x", ids[1], ".rds"), paste0(ids[1], "x.rds"), "unrelated.txt"))
  file.create(decoys)
  filesOf <- function(i) dir(storage, pattern = paste0("^(", paste(i, collapse = "|"), ")[._]"))
  before <- dir(storage)
  filesOfBefore1 <- filesOf(ids[1])

  clearCache(tmpCache, cacheId = ids[1], ask = FALSE, verbose = -2)
  expect_setequal(setdiff(before, dir(storage)), intersect(before, filesOfBefore1))
  expect_length(filesOf(ids[1]), 0L)
  expect_true(all(basename(decoys) %in% dir(storage)))
  expect_true(all(lengths(lapply(ids[-1], filesOf)) > 0L))

  keep <- ids[nIds + 1L]
  idsMany <- ids[2:nIds]   # more than .maxIdsForPatternListing
  expect_gt(length(idsMany), reproducible:::.maxIdsForPatternListing)
  clearCache(tmpCache, cacheId = idsMany, ask = FALSE, verbose = -2)
  expect_length(filesOf(idsMany), 0L)
  expect_true(length(filesOf(keep)) > 0L)
  expect_true(all(basename(decoys) %in% dir(storage)))
})
