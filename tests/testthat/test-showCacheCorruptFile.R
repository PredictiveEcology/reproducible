test_that("showCache survives a corrupt cache file (unknown input format)", {
  ## Regression for: when a cache directory contains a file whose extension
  ## says ".rds" but whose contents aren't valid RDS, loadFile()'s
  ## readRDS() throws "unknown input format". The recovery loop in
  ## showCache() expects loadFile() errors to be caught as `try-error`
  ## values; without the try() wrap, the error escaped and the entire
  ## showCache() call (and any Cache() that triggered showSimilar) blew up.
  skip_on_cran()

  tmpCache <- file.path(tempdir(), basename(tempfile("rcache_corrupt_")))
  dir.create(tmpCache, showWarnings = FALSE, recursive = TRUE)
  withr::local_options(reproducible.cachePath  = tmpCache,
                       reproducible.useMemoise = FALSE,
                       reproducible.verbose    = 0)

  ## Seed one valid cache entry so the dir actually has loadable content.
  Cache(rnorm, 1, cachePath = tmpCache, cacheId = "good_v1", useCloud = FALSE)

  ## Plant a corrupt ".dbFile.rds" file in the cache storage dir. showCache()
  ## scans dir() with pattern ".dbFile.qs2|.dbFile.rds|.dbFile.qs" and calls
  ## loadFile() on each match -- so the corrupt file must match that pattern
  ## to actually exercise the broken loadFile call site.
  storage <- reproducible::CacheStorageDir(tmpCache)
  corruptFile <- file.path(storage, "corruptCacheId_v1.dbFile.rds")
  writeBin(as.raw(c(0x00, 0x01, 0x02, 0x03)), corruptFile)
  expect_true(file.exists(corruptFile))

  ## Pre-condition: readRDS on this file errors with "unknown input format"
  expect_error(readRDS(corruptFile), regexp = "unknown input format")

  ## Core invariant: showCache() must NOT propagate the readRDS error. Before
  ## the fix, the unwrapped loadFile() call let "unknown input format" escape
  ## the lapplyFun() loop and crash any Cache()->showSimilar()->showCache()
  ## chain.
  out <- expect_no_error(
    suppressMessages(reproducible::showCache(tmpCache, verbose = -1))
  )
  expect_true(NROW(out) > 0,
              info = "Valid cache entries should still be returned alongside the bad one")
})
