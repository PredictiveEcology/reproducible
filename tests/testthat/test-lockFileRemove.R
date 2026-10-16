## Lock files are deleted when their lock is released (filelock >= 1.0.3.9002), so a cache does not
## keep one empty <cacheId>.lock and <cacheId>.tags.lock per entry for ever.

test_that("Cache() leaves no lock files behind when filelock can remove them", {
  skip_if_not(.filelockCanRemove(), "this filelock's unlock() has no `remove`")
  skip_on_os("windows") # filelock keeps the file there
  testInit(opts = list(reproducible.useMemoise = FALSE, reproducible.useDBI = FALSE))
  a <- Cache(rnorm, 3, cachePath = tmpCache)
  a <- Cache(rnorm, 3, cachePath = tmpCache) # a hit, which rewrites the tags
  expect_length(dir(CacheStorageDir(tmpCache), pattern = "\\.lock$"), 0L)
})

test_that("reproducible.lockFileRemove = FALSE keeps the lock files", {
  testInit(opts = list(reproducible.useMemoise = FALSE, reproducible.useDBI = FALSE,
                       reproducible.lockFileRemove = FALSE))
  a <- Cache(rnorm, 3, cachePath = tmpCache)
  expect_true(length(dir(CacheStorageDir(tmpCache), pattern = "\\.lock$")) > 0L)
})

test_that("checkSameCacheId() finds an entry's files by name", {
  d <- withr::local_tempdir()
  withr::local_options(reproducible.useDBI = FALSE)
  file.create(file.path(d, c("abc.qs2", "abc.dbFile.qs2", "abc.lock", "abc.tags.lock",
                             "abcd.rds", "xabc.qs2", "abc_r_1.tif")))
  ## the storage file only: tag files, locks, rasters and other entries are not it
  expect_identical(checkSameCacheId(file.path(d, "abc.rds")), "abc.qs2")
  file.create(file.path(d, "abc.rds"))
  expect_identical(checkSameCacheId(file.path(d, "abc.rds")), c("abc.qs2", "abc.rds"))
  expect_identical(checkSameCacheId(file.path(d, "zzz.qs2")), character(0))
})
