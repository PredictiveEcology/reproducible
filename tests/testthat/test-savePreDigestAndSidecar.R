## Two option/state-gated paths that nothing else reaches.
##
## 1. Cache(reproducible.savePreDigest = TRUE) writes a SECOND cache entry, the
##    "preDigest", alongside the real one. It is a debugging aid for working out
##    why a cacheId changed, and the whole block is skipped unless the option is
##    on -- so it is invisible to every other test.
##
## 2. preProcess's checksum phase can skip re-verifying a file when a remote
##    hash sidecar (`.<file>_<url>.hash`) says it was already verified. That
##    branch builds a synthetic "OK" checksum row rather than re-hashing, and is
##    only reachable when such a sidecar exists on disk.
##
## No network, no Drive.

test_that("savePreDigest writes a second, preDigest-prefixed cache entry", {
  testInit()

  withr::local_options(reproducible.savePreDigest = TRUE,
                       reproducible.showCachePreWarm = FALSE,
                       reproducible.ask = FALSE)
  cp <- checkPath(file.path(tmpdir, "cache"), create = TRUE)
  fn <- function(a, b = 1) a + b

  out <- Cache(fn, a = 1, cachePath = cp, verbose = 0)

  ## The option must not change the answer -- it only adds a debugging artifact.
  expect_identical(as.numeric(out), 2)

  ## Two entries now: the result, and its preDigest twin.
  expect_identical(length(unique(showCache(cp, verbose = -2)$cacheId)), 2L)
  ## The twin is name-prefixed so it is distinguishable on disk.
  expect_true(any(grepl("preDigest_", dir(cp, recursive = TRUE))))
})

test_that("savePreDigest works on the DBI backend too", {
  skip_if_not_installed("RSQLite")
  testInit()

  withr::local_options(reproducible.useDBI = TRUE,
                       reproducible.savePreDigest = TRUE,
                       reproducible.showCachePreWarm = FALSE,
                       reproducible.ask = FALSE)
  skip_if_not(useDBI())
  cp <- checkPath(file.path(tmpdir, "cacheDBI"), create = TRUE)
  fn <- function(a, b = 1) a + b

  out <- Cache(fn, a = 1, cachePath = cp, verbose = 0)

  expect_identical(as.numeric(out), 2)
  expect_identical(length(unique(showCache(cp, verbose = -2)$cacheId)), 2L)
})

test_that("a remote-hash sidecar pre-verifies a file instead of re-checksumming", {
  testInit()

  dest <- checkPath(file.path(tmpdir, "dest"), create = TRUE)
  target <- file.path(dest, "a.txt")
  writeLines("payload", target)

  ## Written with the package's own helper so the naming convention cannot
  ## drift from what .findRemoteHashSidecars() looks for.
  url <- "https://example.com/some/path/a.txt"
  hashFile <- makeRemoteHashFile(url = url, destinationPath = dest,
                                 targetFile = "a.txt", remoteHash = "deadbeef",
                                 algorithm = "sha1", write = TRUE)
  expect_true(file.exists(hashFile))

  ## With the sidecar present and the file on disk, preProcess takes the
  ## pre-verified path: a synthetic "OK" row, no re-hashing, no download.
  out <- prepInputs(targetFile = "a.txt", destinationPath = dest,
                    fun = NA, verbose = -1)

  expect_identical(basename(as.character(out)), "a.txt")
  expect_identical(readLines(as.character(out), warn = FALSE), "payload")
})
