## cache_Id_Identical() links a big result to an identical one already in the cache. In the multifile
## backend it used to call showCache(), which reads every entry's tag file: minutes on every big miss
## in a cache of ~230k entries. It now uses the resultHash index beside cacheOutputs.

test_that("an identical big result is linked without reading the whole cache (multifile)", {
  testInit(verbose = TRUE, opts = list(reproducible.useMemoise = FALSE, reproducible.useDBI = FALSE))
  N <- .objectSizeMinForBig / 4
  f1 <- function(n) {
    set.seed(1)
    sample(n)
  }
  f2 <- function(n) {
    2 # a different function, so a different cacheId, with the same result
    set.seed(1)
    sample(n)
  }
  a <- Cache(f1, N, cachePath = tmpCache)
  expect_length(dir(CacheResultHashDir(tmpCache)), 1L)

  testthat::local_mocked_bindings(showCache = function(...) stop("showCache() must not be called"))
  mess <- capture_messages(b <- Cache(f2, N, cachePath = tmpCache))
  expect_true(any(cli::ansi_grepl("A file with identical", mess)))
  expect_identical(as.vector(a), as.vector(b))
  expect_length(dir(CacheResultHashDir(tmpCache)), 1L) # the index keeps the first copy
})

test_that("a stale resultHash index entry is ignored (multifile)", {
  testInit(verbose = TRUE, opts = list(reproducible.useMemoise = FALSE, reproducible.useDBI = FALSE))
  N <- .objectSizeMinForBig / 4
  f1 <- function(n) {
    set.seed(1)
    sample(n)
  }
  f3 <- function(n) {
    3
    set.seed(1)
    sample(n)
  }
  a <- Cache(f1, N, cachePath = tmpCache)
  clearCache(tmpCache, userTags = gsub("cacheId:", "", attr(a, "tags")), ask = FALSE)
  expect_length(dir(CacheResultHashDir(tmpCache)), 1L) # now points at a cleared entry
  mess <- capture_messages(b <- Cache(f3, N, cachePath = tmpCache))
  expect_false(any(cli::ansi_grepl("A file with identical", mess)))
  expect_identical(as.vector(a), as.vector(b))

  clearCache(tmpCache, ask = FALSE)
  expect_false(dir.exists(CacheResultHashDir(tmpCache)))
})
