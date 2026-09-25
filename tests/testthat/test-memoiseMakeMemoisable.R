## loadFromCache() and Cache() share one memoise environment, keyed by cacheId, but store different
## forms there: loadFromCache() stores makeMemoisable() of the unwrapped object, Cache() the wrapped
## object it read from disk. Each must read the other's form. SpaDES.core's cacheChaining jump calls
## loadFromCache() between Cache() hits on the same cacheIds.

test_that("a Cache() memoise hit after loadFromCache() returns the original class", {
  testInit(opts = list(reproducible.useMemoise = TRUE))

  ns <- asNamespace("reproducible")
  registerS3method("makeMemoisable", "memoToy",
                   function(x) structure(unclass(x), class = "memoToy_"), envir = ns)
  registerS3method("unmakeMemoisable", "memoToy_",
                   function(x) structure(unclass(x), class = "memoToy"), envir = ns)

  f <- function(n) structure(list(n = n), class = "memoToy")
  o1 <- Cache(f, 1, cachePath = tmpCache)
  cid <- gsub("cacheId:", "", attr(o1, "tags"))

  memEnv <- memoiseEnv(tmpCache)
  rm(list = cid, envir = memEnv) # as in a fresh session
  expect_s3_class(loadFromCache(tmpCache, cacheId = cid), "memoToy")
  expect_s3_class(get(cid, envir = memEnv), "memoToy_")

  o2 <- Cache(f, 1, cachePath = tmpCache)
  expect_s3_class(o2, "memoToy")
  expect_false(inherits(o2, "memoToy_"))
})

test_that("loadFromCache() after a Cache() memoise of the wrapped object returns it unwrapped", {
  testInit("terra", opts = list(reproducible.useMemoise = TRUE))

  f <- function(n) terra::rast(matrix(1:4 * n, 2))
  o1 <- Cache(f, 1, cachePath = tmpCache)
  cid <- gsub("cacheId:", "", attr(o1, "tags"))

  memEnv <- memoiseEnv(tmpCache)
  rm(list = cid, envir = memEnv) # as in a fresh session
  Cache(f, 1, cachePath = tmpCache) # a disk hit: memoises the wrapped object
  expect_false(is(get(cid, envir = memEnv), "SpatRaster"))

  x <- loadFromCache(tmpCache, cacheId = cid)
  expect_s4_class(x, "SpatRaster")
  expect_equal(terra::values(x), terra::values(o1))
})
