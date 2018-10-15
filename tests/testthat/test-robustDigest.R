test_that("test file-backed raster caching", {
  testInitOut <- testInit("raster", tmpFileExt = c(".tif", ".grd"))
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)


  # make sure that .robustDigest does not include the cache-added attributes
  c1 <- Cache(rnorm, 11, cacheRepo = tmpCache)
  c1a <- .robustDigest(c1)

  c1 <- Cache(rnorm, 11, cacheRepo = tmpCache)
  c1b <- .robustDigest(c1)
  expect_true(identical(c1a, c1b)) # failed pre reproducible 0.2.4.9000

  skip("Just benchmarking")
  microbenchmark::microbenchmark(c1c <- .robustDigest(seq(1e7)))



})
