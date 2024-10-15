##########################
test_that("test reproducible.verbose", {
  testInit(verbose = 4, tmpFileExt = ".rds")

  warn <- capture_warnings(Cache(rnorm, 1, cachePath = tmpdir))
  expect_is(.reproEnv$cacheTimings, "data.frame") ##
  expect_true(NROW(.reproEnv$cacheTimings) == 4) ##
  expect_true(NCOL(.reproEnv$cacheTimings) == 4) ##

  # Test Path class objects
  a <- sample(1e4)

  saveRDS(a, file = tmpfile)
  out1 <- Cache(readRDS, tmpfile, cachePath = tmpdir)
  nam <- grep("hashDetails", names(.reproEnv), value = TRUE)
  out1Details <- .reproEnv[[nam]]
  ap <- asPath(tmpfile)
  out2 <- Cache(readRDS, ap, cachePath = tmpdir)
  out2Details <- .reproEnv[[nam]]

  # should be vastly larger when actual file, rather than just filename
  expect_true((20 * out1Details$objSize[1]) < out2Details$objSize[1]) ##
})
