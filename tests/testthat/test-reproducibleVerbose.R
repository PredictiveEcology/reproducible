##########################
test_that("test reproducible.verbose", {
  testInitOut <- testInit("raster", verbose = 2, tmpFileExt = ".rds")
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  warn <- capture_warnings(Cache(rnorm, 1, cacheRepo = tmpdir))
  expect_is(.reproEnv$cacheTimings, "data.frame") ##
  expect_true(NROW(.reproEnv$cacheTimings) == 4)  ##
  expect_true(NCOL(.reproEnv$cacheTimings) == 4)  ##

  # Test Path class objects
  a <- sample(1e4)

  saveRDS(a, file = tmpfile)
  out1 <- Cache(readRDS, tmpfile, cacheRepo = tmpdir)
  out1Details <- .reproEnv$hashDetailsAll
  out2 <- Cache(readRDS, asPath(tmpfile), cacheRepo = tmpdir)
  out2Details <- .reproEnv$hashDetailsAll

  # should be vastly larger when actual file, rather than just filename
  expect_true( (20*out1Details$objSize[1]) < out2Details$objSize[1]) ##
})
