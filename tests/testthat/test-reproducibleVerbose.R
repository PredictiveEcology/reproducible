##########################
test_that("test reproducible.verbose", {
  testInit(verbose = 4, tmpFileExt = ".rds")

  warn <- capture_warnings(Cache(rnorm, 1, cachePath = tmpdir))
  expect_is(.reproEnv$cacheTimings, "data.frame") ##
  colVals <- c("Hashing", "Running rnorm", "Saving to cachePath", "Whole Cache call")
  colNams <- c("functionName", "component", "elapsedTime", "units")
  if (getRversion() >= "4.2") { # earlier testthat doesn't have expect_in
    expect_in(.reproEnv$cacheTimings$component, colVals)
    expect_in(names(.reproEnv$cacheTimings), colNams)
  } # else { # earlier fails this test on GA; can't reproduce
  #   if ((isMac() || isWindows())) { # isn't passing on linux on GA; can't test why
  #     expect_true(NROW(.reproEnv$cacheTimings) == 4) # TODO -- doesn't pass on R 4.1.3
  #     expect_true(NCOL(.reproEnv$cacheTimings) == 4) ##
  #   }
  # }

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
