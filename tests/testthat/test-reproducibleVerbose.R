##########################
test_that("test reproducible.verbose", {
  testInitOut <- testInit("raster", verbose = 4, tmpFileExt = ".rds")
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  warn <- capture_warnings(Cache(rnorm, 1, cachePath = tmpdir))
  expect_is(.reproEnv$cacheTimings, "data.frame") ##
  expect_true(NROW(.reproEnv$cacheTimings) == 4)  ##
  expect_true(NCOL(.reproEnv$cacheTimings) == 4)  ##

  # Test Path class objects
  a <- sample(1e4)

  saveRDS(a, file = tmpfile)
  out1 <- Cache(readRDS, tmpfile, cachePath = tmpdir)
  out1Details <- .reproEnv$hashDetailsAll
  ap <- asPath(tmpfile)
  out2 <- Cache(readRDS, ap, cachePath = tmpdir)
  out2Details <- .reproEnv$hashDetailsAll

  # should be vastly larger when actual file, rather than just filename
  out <- capture_messages(messageDF(rbind(out1Details, out2Details)))
  out[1] <- paste0("0:", out[1])
  cat(out, file = "~/tmp.txt", sep = "\n")
  expect_true( (20*out1Details$objSize[1]) < out2Details$objSize[1]) ##
})
