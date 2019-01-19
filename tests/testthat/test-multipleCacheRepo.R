##########################
test_that("test multiple cacheRepo", {
  testInitOut <- testInit()
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  opt <- options("reproducible.cachePath" = c(tmpdir, tmpCache))

  a <- Cache(rnorm, 1, cacheRepo = tmpCache)
  suppressMessages(aCache <- showCache(tmpCache))
  expect_true(length(unique(aCache$artifact))==1)

  b <- Cache(rnorm, 2, cacheRepo = tmpdir)
  suppressMessages(bCache <- showCache(tmpdir))
  expect_true(length(unique(bCache$artifact))==1)

  d <- Cache(rnorm, 1, cacheRepo = c(tmpdir, tmpCache))
  suppressMessages(dCache <- showCache(tmpCache))
  expect_true(length(unique(dCache$artifact))==1)

  f <- Cache(rnorm, 2, cacheRepo = c(tmpdir, tmpCache))
  suppressMessages(fCache <- showCache(tmpdir))
  expect_true(length(unique(fCache$artifact))==1)

  d <- Cache(rnorm, 1)
  suppressMessages(dCache <- showCache())
  expect_true(length(unique(dCache$artifact))==1)

  f <- Cache(rnorm, 2)
  suppressMessages(fCache <- showCache())
  expect_true(length(unique(fCache$artifact))==1)

  on.exit(options(opt), add = TRUE)
})


##########################
test_that("test multiple cacheRepo with 1 of them a cloudCache", {
  if (!interactive()) skip(message = "test cloudCache inside Cache")
  testInitOut <- testInit(libraries = "googledrive")
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  options("reproducible.useNewDigestAlgorithm" = TRUE) # need new approach for this to work correctly
  # first time -- looks in cloudFolderID for checksums -- none there, so it makes it
  #   then it runs the function, caching locally, and uploading to cloud -- copy exists in
  #   2 places
  newDir <- drive_mkdir("testFolder")
  on.exit(drive_rm(as_id(newDir$id), add = TRUE))

  opt <- options("reproducible.cachePath" = list(tmpdir, as_id(newDir$id)))
  a1 <- Cache(rnorm, 1)

  on.exit(options(opt), add = TRUE)
})


