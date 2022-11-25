##########################
test_that("test multiple cacheRepo", {
  testInitOut <- testInit()
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  opt <- options("reproducible.cachePath" = c(tmpdir, tmpCache))

  a <- Cache(rnorm, 1, cacheRepo = tmpCache)
  suppressMessages(aCache <- showCache(tmpCache))
  expect_true(length(unique(aCache[[.cacheTableHashColName()]]))==1)

  b <- Cache(rnorm, 2, cacheRepo = tmpdir)
  suppressMessages(bCache <- showCache(tmpdir))
  expect_true(length(unique(bCache[[.cacheTableHashColName()]]))==1)

  d <- Cache(rnorm, 1, cacheRepo = c(tmpdir, tmpCache))
  suppressMessages(dCache <- showCache(tmpCache))
  expect_true(length(unique(dCache[[.cacheTableHashColName()]]))==1)

  f <- Cache(rnorm, 2, cacheRepo = c(tmpdir, tmpCache))
  suppressMessages(fCache <- showCache(tmpdir))
  expect_true(length(unique(fCache[[.cacheTableHashColName()]]))==1)

  d <- Cache(rnorm, 1)
  suppressMessages(dCache <- showCache())
  expect_true(length(unique(dCache[[.cacheTableHashColName()]]))==1)

  f <- Cache(rnorm, 2)
  suppressMessages({
    fCache <- showCache()
  })
  expect_true(length(unique(fCache[[.cacheTableHashColName()]])) == 1)

  on.exit(options(opt), add = TRUE)
})

##########################
test_that("test multiple cacheRepo with 1 of them a cloudCache", {
  skip(message = "test cloudCache inside Cache -- Not fully written test")
  if (!requireNamespace("googledrive", quietly = TRUE))
    stop(requireNamespaceMsg("googledrive", "to use google drive files"))
  #if (!interactive())
  testInitOut <- testInit(libraries = "googledrive")
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  options("reproducible.useNewDigestAlgorithm" = 2) # need new approach for this to work correctly
  # first time -- looks in cloudFolderID for checksums -- none there, so it makes it
  #   then it runs the function, caching locally, and uploading to cloud -- copy exists in
  #   2 places
  newDir <- googledrive::drive_mkdir("testFolder")
  on.exit(googledrive::drive_rm(googledrive::as_id(newDir$id), add = TRUE))

  clearCache(ask = FALSE, cacheRepo = tmpCache)
  cloudSyncCache(cloudFolderID = newDir$id, cacheRepo = tmpCache)
  cloudCache(rnorm, 1, cloudFolderID = newDir$id, cacheRepo = tmpCache)
  mess <- capture_messages(cloudCache(rnorm, 1, cloudFolderID = newDir$id,
                                      cacheRepo = tmpCache))
  # for (i in 1:4) cloudCache(rnorm, 1e4 + i, cloudFolderID = newDir$id)
  expect_true(all(grepl("local and cloud|loading cached result", mess)))

  # cloudCache(rnorm, 1, cloudFolderID = "1JZoXm68NdegrkhKN3THXdXf2ZKeYJ34N")

  opt <- options("reproducible.cachePath" = list(tmpdir, googledrive::as_id(newDir$id)))
  #a1 <- Cache(rnorm, 1)

  on.exit(options(opt), add = TRUE)
})


