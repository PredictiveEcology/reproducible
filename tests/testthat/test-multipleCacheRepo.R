##########################
test_that("test multiple cachePath", {
  testInit()

  opt <- options("reproducible.cachePath" = c(tmpdir, tmpCache))

  a <- Cache(rnorm, 1, cachePath = tmpCache)
  suppressMessages(aCache <- showCache(tmpCache))
  expect_true(length(unique(aCache[[.cacheTableHashColName()]]))==1)

  b <- Cache(rnorm, 2, cachePath = tmpdir)
  suppressMessages(bCache <- showCache(tmpdir))
  expect_true(length(unique(bCache[[.cacheTableHashColName()]]))==1)

  d <- Cache(rnorm, 1, cachePath = c(tmpdir, tmpCache))
  suppressMessages(dCache <- showCache(tmpCache))
  expect_true(length(unique(dCache[[.cacheTableHashColName()]]))==1)

  f <- Cache(rnorm, 2, cachePath = c(tmpdir, tmpCache))
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
test_that("test multiple cachePath with 1 of them a cloudCache", {
  skip(message = "test cloudCache inside Cache -- Not fully written test")

  testInit(libraries = "googledrive", needGoogleDriveAuth = TRUE)

  newDir <- googledrive::drive_mkdir("testFolder")
  on.exit(googledrive::drive_rm(googledrive::as_id(newDir$id), add = TRUE))

  clearCache(ask = FALSE, cachePath = tmpCache)
  cloudSyncCache(cloudFolderID = newDir$id, cachePath = tmpCache)
  cloudCache(rnorm, 1, cloudFolderID = newDir$id, cachePath = tmpCache)
  mess <- capture_messages(cloudCache(rnorm, 1, cloudFolderID = newDir$id,
                                      cachePath = tmpCache))
  # for (i in 1:4) cloudCache(rnorm, 1e4 + i, cloudFolderID = newDir$id)
  expect_true(all(grepl("local and cloud|loading cached result", mess)))

  # cloudCache(rnorm, 1, cloudFolderID = "1JZoXm68NdegrkhKN3THXdXf2ZKeYJ34N")

  opt <- options("reproducible.cachePath" = list(tmpdir, googledrive::as_id(newDir$id)))
  #a1 <- Cache(rnorm, 1)

  on.exit(options(opt), add = TRUE)
})


