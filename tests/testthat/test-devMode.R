test_that("test file-backed raster caching", {
  testInitOut <- testInit("raster", tmpFileExt = c(".tif", ".grd"),
                          opts = list("reproducible.devMode" = TRUE,
                                      "reproducible.useNewDigestAlgorithm" = TRUE))
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  opt <- options("reproducible.devMode" = TRUE)
  clearCache(tmpDir, ask = FALSE)
  centralTendency <- function(x)
    mean(x)
  funnyData <- c(1,1,1,1,10)
  uniqueUserTags <- c("thisIsUnique", "reallyUnique")
  ranNumsB <- Cache(centralTendency, funnyData, cacheRepo = tmpDir,
                    userTags = uniqueUserTags) # sets new value to Cache
  a <- showCache(tmpDir) # 1 unique artifact -- cacheId is 8be9cf2a072bdbb0515c5f0b3578f474
  expect_true(NROW(unique(a$artifact)) == 1)

  # During development, we often redefine function internals
  centralTendency <- function(x)
    median(x)
  # When we rerun, we don't want to keep the "old" cache because the function will
  #   never again be defined that way. Here, because of userTags being the same,
  #   it will replace the entry in the Cache, effetively overwriting it, even though
  #   it has a different cacheId
  ranNumsD <- Cache(centralTendency, funnyData, cacheRepo = tmpDir, userTags = uniqueUserTags) # sets new value to Cache
  a <- showCache(tmpDir) # 1 unique artifact -- cacheId is bb1195b40c8d37a60fd6004e5d526e6b
  expect_true(NROW(unique(a$artifact)) == 1)

  # If it finds it by cacheID, doesn't matter what the userTags are
  ranNumsD <- Cache(centralTendency, funnyData, cacheRepo = tmpDir, userTags = "thisIsUnique") # sets new value to Cache
  a <- showCache(tmpDir) # 1 unique artifact -- cacheId is bb1195b40c8d37a60fd6004e5d526e6b
  expect_true(NROW(unique(a$artifact)) == 1)

  ###### If you don't use userTags -- it acts like normal
  ranNumsE <- Cache(centralTendency, 1:10, cacheRepo = tmpDir)
  centralTendency <- function(x)
    sort(table(a))[1]
  ranNumsF <- Cache(centralTendency, 1:10, cacheRepo = tmpDir)

  a <- showCache(tmpDir) # 1 unique artifact -- cacheId is bb1195b40c8d37a60fd6004e5d526e6b
  expect_true(NROW(unique(a$artifact)) == 3)

  theTags <- "hiTest"
  centralTendency <- function(x)
    median(x)
  ranNumsG <- Cache(centralTendency, 1:11, cacheRepo = tmpDir, userTags = theTags)
  a <- showCache(tmpDir) # 1 unique artifact -- cacheId is bb1195b40c8d37a60fd6004e5d526e6b
  expect_true(NROW(unique(a$artifact)) == 4)

  centralTendency <- function(x)
    sort(table(a))[1]
  ranNumsH <- Cache(centralTendency, 1:11, cacheRepo = tmpDir, userTags = theTags)

  a <- showCache(tmpDir) # 1 unique artifact -- cacheId is bb1195b40c8d37a60fd6004e5d526e6b
  expect_true(NROW(unique(a$artifact)) == 4)

  options(opt)

})
