test_that("test devMode", {
  testInitOut <- testInit("raster", tmpFileExt = c(".tif", ".grd"),
                          opts = list("reproducible.useCache" = "devMode",
                                      "reproducible.useNewDigestAlgorithm" = TRUE))
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  clearCache(tmpdir, ask = FALSE)
  theTags <- "hiTest"
  centralTendency <- function(x)
     mean(x)
  funnyData <- c(1, 1, 1, 1, 10)
  uniqueUserTags <- c("thisIsUnique", "reallyUnique")
  ranNumsB <- Cache(centralTendency, funnyData, cacheRepo = tmpdir,
                    userTags = uniqueUserTags) # sets new value to Cache
  a <- showCache(tmpdir) # 1 unique artifact -- cacheId is 8be9cf2a072bdbb0515c5f0b3578f474
  expect_true(NROW(unique(a$artifact)) == 1)

  # During development, we often redefine function internals
  centralTendency <- function(x)
    median(x)
  # When we rerun, we don't want to keep the "old" cache because the function will
  #   never again be defined that way. Here, because of userTags being the same,
  #   it will replace the entry in the Cache, effetively overwriting it, even though
  #   it has a different cacheId
  ranNumsD <- Cache(centralTendency, funnyData, cacheRepo = tmpdir, userTags = uniqueUserTags)
  a <- showCache(tmpdir) # 1 unique artifact -- cacheId is bb1195b40c8d37a60fd6004e5d526e6b
  expect_true(NROW(unique(a$artifact)) == 1)

  # If it finds it by cacheID, doesn't matter what the userTags are
  ranNumsD <- Cache(centralTendency, funnyData, cacheRepo = tmpdir, userTags = "thisIsUnique")
  a <- showCache(tmpdir) # 1 unique artifact -- cacheId is bb1195b40c8d37a60fd6004e5d526e6b
  expect_true(NROW(unique(a$artifact)) == 1)

  ###### If you don't use userTags -- it acts like normal
  ranNumsE <- Cache(centralTendency, 1:10, cacheRepo = tmpdir)
  centralTendency <- function(x)
    sort(table(a))[1]
  ranNumsF <- Cache(centralTendency, 1:10, cacheRepo = tmpdir)

  a <- showCache(tmpdir) # 1 unique artifact -- cacheId is bb1195b40c8d37a60fd6004e5d526e6b
  expect_true(NROW(unique(a$artifact)) == 3)

  centralTendency <- function(x)
    median(x)
  ranNumsG <- Cache(centralTendency, 1:11, cacheRepo = tmpdir, userTags = theTags)
  a <- showCache(tmpdir) # 1 unique artifact -- cacheId is bb1195b40c8d37a60fd6004e5d526e6b
  expect_true(NROW(unique(a$artifact)) == 4)

  centralTendency <- function(x)
    sort(table(x))[1]
  ranNumsH <- Cache(centralTendency, 1:11, cacheRepo = tmpdir, userTags = theTags)

  a <- showCache(tmpdir) # 1 unique artifact -- cacheId is bb1195b40c8d37a60fd6004e5d526e6b
  expect_true(NROW(unique(a$artifact)) == 4)

  # Test multiple with same userTags, ie, not unambiguous
  opt <- options("reproducible.useCache" = TRUE)
  ranNumsG <- Cache(centralTendency, 1:12, cacheRepo = tmpdir, userTags = theTags)
  options(opt)
  centralTendency <- function(x)
    median(x)
  mess <- capture_messages(ranNumsG <- Cache(centralTendency, 1:12, cacheRepo = tmpdir,
                                     userTags = theTags, verbose = 1))
  expect_true(any(grepl("not unique; defaulting", mess)))

  ### Test that vague userTags don't accidentally delete a non-similar entry
  clearCache(tmpdir, ask = FALSE)
  centralTendency <- function(x)
    sort(table(x))[1]
  ranNumsH <- Cache(centralTendency, 1:11, cacheRepo = tmpdir, userTags = theTags)
  ranNumsI <- Cache(rnorm, 15, cacheRepo = tmpdir, userTags = theTags)
  a <- showCache(tmpdir) # 2 unique artifacts because VERY different
  expect_true(length(unique(a$artifact)) == 2)

})
