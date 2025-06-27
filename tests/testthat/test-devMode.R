test_that("test devMode", {
  testInit(opts = list(reproducible.useCache = "devMode"))

  clearCache(tmpCache, ask = FALSE)
  theTags <- "hiTest"
  centralTendency <- function(x) {
    mean(x)
  }
  funnyData <- c(1, 1, 1, 1, 10)
  uniqueUserTags <- c("thisIsUnique", "reallyUnique")
  ranNumsB <- Cache(centralTendency, funnyData,
    cachePath = tmpCache,
    userTags = uniqueUserTags
  ) # sets new value to Cache
  a <- showCache(tmpCache) # 1 unique artifact -- cacheId is 8be9cf2a072bdbb0515c5f0b3578f474
  expect_true(NROW(unique(a[[.cacheTableHashColName()]])) == 1)

  # During development, we often redefine function internals
  centralTendency <- function(x) {
    median(x)
  }
  # When we rerun, we don't want to keep the "old" cache because the function will
  #   never again be defined that way. Here, because of userTags being the same,
  #   it will replace the entry in the Cache, effetively overwriting it, even though
  #   it has a different cacheId
  ranNumsD <- Cache(centralTendency, funnyData, cachePath = tmpCache, userTags = uniqueUserTags) |>
    capture.output() -> oo
  a <- showCache(tmpCache) # 1 unique artifact -- cacheId is bb1195b40c8d37a60fd6004e5d526e6b
  expect_true(NROW(unique(a[[.cacheTableHashColName()]])) == 1)

  # If it finds it by cacheID, doesn't matter what the userTags are
  ranNumsD <- Cache(centralTendency, funnyData, cachePath = tmpCache, userTags = "thisIsUnique")
  a <- showCache(tmpCache) # 1 unique artifact -- cacheId is bb1195b40c8d37a60fd6004e5d526e6b
  expect_true(NROW(unique(a[[.cacheTableHashColName()]])) == 1)

  ###### If you don't use userTags -- it acts like normal
  ranNumsE <- Cache(centralTendency, 1:10, cachePath = tmpCache)
  centralTendency <- function(x) {
    sort(table(a))[1]
  }
  ranNumsF <- Cache(centralTendency, 1:10, cachePath = tmpCache)

  a <- showCache(tmpCache) # 1 unique artifact -- cacheId is bb1195b40c8d37a60fd6004e5d526e6b
  expect_true(NROW(unique(a[[.cacheTableHashColName()]])) == 3)

  centralTendency <- function(x) {
    median(x)
  }
  ranNumsG <- Cache(centralTendency, 1:11, cachePath = tmpCache, userTags = theTags)
  a <- showCache(tmpCache) # 1 unique artifact -- cacheId is bb1195b40c8d37a60fd6004e5d526e6b
  expect_true(NROW(unique(a[[.cacheTableHashColName()]])) == 4)

  centralTendency <- function(x) {
    sort(table(x))[1]
  }
  ranNumsH <- Cache(centralTendency, 1:11, cachePath = tmpCache, userTags = theTags) |>
    capture.output() -> oo

  a <- showCache(tmpCache) #
  expect_true(NROW(unique(a[[.cacheTableHashColName()]])) == 4)

  # Test multiple with same userTags, ie, not unambiguous
  opt <- options(reproducible.useCache = TRUE)
  ranNumsG <- Cache(centralTendency, 1:12, cachePath = tmpCache, userTags = theTags)
  options(opt)
  centralTendency <- function(x) median(x)
  mess <- capture_messages({
    ranNumsG <- Cache(centralTendency, 1:12, cachePath = tmpCache, userTags = theTags, verbose = 3)
  }) |>
    capture.output() -> oo
  if (!getOption("reproducible.useCacheV3") %in% TRUE)
    expect_true(any(grepl("not unique; defaulting", mess)))

  ### Test that vague userTags don't accidentally delete a non-similar entry
  clearCache(tmpCache, ask = FALSE)
  centralTendency <- function(x) {
    sort(table(x))[1]
  }
  ranNumsH <- Cache(centralTendency, 1:11, cachePath = tmpCache, userTags = theTags)
  ranNumsI <- Cache(rnorm, 15, cachePath = tmpCache, userTags = theTags)
  a <- showCache(tmpCache) # 2 unique artifacts because VERY different
  expect_true(length(unique(a[[.cacheTableHashColName()]])) == 2)
})
