## A Cache() nested inside another counts as nested whether or not either call has userTags.
## Before, the nesting level was only counted when some userTags were present, so a Cache(useCache
## = TRUE) inside an untagged Cache(useCache = FALSE) was cached, but skipped when the outer call
## had userTags; numeric useCache levels were miscounted the same way.

## Three nested Cache() levels, run once; returns how many of them were cached (entries made).
.nestCached <- function(outerUseCache, tags, cachePath) {
  ut <- function(x) if (tags) x else c()
  innerF <- function(x) x
  middleF <- function(x) Cache(innerF(x), useCache = TRUE, userTags = ut("inner"))
  outerF <- function(x) Cache(middleF(x), useCache = TRUE, userTags = ut("middle"))
  Cache(outerF(1), useCache = outerUseCache, userTags = ut("outer"))
  n <- length(unique(showCache(cachePath, verbose = -1)$cacheId))
  clearCache(cachePath, ask = FALSE, verbose = -1)
  n
}

test_that("a nested Cache(useCache = TRUE) takes an outer FALSE with or without userTags", {
  testInit(verbose = -1)
  withr::local_options(reproducible.cachePath = tmpCache)
  expect_equal(.nestCached(FALSE, tags = TRUE, tmpCache), 0)
  expect_equal(.nestCached(FALSE, tags = FALSE, tmpCache), 0)
})

test_that("numeric useCache levels count the same with or without userTags", {
  testInit(verbose = -1)
  withr::local_options(reproducible.cachePath = tmpCache)
  ## useCache = n caches the outer n levels
  for (lev in 0:3) {
    for (tags in c(TRUE, FALSE)) {
      expect_equal(.nestCached(lev, tags = tags, tmpCache), min(lev, 3),
                   info = paste("useCache =", lev, "; userTags:", tags))
    }
  }
})
