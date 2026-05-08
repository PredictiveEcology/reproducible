test_that(".installAsyncShownCache preserves the per-cachePath env shape", {
  ## Regression for the bug where collect_showCache_async() overwrote
  ## pkgEnv[["shownCache"]][[x]] with the raw `sc` data.table, causing the
  ## next synchronous showCache() call to crash at
  ## `is.null(scEnv$FileInfo)` with "$ operator is invalid for atomic vectors".
  pe <- new.env(parent = emptyenv())
  fakeSc <- data.frame(cacheId = "abc", tagKey = "k", tagValue = "v",
                       stringsAsFactors = FALSE)
  reproducible:::.installAsyncShownCache(pe, "/some/cache/path", fakeSc)

  outer <- pe[["shownCache"]]
  expect_true(is.environment(outer))

  scEnv <- outer[["/some/cache/path"]]
  expect_true(is.environment(scEnv))

  ## Sync path uses these two slots; sc must round-trip, FileInfo stays NULL
  ## so the sync path takes the "newOnes <- curFileInfo" branch on its first
  ## subsequent call.
  expect_identical(scEnv$sc, fakeSc)
  expect_null(scEnv$FileInfo)
})

test_that(".installAsyncShownCache is idempotent and updates $sc in place", {
  pe <- new.env(parent = emptyenv())
  reproducible:::.installAsyncShownCache(pe, "/p", data.frame(v = 1))
  scEnv1 <- pe[["shownCache"]][["/p"]]
  reproducible:::.installAsyncShownCache(pe, "/p", data.frame(v = 2))
  scEnv2 <- pe[["shownCache"]][["/p"]]

  ## Same env identity (not replaced); $sc updated to the new data
  expect_identical(scEnv1, scEnv2)
  expect_equal(scEnv2$sc$v, 2)
})
