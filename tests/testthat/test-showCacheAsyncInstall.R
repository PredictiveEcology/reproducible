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

test_that(".installAsyncShownCache copies bindings from an env-shaped result", {
  ## Regression for: child returns the inner per-cachePath env (sc + FileInfo)
  ## and we previously stored that env directly at scEnv$sc, breaking
  ## rbindlist(list(scEnv$sc, ret)) downstream with
  ## "Item 1 of input is not a data.frame, data.table or list".
  pe <- new.env(parent = emptyenv())

  childEnv <- new.env(parent = emptyenv())
  childEnv$sc <- data.frame(cacheId = "abc", tagKey = "k", tagValue = "v",
                            stringsAsFactors = FALSE)
  childEnv$FileInfo <- data.frame(filename = "/tmp/foo.rds",
                                  mtime = Sys.time(), size = 1L,
                                  stringsAsFactors = FALSE)

  reproducible:::.installAsyncShownCache(pe, "/p", childEnv)

  scEnv <- pe[["shownCache"]][["/p"]]
  expect_true(is.environment(scEnv))
  expect_true(is.data.frame(scEnv$sc))
  expect_true(is.data.frame(scEnv$FileInfo))
  expect_identical(scEnv$sc$cacheId, "abc")
  ## scEnv$sc must be a data.frame so rbindlist downstream accepts it
  expect_silent(data.table::rbindlist(list(scEnv$sc, scEnv$sc), fill = TRUE))
})

test_that(".installAsyncShownCache leaves a clean empty env for unsupported inputs", {
  ## A bare atomic (string, numeric, NA) shouldn't poison scEnv. The contract
  ## with the sync path is: scEnv$sc must be NULL or a data.frame/data.table.
  pe <- new.env(parent = emptyenv())
  reproducible:::.installAsyncShownCache(pe, "/p", "some string")

  scEnv <- pe[["shownCache"]][["/p"]]
  expect_true(is.environment(scEnv))
  expect_null(scEnv$sc)
  expect_null(scEnv$FileInfo)
})
