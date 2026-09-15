## Regression tests for the memoise stale-object recovery path in
## `dealWithCacheRecoveryErrors()` (R/cache.R), which is reached from
## `loadFromCache()` (R/cache-repo.R:452).
##
## Background: some objects -- notably Rcpp/terra ones holding external
## pointers -- survive being stored in the memoise environment but are
## structurally dead when read back in a later session. The recovery detects
## that, drops the memoised copy, and forces a re-read from disk.
##
## These tests exist because the detection used to have two independent
## triggers, `outputTestIntegrity` (from `try(output[1])`) and `fns` (from
## `Filenames(output)`). The `fns` trigger was removed when `try2()` was
## removed, so only the `outputTestIntegrity` one remains live. The first two
## tests pin the surviving trigger; the third documents the removed one.

test_that("dealWithCacheRecoveryErrors detects a stale external pointer and drops the memoised copy", {
  testInit(opts = list(reproducible.useMemoise = TRUE))

  cache_key <- "abcdef0123456789"
  memEnv <- memoiseEnv(tmpCache)
  assign(cache_key, "a memoised value", envir = memEnv)

  ## What `try(output[1], silent = TRUE)` yields for a dead external pointer.
  outputTestIntegrity <- try(stop("external pointer is not valid"), silent = TRUE)

  memoiseFail <- dealWithCacheRecoveryErrors(
    memoiseFail = FALSE,
    outputTestIntegrity = outputTestIntegrity,
    fns = character(),
    cache_key = cache_key,
    cachePath = tmpCache,
    outputObjects = NULL
  )

  expect_true(memoiseFail)
  ## The corrupt memoised entry must be gone, so the next read comes from disk.
  expect_false(exists(cache_key, envir = memEnv, inherits = FALSE))
})

test_that("dealWithCacheRecoveryErrors recognizes the NULL-symbol-address variant", {
  testInit(opts = list(reproducible.useMemoise = TRUE))

  cache_key <- "fedcba9876543210"
  memEnv <- memoiseEnv(tmpCache)
  assign(cache_key, "a memoised value", envir = memEnv)

  outputTestIntegrity <- try(stop("NULL value passed as symbol address"), silent = TRUE)

  expect_true(dealWithCacheRecoveryErrors(
    memoiseFail = FALSE, outputTestIntegrity = outputTestIntegrity,
    fns = character(), cache_key = cache_key,
    cachePath = tmpCache, outputObjects = NULL
  ))
  expect_false(exists(cache_key, envir = memEnv, inherits = FALSE))
})

test_that("dealWithCacheRecoveryErrors leaves the memoised copy alone for an unrelated error", {
  testInit(opts = list(reproducible.useMemoise = TRUE))

  cache_key <- "0f1e2d3c4b5a6978"
  memEnv <- memoiseEnv(tmpCache)
  assign(cache_key, "a memoised value", envir = memEnv)

  ## A try-error, but not one of the two external-pointer signatures: the
  ## object is not stale, so recovery must not fire.
  outputTestIntegrity <- try(stop("subscript out of bounds"), silent = TRUE)

  expect_false(dealWithCacheRecoveryErrors(
    memoiseFail = FALSE, outputTestIntegrity = outputTestIntegrity,
    fns = character(), cache_key = cache_key,
    cachePath = tmpCache, outputObjects = NULL
  ))
  expect_true(exists(cache_key, envir = memEnv, inherits = FALSE))
})

test_that("a healthy memoised object does not trigger recovery", {
  testInit(opts = list(reproducible.useMemoise = TRUE))

  cache_key <- "1122334455667788"
  memEnv <- memoiseEnv(tmpCache)
  assign(cache_key, "a memoised value", envir = memEnv)

  ## No try-error at all, and no file-backed filenames: the `else` branch runs
  ## and must be a no-op.
  expect_false(dealWithCacheRecoveryErrors(
    memoiseFail = FALSE, outputTestIntegrity = "fine",
    fns = character(), cache_key = cache_key,
    cachePath = tmpCache, outputObjects = NULL
  ))
  expect_true(exists(cache_key, envir = memEnv, inherits = FALSE))
})

test_that("Filenames() no longer contributes a stale-pointer trigger", {
  ## Documents a deliberate narrowing. `fns` was formerly
  ## `try2(Filenames(output), silent = TRUE)`, so a dead external pointer
  ## surfacing only through `Filenames()` would also trip recovery. With
  ## `try2()` removed, such an error propagates out of `loadFromCache()`
  ## instead of being recovered from, and `fns` can no longer arrive here as a
  ## try-error in normal operation.
  ##
  ## The `is(fns, "try-error")` test in dealWithCacheRecoveryErrors() is
  ## retained, so a caller that does pass one still gets recovery. If that test
  ## is ever deleted, this should fail and prompt a deliberate decision.
  testInit(opts = list(reproducible.useMemoise = TRUE))

  cache_key <- "99aabbccddeeff00"
  memEnv <- memoiseEnv(tmpCache)
  assign(cache_key, "a memoised value", envir = memEnv)

  fns <- try(stop("external pointer is not valid"), silent = TRUE)

  expect_true(dealWithCacheRecoveryErrors(
    memoiseFail = FALSE, outputTestIntegrity = "fine",
    fns = fns, cache_key = cache_key,
    cachePath = tmpCache, outputObjects = NULL
  ))
  expect_false(exists(cache_key, envir = memEnv, inherits = FALSE))
})
