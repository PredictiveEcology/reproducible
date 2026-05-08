## Unit tests for the cloud-mode helpers and the validateUseCloud gate. These
## need no Google Drive auth -- they test the small predicates that decide
## whether a cloud listing/download/upload happens for a given useCloud value.

test_that("cloudWrite recognises TRUE and \"push\"", {
  expect_true(reproducible:::cloudWrite(TRUE))
  expect_true(reproducible:::cloudWrite("push"))
  ## legacy ^w prefix retained
  expect_true(reproducible:::cloudWrite("write"))
})

test_that("cloudWrite returns FALSE for FALSE / NULL / \"pull\"", {
  expect_false(reproducible:::cloudWrite(FALSE))
  expect_false(reproducible:::cloudWrite(NULL))
  expect_false(reproducible:::cloudWrite("pull"))
})

test_that("cloudReadOnly recognises \"pull\" only", {
  expect_true(reproducible:::cloudReadOnly("pull"))
  ## legacy ^r prefix retained
  expect_true(reproducible:::cloudReadOnly("read"))
  expect_true(reproducible:::cloudReadOnly("readOnly"))

  expect_false(reproducible:::cloudReadOnly(TRUE))
  expect_false(reproducible:::cloudReadOnly(FALSE))
  expect_false(reproducible:::cloudReadOnly(NULL))
  expect_false(reproducible:::cloudReadOnly("push"))
})

test_that("cloudRead is TRUE for TRUE / \"push\" / \"pull\"; FALSE for FALSE/NULL", {
  expect_true(reproducible:::cloudRead(TRUE))
  expect_true(reproducible:::cloudRead("push"))
  expect_true(reproducible:::cloudRead("pull"))

  expect_false(reproducible:::cloudRead(FALSE))
  expect_false(reproducible:::cloudRead(NULL))
})

test_that("cloudWriteOrRead is TRUE iff cloud is active", {
  expect_true(reproducible:::cloudWriteOrRead(TRUE))
  expect_true(reproducible:::cloudWriteOrRead("push"))
  expect_true(reproducible:::cloudWriteOrRead("pull"))

  expect_false(reproducible:::cloudWriteOrRead(FALSE))
  expect_false(reproducible:::cloudWriteOrRead(NULL))
})

test_that("validateUseCloud accepts the documented forms and rejects others", {
  for (good in list(TRUE, FALSE, NULL, "pull", "push",
                    "write", "read", "readOnly")) {
    expect_silent(reproducible:::validateUseCloud(good))
  }
  for (bad in list("nonsense", "PULL", c("pull", "push"), 1L, NA)) {
    expect_error(reproducible:::validateUseCloud(bad),
                 regexp = "must be TRUE, FALSE, NULL")
  }
})

test_that("Cache() rejects an unknown character useCloud at the front door", {
  ## Construct a tiny Cache() call with a bad useCloud; should error before
  ## any digest / cloud lookup work happens.
  tmpCache <- file.path(tempdir(), basename(tempfile("rcache_")))
  dir.create(tmpCache, showWarnings = FALSE, recursive = TRUE)
  withr::local_options(reproducible.cachePath = tmpCache,
                       reproducible.useMemoise = FALSE)
  expect_error(
    Cache(rnorm, 1, cachePath = tmpCache, useCloud = "yolo"),
    regexp = "must be TRUE, FALSE, NULL"
  )
})

test_that("Cache(useCloud=\"pull\") with a local hit does not consult Google Drive", {
  ## Verify the local-hit short-circuit: when there's already a local cache
  ## entry, a "pull" call must not trigger any googledrive call. We monkey-
  ## patch reproducible:::driveLs to throw if invoked, then run a Cache call
  ## that should be served entirely from local disk.
  tmpCache <- file.path(tempdir(), basename(tempfile("rcache_pull_")))
  dir.create(tmpCache, showWarnings = FALSE, recursive = TRUE)
  withr::local_options(reproducible.cachePath = tmpCache,
                       reproducible.useMemoise = FALSE,
                       reproducible.verbose = 0)

  ## Seed local cache with a deterministic value
  local_cacheId <- "pullModeLocalHit_v1"
  Cache(rnorm, 1, cachePath = tmpCache, cacheId = local_cacheId, useCloud = FALSE)

  ## Sentinel: any call to driveLs should fail loudly
  ns <- asNamespace("reproducible")
  origDriveLs <- ns$driveLs
  poison <- function(...) stop("driveLs should not be called for a pull-mode local hit")
  unlockBinding("driveLs", ns)
  assign("driveLs", poison, envir = ns)
  withr::defer({
    unlockBinding("driveLs", ns)
    assign("driveLs", origDriveLs, envir = ns)
    lockBinding("driveLs", ns)
  })

  ## The second call (same cacheId) must hit local cache without touching driveLs
  expect_silent({
    out <- Cache(rnorm, 1, cachePath = tmpCache, cacheId = local_cacheId,
                 useCloud = "pull",
                 cloudFolderID = "irrelevant",
                 verbose = 0)
  })
  expect_true(is.numeric(out))
})
