## Helper: does a spawn job exist for `cachePath` in its pkgEnv?
.hasSpawnJob <- function(cachePath) {
  pe <- reproducible:::memoiseEnv(cachePath = cachePath)
  exists("shownCache", envir = pe) &&
    is.environment(pe[["shownCache"]]$shownCache_jobs) &&
    exists(cachePath, envir = pe[["shownCache"]]$shownCache_jobs, inherits = FALSE)
}

test_that("default Cache() does not leak background showCache forks", {
  ## Leak regression (the exit-143 CI OOM): showSimilar=FALSE (the default) never
  ## calls showCache(), so the pre-warm fork it used to spawn was never harvested
  ## -- one lingering child per distinct cachePath until session end (measured at
  ## ~50 forks / ~46 GB across the suite). The fix spawns only on the
  ## showSimilar=TRUE path, so the default path adds zero children. Asserted on
  ## the actual leak metric: the live-child count must not grow.
  skip_on_cran()
  if (.Platform$OS.type == "windows")
    skip("forking-based; not relevant on Windows")
  if (!requireNamespace("parallel", quietly = TRUE))
    skip("parallel not available")

  withr::local_options(reproducible.useMemoise = FALSE,
                       reproducible.useDBI     = FALSE,
                       reproducible.verbose    = 0)

  baseChildren <- length(parallel:::children())
  for (i in seq_len(6L)) {
    cp <- file.path(tempdir(), basename(tempfile("rcache_leak_")))
    dir.create(cp, showWarnings = FALSE, recursive = TRUE)
    invisible(Cache(rnorm, 1, cachePath = cp, cacheId = paste0("leak_", i),
                    useCloud = FALSE))               # default showSimilar = FALSE
    expect_false(.hasSpawnJob(cp),
                 info = "default Cache() must not spawn a pre-warm fork")
  }
  expect_equal(length(parallel:::children()), baseChildren,
               info = "6 default Cache() calls must not leak any background forks")
})

test_that("reproducible.showCachePreWarm = FALSE disables the auto pre-warm fork", {
  ## The advanced off-switch: even on the showSimilar=TRUE path (which normally
  ## pre-warms), showCachePreWarm=FALSE must spawn nothing. Used under covr, where
  ## per-path forks otherwise accumulate to ~38 / ~23 GB and OOM the runner.
  skip_on_cran()
  if (.Platform$OS.type == "windows")
    skip("forking-based; not relevant on Windows")
  if (!requireNamespace("parallel", quietly = TRUE))
    skip("parallel not available")

  withr::local_options(reproducible.useMemoise      = FALSE,
                       reproducible.useDBI           = FALSE,
                       reproducible.showCachePreWarm = FALSE,
                       reproducible.verbose          = 0)

  base <- length(parallel:::children())

  ## (1) auto path: Cache(showSimilar=TRUE) must not spawn.
  cp <- file.path(tempdir(), basename(tempfile("rcache_prewarmoff_")))
  dir.create(cp, showWarnings = FALSE, recursive = TRUE)
  invisible(Cache(rnorm, 1, cachePath = cp, cacheId = "prewarmoff_v1",
                  useCloud = FALSE, showSimilar = TRUE))
  expect_false(.hasSpawnJob(cp),
               info = "showCachePreWarm=FALSE must not spawn even with showSimilar=TRUE")

  ## (2) hard off-switch: even explicit prepopulateCacheAsync() must not spawn.
  cp2 <- file.path(tempdir(), basename(tempfile("rcache_prewarmoff2_")))
  dir.create(cp2, showWarnings = FALSE, recursive = TRUE)
  reproducible::prepopulateCacheAsync(cp2)
  expect_false(.hasSpawnJob(cp2),
               info = "showCachePreWarm=FALSE is a hard off-switch: prepopulateCacheAsync() too")

  expect_equal(length(parallel:::children()), base,
               info = "no background fork when the pre-warm is disabled")
})

## NB: the helper-level contract for .maybeSpawnShowCacheAsync() -- spawns once
## on a direct call for a flat-file path, reaps on the next call, and never forks
## under a DBI backend -- is covered in test-showCacheAsyncInstall.R. Here we only
## assert the Cache() *call-site* gating that decides whether to call it at all.

test_that("prepopulateCacheAsync() is exported and schedules one flat-file scan", {
  skip_on_cran()
  if (.Platform$OS.type == "windows")
    skip("forking-based; not relevant on Windows")
  if (!requireNamespace("parallel", quietly = TRUE))
    skip("parallel not available")
  ## Under covr the pre-warm fork is disabled (memory); spawning here would
  ## re-introduce the covr OOM. The spawn path is exercised on R CMD check legs.
  skip_if(isTRUE(as.logical(Sys.getenv("R_COVR", "false"))),
          "showCache pre-warm fork disabled under covr")

  ## Flat-file backend: the DBI backend has nothing to pre-warm (see
  ## test-showCacheAsyncInstall.R), so pin it off to exercise the fork path.
  ## Force the pre-warm ON (default under R CMD check; covr is skipped above).
  withr::local_options(reproducible.useDBI = FALSE,
                       reproducible.showCachePreWarm = TRUE)

  ## Exported
  expect_true(exists("prepopulateCacheAsync",
                     envir = asNamespace("reproducible"),
                     inherits = FALSE))

  tmpCache <- file.path(tempdir(), basename(tempfile("rcache_prep_")))
  dir.create(tmpCache, showWarnings = FALSE, recursive = TRUE)

  ## First call schedules a background scan job for this path.
  reproducible::prepopulateCacheAsync(tmpCache)
  expect_true(.hasSpawnJob(tmpCache),
              info = "prepopulateCacheAsync() should schedule a job")

  ## Idempotent: a repeat call must not spawn a *second* fork for the same path
  ## (the helper reaps/reuses the existing one -- see the lifecycle tests in
  ## test-showCacheAsyncInstall.R). Asserted on the live-child count.
  base <- length(parallel:::children())
  reproducible::prepopulateCacheAsync(tmpCache)
  expect_lte(length(parallel:::children()), base)

  reproducible:::collect_showCache_async(tmpCache, wait = TRUE, timeout = 10)
})

test_that("prepopulateCacheAsync() is a no-op for invalid inputs", {
  if (.Platform$OS.type == "windows")
    skip("forking-based; not relevant on Windows")

  ## NULL / empty / non-character should silently no-op
  expect_silent(reproducible::prepopulateCacheAsync(NULL))
  expect_silent(reproducible::prepopulateCacheAsync(""))
  expect_silent(reproducible::prepopulateCacheAsync(123))
})
