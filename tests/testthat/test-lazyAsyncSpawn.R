test_that("Cache() lazily spawns the async showCache job for the cachePath used", {
  ## Regression for: .onLoad-only spawn missed the user's real cachePath
  ## (set later by setupProject), so showCache() ran a 60s sync scan on
  ## first call. The lazy spawn from Cache() now targets whichever
  ## cachePath the caller actually uses.
  skip_on_cran()
  if (.Platform$OS.type == "windows")
    skip("forking-based; not relevant on Windows")
  if (!requireNamespace("parallel", quietly = TRUE))
    skip("parallel not available")

  tmpCache <- file.path(tempdir(), basename(tempfile("rcache_lazy_")))
  dir.create(tmpCache, showWarnings = FALSE, recursive = TRUE)
  withr::local_options(reproducible.cachePath  = tmpCache,
                       reproducible.useMemoise = FALSE,
                       reproducible.verbose    = 0)

  ## Pre-condition: no spawn job exists yet for this cachePath
  pkgEnv <- reproducible:::memoiseEnv(cachePath = tmpCache)
  expect_false(exists("shownCache", envir = pkgEnv) &&
               !is.null(pkgEnv[["shownCache"]]$shownCache_jobs) &&
               exists(tmpCache, envir = pkgEnv[["shownCache"]]$shownCache_jobs,
                      inherits = FALSE),
               info = "Pre-condition: no async job before Cache() is called")

  ## Calling Cache() should kick off the spawn
  invisible(Cache(rnorm, 1, cachePath = tmpCache,
                  cacheId = "lazy_v1", useCloud = FALSE))

  ## Post-condition: a spawn job is now registered for this cachePath
  expect_true(exists("shownCache", envir = pkgEnv),
              info = "Cache() should have created the shownCache env")
  jobsEnv <- pkgEnv[["shownCache"]]$shownCache_jobs
  expect_true(is.environment(jobsEnv),
              info = "shownCache_jobs should be an environment")
  expect_true(exists(tmpCache, envir = jobsEnv, inherits = FALSE),
              info = "A spawn job should be registered for the cachePath")
})

test_that("prepopulateCacheAsync() is exported and idempotent", {
  skip_on_cran()
  if (.Platform$OS.type == "windows")
    skip("forking-based; not relevant on Windows")
  if (!requireNamespace("parallel", quietly = TRUE))
    skip("parallel not available")

  tmpCache <- file.path(tempdir(), basename(tempfile("rcache_prep_")))
  dir.create(tmpCache, showWarnings = FALSE, recursive = TRUE)

  ## Exported
  expect_true(exists("prepopulateCacheAsync",
                     envir = asNamespace("reproducible"),
                     inherits = FALSE))

  ## First call schedules a job
  reproducible::prepopulateCacheAsync(tmpCache)
  pkgEnv <- reproducible:::memoiseEnv(cachePath = tmpCache)
  jobsEnv <- pkgEnv[["shownCache"]]$shownCache_jobs
  expect_true(exists(tmpCache, envir = jobsEnv, inherits = FALSE))
  job1 <- get(tmpCache, envir = jobsEnv, inherits = FALSE)

  ## Second call reuses the same job (idempotent: spawn_showCache_async
  ## returns the existing job under overwrite = FALSE)
  reproducible::prepopulateCacheAsync(tmpCache)
  job2 <- get(tmpCache, envir = jobsEnv, inherits = FALSE)
  expect_identical(job1$pid, job2$pid)
})

test_that("prepopulateCacheAsync() is a no-op for invalid inputs", {
  if (.Platform$OS.type == "windows")
    skip("forking-based; not relevant on Windows")

  ## NULL / empty / non-character should silently no-op
  expect_silent(reproducible::prepopulateCacheAsync(NULL))
  expect_silent(reproducible::prepopulateCacheAsync(""))
  expect_silent(reproducible::prepopulateCacheAsync(123))
})
