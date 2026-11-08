## One compute per key, whichever backend is in use.
##
## `Cache()` runs the function between two independent database operations -- "is it
## cached?" and "store it" -- and no transaction spans them. SQLite's own locking
## (WAL, busy_timeout) therefore cannot deduplicate the *computation*, only keep the
## database consistent. The per-key file lock is what deduplicates, and it used to be
## taken only when `!useDBI()`, so under the DBI backend two workers reaching the same
## cold key both computed it.
##
## test-cluster.R can see that as an off-by-one in a count of 20, and only when the
## timing lines up: development stayed green for many runs while the hole was open.
## This makes it deterministic -- two workers, one cold key, and a function slow
## enough that the second worker is certain to arrive while the first is still in it.
## Unlocked that gives two computes; locked it gives one.
##
## Two workers, not more: R CMD check sets _R_CHECK_LIMIT_CORES_, and
## parallel::makeCluster() refuses a third ("3 simultaneous processes spawned").
##
## NB the workers `require("reproducible")`, so this exercises the *installed*
## package, which is what R CMD check runs against. Running it interactively after
## `pkgload::load_all()` tests the installed build too, not the sources -- so a
## source change appears to have no effect until it is installed.

test_that("a cold key is computed once, not once per worker, on both backends", {
  skip_on_cran()
  skip_if_not_installed("parallel")

  testInit("parallel")

  nWorkers <- min(2L, parallel::detectCores())
  skip_if(nWorkers < 2L, "needs 2 cores to have two workers race for one key")

  computesForOneColdKey <- function(useDBIhere, secs = 0.5) {
    cachePath <- file.path(tmpdir, paste0("lockPerKey_", useDBIhere))
    checkPath(cachePath, create = TRUE)
    withr::local_options(reproducible.useDBI = useDBIhere)
    if (isTRUE(useDBIhere) && !file.exists(CacheDBFile(cachePath)))
      createCache(cachePath)

    cl <- parallel::makeCluster(nWorkers)
    on.exit(parallel::stopCluster(cl), add = TRUE)

    ## clusterApply, so both workers start their single task at once
    out <- parallel::clusterApply(
      cl = cl, x = seq_len(nWorkers),
      fun = function(i, cachePath, secs, useDBIhere) {
        suppressWarnings(require("reproducible", quietly = TRUE))
        options(reproducible.useDBI = useDBIhere, reproducible.verbose = -2)
        slowly <- function(sd, secs) { Sys.sleep(secs); stats::rnorm(5, sd = sd) }
        reproducible::Cache(slowly, sd = 1, secs = secs, cachePath = cachePath)
      },
      cachePath = cachePath, secs = secs, useDBIhere = useDBIhere
    )
    sum(vapply(out, function(x) isTRUE(attr(x, ".Cache")[["newCache"]]), logical(1)))
  }

  ## the file backend, which always had the lock
  expect_equal(computesForOneColdKey(FALSE), 1L)

  ## the DBI backend, which did not
  if (requireNamespace("RSQLite", quietly = TRUE) && requireNamespace("DBI", quietly = TRUE))
    expect_equal(computesForOneColdKey(TRUE), 1L)
})

## `useCache = "overwrite"` on an existing key clears the entry (clearCache) while the caller holds the
## entry's lock. clearCache listed `<id>.lock` among the entry's files and deleted it, so the next
## process created a fresh file at that path, locked it, and ran at the same time.

test_that("useCache = 'overwrite' keeps the entry's lock file while the function runs", {
  skip_on_cran()
  skip_if_not_installed("filelock")
  testInit()

  lockFilesDuring <- function(...) {
    seen <- NULL
    f <- function(x) {
      seen <<- dir(CacheStorageDir(tmpCache), pattern = "^[0-9a-zA-Z]+\\.lock$")
      x
    }
    Cache(f, 1, cachePath = tmpCache, verbose = -2, ...)
    seen
  }
  withr::local_options(reproducible.useMemoise = FALSE)
  expect_length(lockFilesDuring(), 1L) # seeds the key; lock present while computing
  duringOverwrite <- lockFilesDuring(useCache = "overwrite")
  expect_length(duringOverwrite, 1L)
})

test_that("two workers with useCache = 'overwrite' on one existing key do not run at once", {
  skip_on_cran()
  skip_if_not_installed("parallel")

  testInit("parallel")

  nWorkers <- min(2L, parallel::detectCores())
  skip_if(nWorkers < 2L, "needs 2 cores to have two workers race for one key")

  cachePath <- file.path(tmpdir, "lockOverwrite")
  checkPath(cachePath, create = TRUE)
  withr::local_options(reproducible.useMemoise = FALSE)
  timed <- function(secs) {
    t0 <- as.numeric(Sys.time())
    Sys.sleep(secs)
    c(t0, as.numeric(Sys.time()))
  }
  ## seed the key; the workers then overwrite an entry that exists
  Cache(timed, secs = 0, omitArgs = "secs", cachePath = cachePath, verbose = -2)

  cl <- parallel::makeCluster(nWorkers)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  out <- parallel::clusterApply(
    cl = cl, x = seq_len(nWorkers),
    fun = function(i, cachePath) {
      suppressWarnings(require("reproducible", quietly = TRUE))
      options(reproducible.useMemoise = FALSE, reproducible.verbose = -2)
      timed <- function(secs) {
        t0 <- as.numeric(Sys.time())
        Sys.sleep(secs)
        c(t0, as.numeric(Sys.time()))
      }
      Sys.sleep(0.4 * (i - 1)) # the second worker arrives while the first is inside the function
      reproducible::Cache(timed, secs = 1, omitArgs = "secs",
                          useCache = "overwrite", cachePath = cachePath)
    },
    cachePath = cachePath
  )
  iv <- out[order(vapply(out, `[`, numeric(1), 1))]
  expect_gte(iv[[2]][1], iv[[1]][2]) # the second function call starts after the first ends
})
