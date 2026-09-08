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
