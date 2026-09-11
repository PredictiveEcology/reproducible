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

test_that(".maybeSpawnShowCacheAsync spawns once, reaps the fork, no accumulation", {
  ## Regression for the fork leak: Cache() -> .maybeSpawnShowCacheAsync() forked a
  ## background showCache scan per cachePath but never collected it unless
  ## showCache() happened to be called, so one live fork leaked per cachePath
  ## (a covr run over hundreds of tmpCache paths OOM-killed the CI runner). It
  ## must now reap the fork on a following call and never re-spawn once the
  ## result is installed.
  skip_on_cran()                     # forks real processes; timing-sensitive
  testthat::skip_on_os("windows")    # no fork backend on Windows
  skip_if_not_installed("parallel")
  ## Under covr the pre-warm fork is disabled (memory); forking here would
  ## re-introduce the very covr OOM the option prevents. The fork path is
  ## exercised on the plain R CMD check legs instead.
  skip_if(isTRUE(as.logical(Sys.getenv("R_COVR", "false"))),
          "showCache pre-warm fork disabled under covr")

  withr::local_options(reproducible.useDBI = FALSE,          # exercise the flat-file fork path
                       reproducible.showCachePreWarm = TRUE) # force ON (default under R CMD check)
  ## The fork is detached, so it is never listed by parallel:::children(); follow the
  ## job entry and the child's pid instead.
  jobOf <- function(cp)
    reproducible:::memoiseEnv(cachePath = cp)[["shownCache"]]$shownCache_jobs[[cp]]
  alive <- function(pid) {
    st <- trimws(suppressWarnings(system2("ps", c("-o", "stat=", "-p", pid),
                                          stdout = TRUE, stderr = FALSE)))
    length(st) > 0 && nzchar(st[1]) && !startsWith(st[1], "Z")
  }

  ## (c) first call spawns exactly one background scan
  cp <- normalizePath(withr::local_tempdir(), mustWork = FALSE)
  reproducible:::.maybeSpawnShowCacheAsync(cp)
  expect_false(is.null(jobOf(cp)))

  ## (b) a later call harvests it once the (empty-cache) child has written its result.
  ##     The leaking, spawn-only version never harvests here, so this fails on it.
  harvested <- FALSE
  for (i in 1:100) {
    reproducible:::.maybeSpawnShowCacheAsync(cp)
    if (is.null(jobOf(cp))) { harvested <- TRUE; break }
    Sys.sleep(0.05)
  }
  expect_true(harvested)

  ## (a) once harvested, further calls neither spawn nor leak
  for (i in 1:20) reproducible:::.maybeSpawnShowCacheAsync(cp)
  expect_null(jobOf(cp))

  ## across many distinct cachePaths no pre-warm process is left running
  pids <- integer()
  for (p in 1:6) {
    cpp <- normalizePath(withr::local_tempdir(), mustWork = FALSE)
    for (k in 1:6) {
      reproducible:::.maybeSpawnShowCacheAsync(cpp)
      if (!is.null(jobOf(cpp))) pids <- c(pids, jobOf(cpp)$pid)
      Sys.sleep(0.03)
    }
  }
  Sys.sleep(2)
  expect_false(any(vapply(unique(pids), alive, logical(1))))
})

test_that(".maybeSpawnShowCacheAsync never forks under a DBI backend", {
  ## A DBI backend answers showCache() from an index, so the flat-file pre-warm
  ## fork is pointless there and must be skipped entirely.
  skip_on_cran()
  testthat::skip_on_os("windows")
  skip_if_not_installed("parallel")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")

  live <- function() length(parallel:::children())
  withr::defer(for (j in parallel:::children())
    try(parallel::mccollect(j, wait = FALSE, timeout = 0), silent = TRUE))
  withr::local_options(reproducible.useDBI = TRUE,
                       reproducible.showCachePreWarm = TRUE) # so the useDBI guard is what blocks the fork

  base <- live()
  cp <- normalizePath(withr::local_tempdir(), mustWork = FALSE)
  for (i in 1:10) reproducible:::.maybeSpawnShowCacheAsync(cp)
  expect_equal(live() - base, 0L)    # no fork ever spawned under useDBI(TRUE)
})

test_that("the pre-warm child exits after its scan even when nobody collects", {
  ## Regression: the child returned its showCache result through the fork's pipe. Once
  ## the result was larger than the pipe buffer (64 KB) the child blocked in that write
  ## until the parent collected, which a batch job may never do: on fireSense fits workers
  ## an idle ~400 MB child sat in anon_pipe_write for hours, and outlived a killed job.
  skip_on_cran()
  testthat::skip_on_os("windows")
  skip_if_not_installed("parallel")
  skip_if(isTRUE(as.logical(Sys.getenv("R_COVR", "false"))),
          "showCache pre-warm fork disabled under covr")

  withr::local_options(reproducible.useDBI = FALSE,
                       reproducible.showCachePreWarm = TRUE,
                       reproducible.verbose = -2)
  withr::defer(for (j in parallel:::children())
    try(parallel::mccollect(j, wait = FALSE, timeout = 0), silent = TRUE))

  cp <- normalizePath(withr::local_tempdir(), mustWork = FALSE)
  for (i in seq_len(150)) Cache(rnorm, i, cachePath = cp, useCloud = FALSE)
  expect_gt(length(serialize(showCache(cp), NULL)), 64 * 1024) # larger than a pipe buffer
  pe <- reproducible:::memoiseEnv(cachePath = cp)   # forget that scan, so only the fork can fill it
  if (exists("shownCache", envir = pe, inherits = FALSE)) rm("shownCache", envir = pe)

  job <- reproducible:::spawn_showCache_async(cp)
  ## A finished child is a zombie (or gone) until collected; a blocked one stays asleep.
  childState <- function()
    suppressWarnings(system2("ps", c("-o", "stat=", "-p", job$pid), stdout = TRUE, stderr = FALSE))
  exited <- FALSE
  for (i in 1:600) {
    st <- trimws(childState())
    if (!length(st) || !nzchar(st[1]) || startsWith(st[1], "Z")) { exited <- TRUE; break }
    Sys.sleep(0.1)
  }
  expect_true(exited, info = "the pre-warm child did not exit within 60 s")

  ## and the result is still installed when it is collected
  reproducible:::collect_showCache_async(cp, wait = TRUE, timeout = 10)
  scEnv <- reproducible:::memoiseEnv(cachePath = cp)[["shownCache"]][[cp]]
  expect_gt(NROW(scEnv$sc), 0)
})
