## With a driver set in options(reproducible.drv) (as a Postgres user does), Cache() said "Using DBI backend."
## on every call, and clearCache() left every row in a server database's table.

test_that("Cache() with reproducible.drv set does not announce the backend on every call", {
  skip_if_not_installed("RSQLite")
  testInit(verbose = TRUE, opts = list(reproducible.useMemoise = FALSE, reproducible.useDBI = TRUE))
  if (is.null(getOption("reproducible.drv"))) withr::local_options(reproducible.drv = RSQLite::SQLite())
  skip_if_not(useDBI())
  mess <- capture_messages(for (i in 1:2) Cache(rnorm, 1, cachePath = tmpdir))
  expect_false(any(grepl("Using DBI backend", mess)))
})

test_that("a set reproducible.drv turns DBI on once, saying why, when useDBI is FALSE", {
  skip_if_not_installed("RSQLite")
  testInit(verbose = TRUE, opts = list(reproducible.useMemoise = FALSE, reproducible.useDBI = FALSE))
  if (is.null(getOption("reproducible.drv"))) withr::local_options(reproducible.drv = RSQLite::SQLite())
  mess1 <- capture_messages(Cache(rnorm, 1, cachePath = tmpdir))
  expect_true(useDBI())
  expect_equal(sum(grepl("Using the DBI backend because", mess1)), 1L)
  mess2 <- capture_messages(Cache(rnorm, 1, cachePath = tmpdir))
  expect_false(any(grepl("DBI backend", mess2)))
})

test_that("clearCache() empties a server database's table", {
  testInit(verbose = -2, opts = list(reproducible.useDBI = TRUE))
  drv <- getOption("reproducible.drv")
  skip_if(is.null(drv) || is(drv, "SQLiteDriver"), "needs a server database in reproducible.drv, e.g. Postgres")
  for (i in 1:2) Cache(rnorm, i, cachePath = tmpdir)
  expect_equal(length(unique(showCache(tmpdir)$cacheId)), 2L)
  clearCache(tmpdir, ask = FALSE)
  expect_equal(NROW(showCache(tmpdir)), 0L)
})
