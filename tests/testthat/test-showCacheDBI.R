## On the DBI backend, a lookup of one entry by cacheId reads only that entry: showCache(cacheId = )
## queried the whole table and filtered in R (5 s on 500,000 entries), and the table had no index on
## cacheId, so showCacheFast() and Cache()'s own lookup scanned it too.

dbiCacheIdSetup <- function(env = parent.frame()) {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")
  withr::local_options(reproducible.useDBI = TRUE, reproducible.verbose = -2, .local_envir = env)
  skip_if_not(useDBI())
  cp <- withr::local_tempdir(.local_envir = env)
  for (i in 1:3) invisible(Cache(rnorm, i, cachePath = cp))
  list(cp = cp, ids = sort(unique(showCache(cp)$cacheId)))
}

## TRUE if the cache table of `cp` has the cacheId index (works on SQLite and Postgres)
hasCacheIdIndex <- function(cp) {
  drv <- getDrv(getOption("reproducible.drv", NULL))
  conn <- dbConnectAll(drv, cachePath = cp)
  on.exit(DBI::dbDisconnect(conn))
  tbl <- CacheDBTableName(cp, drv = drv)
  idx <- if (is(conn, "SQLiteConnection")) {
    DBI::dbGetQuery(conn, "SELECT name FROM sqlite_master WHERE type = 'index' AND tbl_name = $1",
                    params = list(tbl))$name
  } else {
    DBI::dbGetQuery(conn, "SELECT indexname FROM pg_indexes WHERE tablename = $1",
                    params = list(tbl))$indexname
  }
  paste0(tbl, "_cacheId") %in% idx
}

test_that("DBI showCache(cacheId = ) returns the same rows as filtering the whole table", {
  s <- dbiCacheIdSetup()
  all <- showCache(s$cp)
  expect_equal(length(unique(all$cacheId)), 3L)
  for (ci in list(s$ids[2], s$ids[c(1, 3)], "notACacheId")) {
    got <- showCache(s$cp, cacheId = ci)
    want <- all[cacheId %in% ci]
    expect_equal(NROW(got), NROW(want))
    expect_setequal(unique(got$cacheId), unique(want$cacheId))
  }
  expect_equal(NROW(showCacheFast(s$ids[2], cachePath = s$cp)),
               NROW(all[cacheId %in% s$ids[2]]))
})

test_that("DBI showCache(cacheId = ) asks the database for those entries only", {
  s <- dbiCacheIdSetup()
  seen <- character()
  realSend <- DBI::dbSendQuery
  testthat::local_mocked_bindings(dbSendQuery = function(conn, statement, ...) {
    seen <<- c(seen, as.character(statement))
    realSend(conn, statement, ...)
  }, .package = "DBI")
  showCache(s$cp, cacheId = s$ids[2])
  selects <- grep("^SELECT \\* FROM", seen, value = TRUE)
  expect_true(length(selects) > 0L)
  expect_true(all(grepl("WHERE \"cacheId\" IN", selects)))
})

test_that("a DBI cache gets an index on cacheId, also one made before the index existed", {
  s <- dbiCacheIdSetup()
  expect_true(hasCacheIdIndex(s$cp))

  ## a cache made by an earlier version: no index
  drv <- getDrv(getOption("reproducible.drv", NULL))
  conn <- dbConnectAll(drv, cachePath = s$cp)
  tbl <- CacheDBTableName(s$cp, drv = drv)
  DBI::dbExecute(conn, paste0("DROP INDEX \"", tbl, "_cacheId\""))
  DBI::dbDisconnect(conn)
  .pkgEnv$cacheIdIndexed <- NULL
  expect_false(hasCacheIdIndex(s$cp))

  expect_equal(length(unique(showCache(s$cp, cacheId = s$ids[1])$cacheId)), 1L)
  expect_true(hasCacheIdIndex(s$cp))
})
