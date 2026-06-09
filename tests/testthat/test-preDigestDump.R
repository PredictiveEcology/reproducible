test_that("reproducible.preDigestDump emits the full preDigest (message + file modes)", {
  skip_on_cran()
  testInit(opts = list(reproducible.useDBI = FALSE))
  cp <- file.path(tempfile("cache_")); dir.create(cp, recursive = TRUE)

  ## message mode: TRUE -> sorted `name = hash` lines via messageCache
  withr::local_options(reproducible.preDigestDump = TRUE)
  msgs <- capture_messages(
    Cache(FUN = rnorm(2), .functionName = "doEvent.myMod::init", cachePath = cp)
  )
  expect_true(any(grepl("cacheId=", msgs)))
  expect_true(any(grepl("= [0-9a-f]{6,}", msgs)))   # at least one name = hash line

  ## directory mode: one file per call, sorted, with a cacheId header
  dumpDir <- file.path(tempfile("dump_"))
  withr::local_options(reproducible.preDigestDump = dumpDir)
  invisible(Cache(FUN = rnorm(2), .functionName = "doEvent.myMod::init",
                  cachePath = cp, userTags = "two"))
  f <- list.files(dumpDir, pattern = "^preDigest_doEvent", full.names = TRUE)
  expect_length(f, 1L)
  ll <- readLines(f)
  expect_match(ll[[1]], "^# doEvent.myMod::init  cacheId=")
  expect_true(all(grepl(" = ", ll[-1])))
  expect_identical(ll[-1], ll[-1][order(ll[-1])])    # sorted

  ## pattern filter: non-matching .functionName is not dumped
  dumpDir2 <- file.path(tempfile("dump2_"))
  withr::local_options(reproducible.preDigestDump = dumpDir2,
                       reproducible.preDigestDumpPattern = "init|inputObjects")
  invisible(Cache(FUN = rnorm(2), .functionName = "somethingElse", cachePath = cp, userTags = "three"))
  expect_false(dir.exists(dumpDir2) && length(list.files(dumpDir2)) > 0)
})
