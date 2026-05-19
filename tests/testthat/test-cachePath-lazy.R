test_that("reproducibleOptions() ships no default cachePath", {
  # The load-time default used to be file.path(tempdir(), 'reproducible',
  # 'cache'), which silently committed every R session to a non-persistent
  # path before any user/project setup could intervene. The default is now
  # NULL; resolution happens lazily on first use (see .checkCacheRepo).
  opts <- reproducibleOptions()
  expect_true("reproducible.cachePath" %in% names(opts))
  expect_null(opts[["reproducible.cachePath"]])
})

test_that("Cache() lazily resolves and persists cachePath when the option is unset", {
  testInit(verbose = -1)

  # Start from a genuinely unset option, regardless of what earlier tests
  # may have left behind.
  withr::local_options(list(reproducible.cachePath = NULL))
  expect_null(getOption("reproducible.cachePath"))

  # A trivial Cache() call should pick a default and persist it into the
  # option for the rest of the session (within this withr scope).
  suppressMessages(Cache(function() 1L))

  resolved <- getOption("reproducible.cachePath")
  expect_type(resolved, "character")
  expect_true(nzchar(resolved))
  expect_true(dir.exists(resolved))

  # A second Cache() call must reuse the same resolved path, not re-roll it.
  suppressMessages(Cache(function() 2L))
  expect_identical(getOption("reproducible.cachePath"), resolved)
})

test_that("clearCache() lazily resolves cachePath when called with no args", {
  testInit(verbose = -1)
  withr::local_options(list(reproducible.cachePath = NULL))
  expect_null(getOption("reproducible.cachePath"))

  # clearCache() with no x and an empty cache must not error -- it must
  # route the missing x through .checkCacheRepo, which both resolves the
  # path and persists it into the option.
  suppressMessages(expect_no_error(clearCache()))
  resolved <- getOption("reproducible.cachePath")
  expect_type(resolved, "character")
  expect_true(nzchar(resolved))
})
