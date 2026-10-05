## formatCheck() when no cache file exists yet (e.g. another process is still writing it) and
## reproducible.cacheSaveFormat is unset, as in a fresh subprocess: it errored with
## "argument is of length zero" (NULL == "check" is logical(0)). Seen in SpaDES.core's
## test-memoiseNoFileCopies.R, two processes running the same cached init.
test_that("formatCheck falls back to rds when the option is unset and there is no file", {
  withr::local_options(reproducible.cacheSaveFormat = NULL)
  cp <- withr::local_tempdir()
  expect_no_error(fmt <- reproducible:::formatCheck(cp, "0123456789abcdef", cacheSaveFormat = NULL))
  expect_identical(fmt, reproducible:::.rdsFormat)
  expect_identical(reproducible:::formatCheck(cp, "0123456789abcdef", cacheSaveFormat = "check"),
                   reproducible:::.rdsFormat)
})
