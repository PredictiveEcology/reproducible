# Verifies the CRAN-graceful download-failure behaviour added to
# dlErrorHandling(): when a download terminally fails inside a test, the code
# must skip() (not error) where NOT_CRAN != "true" (i.e. on CRAN), and must
# still error where NOT_CRAN == "true" (local dev / CI) so real regressions are
# not masked. See R/download.R.
#
# The URL points at an intentionally-absent asset on this package's own v3.1.1
# release, so the download ALWAYS fails: a 404 when GitHub is reachable, or a
# connection error otherwise. Either way it drives the terminal-failure path
# deterministically, with no dependency on a successful download and no
# dependency on the network being up or down. These tests therefore do not need
# skip_on_cran(): they attempt a request but handle the failure themselves.

absentUrl <- paste0("https://github.com/PredictiveEcology/reproducible/",
                    "releases/download/v3.1.1/intentionallyAbsentFile.tif")

# Run prepInputs() on the absent file and report how it terminated, catching the
# skip / error condition here (before it reaches the test boundary) so the outer
# test can assert on it instead of itself being skipped.
downloadOutcome <- function() {
  td <- withr::local_tempdir(.local_envir = parent.frame())
  withr::local_options(reproducible.interactiveOnDownloadFail = FALSE,
                       .local_envir = parent.frame())
  tryCatch(
    {
      suppressWarnings(suppressMessages(
        prepInputs(url = absentUrl,
                   targetFile = "intentionallyAbsentFile.tif",
                   destinationPath = td, verbose = -2)
      ))
      "completed"
    },
    skip = function(s) "skipped",
    error = function(e) "errored"
  )
}

test_that("download of an absent file skips (not errors) on CRAN (NOT_CRAN unset)", {
  skip_if_not_installed("withr")
  withr::local_envvar(NOT_CRAN = "") # simulate the CRAN environment
  expect_identical(downloadOutcome(), "skipped")
})

test_that("download of an absent file still errors off CRAN (NOT_CRAN = 'true')", {
  skip_if_not_installed("withr")
  withr::local_envvar(NOT_CRAN = "true") # local dev / CI: real failures must surface
  expect_identical(downloadOutcome(), "errored")
})
