## Coverage for dlGoogle()'s downloader selection (R/download.R).
##
## dlGoogle picks between two download mechanisms:
##
##   httr2 available  -> download_resumable_httr2()   (the normal path)
##   httr2 absent     -> googledrive::drive_download() (the fallback)
##
## httr2 is installed on every machine this package is developed, checked or
## coverage-measured on, so the fallback branch never ran anywhere -- it is the
## code that only executes on a user's machine with a thinner install, which is
## precisely where an untested branch does the most damage.
##
## .requireNamespace() is the package's own availability check, so mocking it to
## report httr2 as missing selects the fallback without touching the real
## library path or uninstalling anything.
##
## Uploads, so skip_on_cran().

## Upload `text` as a small private file; returns its URL. Cleans up after itself.
driveFixture <- function(dir, text, envir = parent.frame()) {
  f <- file.path(dir, "dlg.txt")
  writeLines(text, f)
  up <- retry(quote(googledrive::drive_upload(
    f, path = .cloudTestRoot(), name = paste0("dlg", rndstr(1, 6)))))
  withr::defer(try(googledrive::drive_rm(up), silent = TRUE), envir = envir)
  paste0("https://drive.google.com/file/d/", up$id, "/view")
}

test_that("dlGoogle downloads via httr2 when it is available", {
  skip_on_cran()          ## uploads
  skip_if_not_installed("httr2")
  testInit("googledrive", needGoogleDriveAuth = TRUE)

  url <- driveFixture(tmpdir, "httr2-payload")
  dest <- checkPath(file.path(tmpdir, "d1"), create = TRUE)

  res <- dlGoogle(url = url, targetFile = "dlg.txt", destinationPath = dest,
                  overwrite = TRUE, needChecksums = 0,
                  checkSums = .emptyChecksumsResult, verbose = 0)

  ## The contract downloadFile() consumes.
  expect_true(all(c("destFile", "needChecksums") %in% names(res)))
  expect_true(file.exists(res$destFile))
  expect_identical(readLines(res$destFile, warn = FALSE), "httr2-payload")
})

test_that("dlGoogle falls back to googledrive::drive_download without httr2", {
  skip_on_cran()          ## uploads
  testInit("googledrive", needGoogleDriveAuth = TRUE)

  url <- driveFixture(tmpdir, "fallback-payload")
  dest <- checkPath(file.path(tmpdir, "d2"), create = TRUE)

  ## Report httr2 as unavailable, but let every other package query through --
  ## dlGoogle also asks about googledrive, and answering FALSE there would stop
  ## it before the branch under test.
  realRequire <- reproducible:::.requireNamespace
  res <- testthat::with_mocked_bindings(
    dlGoogle(url = url, targetFile = "dlg.txt", destinationPath = dest,
             overwrite = TRUE, needChecksums = 0,
             checkSums = .emptyChecksumsResult, verbose = 0),
    .requireNamespace = function(pkg = "methods", ...) {
      if (identical(pkg, "httr2")) FALSE else realRequire(pkg, ...)
    })

  ## Same observable outcome as the httr2 path: that equivalence is the point.
  ## If the fallback quietly produced nothing, or an empty file, this fails.
  expect_true(file.exists(res$destFile))
  expect_identical(readLines(res$destFile, warn = FALSE), "fallback-payload")
  expect_gt(file.size(res$destFile), 0)
})
