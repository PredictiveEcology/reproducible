## Coverage for .dlGooglePublic() (R/download.R): fetching a Drive file that is
## shared "anyone with the link", without authenticating.
##
## This is the path a user hits when a module points at someone else's public
## Drive file. It had no test, so the interstitial retry -- the part that makes
## it work for files big enough that Drive refuses to virus-scan them -- was
## entirely unexercised.
##
## The happy path is a real round-trip: upload a tiny file, share it publicly,
## fetch it back. The interstitial branches cannot be produced on demand (they
## need a file large enough to trigger Drive's warning page), so those are
## driven by mocking the two detectors, which are themselves covered directly in
## test-googleInterstitial.R.
##
## Uploads, so skip_on_cran() on the round-trip.

## Upload `text` as a public file; returns its human-facing URL. Registers its
## own cleanup on the caller's frame.
publicFixture <- function(dir, text, envir = parent.frame()) {
  f <- file.path(dir, "pub.txt")
  writeLines(text, f)
  up <- retry(quote(googledrive::drive_upload(
    f, path = .cloudTestRoot(), name = paste0("pub", rndstr(1, 6)))))
  withr::defer(try(googledrive::drive_rm(up), silent = TRUE), envir = envir)
  googledrive::drive_share_anyone(up)
  list(url = paste0("https://drive.google.com/file/d/", up$id, "/view"), id = up$id)
}

test_that(".dlGooglePublic downloads a link-shared file without authenticating", {
  skip_on_cran()          ## uploads
  testInit("googledrive", needGoogleDriveAuth = TRUE)

  fx <- publicFixture(tmpdir, "public-payload")
  dest <- checkPath(file.path(tmpdir, "dl"), create = TRUE)

  res <- .dlGooglePublic(url = fx$url, destinationPath = dest,
                         targetFile = "pub.txt", verbose = 0)

  ## The contract downloadFile() relies on.
  expect_true("destFile" %in% names(res))
  expect_true(file.exists(res$destFile))
  ## The real bytes came back, not Drive's HTML warning page.
  expect_identical(readLines(res$destFile, warn = FALSE), "public-payload")
})

test_that(".dlGooglePublic retries with the confirm parameters on an interstitial", {
  skip_on_cran()
  testInit("googledrive", needGoogleDriveAuth = TRUE)

  fx <- publicFixture(tmpdir, "public-payload")
  dest <- checkPath(file.path(tmpdir, "dl2"), create = TRUE)

  ## Report an interstitial on the FIRST check only, so the retry branch runs
  ## and then succeeds -- which is what happens for a genuinely large file.
  n <- 0L
  res <- testthat::with_mocked_bindings(
    testthat::with_mocked_bindings(
      .dlGooglePublic(url = fx$url, destinationPath = dest,
                      targetFile = "pub.txt", verbose = 0),
      .parseGoogleConfirm = function(file)
        list(id = fx$id, export = "download", confirm = "t")),
    .looksLikeGoogleInterstitial = function(file) { n <<- n + 1L; n == 1L })

  ## Both checks ran, so the retry path was taken and then cleared.
  expect_gte(n, 2L)
  expect_true(file.exists(res$destFile))
})

test_that(".dlGooglePublic errors clearly when Drive keeps returning HTML", {
  skip_on_cran()
  testInit("googledrive", needGoogleDriveAuth = TRUE)

  fx <- publicFixture(tmpdir, "public-payload")
  dest <- checkPath(file.path(tmpdir, "dl3"), create = TRUE)

  ## Interstitial every time and no usable form parameters: the file is not
  ## actually shared publicly. That must be an explicit error naming the
  ## likely cause, not a silently-saved HTML page.
  expect_error(
    testthat::with_mocked_bindings(
      testthat::with_mocked_bindings(
        .dlGooglePublic(url = fx$url, destinationPath = dest,
                        targetFile = "pub.txt", verbose = 0),
        .parseGoogleConfirm = function(file) list()),
      .looksLikeGoogleInterstitial = function(file) TRUE),
    "Anyone with the link"
  )
})
