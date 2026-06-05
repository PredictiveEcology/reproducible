# Offline tests for checkAndMakeCloudFolderID's fallback warning. When an
# explicit cloudFolderID cannot be resolved on Google Drive (not found, or not
# accessible to the authenticated account), the code silently substitutes a
# folder derived from the local cache path -- which differs from machine to
# machine and so breaks cross-machine/OS cloud caching. It must warn. Google
# Drive's drive_get is mocked to return an empty result (no network/auth).

test_that("checkAndMakeCloudFolderID warns when an explicit cloudFolderID cannot be resolved", {
  skip_if_not_installed("googledrive")
  skip_if_not_installed("tibble")
  url <- "https://drive.google.com/drive/folders/AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA?usp=share_link"
  testthat::with_mocked_bindings(
    expect_warning(
      reproducible:::checkAndMakeCloudFolderID(url, cachePath = tempdir(),
                                               create = FALSE, verbose = 0),
      "could not be resolved"
    ),
    drive_get = function(...) tibble::tibble(name = character(0), id = character(0)),
    .package = "googledrive"
  )
})

test_that("checkAndMakeCloudFolderID does NOT warn when cloudFolderID is NULL (documented default)", {
  skip_if_not_installed("googledrive")
  skip_if_not_installed("tibble")
  # A NULL cloudFolderID deriving a folder from the cache path is the documented
  # default behaviour, not a surprising substitution -> no warning.
  testthat::with_mocked_bindings(
    expect_no_warning(
      suppressMessages(
        reproducible:::checkAndMakeCloudFolderID(NULL, cachePath = tempdir(),
                                                 create = FALSE, verbose = 0)
      )
    ),
    drive_get = function(...) tibble::tibble(name = character(0), id = character(0)),
    .package = "googledrive"
  )
})
