test_that("pp_remote_hash_check tolerates a remote with no advertised file size", {
  # Regression: getRemoteMetadata() can return fileSize = NULL (e.g. an HTTP
  # source with no content-length header). That made
  #   remoteSize <- as.numeric(NULL)            # numeric(0)
  #   !is.na(remoteSize) && ...                 # logical(0) && ... -> NA
  #   if (NA) ...                               # "missing value where
  #                                             #  TRUE/FALSE needed"
  # The size comparison must treat a missing remote size as unknown and fall
  # through to the normal hash/download path instead of erroring.
  testInit("terra", verbose = -1)

  localFile <- file.path(tmpdir, "ecoregions.zip")
  writeLines("not a real zip, just bytes", localFile)

  ctx <- list(
    url              = "https://example.com/data/ecoregions.zip",
    archive          = NULL,
    neededFiles      = localFile,
    destinationPath  = tmpdir,
    checkSumFilePath = file.path(tmpdir, "CHECKSUMS.txt"),
    verbose          = -1,
    hashVerified     = character(),
    skipDownload     = FALSE,
    remoteMetadata   = NULL
  )

  fakeMeta <- list(
    targetFile      = "ecoregions.zip",
    fileSize        = NULL,            # <- the trigger
    remoteHash      = "\"opaque-etag\"",
    remoteAlgorithm = "etag-opaque",   # opaque -> no positive trust -> download
    timestampOnline = NULL
  )

  testthat::with_mocked_bindings(
    getRemoteMetadata = function(...) fakeMeta,
    {
      res <- expect_no_error(reproducible:::pp_remote_hash_check(ctx))
    }
  )

  # No size/hash trust was established, so the normal download must proceed.
  expect_false(isTRUE(res$skipDownload))

  # Sanity: a genuinely differing *known* size still fast-fails to download
  # (skipDownload stays FALSE) without erroring.
  fakeMeta$fileSize <- 999999999
  testthat::with_mocked_bindings(
    getRemoteMetadata = function(...) fakeMeta,
    {
      res2 <- expect_no_error(reproducible:::pp_remote_hash_check(ctx))
    }
  )
  expect_false(isTRUE(res2$skipDownload))
})
