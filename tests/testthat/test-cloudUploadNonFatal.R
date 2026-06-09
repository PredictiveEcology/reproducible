test_that("cloudUploadFromCache does not abort when predicted files are absent", {
  skip_on_cran()
  skip_if_not_installed("googledrive")
  testInit(opts = list(reproducible.useDBI = FALSE))
  cp <- file.path(tempfile("cache_")); dir.create(cp, recursive = TRUE)
  ## A cacheId with no files on disk in the cache folder (emulates a cached
  ## simList whose predicted raster-backend sidecars were never written under
  ## the cacheId). Previously this stop()ped and crashed the whole run; now it
  ## must warn and skip the (best-effort) cloud upload, never error.
  expect_no_error(
    msgs <- capture_messages(
      reproducible:::cloudUploadFromCache(
        isInCloud = FALSE, outputHash = "0123456789abcdef",
        cachePath = cp, cloudFolderID = NULL,
        outputToSave = NULL, rasters = NULL, verbose = 1
      )
    )
  )
  expect_true(any(grepl("skipping cloud upload", msgs)))
})
