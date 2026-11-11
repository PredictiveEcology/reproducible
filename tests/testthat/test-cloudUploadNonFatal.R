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

test_that("cloudUploadFromCache warns, naming the file, when an upload fails", {
  skip_on_cran()
  skip_if_not_installed("googledrive")
  testInit(opts = list(reproducible.useDBI = FALSE))
  cp <- file.path(tempfile("cache_")); dir.create(cp, recursive = TRUE)
  hash <- "0123456789abcdef"
  ## One cache file on disk, so the upload is attempted.
  f <- reproducible:::CacheStoredFile(cp, hash, "check", obj = NULL)
  checkPath(dirname(f[1]), create = TRUE)
  saveRDS(1, f[1])
  local_mocked_bindings(
    checkAndMakeCloudFolderID = function(...) list(id = "fakeid", name = "fake")
  )
  local_mocked_bindings(
    drive_upload = function(media, ...) {
      if (identical(media, f[1])) stop("boom: upload refused")
      invisible(NULL)
    },
    .package = "googledrive"
  )
  ## Before the fix, `is(<list>, "try-error")` was always FALSE, so this
  ## failure passed with no warning.
  expect_warning(
    out <- reproducible:::cloudUploadFromCache(
      isInCloud = FALSE, outputHash = hash, cachePath = cp,
      cloudFolderID = "fakeid", outputToSave = NULL, rasters = NULL, verbose = 0
    ),
    paste0(basename(f[1]), ": .*boom: upload refused")
  )
  expect_identical(names(out), basename(f[1]))

  ## All uploads succeeding gives no warning.
  local_mocked_bindings(drive_upload = function(...) invisible(NULL), .package = "googledrive")
  expect_no_warning(
    reproducible:::cloudUploadFromCache(
      isInCloud = FALSE, outputHash = hash, cachePath = cp,
      cloudFolderID = "fakeid", outputToSave = NULL, rasters = NULL, verbose = 0
    )
  )
})
