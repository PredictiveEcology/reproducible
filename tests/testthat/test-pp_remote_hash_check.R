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

test_that("sidecar fast-path records skipDownloadFile so pp_download keeps a real path", {
  # Regression: when archive = NA (regular non-archive file like RTM.tif) and
  # the sidecar fast-path skips download, pp_download synthesised
  #   downloaded = ctx$archive  (= NA)
  # which propagated NA into ctx$filesToChecksum and polluted downstream
  # checkSums lookups (eventually crashing with a data.table length-mismatch).
  # Now pp_remote_hash_check records the verified localFile in skipDownloadFile,
  # and pp_download uses it when ctx$archive is NA.
  testInit("terra", verbose = -1)

  localFile <- file.path(tmpdir, "RTM.tif")
  writeLines("not a real tif, just bytes", localFile)
  url <- "https://drive.google.com/open?id=10hnvjk8k9wYGgyZ7dBp7JvxKY0mblI4R"

  reproducible:::makeRemoteHashFile(
    url, tmpdir, basename(localFile),
    remoteHash = "fake-hash", algorithm = "md5", write = TRUE
  )

  ctx <- list(
    url              = url,
    archive          = NA,
    neededFiles      = localFile,
    destinationPath  = tmpdir,
    checkSumFilePath = file.path(tmpdir, "CHECKSUMS.txt"),
    checkSums        = reproducible:::.emptyChecksumsResult,
    needChecksums    = 0L,
    verbose          = -1,
    hashVerified     = character(),
    skipDownload     = FALSE,
    skipDownloadFile = NULL,
    remoteMetadata   = NULL
  )

  res <- reproducible:::pp_remote_hash_check(ctx)
  expect_true(isTRUE(res$skipDownload))
  expect_identical(res$skipDownloadFile, localFile)

  # pp_download fast-path must surface localFile as `downloaded` and propagate
  # it (not NA) into filesToChecksum.
  res$.callingEnv <- environment()
  res$verboseCFS  <- -1
  res$dlFunCaptured <- NULL
  res$dots        <- list()
  res$alsoExtract <- NULL
  res$targetFile  <- basename(localFile)
  res$.tempPath   <- tempdir2(rndstr(1, 6))
  on.exit(unlink(res$.tempPath, recursive = TRUE), add = TRUE)
  res$quick       <- TRUE
  res$overwrite   <- FALSE
  res$purge       <- FALSE

  out <- reproducible:::pp_download(res)
  expect_identical(out$filesToChecksum, localFile)
  expect_false(any(is.na(out$filesToChecksum)))
})

test_that("an opaque ETag is recorded in a sidecar and revalidated, not re-downloaded", {
  # Regression: a server whose ETag is opaque (raw.githubusercontent.com, S3,
  # any edge-generated token) can never match a locally computed digest, so the
  # `.hash` sidecar was never written and EVERY call re-contacted the remote and
  # re-downloaded. An ETag cannot be recomputed locally, but it can be handed
  # back via `If-None-Match`, which is what the server is for.
  testInit("terra", verbose = -1)

  localFile <- file.path(tmpdir, "opaque.tif")
  writeLines("some bytes", localFile)

  url <- "https://example.com/data/opaque.tif"
  ctx <- list(
    url              = url,
    archive          = NULL,
    neededFiles      = localFile,
    destinationPath  = tmpdir,
    checkSumFilePath = file.path(tmpdir, "CHECKSUMS.txt"),
    verbose          = -1,
    hashVerified     = character(),
    skipDownload     = FALSE,
    remoteMetadata   = NULL
  )

  ## an ETag-backed sidecar, as pp_download() now writes after a real download
  sidecar <- reproducible:::makeRemoteHashFile(
    url, tmpdir, basename(localFile), "\"etag-abc\"",
    algorithm = "etag", write = TRUE
  )
  expect_true(file.exists(sidecar))
  parsed <- reproducible:::.parseRemoteHashFile(sidecar)
  expect_identical(parsed$algorithm, "etag")
  expect_identical(parsed$hash, "\"etag-abc\"")

  ## 1. DEFAULT: the sidecar is trusted outright and the remote is never asked.
  ##    A long run must not have its caches invalidated because an upstream file
  ##    changed part-way through.
  called <- FALSE
  testthat::with_mocked_bindings(
    .remoteEtagRevalidate = function(...) { called <<- TRUE; list(unchanged = FALSE, etag = "\"new\"") },
    {
      res <- reproducible:::pp_remote_hash_check(ctx)
    }
  )
  expect_true(isTRUE(res$skipDownload))
  expect_false(called)                      # no network at all
  expect_identical(res$skipDownloadFile, localFile)

  ## 2. Opt in to re-checking: unchanged (304) -> still skip the download
  withr::local_options(reproducible.checkRemoteHash = TRUE)
  testthat::with_mocked_bindings(
    .remoteEtagRevalidate = function(url, etag) list(unchanged = TRUE, etag = etag),
    {
      res <- reproducible:::pp_remote_hash_check(ctx)
    }
  )
  expect_true(isTRUE(res$skipDownload))

  ## 3. Opt in, and the remote really changed -> download, carrying the new ETag
  ##    so pp_download() can record it
  testthat::with_mocked_bindings(
    .remoteEtagRevalidate = function(url, etag) list(unchanged = FALSE, etag = "\"etag-xyz\""),
    {
      res <- reproducible:::pp_remote_hash_check(ctx)
    }
  )
  expect_false(isTRUE(res$skipDownload))
  expect_identical(res$remoteEtagToRecord, "\"etag-xyz\"")

  ## 4. Opt in, but the remote is unreachable -> use the local file rather than
  ##    failing or re-downloading, so re-checking is safe offline
  testthat::with_mocked_bindings(
    .remoteEtagRevalidate = function(url, etag) list(unchanged = NA, etag = NULL),
    {
      res <- reproducible:::pp_remote_hash_check(ctx)
    }
  )
  expect_true(isTRUE(res$skipDownload))
  expect_identical(res$skipDownloadFile, localFile)
})

test_that("an opaque ETag is carried out of pp_remote_hash_check for recording", {
  # With no sidecar yet, an opaque ETag yields no local verification, so the
  # download proceeds -- but the ETag must be carried forward so pp_download()
  # can write the sidecar once the bytes are on disk.
  testInit("terra", verbose = -1)

  localFile <- file.path(tmpdir, "fresh.tif")
  writeLines("some bytes", localFile)

  ctx <- list(
    url              = "https://example.com/data/fresh.tif",
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
    targetFile      = "fresh.tif",
    fileSize        = file.size(localFile),   # same size -> no fast-fail
    remoteHash      = "W/opaque",
    remoteAlgorithm = "etag-opaque",
    timestampOnline = NULL,
    etag            = "W/\"opaque\""
  )

  testthat::with_mocked_bindings(
    getRemoteMetadata = function(...) fakeMeta,
    {
      res <- reproducible:::pp_remote_hash_check(ctx)
    }
  )

  expect_false(isTRUE(res$skipDownload))
  expect_identical(res$remoteEtagToRecord, "W/\"opaque\"")
})

test_that(".remoteEtagRevalidate reports NA when the ETag is missing", {
  # NA means "could not determine", which callers treat as "use the local file"
  expect_identical(reproducible:::.remoteEtagRevalidate("https://example.com/x", NULL)$unchanged, NA)
  expect_identical(reproducible:::.remoteEtagRevalidate("https://example.com/x", "")$unchanged, NA)
})
