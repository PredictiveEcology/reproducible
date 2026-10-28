## Google Drive metadata and CacheGeo ledgers on a machine with no internet (a compute node).
## googledrive is mocked: a counting drive_get()/drive_ls() that answers, or throws the error curl
## gives when it cannot resolve a host name.

gdriveOffline <- function(...) stop("Could not resolve host: www.googleapis.com", call. = FALSE)

fileId <- "1AbCdEfGhIjKlMnOpQrStUvWxYz0123456"
fileUrl <- paste0("https://drive.google.com/file/d/", fileId, "/view?usp=sharing")
folderUrl <- paste0("https://drive.google.com/drive/folders/", fileId)

## A dribble (a data.frame with a `drive_resource` list-column, as in test-cloudPublicFolder.R) for one file
fakeDribble <- function(md5 = "md5A", name = "file.rds") {
  res <- list(id = fileId, name = name, mimeType = "application/octet-stream", size = "10",
              md5Checksum = md5, modifiedTime = "2026-01-01T00:00:00Z", webViewLink = fileUrl)
  d <- data.frame(name = name, id = fileId, stringsAsFactors = FALSE)
  d$drive_resource <- list(res)
  class(d) <- c("dribble", "tbl_df", "tbl", "data.frame")
  d
}

## Mock Drive: every drive_* call counts; `state$answer` online, a connection error when
## `state$offline`, any other condition in `state$error`.
mockDrive <- function(answer = fakeDribble(), .env = parent.frame()) {
  state <- new.env()
  state$offline <- FALSE
  state$calls <- 0L
  state$answer <- answer
  state$error <- NULL
  ask <- function(...) {
    state$calls <- state$calls + 1L
    if (state$offline) gdriveOffline()
    if (!is.null(state$error)) stop(state$error)
    state$answer
  }
  testthat::local_mocked_bindings(drive_get = ask, drive_ls = ask, drive_mkdir = ask, drive_put = ask,
                                  .package = "googledrive", .env = .env)
  testthat::local_mocked_bindings(.gdriveHasToken = function() TRUE, .package = "reproducible", .env = .env)
  state
}

offlineOpts <- list(reproducible.gdriveAuthRetries = 0L, reproducible.useMemoise = FALSE)

test_that("Drive metadata is asked for every time online and the last answer is used offline", {
  skip_if_not_installed("googledrive")
  testInit("googledrive", opts = offlineOpts)
  withr::local_options(reproducible.cachePath = tmpCache)
  drive <- mockDrive()

  m1 <- .gdriveMetadata(fileId, verbose = 0)
  m2 <- .gdriveMetadata(fileId, verbose = 0)
  expect_identical(drive$calls, 2L) # Drive is asked every time: the md5 tells a changed file
  expect_identical(m2$drive_resource[[1]]$md5Checksum, "md5A")
  drive$answer <- fakeDribble("md5B") # the remote file changed
  expect_identical(.gdriveMetadata(fileId, verbose = 0)$drive_resource[[1]]$md5Checksum, "md5B")

  drive$offline <- TRUE
  expect_message(m4 <- .gdriveMetadata(fileId, verbose = 1), "cannot be reached")
  expect_identical(m4$drive_resource[[1]]$md5Checksum, "md5B") # the last answer seen online
  ## a folder listing is kept apart from the file metadata
  expect_error(.gdriveMetadata(fileId, ls = TRUE, verbose = 0), "resolve host")
  ## nothing saved for this id: the error is raised
  expect_error(.gdriveMetadata("1zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz", verbose = 0), "resolve host")
  ## any other error is raised as it is
  drive$offline <- FALSE
  drive$error <- "File not found (404)"
  expect_error(.gdriveMetadata(fileId, verbose = 0), "File not found")
})

## Count the times the saved answer is replaced: Cache(useCache = "overwrite") clears the entry with
## clearCacheOverwrite() (R/cache.R) before saving the new one.
countOverwrites <- function(.env = parent.frame()) {
  state <- new.env()
  state$n <- 0L
  clearing <- reproducible:::clearCacheOverwrite
  counting <- function(...) { state$n <- state$n + 1L; clearing(...) }
  testthat::local_mocked_bindings(clearCacheOverwrite = counting, .package = "reproducible", .env = .env)
  state
}

test_that("the saved Drive answer is rewritten only when the remote file changed", {
  skip_if_not_installed("googledrive")
  testInit("googledrive", opts = offlineOpts)
  withr::local_options(reproducible.cachePath = tmpCache)
  drive <- mockDrive()
  saves <- countOverwrites()

  .gdriveMetadata(fileId, verbose = 0)
  .gdriveMetadata(fileId, verbose = 0)
  .gdriveMetadata(fileId, verbose = 0)
  expect_identical(saves$n, 0L) # first call saved (not an overwrite); same answer: not rewritten
  expect_identical(drive$calls, 3L) # but Drive is still asked every time

  ## a per-request field changes (a new link, a new field): not a change of the file
  d <- fakeDribble()
  d$drive_resource[[1]]$webViewLink <- "https://drive.google.com/other"
  d$drive_resource[[1]]$thumbnailLink <- "https://lh3.example/expires-soon"
  drive$answer <- d
  expect_identical(.gdriveMetadata(fileId, verbose = 0), d) # the fresh answer is what is returned
  expect_identical(saves$n, 0L)

  drive$answer <- fakeDribble("md5B") # a different md5
  expect_identical(.gdriveMetadata(fileId, verbose = 0)$drive_resource[[1]]$md5Checksum, "md5B")
  expect_identical(saves$n, 1L)
})

test_that("a Drive answer saved by 3.2.1.9078 is found offline: the store and its cache key are unchanged", {
  skip_if_not_installed("googledrive")
  testInit("googledrive", opts = offlineOpts)
  withr::local_options(reproducible.cachePath = tmpCache)
  drive <- mockDrive()
  drive$offline <- TRUE

  ## the call .gdriveMetadata() made in 3.2.1.9078 to save what Drive said (R/download.R)
  Cache(.gdriveMetadataSaved, fileId, NULL, FALSE, answer = fakeDribble("md5old"), omitArgs = "answer",
        useCache = "overwrite", verbose = FALSE)
  ## the cacheId 9078 wrote: the store's name, formals and body are part of it. Not under covr,
  ## whose tracing rewrites the body (a different function, so a different key).
  if (!isTRUE(as.logical(Sys.getenv("R_COVR", "false"))))
    expect_true("a8aca050fea83621" %in% showCache(tmpCache, verbose = 0)$cacheId)

  expect_message(m <- .gdriveMetadata(fileId, verbose = 1), "cannot be reached")
  expect_identical(m$drive_resource[[1]]$md5Checksum, "md5old")
  expect_identical(drive$calls, 1L)
})

test_that("assessGoogle() and getRemoteMetadata() answer offline from the last online answer", {
  skip_if_not_installed("googledrive")
  testInit("googledrive", opts = offlineOpts)
  withr::local_options(reproducible.cachePath = tmpCache)
  drive <- mockDrive()

  online <- assessGoogle(fileUrl, destinationPath = tmpdir, verbose = 0)
  meta <- getRemoteMetadata(url = fileUrl)
  expect_identical(meta$remoteHash, "md5A")
  drive$offline <- TRUE
  expect_message(offline <- assessGoogle(fileUrl, destinationPath = tmpdir, verbose = 1), "cannot be reached")
  expect_identical(offline, online)
  expect_identical(getRemoteMetadata(url = fileUrl), meta)
})

## Mock the HEAD request of a non-Drive URL, as mockDrive() does for Drive: `state$answer` is the
## httr2 response online, a connection error when `state$offline`, any other condition in `state$error`.
mockHead <- function(answer = fakeHeadResponse(), .env = parent.frame()) {
  state <- new.env()
  state$offline <- FALSE
  state$calls <- 0L
  state$answer <- answer
  state$error <- NULL
  testthat::local_mocked_bindings(req_perform = function(...) {
    state$calls <- state$calls + 1L
    if (state$offline) gdriveOffline()
    if (!is.null(state$error)) stop(state$error)
    state$answer
  }, .package = "httr2", .env = .env)
  state
}

fakeHeadResponse <- function(etag = "\"etagA\"", name = "file.tif") {
  httr2::response(status_code = 200L, headers = list(
    etag = etag, `content-length` = "10", Date = "Mon, 01 Jan 2026 00:00:00 GMT",
    `content-disposition` = paste0("attachment; filename=\"", name, "\"")))
}

test_that("getRemoteMetadata() on a non-Drive URL answers offline from the last online answer", {
  skip_if_not_installed("httr2")
  testInit(opts = offlineOpts)
  withr::local_options(reproducible.cachePath = tmpCache)
  head <- mockHead()
  url <- "https://example.org/bucket/file.tif"
  other <- "https://example.org/bucket/other.tif"

  m1 <- getRemoteMetadata(isGDurl = FALSE, url = url)
  m2 <- getRemoteMetadata(isGDurl = FALSE, url = url)
  expect_identical(head$calls, 2L) # the server is asked every time: the etag tells a changed file
  expect_identical(m2$remoteHash, "etagA")
  expect_identical(m2$etag, "\"etagA\"")
  expect_identical(m2$fileSize, 10)
  expect_identical(m2$targetFile, "file.tif")
  head$answer <- fakeHeadResponse("\"etagB\"") # the remote file changed
  expect_identical(getRemoteMetadata(isGDurl = FALSE, url = url)$remoteHash, "etagB")

  head$offline <- TRUE
  expect_message(m4 <- getRemoteMetadata(isGDurl = FALSE, url = url), "cannot be reached")
  expect_identical(m4$remoteHash, "etagB") # the last answer seen online
  expect_identical(m4$targetFile, "file.tif")
  ## a supplied targetFile is kept; the rest comes from the saved answer
  expect_identical(getRemoteMetadata(targetFile = "mine.tif", isGDurl = FALSE, url = url)$targetFile, "mine.tif")
  ## nothing saved for this URL: the error is raised
  expect_error(getRemoteMetadata(isGDurl = FALSE, url = other), "resolve host")
  ## any other error is raised as it is
  head$offline <- FALSE
  head$error <- "HTTP 404 Not Found"
  expect_error(getRemoteMetadata(isGDurl = FALSE, url = url), "404")
})

test_that("the sign-in step before the lookup does not stop the offline path (no loaded token)", {
  skip_if_not_installed("googledrive")
  testInit("googledrive", opts = c(offlineOpts, list(gargle_oauth_email = "a@b.com", reproducible.gdriveNoAuth = NULL)))
  withr::local_options(reproducible.cachePath = tmpCache)
  drive <- mockDrive()
  testthat::local_mocked_bindings(.gdriveHasToken = function() FALSE, .package = "reproducible")
  testthat::local_mocked_bindings(drive_auth = function(...) invisible(NULL), drive_deauth = function(...) invisible(NULL),
                                  .package = "googledrive")

  online <- assessGoogle(fileUrl, destinationPath = tmpdir, verbose = 0)
  drive$offline <- TRUE
  expect_message(offline <- assessGoogle(fileUrl, destinationPath = tmpdir, verbose = 1), "cannot be reached")
  expect_identical(offline, online)
})

test_that("a CacheGeo ledger on Google Drive says the remote cannot be reached, keeps the local file", {
  skip_if_not_installed("googledrive")
  testInit(c("googledrive", "sf", "terra"), opts = offlineOpts)
  dPath <- checkPath(file.path(tmpdir, "local"), create = TRUE)
  led <- CacheGeoLedger("toy.rds", remote = folderUrl, destinationPath = dPath, key = "polygonID")
  drive <- mockDrive()
  drive$offline <- TRUE

  expect_message(rows <- CacheGeoRead(led, area = NULL, verbose = 1), "remote unreachable")
  expect_identical(NROW(rows), 0L)

  ## a write keeps the local file, says it was not pushed, and does not touch Drive
  expect_message(CacheGeoWrite(led, toyRows(), verbose = 1), "not pushed")
  expect_true(file.exists(file.path(dPath, ".toy.rds.unpushed.rds")))
  expect_identical(sort(readRDS(file.path(dPath, "toy.rds"))$polygonID), c("P1", "P2", "P3"))

  expect_message(rows <- CacheGeoRead(led, area = NULL, verbose = 1), "remote unreachable, using the local copy")
  expect_identical(sort(rows$polygonID), c("P1", "P2", "P3"))
})

test_that("the saved HEAD answer is rewritten only when the ETag, size or name changed, not the Date", {
  skip_if_not_installed("httr2")
  testInit(opts = offlineOpts)
  withr::local_options(reproducible.cachePath = tmpCache)
  head <- mockHead()
  saves <- countOverwrites()
  url <- "https://example.org/bucket/file.tif"

  getRemoteMetadata(isGDurl = FALSE, url = url)
  expect_identical(saves$n, 0L)
  r <- fakeHeadResponse()
  r$headers$Date <- "Tue, 02 Jan 2026 00:00:00 GMT" # every request has its own Date
  head$answer <- r
  getRemoteMetadata(isGDurl = FALSE, url = url)
  expect_identical(saves$n, 0L)
  head$answer <- fakeHeadResponse("\"etagB\"")
  expect_identical(getRemoteMetadata(isGDurl = FALSE, url = url)$remoteHash, "etagB")
  expect_identical(saves$n, 1L)
})
