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
