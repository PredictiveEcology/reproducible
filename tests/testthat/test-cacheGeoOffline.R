## CacheGeo ledgers when the remote cannot be reached (no internet on a compute node).
## The remote is a shared-disk folder; "unreachable" is a mocked .geoRemoteState() that throws the
## error curl gives when it cannot resolve a host name.

offlineErr <- function() stop("Couldn't resolve host name [www.googleapis.com]", call. = FALSE)

## Returns a function that switches the remote off (TRUE) and on (FALSE)
mockReachability <- function(.env = parent.frame()) {
  state <- new.env()
  state$offline <- FALSE
  real <- reproducible:::.geoRemoteState
  testthat::local_mocked_bindings(
    .geoRemoteState = function(ledger) if (state$offline) offlineErr() else real(ledger),
    .package = "reproducible", .env = .env)
  function(offline) state$offline <- offline
}

offlineLedger <- function(dPath, remote) {
  CacheGeoLedger("toy.rds", remote = remote, destinationPath = dPath, key = "polygonID")
}

pendingFile <- function(dPath) file.path(dPath, ".toy.rds.unpushed.rds")

test_that("offline read uses the local copy", {
  testInit(c("sf", "terra"))
  remote <- checkPath(tempfile("remote"), create = TRUE)
  led <- offlineLedger(checkPath(tempfile("local"), create = TRUE), remote)
  CacheGeoWrite(led, toyRows(), verbose = 0)
  setOffline <- mockReachability()
  setOffline(TRUE)
  expect_message(rows <- CacheGeoRead(led, area = NULL, verbose = 1), "remote unreachable, using the local copy")
  expect_identical(sort(rows$polygonID), c("P1", "P2", "P3"))
})

test_that("offline read with no local copy is like no file", {
  testInit(c("sf", "terra"))
  remote <- checkPath(tempfile("remote"), create = TRUE)
  led <- offlineLedger(checkPath(tempfile("local"), create = TRUE), remote)
  setOffline <- mockReachability()
  setOffline(TRUE)
  expect_message(rows <- CacheGeoRead(led, area = NULL, verbose = 1), "no local copy")
  expect_identical(NROW(rows), 0L)
  expect_s3_class(rows, "sf")
})

test_that("offline write saves locally and records the rows as pending", {
  testInit(c("sf", "terra"))
  remote <- checkPath(tempfile("remote"), create = TRUE)
  dPath <- checkPath(tempfile("local"), create = TRUE)
  led <- offlineLedger(dPath, remote)
  CacheGeoWrite(led, toyRows(), verbose = 0)
  setOffline <- mockReachability()
  setOffline(TRUE)
  expect_message(CacheGeoWrite(led, toySquare("P4", 50, 60), verbose = 1), "not pushed")
  expect_true(file.exists(pendingFile(dPath)))
  expect_true("P4" %in% readRDS(file.path(dPath, "toy.rds"))$polygonID)
  expect_false("P4" %in% readRDS(file.path(remote, "toy.rds"))$polygonID)
  ## writes accumulate, with upsert semantics
  CacheGeoWrite(led, toySquare("P4", 50, 60, param = "again"), verbose = 0)
  CacheGeoWrite(led, toySquare("P5", 70, 80), verbose = 0)
  expect_setequal(readRDS(file.path(dPath, "toy.rds"))$polygonID, c("P1", "P2", "P3", "P4", "P5"))
  expect_identical(sum(readRDS(file.path(dPath, "toy.rds"))$polygonID == "P4"), 1L)
  ## the sidecar is hidden beside the local file
  expect_false(any(grepl("unpushed", list.files(dPath))))
})

test_that("a later write merges the pending rows with what another writer added meanwhile", {
  testInit(c("sf", "terra"))
  remote <- checkPath(tempfile("remote"), create = TRUE)
  dPath <- checkPath(tempfile("local"), create = TRUE)
  led <- offlineLedger(dPath, remote)
  CacheGeoWrite(led, toyRows(), verbose = 0)
  setOffline <- mockReachability()
  setOffline(TRUE)
  CacheGeoWrite(led, toySquare("P4", 50, 60), verbose = 0)
  ## another machine writes to the remote meanwhile
  other <- offlineLedger(checkPath(tempfile("other"), create = TRUE), remote)
  setOffline(FALSE)
  CacheGeoWrite(other, toySquare("P5", 70, 80), verbose = 0)
  CacheGeoWrite(led, toySquare("P6", 90, 100), verbose = 0)
  expected <- c("P1", "P2", "P3", "P4", "P5", "P6")
  expect_setequal(readRDS(file.path(remote, "toy.rds"))$polygonID, expected)
  expect_setequal(readRDS(file.path(dPath, "toy.rds"))$polygonID, expected)
  expect_false(file.exists(pendingFile(dPath)))
})

test_that("a read with a reachable remote returns remote + pending and pushes them", {
  testInit(c("sf", "terra"))
  remote <- checkPath(tempfile("remote"), create = TRUE)
  dPath <- checkPath(tempfile("local"), create = TRUE)
  led <- offlineLedger(dPath, remote)
  CacheGeoWrite(led, toyRows(), verbose = 0)
  setOffline <- mockReachability()
  setOffline(TRUE)
  CacheGeoWrite(led, toySquare("P4", 50, 60), verbose = 0)
  other <- offlineLedger(checkPath(tempfile("other"), create = TRUE), remote)
  setOffline(FALSE)
  CacheGeoWrite(other, toySquare("P5", 70, 80), verbose = 0)
  rows <- CacheGeoRead(led, area = NULL, verbose = 0)
  expect_setequal(rows$polygonID, c("P1", "P2", "P3", "P4", "P5"))
  expect_setequal(readRDS(file.path(remote, "toy.rds"))$polygonID, c("P1", "P2", "P3", "P4", "P5"))
  expect_false(file.exists(pendingFile(dPath)))
})

test_that("a read keeps the pending rows, and does not overwrite them, when there is no Google login", {
  testInit(c("sf", "terra"))
  remote <- checkPath(tempfile("remote"), create = TRUE)
  dPath <- checkPath(tempfile("local"), create = TRUE)
  led <- offlineLedger(dPath, remote)
  CacheGeoWrite(led, toyRows(), verbose = 0)
  setOffline <- mockReachability()
  setOffline(TRUE)
  CacheGeoWrite(led, toySquare("P4", 50, 60), verbose = 0)
  setOffline(FALSE)
  local_mocked_bindings(.geoHasLogin = function() FALSE, .package = "reproducible")
  rows <- CacheGeoRead(led, area = NULL, verbose = 0)
  expect_setequal(rows$polygonID, c("P1", "P2", "P3", "P4"))
  expect_true(file.exists(pendingFile(dPath)))
  expect_false("P4" %in% readRDS(file.path(remote, "toy.rds"))$polygonID)
})

test_that("CacheGeoPush pushes the pending rows, is a no-op without any, and errors while offline", {
  testInit(c("sf", "terra"))
  remote <- checkPath(tempfile("remote"), create = TRUE)
  dPath <- checkPath(tempfile("local"), create = TRUE)
  led <- offlineLedger(dPath, remote)
  CacheGeoWrite(led, toyRows(), verbose = 0)
  expect_message(CacheGeoPush(led, verbose = 1), "no pending")
  setOffline <- mockReachability()
  setOffline(TRUE)
  CacheGeoWrite(led, toySquare("P4", 50, 60), verbose = 0)
  expect_error(CacheGeoPush(led, verbose = 0), "unreachable")
  expect_true(file.exists(pendingFile(dPath)))
  setOffline(FALSE)
  CacheGeoPush(led, verbose = 0)
  expect_true("P4" %in% readRDS(file.path(remote, "toy.rds"))$polygonID)
  expect_false(file.exists(pendingFile(dPath)))
})

test_that("an offline write to a Google Drive ledger needs no Google login", {
  skip_if_not_installed("googledrive")
  testInit(c("sf", "terra"))
  dPath <- checkPath(tempfile("local"), create = TRUE)
  led <- CacheGeoLedger("toy.rds", remote = "fakeFolderId", remoteType = "drive",
                        destinationPath = dPath, key = "polygonID")
  local_mocked_bindings(.gdrivePrepareAuth = function(...) "anon", .gdriveRestoreAuth = function(...) NULL,
                        .geoRemoteState = function(ledger) offlineErr(), .package = "reproducible")
  expect_no_error(CacheGeoWrite(led, toyRows(), verbose = 0))
  expect_true(file.exists(pendingFile(dPath)))
  expect_setequal(readRDS(file.path(dPath, "toy.rds"))$polygonID, c("P1", "P2", "P3"))
})

test_that("an error that is not a connection failure still propagates", {
  testInit(c("sf", "terra"))
  remote <- checkPath(tempfile("remote"), create = TRUE)
  led <- offlineLedger(checkPath(tempfile("local"), create = TRUE), remote)
  CacheGeoWrite(led, toyRows(), verbose = 0)
  local_mocked_bindings(.geoRemoteState = function(ledger) stop("File not found (404)"),
                        .package = "reproducible")
  expect_error(CacheGeoRead(led, area = NULL, verbose = 0), "404")
  expect_error(CacheGeoWrite(led, toySquare("P4", 50, 60), verbose = 0), "404")
  expect_false(file.exists(pendingFile(dirname(led$path))))
})
