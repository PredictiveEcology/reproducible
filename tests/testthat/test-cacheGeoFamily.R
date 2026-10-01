## CacheGeoLedger() / CacheGeoRead() / CacheGeoWrite() and CacheGeo() on top of them.
## Everything here runs without Google credentials: the shared-disk remote is a folder.

toyCRS <- "EPSG:32618"

## A square, `x0:x1` by `y0:y1` kilometres, as one sf row with a key and a list-column.
toySquare <- function(id, x0, x1, y0 = 0, y1 = 10, crs = toyCRS, param = id) {
  box <- sf::st_polygon(list(rbind(c(x0, y0), c(x1, y0), c(x1, y1), c(x0, y1), c(x0, y0))) )
  geom <- sf::st_sfc(box * 1000, crs = crs)
  out <- sf::st_sf(polygonID = id, geometry = geom)
  out$params <- I(list(list(p = param)))
  out
}

## Three polygons: P1 and P2 share an edge, P3 is apart (a gap from 20 to 30 km).
toyRows <- function() {
  rbind(toySquare("P1", 0, 10), toySquare("P2", 10, 20), toySquare("P3", 30, 40))
}

toyArea <- function(x0, x1, y0 = 2, y1 = 8, crs = toyCRS) toySquare("area", x0, x1, y0, y1, crs)[, "polygonID"]

newToyLedger <- function(dPath, rows = toyRows(), ...) {
  led <- CacheGeoLedger("toy.rds", destinationPath = dPath, key = "polygonID", ...)
  if (!is.null(rows)) CacheGeoWrite(led, rows, verbose = 0)
  led
}

test_that("CacheGeoLedger describes a ledger and does no I/O", {
  dPath <- file.path(tempfile("noIO"), "never")
  remote <- file.path(tempfile("noIORemote"), "never")
  led <- CacheGeoLedger("toy.rds", remote = remote, destinationPath = dPath, key = "polygonID")
  expect_s3_class(led, "CacheGeoLedger")
  expect_false(dir.exists(dPath))
  expect_false(dir.exists(remote))
  expect_output(print(led), "toy.rds")
})

test_that("CacheGeoRead match = intersects / covers / within on three polygons", {
  testInit(c("sf", "terra"))
  led <- newToyLedger(checkPath(tempdir2(), create = TRUE))
  ids <- function(...) sort(CacheGeoRead(led, ..., verbose = 0)$polygonID)

  ## intersects: every row overlapping the area, including the ones that do not cover it
  expect_identical(ids(toyArea(2, 8)), "P1")
  expect_identical(ids(toyArea(5, 15)), c("P1", "P2"))
  expect_identical(ids(toyArea(5, 35)), c("P1", "P2", "P3"))
  ## a row that only touches the area along an edge is not an overlap
  expect_identical(ids(toyArea(20, 25)), character(0))

  ## covers: rows are returned only when their union covers the area
  expect_identical(ids(toyArea(2, 8), match = "covers"), "P1")
  expect_identical(ids(toyArea(5, 15), match = "covers"), c("P1", "P2"))
  expect_identical(ids(toyArea(5, 35), match = "covers"), character(0)) # the gap 20-30 km

  ## within: rows lying inside the area
  expect_identical(ids(toyArea(2, 8), match = "within"), character(0))
  expect_identical(ids(toyArea(-5, 25, -5, 15), match = "within"), c("P1", "P2"))
  expect_identical(ids(toyArea(-5, 45, -5, 15), match = "within"), c("P1", "P2", "P3"))

  ## area = NULL is the whole ledger, and has to be asked for
  expect_identical(sort(CacheGeoRead(led, area = NULL, verbose = 0)$polygonID), c("P1", "P2", "P3"))
  expect_error(CacheGeoRead(led, verbose = 0), "area")
})

test_that("CacheGeoRead always returns sf, with the list-columns, and 0 rows when nothing matches", {
  testInit(c("sf", "terra"))
  led <- newToyLedger(checkPath(tempdir2(), create = TRUE))
  hit <- CacheGeoRead(led, toyArea(2, 8), verbose = 0)
  expect_s3_class(hit, "sf")
  expect_identical(hit$params[[1]], list(p = "P1"))
  expect_identical(sf::st_crs(hit), sf::st_crs(toyCRS))

  none <- CacheGeoRead(led, toyArea(100, 110), verbose = 0)
  expect_s3_class(none, "sf")
  expect_identical(NROW(none), 0L)
  expect_true(all(c("polygonID", "params") %in% names(none)))

  ## a ledger that does not exist yet is a 0-row sf too
  empty <- CacheGeoLedger("absent.rds", destinationPath = checkPath(tempdir2(), create = TRUE))
  expect_identical(NROW(CacheGeoRead(empty, area = NULL, verbose = 0)), 0L)
})

test_that("CacheGeoRead transforms an area in another CRS; a SpatVector area is accepted", {
  testInit(c("sf", "terra"))
  led <- newToyLedger(checkPath(tempdir2(), create = TRUE))
  areaLonLat <- sf::st_transform(toyArea(2, 8), 4326)
  expect_identical(CacheGeoRead(led, areaLonLat, verbose = 0)$polygonID, "P1")
  expect_identical(CacheGeoRead(led, terra::vect(areaLonLat), verbose = 0)$polygonID, "P1")
  ## the result stays in the ledger's CRS
  expect_identical(sf::st_crs(CacheGeoRead(led, areaLonLat, verbose = 0)), sf::st_crs(toyCRS))
})

test_that("tolerance drops a sliver and names it", {
  testInit(c("sf", "terra"))
  led <- newToyLedger(checkPath(tempdir2(), create = TRUE))
  ## 9.99 to 15 km: only a 10 m strip of P1, but all of the way across P2
  area <- toyArea(9.99, 15)
  expect_identical(sort(CacheGeoRead(led, area, verbose = 0)$polygonID), c("P1", "P2"))
  expect_message(rows <- CacheGeoRead(led, area, tolerance = 50, verbose = 1), "P1")
  expect_identical(rows$polygonID, "P2")

  ## covers: a gap narrower than the tolerance is ignored
  led2 <- newToyLedger(checkPath(tempdir2(), create = TRUE),
                       rows = rbind(toySquare("P1", 0, 10), toySquare("P2", 10.01, 20)))
  expect_identical(NROW(CacheGeoRead(led2, toyArea(5, 15), match = "covers", verbose = 0)), 0L)
  expect_identical(NROW(CacheGeoRead(led2, toyArea(5, 15), match = "covers", tolerance = 50,
                                     verbose = 0)), 2L)
})

test_that("CacheGeoWrite upserts by key and appends on request", {
  testInit(c("sf", "terra"))
  dPath <- checkPath(tempdir2(), create = TRUE)
  led <- newToyLedger(dPath)
  file <- file.path(dPath, "toy.rds")

  CacheGeoWrite(led, toySquare("P2", 10, 20, param = "refit"), verbose = 0)
  all <- readRDS(file)
  expect_identical(sort(all$polygonID), c("P1", "P2", "P3")) # one row per key
  expect_identical(all$params[[which(all$polygonID == "P2")]], list(p = "refit"))
  expect_identical(all$params[[which(all$polygonID == "P1")]], list(p = "P1")) # others untouched

  CacheGeoWrite(led, toySquare("P2", 10, 20, param = "twice"), mode = "append", verbose = 0)
  expect_identical(sum(readRDS(file)$polygonID == "P2"), 2L)

  ## no change, no new file content
  before <- digest::digest(file = file)
  CacheGeoWrite(led, toySquare("P1", 0, 10), verbose = 0)
  expect_identical(digest::digest(file = file), before)

  ## a new column in the new row is kept; the old rows get NA
  newRow <- toySquare("P4", 50, 60)
  newRow$extra <- 7
  CacheGeoWrite(led, newRow, verbose = 0)
  expect_identical(readRDS(file)$extra[readRDS(file)$polygonID == "P4"], 7)
})

test_that("a ledger with no key replaces a row of the same geometry", {
  testInit(c("sf", "terra"))
  dPath <- checkPath(tempdir2(), create = TRUE)
  led <- CacheGeoLedger("nokey.rds", destinationPath = dPath)
  CacheGeoWrite(led, toySquare("P1", 0, 10, param = "a"), verbose = 0)
  CacheGeoWrite(led, toySquare("P1", 0, 10, param = "b"), verbose = 0)
  CacheGeoWrite(led, toySquare("P2", 10, 20, param = "c"), verbose = 0)
  all <- readRDS(file.path(dPath, "nokey.rds"))
  expect_identical(NROW(all), 2L)
  expect_identical(all$params[[1]], list(p = "b"))
})

test_that("CacheGeoRead re-reads when the ledger file changes (md5 keyed)", {
  testInit(c("sf", "terra"))
  dPath <- checkPath(tempdir2(), create = TRUE)
  led <- newToyLedger(dPath, rows = toySquare("P1", 0, 10))
  expect_identical(NROW(CacheGeoRead(led, area = NULL, verbose = 0)), 1L)
  ## another job rewrites the file behind our back, with the same name and the same size class
  df <- as.data.frame(toyRows())
  saveRDS(df, file.path(dPath, "toy.rds"))
  expect_identical(NROW(CacheGeoRead(led, area = NULL, verbose = 0)), 3L)
  df$params[[1]] <- list(p = "changed")
  saveRDS(df, file.path(dPath, "toy.rds"))
  expect_identical(CacheGeoRead(led, toyArea(2, 8), verbose = 0)$params[[1]], list(p = "changed"))
})

test_that("a ledger file with a `crs` column and no crs on the geometry still reads", {
  testInit(c("sf", "terra"))
  dPath <- checkPath(tempdir2(), create = TRUE)
  df <- as.data.frame(toyRows())
  df$geometry <- sf::st_set_crs(sf::st_geometry(toyRows()), NA) # an old file: no crs on the sfc
  df$crs <- I(rep(list(sf::st_crs(toyCRS)), NROW(df)))
  saveRDS(df, file.path(dPath, "old.rds"))
  led <- CacheGeoLedger("old.rds", destinationPath = dPath, key = "polygonID")
  rows <- CacheGeoRead(led, toyArea(2, 8), verbose = 0)
  expect_identical(rows$polygonID, "P1")
  expect_identical(sf::st_crs(rows), sf::st_crs(toyCRS))
})

test_that("CacheGeoRead has no side effects on a shared-disk remote", {
  testInit(c("sf", "terra"))
  dPath <- checkPath(tempdir2(), create = TRUE)
  remote <- file.path(tempdir2(), "ledgers")
  led <- CacheGeoLedger("toy.rds", remote = remote, destinationPath = dPath, key = "polygonID")
  expect_identical(NROW(CacheGeoRead(led, area = NULL, verbose = 0)), 0L)
  expect_false(dir.exists(remote))
  expect_false(file.exists(file.path(dPath, "toy.rds")))

  CacheGeoWrite(led, toyRows(), verbose = 0)
  expect_true(file.exists(file.path(remote, "toy.rds")))
  remoteMd5 <- digest::digest(file = file.path(remote, "toy.rds"))
  filesBefore <- sort(list.files(remote, all.files = TRUE))
  rows <- CacheGeoRead(led, toyArea(2, 8), verbose = 0)
  expect_identical(rows$polygonID, "P1")
  expect_identical(digest::digest(file = file.path(remote, "toy.rds")), remoteMd5)
  expect_identical(sort(list.files(remote, all.files = TRUE)), filesBefore)
})

test_that("a second user reads what the first wrote, and skips the copy when the md5 matches", {
  testInit(c("sf", "terra"))
  remote <- file.path(tempdir2(), "ledgers")
  user1 <- CacheGeoLedger("toy.rds", remote = remote, destinationPath = checkPath(tempdir2(), create = TRUE),
                          key = "polygonID")
  user2 <- CacheGeoLedger("toy.rds", remote = remote, destinationPath = checkPath(tempdir2(), create = TRUE),
                          key = "polygonID")
  CacheGeoWrite(user1, toyRows(), verbose = 0)
  expect_message(CacheGeoRead(user2, toyArea(2, 8), verbose = 1), "downloaded")
  expect_message(CacheGeoRead(user2, toyArea(2, 8), verbose = 1), "up to date")
  CacheGeoWrite(user1, toySquare("P4", 50, 60), verbose = 0)
  expect_identical(NROW(CacheGeoRead(user2, area = NULL, verbose = 0)), 4L)
})

test_that("CacheGeoWrite to a read-only URL stops", {
  testInit(c("sf", "terra"))
  led <- CacheGeoLedger("toy.rds", remote = "https://example.org/ledgers",
                        destinationPath = checkPath(tempdir2(), create = TRUE))
  expect_error(CacheGeoWrite(led, toyRows(), verbose = 0), "read-only")
})

test_that("a local-only ledger used twice works", {
  testInit(c("sf", "terra"))
  dPath <- checkPath(tempdir2(), create = TRUE)
  led <- CacheGeoLedger("local.rds", destinationPath = dPath, key = "polygonID")
  calls <- 0
  compute <- function(area) {
    calls <<- calls + 1
    toySquare("P1", 0, 10)
  }
  first <- CacheGeo(led, toyArea(2, 8), compute, verbose = 0)
  second <- CacheGeo(led, toyArea(2, 8), compute, verbose = 0)
  expect_identical(calls, 1)
  expect_identical(first$polygonID, "P1")
  expect_identical(second$polygonID, "P1")
  ## the old call with purge = 7 (it is ignored: a local-only ledger is never purged)
  expect_no_error(
    CacheGeo(targetFile = "local.rds", domain = toyArea(2, 8), destinationPath = dPath, purge = 7,
             verbose = 0)
  )
})

test_that("CacheGeo computes only when the area is not covered, and compute errors propagate", {
  testInit(c("sf", "terra"))
  led <- newToyLedger(checkPath(tempdir2(), create = TRUE), rows = toySquare("P1", 0, 10))
  calls <- character()
  compute <- function(area) {
    calls <<- c(calls, "run")
    toySquare("P2", 10, 20)
  }
  CacheGeo(led, toyArea(2, 8), compute, verbose = 0)
  expect_length(calls, 0)
  out <- CacheGeo(led, toyArea(5, 15), compute, verbose = 0)
  expect_length(calls, 1)
  expect_identical(sort(out$polygonID), c("P1", "P2"))
  CacheGeo(led, toyArea(5, 15), compute, verbose = 0)
  expect_length(calls, 1)

  boom <- function(area) stop("the fit failed")
  expect_error(CacheGeo(led, toyArea(100, 110), boom, verbose = 0), "the fit failed")
})

## fireSense_ignitionFit keeps xgboost models in a list-column. Read back from an rds, such a column is
## an ALTREP list that data.table::copy() / as.data.table() stop on ("ALTLIST classes must provide a
## Set_elt method"), so ledger rows must never go through them.
test_that("a list-column holding an xgboost model survives write, read, upsert and a second writer", {
  skip_if_not_installed("xgboost")
  testInit(c("sf", "terra"))
  x <- cbind(a = c(0, 1, 2, 3, 4, 5), b = c(1, 0, 1, 0, 1, 0))
  model <- xgboost::xgb.train(params = list(objective = "binary:logistic"),
                              data = xgboost::xgb.DMatrix(x, label = c(0, 1, 0, 1, 0, 1)), nrounds = 2)
  withModel <- function(id, x0, x1) {
    row <- toySquare(id, x0, x1)
    row$fit <- I(list(list(model)))
    row
  }
  remote <- file.path(tempdir2(), "ledgers")
  led <- CacheGeoLedger("boost.rds", remote = remote, destinationPath = checkPath(tempdir2(), create = TRUE),
                        key = "polygonID")
  CacheGeoWrite(led, withModel("A", 0, 10), verbose = 0)
  CacheGeoWrite(led, withModel("B", 10, 20), verbose = 0) # A is read back from the rds here
  CacheGeoWrite(led, withModel("A", 0, 10), verbose = 0)  # an upsert
  other <- CacheGeoLedger("boost.rds", remote = remote, destinationPath = checkPath(tempdir2(), create = TRUE),
                          key = "polygonID")
  rows <- CacheGeoRead(other, area = NULL, verbose = 0)
  expect_identical(rows$polygonID, c("A", "B"))
  expect_length(predict(rows$fit[[2]][[1]], x), 6L)
})

test_that("two processes upserting different keys into one shared-disk ledger both survive", {
  skip_on_cran()
  skip_if_not_installed("sf")
  cp <- checkPath(tempfile("concurrent"), create = TRUE)
  remote <- file.path(cp, "ledgers")
  rowsFile <- file.path(cp, "rows.rds")
  nProc <- 2L
  nEach <- 4L
  script <- file.path(cp, "writer.R")
  go <- file.path(cp, "go")
  writeLines(c(
    childProcessPreamble(),
    'p <- commandArgs(trailingOnly = TRUE)[1]',
    sprintf('led <- CacheGeoLedger("toy.rds", remote = "%s", destinationPath = file.path("%s", paste0("local", p)), key = "polygonID")',
            remote, cp),
    sprintf('file.create(paste0("%s", p))', file.path(cp, "ready")),
    sprintf('while (!file.exists("%s")) Sys.sleep(0.05)', go),
    sprintf('for (i in seq_len(%d)) {', nEach),
    '  x0 <- (as.integer(p) * 100 + i) * 20',
    '  box <- sf::st_polygon(list(rbind(c(x0, 0), c(x0 + 10, 0), c(x0 + 10, 10), c(x0, 10), c(x0, 0))))',
    '  row <- sf::st_sf(polygonID = paste0("w", p, "_", i), geometry = sf::st_sfc(box, crs = 32618))',
    '  CacheGeoWrite(led, row, verbose = 0)',
    '}',
    'cat("WRITER DONE\\n")'
  ), script)
  logs <- file.path(cp, paste0("writer", seq_len(nProc), ".log"))
  Rscript <- file.path(R.home("bin"), "Rscript")
  Map(function(p, lg) system2(Rscript, c(shQuote(script), p), stdout = lg, stderr = lg, wait = FALSE),
      seq_len(nProc), logs)
  finished <- function() vapply(logs, function(lg) file.exists(lg) &&
                                  any(grepl("^WRITER DONE|^Error", readLines(lg, warn = FALSE))), logical(1))
  deadline <- Sys.time() + 300
  while (sum(file.exists(paste0(file.path(cp, "ready"), seq_len(nProc)))) < nProc &&
         !all(finished()) && Sys.time() < deadline) Sys.sleep(0.1)
  file.create(go)
  deadline <- Sys.time() + 300
  while (!all(finished()) && Sys.time() < deadline) Sys.sleep(0.5)
  expect_true(all(vapply(logs, function(lg) "WRITER DONE" %in% readLines(lg, warn = FALSE), logical(1))))
  final <- readRDS(file.path(remote, "toy.rds"))
  expect_identical(NROW(final), nProc * nEach)
  expect_identical(anyDuplicated(final$polygonID), 0L)
})

## Google Drive: needs a token, as the other Drive tests do. The folder is created for the test.
test_that("Drive remote: write, public-style read, no upload on read, no folder creation on read", {
  skip_on_cran()
  testInit(c("sf", "terra"), needGoogleDriveAuth = TRUE)
  folderName <- paste0("cacheGeoFamily_", rndstr(1, 6))
  folder <- googledrive::drive_mkdir(folderName)
  on.exit(try(googledrive::drive_rm(folder), silent = TRUE), add = TRUE)
  remote <- googledrive::as_id(folder$id)

  led <- CacheGeoLedger("toy.rds", remote = remote, destinationPath = checkPath(tempdir2(), create = TRUE),
                        key = "polygonID")
  CacheGeoWrite(led, toyRows(), verbose = 0)
  inFolder <- googledrive::drive_ls(remote)
  expect_identical(inFolder$name, "toy.rds")
  md5 <- inFolder$drive_resource[[1]]$md5Checksum

  ## a reader with its own empty destinationPath
  reader <- CacheGeoLedger("toy.rds", remote = remote, destinationPath = checkPath(tempdir2(), create = TRUE),
                           key = "polygonID")
  expect_identical(CacheGeoRead(reader, toyArea(2, 8), verbose = 0)$polygonID, "P1")
  expect_message(CacheGeoRead(reader, toyArea(2, 8), verbose = 1), "up to date")

  ## writing the same content again does not upload
  CacheGeoWrite(led, toySquare("P1", 0, 10), verbose = 0)
  expect_identical(googledrive::drive_ls(remote)$drive_resource[[1]]$md5Checksum, md5)
  expect_identical(NROW(googledrive::drive_ls(remote)), 1L)

  ## a read of a folder name that does not exist creates nothing
  before <- NROW(googledrive::drive_find(pattern = paste0("^", folderName, "_missing$")))
  ghost <- CacheGeoLedger("toy.rds", remote = paste0(folderName, "_missing"), remoteType = "drive",
                          destinationPath = checkPath(tempdir2(), create = TRUE))
  suppressWarnings(CacheGeoRead(ghost, area = NULL, verbose = 0))
  expect_identical(NROW(googledrive::drive_find(pattern = paste0("^", folderName, "_missing$"))), before)
})
