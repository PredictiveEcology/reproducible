## A local targetFile that changes between calls (another job appended to the ledger) must be read
## again: the read was cached on the file's path alone, and `useCache = FALSE` did not reach it.
test_that("CacheGeo reads a changed local targetFile again", {
  testInit(c("sf", "terra"), opts = list("reproducible.overwrite" = TRUE))
  dPath <- checkPath(tempdir2(), create = TRUE)
  full <- sf::st_read(system.file("ex/lux.shp", package = "terra"), quiet = TRUE)[, c("NAME_2", "AREA")]
  targetFile <- "ledger.rds"
  writeLedger <- function(rows) saveRDS(as.data.frame(full[rows, ]), file.path(dPath, targetFile))

  for (uc in list(FALSE, TRUE)) {
    writeLedger(1)
    first <- CacheGeo(targetFile = targetFile, destinationPath = dPath, action = "nothing",
                      useCache = uc, verbose = 0)
    expect_equal(NROW(first), 1L)
    writeLedger(1:3)                                   # another job appended two polygons
    second <- CacheGeo(targetFile = targetFile, destinationPath = dPath, action = "nothing",
                       useCache = uc, verbose = 0)
    expect_equal(NROW(second), 3L, info = paste("useCache =", uc))
    unlink(file.path(dPath, targetFile))
    clearCache(ask = FALSE)
  }

  ## useCache = FALSE writes nothing to the cache
  writeLedger(1:2)
  CacheGeo(targetFile = targetFile, destinationPath = dPath, action = "nothing",
           useCache = FALSE, verbose = 0)
  expect_equal(NROW(showCache(verbose = 0)), 0L)
})

## `action = "update"` on a domain already covered by an existing row returned that old
## row without ever calling FUN or writing to disk, i.e. a refit (refitExisting = TRUE
## in fireSense_SpreadFit) was silently lost. FUN must be evaluated and the matching
## row(s) replaced.
test_that("CacheGeo update replaces the existing row for a refit (same polygonID)", {
  testInit(c("sf", "terra"), opts = list("reproducible.overwrite" = TRUE))
  dPath <- checkPath(tempdir2(), create = TRUE)
  full <- sf::st_read(system.file("ex/lux.shp", package = "terra"), quiet = TRUE)[, c("NAME_2", "AREA")]
  zoneA <- full[3, ]
  targetFile <- "ledger.rds"

  writeLedgerRow <- function(zone, id, param) {
    df <- as.data.frame(zone)
    df$polygonID <- id
    df$params <- I(list(param))
    saveRDS(df, file.path(dPath, targetFile))
  }
  writeLedgerRow(zoneA, "A", "p1")

  fun <- function(domain, id, param) {
    d <- as.data.frame(domain)
    d$polygonID <- id
    d$params <- I(list(param))
    d
  }

  out <- CacheGeo(targetFile = targetFile, domain = zoneA,
                   FUN = fun(domain, id = "A", param = "p2"),
                   fun = fun, destinationPath = dPath, action = "update",
                   useCache = FALSE, verbose = 0)

  expect_equal(as.data.frame(out)$params[[1]], "p2")

  ledger <- readRDS(file.path(dPath, targetFile))
  expect_equal(NROW(ledger), 1L)
  expect_equal(ledger$polygonID, "A")
  expect_equal(ledger$params[[1]], "p2")
})

test_that("CacheGeo update of one polygon leaves a second polygon untouched", {
  testInit(c("sf", "terra"), opts = list("reproducible.overwrite" = TRUE))
  dPath <- checkPath(tempdir2(), create = TRUE)
  full <- sf::st_read(system.file("ex/lux.shp", package = "terra"), quiet = TRUE)[, c("NAME_2", "AREA")]
  zoneA <- full[3, ]
  zoneB <- full[8, ] # disjoint from zoneA
  targetFile <- "ledger.rds"

  dfA <- as.data.frame(zoneA); dfA$polygonID <- "A"; dfA$params <- I(list("p1"))
  dfB <- as.data.frame(zoneB); dfB$polygonID <- "B"; dfB$params <- I(list("pB"))
  saveRDS(rbind(dfA, dfB), file.path(dPath, targetFile))

  fun <- function(domain, id, param) {
    d <- as.data.frame(domain)
    d$polygonID <- id
    d$params <- I(list(param))
    d
  }

  CacheGeo(targetFile = targetFile, domain = zoneA,
           FUN = fun(domain, id = "A", param = "p2"),
           fun = fun, destinationPath = dPath, action = "update",
           useCache = FALSE, verbose = 0)

  ledger <- readRDS(file.path(dPath, targetFile))
  expect_equal(NROW(ledger), 2L)
  ledger <- ledger[order(ledger$polygonID), ]
  expect_equal(ledger$params[ledger$polygonID == "A"][[1]], "p2")
  expect_equal(ledger$params[ledger$polygonID == "B"][[1]], "pB")
})

test_that("CacheGeo action = 'nothing' never writes, even when FUN is supplied", {
  testInit(c("sf", "terra"), opts = list("reproducible.overwrite" = TRUE))
  dPath <- checkPath(tempdir2(), create = TRUE)
  full <- sf::st_read(system.file("ex/lux.shp", package = "terra"), quiet = TRUE)[, c("NAME_2", "AREA")]
  zoneA <- full[3, ]
  targetFile <- "ledger.rds"

  df <- as.data.frame(zoneA); df$polygonID <- "A"; df$params <- I(list("p1"))
  saveRDS(df, file.path(dPath, targetFile))
  before <- readRDS(file.path(dPath, targetFile))

  fun <- function(domain, id, param) {
    d <- as.data.frame(domain)
    d$polygonID <- id
    d$params <- I(list(param))
    d
  }

  out <- CacheGeo(targetFile = targetFile, domain = zoneA,
                   FUN = fun(domain, id = "A", param = "p2"),
                   fun = fun, destinationPath = dPath, action = "nothing",
                   useCache = FALSE, verbose = 0)

  after <- readRDS(file.path(dPath, targetFile))
  expect_identical(before, after)
  expect_equal(as.data.frame(out)$params[[1]], "p1")
})

test_that("CacheGeo update replaces by geometry equality when there is no polygonID column", {
  testInit(c("sf", "terra"), opts = list("reproducible.overwrite" = TRUE))
  dPath <- checkPath(tempdir2(), create = TRUE)
  full <- sf::st_read(system.file("ex/lux.shp", package = "terra"), quiet = TRUE)[, c("NAME_2", "AREA")]
  zoneA <- full[3, ]
  targetFile <- "ledger.rds"

  df <- as.data.frame(zoneA); df$params <- I(list("p1"))
  saveRDS(df, file.path(dPath, targetFile))

  fun <- function(domain, param) {
    d <- as.data.frame(domain)
    d$params <- I(list(param))
    d
  }

  CacheGeo(targetFile = targetFile, domain = zoneA,
           FUN = fun(domain, param = "p2"),
           fun = fun, destinationPath = dPath, action = "update",
           useCache = FALSE, verbose = 0)

  ledger <- readRDS(file.path(dPath, targetFile))
  expect_equal(NROW(ledger), 1L)
  expect_equal(ledger$params[[1]], "p2")
})

## Appending to a ledger that holds an xgboost model (fireSense_ignitionFit's fits) stopped with
## "ALTLIST classes must provide a Set_elt method": the append went through as.data.table(), whose
## copy() fails on a list-column holding an ALTREP object read back from an rds.
test_that("CacheGeo update appends to a ledger whose list-column holds an xgboost model", {
  skip_if_not_installed("xgboost")
  testInit(c("sf", "terra"), opts = list("reproducible.overwrite" = TRUE))
  local_cacheGeoReads()
  dPath <- checkPath(tempdir2(), create = TRUE)
  full <- sf::st_read(system.file("ex/lux.shp", package = "terra"), quiet = TRUE)[, c("NAME_2", "AREA")]
  x <- cbind(a = c(0, 1, 2, 3, 4, 5), b = c(1, 0, 1, 0, 1, 0))
  model <- xgboost::xgb.train(params = list(objective = "binary:logistic"),
                              data = xgboost::xgb.DMatrix(x, label = c(0, 1, 0, 1, 0, 1)), nrounds = 2)
  row <- function(zone, id) {
    d <- as.data.frame(zone)
    d$polygonID <- id
    d$fit <- I(list(list(model)))
    d
  }
  fun <- function(domain, id) row(domain, id)
  for (i in 1:2) {   # the second write finds the first row, read back from the rds
    id <- c("A", "B")[i]
    CacheGeo(targetFile = "ledger.rds", domain = full[i, ], FUN = fun(domain, id = id),
             fun = fun, row = row, id = id, destinationPath = dPath, action = "update",
             useCache = FALSE, verbose = 0)
  }
  ledger <- readRDS(file.path(dPath, "ledger.rds"))
  expect_identical(as.character(ledger$polygonID), c("A", "B"))
  expect_length(predict(ledger$fit[[2]][[1]], x), 6L)
})

## The legacy call shapes of the FireSense modules (fireSense_spreadFit, fireSense_ignitionFit,
## fireSense_dataPrepFit, fireSense_ELFs), local only. The objects `FUN` needs go in `...`; one
## named `le` was partially matched to the new `ledger` argument, so CacheGeo() took the call for
## the new API and stopped with "`area` is required".
test_that("legacy CacheGeo: objects for FUN in ... are not taken by ledger/area/compute/match/tolerance", {
  testInit(c("sf", "terra"), opts = list("reproducible.overwrite" = TRUE))
  dPath <- checkPath(tempdir2(), create = TRUE)
  full <- sf::st_read(system.file("ex/lux.shp", package = "terra"), quiet = TRUE)[, c("NAME_2", "AREA")]
  zoneA <- sf::st_as_sf(sf::st_transform(sf::st_geometry(full[3, ]), 32632)) # bufferOK needs a projected CRS
  sf::st_geometry(zoneA) <- "geometry"
  targetFile <- "ledger.rds"
  rowFor <- function(param) {
    r <- zoneA
    r$polygonID <- "A"
    r$params <- I(list(param))
    r
  }

  ## fireSense_ignitionFit: a wrapper passes `domain`, `FUN`, its objects and `action` in `...`
  ledgerCall <- function(...) {
    CacheGeo(cloudFolderID = NULL, targetFile = targetFile, destinationPath = dPath,
             purge = FALSE, ...)
  }
  le <- function(x) x
  out <- ledgerCall(domain = zoneA, FUN = le(ledgerRow), le = le, ledgerRow = rowFor("p1"),
                    action = "update", verbose = 0)
  expect_equal(out$params[[1]], "p1")
  expect_equal(NROW(readRDS(file.path(dPath, targetFile))), 1L)

  ## fireSense_spreadFit: called directly, `purge = 7` with no cloud folder; a refit replaces the row
  out <- CacheGeo(cloudFolderID = NULL, targetFile = targetFile, domain = zoneA,
                  destinationPath = dPath, FUN = le(studyAreaFireSense), le = le, purge = 7,
                  studyAreaFireSense = rowFor("p2"), action = "update", verbose = 0)
  expect_equal(out$params[[1]], "p2")
  ledger <- readRDS(file.path(dPath, targetFile))
  expect_equal(NROW(ledger), 1L)
  expect_equal(ledger$params[[1]], "p2")

  ## objects whose names are prefixes of each of the arguments before `...`
  out <- CacheGeo(targetFile = targetFile, domain = zoneA, destinationPath = dPath,
                  FUN = co(ar(ma(to(rowFor("p3"))))), co = identity, ar = identity,
                  ma = identity, to = identity, rowFor = rowFor, action = "update", verbose = 0)
  expect_equal(out$params[[1]], "p3")

  ## fireSense_dataPrepFit, fireSense_ELFs and the ignitionFit read: domain, action = "nothing"
  for (purge in list(FALSE, 7)) {
    got <- CacheGeo(cloudFolderID = NULL, targetFile = targetFile, purge = purge,
                    domain = zoneA, action = "nothing", useCache = FALSE,
                    destinationPath = dPath, bufferOK = TRUE, verbose = 0)
    expect_equal(got$polygonID, "A")
    expect_equal(got$params[[1]], "p3")
  }

  ## a read with no domain returns the whole ledger
  expect_message(
    got <- CacheGeo(cloudFolderID = NULL, targetFile = targetFile, destinationPath = dPath,
                    action = "nothing", useCache = FALSE, verbose = 0),
    "Spatial domain is missing")
  expect_equal(NROW(got), 1L)
  expect_equal(NROW(readRDS(file.path(dPath, targetFile))), 1L)
})

## The rows CacheGeo() returns after a write hold the objects FUN returned, not copies. A copied
## xgboost model is a new booster (another `ptr`), so it was equal but no longer identical
## (fireSense_ignitionFit compares the ledger's model with the live one).
test_that("legacy CacheGeo update returns FUN's xgboost model itself, not a copy", {
  skip_if_not_installed("xgboost")
  testInit(c("sf", "terra"), opts = list("reproducible.overwrite" = TRUE))
  local_cacheGeoReads()
  dPath <- checkPath(tempdir2(), create = TRUE)
  x <- cbind(a = c(0, 1, 2, 3, 4, 5), b = c(1, 0, 1, 0, 1, 0))
  model <- xgboost::xgb.train(params = list(objective = "binary:logistic", nthread = 1),
                              data = xgboost::xgb.DMatrix(x, label = c(0, 1, 0, 1, 0, 1)), nrounds = 2)
  sq <- function(id, x0) {
    box <- sf::st_polygon(list(rbind(c(x0, 0), c(x0 + 10, 0), c(x0 + 10, 10), c(x0, 10), c(x0, 0))))
    out <- sf::st_sf(polygonID = id, geometry = sf::st_sfc(box, crs = 32618))
    out$fit <- I(list(list(model = model)))
    out
  }
  le <- function(x) x
  for (id in c("A", "B")) { # a new ledger, then a second row added to it
    row <- sq(id, if (id == "A") 0 else 10)
    out <- CacheGeo(targetFile = "boost.rds", destinationPath = dPath, domain = row,
                    FUN = le(ledgerRow), le = le, ledgerRow = row, action = "update", verbose = 0)
    ## B touches A, so the rows returned are A and B, as the old CacheGeo() returned them
    expect_identical(out$fit[[which(out$polygonID == id)]]$model, model)
  }
})

## fireSense_dataPrepFit reads the spread-fit ledger with the old arguments and `bufferOK = TRUE`.
## That became a `tolerance` of 2.5% of the ledger's extent (86 km on the FireSense ledger), and
## the study area's own row, narrower than twice that, was dropped as a sliver: 0 rows, where the
## old CacheGeo() returned the row and its touching neighbour (ELF 13.1, 2026-10-08).
test_that("legacy CacheGeo read returns the rows the old CacheGeo returned, for a narrow polygon in a wide ledger", {
  testInit("sf", opts = list("reproducible.overwrite" = TRUE))
  dPath <- checkPath(tempdir2(), create = TRUE)
  box <- function(id, x0, x1, y0 = 0, y1 = 100) {
    sf::st_sf(polygonID = id, params = I(list(id)),
              geometry = sf::st_sfc(sf::st_polygon(list(1000 * rbind(c(x0, y0), c(x1, y0), c(x1, y1),
                                                                     c(x0, y1), c(x0, y0)))),
                                    crs = 32618))
  }
  ## A is 10 km wide; B touches it; C, far away, makes the ledger 1100 km wide
  saveRDS(as.data.frame(rbind(box("A", 0, 10), box("B", 10, 20), box("C", 1000, 1100))),
          file.path(dPath, "ledger.rds"))
  read <- function(domain, bufferOK)
    CacheGeo(cloudFolderID = NULL, targetFile = "ledger.rds", purge = 7, domain = domain,
             action = "nothing", useCache = FALSE, destinationPath = dPath, bufferOK = bufferOK,
             verbose = 0)
  areaA <- box("A", 0, 10)["polygonID"]
  for (bufferOK in c(TRUE, FALSE))
    expect_identical(sort(read(areaA, bufferOK)$polygonID), c("A", "B"))
  ## 5 km wider than A on the left: covered only with bufferOK's 10 km buffer
  wider <- box("A", -5, 10)["polygonID"]
  expect_identical(sort(read(wider, TRUE)$polygonID), c("A", "B"))
  expect_identical(NROW(read(wider, FALSE)), 0L)
})
