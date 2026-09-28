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
