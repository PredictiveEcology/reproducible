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
