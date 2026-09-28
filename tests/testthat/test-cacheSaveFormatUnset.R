## reproducible.cacheSaveFormat defaults to NULL (unset), not "rds" -- see R/options.R.
## A session that never set the option must be indistinguishable from one that
## can't tell "rds" was ever chosen, so reading an existing qs2 entry (via
## loadFromCache() or a Cache() hit) must never convert it to rds.
## CacheStoredFile()/CacheDBFileSingle() treat NULL as "look on disk, whatever
## is there" (R/DBI.R, the `cacheSaveFormat <- "check"` translation).
##
## Both backends are exercised explicitly (useDBI(TRUE)/useDBI(FALSE)): CI does
## not vary reproducible.useDBI, so a single-backend test would leave the other
## path (the non-DBI, file-based metadata backend) unchecked.

test_that("unset cacheSaveFormat reads an existing qs2 entry without converting it", {
  skip_if_not_installed("qs2")
  testInit()

  origUseDBI <- useDBI()
  on.exit(useDBI(origUseDBI), add = TRUE)

  for (useDBIVal in c(TRUE, FALSE)) {
    if (useDBIVal && !requireNamespace("RSQLite", quietly = TRUE)) next
    useDBI(useDBIVal)
    lbl <- paste("useDBI =", useDBIVal)

    cp <- checkPath(file.path(tmpdir, paste0("unsetFmt-", useDBIVal)), create = TRUE)

    withr::local_options(reproducible.cacheSaveFormat = "qs2")
    runs <- 0L
    expensive <- function(x) { runs <<- runs + 1L; x + 1 }
    first <- Cache(expensive, x = 1, cachePath = cp, verbose = 0)
    cacheId <- gsub("cacheId:", "", attr(first, "tags"))

    qs2Before <- dir(CacheStorageDir(cp), pattern = "\\.qs2$")
    expect_true(length(qs2Before) >= 1, label = lbl)

    withr::local_options(reproducible.cacheSaveFormat = NULL)

    ## loadFromCache() must not convert the file
    loaded <- loadFromCache(cp, cacheId = cacheId)
    expect_identical(as.numeric(loaded), 2, label = lbl)
    expect_identical(dir(CacheStorageDir(cp), pattern = "\\.qs2$"), qs2Before, label = lbl)
    expect_length(dir(CacheStorageDir(cp), pattern = "\\.rds$"), 0L)

    ## a Cache() hit must not convert the file either
    second <- Cache(expensive, x = 1, cachePath = cp, verbose = 0)
    expect_false(attr(second, ".Cache")$newCache, label = lbl)
    expect_identical(runs, 1L, label = lbl)
    expect_identical(dir(CacheStorageDir(cp), pattern = "\\.qs2$"), qs2Before, label = lbl)
    expect_length(dir(CacheStorageDir(cp), pattern = "\\.rds$"), 0L)
  }
})

test_that("cacheSaveFormat explicitly set to rds still converts an existing qs2 entry", {
  skip_if_not_installed("qs2")
  testInit()

  origUseDBI <- useDBI()
  on.exit(useDBI(origUseDBI), add = TRUE)

  for (useDBIVal in c(TRUE, FALSE)) {
    if (useDBIVal && !requireNamespace("RSQLite", quietly = TRUE)) next
    useDBI(useDBIVal)
    lbl <- paste("useDBI =", useDBIVal)

    cp <- checkPath(file.path(tmpdir, paste0("explicitSwap-", useDBIVal)), create = TRUE)

    withr::local_options(reproducible.cacheSaveFormat = "qs2")
    runs <- 0L
    expensive <- function(x) { runs <<- runs + 1L; x + 1 }
    Cache(expensive, x = 1, cachePath = cp, verbose = 0)

    withr::local_options(reproducible.cacheSaveFormat = "rds")
    second <- Cache(expensive, x = 1, cachePath = cp, verbose = 0)

    expect_identical(runs, 1L, label = lbl) # cache hit, not recomputed
    expect_identical(as.numeric(second), 2, label = lbl)
    expect_length(dir(CacheStorageDir(cp), pattern = "\\.qs2$"), 0L)
    expect_true(length(dir(CacheStorageDir(cp), pattern = "\\.rds$")) >= 1, label = lbl)
  }
})

test_that("unset cacheSaveFormat saves a new entry as rds", {
  testInit()

  origUseDBI <- useDBI()
  on.exit(useDBI(origUseDBI), add = TRUE)

  for (useDBIVal in c(TRUE, FALSE)) {
    if (useDBIVal && !requireNamespace("RSQLite", quietly = TRUE)) next
    useDBI(useDBIVal)
    lbl <- paste("useDBI =", useDBIVal)

    cp <- checkPath(file.path(tmpdir, paste0("newRds-", useDBIVal)), create = TRUE)
    withr::local_options(reproducible.cacheSaveFormat = NULL)

    addOne <- function(x) x + 1
    out <- Cache(addOne, x = 1, cachePath = cp, verbose = 0)
    expect_identical(as.numeric(out), 2, label = lbl)

    rdsFiles <- dir(CacheStorageDir(cp), pattern = "\\.rds$")
    qs2Files <- dir(CacheStorageDir(cp), pattern = "\\.qs2$")
    expect_true(length(rdsFiles) >= 1, label = lbl)
    expect_length(qs2Files, 0L)
  }
})

test_that("unset cacheSaveFormat keeps a qs2 entry's tag file in qs2 on a Cache() hit", {
  ## A hit appends an "accessed" tag. With the option unset, the tag file path
  ## resolved to .dbFile.qs2 but was written with saveRDS(), so the next hit
  ## could not read it (and fell through to asking for the qs package).
  skip_if_not_installed("qs2")
  testInit()

  origUseDBI <- useDBI()
  on.exit(useDBI(origUseDBI), add = TRUE)
  useDBI(FALSE) # the tag file only exists on the file-based backend

  cp <- checkPath(file.path(tmpdir, "unsetFmtTags"), create = TRUE)
  withr::local_options(reproducible.cacheSaveFormat = "qs2")
  runs <- 0L
  expensive <- function(x) { runs <<- runs + 1L; x + 1 }
  Cache(expensive, x = 1, cachePath = cp, verbose = 0)

  withr::local_options(reproducible.cacheSaveFormat = NULL)
  Cache(expensive, x = 1, cachePath = cp, verbose = 0)

  tagFile <- dir(CacheStorageDir(cp), pattern = "\\.dbFile\\.qs2$", full.names = TRUE)
  expect_length(tagFile, 1L)
  expect_no_error(qs2::qs_read(tagFile))

  third <- Cache(expensive, x = 1, cachePath = cp, verbose = 0)
  expect_identical(as.numeric(third), 2)
  expect_identical(runs, 1L)
})

test_that("a qs2-named tag file holding rds is read and rewritten as qs2", {
  ## 3.2.1.9046-9047 left such files behind (see the test above); the next hit
  ## must read them and save them back as qs2, without converting the entry.
  skip_if_not_installed("qs2")
  testInit()

  origUseDBI <- useDBI()
  on.exit(useDBI(origUseDBI), add = TRUE)
  useDBI(FALSE)

  cp <- checkPath(file.path(tmpdir, "rdsInQs2Tags"), create = TRUE)
  withr::local_options(reproducible.cacheSaveFormat = "qs2")
  runs <- 0L
  expensive <- function(x) { runs <<- runs + 1L; x + 1 }
  Cache(expensive, x = 1, cachePath = cp, verbose = 0)

  tagFile <- dir(CacheStorageDir(cp), pattern = "\\.dbFile\\.qs2$", full.names = TRUE)
  saveRDS(qs2::qs_read(tagFile), tagFile) # damage it as 3.2.1.9047 did
  expect_error(qs2::qs_read(tagFile))

  withr::local_options(reproducible.cacheSaveFormat = NULL)
  out <- Cache(expensive, x = 1, cachePath = cp, verbose = 0)
  expect_identical(as.numeric(out), 2)
  expect_identical(runs, 1L)
  expect_no_error(qs2::qs_read(tagFile))
  expect_length(dir(CacheStorageDir(cp), pattern = "\\.rds$"), 0L)
})
