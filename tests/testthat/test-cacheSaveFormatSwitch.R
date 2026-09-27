## Changing options(reproducible.cacheSaveFormat) must NOT invalidate the cache.
##
## The entry is migrated to the new format (loadFromCacheSwitchFormat ->
## swapCacheFileFormat) rather than recomputed. This regressed on the file-backed
## backend and went unnoticed for a telling reason, which shapes these tests:
##
##   `reproducible.cachePath` MUST BE UNSET here.
##
## onlyStorageFiles() built its match pattern via CacheStoredFile() without a
## cachePath. With the option SET it resolved fine; with it unset -- the normal
## state when a caller passes cachePath= straight to Cache() -- CacheStoredFile()
## returned character(0), the pattern became the literal
## "character(0)|character(0)|character(0)", and checkSameCacheId() matched
## nothing. The changed-format recovery then never fired and every entry
## recomputed. Because testInit() sets reproducible.cachePath, a test written the
## usual way passes against the broken code and proves nothing.
##
## The assertion is on EVALUATION COUNT, not the returned value: the value is
## correct either way, which is exactly what made the bug silent.
##
## No network, no Drive.

## Cache `x + 1` under `from`, then request it under `to`, and report how many
## times the body actually ran. 1 = cache worked; 2 = silently recomputed.
runsAcrossFormatSwitch <- function(cachePath, from, to) {
  runs <- 0L
  expensive <- function(x) {
    runs <<- runs + 1L
    x + 1
  }
  withr::local_options(reproducible.cacheSaveFormat = from)
  first <- Cache(expensive, x = 1, cachePath = cachePath, verbose = 0)
  withr::local_options(reproducible.cacheSaveFormat = to)
  second <- Cache(expensive, x = 1, cachePath = cachePath, verbose = 0)
  list(runs = runs, first = as.numeric(first), second = as.numeric(second))
}

test_that("changing cacheSaveFormat recovers from cache rather than recomputing", {
  skip_if_not_installed("qs2")
  testInit()

  for (useDBI in c(FALSE, TRUE)) {
    if (useDBI && !requireNamespace("RSQLite", quietly = TRUE)) next
    for (fmts in list(c("rds", "qs2"), c("qs2", "rds"))) {
      ## cachePath deliberately unset -- see the file header. This is the
      ## condition under which the bug appears at all.
      withr::local_options(reproducible.cachePath = NULL,
                           reproducible.useDBI = useDBI,
                           reproducible.showCachePreWarm = FALSE,
                           reproducible.ask = FALSE)
      if (useDBI && !useDBI()) next

      cp <- checkPath(file.path(tmpdir, paste0("fmt-", useDBI, "-", paste(fmts, collapse = ""))),
                      create = TRUE)
      res <- runsAcrossFormatSwitch(cp, fmts[1], fmts[2])

      lbl <- paste0("useDBI=", useDBI, " ", fmts[1], "->", fmts[2])
      ## THE assertion: the body ran once, not twice.
      expect_identical(res$runs, 1L, label = paste(lbl, "evaluations"))
      ## The value was always right -- assert it so a "fix" that breaks
      ## correctness to satisfy the count cannot pass.
      expect_identical(res$second, 2, label = paste(lbl, "value"))
    }
  }
})

test_that("the migrated entry leaves exactly one object file, in the new format", {
  skip_if_not_installed("qs2")
  testInit()

  withr::local_options(reproducible.cachePath = NULL,
                       reproducible.useDBI = FALSE,
                       reproducible.showCachePreWarm = FALSE,
                       reproducible.ask = FALSE)
  cp <- checkPath(file.path(tmpdir, "migrate"), create = TRUE)
  invisible(runsAcrossFormatSwitch(cp, "rds", "qs2"))

  ## Migration, not accumulation: the old .rds must be gone. Previously BOTH
  ## formats were left on disk, the orphan never collected.
  objFiles <- grep("dbFile|lock", dir(CacheStorageDir(cp)), invert = TRUE, value = TRUE)
  expect_length(objFiles, 1L)
  expect_match(objFiles, "\\.qs2$")
})

test_that("loadFromCache() migrating an entry keeps its original tags", {
  ## loadFromCacheSwitchFormat() (R/DBI.R) calls swapCacheFileFormat() without
  ## passing the entry's userTags. swapCacheFileFormat() re-saves the entry via
  ## saveToCache(obj, ..., userTags = userTags), and saveToCache() treats a
  ## missing userTags as "otherFunctions" (R/DBI.R, saveToCache()), then the old
  ## entry (and its tag file) is removed via rmFromCache(). Every other tag --
  ## function, preDigest, accessed, elapsedTime, cacheChaining_*, and any
  ## userTags the caller set -- is lost. Run under both cache backends: CI does
  ## not vary reproducible.useDBI, so this loops over both itself (pattern from
  ## "test useDBI TRUE <--> FALSE" in test-cache.R).
  skip_if_not_installed("qs2")
  testInit()

  origUseDBI <- useDBI()
  on.exit(useDBI(origUseDBI), add = TRUE)

  for (useDBIVal in c(FALSE, TRUE)) {
    if (useDBIVal && !requireNamespace("RSQLite", quietly = TRUE)) next
    useDBI(useDBIVal, verbose = -1)
    if (isTRUE(useDBIVal) && !isTRUE(useDBI())) next ## RSQLite/DBI unavailable

    withr::local_options(reproducible.cachePath = NULL,
                         reproducible.showCachePreWarm = FALSE,
                         reproducible.ask = FALSE,
                         reproducible.cacheSaveFormat = "rds")
    cp <- checkPath(file.path(tmpdir, paste0("tagsPreserved-", useDBIVal)), create = TRUE)

    expensive <- function(x) x + 1
    Cache(expensive, x = 1, cachePath = cp, verbose = 0, userTags = "myUserTag:hello")
    cacheId <- showCache(cp)$cacheId[1]
    tagsBefore <- sort(unique(showCache(cp, cacheId = cacheId)$tagKey))

    ## Directly exercise loadFromCache() with a different cacheSaveFormat than
    ## the entry was saved in, so it goes through loadFromCacheSwitchFormat().
    loadFromCache(cachePath = cp, cacheId = cacheId, preDigest = list(),
                 cacheSaveFormat = "qs2", verbose = 0)

    tagsAfter <- sort(unique(showCache(cp, cacheId = cacheId)$tagKey))

    lbl <- paste0("useDBI=", useDBIVal)
    expect_true("myUserTag" %in% tagsAfter, label = paste(lbl, "myUserTag"))
    expect_true("function" %in% tagsAfter, label = paste(lbl, "function"))
    expect_setequal(tagsAfter, tagsBefore)
  }
})

test_that("onlyStorageFiles keeps object files and drops metadata/lock files", {
  testInit()

  ## The unit underneath the above. It must pick the storage file out of a
  ## listing that also holds the per-cacheId metadata and lock files -- and it
  ## must do so without depending on reproducible.cachePath.
  withr::local_options(reproducible.cachePath = NULL)
  cp <- checkPath(file.path(tmpdir, "osf"), create = TRUE)
  cid <- "abc123"
  files <- c(paste0(cid, ".dbFile.rds"), paste0(cid, ".lock"), paste0(cid, ".rds"))

  kept <- onlyStorageFiles(files, cid, cachePath = cp)

  expect_identical(kept, paste0(cid, ".rds"))
})
