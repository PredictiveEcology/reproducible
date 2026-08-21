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
