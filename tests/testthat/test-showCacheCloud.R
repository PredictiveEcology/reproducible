## Google Drive round-trip for showCacheCloud() (R/cloud.R) and the useCloud
## branch of showSimilar() (R/GPT2.R).
##
## Companion to test-mergeShownCacheCloud.R, which covers the pure data.table
## fold with no credentials. This file covers the half that cannot run without
## them: listing the per-cacheId `.dbFile.*` metadata off Drive and folding it
## into a local showCache() result.
##
## The scenario is the one the code exists for. Two cachePaths share one
## cloudFolderID, standing in for two machines: A computes and uploads, B has
## an empty local cache. Without the cloud lookup, B's showSimilar() reports
## "no similar item" even though a near match exists remotely.
##
## Objects are scalar numerics and the cache holds a single entry, so this is a
## handful of tiny Drive calls. skip_on_cran() plus the needGoogleDriveAuth
## gate in testInit() keep it off CRAN and off any machine without credentials.

test_that("showSimilar finds a cacheId that only exists in the cloud", {
  skip_on_cran()          ## uploads
  testInit("googledrive", needGoogleDriveAuth = TRUE)

  ## showSimilar reads showCache(); the pre-warm fork is irrelevant here and is
  ## disabled under covr anyway.
  withr::local_options(reproducible.showCachePreWarm = FALSE, reproducible.ask = FALSE)

  cloudFolder <- retry(quote(googledrive::drive_mkdir(
    name = paste0("showCacheCloud", rndstr(1, 6)), path = .cloudTestRoot())))
  on.exit(try(googledrive::drive_rm(cloudFolder), silent = TRUE), add = TRUE)

  ## Two independent local caches, one shared cloud folder.
  cpA <- checkPath(file.path(tmpdir, "machineA"), create = TRUE)
  cpB <- checkPath(file.path(tmpdir, "machineB"), create = TRUE)

  fn <- function(a, b = 1) a + b

  ## --- machine A: compute and upload ---------------------------------------
  invisible(Cache(fn, a = 1, cachePath = cpA, useCloud = TRUE,
                  cloudFolderID = cloudFolder, verbose = 0))

  ## --- machine B: the cloud metadata is visible even though nothing is local
  expect_identical(NROW(showCache(cpB, verbose = -2)), 0L)

  cloudShown <- showCacheCloud(cloudFolder, cpB, verbose = 0)

  ## A's cacheId comes back, with the tags showSimilar needs to diff against.
  expect_true(NROW(cloudShown) > 0)
  expect_true("function" %in% cloudShown$tagKey)

  ## existingCacheIds is the "I already have this locally" filter -- the whole
  ## point of passing it is to avoid re-downloading metadata already in hand.
  expect_identical(
    NROW(showCacheCloud(cloudFolder, cpB, existingCacheIds = unique(cloudShown$cacheId),
                        verbose = 0)),
    0L
  )

  ## --- machine B: a DIFFERENT call, so showSimilar has to explain the miss --
  mess <- capture_messages(
    res <- Cache(fn, a = 2, cachePath = cpB, useCloud = TRUE,
                 cloudFolderID = cloudFolder, showSimilar = TRUE, verbose = 1)
  )

  ## The result is still correct -- showSimilar is diagnostics, not control flow.
  ## Compared as a bare value: Cache() attaches its tags as attributes, so the
  ## returned object is deliberately not identical() to a plain numeric.
  expect_identical(as.numeric(res), 3)

  ## And it found the remote entry rather than reporting nothing to compare to.
  expect_false(any(grepl("no similar item", mess)))
})

test_that("showCacheCloud returns an empty table for a cloud folder with no cache", {
  skip_on_cran()
  testInit("googledrive", needGoogleDriveAuth = TRUE)

  ## An empty (or never-written) cloudFolderID must yield an empty table rather
  ## than an error: showSimilar calls this on the first ever useCloud run, when
  ## the folder necessarily holds nothing.
  emptyFolder <- retry(quote(googledrive::drive_mkdir(
    name = paste0("showCacheCloudEmpty", rndstr(1, 6)), path = .cloudTestRoot())))
  on.exit(try(googledrive::drive_rm(emptyFolder), silent = TRUE), add = TRUE)

  out <- showCacheCloud(emptyFolder, checkPath(file.path(tmpdir, "c"), create = TRUE),
                        verbose = 0)

  expect_identical(NROW(out), 0L)
  expect_true(all(.dtFileMainCols %in% names(out)))
})
