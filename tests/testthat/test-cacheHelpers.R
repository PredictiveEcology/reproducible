test_that("test miscellaneous unit tests cache-helpers", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  testInit(opts = list(reproducible.useMemoise = TRUE))

  a <- 1
  mess <- capture_message(.cacheMessage(a, "test", TRUE))
  expect_true(any(grepl(.message$LoadedCacheResult("Memoised"), mess)))

  mess <- capture_message(.cacheMessage(a, "test", FALSE))
  ## TODO: what was the old expected behaviour here? message now includes "added memoised copy"
  # expect_false(any(grepl(paste0(.message$LoadedCacheResult(), ".*added"), mess)))

  mess <- capture_message(.cacheMessage(a, "test", NA))
  expect_true(any(grepl(.message$LoadedCacheResult(), mess)))
  expect_false(all(grepl("adding", mess)))

  # studyAreaName with sf and sfc
  if (requireNamespace("sf", quietly = TRUE)) {
    pol <- sf::st_sfc(sf::st_polygon(list(cbind(c(0, 3, 3, 0, 0), c(0, 0, 3, 3, 0)))))
    h <- sf::st_sf(r = 5, pol)
    expect_true(is(studyAreaName(pol), "character"))
    expect_true(is(studyAreaName(h), "character"))
  }

  # studyAreaName with SpatVector
  if (requireNamespace("terra", quietly = TRUE)) {
    v <- terra::vect(system.file("ex/lux.shp", package = "terra"))
    expect_true(is(studyAreaName(v), "character"))
  }

  # studyAreaName with SPDF/SP
  # coords <- structure(c(-122.98, -116.1, -99.2, -106, -122.98, 59.9, 65.73, 63.58, 54.79, 59.9),
  #                     .Dim = c(5L, 2L))
  # Sr1 <- Polygon(coords)
  # Srs1 <- Polygons(list(Sr1), "s1")
  # StudyArea <- SpatialPolygons(list(Srs1), 1L)

  coords <- structure(c(-122.98, -116.1, -99.2, -106, -122.98, 59.9, 65.73, 63.58, 54.79, 59.9),
    .Dim = c(5L, 2L)
  )
  StudyArea <- terra::vect(coords, "polygons")
  terra::crs(StudyArea) <- crsToUse

  df <- data.frame(a = 1, row.names = row.names(StudyArea))

  # SPDF <- SpatialPolygonsDataFrame(StudyArea, df, match.ID = TRUE)
  expect_true(is(studyAreaName(StudyArea), "character"))
  # expect_true(is(studyAreaName(SPDF), "character"))

  # studyAreaName with random object
  expect_error(studyAreaName(integer(0)))

  # .checkCacheRepo
  withr::local_options(reproducible.cachePath = .reproducibleTempCacheDir())
  mess <- capture_message(.checkCacheRepo(a))
  expect_true(any(grepl(.message$NoCacheRepoSuppliedGrep, mess)))

  withr::local_options(reproducible.cachePath = NULL,
                       "rasterTmpDir" = tempdir2(rndstr(1, 6)),
                       "reproducible.inputPaths" = NULL,
                       "reproducible.overwrite" = TRUE)
  mess <- capture_message(.checkCacheRepo(a))
  expect_true(any(grepl(paste0(.message$NoCachePathSupplied, ". Using"), mess)))

  ## nextNumericName
  b <- nextNumericName("test.pdf")
  b1 <- nextNumericName(b)

  ## expect_true(grepl("_2.pdf", b1)) ## TODO: this number is not consistently 2 or 3
  aMess <- capture_messages({
    a <- Cache(rnorm, 1, useCache = FALSE, cachePath = tmpCache)
  })
  bMess <- capture_messages({
    b <- Cache(rnorm, 1, useCache = FALSE, cachePath = tmpCache)
  })
  expect_false(identical(a, b))
  expect_true(grepl("skipping Cache", aMess))
  expect_true(grepl("skipping Cache", bMess))

  ## getOption("reproducible.useMemoise" = FALSE)
  opt22 <- options("reproducible.useMemoise" = FALSE)
  aMess <- capture_messages({
    a <- Cache(rnorm, 1, cachePath = tmpCache)
  })
  bMess <- capture_messages({
    a <- Cache(rnorm, 1, cachePath = tmpCache)
  })
  options(opt22)
  cMess <- capture_messages({
    a <- Cache(rnorm, 1, cachePath = tmpCache)
  })
  dMess <- capture_messages({
    a <- Cache(rnorm, 1, cachePath = tmpCache)
  })
  # expect_true(identical(aMess, bMess[1]))
  expect_false(any(grepl(.message$LoadedCacheResult("Memoised"), bMess)))
  expect_true(any(grepl(.message$LoadedCacheResult("Memoised"), dMess)))

  ## showSimilar
  try(clearCache(ask = FALSE, x = tmpCache), silent = TRUE)
  aMess <- capture_messages({
    a <- Cache(rnorm, n = 1, mean = 1, cachePath = tmpCache)
  })
  # lapply(letters[11], function(l) assign(paste(rep(l, 4), collapse = ""), 1, envir = .GlobalEnv))
  oo <- capture.output(
    bMess <- capture_messages({
      b <- Cache(rnorm, n = 2, mean = 1, sd = 3, showSimilar = TRUE, cachePath = tmpCache)
    }))

  if (!getOption("reproducible.useCacheV3") %in% TRUE) {
    expect_true(any(grepl("different n", bMess)))
    expect_true(any(grepl("different .+sd", bMess)))
    # expect_true(any(grepl("new argument.*sd", bMess)))
    expect_true(any(grepl("next closest cacheId", bMess)))
    cMess <- capture_messages({
      b <- Cache(rnorm, n = 3, mean = 1, sd = 3, showSimilar = TRUE, cachePath = tmpCache)
    })
    expect_true(any(grepl("different n", cMess)))
    expect_false(any(grepl("new argument.*sd", cMess)))
    cMessCacheId <- gsub(".*cacheId (.*)\x1b\\[.*", "\\1", grep("cacheId", cMess, value = TRUE))
    bMessCacheId <- gsub(".*cacheId (.*)\x1b\\[.*", "\\1", grep("cacheId", bMess, value = TRUE))
    expect_false(identical(cMessCacheId, bMessCacheId))

    dMess <- capture_messages({
      b <- Cache(rnorm, n = 4, mean = 1, sd = 4, showSimilar = TRUE, cachePath = tmpCache)
    })

    # There are 2 ways this may come out -- similarity to 1 of 2 alternatives above
    expect1 <- any(grepl("different n, sd", dMess))
    expect2 <- any(grepl("different n", dMess)) && any(grepl("new argument.*sd", dMess))
    expect_true(expect1 || (expect2))
    dMessCacheId <- gsub(".*cacheId (.*)\x1b\\[.*", "\\1", grep("cacheId", dMess, value = TRUE))
    bMessCacheId <- gsub(".*cacheId (.*)\x1b\\[.*", "\\1", grep("cacheId", bMess, value = TRUE))
    if (expect1) {
      expect_false(identical(dMessCacheId, bMessCacheId))
    } else {
      expect_true(identical(dMessCacheId, bMessCacheId))
    }
  }

  rcompletelynew <- rmultinom
  # Now check function is prefered over args
  clearCache(tmpCache, ask = FALSE)
  eMess <- capture_messages({
    b <- Cache(rbinom, 4, 5, prob = 0.6, showSimilar = TRUE, cachePath = tmpCache)
  })
  fMess <- capture_messages({
    b <- Cache(rmultinom, 4, 5, prob = 0.6, showSimilar = TRUE, cachePath = tmpCache)
  })
  gMess <- capture_messages({
    b <- Cache(rmultinom, 14, 15, prob = 0.8, showSimilar = TRUE, cachePath = tmpCache)
  }) |> capture.output() -> oo
  hMess <- capture_messages({
    b <- Cache(rbinom, 14, 15, prob = 0.8, showSimilar = TRUE, cachePath = tmpCache)
  }) |> capture.output() -> oo
  iMess <- capture_messages({
    b <- Cache(rcompletelynew, 12, 15, prob = 0.8, showSimilar = TRUE, cachePath = tmpCache)
  })
  expect_true(any(grepl("no similar item", eMess))) # shouldn't find b/c new
  expect_true(any(grepl("no similar item", fMess))) # shouldn't find b/c args are same
  if (!getOption("reproducible.useCacheV3") %in% TRUE) {
    expect_true(any(grepl("next closest.+rmultin", gMess))) # should only find rmultinom
    expect_true(any(grepl("next closest.+rbinom", hMess))) # should only find rbinom
    expect_true(sum(grepl(".+rcompletelynew|next closest.+rmultin", iMess)) == 3) # should notice different name, but still find
  }

  ### UserTags matching -- prefer similar if all userTags match
  rcompletelynew <- rnorm
  # Now check function is prefered over args
  clearCache(tmpCache, ask = FALSE)
  jMess <- capture_messages({
    bj <- Cache(rnorm, 1, 2, 3, showSimilar = TRUE, cachePath = tmpCache, userTags = c("Hi"))
  })
  expect_true(any(grepl("no similar item", jMess))) # shouldn't find b/c new

  kMess <- capture_messages({
    bk <- Cache(rnorm, 1, 3, 4, showSimilar = TRUE, cachePath = tmpCache, userTags = c("By")) # not similar
  })

  expect_false(any(grepl("no similar item", kMess))) # should find bj as similar (args differ: 1,2,3 vs 1,3,4)

  lMess <- capture_messages({
    bl <- Cache(rnorm, 1, 3, 4, showSimilar = TRUE, cachePath = tmpCache, userTags = c("Hi")) # same, recovered
  })
  expect_true(any(grepl("Loaded", lMess))) # should only find rmultinom

  mMess <- capture_messages({
    bm <- Cache(rnorm, 1, 2, 3, showSimilar = TRUE, cachePath = tmpCache, userTags = c("By")) # same recovered
  })
  expect_true(any(grepl("Loaded", mMess))) # should only find rmultinom

  nMess <- capture_messages({
    bn <- Cache(rnorm, 1, 2, 2, showSimilar = TRUE, cachePath = tmpCache, userTags = c("By")) # similar to kMess
  }) |> capture.output() -> oo
  if (!getOption("reproducible.useCacheV3") %in% TRUE) {
    nMess <- grep("^.+next closest cacheId\\(s\\) (.+) of .+$", nMess, value = TRUE)
    expect_true(grepl(
      x = attr(bm, "tags"),
      gsub("^.+next closest cacheId\\(s\\) (.+) of .+$", "\\1", nMess)
    )) ## find mMess (jMess) because it's the most recent

    oMess <- capture_messages({
      bo <- Cache(rnorm, 1, 2, 1, showSimilar = TRUE, cachePath = tmpCache) # similar to kMess
    })
    oMess <- grep("^.+next closest cacheId\\(s\\) (.+) of .+$", oMess, value = TRUE)
    expect_true(grepl(
      x = attr(bn, "tags"),
      gsub("^.+next closest cacheId\\(s\\) (.+) of .+$", "\\1", oMess) ## TODO: fix failing test
    )) ## find nMess (jMess) because it's the most recent
  }
})

test_that("test debugCache arg", {
  testInit(opts = list(reproducible.useMemoise = TRUE))

  ## debugCache -- "complete"
  thing <- 1
  aa <- Cache(rnorm, thing, debugCache = "complete", cachePath = tmpCache)
  if (!getOption("reproducible.useCacheV3") %in% TRUE) {
    expect_true(identical(thing, attr(aa, "debugCache1")[[1]]))
  } else {
    expect_true(identical(thing, attr(aa, "debugCache1")[[2]]))
  }
  expect_true(identical(.robustDigest(thing), attr(aa, "debugCache2")$n))
  expect_true(is.numeric(aa))

  ## debugCache -- "quick"
  aa <- Cache(rnorm, thing, debugCache = "quick", cachePath = tmpCache)
  expect_true(identical(.robustDigest(thing), aa$hash$n))
  if (!getOption("reproducible.useCacheV3") %in% TRUE) {
    expect_true(identical(thing, aa$content[[1]]))
  } else {
    expect_true(identical(thing, aa$content[[2]]))
  }

  ## .unlistToCharacter
  expect_true(grepl("not list", unlist(.unlistToCharacter(1, 1))))
  expect_true(grepl("not list2", unlist(.unlistToCharacter(1, 0))))

  ## writeFuture is defunct as of 3.2.2: the future-backed cache writing it
  ## supported was off by default and has been removed. It stays exported so old
  ## code gets a message naming the situation rather than "could not find
  ## function"; that message is now the whole contract.
  expect_error(writeFuture(1, "sdf", cachePath = tmpCache, userTags = ""), "defunct")

  try(silent = TRUE, clearCache(tmpCache, ask = FALSE))
  warn <- capture_warnings(
    bMess <- capture_output({
      aMess <- capture_messages({
        aa <- Cache(fnCacheHelper, 1, verbose = 4, cachePath = tmpCache, cacheRepo2 = tmpCache)
      })
    })
  )
  # warn has 'package:reproducible' may not be available when loading, not relevant
  expect_true(any(grepl("fnCacheHelper", aMess)))
  expect_true(any(grepl("The hashing details", aMess)))
})

test_that("test warnings from cached functions", {
  skip_if_not_installed("sf")

  testInit(libraries = c("sf"), opts = list(reproducible.useMemoise = FALSE))
  warn1 <- capture_warnings({
    b <- Cache(rbinom, 4, 5, prob = 6, cachePath = tmpCache)
  })

  fun <- function(n, size, prob) {
    rbinom(n, size, prob)
  }
  warn2 <- capture_warnings({
    d <- Cache(fun, 4, 5, 6, cachePath = tmpCache)
  })
  warnCompare <- "rbinom.+NAs produced"
  expect_true(grepl(warnCompare, warn1)) # includes the call because .call = FALSE, and call added manually in Cache
  expect_true(grepl("NAs produced", warn2))
  expect_false(grepl(warnCompare, warn2)) # this is false because the warning message doesn't include the call with normal warn
})

test_that("test cache-helpers with stacks", {
  skip_if_not_installed("raster")

  # THIS TEST CAN BE DELETED AFTER RASTER IS DEFUNCT
  testInit("raster")

  tmpfile <- tempfile(tmpdir = tmpdir, fileext = ".tif")
  tmpfile2 <- tempfile(tmpdir = tmpdir, fileext = ".tif")
  r <- raster(extent(0, 5, 0, 5), resolution = 1, vals = rep(1:2, length.out = 25))
  r1 <- raster(extent(0, 5, 0, 5), resolution = 1, vals = rep(1:2, length.out = 25))
  s <- raster::stack(r, r1)

  ## in memory
  b <- .prepareFileBackedRaster(s, tmpCache)
  is(b, "RasterStack")
  expect_true(length(list.files(file.path(tmpCache, "rasters"))) == 0)

  ## with 1 backups
  r <- .writeRaster(r, filename = tmpfile, overwrite = TRUE)
  s <- addLayer(r, r1)
  b <- .prepareFileBackedRaster(s, tmpCache)

  expect_true(all(basename(c(tmpfile)) %in% basename(list.files(tmpCache, recursive = TRUE))))
  expect_false(all(basename(c(tmpfile2)) %in% basename(list.files(tmpCache, recursive = TRUE))))

  ## with 2 backups
  r1 <- .writeRaster(r1, filename = tmpfile2, overwrite = TRUE)
  s <- addLayer(r, r1)
  b <- .prepareFileBackedRaster(s, tmpCache)
  expect_true(all(basename(c(tmpfile, tmpfile2)) %in% basename(list.files(tmpCache, recursive = TRUE))))

  ## removing entry from Cache
  fls <- grep(basename(tmpfile),
    list.files(tmpCache, recursive = TRUE, full.names = TRUE),
    value = TRUE
  )
  file.remove(fls)
  expect_false(all(basename(c(tmpfile, tmpfile2)) %in% basename(list.files(tmpCache, recursive = TRUE))))
  b <- .prepareFileBackedRaster(s, tmpCache)
  expect_true(all(basename(c(tmpfile, tmpfile2)) %in% basename(list.files(tmpCache, recursive = TRUE))))

  # Test deleted raster backed file
  file.remove(tmpfile2)
  expect_error(
    {
      b <- .prepareFileBackedRaster(s, tmpCache)
    },
    "The following file-backed rasters"
  )
})

test_that("test miscellaneous unit tests cache-helpers", {
  skip_if_not_installed("googledrive")

  testInit("googledrive")
  a <- Cache(rnorm, 1, cachePath = tmpCache)
  mess <- capture_messages(clearCache(cachePath = tmpCache))
  expect_true(any(grepl("x not specified, but cachePath is", mess)))
})

test_that("cache tag values containing quotes round-trip through both backends", {
  testInit()

  ## Both backends where possible; the DBI one is where the bug lived.
  backends <- if (.requireNamespace("RSQLite") && .requireNamespace("DBI")) {
    c(FALSE, TRUE)
  } else {
    FALSE
  }

  for (ud in backends) {
    withr::local_options(reproducible.useDBI = ud)
    expect_identical(useDBI(), ud)

    cp <- file.path(tmpdir, paste0("quoteTags", ud))
    a <- Cache(rnorm, 1, cachePath = cp)
    cid <- gsub("cacheId:", "", grep("cacheId:", attr(a, "tags"), value = TRUE))

    ## A single quote in a tagValue used to be pasted straight into the SQL,
    ## making it invalid. Because that failure is deterministic, the retry() in
    ## .addTagsRepo re-ran it 250 times before erroring -- a multi-minute hang.
    tricky <- "/home/o'brien/Ian's \"data\".tif"
    .addTagsRepo(cid, cp, tagKey = "origFilename", tagValue = tricky)
    sc <- showCacheFast(cid, cp, strict = FALSE)
    expect_identical(extractFromCache(sc, "origFilename"), tricky)

    ## .updateTagsRepo: both the update-in-place and the add-if-absent branches.
    tricky2 <- paste0(tricky, "'v2")
    .updateTagsRepo(cid, cp, tagKey = "origFilename", tagValue = tricky2)
    .updateTagsRepo(cid, cp, tagKey = "quotedNewKey", tagValue = tricky2, add = TRUE)
    sc <- showCacheFast(cid, cp, strict = FALSE)
    expect_identical(extractFromCache(sc, "origFilename"), tricky2)
    expect_identical(extractFromCache(sc, "quotedNewKey"), tricky2)

    ## showCacheFast returns only the requested cacheId (the DBI branch queries
    ## for it; the file-backed one reads that cacheId's metadata file).
    b <- Cache(rnorm, 2, cachePath = cp)
    expect_identical(unique(showCacheFast(cid, cp, strict = FALSE)[["cacheId"]]), cid)
  }
})

test_that("CacheIsACache does not rename another cachePath's table (DBI)", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")
  testInit()
  withr::local_options(reproducible.useDBI = TRUE)
  skip_if_not(useDBI())

  drv <- getDrv(NULL)
  cpA <- file.path(tmpdir, "repoA")
  cpB <- file.path(tmpdir, "repoB")
  invisible(Cache(rnorm, 1, cachePath = cpA))
  invisible(Cache(rnorm, 2, cachePath = cpB))

  connA <- dbConnectAll(drv, cachePath = cpA)
  on.exit(try(DBI::dbDisconnect(connA), silent = TRUE), add = TRUE)
  tableA <- DBI::dbListTables(connA)

  ## A connection for repoA paired with repoB's cachePath is a *normal*
  ## occurrence -- `conn` defaults to the single global reproducible.conn, and
  ## Cache() may span several cachePaths. The table-name mismatch that follows
  ## must NOT be read as "this repo moved": movedCache() would ALTER TABLE ...
  ## RENAME repoA's table to repoB's name, silently corrupting repoA.
  expect_false(CacheIsACache(cpB, drv = drv, conn = connA))
  expect_identical(DBI::dbListTables(connA), tableA)

  ## ... but a genuinely moved repo (same database file, stale table name) must
  ## still self-repair, which is what movedCache() is for.
  cpMoved <- file.path(tmpdir, "repoMoved")
  checkPath(file.path(cpMoved, "cacheOutputs"), create = TRUE)
  froms <- dir(cpA, recursive = TRUE, full.names = TRUE)
  file.copy(froms, gsub(basename(cpA), basename(cpMoved), froms), overwrite = TRUE)

  connMoved <- dbConnectAll(drv, cachePath = cpMoved)
  on.exit(try(DBI::dbDisconnect(connMoved), silent = TRUE), add = TRUE)
  expect_identical(DBI::dbListTables(connMoved), tableA) # stale name, pre-repair
  suppressWarnings(CacheIsACache(cpMoved, drv = drv, conn = connMoved))
  expect_identical(DBI::dbListTables(connMoved), CacheDBTableName(cpMoved, drv = drv))
})
