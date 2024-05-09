test_that("test miscellaneous unit tests cache-helpers", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  testInit(libraries = c("sf", "terra"), opts = list(reproducible.useMemoise = TRUE))

  a <- 1
  mess <- capture_message(.cacheMessage(a, "test", TRUE))
  expect_true(any(grepl(.messageLoadedCacheResult("Memoised"), mess)))

  mess <- capture_message(.cacheMessage(a, "test", FALSE))
  ## TODO: what was the old expected behaviour here? message now includes "added memoised copy"
  # expect_false(any(grepl(paste0(.messageLoadedCacheResult(), ".*added"), mess)))

  mess <- capture_message(.cacheMessage(a, "test", NA))
  expect_true(any(grepl(.messageLoadedCacheResult(), mess)))
  expect_false(all(grepl("adding", mess)))

  # studyAreaName with sf and sfc
  if (require("sf")) {
    pol <- st_sfc(st_polygon(list(cbind(c(0, 3, 3, 0, 0), c(0, 0, 3, 3, 0)))))
    h <- st_sf(r = 5, pol)
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
  options(reproducible.cachePath = .reproducibleTempCacheDir())
  mess <- capture_message(.checkCacheRepo(a))
  expect_true(any(grepl(.messageNoCacheRepoSuppliedGrep, mess)))

  opt11 <- options("reproducible.cachePath" = NULL)
  on.exit(
    {
      options(opt11)
    },
    add = TRUE
  )
  mess <- capture_message(.checkCacheRepo(a))
  expect_true(any(grepl(paste0(.messageNoCachePathSupplied, ". Using"), mess)))

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
  expect_false(any(grepl(.messageLoadedCacheResult("Memoised"), bMess)))
  expect_true(any(grepl(.messageLoadedCacheResult("Memoised"), dMess)))

  ## showSimilar
  try(clearCache(ask = FALSE, x = tmpCache), silent = TRUE)
  aMess <- capture_messages({
    a <- Cache(rnorm, n = 1, mean = 1, cachePath = tmpCache)
  })
  # lapply(letters[11], function(l) assign(paste(rep(l, 4), collapse = ""), 1, envir = .GlobalEnv))
  bMess <- capture_messages({
    b <- Cache(rnorm, n = 2, mean = 1, sd = 3, showSimilar = TRUE, cachePath = tmpCache)
  })
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
  })
  hMess <- capture_messages({
    b <- Cache(rbinom, 14, 15, prob = 0.8, showSimilar = TRUE, cachePath = tmpCache)
  })
  iMess <- capture_messages({
    b <- Cache(rcompletelynew, 12, 15, prob = 0.8, showSimilar = TRUE, cachePath = tmpCache)
  })
  expect_true(any(grepl("no similar item", eMess))) # shouldn't find b/c new
  expect_true(any(grepl("no similar item", fMess))) # shouldn't find b/c args are same
  expect_true(any(grepl("next closest.+rmultin", gMess))) # should only find rmultinom
  expect_true(any(grepl("next closest.+rbinom", hMess))) # should only find rbinom
  expect_true(sum(grepl(".+rcompletelynew|next closest.+rmultin", iMess)) == 3) # should notice different name, but still find

  ### UserTags matching -- prefer similar if all userTags match
  rcompletelynew <- rnorm
  # Now check function is prefered over args
  clearCache(tmpCache, ask = FALSE)
  jMess <- capture_messages({
    b <- Cache(rnorm, 1, 2, 3, showSimilar = TRUE, cachePath = tmpCache, userTags = c("Hi"))
  })
  kMess <- capture_messages({
    b1 <- Cache(rnorm, 1, 3, 4, showSimilar = TRUE, cachePath = tmpCache, userTags = c("By")) # not similar
  })
  lMess <- capture_messages({
    b <- Cache(rnorm, 1, 3, 4, showSimilar = TRUE, cachePath = tmpCache, userTags = c("Hi")) # same, recovered
  })
  mMess <- capture_messages({
    b <- Cache(rnorm, 1, 2, 3, showSimilar = TRUE, cachePath = tmpCache, userTags = c("By")) # same recovered
  })
  nMess <- capture_messages({
    b <- Cache(rnorm, 1, 2, 2, showSimilar = TRUE, cachePath = tmpCache, userTags = c("By")) # similar to kMess
  })
  oMess <- capture_messages({
    b <- Cache(rnorm, 1, 2, 1, showSimilar = TRUE, cachePath = tmpCache) # similar to kMess
  })
  expect_true(any(grepl("no similar item", jMess))) # shouldn't find b/c new
  expect_true(any(grepl("no similar item", kMess))) # shouldn't find b/c args are same
  expect_true(any(grepl("Loaded", lMess))) # should only find rmultinom
  expect_true(any(grepl("Loaded", mMess))) # should only find rmultinom
  nMess <- grep("^.+next closest cacheId\\(s\\) (.+) of .+$", nMess, value = TRUE)
  expect_true(grepl(
    x = attr(b1, "tags"),
    gsub("^.+next closest cacheId\\(s\\) (.+) of .+$", "\\1", nMess) ## TODO: fix failing test
  )) # should only find kMess

  ## debugCache -- "complete"
  thing <- 1
  aa <- Cache(rnorm, thing, debugCache = "complete", cachePath = tmpCache)
  expect_true(identical(thing, attr(aa, "debugCache1")[[1]]))
  expect_true(identical(.robustDigest(thing), attr(aa, "debugCache2")$n))
  expect_true(is.numeric(aa))

  ## debugCache -- "quick"
  aa <- Cache(rnorm, thing, debugCache = "quick", cachePath = tmpCache)
  expect_true(identical(.robustDigest(thing), aa$hash$n))
  expect_true(identical(thing, aa$content[[1]]))

  ## .unlistToCharacter
  expect_true(grepl("not list", unlist(.unlistToCharacter(1, 1))))
  expect_true(grepl("other", unlist(.unlistToCharacter(1, 0))))

  ## writeFuture
  comp <- # if (useDBI())
    .robustDigest("sdf") # else
  # "dda1fbb70d256e6b3b696ef0176c63de"
  drvHere <- if (useDBI() && .requireNamespace("RSQLite")) RSQLite::SQLite() else NULL

  expect_true(identical(
    comp,
    writeFuture(1, "sdf",
      cachePath = tmpCache, userTags = "",
      drv = drvHere
    )
  ))
  expect_error(writeFuture(1, "sdf", cachePath = "sdfd", userTags = ""))

  if (interactive()) {
    try(silent = TRUE, clearCache(tmpCache, ask = FALSE))
    bMess <- capture_output({
      aMess <- capture_messages({
        aa <- Cache(fnCacheHelper, 1, verbose = 4, cachePath = tmpCache, cacheRepo2 = tmpCache)
      })
    })
    expect_true(any(grepl("fnCacheHelper", aMess))) # TODO: fix this;
    expect_true(any(grepl("The hashing details", aMess)))
  }
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
