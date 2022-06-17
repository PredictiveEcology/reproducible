##########################
test_that("test miscellaneous unit tests cache-helpers", {
  testInitOut <- testInit(opts = list(reproducible.useMemoise = TRUE))
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  a <- 1
  mess <- capture_message(.cacheMessage(a, "test", TRUE))
  expect_true(any(grepl(.loadedMemoisedResultMsg, mess)))

  mess <- capture_message(.cacheMessage(a, "test", FALSE))
  expect_true(any(grepl(paste0(.loadedCacheResultMsg, ".*added"), mess)))

  mess <- capture_message(.cacheMessage(a, "test", NA))
  expect_true(any(grepl(.loadedCacheResultMsg, mess)))
  expect_false(all(grepl("adding", mess)))

  # studyAreaName with sf and sfc
  if (require("sf")) {
    pol <- st_sfc(st_polygon(list(cbind(c(0,3,3,0,0),c(0,0,3,3,0)))))
    h <- st_sf(r = 5, pol)
    expect_true(is(studyAreaName(pol), "character"))
    expect_true(is(studyAreaName(h), "character"))
  }

  # studyAreaName with SPDF/SP
  coords <- structure(c(-122.98, -116.1, -99.2, -106, -122.98, 59.9, 65.73, 63.58, 54.79, 59.9),
                      .Dim = c(5L, 2L))
  Sr1 <- Polygon(coords)
  Srs1 <- Polygons(list(Sr1), "s1")
  StudyArea <- SpatialPolygons(list(Srs1), 1L)
  df <- data.frame(a = 1, row.names = row.names(StudyArea))

  SPDF <- SpatialPolygonsDataFrame(StudyArea, df, match.ID = TRUE)
  expect_true(is(studyAreaName(StudyArea), "character"))
  expect_true(is(studyAreaName(SPDF), "character"))

  # studyAreaName with random object
  expect_error(studyAreaName(integer(0)))

  # .checkCacheRepo
  options(reproducible.cachePath = .reproducibleTempCacheDir())
  mess <- capture_message(.checkCacheRepo(a))
  expect_true(any(grepl(messageNoCacheRepo, mess)))

  opt11 <- options("reproducible.cachePath" = NULL)
  on.exit({
    options(opt11)
  }, add = TRUE)
  mess <- capture_message(.checkCacheRepo(a))
  expect_true(any(grepl("No cacheRepo supplied. Using", mess)))

  # getFunctionName
  fn <- function(FUN) {
    getFunctionName(fn, overrideCall = "fn")
  }
  expect_true(fn(1)$functionName == "FUN")

  fn <- function(FUN) {
    getFunctionName(fn, overrideCall = "fn")
  }
  expect_true(fn(2)$functionName == "FUN")

  fn <- function(FUN) {
    getFunctionName(1, overrideCall = "fn")
  }
  expect_true(fn(2)$functionName == "FUN")
  expect_true(is.null(fn(2)$.FUN))

  fn <- function(FUN) {
    getFunctionName(1, overrideCall = "fn")
  }
  expect_true(fn(log(1))$functionName== "FUN")

  ## nextNumericName
  b <- nextNumericName("test.pdf")
  b1 <- nextNumericName(b)
  expect_true(grepl("_2.pdf", b1))
  aMess <- capture_messages({
    a <- Cache(rnorm, 1, useCache = FALSE, cacheRepo = tmpCache)
  })
  bMess <- capture_messages({
    b <- Cache(rnorm, 1, useCache = FALSE, cacheRepo = tmpCache)
  })
  expect_false(identical(a,b))
  expect_true(grepl("skipping Cache", aMess))
  expect_true(grepl("skipping Cache", bMess))

  ## getOption("reproducible.useMemoise" = FALSE)
  opt22 <- options("reproducible.useMemoise" = FALSE)
  aMess <- capture_messages({
    a <- Cache(rnorm, 1, cacheRepo = tmpCache)
  })
  bMess <- capture_messages({
    a <- Cache(rnorm, 1, cacheRepo = tmpCache)
  })
  options(opt22)
  cMess <- capture_messages({
    a <- Cache(rnorm, 1, cacheRepo = tmpCache)
  })
  dMess <- capture_messages({
    a <- Cache(rnorm, 1, cacheRepo = tmpCache)
  })
  #expect_true(identical(aMess, bMess[1]))
  expect_false(any(grepl("memoise", bMess)))
  expect_true(any(grepl("memoise", dMess)))

  ## showSimilar
  try(clearCache(ask = FALSE, x = tmpCache), silent = TRUE)
  aMess <- capture_messages({
    a <- Cache(rnorm, n = 1, mean = 1, cacheRepo = tmpCache)
  })
  #lapply(letters[11], function(l) assign(paste(rep(l, 4), collapse = ""), 1, envir = .GlobalEnv))
  bMess <- capture_messages({
    b <- Cache(rnorm, n = 2, mean = 1, sd = 3, showSimilar = TRUE, cacheRepo = tmpCache)
  })
  expect_true(any(grepl("different n", bMess)))
  expect_true(any(grepl("new argument.*sd", bMess)))
  expect_true(any(grepl("next closest cacheId", bMess)))
  # aaaa <<- bbbb <<- cccc <<- 1
  cMess <- capture_messages({
    b <- Cache(rnorm, n = 3, mean = 1, sd = 3, showSimilar = TRUE, cacheRepo = tmpCache)
  })
  expect_true(any(grepl("different n", cMess)))
  expect_false(any(grepl("new argument.*sd", cMess)))
  cMessCacheId <- gsub(".*cacheId (.*)\x1b\\[.*", "\\1", grep("cacheId", cMess, value = TRUE))
  bMessCacheId <- gsub(".*cacheId (.*)\x1b\\[.*", "\\1", grep("cacheId", bMess, value = TRUE))
  expect_false(identical(cMessCacheId, bMessCacheId))

  dMess <- capture_messages({
    b <- Cache(rnorm, n = 4, mean = 1, sd = 4, showSimilar = TRUE, cacheRepo = tmpCache)
  })

  # There are 2 ways this may come out -- similarity to 1 of 2 alternatives above
  expect1 <- any(grepl("different n, sd", dMess))
  expect2 <- any(grepl("different n", dMess)) && any(grepl("new argument.*sd", dMess))
  expect_true(expect1 ||  (expect2))
  dMessCacheId <- gsub(".*cacheId (.*)\x1b\\[.*", "\\1", grep("cacheId", dMess, value = TRUE))
  bMessCacheId <- gsub(".*cacheId (.*)\x1b\\[.*", "\\1", grep("cacheId", bMess, value = TRUE))
    if (expect1) {
      expect_false(identical(dMessCacheId, bMessCacheId))
    } else {
      expect_true(identical(dMessCacheId, bMessCacheId))
    }

  eMess <- capture_messages({
    b <- Cache(rlnorm, 4, sd = 5, showSimilar = TRUE, cacheRepo = tmpCache)
  })
  expect_true(any(grepl("different .FUN", eMess)))
  expect_false(grepl(" n[ ,{\x1b}]", grep("different", eMess, value = TRUE)))
  expect_false(grepl("[ ,]sd[ ,{\x1b}]", grep("different", eMess, value = TRUE)))

  ## debugCache -- "complete"
  thing <- 1
  aa <- Cache(rnorm, thing, debugCache = "complete", cacheRepo = tmpCache)
  expect_true(identical(thing, attr(aa, "debugCache1")[[1]]))
  expect_true(identical(.robustDigest(thing), attr(aa, "debugCache2")$n))
  expect_true(is.numeric(aa))

  ## debugCache -- "quick"
  aa <- Cache(rnorm, thing, debugCache = "quick", cacheRepo = tmpCache)
  expect_true(identical(.robustDigest(thing), aa$hash$n))
  expect_true(identical(thing, aa$content[[1]]))

  ## .unlistToCharacter
  expect_true(grepl("not list", unlist(.unlistToCharacter(1, 1))))
  expect_true(grepl("other", unlist(.unlistToCharacter(1, 0))))

  ## writeFuture
  comp <- .robustDigest("sdf")
  expect_true(identical(comp,
                        writeFuture(1, "sdf", cacheRepo = tmpCache, userTags = "",
                                    drv = "fst"))) # RSQLite::SQLite()
  expect_error(writeFuture(1, "sdf", cacheRepo = "sdfd", userTags = ""))

  if (interactive()) {
    try(silent = TRUE, clearCache(tmpCache, ask = FALSE))
    bMess <- capture_output({
      aMess <- capture_messages({
        aa <- Cache(fnCacheHelper, 1, verbose = 4, cacheRepo = tmpCache, cacheRepo2 = tmpCache)
      })
    })
    expect_true(any(grepl("fnCacheHelper", aMess))) # TODO: fix this;
    expect_true(any(grepl("The hashing details", aMess)))
  }
})

test_that("test cache-helpers with stacks", {
  testInitOut <- testInit("raster")
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  tmpfile <- tempfile(tmpdir = tmpdir, fileext = ".tif")
  tmpfile2 <- tempfile(tmpdir = tmpdir, fileext = ".tif")
  r <- raster(extent(0, 5, 0, 5), res = 1, vals = rep(1:2, length.out = 25))
  r1 <- raster(extent(0, 5, 0, 5), res = 1, vals = rep(1:2, length.out = 25))
  s <- raster::stack(r, r1)

  ## in memory
  b <- .prepareFileBackedRaster(s, tmpCache)
  is(b, "RasterStack")
  browser()
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
  grep(basename(tmpfile), list.files(tmpCache, recursive = TRUE, full.names = TRUE),
       value = TRUE) |>
    file.remove()
  expect_false(all(basename(c(tmpfile, tmpfile2)) %in% basename(list.files(tmpCache, recursive = TRUE))))
  b <- .prepareFileBackedRaster(s, tmpCache)
  expect_true(all(basename(c(tmpfile, tmpfile2)) %in% basename(list.files(tmpCache, recursive = TRUE))))

  # Test deleted raster backed file
  file.remove(tmpfile2)
  expect_error({b <- .prepareFileBackedRaster(s, tmpCache)}, "The following file-backed rasters")
})

##########################
test_that("test miscellaneous unit tests cache-helpers", {
  testInitOut <- testInit()
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  a <- Cache(rnorm, 1, cacheRepo = tmpCache)
  mess <- capture_messages(clearCache(cacheRepo = tmpCache))
  expect_true(any(grepl("x not specified, but cacheRepo is", mess)))
  mess <- capture_messages(clearCache(x = tmpCache, useCloud = TRUE, cloudFolderID = NULL))
  expect_equal(sum(grepl("0 bytes", mess)), 2)
})
