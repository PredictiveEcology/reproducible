test_that("test file-backed raster caching", {
  skip_on_cran()
  testInit("terra",
    tmpFileExt = c(".tif", ".grd"),
    opts = list(reproducible.useMemoise = FALSE)
  )

  nOT <- Sys.time()

  randomPolyToDisk <- function(tmpfile) {
    r <- terra::rast(ext(0, 10, 0, 10), vals = sample(1:30, size = 100, replace = TRUE))
    .writeRaster(r, tmpfile[1], overwrite = TRUE)
    r <- terra::rast(tmpfile[1])
    r
  }

  a <- randomPolyToDisk(tmpfile[1])
  # confirm that the raster has the given tmp filename
  expect_equivalent(
    strsplit(normPath(tmpfile[1]), split = "[\\/]"),
    strsplit(normPath(Filenames(a)), split = "[\\/]")
  )

  # Using mock interactive function
  # https://www.mango-solutions.com/blog/testing-without-the-internet-using-mock-functions
  # https://github.com/r-lib/testthat/issues/734 to direct it to reproducible::isInteractive
  #   solves the error about not being in the testthat package
  val1 <- .cacheNumDefaultTags() + 1 + 11 # adding a userTag here... the +8 is the SpatRaster extras
  ik <- .ignoreTagKeys()
  # with_mock(
  #   "reproducible::isInteractive" = function() TRUE,
  #   {
  aa <- Cache(randomPolyToDisk, tmpfile[1], cachePath = tmpCache, userTags = "something2")
  # Test clearCache by tags

  expect_equal(NROW(showCache(tmpCache)[!tagKey %in% .ignoreTagKeys()]), val1)
  clearCache(tmpCache, userTags = "something$", ask = FALSE)
  expect_equal(NROW(showCache(tmpCache)[!tagKey %in% .ignoreTagKeys()]), val1)
  clearCache(tmpCache, userTags = "something2", ask = FALSE)
  expect_equal(NROW(showCache(tmpCache)), 0)

  aa <- Cache(randomPolyToDisk, tmpfile[1], cachePath = tmpCache, userTags = "something2")
  expect_equal(NROW(showCache(tmpCache)[!tagKey %in% .ignoreTagKeys()]), val1)
  clearCache(tmpCache, userTags = c("something$", "testing$"), ask = FALSE)
  expect_equal(NROW(showCache(tmpCache)[!tagKey %in% .ignoreTagKeys()]), val1)
  clearCache(tmpCache, userTags = c("something2$", "testing$"), ask = FALSE)
  expect_equal(NROW(showCache(tmpCache)[!tagKey %in% .ignoreTagKeys()]), val1)

  clearCache(tmpCache, userTags = c("something2$", "randomPolyToDisk$"), ask = FALSE)
  expect_equal(NROW(showCache(tmpCache)), 0)

  aa <- Cache(randomPolyToDisk, tmpfile[1], cachePath = tmpCache, userTags = "something2")

  # confirm that the raster has the new filename in the cachePath
  if (is(aa, "Raster")) {
    expect_false(identical(
      strsplit(tmpfile[1], split = "[\\/]"),
      strsplit(file.path(
        tmpCache, "rasters",
        basename(tmpfile[1])
      ), split = "[\\/]")
    ))
    expect_true(any(grepl(
      pattern = basename(tmpfile[1]),
      dir(file.path(tmpCache, "rasters"))
    )))
  } else {
    sc <- showCache(tmpCache)
    origFile <- sc[tagKey == "origFilename"]$cacheId
    expect_true(length(dir(CacheStorageDir(tmpCache), pattern = origFile)) == 1 + !useDBI())
  }

  clearCache(x = tmpCache)
  bb <- Cache(randomPolyToDisk, tmpfile[1],
    cachePath = tmpCache, userTags = "something2",
    quick = TRUE
  )
  # bb <- Cache(randomPolyToDisk, tmpfile[1], cachePath = tmpdir, userTags = "something2")
  # clearCache(x = tmpdir)
  clearCache(x = tmpdir)
  try(unlink(CacheDBFile(tmpdir)), silent = TRUE)
  try(unlink(CacheStorageDir(tmpdir), recursive = TRUE), silent = TRUE)
  froms <- normPath(dir(tmpCache, recursive = TRUE, full.names = TRUE))
  checkPath(file.path(tmpdir, "rasters"), create = TRUE)
  checkPath(file.path(tmpdir, "cacheOutputs"), create = TRUE)
  file.copy(
    from = froms, overwrite = TRUE,
    to = gsub(normPath(tmpCache), normPath(tmpdir), froms)
  )
  # movedCache(tmpdir)
  # ._prepareOutputs_1 <<- ._prepareOutputs_2 <<- ._getFromRepo <<- 1
  # Will silently update the filename of the RasterLayer, and recover it
  type <- gsub("Connection", "", class(getOption("reproducible.conn")))
  isSQLite <- grepl(type, "NULL")
  if (!isSQLite) {
    warn1 <- capture_warnings(movedCache(tmpdir, old = tmpCache))
  }

  warn <- capture_warnings({
    bb <- Cache(randomPolyToDisk, tmpfile[1],
      cachePath = tmpdir, userTags = "something2",
      quick = TRUE
    )
  })

  expect_false(attr(bb, ".Cache")$newCache)
  expect_true(file.exists(Filenames(bb)))
  expect_silent(bb[] <- bb[])

  # Delete the old everything to make sure previous didn't succeed because old pointer
  clearCache(x = tmpCache)
  try(unlink(CacheDBFile(tmpCache)), silent = TRUE)
  try(unlink(CacheStorageDir(tmpCache), recursive = TRUE), silent = TRUE)

  # ._Cache_6 <<- 1
  bb <- Cache(randomPolyToDisk, tmpfile[1],
    cachePath = tmpdir, userTags = "something2",
    quick = TRUE
  )
  expect_false(attr(bb, ".Cache")$newCache)
  expect_true(file.exists(Filenames(bb)))
  expect_silent(bb[] <- bb[])

  clearCache(tmpCache)
  clearCache(tmpdir)
  cc <- Cache(randomPolyToDisk, tmpfile[2],
    cachePath = tmpCache, userTags = "something2",
    quick = TRUE
  )
  bb <- Cache(randomPolyToDisk, tmpfile[1],
    cachePath = tmpCache, userTags = "something2",
    quick = TRUE
  )
  try(movedCache(tmpdir, tmpCache), silent = TRUE)

  ######
  bbS <- c(bb, cc)
  fn2 <- function(stk) {
    stk
  }
  aaaa <<- 1
  out <- Cache(fn2, bbS, cachePath = tmpCache, userTags = "something2")
  froms <- normPath(dir(tmpCache, recursive = TRUE, full.names = TRUE))
  # checkPath(file.path(tmpdir, "rasters"), create = TRUE)
  checkPath(file.path(tmpdir, "cacheOutputs"), create = TRUE)
  file.copy(
    from = froms, overwrite = TRUE,
    to = gsub(normPath(tmpCache), normPath(tmpdir), froms)
  )
  if (!isSQLite) {
    DBI::dbRemoveTable(conn, CacheDBTableName(tmpdir))
  }
  movedCache(tmpdir, tmpCache)
  out <- Cache(fn2, bbS, cachePath = tmpdir, userTags = "something2")

  clearCache(tmpdir)
  clearCache(tmpCache)

  ### Test for 2 caching events with same file-backing name
  randomPolyToDisk2 <- function(tmpfile, rand) {
    r <- terra::rast(terra::ext(0, 10, 0, 10), vals = sample(1:30, size = 100, replace = TRUE))
    .writeRaster(r, tmpfile[1], overwrite = TRUE)
    r <- terra::rast(tmpfile[1])
    r
  }

  a <- Cache(randomPolyToDisk2, tmpfile[1], runif(1))
  b <- Cache(randomPolyToDisk2, tmpfile[1], runif(1))

  # changed behaviour as of reproducible 1.2.0.9020
  #  -- now Cache doesn't protect user from filename collisions if user makes them
  expect_true(identical(Filenames(a), Filenames(b)))

  # Caching a raster as an input works
  rasterTobinary <- function(raster) {
    ceiling(raster[] / (mean(raster[]) + 1))
  }
  nOT <- Sys.time() + 1

  for (i in 1:2) {
    strt <- Sys.time()
    assign(paste0("a", i), Cache(rasterTobinary, a, cachePath = tmpCache, notOlderThan = nOT))
    fin <- Sys.time()
    assign(paste0("b", i), fin - strt)
    nOT <- Sys.time() - 100
  }
  expect_true(all.equalWONewCache(a1, a2))
  expect_true(identical(attr(a1, ".Cache")$newCache, TRUE))
  expect_true(identical(attr(a2, ".Cache")$newCache, FALSE))

  clearCache(tmpCache, ask = FALSE)

  # Check that Caching of rasters saves them to tif file instead of rdata
  randomPolyToMemory <- function() {
    r <- terra::rast(terra::ext(0, 10, 0, 10), vals = sample(1:30, size = 100, replace = TRUE))
    # terra::datatype(r) <- "INT1U"
    r
  }

  bb <- Cache(randomPolyToMemory, cachePath = tmpdir)
  expect_true(Filenames(bb) == "")
  expect_true(inMemory(bb))

  bb <- Cache(randomPolyToMemory, cachePath = tmpdir)
  expect_true(NROW(showCache(tmpdir)[!tagKey %in% .ignoreTagKeys()]) == .cacheNumDefaultTags())

  # Test that factors are saved correctly
  randomPolyToFactorInMemory <- function() {
    r <- terra::rast(terra::ext(0, 10, 0, 10), vals = sample(1:30, size = 100, replace = TRUE))
    levels(r) <- data.frame(
      ID = 1:30, vals = sample(LETTERS[1:5], size = 30, replace = TRUE),
      vals2 = sample(1:7, size = 30, replace = TRUE)
    )
    # dataType(r) <- "INT1U"
    r
  }
  bb <- Cache(randomPolyToFactorInMemory, cachePath = tmpdir)
  expect_true(terra::is.int(bb))
  # expect_equal(dataType2(bb), "INT1U") # irrelevant because on disk
  expect_true(terra::is.factor(bb))
  expect_true(is(terra::cats(bb)[[1]], "data.frame"))
  expect_true(NCOL(terra::cats(bb)[[1]]) == 3)
  expect_true(NROW(terra::cats(bb)[[1]]) == 30)

  randomPolyToFactorOnDisk <- function(tmpfile) {
    r <- terra::rast(terra::ext(0, 10, 0, 10), vals = sample(1:30, size = 100, replace = TRUE))
    levels(r) <- data.frame(
      ID = 1:30, vals = sample(LETTERS[1:5], size = 30, replace = TRUE),
      vals2 = sample(1:7, size = 30, replace = TRUE)
    )
    r <- .writeRaster(r, tmpfile, overwrite = TRUE, datatype = "INT1U")
    r
  }

  # bb1 has original tmp filename
  bb1 <- randomPolyToFactorOnDisk(tmpfile[2])
  # bb has new one, inside of cache repository, with same basename
  bb <- Cache(randomPolyToFactorOnDisk, tmpfile = tmpfile[2], cachePath = tmpdir)
  # changed behaviour as of reproducible 1.2.0.9020 -- now Cache doesn't protect user from filename collisions if user makes them
  expect_true(unique(dirname(normPath(Filenames(bb)))) != normPath(file.path(tmpdir, "rasters")))
  expect_true(identical(basename(Filenames(bb, allowMultiple = FALSE)), basename(tmpfile[2])))
  expect_equivalent(normPath(Filenames(bb, allowMultiple = FALSE)), normPath(tmpfile[2]))
  expect_equivalent(normPath(dirname(Filenames(bb1, allowMultiple = FALSE))), normPath(dirname(tmpfile[2])))
  expect_true(basename(Filenames(bb1, allowMultiple = FALSE)) == basename(tmpfile[2]))
  expect_true(dataType2(bb) == "INT1U")
  if (.requireNamespace("raster")) {
    expect_true(raster::is.factor(bb))
  }
  expect_true(is(terra::cats(bb)[[1]], "data.frame"))
  expect_true(NCOL(terra::cats(bb)[[1]]) == 3)
  expect_true(NROW(terra::cats(bb)[[1]]) == 30)

  clearCache(tmpdir, ask = FALSE)
  # })
})

test_that("test memory backed raster robustDigest", {
  testInit("terra", tmpFileExt = c(".tif", ".tif"))
  set.seed(123)
  r1 <- terra::rast(terra::ext(0, 10, 0, 10), vals = sample(1:30, size = 100, replace = TRUE))
  r2 <- terra::rast(terra::ext(0, 10, 0, 10), vals = sample(1:30, size = 100, replace = TRUE))
  set.seed(123)
  r3 <- terra::rast(terra::ext(0, 20, 0, 5), vals = sample(1:30, size = 100, replace = TRUE))
  expect_false(identical(.robustDigest(r1), .robustDigest(r2)))
  # metadata same, content different
  expect_false(identical(.robustDigest(r1), .robustDigest(r3)))
  # metadata different, content same
  expect_false(identical(.robustDigest(r1), .robustDigest(r3)))
  expect_true(identical(r1[], r3[]))

  set.seed(123)
  r1 <- terra::rast(terra::ext(0, 10, 0, 10), vals = sample(1:30, size = 100, replace = TRUE))
  set.seed(123)
  r2 <- terra::rast(terra::ext(0, 10, 0, 10), vals = sample(1:30, size = 100, replace = TRUE))
  expect_true(identical(.robustDigest(r1), .robustDigest(r2)))

  r <- terra::rast(matrix(1:10, 2, 5))
  b <- c(r, r)
  dig <- .robustDigest(b)

  r1 <- terra::rast(matrix(1:10, 2, 5))
  b1 <- c(r1, r1)
  dig1 <- .robustDigest(b1)

  expect_identical(dig, dig1)

  b <- .writeRaster(b, filename = tmpfile[1], overwrite = TRUE)
  dig <- .robustDigest(b)

  r <- terra::rast(matrix(1:10, 2, 5))
  b <- c(r, r)
  bb1 <- .writeRaster(b, filename = tmpfile[1], overwrite = TRUE)
  dig1 <- .robustDigest(bb1)

  expect_identical(dig, dig1)

  # Stacks
  dimA <- 100
  r <- terra::rast(matrix(1:dimA, round(sqrt(dimA)), round(sqrt(dimA))))
  b <- c(r, r)
  dig <- .robustDigest(b)

  r1 <- terra::rast(matrix(1:dimA, round(sqrt(dimA)), round(sqrt(dimA))))
  b1 <- c(r1, r1)
  dig1 <- .robustDigest(b1)

  expect_identical(dig, dig1)

  r4 <- .writeRaster(r, filename = tmpfile[1], overwrite = TRUE)
  r5 <- .writeRaster(r, filename = tmpfile[2], overwrite = TRUE)
  b <- c(r4, r5)
  dig <- .robustDigest(b)

  r2 <- .writeRaster(r1, filename = tmpfile[1], overwrite = TRUE)
  r3 <- .writeRaster(r1, filename = tmpfile[2], overwrite = TRUE)
  b1 <- c(r2, r3)
  # b1 <- .writeRaster(b1, filename = tmpfile[1], overwrite = TRUE)
  dig1 <- .robustDigest(b1)

  expect_identical(dig, dig1)
})

test_that("test 'quick' argument", {
  testInit("terra",
    tmpFileExt = c(".tif", ".tif", ".tif"),
    opts = list(
      "reproducible.useMemoise" = TRUE,
      "reproducible.showSimilar" = FALSE
    )
  )

  ### Make raster using Cache
  set.seed(123)
  # unlink(tmpfile[1])
  r1 <- terra::rast(terra::ext(0, 10, 0, 10), vals = sample(1:30, size = 100, replace = TRUE))
  r1 <- .writeRaster(r1, filename = tmpfile[1], overwrite = TRUE)
  quickFun <- function(rasFile) {
    ras <- terra::rast(rasFile)
    ras[sample(ncell(ras), size = 1)]
  }
  fn <- Filenames(r1)
  thePath1 <- asPath(Filenames(r1))

  out1a <- Cache(quickFun, thePath1, cachePath = tmpdir)
  out1b <- Cache(quickFun, thePath1, cachePath = tmpdir, quick = TRUE)
  r1[4] <- r1[4] + 1 # brings to memory
  r1 <- .writeRaster(r1, filename = tmpfile[2], overwrite = TRUE)
  thePath <- asPath(Filenames(r1))
  mess1 <- capture_messages({
    out1c <- Cache(quickFun, thePath, cachePath = tmpdir, quick = TRUE)
  })

  expect_true(sum(grepl(
    paste0(
      paste(.loadedCacheMsg(.loadedCacheResultMsg, "quickFun"), .addingToMemoisedMsg), "|",
      .loadedCacheMsg(.loadedMemoisedResultMsg, "quickFun")
    ),
    mess1
  )) == 0)
  # expect_true(any(grepl(paste(.loadedCacheResultMsg, "quickFun call, adding to memoised copy"), mess1 )))
  mess2 <- capture_messages({
    out1c <- Cache(quickFun, thePath, cachePath = tmpdir, quick = FALSE)
  })
  expect_true(length(mess2) == 0) # because it is looking at the file contents

  # Using Raster directly -- not file
  quickFun <- function(ras) {
    ras[sample(ncell(ras), size = 1)]
  }

  out1a <- Cache(quickFun, r1, cachePath = tmpdir)
  out1b <- Cache(quickFun, r1, cachePath = tmpdir, quick = TRUE)
  r1[4] <- r1[4] + 1
  r1 <- .writeRaster(r1, filename = tmpfile[3], overwrite = TRUE)
  # bbbb <<- 1
  # on.exit(rm(bbbb, envir = .GlobalEnv), add = TRUE)
  mess1 <- capture_messages({
    out1c <- Cache(quickFun, r1, cachePath = tmpdir, quick = TRUE)
  })
  expect_true(sum(grepl(
    paste0(
      paste(.loadedCacheMsg(.loadedCacheResultMsg, "quickFun"), .addingToMemoisedMsg), "|",
      paste(.loadedMemoisedResultMsg, "quickFun call")
    ),
    mess1
  )) == 0)

  # mess3 <- capture_messages({ out1c <- Cache(quickFun, r1, cachePath = tmpdir, quick = FALSE) })
  expect_silent({
    out1c <- Cache(quickFun, r1, cachePath = tmpdir, quick = FALSE)
  })
})

test_that("test date-based cache removal", {
  testInit("terra", tmpFileExt = ".pdf")

  a <- Cache(runif, 1, cachePath = tmpdir)
  a1 <- showCache(tmpdir)
  expect_true(NROW(a1) > 0)
  b <- clearCache(tmpdir, before = Sys.Date() - 1, ask = FALSE)
  expect_true(NROW(b) == 0)
  expect_identical(a1, showCache(tmpdir))

  b <- clearCache(tmpdir, before = Sys.Date() + 1, ask = FALSE)
  expect_identical(data.table::setindex(b, NULL), data.table::setindex(a1, NULL))
})

test_that("test keepCache", {
  testInit("terra")
  Cache(rnorm, 10, cachePath = tmpdir)
  Cache(runif, 10, cachePath = tmpdir)

  # this next is round which is a primitive *with* methods -->
  # --> an exception see ?match.call which says "normally positional"
  # This failed previously because round dispatches .Primitive("round") which internally in the C
  # code now (R > 4.3.1) matches non-positionally e.g., round.POSIXt has 'units'
  Cache(round, runif(4), cachePath = tmpdir)

  expect_true(NROW(showCache(tmpdir)[!tagKey %in% .ignoreTagKeys()]) ==
    .cacheNumDefaultTags() * 3)
  expect_true(NROW(showCache(tmpdir, c("rnorm", "runif"))[!tagKey %in% .ignoreTagKeys()]) ==
    0) # and search
  expect_true(NROW(keepCache(tmpdir, "rnorm", ask = FALSE)[!tagKey %in% .ignoreTagKeys()]) == .cacheNumDefaultTags())

  # do it twice to make sure it can deal with repeats
  expect_true(NROW(keepCache(tmpdir, "rnorm", ask = FALSE)[!tagKey %in% .ignoreTagKeys()]) == .cacheNumDefaultTags())
  st <- Sys.time()
  Sys.sleep(1)
  Cache(sample, 10, cachePath = tmpdir)
  Cache(length, 10, cachePath = tmpdir)
  Cache(sum, runif(4), cachePath = tmpdir)
  expect_true(NROW(showCache(tmpdir, before = st)[!tagKey %in% .ignoreTagKeys()]) == .cacheNumDefaultTags())
  expect_true(NROW(keepCache(tmpdir, before = st, ask = FALSE)[!tagKey %in% .ignoreTagKeys()]) == .cacheNumDefaultTags())
  expect_true(NROW(showCache(tmpdir)[!tagKey %in% .ignoreTagKeys()]) == .cacheNumDefaultTags())

  ranNums <- Cache(runif, 4, cachePath = tmpdir, userTags = "objectName:a")
  ranNums <- Cache(rnorm, 4, cachePath = tmpdir, userTags = "objectName:a")
  showCache(tmpdir) # shows rnorm, runif and rnorm objects

  # shows nothing because object has both spades and rnorm
  expect_true(NROW(showCache(tmpdir, userTags = c("spades", "rnorm"))) == 0)

  # "or" search
  expect_true(length(unique(
    showCache(tmpdir, userTags = "spades|rnorm")[[.cacheTableHashColName()]]
  )) == 2)

  # keep all with spades or rnorm
  expect_true(length(unique(
    keepCache(tmpdir, userTags = "spades|rnorm", ask = FALSE)[[.cacheTableHashColName()]]
  )) == 2)

  # shows spades, runif and rnorm objects
  expect_true(length(unique(showCache(tmpdir)[[.cacheTableHashColName()]])) == 2)

  # This is from changes to round by luke tierney
  aa <- Sys.time()
  Cache(round, aa, units = "mins", cachePath = tmpdir)
})

test_that("test environments", {
  testInit("terra", tmpFileExt = ".pdf")

  # make several unique environments
  a <- new.env(parent = emptyenv())
  b <- new.env(parent = emptyenv())
  g <- new.env(parent = emptyenv())
  d <- new.env(parent = emptyenv())
  f <- new.env(parent = emptyenv())
  h <- new.env(parent = emptyenv())
  # give the same value to a in each
  a$a <- runif(10)
  b$a <- a$a
  g$a <- a$a
  # put an environment in a
  a$d <- d
  b$d <- f
  g$d <- h

  # Upper level -- all are same, because values are same, even though all are enviros
  shortFn <- function(a) sample(a$a)
  out <- Cache(shortFn, a = a, cachePath = tmpdir)
  out2 <- Cache(shortFn, a = b, cachePath = tmpdir)
  out3 <- Cache(shortFn, a = g, cachePath = tmpdir)
  attr(out2, ".Cache")$newCache <- TRUE
  expect_true(identical(attributes(out)["tags"], attributes(out2)["tags"]))
  expect_true(identical(attributes(out)["tags"], attributes(out3)["tags"]))

  # put 2 different values, 1 and 2 ... a's and g's envirs are same value, but
  #    different environment .. b's envir is different value
  a$d$a <- 1
  b$d$a <- 2
  g$d$a <- 1
  i <- as.list(a)
  i2 <- i
  i2$d <- as.list(i2$d)

  out <- Cache(shortFn, a = a, cachePath = tmpdir)
  out2 <- Cache(shortFn, a = b, cachePath = tmpdir)
  out3 <- Cache(shortFn, a = g, cachePath = tmpdir)
  out4 <- Cache(shortFn, a = i, cachePath = tmpdir)
  out5 <- Cache(shortFn, a = i2, cachePath = tmpdir)

  # test different values are different
  expect_false(identical(attributes(out), attributes(out2)))

  # test same values but different enviros are same
  expect_true(identical(attributes(out)["tags"], attributes(out3)["tags"]))

  # test environment is same as a list -- not sure what they should be
  expect_true(identical(attributes(out)["tags"], attributes(out4)["tags"]))

  # test environment is same as recursive list - they shouldn't be
  expect_true(identical(attributes(out)["tags"], attributes(out5)["tags"]))

  df <- data.frame(a = a$a, b = LETTERS[1:10])
  out6 <- Cache(shortFn, a = df, cachePath = tmpdir)
  out7 <- Cache(shortFn, a = df, cachePath = tmpdir)
  expect_true(identical(attributes(out6)["tags"], attributes(out7)["tags"])) # test data.frame
})

test_that("test asPath", {
  testInit("terra",
    tmpFileExt = "pdf",
    verbose = TRUE,
    opts = list(
      "reproducible.useMemoise" = TRUE,
      "reproducible.showSimilar" = FALSE
    )
  )
  unlink(dir(tmpdir, full.names = TRUE))

  obj <- sample(1e5, 10)
  # First -- has no filename.RData
  a1 <- capture_messages(Cache(saveRDS, obj, file = "filename.RData", cachePath = tmpdir))
  # Second -- has a filename.RData, and passing a character string,
  #           it tries to see if it is a file, if yes, it digests it
  a2 <- capture_messages(Cache(saveRDS, obj, file = "filename.RData", cachePath = tmpdir))
  # Third -- finally has all same as second time
  a3 <- capture_messages(Cache(saveRDS, obj, file = "filename.RData", cachePath = tmpdir))

  expect_true(length(a1) == 0)
  expect_true(length(a2) == 0)
  expect_true(sum(grepl(paste(
    .loadedMemoisedResultMsg, "|",
    .loadedCacheResultMsg
  ), a3)) == 1)

  unlink("filename.RData")
  try(clearCache(tmpdir, ask = FALSE), silent = TRUE)
  a1 <- capture_messages(Cache(saveRDS, obj,
    file = asPath("filename.RData"),
    quick = TRUE, cachePath = tmpdir
  ))
  a2 <- capture_messages(Cache(saveRDS, obj,
    file = asPath("filename.RData"),
    quick = TRUE, cachePath = tmpdir
  ))
  a3 <- capture_messages(Cache(saveRDS, obj,
    file = asPath("filename.RData"),
    quick = TRUE, cachePath = tmpdir
  ))
  expect_true(length(a1) == 0)
  expect_true(sum(grepl(paste(
    .loadedCacheResultMsg, "|",
    .loadedMemoisedResultMsg
  ), a2)) == 1)
  expect_true(sum(grepl(paste(.loadedMemoisedResultMsg, "saveRDS call"), a3)) == 1)

  unlink("filename.RData")
  try(clearCache(tmpdir, ask = FALSE), silent = TRUE)
  a1 <- capture_messages(Cache(saveRDS, obj,
    file = as("filename.RData", "Path"),
    quick = TRUE, cachePath = tmpdir
  ))
  a2 <- capture_messages(Cache(saveRDS, obj,
    file = as("filename.RData", "Path"),
    quick = TRUE, cachePath = tmpdir
  ))
  a3 <- capture_messages(Cache(saveRDS, obj,
    file = as("filename.RData", "Path"),
    quick = TRUE, cachePath = tmpdir
  ))
  expect_true(length(a1) == 0)
  expect_true(sum(grepl(paste(
    .loadedCacheResultMsg, "|",
    .loadedMemoisedResultMsg
  ), a2)) == 1)
  expect_true(sum(grepl(paste(.loadedMemoisedResultMsg, "saveRDS call"), a3)) == 1)
})

test_that("test wrong ways of calling Cache", {
  try(testInit(tmpFileExt = ".pdf"))

  # expect_error(Cache(sample(1), cachePath = tmpdir), "Can't understand")
  # expect_error(Cache(a <- sample(1), cachePath = tmpdir), "Can't understand")
  expect_true(1 == Cache(sample, 1, cachePath = tmpdir))
})

test_that("test quoted FUN in Cache", {
  testInit()

  A <- Cache(rnorm, 10, 16, cachePath = tmpdir) # nolint

  ## recover cached copies:
  B <- Cache(rnorm, 10, 16, cachePath = tmpdir) # nolint
  C <- Cache(quote(rnorm(n = 10, 16)), cachePath = tmpdir) # nolint

  expect_true(all.equalWONewCache(A, B))
  expect_true(all.equalWONewCache(A, C))
})

test_that("test Cache argument inheritance to inner functions", {
  testInit("terra",
    verbose = TRUE,
    opts = list(
      "reproducible.showSimilar" = FALSE,
      "reproducible.useMemoise" = FALSE
    )
  )
  tmpDirFiles <- dir(tempdir())
  on.exit(
    {
      newOnes <- setdiff(tmpDirFiles, dir(tempdir()))
      unlink(newOnes, recursive = TRUE)
    },
    add = TRUE
  )

  outer <- function(n) {
    Cache(rnorm, n)
  }

  expect_silent(Cache(outer, n = 2, cachePath = tmpdir))
  clearCache(ask = FALSE, x = tmpdir)

  options(reproducible.cachePath = tmpCache)
  out <- capture_messages(Cache(outer, n = 2))
  expect_true(all(unlist(lapply(
    c(messageNoCacheRepo, messageNoCacheRepo),
    function(mess) any(grepl(mess, out))
  ))))

  # does Sys.time() propagate to outer ones
  out <- capture_messages(Cache(outer, n = 2, notOlderThan = Sys.time() + 1))
  expect_true(all(grepl(messageNoCacheRepo, out)))

  # does Sys.time() propagate to outer ones -- no message about cachePath being tempdir()
  expect_silent(Cache(outer, n = 2, notOlderThan = Sys.time(), cachePath = tmpdir))

  # does cachePath propagate to outer ones -- no message about cachePath being tempdir()
  out <- capture_messages(Cache(outer, n = 2, cachePath = tmpdir))
  expect_true(length(out) == 2)
  expect_true(sum(grepl(paste(.loadedCacheResultMsg, "outer call"), out)) == 1)

  # check that the rnorm inside "outer" returns cached value even if outer "outer" function is changed
  outer <- function(n) {
    a <- 1
    Cache(rnorm, n)
  }
  out <- capture_messages(Cache(outer, n = 2, cachePath = tmpdir))
  expect_true(length(out) == 2)
  msgGrep <- paste(paste(.loadedCacheResultMsg, "rnorm call"),
    "There is no similar item in the cachePath",
    sep = "|"
  )
  expect_true(sum(grepl(msgGrep, out)) == 1)

  # Override with explicit argument
  outer <- function(n) {
    a <- 1
    Cache(rnorm, n, notOlderThan = Sys.time() + 1)
  }
  out <- capture_messages(Cache(outer, n = 2, cachePath = tmpdir))
  expect_true(length(out) == 0)
  # expect_true(all(grepl("There is no similar item in the cachePath", out)))

  # change the outer function, so no cache on that, & have notOlderThan on rnorm,
  #    so no Cache on that
  outer <- function(n) {
    b <- 1
    Cache(rnorm, n, notOlderThan = Sys.time() + 1)
  }
  out <- capture_messages(Cache(outer, n = 2, cachePath = tmpdir))
  expect_true(length(out) == 0)
  # expect_true(all(grepl("There is no similar item in the cachePath", out)))
  # Second time will get a cache on outer
  out <- capture_messages(Cache(outer, n = 2, cachePath = tmpdir))
  expect_true(length(out) == 2)
  expect_true(sum(grepl(paste(.loadedCacheResultMsg, "outer call"), out)) == 1)

  # doubly nested
  inner <- function(mean, useCache = TRUE) {
    d <- 1
    Cache(rnorm, n = 3, mean = mean, useCache = useCache)
  }
  outer <- function(n, useCache = TRUE, ...) {
    Cache(inner, 0.1, useCache = useCache, ...)
  }
  out <- capture_messages(Cache(outer, n = 2, cachePath = tmpdir))

  outer <- function(n) {
    Cache(inner, 0.1, notOlderThan = Sys.time() - 1e4)
  }

  out <- capture_messages(Cache(outer, n = 2, cachePath = tmpdir, notOlderThan = Sys.time()))
  msgGrep <- paste(paste(.loadedCacheResultMsg, "inner call"),
    "There is no similar item in the cachePath",
    sep = "|"
  )
  expect_true(sum(grepl(msgGrep, out)) == 1)

  outer <- function(n) {
    Cache(inner, 0.1, notOlderThan = Sys.time())
  }
  inner <- function(mean) {
    d <- 1
    Cache(rnorm, n = 3, mean = mean, notOlderThan = Sys.time() - 1e5)
  }

  out <- capture_messages(Cache(outer, n = 2, cachePath = tmpdir, notOlderThan = Sys.time()))
  msgGrep <- paste(paste(.loadedCacheResultMsg, "rnorm call"),
    "There is no similar item in the cachePath",
    sep = "|"
  )
  expect_true(sum(grepl(msgGrep, out)) == 1)

  # Check userTags -- all items have it
  clearCache(tmpdir, ask = FALSE)
  outerTag <- "howdie"
  aa <- Cache(outer, n = 2, cachePath = tmpdir, userTags = outerTag)
  bb <- showCache(tmpdir, userTags = outerTag)
  cc <- showCache(tmpdir)
  data.table::setorderv(bb, c(.cacheTableHashColName(), "tagKey", "tagValue"))
  data.table::setorderv(cc, c(.cacheTableHashColName(), "tagKey", "tagValue"))
  expect_true(identical(bb, cc))

  # Check userTags -- all items have the outer tag propagate, plus inner ones only have inner ones
  innerTag <- "notHowdie"
  inner <- function(mean) {
    d <- 1
    Cache(rnorm, n = 3, mean = mean, notOlderThan = Sys.time() - 1e5, userTags = innerTag)
  }

  clearCache(tmpdir, ask = FALSE)
  aa <- Cache(outer, n = 2, cachePath = tmpdir, userTags = outerTag)
  bb <- showCache(tmpdir, userTags = outerTag)
  cc <- showCache(tmpdir)
  data.table::setorderv(cc)
  data.table::setorderv(bb)
  expect_true(identical(bb, cc))

  #
  bb <- showCache(tmpdir, userTags = "notHowdie")
  cc <- showCache(tmpdir)
  data.table::setorderv(cc)
  data.table::setorderv(bb)
  expect_false(identical(bb, cc))
  expect_true(length(unique(bb[[.cacheTableHashColName()]])) == 1)
  expect_true(length(unique(cc[[.cacheTableHashColName()]])) == 3)
})

test_that("test future", {
  skip_on_cran()
  skip_on_ci()
  # skip_if_not_installed("future")

  .onLinux <- .Platform$OS.type == "unix" && unname(Sys.info()["sysname"]) == "Linux"
  # if (.onLinux) {
  testInit(c("terra", "future"),
    verbose = TRUE, tmpFileExt = ".rds",
    opts = list(
      "future.supportsMulticore.unstable" = "quiet",
      "reproducible.futurePlan" = "multicore"
    )
  )

  # There is now a warning with future package
  a <- list()
  (aa <- system.time({
    for (i in c(1:3)) a[[i]] <- Cache(cachePath = tmpCache, rnorm, 1e6 + i)
  }))
  sca <- showCache(tmpCache)
  expect_true(length(unique(sca[[.cacheTableHashColName()]])) == 3)

  try(unlink(tmpCache, recursive = TRUE))
  b <- list()
  (bb <- system.time({
    for (i in 1:3) b[[i]] <- Cache(cachePath = tmpCache, rnorm(1e6 + i))
  }))
  bb <- showCache(tmpCache)
  expect_true(length(unique(bb[[.cacheTableHashColName()]])) == 3)

  # Test the speed of rerunning same line
  d <- list()
  (dd <- system.time({
    for (i in 1:3) d[[i]] <- Cache(cachePath = tmpCache, rnorm(1e6 + i))
  }))
  expect_true((dd[[3]] * 3) < aa[[3]])
  for (i in 1:3) {
    expect_true(identical(attr(d[[i]], ".Cache")$newCache, FALSE))
  }
  # }
})

test_that("test mergeCache", {
  testInit("data.table", verbose = TRUE)

  a <- Cache(rnorm, 1, cachePath = tmpdir)
  b <- Cache(rnorm, 2, cachePath = tmpCache)

  aCache <- showCache(tmpdir)
  bCache <- showCache(tmpCache)

  d <- mergeCache(tmpCache, tmpdir)

  dCache <- showCache(d)
  abCache <- rbindlist(list(aCache, bCache))

  # Remove date and accessed time stamps
  dCache <- dCache[!tagKey %in% c("date", "accessed")]
  abCache <- abCache[!tagKey %in% c("date", "accessed")]

  # remove keys
  setkeyv(dCache, .cacheTableHashColName())
  setkeyv(abCache, .cacheTableHashColName())

  expect_true(all.equal(
    abCache[, list(
      get(.cacheTableHashColName()),
      tagKey, get(.cacheTableTagColName())
    )],
    dCache[, list(
      get(.cacheTableHashColName()),
      tagKey, get(.cacheTableTagColName())
    )]
  ))
  mess <- capture_messages({
    d1 <- mergeCache(tmpCache, tmpdir)
  })
  expect_true(any(grepl("Skipping", mess)))
  expect_true(identical(showCache(d), showCache(d1)))
})

# DELETE THIS ONE; IT IS NOT RELEVANT ANY MORE AND IT IS BROKEN
# test_that("test cache-helpers", {
#   testInit("terra")
#   out <- createCache(tmpCache)
#
#   tmpfile <- tempfile(tmpdir = tmpdir, fileext = ".grd")
#   tmpfile2 <- tempfile(tmpdir = tmpdir, fileext = ".grd")
#   tmpfile3 <- tempfile(tmpdir = tmpdir, fileext = ".grd")
#   r <- terra::rast(terra::ext(0, 5, 0, 5), resolution = 1, vals = rep(1:2, length.out = 25))
#   # levels(r) <- data.frame(ID = 1:2, Val = 3:4)
#   # b <- .prepareFileBackedRaster(r, tmpCache)
#   # is(b, "RasterLayer")
#
#   r1 <- terra::rast(terra::ext(0, 5, 0, 5), resolution = 1, vals = rep(1:2, length.out = 25))
#   # s <- c(r, r1)
#   # b <- .prepareFileBackedRaster(s, tmpCache)
#
#   r <- .writeRaster(r, filename = tmpfile, overwrite = TRUE)
#   r1 <- .writeRaster(r1, filename = tmpfile2, overwrite = TRUE)
#   s <- c(r, r1)
#
#   # Test deleted raster backed file
#   # file.remove(tmpfile2)
#   # expect_error(b <- .prepareFileBackedRaster(s, tmpCache), "The following file-backed rasters")
#   # expect_error(b <- .prepareFileBackedRaster(r1, tmpCache), "The following file-backed rasters")
#
#   # Test wrong folder names
#   tmpfile <- file.path(tmpCache, basename(tempfile(tmpdir = tmpdir, fileext = ".grd")))
#   r <- .writeRaster(r, filename = tmpfile, overwrite = TRUE)
#   # r@file@name <- gsub(pattern = dirname(tmpfile),
#   #                     normalizePath(tmpfile, winslash = "/", mustWork = FALSE),
#   #                     replacement = dirname(dirname(tmpfile)))
#   # # show it is not there, so it is the wrong name
#   # expect_false(all(file.exists(Filenames(r))))
#   # fix it, by giving correct tmpCache path
#   # b <- .prepareFileBackedRaster(r, tmpCache)
#   # expect_true(all(file.exists(Filenames(b))))
#   # Check that it makes a new name if already in Cache
#   # checkPath(file.path(tmpCache, "rasters"), create = TRUE)
#   # r1 <- .writeRaster(r1, filename = file.path(tmpCache, "rasters", basename(tmpfile2)), overwrite = TRUE)
#   # b <- .prepareFileBackedRaster(r1, tmpCache)
#   expect_true(identical(normalizePath(Filenames(b), winslash = "/", mustWork = FALSE),
#                         normalizePath(file.path(dirname(Filenames(r1)),
#                                                 nextNumericName(basename(Filenames(r1)))),
#                                       winslash = "/", mustWork = FALSE)))
#
#   r <- raster(extent(0, 5, 0, 5), res = 1, vals = rep(1:2, length.out = 25))
#   r1 <- raster(extent(0, 5, 0, 5), res = 1, vals = rep(1:2, length.out = 25))
#   tmpfile <- tempfile(tmpdir = tmpdir, fileext = ".grd")
#   r <- .writeRaster(r, filename = tmpfile, overwrite = TRUE)
#   r1 <- .writeRaster(r1, filename = tmpfile2, overwrite = TRUE)
#   s <- addLayer(r, r1)
#   b1 <- .prepareFileBackedRaster(s, repoDir = tmpCache)
#   expect_true(is(b1, "RasterStack"))
#   expect_true(identical(Filenames(b1), ""))
#   expect_true(identical(normalizePath(Filenames(b1$layer.1), winslash = "/", mustWork = FALSE),
#                         normalizePath(file.path(tmpCache, "rasters", basename(Filenames(r))), winslash = "/", mustWork = FALSE)))
#
#   # Give them single file -- 2 layer stack; like a raster::brick, but a stack
#   r[] <- r[]
#   r1[] <- r1[]
#   b <- raster::stack(r, r1)
#
#   b <- .writeRaster(b, filename = tmpfile, overwrite = TRUE)
#   b <- raster::stack(b)
#   expect_true(nlayers2(b) == 2)
#   expect_true(identical(normPath(b$layer.1@file@name),
#                         normPath(b$layer.2@file@name)))
#
#   b1 <- .prepareFileBackedRaster(b, tmpCache)
#   expect_true(nlayers2(b1) == 2)
#   b1a <- raster::stack(Filenames(b1)[1])
#   expect_true(nlayers2(b1a) == 2)
#
# })

test_that("test cache-helpers", {
  testInit(c("raster"), tmpFileExt = c(rep(".grd", 3), rep(".tif", 3)))
  # out <- reproducible::createCache(tmpCache)

  r1 <- raster::raster(raster::extent(0, 3, 0, 3), vals = 1)
  r2 <- raster::raster(raster::extent(0, 3, 0, 3), vals = 2)
  r3 <- raster::raster(raster::extent(0, 3, 0, 3), vals = 3)
  r2 <- writeRaster(r1, filename = tmpfile[2], overwrite = TRUE)
  r3 <- writeRaster(r1, filename = tmpfile[3], overwrite = TRUE)
  r2tif <- suppressWarningsSpecific(
    falseWarnings = proj6Warn,
    writeRaster(r1, filename = tmpfile[5], overwrite = TRUE)
  )
  r3tif <- suppressWarningsSpecific(
    falseWarnings = proj6Warn,
    writeRaster(r1, filename = tmpfile[6], overwrite = TRUE)
  )

  s1 <- raster::stack(r1, r1)
  s2 <- raster::stack(r1, r2)
  s3 <- raster::stack(r3, r2)
  s1 <- raster::stack(r1, r1)
  s2tif <- raster::stack(r1, r2tif)
  s3tif <- raster::stack(r3tif, r2tif)

  i <- 1
  for (rr in list(r1, r2, r3, r2tif, r3tif, s1, s2, s3, s2tif, s3tif)) {
    message(i)
    i <- i + 1

    out2 <- .prepareFileBackedRaster(rr, repoDir = tmpCache)
    test1 <- identical(out2, rr)
    test2 <- identical(Filenames(out2), Filenames(rr))
    test3 <- identical(
      Filenames(out2, allowMultiple = FALSE),
      Filenames(rr, allowMultiple = FALSE)
    )
    test4 <- identical(
      basename(Filenames(out2, allowMultiple = TRUE)),
      basename(Filenames(rr, allowMultiple = TRUE))
    )
    test5 <- identical(length(Filenames(out2)), length(Filenames(rr)))
    if (any(nchar(Filenames(out2)) > 0)) {
      expect_false(test1 && test2 && test3)
      expect_true(test4 && test5)
    } else {
      expect_true(test1 && test2 && test3 && test4 && test5)
    }
    unlink(Filenames(out2))
  }

  out2 <- .prepareFileBackedRaster(s2, repoDir = tmpCache)
  out3 <- .prepareFileBackedRaster(s2, repoDir = tmpCache)
  fn2 <- Filenames(out2)
  fn3 <- Filenames(out3)
  actualFiles <- nchar(fn2) > 0
  bnfn2 <- basename(fn2[actualFiles])
  bnfn3 <- basename(fn3[actualFiles])
  bnfn2 <- unique(filePathSansExt(bnfn2))
  bnfn3 <- unique(filePathSansExt(bnfn3))
  sameFileBase <- grepl(pattern = bnfn2, x = bnfn3)
  expect_true(sameFileBase)

  unlink(Filenames(s2))
  expect_error(
    {
      out2 <- .prepareFileBackedRaster(s2, repoDir = tmpCache)
    },
    "most likely"
  )
})

test_that("test useCache = 'overwrite'", {
  testInit(verbose = TRUE)

  a <- Cache(rnorm, 1, useCache = "overwrite", cachePath = tmpCache)
  mess <- capture_messages({
    b <- Cache(rnorm, 1, useCache = "overwrite", cachePath = tmpCache)
  })
  expect_true(!identical(a, b))
  expect_true(any(grepl(pattern = "Overwriting", mess)))

  clearCache(x = tmpCache, ask = FALSE)

  withr::deferred_clear()
  testInit(
    ask = FALSE, verbose = TRUE,
    opts = list("reproducible.useCache" = "overwrite")
  )

  a <- Cache(rnorm, 1, cachePath = tmpCache)
  mess <- capture_messages({
    b <- Cache(rnorm, 1, cachePath = tmpCache)
  })
  expect_true(!identical(a, b))
  expect_true(any(grepl(pattern = "Overwriting", mess)))
})

test_that("test rm large non-file-backed rasters", {
  ## This is a large object test!
  skip_on_cran()
  if (!is.null(getOption("reproducible.conn", NULL))) {
    if (!grepl("SQLite", class(getOption("reproducible.conn", NULL)))) {
      skip("This is not for non-SQLite")
    }
  }

  testInit(c("qs", "terra"), opts = list("reproducible.cacheSpeed" = "fast",
                             "reproducible.cacheSaveFormat" = "qs"))

  ext <- terra::ext(0, 10000, 0, 10000)
  r <- Cache(terra::rast, ext,
    resolution = 1, vals = 1,
    cachePath = tmpdir, userTags = "first"
  )
  st1 <- system.time(clearCache(tmpdir, userTags = "first", ask = FALSE))
  expect_true(st1["user.self"] < 0.75) # This was > 2 seconds in old way
})

test_that("test cc", {
  skip_on_cran()
  # skip_on_ci()

  testInit(verbose = TRUE)

  Cache(rnorm, 1, cachePath = tmpCache)
  Sys.sleep(1) # 0.2
  thisTime <- Sys.time()
  Sys.sleep(1) # 0.2
  Cache(rnorm, 2, cachePath = tmpCache)
  Cache(rnorm, 3, cachePath = tmpCache)
  Cache(rnorm, 4, cachePath = tmpCache)
  a <- showCache(x = tmpCache) # shows all 4 entries
  expect_true(length(unique(a[[.cacheTableHashColName()]])) == 4)

  # rmFC <<- 1
  cc(ask = FALSE, x = tmpCache)
  b <- showCache(x = tmpCache) # most recent is gone
  expect_true(length(unique(b[[.cacheTableHashColName()]])) == 3)

  # ._rmFromCache_1 <<- 1
  cc(thisTime, ask = FALSE, x = tmpCache)
  d <- showCache(x = tmpCache) # all those after this time gone, i.e., only 1 left
  expect_true(length(unique(d[[.cacheTableHashColName()]])) == 1)

  cc(ask = FALSE, x = tmpCache) # Cache is
  b1 <- showCache(x = tmpCache) # most recent is gone
  expect_true(length(unique(b1[[.cacheTableHashColName()]])) == 0)

  mess <- capture_messages(cc(ask = FALSE, x = tmpCache)) # Cache is already empty
  expect_true(any(grepl("Cache already empty", mess)))
})

test_that("test pre-creating conn", {
  if (!useDBI()) skip("Only relevant for DBI backend")
  testInit("terra", ask = FALSE, tmpFileExt = c(".tif", ".tif"))
  on.exit({
    DBI::dbDisconnect(conn)
  })

  conn <- dbConnectAll(cachePath = tmpdir, conn = NULL)
  ra <- terra::rast(terra::ext(0, 10, 0, 10), vals = sample(1:100))
  rb <- terra::rast(terra::ext(0, 10, 0, 10), vals = sample(1:100))
  r1 <- Cache(.writeRaster, ra, filename = tmpfile[1], overwrite = TRUE, cachePath = tmpCache)
  r2 <- Cache(.writeRaster, rb,
    filename = tmpfile[2], overwrite = TRUE, cachePath = tmpdir,
    conn = conn
  )
  expect_true(file.exists(Filenames(r1)))
  expect_true(file.exists(Filenames(r2)))
  expect_false(grepl(basename(dirname(Filenames(r1))), "rasters")) # changed behaviour as of reproducible 1.2.0.9020
  expect_false(grepl(basename(dirname(Filenames(r2))), "rasters")) # changed behaviour as of reproducible 1.2.0.9020
})

test_that("test .defaultUserTags", {
  testInit()

  b <- Cache(rnorm, 1, cachePath = tmpCache)
  sc <- showCache(tmpCache)
  actualTags <- sc$tagKey %in% .defaultUserTags
  anyNewTags <- any(!actualTags)
  if (isTRUE(anyNewTags)) stop("A new default userTag was added; please update .defaultUserTags")
  expect_false(anyNewTags)
})

test_that("test failed Cache recovery -- message to delete cacheId", {
  if (!useDBI()) skip("Not relevant for multipleDBfiles")
  testInit(opts = list("reproducible.useMemoise" = FALSE))

  b <- Cache(rnorm, 1, cachePath = tmpdir)
  sc <- showCache(tmpdir)
  ci <- unique(sc[[.cacheTableHashColName()]])
  unlink(CacheStoredFile(tmpdir, ci))

  warn <- capture_warnings({
    err <- capture_error({
      b <- Cache(rnorm, 1, cachePath = tmpdir)
    })
  })
  expect_true(grepl(paste0("(trying to recover).*(", ci, ")"), err))
  expect_true(grepl(paste0("[cannot|failed to] open"), paste(warn, err)))
})

test_that("test changing reproducible.cacheSaveFormat midstream", {
  if (!.requireNamespace("qs")) skip("Need qs; skipping test")
  testInit(opts = list(
    "reproducible.useMemoise" = FALSE,
    "reproducible.cacheSaveFormat" = "rds"
  ))

  b <- Cache(rnorm, 1, cachePath = tmpdir)
  sc <- showCache(tmpdir)
  ci <- unique(sc[[.cacheTableHashColName()]])
  options("reproducible.cacheSaveFormat" = "qs")
  on.exit(
    {
      options(opts)
    },
    add = TRUE
  )
  mess <- capture_messages({
    b <- Cache(rnorm, 1, cachePath = tmpdir)
  })
  expect_false(attr(b, ".Cache")$newCache)
  expect_true(sum(grepl("Changing format of Cache entry from rds to qs", mess)) == 1)

  options("reproducible.cacheSaveFormat" = "rds")
  mess <- capture_messages({
    b <- Cache(rnorm, 1, cachePath = tmpdir)
  })
  expect_false(attr(b, ".Cache")$newCache)
  expect_true(sum(grepl("Changing format of Cache entry from qs to rds", mess)) == 1)
})

test_that("test file link with duplicate Cache", {
  testInit(verbose = TRUE, opts = list("reproducible.useMemoise" = FALSE))

  sam <- function(...) {
    sample(...)
  }

  sam1 <- function(...) {
    1
    sample(...)
  }
  N <- 4e5

  set.seed(123)
  mess1 <- capture_messages({
    b <- Cache(sam, N, cachePath = tmpCache)
  })

  # Change in RSQLite 2.2.2 -- there is now a random number used in dbAppend,
  #   so this test no longer works after the second time -- running it a 3rd time
  #   is sufficient for the test. The point is, if it is an identical result,
  #   then there will be a file.link
  set.seed(123)
  mess2 <- capture_messages({
    d <- Cache(sample, N, cachePath = tmpCache)
  })

  set.seed(123)
  mess3 <- capture_messages({
    g <- Cache(sam1, N, cachePath = tmpCache)
  })

  expect_true(grepl("A file with identical", mess3))

  set.seed(123)
  mess1 <- capture_messages({
    b <- Cache(sam, N, cachePath = tmpCache)
  })
  set.seed(123)
  # Because of RSQLite 2.2.2 this 2nd time is not considered identical -- need 3rd time
  mess2 <- capture_messages({
    d <- Cache(sample, N, cachePath = tmpCache)
  })
  clearCache(tmpCache, userTags = gsub("cacheId:", "", attr(b, "tags")))
  set.seed(123)
  mess2 <- capture_messages({
    d <- Cache(sam1, N, cachePath = tmpCache)
  })
  expect_true(any(grepl("loaded cached", mess2)))
  expect_true(any(grepl("loaded cached", mess1)))
  # There are intermittent "status 5" warnings on next line on Windows -- not relevant here
  warns <- capture_warnings({
    out1 <- try(system2("du", paste0("\"", tmpCache, "\""), stdout = TRUE), silent = TRUE)
  })
  # out1 <- try(system2("du", tmpCache, stdout = TRUE), silent = TRUE)
  if (!is(out1, "try-error")) {
    fs1 <- as.numeric(gsub("([[:digit:]]*).*", "\\1", out1))
  }

  # It must be same output, not same input
  clearCache(tmpCache)
  set.seed(123)
  mess1 <- capture_messages({
    b <- Cache(sam, N, cachePath = tmpCache)
  })
  set.seed(1234)
  mess2 <- capture_messages({
    d <- Cache(sample, N, cachePath = tmpCache)
  })
  # Different inputs AND different output -- so no cache recovery and no file link
  expect_true(length(mess2) == 0)
  out2 <- try(system2("du", tmpCache, stdout = TRUE), silent = TRUE)
  if (!is(out2, "try-error")) {
    fs2 <- as.numeric(gsub("([[:digit:]]*).*", "\\1", out2))
    expect_true(all(fs1 * 1.9 < fs2))
  }
  # Test if the `try` works if the file.link is not to a meaningful file
  cacheIds <- unique(showCache(tmpCache)[[.cacheTableHashColName()]])
  unlink(dir(CacheStorageDir(tmpCache), pattern = cacheIds[1], full.names = TRUE))
  unlink(dir(CacheStorageDir(tmpCache), pattern = cacheIds[2], full.names = TRUE))
  set.seed(1234)

  warn <- capture_warnings({
    d1 <- Cache(sam1, N, cachePath = tmpCache)
  })
  expect_true(length(warn) == 0)
})

test_that("test .object arg for list in Cache", {
  testInit()
  opts <- options(reproducible.cachePath = tmpdir)
  on.exit(
    {
      options(opts)
    },
    add = TRUE
  )
  l <- list(a = 1, b = 2, f = 3)
  out1 <- Cache(unlist, l, .objects = "a")
  out2 <- Cache(unlist, l, .objects = "b")
  out3 <- Cache(unlist, l)
  out4 <- Cache(unlist, l, .objects = "c") # not in list, so cache empty list
  out5 <- Cache(unlist, l, .objects = "d")
  out6 <- Cache(unlist, l, .objects = c("a", "b", "f"))
  expect_false(identical(out1, out2))
  expect_false(identical(out1, out3))
  expect_false(identical(out2, out3))
  expect_true(sum(out4 - out5) == 0)
  expect_true(sum(out3 - out6) == 0)

  l <- list(a = 1, b = 2, f = 3, g = 4) # change list
  out7 <- Cache(unlist, l, .objects = c("a", "b", "f")) # subset should still be same as prev whole list
  expect_true(sum(out3 - out7) == 0)

  out1 <- .robustDigest(l, .objects = "a")
  out2 <- .robustDigest(l, .objects = "b")
  out3 <- .robustDigest(l)
  expect_false(identical(out1, out2))
  expect_false(identical(out1, out3))
  expect_false(identical(out2, out3))
})

test_that("quick arg in Cache as character", {
  skip_on_cran()
  testInit("terra", verbose = TRUE, tmpFileExt = c("rds", "tif"))

  tf <- tmpfile[[1]]
  tf2 <- tmpfile[[2]]

  messes <- list()
  quicks <- rep(list(FALSE, FALSE, "file", "file", TRUE, TRUE), 2)
  rasRan <- c(rep(TRUE, 6), rep(FALSE, 6))

  ##     quicks ranRas
  ## 1   FALSE   TRUE
  ## 2   FALSE   TRUE
  ## 3    file   TRUE
  ## 4    file   TRUE
  ## 5    TRUE   TRUE
  ## 6    TRUE   TRUE
  ## 7   FALSE  FALSE
  ## 8   FALSE  FALSE
  ## 9    file  FALSE
  ## 10   file  FALSE
  ## 11   TRUE  FALSE
  ## 12   TRUE  FALSE

  for (i in seq(quicks)) {
    vals <- if (rasRan[i]) sample(1:100) else 1:100
    ranRas <- terra::rast(terra::ext(0, 10, 0, 10), vals = vals)
    ranRas <- suppressWarningsSpecific(
      falseWarnings = proj6Warn,
      writeRaster(ranRas, filename = tf2, overwrite = TRUE)
    )
    a <- sample(1e7, 1)
    saveRDS(a, file = tf)

    # new copy
    messes[[i]] <- capture_messages(Cache(saveRDS, ranRas,
      file = tf, cachePath = tmpCache,
      quick = quicks[[i]]
    ))
  }

  expect_true(length(messes[[6]]) > 0) # undesirable: quick = TRUE -- raster has changed
  expect_true(length(messes[[8]]) == 0) # undesirable: quick = FALSE -- raster & file not changed

  ## Desired: 9 not cache, 10 cached. But 9 is picking up cache from 4.
  expect_true(length(messes[[9]]) == 0) # undesirable: quick = 'file' -- raster & file changed
  expect_true(length(messes[[10]]) > 0) # undesirable: quick = 'file -- raster & file not changed
  expect_true(sum(unlist(lapply(messes, function(x) length(x) > 0))) == 4L)
})

test_that("List of Rasters", {
  testInit("terra", tmpFileExt = c("tif", "tif"))

  listOfRas <- lapply(1:2, function(x) {
    vals <- sample(1:100)
    ranRas <- terra::rast(terra::ext(0, 10, 0, 10), vals = vals)
    ranRas <- suppressWarningsSpecific(
      falseWarnings = proj6Warn,
      writeRaster(ranRas, filename = tmpfile[[x]], overwrite = TRUE)
    )
  })

  writeRasterList <- function(rasList) {
    lapply(rasList, function(ras) {
      filename <- paste0(Filenames(ras), rndstr(1, 6), ".tif")
      ras[] <- ras[]
      ranRas <- suppressWarningsSpecific(
        falseWarnings = proj6Warn,
        writeRaster(ras, filename = filename, overwrite = FALSE)
      )
    })
  }
  a <- Cache(writeRasterList, listOfRas, cachePath = tmpCache)
  b <- Cache(writeRasterList, listOfRas, cachePath = tmpCache)
  expect_false(isTRUE(attr(b, ".Cache")$newCache))
  expect_true(isTRUE(attr(a, ".Cache")$newCache))
})

test_that("Cache the dots; .cacheExtra", {
  testInit()

  fn1 <- function(a, b, ...) {
    out <- fn2(...)
    return(out)
  }

  fn2 <- function(d, e = 0, f = 1) {
    rnorm(d, e, f)
  }

  suppressMessages({
    out1 <- Cache(fn1, a = 1, b = 2, d = 1, cachePath = tmpCache)
  })
  suppressMessages({
    out2 <- Cache(fn1, a = 1, b = 2, d = 2, cachePath = tmpCache)
  })
  expect_true(!identical(out1, out2))


  fn3 <- function(a, b) {
    out <- fn2(d = 1)
  }

  suppressMessages({
    out3 <- Cache(fn3, a = 1, b = 2, .cacheExtra = "12342", cachePath = tmpCache)
  })
  suppressMessages({
    out4 <- Cache(fn3, a = 1, b = 2, .cacheExtra = "123422", cachePath = tmpCache)
  })
  expect_true(!identical(out3, out4))

  # These are now the same because the .cacheExtra is the same and the arg is ignored
  suppressMessages({
    out5 <- Cache(mean, 6, omitArgs = "x", .cacheExtra = "234", cachePath = tmpCache)
  })
  suppressMessages({
    out6 <- Cache(mean, 7, omitArgs = "x", .cacheExtra = "234", cachePath = tmpCache)
  })
  expect_true(out6 - 6 == 0) # takes first one
  expect_equivalent(out5, out6) # the attributes will be different because one is a recovery of the other
})

test_that("change to new capturing of FUN & base pipe", {
  testInit(opts = list(reproducible.verbose = 5))
  skip_if(getRversion() < "4.2.0")

  Nrand2 <- Nrand <- 1e6
  mess0 <- capture_messages(
    out0 <- Cache(rnorm(1, 2, round(mean(runif(Nrand, 1, 1.1)))), cachePath = tmpCache)
  )

  mess1 <- capture_messages(
    out1 <- Cache(do.call(rnorm, list(1, 2, sd = round(mean(runif(Nrand2, 1, 1.1))))),
      cachePath = tmpCache
    )
  )

  # NO LONGER THE SAME CALL AS ABOVE
  f1 <- paste("
      {runif(1e6, 1, 1.1) |>
        mean() |>
        round() |>
        rnorm(1, 2, sd = _)} |> # _ Only works with R >= 4.2.0
        Cache(cachePath = tmpCache)
    ")
  mess2 <- capture_messages(
    out2 <- eval(parse(text = f1))
  )
  f2 <- paste("out3 <- {runif(Nrand, 1, 1.1) |>
        mean() |>
        round() |>
        rnorm(1, 2, sd = _)} |> # _ Only works with R >= 4.2.0
        # (function(xx) rnorm(1, 2, sd = xx))() |>
        Cache(cachePath = tmpCache)
    ")
  mess3 <- capture_messages(
    eval(parse(text = f2))
  )
  expect_true(attr(out0, ".Cache")$newCache)
  expect_false(attr(out1, ".Cache")$newCache)
  expect_true(attr(out2, ".Cache")$newCache)
  expect_false(attr(out3, ".Cache")$newCache)

  expect_true(length(grep("\\<sd\\>", mess0)) == 1) # digests just the 1
  expect_true(length(grep("\\<sd\\>", mess1)) == 1) # digests just the 1
  expect_true(length(grep("\\<sd\\>", mess2)) == 6) # digests each element
  expect_true(length(grep("\\<sd\\>", mess3)) == 6) # digests each element

  clearCache(tmpCache)
  for (i in 1:3) Cache(rnorm, i, cachePath = tmpCache)
  expect_true(length(unique(showCache(tmpCache)$cacheId)) == 3)
  # This would make sense it if only generates one Cache entry... i.e., do not evaluate the sample

  clearCache(tmpCache)


  for (i in 1:3) {
    sss <- paste("sample(100000000, ", i, ") |>  # creates 1 random number
      rnorm(1, 2, sd = _) |>  # passed to sd of rnorm
      Cache(cachePath = tmpCache)")
    eval(parse(text = sss))
  }
  sc <- data.table::copy(showCache(tmpCache))
  expect_true(length(unique(sc$cacheId)) == 3)

  # This, different way; not evaluating `sample`; so this is same as prev
  for (i in 1:3) Cache(rnorm(1, 2, sample(10000000, i)), cachePath = tmpCache)
  sc1 <- showCache(tmpCache)
  expect_true(length(unique(sc1$cacheId)) == 6)
  expect_true(NROW(sc1) > NROW(sc))


  # Try with squiggly braces
  out0 <- Cache(rnorm(1, 2, round(mean(runif(Nrand, 1, 1.1)))), cachePath = tmpCache)
  f1 <- paste("
      {runif(Nrand, 1, 1.1) |>
        mean() |>
        round() |>
        rnorm(1, 2, sd = _)} |> # _ Only works with R >= 4.2.0
        Cache(cachePath = tmpCache)
    ")
  mn <- 1
  st3 <- system.time(out2 <- eval(parse(text = f1)))
  st4 <- system.time(out3 <- Cache(
    {
      rnorm(1, 2, round(mean(runif(Nrand, 1, 1.1))))
    },
    cachePath = tmpCache
  ))
  # can pass a variable, but not a function
  st5 <- system.time(out3 <- Cache(
    {
      rnorm(1, 2, round(mean(runif(Nrand, mn, 1.1))))
    },
    cachePath = tmpCache
  ))
  f1 <- paste("
      { a <- runif(Nrand, 1, 1.1)
        b <- mean(a)
        d <- round(b)
        rnorm(1, 2, sd = d)} |> # _ Only works with R >= 4.2.0
        Cache(cachePath = tmpCache)
    ")
  err <- capture_error(out2 <- eval(parse(text = f1)))
  expect_true(is(err, "simpleError"))

  # Test for new `round` in R > 4.3.1 with ... i.e., a primitive with method dispatch
  f1 <- paste("
      {runif(1e6, 1, 1.1) |>
        mean() |>
        round()} |> # _ Only works with R >= 4.2.0
        Cache(cachePath = tmpCache)
    ")
  expect_no_error(mess2 <- capture_messages(
    out2 <- eval(parse(text = f1))
  ))
})

test_that("test cache with new approach to match.call", {
  testInit(opts = list("reproducible.verbose" = -2))
  opts <- options(reproducible.cachePath = tmpdir)
  on.exit(
    {
      options(opts)
    },
    add = TRUE
  )

  b <- list(fun = rnorm)
  a <- list()
  clearCache(ask = FALSE)

  bbb <- 1
  ee <- new.env(parent = emptyenv())
  ee$qq <- bbb
  a[[1]] <- Cache(rnorm(1)) # no evaluation prior to Cache
  a[[2]] <- Cache(rnorm, 1) # no evaluation prior to Cache
  a[[3]] <- Cache(do.call, rnorm, list(1))
  a[[4]] <- Cache(do.call(rnorm, list(1)))
  a[[5]] <- Cache(do.call(b$fun, list(1)))
  a[[6]] <- Cache(do.call, b$fun, list(1))
  a[[7]] <- Cache(b$fun, 1)
  a[[8]] <- Cache(b$fun(1))
  a[[9]] <- Cache(quote(rnorm(1)))
  a[[10]] <- Cache(stats::rnorm(1))
  a[[11]] <- Cache(stats::rnorm, 1)
  a[[12]] <- Cache(rnorm(1, 0, get("bbb", inherits = FALSE)))
  a[[13]] <- Cache(rnorm(1, 0, get("qq", inherits = FALSE, envir = ee)))
  a[[14]] <- Cache(rnorm(1, bbb - bbb, get("bbb", inherits = FALSE)))
  a[[15]] <- Cache(rnorm(sd = 1, 0, n = get("bbb", inherits = FALSE))) # change order
  a[[16]] <- Cache(rnorm(1, sd = get("ee", inherits = FALSE)$qq), mean = 0)
  if (isTRUE(getRversion() >= "4.1.0")) {
    a[[17]] <- eval(parse(text = "b$fun(1) |> Cache()"))
  }
  if (isTRUE(getRversion() >= "4.2.0")) {
    ss <- '{"bbb" |>
      parse(text = _) |>
      eval() |>
      rnorm()} |>
    Cache()'
    a[[18]] <- eval(parse(text = ss))
  }
  expect_true(identical(attr(a[[1]], ".Cache")$newCache, TRUE))
  for (i in 2:NROW(a)) {
    expect_true(identical(attr(a[[i]], ".Cache")$newCache, FALSE))
  }

  for (fun in list(.robustDigest, print)) {
    clearCache(ask = FALSE)
    b <- list(fun = fun)
    a <- list()
    lala <- capture.output(a[[1]] <- Cache(fun(1)))
    a[[2]] <- Cache(fun, 1)
    a[[3]] <- Cache(do.call, fun, list(1))
    a[[4]] <- Cache(do.call(fun, list(1)))
    a[[5]] <- Cache(do.call(b$fun, list(1)))
    a[[6]] <- Cache(do.call, b$fun, list(1))
    a[[7]] <- Cache(b$fun, 1)
    a[[8]] <- Cache(b$fun(1))
    a[[9]] <- Cache(quote(fun(1)))
    expect_true(identical(attr(a[[1]], ".Cache")$newCache, TRUE))

    if (isTRUE(getRversion() >= "4.1.0")) {
      a[[9]] <- eval(parse(text = "b$fun(1) |> Cache()"))
    }

    for (i in 2:NROW(a)) {
      test <- identical(attr(a[[i]], ".Cache")$newCache, FALSE)
      if (isFALSE(test)) browser()
      expect_true(test)
    }
  }

  # This tries to do a method that is not actually exported from a package; the generic (sf::st_make_valid) is
  if (.requireNamespace("sf")) {
    clearCache(ask = FALSE)
    b <- list(fun = fun)
    a <- list()

    p1 <- sf::st_as_sfc("POLYGON((0 0, 0 10, 10 0, 10 10, 0 0))")
    a[[1]] <- Cache(sf::st_make_valid(p1)) # not
    a[[2]] <- Cache(sf::st_make_valid, p1) # not
    a[[3]] <- Cache(quote(sf::st_make_valid(p1))) # not
    library(sf)
    # on.exit({
    #   try(detach("package:sf", unload = TRUE), silent = TRUE)
    # }, add = TRUE)
    a[[4]] <- Cache(st_make_valid(p1)) # not
    ff <- sf::st_make_valid
    a[[5]] <- Cache(ff(p1))

    expect_true(identical(attr(a[[1]], ".Cache")$newCache, TRUE))
    for (i in 2:length(a)) {
      test <- identical(attr(a[[i]], ".Cache")$newCache, FALSE)
      if (isFALSE(test)) browser()
      expect_true(test)
    }
  }
})

test_that("test cache; new approach to match.call, postProcess", {
  skip_if_not_installed("DBI") # sf needs DBI
  testInit(c("terra", "sf"),
    tmpFileExt = c(".tif", ".tif"),
    opts = list(
      "rasterTmpDir" = tempdir2(rndstr(1, 6)),
      "reproducible.inputPaths" = NULL,
      "reproducible.overwrite" = TRUE
    )
  )
  on.exit(
    {
      options(opts)
    },
    add = TRUE
  )

  opts <- options("reproducible.cachePath" = tmpdir)

  # Add a study area to Crop and Mask to
  # Create a "study area"
  coords <- structure(c(-122.98, -116.1, -99.2, -106, -122.98, 59.9, 65.73, 63.58, 54.79, 59.9),
    .Dim = c(5L, 2L)
  )
  coords2 <- structure(c(-115.98, -116.1, -99.2, -106, -122.98, 59.9, 65.73, 63.58, 54.79, 59.9),
    .Dim = c(5L, 2L)
  )

  StudyArea <- terra::vect(coords, "polygons")
  terra::crs(StudyArea) <- crsToUse

  # Sr1 <- Polygon(coords)
  # Srs1 <- Polygons(list(Sr1), "s1")
  # StudyArea <- SpatialPolygons(list(Srs1), 1L)
  # crs(StudyArea) <- crsToUse

  # Sr1 <- Polygon(coords2)
  # Srs1 <- Polygons(list(Sr1), "s1")
  # StudyArea2 <- SpatialPolygons(list(Srs1), 1L)
  # crs(StudyArea2) <- crsToUse

  StudyArea2 <- terra::vect(coords2, "polygons")
  terra::crs(StudyArea2) <- crsToUse

  nonLatLongProj <- paste(
    "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
    "+x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"
  )
  nc <- sf::st_as_sf(StudyArea) # system.file("shape/nc.shp", package="sf"))
  nc1 <- sf::st_transform(nc, nonLatLongProj)
  ncSmall <- sf::st_as_sf(StudyArea2)
  ncSmall <- sf::st_transform(ncSmall, nonLatLongProj)
  ncSmall <- sf::st_buffer(ncSmall, dist = -10000)

  # S3 classes
  b1 <- Cache(postProcess(nc1, studyArea = ncSmall, filename2 = NULL))
  expect_true(identical(attr(b1, ".Cache")$newCache, TRUE))
  b2 <- Cache(postProcess(nc1, studyArea = ncSmall, filename2 = NULL))
  expect_true(identical(attr(b2, ".Cache")$newCache, FALSE))
  b3 <- Cache(postProcess, nc1, studyArea = ncSmall, filename2 = NULL)
  expect_true(identical(attr(b3, ".Cache")$newCache, FALSE))

  # S4 classes
  c1 <- Cache(.robustDigest(data.frame(a = 1)))
  c2 <- Cache(.robustDigest(data.frame(a = 1)))
  c3 <- Cache(.robustDigest(data.frame(a = 1)), verbose = -2) # add a non-relevant arg
  c4 <- Cache(.robustDigest, data.frame(a = 1))
  c5 <- Cache(.robustDigest, data.frame(a = 1), verbose = -2)

  expect_true(identical(attr(c1, ".Cache")$newCache, TRUE))
  expect_true(identical(attr(c2, ".Cache")$newCache, FALSE))
  expect_true(identical(attr(c3, ".Cache")$newCache, FALSE))
  expect_true(identical(attr(c4, ".Cache")$newCache, FALSE))
  expect_true(identical(attr(c5, ".Cache")$newCache, FALSE))

  # S4 classes
  d1 <- Cache(Checksums(tmpdir, write = FALSE))
  d2 <- Cache(Checksums(tmpdir, write = FALSE))
  expect_true(identical(attr(c1, ".Cache")$newCache, TRUE))
  expect_true(identical(attr(c2, ".Cache")$newCache, FALSE))
})

test_that("test cache; SpatRaster attributes", {
  testInit(c("terra", "sf"),
    tmpFileExt = c(".tif", ".tif"),
    opts = list(
      "rasterTmpDir" = tempdir2(rndstr(1, 6)),
      "reproducible.inputPaths" = NULL,
      "reproducible.overwrite" = TRUE
    ), needInternet = TRUE
  )

  options("reproducible.cachePath" = tmpdir)
  dPath <- file.path(tmpdir, "inputs")

  targetFile <- "rasterTest.tif"
  url <- "https://github.com/tati-micheletti/host/raw/master/data/rasterTest.tif"

  testFun <- function(url, targetFile) {
    ras <- prepInputs(
      url = url,
      targetFile = targetFile
    )
    pixIDs <- which(as.vector(ras[]) == 1)
    attr(ras, "pixIDs") <- pixIDs
    ras
  }

  ras <- Cache(testFun,
    url = url,
    targetFile = targetFile
  )
  expect_true(is.integer(attr(x = ras, "pixIDs")))

  ## re-run. attributes still there?
  ras <- Cache(testFun,
    url = url,
    targetFile = targetFile
  )
  expect_true(is.integer(attr(x = ras, "pixIDs")))
})

test_that("Issue 316 - writeOutputs in a non getwd dir", {
  testInit(c("terra"), tmpFileExt = c(".tif", ".tif"))

  cPath <- file.path(tmpdir, "cache")

  withr::local_options(
    "reproducible.cachePath" = cPath,
    "reproducible.destinationPath" = tmpdir
  )

  cacheTags <- "test"
  studyAreaName <- "test"

  f <- system.file("ex/elev.tif", package = "terra")
  rasterToMatch <- terra::rast(f)

  RTMvals <- as.vector(rasterToMatch[])
  rasterToMatch[!is.na(RTMvals)] <- 1

  rasterToMatchLarge <- list()
  for (i in 1:2) {
    rasterToMatchLarge[[i]] <- Cache(writeOutputs, rasterToMatch,
      filename2 = .suffix(
        file.path(tmpdir, "rasterToMatchLarge.tif"),
        paste0("_", studyAreaName)
      ), datatype = "INT2U",
      overwrite = TRUE, userTags = c(cacheTags, "rasterToMatchLarge"),
      quick = "filename2",
      omitArgs = c("userTags")
    )
  }

  expect_equivalent(rasterToMatchLarge[[1]], rasterToMatchLarge[[2]])
})

test_that("test useDBI TRUE <--> FALSE", {
  testInit(verbose = TRUE)
  on.exit(
    {
      useDBI(orig)
    },
    add = TRUE
  )
  options(reproducible.cachePath = tmpdir)
  orig <- useDBI()
  useDBI(TRUE)
  d <- b <- a <- list()
  b[[1]] <- Cache(rnorm(1))
  b[[2]] <- Cache(rnorm(2))
  b[[3]] <- Cache(runif(3))
  useDBI(FALSE)
  a[[1]] <- Cache(rnorm(1))
  a[[2]] <- Cache(rnorm(2))
  a[[3]] <- Cache(runif(3))
  useDBI(TRUE)
  d[[1]] <- Cache(rnorm(1))
  d[[2]] <- Cache(rnorm(2))
  d[[3]] <- Cache(runif(3))
  lapply(a, function(aa) expect_false(attr(aa, ".Cache")$newCache))
  lapply(b, function(aa) expect_true(attr(aa, ".Cache")$newCache))
  lapply(d, function(aa) expect_false(attr(aa, ".Cache")$newCache))
})


test_that("lightweight tests for preProcess code coverage", {
  skip_on_cran()
  out <- testInit(verbose = TRUE)

  opts <- options(reproducible.cachePath = tmpdir)

  on.exit(
    {
      options(opts)
    },
    add = TRUE
  )
  expect_error(Cache(), "requires")
  expect_message(Cache(compareRasterFileLength = TRUE, rnorm(1)), regexp = "compareRasterFileLength")
  expect_message(Cache(sideEffect = TRUE, rnorm(1)), regexp = "sideEffect")
  expect_error(
    Cache(cachePath = tmpCache, conn = list(tmpCache, tmpdir), rnorm(1)),
    "different lengths"
  )
})



test_that("terra files were creating file.link", {
  testInit("terra",
           tmpFileExt = c(".tif", ".tif"),
           opts = list(reproducible.useMemoise = FALSE)
  )
  rasts <- lapply(1:2, function(x)
    ras1 <- terra::rast(nrows = 1e3, ncols = 1e3, vals = sample(1e6),
                        resolution = 1, xmin = 0, xmax = 1000, ymin = 0, ymax = 1000)
  )
  func <- function(ras, fn) {
    ras <- Cache(writeRaster(ras, filename = fn), quick = c("fn"))
    ras
  }
  mess <- capture_messages(Map(f = func, fn = tmpfile, ras = rasts))
  expect_false(any(grepl("file.link", mess)))

})

test_that("pass NA to userTags", {
  testInit(verbose = FALSE)
  expect_no_error(a <- Cache(rnorm(1), userTags = c("NA", "hi")))
})
