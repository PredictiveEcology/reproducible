options(reproducible.drv = "csv")

test_that("test file-backed raster caching", {
  testInitOut <- testInit("terra", tmpFileExt = c(".tif", ".grd"))

  on.exit({
    testOnExit(testInitOut)
    # options(opts1)
  }, add = TRUE)

  nOT <- Sys.time()

  randomPolyToDisk <- function(tmpfile) {
    r <- rast(ext(0, 10, 0, 10), vals = sample(1:30, size = 100, replace = TRUE))
    .writeRaster(r, tmpfile[1], overwrite = TRUE)
    r <- rast(tmpfile[1])
    r
  }

  a <- randomPolyToDisk(tmpfile[1])
  # confirm that the raster has the given tmp filename
  expect_identical(tmpfile[1], Filenames(a)) # both have had a checkPath already

  val1 <- .cacheNumDefaultTags()  # adding a userTag here
  ik <- .ignoreTagKeys()
  aa <- Cache(randomPolyToDisk(tmpfile[1]), cacheRepo = tmpCache, userTags = "something2")

  # Test clearCache by tags
  expect_equal(NROW(showCache(tmpCache)[!tagKey %in% .ignoreTagKeys()]), val1)
  clearCache(tmpCache, userTags = "something$", ask = FALSE)
  expect_equal(NROW(showCache(tmpCache)[!tagKey %in% .ignoreTagKeys()]), val1)
  clearCache(tmpCache, userTags = "something2", ask = FALSE)
  expect_equal(NROW(showCache(tmpCache)), 0)

  aa <- Cache(randomPolyToDisk, tmpfile[1], cacheRepo = tmpCache, userTags = "something2")
  expect_equal(NROW(showCache(tmpCache)[!tagKey %in% .ignoreTagKeys()]), val1)
  clearCache(tmpCache, userTags = c("something$", "testing$"), ask = FALSE)
  expect_equal(NROW(showCache(tmpCache)[!tagKey %in% .ignoreTagKeys()]), val1)
  clearCache(tmpCache, userTags = c("something2$", "testing$"), ask = FALSE)
  expect_equal(NROW(showCache(tmpCache)[!tagKey %in% .ignoreTagKeys()]), val1)

  clearCache(tmpCache, userTags = c("something2$", "randomPolyToDisk$"), ask = FALSE)
  expect_equal(NROW(showCache(tmpCache)), 0)

  aa <- Cache(randomPolyToDisk, tmpfile[1], cacheRepo = tmpCache, userTags = "something2")

  # ~~confirm that the raster has the new filename in the cachePath ~~
  #  CHANGED BEHAVIOUR -- Cached File-backed object no longer has that the Cached filename
  expect_false(identical(normPath(Filenames(aa)),
                         normPath(CacheStoredRasterFile(tmpCache, filename = tmpfile[1]))))

  clearCache(x = tmpCache)
  bb <- Cache(randomPolyToDisk, tmpfile[1], cacheRepo = tmpCache, userTags = "something2",
              quick = TRUE)
  #bb <- Cache(randomPolyToDisk, tmpfile[1], cacheRepo = tmpdir, userTags = "something2")
  #clearCache(x = tmpdir)
  clearCache(x = tmpdir)
  try(unlink(CacheDBFile(tmpdir)), silent =  TRUE)
  try(unlink(CacheStorageDir(tmpdir), recursive = TRUE), silent =  TRUE)
  froms <- normPath(dir(tmpCache, recursive = TRUE, full.names = TRUE))
  checkPath(CacheStorageRasterDir(tmpdir), create = TRUE)
  checkPath(CacheStorageDir(tmpdir), create = TRUE)
  file.copy(from = froms, overwrite = TRUE,
            to = gsub(normPath(tmpCache), normPath(tmpdir), froms))
  # movedCache(tmpdir)
  # ._prepareOutputs_1 <<- ._prepareOutputs_2 <<- ._getFromRepo <<- 1
  # Will silently update the filename of the RasterLayer, and recover it
  type <- gsub("Connection", "", class(getOption("reproducible.conn")))
  isSQLite <- grepl(type, "NULL")
  if (!isSQLite) {
    warn1 <- capture_warnings(movedCache(tmpdir, old = tmpCache))
  }

  warn <- capture_warnings({
    bb <- Cache(randomPolyToDisk, tmpfile[1], cacheRepo = tmpdir, userTags = "something2",
                quick = TRUE)
  })

  expect_false(attr(bb, ".Cache")$newCache)
  expect_true(file.exists(Filenames(bb)))
  expect_silent(bb[] <- bb[])

  # Delete the old everything to make sure previous didn't succeed because old pointer
  clearCache(x = tmpCache)
  try(unlink(CacheDBFile(tmpCache)), silent =  TRUE)
  try(unlink(CacheStorageDir(tmpCache), recursive = TRUE), silent =  TRUE)

  # ._Cache_6 <<- 1
  bb <- Cache(randomPolyToDisk, tmpfile[1], cacheRepo = tmpdir, userTags = "something2",
              quick = TRUE)
  expect_false(attr(bb, ".Cache")$newCache)
  expect_true(file.exists(Filenames(bb)))
  expect_silent(bb[] <- bb[])

  ###############
  clearCache(tmpCache) # no longer a Cache repo
  clearCache(tmpdir)
  cc <- Cache(randomPolyToDisk, tmpfile[2], cacheRepo = tmpCache, userTags = "something2",
              quick = TRUE)
  bb <- Cache(randomPolyToDisk, tmpfile[1], cacheRepo = tmpCache, userTags = "something2",
              quick = TRUE)
  try(movedCache(tmpdir, tmpCache), silent = TRUE)

  ######
  bbS <- raster::stack(bb, cc)
  fn2 <- function(stk) {
    stk
  }

  out <- Cache(fn2, bbS, cacheRepo = tmpCache, userTags = "something2")
  tmpdir2 <- file.path(tmpdir, "newCache")

  # Behaviour of Rasters within Cache has changed ... now, the Filenames(ras) returns
  #   the non-cache directory -- so this test doesn't work anymore
  froms <- normPath(dir(tmpCache, recursive = TRUE, full.names = TRUE))
  checkPath(CacheStorageRasterDir(tmpdir2), create = TRUE)
  checkPath(CacheStorageDir(tmpdir2), create = TRUE)
  file.copy(from = froms, overwrite = TRUE,
            to = gsub(normPath(tmpCache), normPath(tmpdir2), froms))
  if (is(getOption("reproducible.drv"), "PqDriver")) {
    DBI::dbRemoveTable(getOption("reproducible.conn"), CacheDBTableName(tmpdir))
  }
  movedCache(tmpdir2, tmpCache)
  out <- Cache(fn2, bbS, cacheRepo = tmpdir2, userTags = "something2")
  expect_false(attr(out, ".Cache")$newCache) # because it used moved cache

  clearCache(tmpdir)
  clearCache(tmpCache)

  ### Test for 2 caching events with same file-backing name
  randomPolyToDisk2 <- function(tmpfile, rand) {
    r <- raster(extent(0, 10, 0, 10), vals = sample(1:30, size = 100, replace = TRUE))
    .writeRaster(r, tmpfile[1], overwrite = TRUE)
    r <- raster(tmpfile[1])
    r
  }

  a <- Cache(randomPolyToDisk2, tmpfile[1], runif(1))
  b <- Cache(randomPolyToDisk2, tmpfile[1], runif(1))

  # changed behaviour as of reproducible 1.2.0.9020
  #  -- now Cache doesn't protect user from filename collisions if user makes them
  # rechanged behaviour -- as of reproducible 1.2.9 -- using harlinks
  expect_true(identical(Filenames(a), Filenames(b)))

  # Caching a raster as an input works
  rasterTobinary <- function(raster) {
    ceiling(raster[] / (mean(raster[]) + 1))
  }
  nOT <- Sys.time()

  for (i in 1:2) {
    strt <- Sys.time()
    assign(paste0("a", i), Cache(rasterTobinary, a, cacheRepo = tmpCache, notOlderThan = nOT))
    fin <- Sys.time()
    assign(paste0("b", i), fin - strt)
    nOT <- Sys.time() - 100
  }

  attr(a1, ".Cache")$newCache <- NULL
  attr(a2, ".Cache")$newCache <- NULL
  # test that they are identical
  expect_equal(a1, a2)

  # confirm that the second one was obtained through reading from Cache... much faster than writing
  expect_true(b1[1] > b2[1])

  clearCache(tmpCache, ask = FALSE)

  # Check that Caching of rasters saves them to tif file instead of rdata
  randomPolyToMemory <- function() {
    r <- raster(extent(0, 10, 0, 10), vals = sample(1:30, size = 100, replace = TRUE))
    dataType(r) <- "INT1U"
    r
  }

  bb <- Cache(randomPolyToMemory, cacheRepo = tmpdir)
  expect_true(filename(bb) == "")
  expect_true(inMemory(bb))

  bb <- Cache(randomPolyToMemory, cacheRepo = tmpdir)
  expect_true(NROW(showCache(tmpdir)[!tagKey %in% .ignoreTagKeys()]) == .cacheNumDefaultTags() - 1)

  # Test that factors are saved correctly
  randomPolyToFactorInMemory <- function() {
    r <- raster(extent(0, 10, 0, 10), vals = sample(1:30, size = 100, replace = TRUE))
    levels(r) <- data.frame(ID = 1:30, vals = sample(LETTERS[1:5], size = 30, replace = TRUE),
                            vals2 = sample(1:7, size = 30, replace = TRUE))
    dataType(r) <- "INT1U"
    r
  }
  bb <- Cache(randomPolyToFactorInMemory, cacheRepo = tmpdir)
  expect_equal(dataType(bb), "INT1U")
  expect_true(raster::is.factor(bb))
  expect_true(is(raster::levels(bb)[[1]], "data.frame"))
  expect_true(NCOL(raster::levels(bb)[[1]]) == 3)
  expect_true(NROW(raster::levels(bb)[[1]]) == 30)

  randomPolyToFactorOnDisk <- function(tmpfile) {
    r <- raster(extent(0, 10, 0, 10), vals = sample(1:30, size = 100, replace = TRUE))
    levels(r) <- data.frame(ID = 1:30, vals = sample(LETTERS[1:5], size = 30, replace = TRUE),
                            vals2 = sample(1:7, size = 30, replace = TRUE))
    r <- .writeRaster(r, tmpfile, overwrite = TRUE, datatype = "INT1U")
    r
  }

  # bb1 has original tmp filename
  bb1 <- randomPolyToFactorOnDisk(tmpfile[2])
  # bb has new one, inside of cache repository, with same basename
  bb <- Cache(randomPolyToFactorOnDisk, tmpfile = tmpfile[2], cacheRepo = tmpdir)
  # changed behaviour as of reproducible 1.2.0.9020 -- now Cache doesn't protect user from filename collisions if user makes them
  expect_false(dirname(Filenames(bb, allowMultiple = FALSE)) == CacheStorageRasterDir(tmpdir))
  expect_true(identical(basename(Filenames(bb, allowMultiple = FALSE)), basename(tmpfile[2])))
  expect_true(identical(normPath(Filenames(bb, allowMultiple = FALSE)), tmpfile[2]))
  expect_true(dirname(filename(bb1)) == dirname(tmpfile[2]))
  expect_true(basename(filename(bb1)) == basename(tmpfile[2]))
  expect_true(dataType(bb) == "INT1U")
  expect_true(raster::is.factor(bb))
  expect_true(is(raster::levels(bb)[[1]], "data.frame"))
  expect_true(NCOL(raster::levels(bb)[[1]]) == 3)
  expect_true(NROW(raster::levels(bb)[[1]]) == 30)

  clearCache(tmpdir, ask = FALSE)
  #})
})

test_that("test memory backed raster robustDigest", {
  testInitOut <- testInit("raster", tmpFileExt = c("",""))
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  set.seed(123)
  r1 <- raster(extent(0, 10, 0, 10), vals = sample(1:30, size = 100, replace = TRUE))
  r2 <- raster(extent(0, 10, 0, 10), vals = sample(1:30, size = 100, replace = TRUE))
  set.seed(123)
  r3 <- raster(extent(0, 20, 0, 5), vals = sample(1:30, size = 100, replace = TRUE))
  expect_false(identical(.robustDigest(r1), .robustDigest(r2)))
  # metadata same, content different
  expect_false(identical(.robustDigest(r1), .robustDigest(r3)))
  # metadata different, content same
  expect_false(identical(.robustDigest(r1), .robustDigest(r3)))
  expect_true(identical(r1[], r3[]))

  set.seed(123)
  r1 <- raster(extent(0, 10, 0, 10), vals = sample(1:30, size = 100, replace = TRUE))
  set.seed(123)
  r2 <- raster(extent(0, 10, 0, 10), vals = sample(1:30, size = 100, replace = TRUE))
  expect_true(identical(.robustDigest(r1), .robustDigest(r2)))

  # Brick
  r <- raster(matrix(1:10, 2, 5))
  b <- brick(r, r)
  dig <- .robustDigest(b)

  r1 <- raster(matrix(1:10, 2, 5))
  b1 <- brick(r1, r1)
  dig1 <- .robustDigest(b1)

  expect_identical(dig, dig1)

  b <- .writeRaster(b, file = tmpfile[1], overwrite = TRUE)
  dig <- .robustDigest(b)

  r <- raster(matrix(1:10, 2, 5))
  b <- brick(r, r)
  bb1 <- .writeRaster(b, file = tmpfile[1], overwrite = TRUE)
  dig1 <- .robustDigest(bb1)

  expect_identical(dig, dig1)

  # Stacks
  dimA <- 100
  r <- raster(matrix(1:dimA, round(sqrt(dimA)), round(sqrt(dimA))))
  b <- raster::stack(r, r)
  dig <- .robustDigest(b)

  r1 <- raster(matrix(1:dimA, round(sqrt(dimA)), round(sqrt(dimA))))
  b1 <- raster::stack(r1, r1)
  dig1 <- .robustDigest(b1)

  expect_identical(dig, dig1)

  r4 <- .writeRaster(r, file = tmpfile[1], overwrite = TRUE)
  r5 <- .writeRaster(r, file = tmpfile[2], overwrite = TRUE)
  b <- raster::stack(r4, r5)
  dig <- .robustDigest(b)

  r2 <- .writeRaster(r1, file = tmpfile[1], overwrite = TRUE)
  r3 <- .writeRaster(r1, file = tmpfile[2], overwrite = TRUE)
  b1 <- raster::stack(r2, r3)
  #b1 <- .writeRaster(b1, file = tmpfile[1], overwrite = TRUE)
  dig1 <- .robustDigest(b1)

  expect_identical(dig, dig1)
})

test_that("test 'quick' argument", {
  testInitOut <- testInit("raster", tmpFileExt = ".tif",
                          opts = list("reproducible.useMemoise" = TRUE,
                                      "reproducible.showSimilar" = FALSE))
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  ### Make raster using Cache
  set.seed(123)
  r1 <- raster(extent(0, 10, 0, 10), vals = sample(1:30, size = 100, replace = TRUE))
  r1 <- .writeRaster(r1, filename = tmpfile, overwrite = TRUE)
  quickFun <- function(rasFile) {
    ras <- raster(rasFile)
    ras[sample(ncell(ras), size = 1)]
  }
  fn <- filename(r1)
  out1a <- Cache(quickFun, asPath(filename(r1)), cacheRepo = tmpdir)
  out1b <- Cache(quickFun, asPath(filename(r1)), cacheRepo = tmpdir, quick = TRUE)
  r1[4] <- r1[4] + 1
  r1 <- .writeRaster(r1, filename = tmpfile, overwrite = TRUE)
  mess1 <- capture_messages({
    out1c <- Cache(quickFun, asPath(filename(r1)), cacheRepo = tmpdir, quick = TRUE)
  })

  expect_true(sum(grepl(paste0(paste(.loadedCacheMsg(.loadedCacheResultMsg, "quickFun"), .addingToMemoisedMsg),"|",
                               .loadedCacheMsg(.loadedMemoisedResultMsg, "quickFun")),
                        mess1)) == 1)
  # expect_true(any(grepl(paste(.loadedCacheResultMsg, "quickFun call, adding to memoised copy"), mess1 )))
  expect_silent({
    out1c <- Cache(quickFun, asPath(filename(r1)), cacheRepo = tmpdir, quick = FALSE)
  })

  # Using Raster directly -- not file
  quickFun <- function(ras) {
    ras[sample(ncell(ras), size = 1)]
  }
  out1a <- Cache(quickFun, r1, cacheRepo = tmpdir)
  out1b <- Cache(quickFun, r1, cacheRepo = tmpdir, quick = TRUE)
  r1[4] <- r1[4] + 1
  r1 <- .writeRaster(r1, filename = tmpfile, overwrite = TRUE)
  mess1 <- capture_messages({
    out1c <- Cache(quickFun, r1, cacheRepo = tmpdir, quick = TRUE)
  })
  expect_true(sum(grepl(paste0(paste(.loadedCacheMsg(.loadedCacheResultMsg, "quickFun"), .addingToMemoisedMsg),"|",
                               paste(.loadedMemoisedResultMsg, "quickFun call")),
                    mess1)) == 1)

  #mess3 <- capture_messages({ out1c <- Cache(quickFun, r1, cacheRepo = tmpdir, quick = FALSE) })
  expect_silent({
    out1c <- Cache(quickFun, r1, cacheRepo = tmpdir, quick = FALSE)
  })
})

test_that("test date-based cache removal", {
  testInitOut <- testInit("raster", tmpFileExt = ".pdf")
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  a <- Cache(runif, 1, cacheRepo = tmpdir)
  a1 <- showCache(tmpdir)
  expect_true(NROW(a1) > 0)
  b <- clearCache(tmpdir, before = Sys.Date() - 1, ask = FALSE)
  expect_true(NROW(b) == 0)
  expect_identical(a1, showCache(tmpdir))

  b <- clearCache(tmpdir, before = Sys.Date() + 1, ask = FALSE)
  expect_identical(data.table::setindex(b, NULL), data.table::setindex(a1, NULL))
})

test_that("test keepCache", {
  testInitOut <- testInit("raster")
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  vals3 <- .cacheNumDefaultTags() - 1
  Cache(rnorm, 10, cacheRepo = tmpdir)
  Cache(runif, 10, cacheRepo = tmpdir)
  Cache(round, runif(4), cacheRepo = tmpdir)
  expect_true(NROW(showCache(tmpdir)[!tagKey %in% .ignoreTagKeys()]) ==
                vals3 * 3)
  expect_true(NROW(showCache(tmpdir, c("rnorm", "runif"))[!tagKey %in% .ignoreTagKeys()]) ==
                0) # and search
  expect_true(NROW(keepCache(tmpdir, "rnorm", ask = FALSE)[!tagKey %in% .ignoreTagKeys()]) == vals3)

  # do it twice to make sure it can deal with repeats
  expect_true(NROW(keepCache(tmpdir, "rnorm", ask = FALSE)[!tagKey %in% .ignoreTagKeys()]) == vals3)
  st <- Sys.time()
  Sys.sleep(1)
  Cache(sample, 10, cacheRepo = tmpdir)
  Cache(length, 10, cacheRepo = tmpdir)
  Cache(sum, runif(4), cacheRepo = tmpdir)
  expect_true(NROW(showCache(tmpdir, before = st)[!tagKey %in% .ignoreTagKeys()]) == vals3)
  expect_true(NROW(keepCache(tmpdir, before = st, ask = FALSE)[!tagKey %in% .ignoreTagKeys()]) == vals3)
  expect_true(NROW(showCache(tmpdir)[!tagKey %in% .ignoreTagKeys()]) == vals3)

  ranNums <- Cache(runif, 4, cacheRepo = tmpdir, userTags = "objectName:a")
  ranNums <- Cache(rnorm, 4, cacheRepo = tmpdir, userTags = "objectName:a")
  showCache(tmpdir) # shows rnorm, runif and rnorm objects

  # shows nothing because object has both spades and rnorm
  expect_true(NROW(showCache(tmpdir, userTags = c("spades", "rnorm"))) == 0)

  # "or" search
  expect_true(length(unique(
    showCache(tmpdir, userTags = "spades|rnorm")[[.cacheTableHashColName()]])) == 2)

  # keep all with spades or rnorm
  expect_true(length(unique(
    keepCache(tmpdir, userTags = "spades|rnorm", ask = FALSE)[[.cacheTableHashColName()]])) == 2)

  # shows spades, runif and rnorm objects
  expect_true(length(unique(showCache(tmpdir)[[.cacheTableHashColName()]])) == 2)
})

test_that("test environments", {
  testInitOut <- testInit("raster", tmpFileExt = ".pdf")
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

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
  out <- Cache(shortFn, a = a, cacheRepo = tmpdir)
  out2 <- Cache(shortFn, a = b, cacheRepo = tmpdir)
  out3 <- Cache(shortFn, a = g, cacheRepo = tmpdir)
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

  out <- Cache(shortFn, a = a, cacheRepo = tmpdir)
  out2 <- Cache(shortFn, a = b, cacheRepo = tmpdir)
  out3 <- Cache(shortFn, a = g, cacheRepo = tmpdir)
  out4 <- Cache(shortFn, a = i, cacheRepo = tmpdir)
  out5 <- Cache(shortFn, a = i2, cacheRepo = tmpdir)

  # test different values are different
  expect_false(identical(attributes(out), attributes(out2)))

  # test same values but different enviros are same
  expect_true(identical(attributes(out)["tags"], attributes(out3)["tags"]))

  # test environment is same as a list
  expect_true(identical(attributes(out)["tags"], attributes(out4)["tags"]))

  # test environment is same as recursive list
  expect_true(identical(attributes(out)["tags"], attributes(out5)["tags"]))

  df <- data.frame(a = a$a, b = LETTERS[1:10])
  out6 <- Cache(shortFn, a = df, cacheRepo = tmpdir)
  out7 <- Cache(shortFn, a = df, cacheRepo = tmpdir)
  expect_true(identical(attributes(out6)["tags"], attributes(out7)["tags"])) # test data.frame
})

test_that("test asPath", {
  testInitOut <- testInit("raster", tmpFileExt = "pdf",
                          opts = list("reproducible.useMemoise" = TRUE,
                                      "reproducible.showSimilar" = FALSE))
  unlink(dir(tmpdir, full.names = TRUE))
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  obj <- sample(1e5,10)
  # First -- has no filename.RData
  fn <- file.path(tempdir(), "filename.RData")
  on.exit(unlink(fn), add = TRUE)
  a1 <- capture_messages(Cache(saveRDS, obj, file = fn, cacheRepo = tmpdir))
  # Second -- has a filename.RData, and passing a character string,
  #           it tries to see if it is a file, if yes, it digests it
  a2 <- capture_messages(Cache(saveRDS, obj, file = fn, cacheRepo = tmpdir))
  # Third -- finally has all same as second time
  a3 <- capture_messages(Cache(saveRDS, obj, file = fn, cacheRepo = tmpdir))

  expect_true(length(a1) == 0)
  expect_true(length(a2) == 0)
  expect_true(sum(grepl(paste(.loadedMemoisedResultMsg, "|",
                              .loadedCacheResultMsg), a3)) == 1)

  unlink(fn)
  try(clearCache(tmpdir, ask = FALSE), silent = TRUE)
  a1 <- capture_messages(Cache(saveRDS, obj, file = asPath(fn),
                               quick = TRUE, cacheRepo = tmpdir))
  a2 <- capture_messages(Cache(saveRDS, obj, file = asPath(fn),
                               quick = TRUE, cacheRepo = tmpdir))
  a3 <- capture_messages(Cache(saveRDS, obj, file = asPath(fn),
                               quick = TRUE, cacheRepo = tmpdir))
  expect_true(length(a1) == 0)
  expect_true(sum(grepl(paste(.loadedCacheResultMsg, "|",
                              .loadedMemoisedResultMsg), a2)) == 1)
  expect_true(sum(grepl(paste(.loadedMemoisedResultMsg, "saveRDS call"), a3)) == 1)

  unlink(fn)
  tmpdir2 <- tmpdir
  try(clearCache(tmpdir2, ask = FALSE), silent = TRUE)
  a1 <- capture_messages(Cache(saveRDS, obj, file = as(fn, "Path"),
                               quick = TRUE, cacheRepo = tmpdir))
  a2 <- capture_messages(Cache(saveRDS, obj, file = as(fn, "Path"),
                               quick = TRUE, cacheRepo = tmpdir))
  a3 <- capture_messages(Cache(saveRDS, obj, file = as(fn, "Path"),
                               quick = TRUE, cacheRepo = tmpdir))
  expect_true(length(a1) == 0)
  expect_true(sum(grepl(paste(.loadedCacheResultMsg, "|",
                              .loadedMemoisedResultMsg), a2)) == 1)
  expect_true(sum(grepl(paste(.loadedMemoisedResultMsg, "saveRDS call"), a3)) == 1)

})

test_that("test wrong ways of calling Cache", {
  testInitOut <- try(testInit(tmpFileExt = ".pdf"))
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  # expect_error(Cache(sample(1), cacheRepo = tmpdir), "Can't understand")
  # expect_error(Cache(a <- sample(1), cacheRepo = tmpdir), "unexpected assignment")
  expect_true(1 == Cache(sample, 1, cacheRepo = tmpdir))
})


test_that("test quoted FUN in Cache", {
  testInitOut <- testInit()
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  A <- Cache(rnorm, 10, 16, cacheRepo = tmpdir) # nolint

  ## recover cached copies:
  B <- Cache(rnorm, 10, 16, cacheRepo = tmpdir) # nolint
  C <- Cache(quote(rnorm(n = 10, 16)), cacheRepo = tmpdir) # nolint

  D <- rnorm(10, 16) |> Cache(cacheRepo = tmpdir) # nolint

  expect_true(all.equalCache(A, B))
  expect_true(all.equalCache(A, C))
  expect_true(all.equalCache(A, D))
})

test_that("test Cache argument inheritance to inner functions", {
  testInitOut <- testInit("raster", opts = list("reproducible.showSimilar" = FALSE,
                                                "reproducible.useMemoise" = FALSE))
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  tmpDirFiles <- dir(tempdir())
  on.exit({
    newOnes <- setdiff(tmpDirFiles, dir(tempdir()))
    unlink(newOnes, recursive = TRUE)
  }, add = TRUE)

  outer <- function(n) {
    Cache(rnorm, n)
  }

  expect_silent(Cache(outer, n = 2, cacheRepo = tmpdir))
  clearCache(ask = FALSE, x = tmpdir)

  options(reproducible.cachePath = .reproducibleTempCacheDir())
  out <- capture_messages(Cache(outer, n = 2))
  expect_true(all(unlist(lapply(
    c(messageNoCacheRepo, messageNoCacheRepo),
    function(mess) any(grepl(mess, out))))))

  # does Sys.time() propagate to outer ones
  out <- capture_messages(Cache(outer, n = 2, notOlderThan = Sys.time()))
  expect_true(all(grepl(messageNoCacheRepo, out)))

  # does Sys.time() propagate to outer ones -- no message about cacheRepo being tempdir()
  expect_silent(Cache(outer, n = 2, notOlderThan = Sys.time(), cacheRepo = tmpdir))

  # does cacheRepo propagate to outer ones -- no message about cacheRepo being tempdir()
  out <- capture_messages(Cache(outer, n = 2, cacheRepo = tmpdir))
  expect_true(length(out) == 2)
  expect_true(sum(grepl(paste(.loadedCacheResultMsg, "outer call"), out)) == 1)

  # check that the rnorm inside "outer" returns cached value even if outer "outer" function is changed
  outer <- function(n) {
    a <- 1
    Cache(rnorm, n)
  }
  out <- capture_messages(Cache(outer, n = 2, cacheRepo = tmpdir))
  expect_true(length(out) == 2)
  msgGrep <- paste(paste(.loadedCacheResultMsg, "rnorm call"),
                   "There is no similar item in the cacheRepo",
                   sep = "|")
  expect_true(sum(grepl(msgGrep, out)) == 1)

  # Override with explicit argument
  outer <- function(n) {
    a <- 1
    Cache(rnorm, n, notOlderThan = Sys.time())
  }
  out <- capture_messages(Cache(outer, n = 2, cacheRepo = tmpdir))
  expect_true(length(out) == 0)
  # expect_true(all(grepl("There is no similar item in the cacheRepo", out)))

  # change the outer function, so no cache on that, & have notOlderThan on rnorm,
  #    so no Cache on that
  outer <- function(n) {
    b <- 1
    Cache(rnorm, n, notOlderThan = Sys.time())
  }
  out <- capture_messages(Cache(outer, n = 2, cacheRepo = tmpdir))
  expect_true(length(out) == 0)
  # expect_true(all(grepl("There is no similar item in the cacheRepo", out)))
  # Second time will get a cache on outer
  out <- capture_messages(Cache(outer, n = 2, cacheRepo = tmpdir))
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
  out <- capture_messages(Cache(outer, n = 2, cacheRepo = tmpdir))
  #expect_true(all(grepl("There is no similar item in the cacheRepo", out)))
  #expect_true(all(grepl(paste(.loadedCacheResultMsg, "outer call"), out)))

  outer <- function(n) {
    Cache(inner, 0.1, notOlderThan = Sys.time() - 1e4)
  }

  out <- capture_messages(Cache(outer, n = 2, cacheRepo = tmpdir, notOlderThan = Sys.time()))
  msgGrep <- paste(paste(.loadedCacheResultMsg, "inner call"),
                   "There is no similar item in the cacheRepo",
                   sep = "|")
  expect_true(sum(grepl(msgGrep, out)) == 1)

  outer <- function(n) {
    Cache(inner, 0.1, notOlderThan = Sys.time())
  }
  inner <- function(mean) {
    d <- 1
    Cache(rnorm, n = 3, mean = mean, notOlderThan = Sys.time() - 1e5)
  }

  out <- capture_messages(Cache(outer, n = 2, cacheRepo = tmpdir, notOlderThan = Sys.time()))
  msgGrep <- paste(paste(.loadedCacheResultMsg, "rnorm call"),
                   "There is no similar item in the cacheRepo",
                   sep = "|")
  expect_true(sum(grepl(msgGrep, out)) == 1)

  # Check userTags -- all items have it
  clearCache(tmpdir, ask = FALSE)
  outerTag <- "howdie"
  aa <- Cache(outer, n = 2, cacheRepo = tmpdir, userTags = outerTag)
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
  aa <- Cache(outer, n = 2, cacheRepo = tmpdir, userTags = outerTag)
  bb <- showCache(tmpdir, userTags = outerTag)
  cc <- showCache(tmpdir)
  expect_true(identical(bb, cc))

  #
  bb <- showCache(tmpdir, userTags = "notHowdie")
  cc <- showCache(tmpdir)
  expect_false(identical(bb, cc))
  expect_true(length(unique(bb[[.cacheTableHashColName()]])) == 1)
  expect_true(length(unique(cc[[.cacheTableHashColName()]])) == 3)
})

##########################
test_that("test future", {

  skip_on_cran()
  skip_on_os("mac")
  skip_on_os("solaris")
  if (interactive()) {

    if (requireNamespace("future") && requireNamespace("future.callr")) {
      testInitOut <- testInit("raster", verbose = TRUE, tmpFileExt = ".rds",
                              opts = list("future.supportsMulticore.unstable" = "quiet",
                                          "reproducible.futurePlan" = "future.callr::callr"))
      on.exit({
        testOnExit(testInitOut)
        options(opts)
      }, add = TRUE)

      opts <- options("reproducible.futureForce" = TRUE)

      # There is now a warning with future package
      a <- d <- list()
      clearCache(tmpCache)
      (aa <- system.time({for (i in c(1:3)) a[[i]] <- Cache(cacheRepo = tmpCache, seq, 5, 1e7 + i)}))
      sc1 <- showCache(tmpCache)
      expect_true(length(unique(sc1$cacheId)) == 3)

      # Future ones show up in same Cache repo as non-future
      options("reproducible.futurePlan" = FALSE)
      a <- d <- list()
      (aa <- system.time({for (i in c(1:3)) d[[i]] <- Cache(cacheRepo = tmpCache, seq, 5, 1e7 + i)}))
      sc2 <- showCache(tmpCache)
      expect_true(length(unique(sc1$cacheId)) == 3)
      expect_true(sum(sapply(d, function(dd) attr(dd, ".Cache")$newCache)) == 0)

      options("reproducible.futurePlan" = "future.callr::callr")
      a <- d <- list()
      (aa <- system.time({for (i in c(1:3)) a[[i]] <- Cache(cacheRepo = tmpCache, seq, 5, 1e7 + i + 1)}))
      expect_true(sum(sapply(a, function(aa) attr(aa, ".Cache")$newCache)) == 1)

      # These next 2 are large enough; don't need to force
      options("reproducible.futureForce" = NULL)
      options("reproducible.futurePlan" = "future.callr::callr")
      a <- d <- list()
      (aa <- system.time({for (i in c(1:3)) a[[i]] <- Cache(cacheRepo = tmpCache, rnorm(2e5 + i + 1))}))
      expect_true(sum(sapply(a, function(aaa) attr(aaa, ".Cache")$newCache)) == 3)

      a <- d <- list()
      (aa <- system.time({for (i in c(1:3)) a[[i]] <- Cache(cacheRepo = tmpCache, rnorm(2e5 + i + 1))}))
      expect_true(sum(sapply(a, function(aaa) attr(aaa, ".Cache")$newCache)) == 0)

    }
  }
})

##########################
test_that("test mergeCache", {
  testInitOut <- testInit("data.table")
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  # aaaa <<- gggg <<- kkkk <<- 1
  a <- Cache(rnorm, 1, cacheRepo = tmpdir)
  b <- Cache(rnorm, 2, cacheRepo = tmpCache)

  aCache <- showCache(tmpdir)
  bCache <- showCache(tmpCache)

  #nnnn <<- ffff <<- aaaa <<- bbbb <<-

  d <- mergeCache(tmpCache, tmpdir)

  dCache <- showCache(d)
  abCache <- rbindlist(list(aCache, bCache))

  # Remove date and accessed time stamps
  dCache <- dCache[!tagKey %in% c("date", "accessed")]
  abCache <- abCache[!tagKey %in% c("date", "accessed")]

  # remove keys
  setkeyv(dCache, .cacheTableHashColName())
  setkeyv(abCache, .cacheTableHashColName())

  expect_true(all.equal(abCache[, list(get(.cacheTableHashColName()),
                                       tagKey, get(.cacheTableTagColName()))],
                        dCache[, list(get(.cacheTableHashColName()),
                                      tagKey, get(.cacheTableTagColName()))]))
  mess <- capture_messages({
    d1 <- mergeCache(tmpCache, tmpdir)
  })
  expect_true(any(grepl("Skipping", mess)))
  expect_true(identical(showCache(d), showCache(d1)))
})

##########################
test_that("test cache-helpers", {
  testInitOut <- testInit("raster")
  out <- createCache(tmpCache)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  tmpfile <- tempfile(tmpdir = tmpdir, fileext = ".grd")
  tmpfile2 <- tempfile(tmpdir = tmpdir, fileext = ".grd")
  tmpfile3 <- tempfile(tmpdir = tmpdir, fileext = ".grd")
  r <- raster(extent(0, 5, 0, 5), res = 1, vals = rep(1:2, length.out = 25))
  levels(r) <- data.frame(ID = 1:2, Val = 3:4)
  b <- .prepareFileBackedRaster(r, tmpCache)
  is(b, "RasterLayer")
  expect_true(!nzchar(filename(b)))

  r1 <- raster(extent(0,5,0,5), res = 1, vals = rep(1:2, length.out = 25))
  s <- raster::stack(r, r1)
  b <- .prepareFileBackedRaster(s, tmpCache)

  r <- .writeRaster(r, filename = tmpfile, overwrite = TRUE)
  r1 <- .writeRaster(r1, filename = tmpfile2, overwrite = TRUE)
  s <- addLayer(r, r1)

  # Test deleted raster backed file
  file.remove(tmpfile2)
  expect_error(b <- .prepareFileBackedRaster(s, tmpCache), "The following file-backed rasters")
  expect_error(b <- .prepareFileBackedRaster(r1, tmpCache), "The following file-backed rasters")

  # Test wrong folder names
  tmpfile <- file.path(tmpCache, basename(tempfile(tmpdir = tmpdir, fileext = ".grd")))
  r <- .writeRaster(r, filename = tmpfile, overwrite = TRUE)
  r@file@name <- gsub(pattern = normalizePath(tempdir(), winslash = "/", mustWork = FALSE),
                      normalizePath(tmpfile, winslash = "/", mustWork = FALSE),
                      replacement = basename(tempdir()))
  # show it is not there, so it is the wrong name
  expect_false(file.exists(filename(r)))
  # fix it, by giving correct tmpCache path
  b <- .prepareFileBackedRaster(r, tmpCache)
  expect_true(file.exists(filename(b)))
  # Check that it makes a new name if already in Cache
  checkPath(CacheStorageRasterDir(tmpCache), create = TRUE)
  r1 <- .writeRaster(r1, filename = file.path(CacheStorageRasterDir(tmpCache), basename(tmpfile2)), overwrite = TRUE)
  b <- .prepareFileBackedRaster(r1, tmpCache)
  expect_true(identical(normalizePath(filename(b), winslash = "/", mustWork = FALSE),
                        normalizePath(file.path(dirname(filename(r1)),
                                                nextNumericName(basename(filename(r1)))),
                                      winslash = "/", mustWork = FALSE)))

  r <- raster(extent(0, 5, 0, 5), res = 1, vals = rep(1:2, length.out = 25))
  r1 <- raster(extent(0, 5, 0, 5), res = 1, vals = rep(1:2, length.out = 25))
  tmpfile <- tempfile(tmpdir = tmpdir, fileext = ".grd")
  r <- .writeRaster(r, filename = tmpfile, overwrite = TRUE)
  r1 <- .writeRaster(r1, filename = tmpfile2, overwrite = TRUE)
  s <- addLayer(r, r1)
  b1 <- .prepareFileBackedRaster(s, cacheRepo = tmpCache)
  expect_true(is(b1, "RasterStack"))
  expect_true(identical(filename(b1), ""))

  expect_true(identical(normalizePath(filename(b1$layer.1), winslash = "/", mustWork = FALSE),
                        normalizePath(file.path(CacheStorageRasterDir(tmpCache),
                                                basename(filename(r))), winslash = "/", mustWork = FALSE)))

  # Give them single file -- 2 layer stack; like a brick, but a stack
  r[] <- r[]
  r1[] <- r1[]
  b <- raster::stack(r, r1)

  b <- .writeRaster(b, filename = tmpfile, overwrite = TRUE)
  b <- raster::stack(b)
  expect_true(nlayers(b) == 2)
  expect_true(identical(normPath(b$layer.1@file@name),
                        normPath(b$layer.2@file@name)))

  b1 <- .prepareFileBackedRaster(b, tmpCache)
  expect_true(nlayers(b1) == 2)
  b1a <- raster::stack(Filenames(b1)[1])
  expect_true(nlayers(b1a) == 2)

})

test_that("test cache-helpers", {
  testInitOut <- testInit("raster")
  out <- reproducible::createCache(tmpCache)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  tmpfile <- tempfile(tmpdir = tmpdir, fileext = ".grd")
  tmpfile2 <- tempfile(tmpdir = tmpdir, fileext = ".grd")
  tmpfile3 <- tempfile(tmpdir = tmpdir, fileext = ".grd")
  tmpfile1tif <- tempfile(tmpdir = tmpdir, fileext = ".tif")
  tmpfile2tif <- tempfile(tmpdir = tmpdir, fileext = ".tif")
  tmpfile3tif <- tempfile(tmpdir = tmpdir, fileext = ".tif")

  r1 <- raster(extent(0,3,0,3), vals = 1)
  r2 <- raster(extent(0,3,0,3), vals = 2)
  r3 <- raster(extent(0,3,0,3), vals = 3)
  r2 <- .writeRaster(r1, filename = tmpfile2)
  r3 <- .writeRaster(r1, filename = tmpfile3)
  r2tif <- suppressWarningsSpecific(falseWarning = proj6Warn,
                                    .writeRaster(r1, filename = tmpfile2tif))
  r3tif <- suppressWarningsSpecific(falseWarning = proj6Warn,
                           .writeRaster(r1, filename = tmpfile3tif))

  s1 <- raster::stack(r1, r1)
  s2 <- raster::stack(r1, r2)
  s3 <- raster::stack(r3, r2)
  s1 <- raster::stack(r1, r1)
  s2tif <- raster::stack(r1, r2tif)
  s3tif <- raster::stack(r3tif, r2tif)



  i <- 1
  for (rr in list(r1, r2, r3, r2tif, r3tif, s1, s2, s3, s2tif, s3tif)) {
    message(i); i <- i + 1

    out2 <- .prepareFileBackedRaster(rr, cacheRepo = tmpCache)
    test1 <- identical(out2, rr)
    test2 <- identical(Filenames(out2), Filenames(rr))
    test3 <- identical(Filenames(out2, allowMultiple = FALSE),
                       Filenames(rr, allowMultiple = FALSE))
    test4 <- identical(basename(Filenames(out2, allowMultiple = TRUE)),
                       basename(Filenames(rr, allowMultiple = TRUE)))
    test5 <- identical(length(Filenames(out2)), length(Filenames(rr)))
    if (any(nchar(Filenames(out2)) > 0)) {
      expect_false(test1 && test2 && test3)
      expect_true(test4 && test5)
    } else {
      expect_true(test1 && test2 && test3 && test4 && test5)
    }
    unlink(Filenames(out2))
  }

  out2 <- .prepareFileBackedRaster(s2, cacheRepo = tmpCache)
  out3 <- .prepareFileBackedRaster(s2, cacheRepo = tmpCache)
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
  expect_error(out2 <- .prepareFileBackedRaster(s2, cacheRepo = tmpCache), "most likely")

})

test_that("test useCache = 'overwrite'", {
  testInitOut <- testInit(ask = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  a <- Cache(rnorm, 1, useCache = "overwrite", cacheRepo = tmpCache)
  #aaaa <<- bbbb <<- cccc <<- dddd <<- eeee <<- ffff <<- gggg <<- 1
  # ffff <<- 1
  mess <- capture_messages({
    b <- Cache(rnorm, 1, useCache = "overwrite", cacheRepo = tmpCache)
  })
  expect_true(!identical(a, b))
  expect_true(any(grepl(pattern = "Overwriting", mess)))

  clearCache(x = tmpCache, ask = FALSE)

  testOnExit(testInitOut)
  testInitOut <- testInit(ask = FALSE, opts = list("reproducible.useCache" = "overwrite"))

  a <- Cache(rnorm, 1, cacheRepo = tmpCache)
  mess <- capture_messages({
    b <- Cache(rnorm, 1, cacheRepo = tmpCache)
  })
  expect_true(!identical(a, b))
  expect_true(any(grepl(pattern = "Overwriting", mess)))
})

test_that("test rm large non-file-backed rasters", {
  ## This is a large object test!
  skip_on_cran()

  skip_on_ci()

  testInitOut <- testInit("raster", ask = FALSE)

  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  opts11 <- options("reproducible.cacheSpeed" = "slow",
                    "reproducible.cacheSaveFormat" = "qs")
  on.exit(options(opts11), add = TRUE)

  r <- Cache(raster, extent(0, 10000, 0, 10000), res = 1, vals = 1,
             cacheRepo = tmpdir, userTags = "first")
  st1 <- system.time(clearCache(tmpdir, userTags = "first", ask = FALSE ))
  expect_true(st1["user.self"] < 0.75) # This was > 2 seconds in old way
})

test_that("test cc", {
  skip_on_cran()

  skip_on_ci()

  testInitOut <- testInit(ask = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  Cache(rnorm, 1, cacheRepo = tmpCache)
  Sys.sleep(1) # 0.2
  thisTime <- Sys.time()
  Sys.sleep(1) # 0.2
  Cache(rnorm, 2, cacheRepo = tmpCache)
  Cache(rnorm, 3, cacheRepo = tmpCache)
  Cache(rnorm, 4, cacheRepo = tmpCache)
  a <- showCache(x = tmpCache) # shows all 4 entries
  expect_true(length(unique(a[[.cacheTableHashColName()]])) == 4)

  #rmFC <<- 1
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
  testInitOut <- testInit("raster", ask = FALSE, tmpFileExt = c(".tif", ".tif"))
  on.exit({
    dbDisconnectAll(conn)
    testOnExit(testInitOut)
  }, add = TRUE)

  conn <- dbConnectAll(cachePath = tmpdir, conn = NULL)
  ra <- raster(extent(0,10,0,10), vals = sample(1:100))
  rb <- raster(extent(0,10,0,10), vals = sample(1:100))
  r1 <- Cache(.writeRaster, ra, filename = tmpfile[1], overwrite = TRUE, cacheRepo = tmpCache)
  r2 <- Cache(.writeRaster, rb, filename = tmpfile[2], overwrite = TRUE, cacheRepo = tmpdir,
              conn = conn)
  expect_true(file.exists(filename(r1)))
  expect_true(file.exists(filename(r2)))
  expect_false(grepl(basename(dirname(filename(r1))), "rasters")) # no longer here
  expect_false(grepl(basename(dirname(filename(r2))), "rasters")) # no longer here

})

test_that("test .defaultUserTags", {
  testInitOut <- testInit()
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  b <- Cache(rnorm, 1, cacheRepo = tmpCache)
  sc <- showCache(tmpCache)
  actualTags <- sc$tagKey %in% .defaultUserTags
  anyNewTags <- any(!actualTags)
  expect_false(anyNewTags)
  if (isTRUE(anyNewTags)) stop("A new default userTag was added; please update .defaultUserTags")

})

test_that("test failed Cache recovery -- message to delete cacheId", {
  testInitOut <- testInit(opts = list("reproducible.useMemoise" = FALSE))
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  b <- Cache(rnorm, 1, cacheRepo = tmpdir)
  sc <- showCache(tmpdir)
  ci <- unique(sc[[.cacheTableHashColName()]])
  unlink(CacheStoredFile(tmpdir, ci))
  warn <- capture_warnings({
    err <- capture_error({
      b <- Cache(rnorm, 1, cacheRepo = tmpdir)
    })
  })
  expect_true(grepl(paste0("(trying to recover).*(",ci,")"), err))
  expect_true(grepl(paste0("[cannot|failed to] open"), paste(warn, err)))

})

test_that("test changing reproducible.cacheSaveFormat midstream", {
  if (!.requireNamespace("qs")) skip("Need qs; skipping test")
  testInitOut <- testInit(opts = list("reproducible.useMemoise" = FALSE))
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  opts <- options("reproducible.cacheSaveFormat" = "rds")
  b <- Cache(rnorm, 1, cacheRepo = tmpdir)
  sc <- showCache(tmpdir)
  ci <- unique(sc[[.cacheTableHashColName()]])
  options("reproducible.cacheSaveFormat" = "qs")
  on.exit({
    options(opts)
  }, add = TRUE)
  mess <- capture_messages({
    b <- Cache(rnorm, 1, cacheRepo = tmpdir)
  })
  expect_true(sum(grepl("Changing format of Cache entry from rds to qs", mess)) == 1)

  options("reproducible.cacheSaveFormat" = "rds")
  mess <- capture_messages({
    b <- Cache(rnorm, 1, cacheRepo = tmpdir)
  })
  expect_true(sum(grepl("Changing format of Cache entry from qs to rds", mess)) == 1)
})

test_that("test file link with duplicate Cache", {
  testInitOut <- testInit(opts = list("reproducible.useMemoise" = FALSE))
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

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
    b <- Cache(sam, N, cacheRepo = tmpCache)
  })

  # Change in RSQLite 2.2.2 -- there is now a random number used in dbAppend,
  #   so this test no longer works after the second time -- running it a 3rd time
  #   is sufficient for the test. The point is, if it is an identical result,
  #   then there will be a file.link
  set.seed(123)
  mess2 <- capture_messages({
    d <- Cache(sample, N, cacheRepo = tmpCache)
  })

  set.seed(123)
  mess3 <- capture_messages({
    g <- Cache(sam1, N, cacheRepo = tmpCache)
  })

  expect_true(sum(grepl("A file with identical", mess3)) == 1)

  set.seed(123)
  mess1 <- capture_messages({b <- Cache(sam, N, cacheRepo = tmpCache)})
  set.seed(123)
  # Because of RSQLite 2.2.2 this 2nd time is not considered identical -- need 3rd time
  mess2 <- capture_messages({d <- Cache(sample, N, cacheRepo = tmpCache)})
  clearCache(tmpCache, userTags = gsub("cacheId:", "", attr(b, "tags")))
  set.seed(123)
  mess2 <- capture_messages({d <- Cache(sam1, N, cacheRepo = tmpCache)})
  expect_true(any(grepl("loaded cached", mess2)))
  expect_true(any(grepl("loaded cached", mess1)))
  # There are intermittent "status 5" warnings on next line on Windows -- not relevant here
  warns <- capture_warnings({
    out1 <- try(system2("du", tmpCache, stdout = TRUE), silent = TRUE)
  })
  # out1 <- try(system2("du", tmpCache, stdout = TRUE), silent = TRUE)
  if (!is(out1, "try-error"))
    fs1 <- as.numeric(gsub("([[:digit:]]*).*", "\\1", out1))

  # It must be same output, not same input
  clearCache(tmpCache)
  set.seed(123)
  mess1 <- capture_messages({b <- Cache(sam, N, cacheRepo = tmpCache)})
  set.seed(1234)
  mess2 <- capture_messages({d <- Cache(sample, N, cacheRepo = tmpCache)})
  # Different inputs AND different output -- so no cache recovery and no file link
  expect_true(sum(grepl("loaded cache", mess2) | grepl("link", mess2)) == 0)
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

#  ._saveToCache_1 <<- 1
#  on.exit(rm(list = c("._saveToCache_1"), envir = .GlobalEnv), add = TRUE)
  warn <- capture_warnings({d1 <- Cache(sam1, N, cacheRepo = tmpCache)})
  expect_true(length(warn) == 0)
})

test_that("test .object arg for list in Cache", {
  testInitOut <- testInit()
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
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
  testInitOut <- testInit("raster", tmpFileExt = c("rds", "tif"))
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

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
    ranRas <- raster(extent(0, 10, 0, 10), vals = vals);
    ranRas <- suppressWarningsSpecific(
      falseWarnings = proj6Warn,
      .writeRaster(ranRas, filename = tf2, overwrite = TRUE)
    )
    a <- sample(1e7, 1);
    saveRDS(a, file = tf);

    # new copy
    messes[[i]] <- capture_messages(Cache(saveRDS, ranRas, file = tf, cacheRepo = tmpCache,
                                          quick = quicks[[i]]))
  }

  expect_true(length(messes[[6]]) > 0)  # undesirable: quick = TRUE -- raster has changed
  expect_true(length(messes[[8]]) == 0) # undesirable: quick = FALSE -- raster & file not changed

  ## Desired: 9 not cache, 10 cached. But 9 is picking up cache from 4.
  expect_true(length(messes[[9]]) == 0) # undesirable: quick = 'file' -- raster & file changed
  expect_true(length(messes[[10]]) > 0) # undesirable: quick = 'file -- raster & file not changed
  expect_true(sum(unlist(lapply(messes, function(x) length(x) > 0))) == 4L)
})

test_that("List of Rasters", {
  testInitOut <- testInit("raster", tmpFileExt = c("tif", "tif"))
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  listOfRas <- lapply(1:2, function(x) {
    vals <- sample(1:100)
    ranRas <- raster(extent(0,10,0,10), vals = vals);
    ranRas <- suppressWarningsSpecific(
      falseWarnings = proj6Warn,
      .writeRaster(ranRas, filename = tmpfile[[x]], overwrite = TRUE))
  })

  writeRasterList <- function(rasList) {
    lapply(rasList, function(ras) {
      filename <- paste0(Filenames(ras), rndstr(1, 6), ".tif")
      ras[] <- ras[]
      ranRas <- suppressWarningsSpecific(
        falseWarnings = proj6Warn,
        .writeRaster(ras, filename = filename, overwrite = FALSE))
    }
    )
  }
  a <- Cache(writeRasterList, listOfRas, cacheRepo = tmpCache)
  b <- Cache(writeRasterList, listOfRas, cacheRepo = tmpCache)
  expect_false(isTRUE(attr(b, ".Cache")$newCache))
  expect_true(isTRUE(attr(a, ".Cache")$newCache))
})



test_that("Cache the dots; .cacheExtra", {
  testInitOut <- testInit()
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  fn1 <- function(a, b, ...) {
    out <- fn2(...)
    return(out)
  }

  fn2 <- function(d, e = 0, f = 1) {
    rnorm(d, e, f)
  }


  suppressMessages(out1 <- Cache(fn1, a = 1, b = 2, d = 1, cacheRepo = tmpCache))
  suppressMessages(out2 <- Cache(fn1, a = 1, b = 2, d = 2, cacheRepo = tmpCache))
  expect_true(!identical(out1, out2))


  fn3 <- function(a, b) {
    out <- fn2(d = 1)
  }

  suppressMessages(out3 <- Cache(fn3, .cacheExtra = "12342", cacheRepo = tmpCache))
  suppressMessages(out4 <- Cache(fn3, .cacheExtra = "123422", cacheRepo = tmpCache))
  expect_true(!identical(out3, out4))

  # These are now the same because the .cacheExtra is the same and the arg is ignored
  suppressMessages(out5 <- Cache(mean, 6, omitArgs = "x", .cacheExtra = "234", cacheRepo = tmpCache))
  suppressMessages(out6 <- Cache(mean, 7, omitArgs = "x", .cacheExtra = "234", cacheRepo = tmpCache))
  expect_true(out6 - 6 == 0) # takes first one
  expect_equivalent(out5, out6) # the attributes will be different because one is a recovery of the other

})

test_that("switching between SQLite, 'csv'", {
  if (!requireNamespace("RSQLite")) skip()

  testInitOut <- testInit()
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  # suppressWarnings(try(rm(list = c("ccc", "ddd", "eee", "fff"), envir = .GlobalEnv)))
  drvs <- list(RSQLite::SQLite(), "csv")
  for(drv2 in drvs) {
    drv1s <- setdiff(drvs, list(drv2))
    for (drv1 in drv1s) {
      unlink(tmpCache, recursive = T)
      clearCache(drv = drv1, x = tmpCache)
      clearCache(drv = drv2, x = tmpCache)

      out <- Cache(rnorm, 1, drv = drv1, cacheRepo = tmpCache)
      expect_true(identical(attr(out, ".Cache")$newCache, TRUE))
      out <- Cache(rnorm, 1, drv = drv1, cacheRepo = tmpCache)
      expect_false(identical(attr(out, ".Cache")$newCache, TRUE))
      expect_true(file.exists(CacheDBFile(tmpCache, drv1)))

      # Now convert
      out <- Cache(rnorm, 1, drv = drv2, cacheRepo = tmpCache)
      expect_true(identical(attr(out, ".Cache")$newCache, FALSE))
      expect_true(!file.exists(CacheDBFile(tmpCache, drv1)))

      # Now convert back
      out <- Cache(rnorm, 1, drv = drv1, cacheRepo = tmpCache)
      expect_true(identical(attr(out, ".Cache")$newCache, FALSE))
      expect_true(!file.exists(CacheDBFile(tmpCache, drv2)))

      # Now convert to orig
      out <- Cache(rnorm, 1, drv = drv2, cacheRepo = tmpCache)
      expect_true(identical(attr(out, ".Cache")$newCache, FALSE))
      expect_true(!file.exists(CacheDBFile(tmpCache, drv1)))

      sc2 <- showCache(drv = drv2, tmpCache)
      expect_true(file.exists(CacheDBFile(tmpCache, drv = drv2)))
      expect_false(file.exists(CacheDBFile(tmpCache, drv = drv1)))
      sc1 <- showCache(drv = drv1, tmpCache)
      expect_true(identical(sc2, sc1)) # because it switches backend seamlessly
      expect_true(file.exists(CacheDBFile(tmpCache, drv = drv1)))
      expect_false(file.exists(CacheDBFile(tmpCache, drv = drv2)))
    }
  }
})

test_that("testing parallel", {
  skip("Parallel test must be run manually")


  # Set running on one R session in background
  drvs <- list("csv", RSQLite::SQLite())
  i <- 1
  options("reproducible.drv" = drvs[[i]])
  repo <- "~/tmp/test"
  unlink(repo, recursive = TRUE)
  checkPath(repo, create = TRUE)

  options(reproducible.verbose = 0)
    # Background
    out = lapply(1:20000, function(x) Cache(rnorm, sample(1, 1), cacheRepo = repo))

    # Run this in foreground
    options(reproducible.verbose = 0)
    st1 <- system.time(
      outs <- lapply(1:100, function(yy) {
      outs <- lapply(1:2, function(x) Cache(rnorm, 3456 + yy, cacheRepo = repo))
      sum(sapply(outs, function(x) attr(x, ".Cache")$newCache)) == 1
    })
    )
    sum(unlist(outs)) # should be 100 if no write problems occurred

  i <- 2
    options("reproducible.drv" = drvs[[i]])
    repo <- "~/tmp/test"
    unlink(repo, recursive = TRUE)
    checkPath(repo, create = TRUE)

    options(reproducible.verbose = 0)
    out = lapply(1:20000, function(x) Cache(rnorm, sample(1, 1), cacheRepo = repo))

    # Run this in foreground
    options(reproducible.verbose = 0)
    st2 <- system.time(
    outs <- lapply(1:100, function(yy) {
      outs <- lapply(1:2, function(x) Cache(rnorm, 3456 + yy, cacheRepo = repo))
      sum(sapply(outs, function(x) attr(x, ".Cache")$newCache)) == 1
    })
    )
    sum(unlist(outs)) # should be 100 if no write problems occurred



})

test_that("testing parallel", {
  skip("Parallel test must be run manually")


  # Set running on one R session in background
  fn1 <- function(x) for (i in 1:1e6) rnorm(1e5)
  fn2 <- function(x) for (i in 1:1e6) rnorm(1e5)
  if (requireNamespace("future"))
    a <- future::future(fn1(1), globals = list(fn1))

  future::futureCall(getFromNamespace("ongoingMemoryThisPid", "SpaDES.core")(seconds = seconds,
                                                                             interval = interval,
                                                                             thisPid = thisPid,
                                                                             outputFile = outputFile),
                     globals = list(memoryUseThisSession = memoryUseThisSession,
                                    outputFile = outputFile, thisPid = thisPid,
                                    seconds = seconds, interval = interval))





  drvs <- list("csv", RSQLite::SQLite())
  i <- 1
  options("reproducible.drv" = drvs[[i]])
  repo <- "~/tmp/test"
  unlink(repo, recursive = TRUE)
  checkPath(repo, create = TRUE)

  options(reproducible.verbose = 0)
  out = lapply(1:20000, function(x) Cache(rnorm, sample(1, 1), cacheRepo = repo))

  # Run this in foreground
  options(reproducible.verbose = 0)
  st1 <- system.time(
    outs <- lapply(1:100, function(yy) {
      outs <- lapply(1:2, function(x) Cache(rnorm, 3456 + yy, cacheRepo = repo))
      sum(sapply(outs, function(x) attr(x, ".Cache")$newCache)) == 1
    })
  )
  sum(unlist(outs)) # should be 100 if no write problems occurred

  i <- 2
  options("reproducible.drv" = drvs[[i]])
  repo <- "~/tmp/test"
  unlink(repo, recursive = TRUE)
  checkPath(repo, create = TRUE)

  options(reproducible.verbose = 0)
  out = lapply(1:20000, function(x) Cache(rnorm, sample(1, 1), cacheRepo = repo))

  # Run this in foreground
  options(reproducible.verbose = 0)
  st2 <- system.time(
    outs <- lapply(1:100, function(yy) {
      outs <- lapply(1:2, function(x) Cache(rnorm, 3456 + yy, cacheRepo = repo))
      sum(sapply(outs, function(x) attr(x, ".Cache")$newCache)) == 1
    })
  )
  sum(unlist(outs)) # should be 100 if no write problems occurred

})

test_that("change to new capturing of FUN & base pipe", {
  testInitOut <- testInit()
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  if (compareVersion(format(getRversion()), "4.2.0") >= 0) {
    st1 <- system.time(
      out1 <- Cache(do.call(rnorm, list(1, 2, sd = round(mean(runif(1e8, 4, 6))))),
                    cacheRepo = tmpCache)
    )
    st2 <- system.time(
      out2 <- runif(1e8, 4, 6) |>
        mean() |>
        round() |>
        rnorm(1, 2, sd = _) |> # _ Only works with R >= 4.2.0
        # (function(xx) rnorm(1, 2, sd = xx))() |>
        Cache(cacheRepo = tmpCache)
    )
    st3 <- system.time(
      out3 <- runif(1e8, 4, 6) |>
        mean() |>
        round() |>
        rnorm(1, 2, sd = _) |> # _ Only works with R >= 4.2.0
        # (function(xx) rnorm(1, 2, sd = xx))() |>
        Cache(cacheRepo = tmpCache)
    )
    expect_true(attr(out1, ".Cache")$newCache)
    expect_false(attr(out2, ".Cache")$newCache)
    expect_false(attr(out3, ".Cache")$newCache)

    expect_true(all((st1[1] * 50) > st3[1]))
  }

})

