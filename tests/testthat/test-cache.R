test_that("test file-backed raster caching", {
  testInitOut <- testInit("raster", tmpFileExt = c(".tif", ".grd"))
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  nOT <- Sys.time()

  randomPolyToDisk <- function(tmpfile) {
    r <- raster(extent(0, 10, 0, 10), vals = sample(1:30, size = 100, replace = TRUE))
    writeRaster(r, tmpfile[1], overwrite = TRUE)
    r <- raster(tmpfile[1])
    r
  }

  a <- randomPolyToDisk(tmpfile[1])
  # confirm that the raster has the given tmp filename
  expect_identical(strsplit(tmpfile[1], split = "[\\/]"),
                   strsplit(a@file@name, split = "[\\/]"))
  aa <- Cache(randomPolyToDisk, tmpfile[1], cacheRepo = tmpdir, userTags = "something2")

  # Test clearCache by tags
  expect_equal(NROW(showCache(tmpdir)[tagKey != "otherFunctions"]), 11)
  clearCache(tmpdir, userTags = "something$", ask = FALSE)
  expect_equal(NROW(showCache(tmpdir)[tagKey != "otherFunctions"]), 11)
  clearCache(tmpdir, userTags = "something2", ask = FALSE)
  expect_equal(NROW(showCache(tmpdir)), 0)

  aa <- Cache(randomPolyToDisk, tmpfile[1], cacheRepo = tmpdir, userTags = "something2")
  expect_equal(NROW(showCache(tmpdir)[tagKey != "otherFunctions"]), 11)
  clearCache(tmpdir, userTags = c("something$", "testing$"), ask = FALSE)
  expect_equal(NROW(showCache(tmpdir)[tagKey != "otherFunctions"]), 11)
  clearCache(tmpdir, userTags = c("something2$", "testing$"), ask = FALSE)
  expect_equal(NROW(showCache(tmpdir)[tagKey != "otherFunctions"]), 11)
  clearCache(tmpdir, userTags = c("something2$", "randomPolyToDisk$"), ask = FALSE)
  expect_equal(NROW(showCache(tmpdir)), 0)

  aa <- Cache(randomPolyToDisk, tmpfile[1], cacheRepo = tmpdir, userTags = "something2")

  # confirm that the raster has the new filename in the cachePath
  expect_false(identical(strsplit(tmpfile[1], split = "[\\/]"),
                         strsplit(file.path(tmpdir, "rasters",
                                            basename(tmpfile[1])), split = "[\\/]")))
  expect_true(any(grepl(pattern = basename(tmpfile[1]),
                        dir(file.path(tmpdir, "rasters")))))

  ### Test for 2 caching events with same file-backing name
  randomPolyToDisk2 <- function(tmpfile, rand) {
    r <- raster(extent(0, 10, 0, 10), vals = sample(1:30, size = 100, replace = TRUE))
    writeRaster(r, tmpfile[1], overwrite = TRUE)
    r <- raster(tmpfile[1])
    r
  }

  a <- Cache(randomPolyToDisk2, tmpfile[1], runif(1))
  b <- Cache(randomPolyToDisk2, tmpfile[1], runif(1))

  expect_false(identical(filename(a), filename(b)))

  # Caching a raster as an input works
  rasterTobinary <- function(raster) {
    ceiling(raster[] / (mean(raster[]) + 1))
  }
  nOT <- Sys.time()
  for (i in 1:2) {
    assign(paste0("b", i), system.time(
      assign(paste0("a", i), Cache(rasterTobinary, aa, cacheRepo = tmpdir, notOlderThan = nOT))
    ))
    nOT <- Sys.time() - 100
  }

  attr(a1, ".Cache")$newCache <- NULL
  attr(a2, ".Cache")$newCache <- NULL
  # test that they are identical
  expect_equal(a1, a2)

  # confirm that the second one was obtained through reading from Cache... much faster than writing
  expect_true(b1[1] > b2[1])

  clearCache(tmpdir, ask = FALSE)

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
  expect_true(NROW(showCache(tmpdir)[tagKey != "otherFunctions"]) == 10)

  # Test that factors are saved correctly
  randomPolyToFactorInMemory <- function() {
    r <- raster(extent(0, 10, 0, 10), vals = sample(1:30, size = 100, replace = TRUE))
    levels(r) <- data.frame(ID = 1:30, vals = sample(LETTERS[1:5], size = 30, replace = TRUE),
                            vals2 <- sample(1:7, size = 30, replace = TRUE))
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
    r <- writeRaster(r, tmpfile, overwrite = TRUE, datatype = "INT1U")
    r
  }

  # bb1 has original tmp filename
  bb1 <- randomPolyToFactorOnDisk(tmpfile[2])
  # bb has new one, inside of cache repository, with same basename
  bb <- Cache(randomPolyToFactorOnDisk, tmpfile = tmpfile[2], cacheRepo = tmpdir)
  expect_true(dirname(normPath(filename(bb))) == normPath(file.path(tmpdir, "rasters")))
  expect_true(basename(filename(bb)) == basename(tmpfile[2]))
  expect_false(filename(bb) == tmpfile[2])
  expect_true(dirname(filename(bb1)) == dirname(tmpfile[2]))
  expect_true(basename(filename(bb1)) == basename(tmpfile[2]))
  expect_true(dataType(bb) == "INT1U")
  expect_true(raster::is.factor(bb))
  expect_true(is(raster::levels(bb)[[1]], "data.frame"))
  expect_true(NCOL(raster::levels(bb)[[1]]) == 3)
  expect_true(NROW(raster::levels(bb)[[1]]) == 30)

  clearCache(tmpdir, ask = FALSE)
})

test_that("test memory backed raster robustDigest", {
  testInitOut <- testInit("raster", tmpFileExt = c("",""))
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  # library(raster)
  # tmpdir <- file.path(tempdir(), "testCache") %>% checkPath(create = TRUE)
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

  b <- writeRaster(b, file = tmpfile[1], overwrite = TRUE)
  dig <- .robustDigest(b)

  r <- raster(matrix(1:10, 2, 5))
  b <- brick(r, r)
  b <- writeRaster(b, file = tmpfile[1], overwrite = TRUE)
  dig1 <- .robustDigest(b)

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

  r4 <- writeRaster(r, file = tmpfile[1], overwrite = TRUE)
  r5 <- writeRaster(r, file = tmpfile[2], overwrite = TRUE)
  b <- raster::stack(r4, r5)
  dig <- .robustDigest(b)

  r2 <- writeRaster(r1, file = tmpfile[1], overwrite = TRUE)
  r3 <- writeRaster(r1, file = tmpfile[2], overwrite = TRUE)
  b1 <- raster::stack(r2, r3)
  #b1 <- writeRaster(b1, file = tmpfile[1], overwrite = TRUE)
  dig1 <- .robustDigest(b1)

  expect_identical(dig, dig1)
})

test_that("test 'quick' argument", {
  testInitOut <- testInit("raster", tmpFileExt = ".tif")
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  ### Make raster using Cache
  set.seed(123)
  r1 <- raster(extent(0, 10, 0, 10), vals = sample(1:30, size = 100, replace = TRUE))
  r1 <- writeRaster(r1, filename = tmpfile, overwrite = TRUE)
  quickFun <- function(rasFile) {
    ras <- raster(rasFile)
    ras[sample(ncell(ras), size = 1)]
  }
  fn <- filename(r1)
  out1a <- Cache(quickFun, asPath(filename(r1)), cacheRepo = tmpdir)
  out1b <- Cache(quickFun, asPath(filename(r1)), cacheRepo = tmpdir, quick = TRUE)
  r1[4] <- r1[4] + 1
  r1 <- writeRaster(r1, filename = tmpfile, overwrite = TRUE)
  mess1 <- capture_message(out1c <- Cache(quickFun, asPath(filename(r1)),
                                          cacheRepo = tmpdir, quick = TRUE))
  expect_true(grepl("loading cached result from previous quickFun call, adding to memoised copy", mess1 ))
  mess1 <- capture_message(out1c <- Cache(quickFun, asPath(filename(r1)),
                                          cacheRepo = tmpdir, quick = FALSE))
  expect_null(mess1)

  # Using Raster directly -- not file
  quickFun <- function(ras) {
    ras[sample(ncell(ras), size = 1)]
  }
  out1a <- Cache(quickFun, r1, cacheRepo = tmpdir)
  out1b <- Cache(quickFun, r1, cacheRepo = tmpdir, quick = TRUE)
  r1[4] <- r1[4] + 1
  r1 <- writeRaster(r1, filename = tmpfile, overwrite = TRUE)
  mess1 <- capture_message(out1c <- Cache(quickFun, r1, cacheRepo = tmpdir, quick = TRUE))
  expect_true(grepl("loading cached result from previous quickFun call, adding to memoised copy", mess1 ))
  mess1 <- capture_message(out1c <- Cache(quickFun, r1, cacheRepo = tmpdir, quick = FALSE))
  expect_null(mess1)


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
  expect_identical(data.table::setindex(b, NULL), a1)
})

test_that("test keepCache", {
  testInitOut <- testInit("raster")
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  Cache(rnorm, 10, cacheRepo = tmpdir)
  Cache(runif, 10, cacheRepo = tmpdir)
  Cache(round, runif(4), cacheRepo = tmpdir)
  expect_true(NROW(showCache(tmpdir)[tagKey != "otherFunctions"]) == 30)
  expect_true(NROW(showCache(tmpdir, c("rnorm", "runif"))) == 0) # and search
  expect_true(NROW(keepCache(tmpdir, "rnorm", ask = FALSE)[tagKey != "otherFunctions"]) == 10)

  # do it twice to make sure it can deal with repeats
  expect_true(NROW(keepCache(tmpdir, "rnorm", ask = FALSE)[tagKey != "otherFunctions"]) == 10)
  Sys.sleep(1)
  st <- Sys.time()
  Sys.sleep(1)
  Cache(sample, 10, cacheRepo = tmpdir)
  Cache(length, 10, cacheRepo = tmpdir)
  Cache(sum, runif(4), cacheRepo = tmpdir)
  expect_true(NROW(showCache(tmpdir, before = st)[tagKey != "otherFunctions"]) == 10)
  expect_true(NROW(keepCache(tmpdir, before = st, ask = FALSE)[tagKey != "otherFunctions"]) == 10)
  expect_true(NROW(showCache(tmpdir)[tagKey != "otherFunctions"]) == 10)

  ranNums <- Cache(runif, 4, cacheRepo = tmpdir, userTags = "objectName:a")
  ranNums <- Cache(rnorm, 4, cacheRepo = tmpdir, userTags = "objectName:a")
  showCache(tmpdir) # shows spades, runif and rnorm objects

  # shows nothing because object has both spades and rnorm
  expect_true(NROW(showCache(tmpdir, userTags = c("spades", "rnorm"))) == 0)

  # "or" search
  expect_true(length(unique(showCache(tmpdir, userTags = "spades|rnorm")$artifact)) == 2)

  # keep all with spades or rnorm
  expect_true(length(unique(
    keepCache(tmpdir, userTags = "spades|rnorm", ask = FALSE)$artifact)) == 2)

  # shows spades, runif and rnorm objects
  expect_true(length(unique(showCache(tmpdir)$artifact)) == 2)
})

test_that("test environments", {
  testInitOut <- testInit("raster", tmpFileExt = ".pdf")
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  # make several unique environments
  a <- new.env()
  b <- new.env()
  g <- new.env()
  d <- new.env()
  f <- new.env()
  h <- new.env()
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
  testInitOut <- testInit("raster", tmpFileExt = "pdf")
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  obj <- 1:10
  origDir <- getwd()
  on.exit(setwd(origDir))
  setwd(tmpdir)
  # First -- has no filename.RData
  a1 <- capture_messages(Cache(saveRDS, obj, file = "filename.RData", cacheRepo = tmpdir))
  # Second -- has a filename.RData, and passing a character string,
  #           it tries to see if it is a file, if yes, it digests it
  a2 <- capture_messages(Cache(saveRDS, obj, file = "filename.RData", cacheRepo = tmpdir))
  # Third -- finally has all same as second time
  a3 <- capture_messages(Cache(saveRDS, obj, file = "filename.RData", cacheRepo = tmpdir))

  expect_true(length(a1) == 0)
  expect_true(length(a2) == 0)
  expect_true(grepl("loading cached", a3))

  unlink("filename.RData")
  try(clearCache(tmpdir, ask = FALSE), silent = TRUE)
  a1 <- capture_messages(Cache(saveRDS, obj, file = asPath("filename.RData"),
                               quick = TRUE, cacheRepo = tmpdir))
  a2 <- capture_messages(Cache(saveRDS, obj, file = asPath("filename.RData"),
                               quick = TRUE, cacheRepo = tmpdir))
  a3 <- capture_messages(Cache(saveRDS, obj, file = asPath("filename.RData"),
                               quick = TRUE, cacheRepo = tmpdir))
  expect_true(length(a1) == 0)
  expect_true(grepl("loading cached", a2))
  expect_true(grepl("loading memoised result from previous saveRDS call", a3))

  unlink("filename.RData")
  try(clearCache(tmpdir, ask = FALSE), silent = TRUE)
  a1 <- capture_messages(Cache(saveRDS, obj, file = as("filename.RData", "Path"),
                               quick = TRUE, cacheRepo = tmpdir))
  a2 <- capture_messages(Cache(saveRDS, obj, file = as("filename.RData", "Path"),
                               quick = TRUE, cacheRepo = tmpdir))
  a3 <- capture_messages(Cache(saveRDS, obj, file = as("filename.RData", "Path"),
                               quick = TRUE, cacheRepo = tmpdir))
  expect_true(length(a1) == 0)
  expect_true(grepl("loading cached", a2))
  expect_true(grepl("loading memoised result from previous saveRDS call", a3))

  # setwd(origDir)
  # unlink(tmpdir, recursive = TRUE)

  # make several unique environments
})

test_that("test wrong ways of calling Cache", {
  testInitOut <- try(testInit(tmpFileExt = ".pdf"))
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  expect_error(Cache(sample(1), cacheRepo = tmpdir), "Can't understand")
  expect_error(Cache(a <- sample(1), cacheRepo = tmpdir), "Can't understand")
  expect_true(1 == Cache(sample, 1, cacheRepo = tmpdir))
})

test_that("test pipe for Cache", {
  testInitOut <- testInit(c("raster", "magrittr"), tmpFileExt = ".pdf")
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  a <- rnorm(10, 16) %>% mean() %>% prod(., 6) # nolint
  b <- Cache(cacheRepo = tmpdir) %>% rnorm(10, 16) %>% mean() %C% prod(., 6) # nolint
  d <- Cache(cacheRepo = tmpdir) %>% rnorm(10, 16) %>% mean() %C% prod(., 6)  # nolint
  expect_true(all.equalWONewCache(b, d))
  expect_false(isTRUE(all.equalWONewCache(a, b)))
  d1 <- Cache(cacheRepo = tmpdir) %>% rnorm(10, 6) %>% mean() %C% prod(., 6)  # nolint
  expect_false(isTRUE(all.equalWONewCache(d1, d)))

  d1 <- Cache(cacheRepo = tmpdir) %>% rnorm(10, 16) %>% mean() %C% prod(., 16)  # nolint
  expect_false(isTRUE(all.equalWONewCache(d1, d)))

  d2 <- Cache(cacheRepo = tmpdir, notOlderThan = Sys.time()) %>%
    rnorm(10, 16) %>%
    mean() %C%
    prod(., 16)

  expect_false(isTRUE(all.equalWONewCache(d1, d2)))

  # New Pipe
  clearCache(tmpdir, ask = FALSE)
  a <- rnorm(10, 16) %>% mean() %>% prod(., 6) # nolint
  b <- Cache(cacheRepo = tmpdir) %C% rnorm(10, 16) %>% mean() %>% prod(., 6) # nolint
  d <- Cache(cacheRepo = tmpdir) %C% rnorm(10, 16) %>% mean() %>% prod(., 6) # nolint
  expect_true(all.equalWONewCache(b, d))
  expect_false(isTRUE(all.equalWONewCache(a, b)))
  d1 <- Cache(cacheRepo = tmpdir) %C% rnorm(10, 6) %>% mean() %>% prod(., 6) # nolint
  expect_false(isTRUE(all.equalWONewCache(d1, d)))

  d1 <- Cache(cacheRepo = tmpdir) %C% rnorm(10, 16) %>% mean() %>% prod(., 16) # nolint
  expect_false(isTRUE(all.equalWONewCache(d1, d)))

  d2 <- Cache(cacheRepo = tmpdir, notOlderThan = Sys.time()) %C%
    rnorm(10, 16) %>%
    mean() %>%
    prod(., 16)

  expect_false(isTRUE(all.equalWONewCache(d1, d2)))


  clearCache(tmpdir, ask = FALSE)
  a <- rnorm(10, 16) %>% mean() %>% prod(., 6) # nolint
  b <- Cache(cacheRepo = tmpdir) %C% rnorm(10, 16) %>% mean() # nolint
  d <- Cache(cacheRepo = tmpdir) %C% rnorm(10, 16) %>% mean() # nolint
  expect_true(all.equalWONewCache(b, d))

})

test_that("test quoted FUN in Cache", {
  testInitOut <- testInit("magrittr")
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  A <- Cache(rnorm, 10, 16, cacheRepo = tmpdir) # nolint

  ## recover cached copies:
  B <- Cache(rnorm, 10, 16, cacheRepo = tmpdir) # nolint
  C <- Cache(quote(rnorm(n = 10, 16)), cacheRepo = tmpdir) # nolint

  D <- try(Cache(cacheRepo = tmpdir) %>% rnorm(10, 16) , silent = TRUE) # nolint

  expect_true(all.equalWONewCache(A,B))
  expect_true(all.equalWONewCache(A, C))
  if (!is(D, "try-error"))
    expect_true(all.equalWONewCache(A, D))
})


test_that("test Cache argument inheritance to inner functions", {
  testInitOut <- testInit("raster")
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

  options(reproducible.cachePath = tempdir())
  out <- capture_messages(Cache(outer, n = 2))
  expect_true(all(unlist(lapply(c("No cacheRepo supplied and getOption\\('reproducible.cachePath'\\) is the temporary",
                                  "No cacheRepo supplied and getOption\\('reproducible.cachePath'\\) is the temporary"),
                                function(mess) any(grepl(mess, out))))))

  # does Sys.time() propagate to outer ones
  out <- capture_messages(Cache(outer, n = 2, notOlderThan = Sys.time()))
  expect_true(all(grepl("No cacheRepo supplied and getOption\\('reproducible.cachePath'\\) is the temporary", out)))

  # does Sys.time() propagate to outer ones -- no message about cacheRepo being tempdir()
  out <- expect_silent(Cache(outer, n = 2, notOlderThan = Sys.time(),
                             cacheRepo = tmpdir))

  # does cacheRepo propagate to outer ones -- no message about cacheRepo being tempdir()
  out <- capture_messages(Cache(outer, n = 2, cacheRepo = tmpdir))
  expect_true(length(out) == 1)
  expect_true(all(grepl("loading cached result from previous outer call", out)))

  # check that the rnorm inside "outer" returns cached value even if outer "outer" function is changed
  outer <- function(n) {
    a <- 1
    Cache(rnorm, n)
  }
  out <- capture_messages(Cache(outer, n = 2, cacheRepo = tmpdir))
  expect_true(length(out) == 1)
  expect_true(all(grepl("loading cached result from previous rnorm call", out)))

  # Override with explicit argument
  outer <- function(n) {
    a <- 1
    Cache(rnorm, n, notOlderThan = Sys.time())
  }
  out <- capture_messages(Cache(outer, n = 2, cacheRepo = tmpdir))
  expect_true(length(out) == 0)

  # change the outer function, so no cache on that, & have notOlderThan on rnorm,
  #    so no Cache on that
  outer <- function(n) {
    b <- 1
    Cache(rnorm, n, notOlderThan = Sys.time())
  }
  out <- capture_messages(Cache(outer, n = 2, cacheRepo = tmpdir))
  expect_true(length(out) == 0)
  # Second time will get a cache on outer
  out <- capture_messages(Cache(outer, n = 2, cacheRepo = tmpdir))
  expect_true(length(out) == 1)
  expect_true(all(grepl("loading cached result from previous outer call", out)))

  # doubly nested
  inner <- function(mean) {
    d <- 1
    Cache(rnorm, n = 3, mean = mean)
  }
  outer <- function(n) {
    Cache(inner, 0.1)
  }
  out <- capture_messages(Cache(outer, n = 2, cacheRepo = tmpdir))
  expect_true(all(grepl("loading cached result from previous outer call", out)))

  outer <- function(n) {
    Cache(inner, 0.1, notOlderThan = Sys.time() - 1e4)
  }

  out <- capture_messages(Cache(outer, n = 2, cacheRepo = tmpdir, notOlderThan = Sys.time()))
  expect_true(all(grepl("loading cached result from previous inner call", out)))

  outer <- function(n) {
    Cache(inner, 0.1, notOlderThan = Sys.time())
  }
  inner <- function(mean) {
    d <- 1
    Cache(rnorm, n = 3, mean = mean, notOlderThan = Sys.time() - 1e5)
  }

  out <- capture_messages(Cache(outer, n = 2, cacheRepo = tmpdir, notOlderThan = Sys.time()))
  expect_true(all(grepl("loading cached result from previous rnorm call", out)))

  # Check userTags -- all items have it
  clearCache(tmpdir, ask = FALSE)
  outerTag <- "howdie"
  aa <- Cache(outer, n = 2, cacheRepo = tmpdir, userTags = outerTag)
  bb <- showCache(tmpdir, userTags = outerTag)
  cc <- showCache(tmpdir)
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
  expect_true(length(unique(bb$artifact)) == 1)
  expect_true(length(unique(cc$artifact)) == 3)
})

##########################
test_that("test future", {
  skip_on_cran()
  .onLinux <- .Platform$OS.type == "unix" && unname(Sys.info()["sysname"]) == "Linux"
  if (.onLinux) {
    if (requireNamespace("future", quietly = TRUE)) {
      testInitOut <- testInit("raster", verbose = TRUE, tmpFileExt = ".rds")
      on.exit({
        testOnExit(testInitOut)
      }, add = TRUE)

      options("reproducible.futurePlan" = "multiprocess")
      (aa <- system.time({for (i in c(1:3)) a <- Cache(cacheRepo = tmpCache, seq, 5, 1e7 + i)}))
      a <- showCache(tmpCache)

      options("reproducible.futurePlan" = FALSE)
      try(unlink(tmpCache, recursive = TRUE))
      (bb <- system.time({for (i in 1:3) a <- Cache(cacheRepo = tmpCache, seq, 5, 1e7 + i)}))

      # Test the speed of rerunning same line
      (aa <- system.time({for (i in c(1,1)) a <- Cache(cacheRepo = tmpCache, seq, 5, 1e7 + i)}))
    }
  }
})


##########################
test_that("test mergeCache", {
  testInitOut <- testInit("data.table")
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  a <- Cache(rnorm, 1, cacheRepo = tmpdir)
  b <- Cache(rnorm, 1, cacheRepo = tmpCache)

  aCache <- showCache(tmpdir)
  bCache <- showCache(tmpCache)

  d <- mergeCache(tmpCache, tmpdir)

  dCache <- showCache(d)
  abCache <- rbindlist(list(aCache,bCache))

  # Remove date and accessed time stamps
  dCache <- dCache[!tagKey %in% c("date", "accessed")]
  abCache <- abCache[!tagKey %in% c("date", "accessed")]

  # remove keys
  setkey(dCache, artifact)
  setkey(abCache, artifact)

  expect_true(identical(abCache[, list(artifact, tagKey, tagValue)],
                        dCache[, list(artifact, tagKey, tagValue)]))

  mess <- capture_messages(d1 <- mergeCache(tmpCache, tmpdir))
  expect_true(any(grepl("Skipping", mess)))
  expect_true(identical(d, d1))
})

##########################
test_that("test cache-helpers", {
  testInitOut <- testInit("raster")
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  tmpfile <- tempfile(fileext = ".grd")
  tmpfile2 <- tempfile(fileext = ".grd")
  tmpfile3 <- tempfile(fileext = ".grd")
  r <- raster(extent(0, 5, 0, 5), res = 1, vals = rep(1:2, length.out = 25))
  levels(r) <- data.frame(ID = 1:2, Val = 3:4)
  b <- .prepareFileBackedRaster(r, tmpCache)
  is(b, "RasterLayer")
  expect_true(!nzchar(filename(b)))

  r1 <- raster(extent(0,5,0,5), res = 1, vals = rep(1:2, length.out = 25))
  s <- raster::stack(r, r1)
  b <- .prepareFileBackedRaster(s, tmpCache)

  r <- writeRaster(r, filename = tmpfile, overwrite = TRUE)
  r1 <- writeRaster(r1, filename = tmpfile2, overwrite = TRUE)
  s <- addLayer(r, r1)

  # Test deleted raster backed file
  file.remove(tmpfile2)
  expect_error(b <- .prepareFileBackedRaster(s, tmpCache), "The following file-backed rasters")
  expect_error(b <- .prepareFileBackedRaster(r1, tmpCache), "The following file-backed rasters")

  # Test wrong folder names
  tmpfile <- file.path(tmpCache, basename(tempfile(fileext = ".grd")))
  r <- writeRaster(r, filename = tmpfile, overwrite = TRUE)
  r@file@name <- gsub(pattern = normalizePath(tempdir(), winslash = "/", mustWork = FALSE),
                      normalizePath(tmpfile, winslash = "/", mustWork = FALSE),
                      replacement = basename(tempdir()))
  # show it is not there, so it is the wrong name
  expect_false(file.exists(filename(r)))
  # fix it, by giving correct tmpCache path
  b <- .prepareFileBackedRaster(r, tmpCache)
  expect_true(file.exists(filename(b)))

  # Check that it makes a new name if already in Cache
  checkPath(file.path(tmpCache, "rasters"), create = TRUE)
  r1 <- writeRaster(r1, filename = file.path(tmpCache, "rasters", basename(tmpfile2)), overwrite = TRUE)
  b <- .prepareFileBackedRaster(r1, tmpCache)
  expect_true(identical(normalizePath(filename(b), winslash = "/", mustWork = FALSE),
                        normalizePath(file.path(dirname(filename(r1)),
                                                nextNumericName(basename(filename(r1)))),
                                                winslash = "/", mustWork = FALSE)))

  r <- raster(extent(0, 5, 0, 5), res = 1, vals = rep(1:2, length.out = 25))
  r1 <- raster(extent(0, 5, 0, 5), res = 1, vals = rep(1:2, length.out = 25))
  tmpfile <- tempfile(fileext = ".grd")
  r <- writeRaster(r, filename = tmpfile, overwrite = TRUE)
  r1 <- writeRaster(r1, filename = tmpfile2, overwrite = TRUE)
  s <- addLayer(r, r1)
  b1 <- .prepareFileBackedRaster(s, repoDir = tmpCache)
  expect_true(is(b1, "RasterStack"))
  expect_true(identical(filename(b1), ""))
  expect_true(identical(normalizePath(filename(b1$layer.1), winslash = "/", mustWork = FALSE),
                        normalizePath(file.path(tmpCache, "rasters", basename(filename(r))), winslash = "/", mustWork = FALSE)))

  # Give them same name
  r1 <- writeRaster(r1, filename = tmpfile, overwrite = TRUE)
  b <- raster::stack(r, r1)
  b1 <- .prepareFileBackedRaster(b, tmpCache)
  expect_true(identical(b1$layer.1@file@name, b1$layer.2@file@name))
})

test_that("test useCache = 'overwrite'", {
  testInitOut <- testInit(ask = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  a <- Cache(rnorm, 1, useCache = "overwrite", cacheRepo = tmpCache)
  mess <- capture_messages(b <- Cache(rnorm, 1, useCache = "overwrite", cacheRepo = tmpCache))
  expect_true(!identical(a, b))
  expect_true(any(grepl(pattern = "Overwriting", mess)))

  clearCache(x = tmpCache, ask = FALSE)

  testOnExit(testInitOut)
  testInitOut <- testInit(ask = FALSE, opts = list("reproducible.useCache" = "overwrite"))

  a <- Cache(rnorm, 1, cacheRepo = tmpCache)
  mess <- capture_messages(b <- Cache(rnorm, 1, cacheRepo = tmpCache))
  expect_true(!identical(a, b))
  expect_true(any(grepl(pattern = "Overwriting", mess)))
})

test_that("test rm large non-file-backed rasters", {
  ## This is a large object test!
  skip_on_appveyor()
  skip_on_cran()
  skip_on_travis()

  testInitOut <- testInit(ask = FALSE, opts = list("reproducible.cachePath" = .reproducibleTempCacheDir))
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  st0 <- system.time(r <- Cache(raster, extent(0, 10000, 0, 10000), res = 1, vals = 1, userTags = "first"))
  st1 <- system.time(clearCache(userTags = "first", ask = FALSE))
  expect_true(st1["elapsed"] < 0.75) # This was > 2 seconds in old way
})

test_that("test cc", {
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  testInitOut <- testInit(ask = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  Cache(rnorm, 1, cacheRepo = tmpCache)
  thisTime <- Sys.time()
  Sys.sleep(1) # 0.2
  Cache(rnorm, 2, cacheRepo = tmpCache)
  Cache(rnorm, 3, cacheRepo = tmpCache)
  Cache(rnorm, 4, cacheRepo = tmpCache)
  a <- showCache(x = tmpCache) # shows all 4 entries
  expect_true(length(unique(a$artifact)) == 4)

  cc(ask = FALSE, x = tmpCache)
  b <- showCache(x = tmpCache) # most recent is gone
  expect_true(length(unique(b$artifact)) == 3)

  cc(thisTime, ask = FALSE, x = tmpCache)
  d <- showCache(x = tmpCache) # all those after this time gone, i.e., only 1 left
  expect_true(length(unique(d$artifact)) == 1)

  cc(ask = FALSE, x = tmpCache) # Cache is
  b1 <- showCache(x = tmpCache) # most recent is gone
  expect_true(length(unique(b1$artifact)) == 0)

  mess <- capture_messages(cc(ask = FALSE, x = tmpCache)) # Cache is already empty
  expect_true(any(grepl("Cache already empty", mess)))
})
