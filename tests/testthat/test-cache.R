test_that("test file-backed raster caching", {
  library(magrittr)
  library(raster)

  tmpdir <- file.path(tempdir(), "testCache") %>% checkPath(create = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
  tmpRasterfile <- tempfile(tmpdir = tmpdir, fileext = ".tif")
  file.create(tmpRasterfile)
  tmpRasterfile <- normPath(tmpRasterfile)
  try(clearCache(tmpdir), silent = TRUE)

  nOT <- Sys.time()

  randomPolyToDisk <- function(tmpdir, tmpRasterfile) {
    r <- raster(extent(0, 10, 0, 10), vals = sample(1:30, size = 100, replace = TRUE))
    #r <- randomPolygons(numTypes = 30)
    writeRaster(r, tmpRasterfile, overwrite = TRUE)
    r <- raster(tmpRasterfile)
    r
  }

  a <- randomPolyToDisk(tmpdir, tmpRasterfile)
  # confirm that the raster has the given tmp filename
  expect_identical(strsplit(tmpRasterfile, split = "[\\/]"),
                   strsplit(a@file@name, split = "[\\/]"))
  aa <- suppressWarnings(Cache(randomPolyToDisk, tmpdir, tmpRasterfile, cacheRepo = tmpdir, userTags = "something2"))

  # Test clearCache by tags
  expect_equal(NROW(showCache(tmpdir)), 9)
  suppressWarnings(clearCache(tmpdir, userTags = "something$"))
  expect_equal(NROW(showCache(tmpdir)), 9)
  suppressWarnings(clearCache(tmpdir, userTags = "something2"))
  expect_equal(NROW(showCache(tmpdir)), 0)

  aa <- suppressWarnings(Cache(randomPolyToDisk, tmpdir, tmpRasterfile, cacheRepo = tmpdir, userTags = "something2"))
  expect_equal(NROW(showCache(tmpdir)), 9)
  suppressWarnings(clearCache(tmpdir, userTags = c("something$", "testing$")))
  expect_equal(NROW(showCache(tmpdir)), 9)
  suppressWarnings(clearCache(tmpdir, userTags = c("something2$", "testing$")))
  expect_equal(NROW(showCache(tmpdir)), 9)
  suppressWarnings(clearCache(tmpdir, userTags = c("something2$", "randomPolyToDisk$")))
  expect_equal(NROW(showCache(tmpdir)), 0)

  aa <- suppressWarnings(Cache(randomPolyToDisk, tmpdir, tmpRasterfile, cacheRepo = tmpdir, userTags = "something2"))

  # confirm that the raster has the new filename in the cachePath
  expect_false(identical(strsplit(tmpRasterfile, split = "[\\/]"),
                         strsplit(file.path(tmpdir, "rasters", basename(tmpRasterfile)), split = "[\\/]")))
  expect_true(any(grepl(pattern = basename(tmpRasterfile),
                        dir(file.path(tmpdir, "rasters")))))

  # Caching a raster as an input works
  rasterTobinary <- function(raster) {
    ceiling(raster[] / (mean(raster[]) + 1))
  }
  nOT <- Sys.time()
  for (i in 1:2) {
    assign(paste0("b", i), system.time(
      assign(paste0("a", i), suppressWarnings(Cache(rasterTobinary, aa, cacheRepo = tmpdir, notOlderThan = nOT)))
    ))
    nOT <- Sys.time() - 100
  }

  # test that they are identical
  expect_equal(a1, a2)

  # confirm that the second one was obtained through reading from Cache... much faster than writing
  expect_true(b1[1] > b2[1])

  suppressWarnings(clearCache(tmpdir))

  # Check that Caching of rasters saves them to tif file instead of rdata
  randomPolyToMemory <- function(tmpdir) {
    r <- raster(extent(0, 10, 0, 10), vals = sample(1:30, size = 100, replace = TRUE))
    #r <- randomPolygons(numTypes = 30)
    dataType(r) <- "INT1U"
    r
  }

  bb <- suppressWarnings(Cache(randomPolyToMemory, tmpdir, cacheRepo = tmpdir))
  expect_true(filename(bb) == "")
  expect_true(inMemory(bb))

  bb <- suppressWarnings(Cache(randomPolyToMemory, tmpdir, cacheRepo = tmpdir))
  expect_true(NROW(showCache(tmpdir)) == 9)

  # Test that factors are saved correctly
  randomPolyToFactorInMemory <- function(tmpdir) {
    r <- raster(extent(0, 10, 0, 10), vals = sample(1:30, size = 100, replace = TRUE))
    levels(r) <- data.frame(ID = 1:30, vals = sample(LETTERS[1:5], size = 30,replace = TRUE),
                            vals2 <- sample(1:7, size = 30, replace = TRUE))
    dataType(r) <- "INT1U"
    r
  }
  bb <- suppressWarnings(Cache(randomPolyToFactorInMemory, tmpdir, cacheRepo = tmpdir))
  expect_true(dataType(bb) == "INT1U")
  expect_true(raster::is.factor(bb))
  expect_true(is(raster::levels(bb)[[1]], "data.frame"))
  expect_true(NCOL(raster::levels(bb)[[1]]) == 3)
  expect_true(NROW(raster::levels(bb)[[1]]) == 30)

  randomPolyToFactorOnDisk <- function(tmpdir, tmpFile) {
    r <- raster(extent(0, 10, 0, 10), vals = sample(1:30, size = 100, replace = TRUE))
    levels(r) <- data.frame(ID = 1:30, vals = sample(LETTERS[1:5], size = 30, replace = TRUE),
                            vals2 = sample(1:7, size = 30, replace = TRUE))
    r <- writeRaster(r, tmpFile, overwrite = TRUE, datatype = "INT1U")
    #r <- raster(tmpRasterfile)
    r
  }
  tf <- tempfile(fileext = ".grd")
  file.create(tf)
  tf <- normPath(tf)

  # bb1 has original tmp filename
  bb1 <- randomPolyToFactorOnDisk(tmpdir, tf)
  # bb has new one, inside of cache repository, with same basename
  bb <- suppressWarnings(Cache(randomPolyToFactorOnDisk, tmpdir, tmpFile = tf, cacheRepo = tmpdir))
  expect_true(dirname(filename(bb)) == file.path(tmpdir, "rasters"))
  expect_true(basename(filename(bb)) == basename(tf))
  expect_false(filename(bb) == tf)
  expect_true(dirname(filename(bb1)) == dirname(tf))
  expect_true(basename(filename(bb1)) == basename(tf))
  expect_true(dataType(bb) == "INT1U")
  expect_true(raster::is.factor(bb))
  expect_true(is(raster::levels(bb)[[1]], "data.frame"))
  expect_true(NCOL(raster::levels(bb)[[1]]) == 3)
  expect_true(NROW(raster::levels(bb)[[1]]) == 30)

  suppressWarnings(clearCache(tmpdir))
})

test_that("test date-based cache removal", {
  library(magrittr)
  tmpdir <- file.path(tempdir(), "testCache") %>% checkPath(create = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

  tmpfile <- tempfile(fileext = ".pdf")
  file.create(tmpfile)
  tmpfile <- normPath(tmpfile)
  try(clearCache(tmpdir), silent = TRUE)

  a <- suppressWarnings(Cache(runif, 1, cacheRepo = tmpdir))
  a1 <- showCache(tmpdir)
  expect_true(NROW(a1) > 0)
  b <- clearCache(tmpdir, before = Sys.Date() - 1)
  expect_true(NROW(b) == 0)
  expect_identical(a1, showCache(tmpdir))

  b <- suppressWarnings(clearCache(tmpdir, before = Sys.Date() + 1))
  expect_identical(b, a1)
})

test_that("test keepCache", {
  library(magrittr)
  library(raster)

  tmpdir <- file.path(tempdir(), "testCache") %>% checkPath(create = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

  try(clearCache(tmpdir), silent = TRUE)
  suppressWarnings(Cache(rnorm, 10, cacheRepo = tmpdir))
  suppressWarnings(Cache(runif, 10, cacheRepo = tmpdir))
  suppressWarnings(Cache(round, runif(4), cacheRepo = tmpdir))
  expect_true(NROW(showCache(tmpdir)) == 24)
  expect_true(NROW(showCache(tmpdir, c("rnorm","runif"))) == 0) # and search
  expect_true(NROW(suppressWarnings(keepCache(tmpdir, "rnorm"))) == 8)
  # do it twice to make sure it can deal with repeats
  expect_true(NROW(keepCache(tmpdir, "rnorm")) == 8)
  Sys.sleep(1)
  st <- Sys.time()
  Sys.sleep(1)
  suppressWarnings(Cache(sample, 10, cacheRepo = tmpdir))
  suppressWarnings(Cache(length, 10, cacheRepo = tmpdir))
  suppressWarnings(Cache(sum, runif(4), cacheRepo = tmpdir))
  showCache(tmpdir, after = st)
  expect_true(NROW(showCache(tmpdir, before = st)) == 8)
  expect_true(NROW(suppressWarnings(keepCache(tmpdir, before = st))) == 8)
  expect_true(NROW(showCache(tmpdir)) == 8)

  ranNums <- suppressWarnings(Cache(runif, 4, cacheRepo = tmpdir, userTags = "objectName:a"))
  ranNums <- suppressWarnings(Cache(rnorm, 4, cacheRepo = tmpdir, userTags = "objectName:a"))
  showCache(tmpdir) # shows spades, runif and rnorm objects
  expect_true(NROW(showCache(tmpdir, userTags = c("spades","rnorm"))) == 0) # shows nothing because object has both spades and rnorm
  expect_true(length(unique(showCache(tmpdir, userTags = "spades|rnorm")$artifact)) == 2) # "or" search
  expect_true(length(unique(suppressWarnings(keepCache(tmpdir, userTags = "spades|rnorm")$artifact))) == 2)  # keep all with spades or rnorm
  expect_true(length(unique(showCache(tmpdir)$artifact)) == 2) # shows spades, runif and rnorm objects
})

test_that("test environments", {
  library(magrittr)
  library(raster)

  tmpdir <- file.path(tempdir(), "testCache") %>% checkPath(create = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

  try(clearCache(tmpdir), silent = TRUE)
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
  out <- suppressWarnings(Cache(shortFn, a = a, cacheRepo = tmpdir))
  out2 <- suppressWarnings(Cache(shortFn, a = b, cacheRepo = tmpdir))
  out3 <- suppressWarnings(Cache(shortFn, a = g, cacheRepo = tmpdir))
  expect_true(identical(attributes(out), attributes(out2)))
  expect_true(identical(attributes(out), attributes(out3)))

  # put 2 different values, 1 and 2 ... a's and g's envirs are same value, but
  #    different environment .. b's envir is different value
  a$d$a <- 1
  b$d$a <- 2
  g$d$a <- 1
  i <- as.list(a)
  i2 <- i
  i2$d <- as.list(i2$d)

  out <- suppressWarnings(Cache(shortFn, a = a, cacheRepo = tmpdir))
  out2 <- suppressWarnings(Cache(shortFn, a = b, cacheRepo = tmpdir))
  out3 <- suppressWarnings(Cache(shortFn, a = g, cacheRepo = tmpdir))
  out4 <- suppressWarnings(Cache(shortFn, a = i, cacheRepo = tmpdir))
  out5 <- suppressWarnings(Cache(shortFn, a = i2, cacheRepo = tmpdir))

  expect_false(identical(attributes(out), attributes(out2))) # test different values are different
  expect_true(identical(attributes(out), attributes(out3))) # test same values but different enviros are same
  expect_true(identical(attributes(out), attributes(out4))) # test environment is same as a list
  expect_true(identical(attributes(out), attributes(out5))) # test environment is same as recursive list

  df <- data.frame(a = a$a, b = LETTERS[1:10])
  out6 <- suppressWarnings(Cache(shortFn, a = df, cacheRepo = tmpdir))
  out7 <- suppressWarnings(Cache(shortFn, a = df, cacheRepo = tmpdir))
  expect_true(identical(attributes(out6), attributes(out7))) # test data.frame
})
