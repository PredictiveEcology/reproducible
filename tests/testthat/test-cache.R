test_that("test file-backed raster caching", {
  library(raster)

  tmpdir <- file.path(tempdir(), "test_Cache") %>% checkPath(create = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
  tmpRasterfile <- tempfile(tmpdir = tmpdir, fileext = ".tif")
  file.create(tmpRasterfile)
  tmpRasterfile <- normPath(tmpRasterfile)
  try(clearCache(tmpdir), silent = TRUE)

  nOT <- Sys.time()

  randomPolyToDisk <- function(tmpRasterfile) {
    r <- raster(extent(0, 10, 0, 10), vals = sample(1:30, size = 100, replace = TRUE))
    writeRaster(r, tmpRasterfile, overwrite = TRUE)
    r <- raster(tmpRasterfile)
    r
  }

  a <- randomPolyToDisk(tmpRasterfile)
  # confirm that the raster has the given tmp filename
  expect_identical(strsplit(tmpRasterfile, split = "[\\/]"),
                   strsplit(a@file@name, split = "[\\/]"))
  aa <- Cache(randomPolyToDisk, tmpRasterfile, cacheRepo = tmpdir, userTags = "something2")

  # Test clearCache by tags
  expect_equal(NROW(showCache(tmpdir)[tagKey != "otherFunctions"]), 9)
  clearCache(tmpdir, userTags = "something$")
  expect_equal(NROW(showCache(tmpdir)[tagKey != "otherFunctions"]), 9)
  clearCache(tmpdir, userTags = "something2")
  expect_equal(NROW(showCache(tmpdir)), 0)

  aa <- Cache(randomPolyToDisk, tmpRasterfile, cacheRepo = tmpdir, userTags = "something2")
  expect_equal(NROW(showCache(tmpdir)[tagKey != "otherFunctions"]), 9)
  clearCache(tmpdir, userTags = c("something$", "testing$"))
  expect_equal(NROW(showCache(tmpdir)[tagKey != "otherFunctions"]), 9)
  clearCache(tmpdir, userTags = c("something2$", "testing$"))
  expect_equal(NROW(showCache(tmpdir)[tagKey != "otherFunctions"]), 9)
  clearCache(tmpdir, userTags = c("something2$", "randomPolyToDisk$"))
  expect_equal(NROW(showCache(tmpdir)), 0)

  aa <- Cache(randomPolyToDisk, tmpRasterfile, cacheRepo = tmpdir, userTags = "something2")

  # confirm that the raster has the new filename in the cachePath
  expect_false(identical(strsplit(tmpRasterfile, split = "[\\/]"),
                         strsplit(file.path(tmpdir, "rasters",
                                            basename(tmpRasterfile)), split = "[\\/]")))
  expect_true(any(grepl(pattern = basename(tmpRasterfile),
                        dir(file.path(tmpdir, "rasters")))))

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

  # test that they are identical
  expect_equal(a1, a2)

  # confirm that the second one was obtained through reading from Cache... much faster than writing
  expect_true(b1[1] > b2[1])

  clearCache(tmpdir)

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
  expect_true(NROW(showCache(tmpdir)[tagKey != "otherFunctions"]) == 9)

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

  randomPolyToFactorOnDisk <- function(tmpFile) {
    r <- raster(extent(0, 10, 0, 10), vals = sample(1:30, size = 100, replace = TRUE))
    levels(r) <- data.frame(ID = 1:30, vals = sample(LETTERS[1:5], size = 30, replace = TRUE),
                            vals2 = sample(1:7, size = 30, replace = TRUE))
    r <- writeRaster(r, tmpFile, overwrite = TRUE, datatype = "INT1U")
    r
  }
  tf <- tempfile(fileext = ".grd")
  file.create(tf)
  tf <- normPath(tf)

  # bb1 has original tmp filename
  bb1 <- randomPolyToFactorOnDisk(tf)
  # bb has new one, inside of cache repository, with same basename
  bb <- Cache(randomPolyToFactorOnDisk, tmpFile = tf, cacheRepo = tmpdir)
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

  clearCache(tmpdir)
})

test_that("test memory backed raster robustDigest", {
  library(raster)
  tmpdir <- file.path(tempdir(), "testCache") %>% checkPath(create = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

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
  dig <- reproducible:::.robustDigest(b)

  r1 <- raster(matrix(1:10, 2, 5))
  b1 <- brick(r1, r1)
  dig1 <- reproducible:::.robustDigest(b1)

  expect_identical(dig, dig1)

  b <- writeRaster(b, file = "test", overwrite = TRUE)
  dig <- reproducible:::.robustDigest(b)

  r <- raster(matrix(1:10, 2, 5))
  b <- brick(r, r)
  b <- writeRaster(b, file = "test", overwrite = TRUE)
  dig1 <- reproducible:::.robustDigest(b)

  expect_identical( dig, dig1)

  # Stacks
  dimA <- 100
  r <- raster(matrix(1:dimA, round(sqrt(dimA)), round(sqrt(dimA))))
  b <- raster::stack(r, r)
  dig <- reproducible:::.robustDigest(b)

  r1 <- raster(matrix(1:dimA, round(sqrt(dimA)), round(sqrt(dimA))))
  b1 <- raster::stack(r1, r1)
  dig1 <- reproducible:::.robustDigest(b1)

  expect_identical(dig, dig1)

  r4 <- writeRaster(r, file = "test", overwrite = TRUE)
  r5 <- writeRaster(r, file = "test1", overwrite = TRUE)
  b <- raster::stack(r4, r5)
  dig <- reproducible:::.robustDigest(b)

  r2 <- writeRaster(r1, file = "test", overwrite = TRUE)
  r3 <- writeRaster(r1, file = "test1", overwrite = TRUE)
  b1 <- raster::stack(r2, r3)
  #b1 <- writeRaster(b1, file = "test", overwrite = TRUE)
  dig1 <- reproducible:::.robustDigest(b1)

  expect_identical( dig, dig1)

})

test_that("test date-based cache removal", {
  tmpdir <- file.path(tempdir(), "testCache") %>% checkPath(create = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

  tmpfile <- tempfile(fileext = ".pdf")
  file.create(tmpfile)
  tmpfile <- normPath(tmpfile)
  try(clearCache(tmpdir), silent = TRUE)

  a <- Cache(runif, 1, cacheRepo = tmpdir)
  a1 <- showCache(tmpdir)
  expect_true(NROW(a1) > 0)
  b <- clearCache(tmpdir, before = Sys.Date() - 1)
  expect_true(NROW(b) == 0)
  expect_identical(a1, showCache(tmpdir))

  b <- clearCache(tmpdir, before = Sys.Date() + 1)
  expect_identical(b, a1)
})

test_that("test keepCache", {
  library(raster)

  tmpdir <- file.path(tempdir(), "testCache") %>% checkPath(create = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

  try(clearCache(tmpdir), silent = TRUE)
  Cache(rnorm, 10, cacheRepo = tmpdir)
  Cache(runif, 10, cacheRepo = tmpdir)
  Cache(round, runif(4), cacheRepo = tmpdir)
  expect_true(NROW(showCache(tmpdir)[tagKey != "otherFunctions"]) == 24)
  expect_true(NROW(showCache(tmpdir, c("rnorm", "runif"))) == 0) # and search
  expect_true(NROW(keepCache(tmpdir, "rnorm")[tagKey != "otherFunctions"]) == 8)

  # do it twice to make sure it can deal with repeats
  expect_true(NROW(keepCache(tmpdir, "rnorm")[tagKey != "otherFunctions"]) == 8)
  Sys.sleep(1)
  st <- Sys.time()
  Sys.sleep(1)
  Cache(sample, 10, cacheRepo = tmpdir)
  Cache(length, 10, cacheRepo = tmpdir)
  Cache(sum, runif(4), cacheRepo = tmpdir)
  showCache(tmpdir, after = st)
  expect_true(NROW(showCache(tmpdir, before = st)[tagKey != "otherFunctions"]) == 8)
  expect_true(NROW(keepCache(tmpdir, before = st)[tagKey != "otherFunctions"]) == 8)
  expect_true(NROW(showCache(tmpdir)[tagKey != "otherFunctions"]) == 8)

  ranNums <- Cache(runif, 4, cacheRepo = tmpdir, userTags = "objectName:a")
  ranNums <- Cache(rnorm, 4, cacheRepo = tmpdir, userTags = "objectName:a")
  showCache(tmpdir) # shows spades, runif and rnorm objects

  # shows nothing because object has both spades and rnorm
  expect_true(NROW(showCache(tmpdir, userTags = c("spades", "rnorm"))) == 0)

  # "or" search
  expect_true(length(unique(showCache(tmpdir, userTags = "spades|rnorm")$artifact)) == 2)

  # keep all with spades or rnorm
  expect_true(length(unique(keepCache(tmpdir, userTags = "spades|rnorm")$artifact)) == 2)

  # shows spades, runif and rnorm objects
  expect_true(length(unique(showCache(tmpdir)$artifact)) == 2)
})

test_that("test environments", {
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
  out <- Cache(shortFn, a = a, cacheRepo = tmpdir)
  out2 <- Cache(shortFn, a = b, cacheRepo = tmpdir)
  out3 <- Cache(shortFn, a = g, cacheRepo = tmpdir)
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

  out <- Cache(shortFn, a = a, cacheRepo = tmpdir)
  out2 <- Cache(shortFn, a = b, cacheRepo = tmpdir)
  out3 <- Cache(shortFn, a = g, cacheRepo = tmpdir)
  out4 <- Cache(shortFn, a = i, cacheRepo = tmpdir)
  out5 <- Cache(shortFn, a = i2, cacheRepo = tmpdir)

  # test different values are different
  expect_false(identical(attributes(out), attributes(out2)))

  # test same values but different enviros are same
  expect_true(identical(attributes(out), attributes(out3)))

  # test environment is same as a list
  expect_true(identical(attributes(out), attributes(out4)))

  # test environment is same as recursive list
  expect_true(identical(attributes(out), attributes(out5)))

  df <- data.frame(a = a$a, b = LETTERS[1:10])
  out6 <- Cache(shortFn, a = df, cacheRepo = tmpdir)
  out7 <- Cache(shortFn, a = df, cacheRepo = tmpdir)
  expect_true(identical(attributes(out6), attributes(out7))) # test data.frame
})

test_that("test asPath", {
  library(raster)

  tmpdir <- file.path(tempdir(), "testCache") %>% checkPath(create = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

  try(clearCache(tmpdir), silent = TRUE)
  obj <- 1:10
  origDir <- getwd()
  on.exit(setwd(origDir))
  setwd(tmpdir)
  a1 <- capture_messages(Cache(saveRDS, obj, file = "filename.RData", cacheRepo = tmpdir))
  a2 <- capture_messages(Cache(saveRDS, obj, file = "filename.RData", cacheRepo = tmpdir))
  a3 <- capture_messages(Cache(saveRDS, obj, file = "filename.RData", cacheRepo = tmpdir))

  expect_true(length(a1) == 0)
  expect_true(length(a2) == 0)
  expect_true(grepl("loading cached", a3))

  unlink("filename.RData")
  try(clearCache(tmpdir), silent = TRUE)
  a1 <- capture_messages(Cache(saveRDS, obj, file = asPath("filename.RData"), cacheRepo = tmpdir))
  a2 <- capture_messages(Cache(saveRDS, obj, file = asPath("filename.RData"), cacheRepo = tmpdir))
  a3 <- capture_messages(Cache(saveRDS, obj, file = asPath("filename.RData"),
                               cacheRepo = tmpdir))
  expect_true(length(a1) == 0)
  expect_true(grepl("loading cached", a2))
  expect_true(grepl("loading cached", a3))

  unlink("filename.RData")
  try(clearCache(tmpdir), silent = TRUE)
  a1 <- capture_messages(Cache(saveRDS, obj, file = as("filename.RData", "Path"),
                               cacheRepo = tmpdir))
  a2 <- capture_messages(Cache(saveRDS, obj, file = as("filename.RData", "Path"),
                               cacheRepo = tmpdir))
  a3 <- capture_messages(Cache(saveRDS, obj, file = as("filename.RData", "Path"),
                               cacheRepo = tmpdir))
  expect_true(length(a1) == 0)
  expect_true(grepl("loading cached", a2))
  expect_true(grepl("loading cached", a3))

  setwd(origDir)
  unlink(tmpdir, recursive = TRUE)

  # make several unique environments
})

test_that("test wrong ways of calling Cache", {
  tmpdir <- file.path(tempdir(), "testCache")
  checkPath(tmpdir, create = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

  expect_error(Cache(sample(1), cacheRepo = tmpdir), "Can't understand")
  expect_error(Cache(a <- sample(1), cacheRepo = tmpdir), "Can't understand")
  expect_true(1 == Cache(sample, 1, cacheRepo = tmpdir))
})

test_that("test pipe for Cache", {
  tmpdir <- file.path(tempdir(), "testCache")
  checkPath(tmpdir, create = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

  a <- rnorm(10, 16) %>% mean() %>% prod(., 6) # nolint
  b <- rnorm(10, 16) %>% mean() %>% prod(., 6) %>% Cache(cacheRepo = tmpdir) # nolint
  d <- rnorm(10, 16) %>% mean() %>% prod(., 6) %>% Cache(cacheRepo = tmpdir) # nolint
  expect_true(all.equal(b, d))
  expect_false(isTRUE(all.equal(a, b)))
  d1 <- rnorm(10, 6) %>% mean() %>% prod(., 6) %>% Cache(cacheRepo = tmpdir) # nolint
  expect_false(isTRUE(all.equal(d1, d)))

  d1 <- rnorm(10, 16) %>% mean() %>% prod(., 16) %>% Cache(cacheRepo = tmpdir) # nolint
  expect_false(isTRUE(all.equal(d1, d)))

  d2 <- rnorm(10, 16) %>%
    mean() %>%
    prod(., 16) %>%
    Cache(cacheRepo = tmpdir, notOlderThan = Sys.time())
  expect_false(isTRUE(all.equal(d1, d2)))
})

test_that("test quoted FUN in Cache", {
  tmpdir <- file.path(tempdir(), "testCache")
  checkPath(tmpdir, create = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

  A <- Cache(rnorm, 10, 16, cacheRepo = tmpdir) # nolint

  ## recover cached copies:
  B <- Cache(rnorm, 10, 16, cacheRepo = tmpdir) # nolint
  C <- Cache(quote(rnorm(n = 10, 16)), cacheRepo = tmpdir) # nolint
  D <- rnorm(10, 16) %>% Cache(cacheRepo = tmpdir) # nolint

  expect_true(all.equal(A, B))
  expect_true(all.equal(A, C))
  expect_true(all.equal(A, D))
})

test_that("test multiple pipe Cache calls", {
  tmpdir <- file.path(tempdir(), "testCache")
  checkPath(tmpdir, create = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

  d <- list()
  mess <- list()
  for (i in 1:2) {
    mess[[i]] <- capture_messages(d[[i]] <- rnorm(10, 16) %>%
      mean() %>%
      Cache(cacheRepo = tmpdir) %>%
      prod(., 6) %>%
      Cache(cacheRepo = tmpdir) %>%
      runif(4, 0, .) %>%
      Cache(cacheRepo = tmpdir))
  }
  expect_identical(d[[1]], d[[2]])
  expect_true(length(mess[[1]]) == 0)
  expect_true(length(mess[[2]]) == 3)

  # Removed last step and Cache
  mess <- capture_messages(
    b <- rnorm(10, 16) %>%
      mean() %>%
      Cache(cacheRepo = tmpdir) %>%
      prod(., 6) %>%
      Cache(cacheRepo = tmpdir))
  expect_true(NROW(mess) == 2)

  # Removed last several steps
  mess <- capture_messages(
    b <- rnorm(10, 16) %>%
      mean() %>%
      Cache(cacheRepo = tmpdir))
  expect_true(NROW(mess) == 1)

  # Changed intermediate step
  mess <- capture_messages(rnorm(10, 16) %>%
    mean() %>%
    Cache(cacheRepo = tmpdir) %>%
    prod(., 16) %>%
    Cache(cacheRepo = tmpdir) %>%
    runif(4, 0, .) %>%
    Cache(cacheRepo = tmpdir)
  )
  expect_true(NROW(mess) == 1)

  # Removed intermediate Cache
  mess <- capture_messages(rnorm(10, 16) %>%
    mean() %>%
    Cache(cacheRepo = tmpdir) %>%
    prod(., 6) %>%
    runif(4, 0, .) %>%
    Cache(cacheRepo = tmpdir)
  )
  expect_length(mess, 1)
})


test_that("test masking of %>% error message", {
  tmpdir <- file.path(tempdir(), "testCache")
  checkPath(tmpdir, create = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
  on.exit(try(detach("package:magrittr"), silent = TRUE), add = TRUE)

  mess <- capture_messages(library(magrittr))

  expect_length(grep("package:reproducible", mess, value = TRUE), 1)

  srch <- search()
  whereRepro <- which(endsWith(srch, "reproducible")) - 1
  if (whereRepro > 1) {
    srchNum <- seq_len(whereRepro)
    for (sr in srchNum) {
      masker <- exists("%>%", srch[sr], inherits = FALSE)
      if (masker) break
    }
  }
  expect_true(masker)
  if (interactive()) {
    # somehow, in a non-interactive session, R is finding reproducible::`%>%`
    # even though it is after magrittr on the search path -- somehow reproducible is
    # being kept on top... i.e,. overriding search()
    expect_error(a <- rnorm(10) %>% Cache(cacheRepo = tmpdir))
    detach("package:magrittr")
    expect_silent(a <- rnorm(10) %>% Cache(cacheRepo = tmpdir))
  }
})
