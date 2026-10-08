test_that("a failing dlFun errors and leaves no file at targetFile", {
  testInit("digest")
  dest <- file.path(tmpdir, "dest")
  dir.create(dest)
  bad <- function() stop("dlFun boom")
  expect_error(
    prepInputs(targetFile = "x.rds", destinationPath = dest, dlFun = bad,
               fun = "base::readRDS", verbose = -1),
    "boom")
  expect_false(file.exists(file.path(dest, "x.rds")))

  ## fixing dlFun then works
  good <- function() {
    saveRDS(1:3, file.path(dest, "x.rds"))
    NULL
  }
  out <- prepInputs(targetFile = "x.rds", destinationPath = dest, dlFun = good,
                    fun = "base::readRDS", verbose = -1)
  expect_identical(out, 1:3)
})

test_that("a quoted-call dlFun is evaluated once and writes where prepInputs looks", {
  testInit("digest")
  counter <- 0L
  writeIt <- function(tbl, targetFile, destinationPath) {
    counter <<- counter + 1L
    saveRDS(tbl, file.path(destinationPath, targetFile))
    TRUE
  }
  tbl <- 4:6

  ## no shared store
  dest <- file.path(tmpdir, "dest")
  dir.create(dest)
  out <- prepInputs(targetFile = "a.rds", destinationPath = dest,
                    dlFun = quote(writeIt(tbl, targetFile, destinationPath)),
                    fun = "base::readRDS", verbose = -1)
  expect_identical(counter, 1L)
  expect_true(file.exists(file.path(dest, "a.rds")))

  ## shared store
  skip_on_os("windows")
  shared <- file.path(tmpdir, "shared")
  dir.create(shared)
  dest2 <- file.path(tmpdir, "dest2")
  dir.create(dest2)
  withr::local_options(reproducible.destinationPathShared = shared)
  counter <- 0L
  out <- prepInputs(targetFile = "b.rds", destinationPath = dest2,
                    dlFun = quote(writeIt(tbl, targetFile, destinationPath)),
                    fun = "base::readRDS", verbose = -1)
  expect_identical(counter, 1L)
  expect_true(file.exists(file.path(shared, "b.rds")))
  expect_true(file.exists(file.path(dest2, "b.rds")))
  expect_identical(file.info(file.path(shared, "b.rds"))$inode,
                   file.info(file.path(dest2, "b.rds"))$inode)
})

test_that("a quoted-call dlFun sees `...` values and the calling environment's enclosures", {
  testInit("digest")
  dest <- file.path(tmpdir, "dest")
  dir.create(dest)
  mk <- function() {
    outer <- 7L # only in this enclosing frame
    function() {
      writeIt <- function(v, o, d, f) {
        saveRDS(v + o, file.path(d, f))
        NULL
      }
      prepInputs(targetFile = "c.rds", destinationPath = dest, myVal = 3L,
                 dlFun = quote(writeIt(myVal, outer, destinationPath, targetFile)),
                 fun = "base::readRDS", verbose = -1)
    }
  }
  expect_identical(mk()(), 10L)
})

test_that("a quoted-call dlFun that fails in the first environment still uses the frame search", {
  testInit("digest")
  dest <- file.path(tmpdir, "dest")
  dir.create(dest)
  ## `writeIt` and `onlyHere` live in a calling frame that is not lexically visible from `inner`
  inner <- function() {
    prepInputs(targetFile = "e.rds", destinationPath = dest,
               dlFun = quote(writeIt(onlyHere, dest)),
               fun = "base::readRDS", verbose = -1)
  }
  outer <- function() {
    onlyHere <- 11L
    writeIt <- function(v, d) {
      saveRDS(v, file.path(d, "e.rds"))
      NULL
    }
    inner()
  }
  expect_identical(outer(), 11L)
})
