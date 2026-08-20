## Tests for methods in R/exportedMethods.R that had no coverage.
##
## These are the S3 methods Cache() dispatches on when preparing an object for
## storage (.wrap/.unwrap), when repointing a file-backed object at new files
## (updateFilenameSlots), and when memoising (makeMemoisable/unmakeMemoisable).
## Several are raster-specific; `raster` is in Suggests, so those construct a
## small dummy RasterLayer rather than relying on a fixture, and skip when the
## package is absent.

test_that(".wrap/.unwrap round-trip an environment", {
  testInit()
  cp <- checkPath(file.path(tmpdir, "cache"), create = TRUE)

  e <- new.env(parent = emptyenv())
  e$a <- 1:3
  e$b <- "x"

  w <- .wrap(e, cachePath = cp, preDigest = list(), cacheId = "wrapEnvId")
  expect_true(is.environment(w))

  u <- .unwrap(w, cachePath = cp, cacheId = "wrapEnvId")
  expect_true(is.environment(u))
  ## Contents survive the round trip unchanged -- this is what Cache() relies on
  ## when an environment is cached and later restored.
  expect_identical(u$a, 1:3)
  expect_identical(u$b, "x")
})

test_that(".wrap.environment drops everything outside outputObjects", {
  testInit()
  cp <- checkPath(file.path(tmpdir, "cache"), create = TRUE)

  e <- new.env(parent = emptyenv())
  e$keep <- 1
  e$drop <- 2

  ## NOTE: this mutates `e` in place (rm(list = ..., envir = obj)), it does not
  ## work on a copy. Asserted here because a caller passing a live environment
  ## will see its contents removed.
  .wrap(e, cachePath = cp, preDigest = list(), cacheId = "outputObjId",
        outputObjects = "keep")

  expect_identical(ls(e), "keep")
  expect_identical(e$keep, 1)
})

test_that("makeMemoisable.data.table returns an independent copy", {
  skip_if_not_installed("data.table")
  testInit()

  dt <- data.table::data.table(a = 1:3, b = letters[1:3])
  mm <- makeMemoisable(dt)

  expect_s3_class(mm, "data.table")
  expect_equal(mm$a, 1:3)

  ## The point of the method: data.table modifies by reference, so a memoised
  ## copy must not track later edits to the original.
  dt[, a := 99L]
  expect_equal(mm$a, 1:3)
  expect_false(identical(mm$a, dt$a))
})

test_that("unmakeMemoisable returns its input unchanged by default", {
  testInit()

  expect_identical(unmakeMemoisable(1:3), 1:3)
  expect_identical(unmakeMemoisable("a"), "a")

  l <- list(a = 1, b = "two")
  expect_identical(unmakeMemoisable(l), l)

  ## The default method is an identity, so a round trip through both is a no-op
  ## for anything without a specialised method.
  expect_identical(unmakeMemoisable(makeMemoisable(l)), l)
})

test_that("updateFilenameSlots repoints a file-backed RasterLayer", {
  skip_if_not_installed("raster")
  testInit("raster")

  r <- raster::raster(raster::extent(0, 10, 0, 10), resolution = 1, vals = 1)
  r <- raster::writeRaster(r, filename = file.path(tmpdir, "r1.grd"), overwrite = TRUE)
  expect_identical(Filenames(r, allowMultiple = FALSE), normPath(file.path(tmpdir, "r1.grd")))

  newf <- file.path(tmpdir, "moved.grd")
  r2 <- updateFilenameSlots(r, newFilenames = newf)

  ## Only the recorded filename changes; this is a slot edit, not a file move.
  expect_identical(Filenames(r2, allowMultiple = FALSE), normPath(newf))
  expect_true(file.exists(file.path(tmpdir, "r1.grd")))

  ## newFilenames is required -- omitting it must be an error, not a silent no-op.
  expect_error(updateFilenameSlots(r), "newFilenames")
})

test_that("updateFilenameSlots accepts a directory instead of filenames", {
  skip_if_not_installed("raster")
  testInit("raster")

  r <- raster::raster(raster::extent(0, 10, 0, 10), resolution = 1, vals = 1)
  r <- raster::writeRaster(r, filename = file.path(tmpdir, "dir1.grd"), overwrite = TRUE)

  newDir <- checkPath(file.path(tmpdir, "elsewhere"), create = TRUE)
  r2 <- updateFilenameSlots(r, newFilenames = newDir)

  ## A single existing directory means "keep the basename, change the folder".
  expect_identical(basename(Filenames(r2, allowMultiple = FALSE)), "dir1.grd")
  expect_identical(dirname(Filenames(r2, allowMultiple = FALSE)), normPath(newDir))
})

test_that("updateFilenameSlots.list handles a list of RasterLayers", {
  skip_if_not_installed("raster")
  testInit("raster")

  mk <- function(nm) {
    rr <- raster::raster(raster::extent(0, 10, 0, 10), resolution = 1, vals = 1)
    raster::writeRaster(rr, filename = file.path(tmpdir, nm), overwrite = TRUE)
  }
  rl <- list(mk("l1.grd"), mk("l2.grd"))

  newf <- c(file.path(tmpdir, "n1.grd"), file.path(tmpdir, "n2.grd"))
  out <- updateFilenameSlots(rl, newFilenames = newf)

  ## The list method routes through raster::stack()/unstack(), so the result
  ## comes back as a list of the same length, one filename updated per element.
  expect_type(out, "list")
  expect_length(out, 2L)
  expect_true(all(vapply(out, inherits, logical(1), "RasterLayer")))
})

test_that("updateFilenameSlots.environment maps over the environment", {
  skip_if_not_installed("raster")
  testInit("raster")

  r <- raster::raster(raster::extent(0, 10, 0, 10), resolution = 1, vals = 1)
  r <- raster::writeRaster(r, filename = file.path(tmpdir, "e1.grd"), overwrite = TRUE)

  ee <- new.env()
  ee$r1 <- r

  out <- updateFilenameSlots(ee, newFilenames = file.path(tmpdir, "eNew.grd"))

  ## The environment method lapply()s and therefore returns a list, not an
  ## environment -- asserted so a change in that shape is caught.
  expect_type(out, "list")
  expect_length(out, 1L)
})
