## How the cache copies files: what Copy() does with a file-backed SpatRaster, what a memoised
## entry points at, and how a cached file is restored beside other processes.
##
## On 2026-09-28 two FireSense jobs sharing one output folder corrupted a 1.6 GB climate raster.
## Each job's Cache() memoised the UNWRAPPED simList; Copy() then wrote every file-backed raster
## beside its original as "<name>_1.tif" -- the same name in both jobs, because nextNumericName()
## cut names at their first dot -- and the two jobs overwrote each other. Every disk-cache load
## also deleted and rewrote the rasters in that folder.

test_that("nextNumericName() counts the siblings of a name with dots in it", {
  testInit()
  d <- withr::local_tempdir()
  f <- file.path(d, "a_4.2.2updated.tif")
  file.create(f)
  expect_identical(basename(nextNumericName(f)), "a_4.2.2updated_1.tif")
  file.create(file.path(d, "a_4.2.2updated_1.tif"))
  expect_identical(basename(nextNumericName(f)), "a_4.2.2updated_2.tif")
  ## a sibling that only shares a prefix is not counted
  g <- file.path(d, "b.tif")
  file.create(c(g, file.path(d, "bb_7.tif")))
  expect_identical(basename(nextNumericName(g)), "b_1.tif")
})

test_that("Copy() of a file-backed SpatRaster never writes beside the original", {
  skip_if_not_installed("terra")
  testInit("terra", tmpFileExt = ".tif")
  ras <- terra::writeRaster(terra::rast(terra::ext(0, 10, 0, 10), vals = 1), tmpfile, overwrite = TRUE)
  before <- dir(dirname(tmpfile))

  ## default: a fresh temporary directory
  r2 <- Copy(ras)
  expect_equal(terra::values(r2), terra::values(ras))
  expect_false(identical(normPath(dirname(Filenames(r2))), normPath(dirname(tmpfile))))
  expect_identical(dir(dirname(tmpfile)), before)

  ## NULL: no copy; the same file backs both
  r3 <- Copy(ras, filebackedDir = NULL)
  expect_identical(normPath(Filenames(r3)), normPath(tmpfile))
  expect_identical(dir(dirname(tmpfile)), before)

  ## a directory: the copy goes there under its own name; a second copy takes the next number
  d <- withr::local_tempdir()
  r4 <- Copy(ras, filebackedDir = d)
  expect_identical(normPath(Filenames(r4)), normPath(file.path(d, basename(tmpfile))))
  r5 <- Copy(ras, filebackedDir = d)
  expect_identical(basename(Filenames(r5)), paste0(filePathSansExt(basename(tmpfile)), "_1.tif"))
  expect_equal(terra::values(r5), terra::values(ras))
})

test_that("publishFile() leaves a restored file alone and never shows it half-written", {
  testInit()
  src <- withr::local_tempfile(fileext = ".bin")
  writeBin(as.raw(1:200), src)
  d <- withr::local_tempdir()
  to <- file.path(d, "x.bin")

  publishFile(src, to)
  expect_identical(readBin(to, "raw", 1000), as.raw(1:200))
  ctime1 <- file.info(to)$ctime
  ## the same source again: nothing is rewritten
  publishFile(src, to)
  expect_identical(file.info(to)$ctime, ctime1)
  ## a copy that did not keep the date (an earlier version made those) is recognised by content
  Sys.setFileTime(to, Sys.time() - 3600)
  ctime2 <- file.info(to)$ctime
  publishFile(src, to)
  expect_identical(file.info(to)$ctime, ctime2)
  ## a stale file of that name is replaced
  writeBin(as.raw(200:1), to)
  publishFile(src, to)
  expect_identical(readBin(to, "raw", 1000), as.raw(1:200))
  ## no temporary file is left behind, and source == destination is a no-op
  expect_identical(dir(d, all.files = TRUE, no.. = TRUE), "x.bin")
  expect_silent(publishFile(to, to))
})

test_that("a second restore from the cache does not rewrite the file", {
  skip_if_not_installed("terra")
  testInit("terra", opts = list(reproducible.useMemoise = FALSE, reproducible.showSimilar = FALSE))
  withr::local_options(reproducible.cachePath = tmpdir)
  dp <- withr::local_tempdir("maps")
  mk <- function(val) {
    terra::writeRaster(terra::rast(nrows = 10, ncols = 10, vals = val),
                       file.path(dp, "layer.tif"), overwrite = TRUE)
  }
  r1 <- Cache(mk(3), .functionName = "mk")
  r2 <- Cache(mk(3), .functionName = "mk") # first disk hit
  ctime <- file.info(file.path(dp, "layer.tif"))$ctime
  r3 <- Cache(mk(3), .functionName = "mk") # second disk hit: the file is already in place
  expect_identical(file.info(file.path(dp, "layer.tif"))$ctime, ctime)
  expect_equal(terra::values(r3)[1], 3)
  expect_identical(dir(dp, all.files = TRUE, no.. = TRUE), "layer.tif")
})

test_that("memoising a result with a file-backed raster writes nothing and points at the cache", {
  skip_if_not_installed("terra")
  testInit(c("terra", "data.table"),
           opts = list(reproducible.useMemoise = TRUE, reproducible.showSimilar = FALSE))
  withr::local_options(reproducible.cachePath = tmpdir)
  dp <- withr::local_tempdir("maps")
  mk <- function(val) {
    list(r = terra::writeRaster(terra::rast(nrows = 10, ncols = 10, vals = val),
                                file.path(dp, "layer.tif"), overwrite = TRUE),
         dt = data.table::data.table(a = val))
  }
  o1 <- Cache(mk(5), .functionName = "mkL") # a miss: saved, and memoised
  expect_identical(dir(dp), "layer.tif") # no "layer_1.tif" beside it
  cid <- gsub("cacheId:", "", grep("^cacheId:", attr(o1, "tags"), value = TRUE))
  memEnv <- memoiseEnv(tmpdir)
  mem <- get(cid, envir = memEnv)
  expect_false(is(mem$r, "SpatRaster")) # the wrapped form: a path with the cache's tags
  expect_true(any(grepl("filenamesInCache", attr(mem$r, "tags"))))

  o2 <- Cache(mk(5), .functionName = "mkL") # a memoise hit
  expect_s4_class(o2$r, "SpatRaster")
  expect_equal(terra::values(o2$r)[1], 5)
  expect_identical(normPath(terra::sources(o2$r)), normPath(file.path(dp, "layer.tif")))
  expect_identical(dir(dp), "layer.tif")
  ## a snapshot: changing the result by reference does not change the next hit
  data.table::set(o2$dt, j = "a", value = 99)
  o3 <- Cache(mk(5), .functionName = "mkL")
  expect_equal(o3$dt$a, 5)

  ## the same after a disk hit (a fresh session)
  rm(list = cid, envir = memEnv)
  o4 <- Cache(mk(5), .functionName = "mkL")
  expect_false(is(get(cid, envir = memEnv)$r, "SpatRaster"))
  o5 <- Cache(mk(5), .functionName = "mkL")
  expect_equal(terra::values(o5$r)[1], 5)
  expect_identical(dir(dp), "layer.tif")
})

test_that("two processes restoring one cached result into a shared folder do not corrupt it", {
  skip_on_cran()
  skip_on_os("windows")
  skip_if_not_installed("terra")
  testInit("terra", opts = list(reproducible.useMemoise = TRUE, reproducible.showSimilar = FALSE))
  withr::local_options(reproducible.cachePath = tmpdir)
  dp <- withr::local_tempdir("shared")
  ## The function both the parent and the children call, from one file so the cache key agrees.
  ## Its result is a miniature of a SpaDES simList: a class reproducible cannot see into, with its
  ## own .wrap/.unwrap and Filenames methods, memoised through a Copy(). That Copy() is what used to
  ## land beside the originals.
  def <- withr::local_tempfile(fileext = ".R")
  writeLines(c(
    "mk <- function(dp) {",
    "  structure(lapply(c(a = 11, b = 22), function(v) {",
    "    terra::writeRaster(terra::rast(nrows = 300, ncols = 300, vals = v),",
    "                       file.path(dp, paste0('r', v, '.tif')), overwrite = TRUE)",
    "  }), class = 'memoSim')",
    "}",
    "ns <- asNamespace('reproducible')",
    "registerS3method('.wrap', 'memoSim', function(obj, ...)",
    "  structure(reproducible:::.wrap(unclass(obj), ...), class = 'memoSim'), envir = ns)",
    "registerS3method('.unwrap', 'memoSim', function(obj, ...)",
    "  structure(reproducible:::.unwrap(unclass(obj), ...), class = 'memoSim'), envir = ns)",
    "registerS3method('makeMemoisable', 'memoSim',",
    "                 function(x) structure(reproducible::Copy(unclass(x)), class = 'memoSim_'), envir = ns)",
    "registerS3method('unmakeMemoisable', 'memoSim_',",
    "                 function(x) structure(unclass(x), class = 'memoSim'), envir = ns)",
    "methods::setOldClass('memoSim')",
    "methods::setMethod('Filenames', 'memoSim', function(obj, allowMultiple = TRUE, returnList = FALSE)",
    "  reproducible::Filenames(unclass(obj), allowMultiple = allowMultiple, returnList = returnList))"), def)
  source(def, local = TRUE)
  o <- Cache(mk(dp), .functionName = "shared") # cached once, here
  expect_identical(sort(dir(dp)), c("r11.tif", "r22.tif"))

  script <- withr::local_tempfile(fileext = ".R")
  writeLines(c(
    childProcessPreamble(),
    sprintf('options(reproducible.cachePath = "%s", reproducible.useMemoise = TRUE,', tmpdir),
    '        reproducible.showSimilar = FALSE, reproducible.verbose = -1)',
    sprintf('source("%s", local = TRUE)', def),
    sprintf('dp <- "%s"', dp),
    'ok <- TRUE',
    'for (i in 1:12) {',
    '  o <- Cache(mk(dp), .functionName = "shared")',
    '  ok <- ok && all(terra::values(o$a) == 11) && all(terra::values(o$b) == 22)',
    '}',
    'cat(if (ok) "ALLGOOD" else "BAD", "\\n")'), script)
  logs <- file.path(withr::local_tempdir("logs"), c("p1.log", "p2.log"))
  ## two processes at once, both restoring the same entry into the same folder
  Rscript <- shQuote(file.path(R.home("bin"), "Rscript")) # R CMD check refuses a bare "Rscript"
  system(sprintf("%s %s > %s 2>&1 & %s %s > %s 2>&1; wait",
                 Rscript, shQuote(script), shQuote(logs[1]), Rscript, shQuote(script), shQuote(logs[2])))
  for (lg in logs) expect_true(any(grepl("ALLGOOD", readLines(lg))), info = paste(readLines(lg), collapse = "\n"))
  ## nothing but the two rasters: no "_1" copies, no temporary files
  expect_identical(sort(dir(dp, all.files = TRUE, no.. = TRUE)), c("r11.tif", "r22.tif"))
  expect_equal(terra::values(terra::rast(file.path(dp, "r11.tif")))[1], 11)
  expect_equal(terra::values(terra::rast(file.path(dp, "r22.tif")))[1], 22)
})

test_that("a cached SpatRaster keeps its layer order and repeated layers", {
  skip_if_not_installed("terra")
  testInit("terra", opts = list(reproducible.useMemoise = FALSE, reproducible.showSimilar = FALSE))
  withr::local_options(reproducible.cachePath = tmpdir)
  dp <- withr::local_tempdir("stack")
  full <- terra::writeRaster(terra::rast(nrows = 5, ncols = 5, nlyrs = 4, vals = rep(1:4, each = 25)),
                             file.path(dp, "stack.tif"), overwrite = TRUE)
  ## a sample with replacement, out of order: what a hindcast climate stack is
  pick <- function(full) {
    s <- full[[c(3, 1, 1)]]
    names(s) <- c("a", "b", "c")
    s
  }
  s1 <- Cache(pick(full), .functionName = "pick")
  s2 <- Cache(pick(full), .functionName = "pick") # a disk hit
  expect_identical(names(s2), c("a", "b", "c"))
  expect_equal(unname(terra::values(s2)[1, ]), c(3, 1, 1))
})
