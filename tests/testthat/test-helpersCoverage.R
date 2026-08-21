## Coverage for small helpers in R/helpers.R that had none.
##
## These are pure or near-pure utilities -- string padding, path predicates,
## namespace probing, retry logic -- so they are cheap to test and cheap to keep
## correct. No network, no Drive.

test_that("paddedFloatToChar pads left and right independently", {
  testInit()

  ## Whole numbers keep no decimal part at all.
  expect_identical(paddedFloatToChar(3), "3")
  ## A fractional value is padded out to padR places.
  expect_identical(paddedFloatToChar(1.5), "1.500")

  ## padL pads the integer part, which is what makes filenames sort correctly.
  expect_identical(paddedFloatToChar(c(1, 10, 100), padL = 3), c("001", "010", "100"))
  expect_length(unique(nchar(paddedFloatToChar(c(1, 10, 100), padL = 3))), 1L)

  ## padR is a MINIMUM, not a maximum: more decimals than requested are kept
  ## rather than rounded away. Worth pinning -- the opposite would silently lose
  ## precision in a filename.
  expect_identical(paddedFloatToChar(1.23456, padR = 2), "1.23456")
})

test_that(".prefix and .suffix change only the basename stem", {
  testInit()

  expect_identical(.prefix("a/b/c.tif", "X_"), "a/b/X_c.tif")
  expect_identical(.suffix("a/b/c.tif", "_X"), "a/b/c_X.tif")

  ## Directory part is untouched, including when it looks like the stem.
  expect_identical(.suffix("c/c.tif", "_X"), "c/c_X.tif")

  ## Empty prefix/suffix is a no-op rather than an error.
  expect_identical(.prefix("a/c.tif"), "a/c.tif")
  expect_identical(.suffix("a/c.tif"), "a/c.tif")

  ## Vectorised.
  expect_identical(.suffix(c("d/a.tif", "d/b.tif"), "_1"), c("d/a_1.tif", "d/b_1.tif"))

  ## A bare filename gains "./" because the result is rebuilt with
  ## file.path(dirname(f), ...) and dirname("a.tif") is ".". Pinned because it
  ## surprises callers who pass basenames.
  expect_identical(.suffix("a.tif", "_1"), "./a_1.tif")
})

test_that("isCI reads the CI environment variable, and is NA when unset", {
  testInit()

  withr::local_envvar(CI = "true")
  expect_true(isCI())

  withr::local_envvar(CI = "false")
  expect_false(isCI())

  ## as.logical("") is NA, so an unset CI yields NA rather than FALSE. Callers
  ## must use isTRUE() around this; pinned because `if (isCI())` would error.
  withr::local_envvar(CI = "")
  expect_true(is.na(suppressWarnings(isCI())))
})

test_that("isDirectory and isFile classify paths and reject non-character", {
  testInit()

  d <- checkPath(file.path(tmpdir, "adir"), create = TRUE)
  f <- file.path(tmpdir, "afile.txt")
  writeLines("x", f)

  expect_true(isDirectory(d))
  expect_false(isDirectory(f))
  expect_true(isFile(f))
  expect_false(isFile(d))

  ## Results are named by the path passed in, which callers use to report which
  ## input was wrong.
  expect_identical(names(isFile(f)), f)

  ## Zero-length input is zero-length output, not an error.
  expect_length(isDirectory(character(0)), 0L)

  ## Wrong argument types are rejected rather than silently coerced.
  expect_error(isDirectory(1), "must be character")
  expect_error(isFile(1), "must be character")
})

test_that(".requireNamespace reports availability and can stop", {
  testInit()

  ## A package that is certainly present.
  expect_true(.requireNamespace("stats"))

  ## An absent package is FALSE by default ...
  expect_false(.requireNamespace("thisPackageDoesNotExist1234", stopOnFALSE = FALSE))
  ## ... and an error when asked to stop, carrying the package name so the user
  ## knows what to install.
  expect_error(
    .requireNamespace("thisPackageDoesNotExist1234", stopOnFALSE = TRUE),
    "thisPackageDoesNotExist1234"
  )
})

test_that("retry returns on success and gives up after the set number of tries", {
  testInit()

  ## Succeeds first time -> the value, no retries.
  expect_identical(retry(quote(42), retries = 3), 42)

  ## Succeeds on the 3rd attempt -> the value, having retried.
  attempts <- 0
  fn <- function() {
    attempts <<- attempts + 1
    if (attempts < 3) stop("not yet")
    "finally"
  }
  expect_identical(
    suppressMessages(retry(quote(fn()), retries = 5, exponentialDecayBase = 1)),
    "finally"
  )
  expect_identical(attempts, 3)

  ## Always fails -> errors, and says how many attempts were made. This is the
  ## path that turns a deterministic failure into a long stall, so the count
  ## matters.
  expect_error(
    suppressMessages(retry(quote(stop("always")), retries = 2, exponentialDecayBase = 1)),
    "Failed after 2 attempts"
  )
})

test_that("rasterRead builds a raster using the configured reader", {
  skip_if_not_installed("terra")
  testInit("terra")

  withr::local_options(reproducible.rasterRead = "terra::rast")
  r <- rasterRead(nrows = 2, ncols = 2, vals = 1)
  expect_s4_class(r, "SpatRaster")
})

test_that("detectActiveCores counts busy processes on unix", {
  skip_on_os("windows")
  testInit()

  ## Counts processes above a CPU threshold; with an impossibly high threshold
  ## nothing qualifies, which exercises the counting path deterministically.
  expect_identical(detectActiveCores(minCPU = 1e6), 0L)

  n <- detectActiveCores()
  expect_type(n, "integer")
  expect_true(n >= 0)
})

test_that("prefixCacheId builds a filename stem; cacheId reads the tag", {
  testInit()

  ## prefixCacheId is a FILENAME helper: cacheId + "_", used to name files in
  ## the cache. Not a "cacheId:" tag, despite the name.
  expect_identical(prefixCacheId("abc"), "abc_")
  ## NULL yields character(0), so paste0()-ing it produces nothing rather than
  ## the string "NULL_".
  expect_identical(prefixCacheId(NULL), character())

  ## cacheId() goes the other way, off an object's `tags` attribute.
  obj <- structure(1, tags = c("function:rnorm", "cacheId:abc123"))
  expect_identical(cacheId(obj), "abc123")

  ## No cacheId tag -> NULL, which callers test with is.null().
  expect_null(cacheId(structure(1, tags = "function:rnorm")))
  expect_null(cacheId(1))
})

test_that(".getDataPath and .getDataPathRecursive delegate to the shared path", {
  testInit()

  ## Deprecated aliases kept for back-compatibility; assert only that they
  ## delegate without error and agree with what they wrap.
  expect_identical(.getDataPath(), .getDestinationPathShared())
  expect_identical(.getDataPathRecursive(), .getDestinationPathSharedRecursive())
})

test_that("isWindows/isUnix/isMac are consistent and mockable", {
  testInit()

  ## .Platform$OS.type is only ever "unix" or "windows", so these are exact
  ## complements. Asserted so a future edit cannot leave them disagreeing.
  expect_type(isWindows(), "logical")
  expect_type(isUnix(), "logical")
  expect_identical(isUnix(), !isWindows())
  expect_false(isWindows() && isMac())

  ## isLinux is NARROWER than isUnix: macOS is unix but not Linux. Keeping them
  ## distinct matters -- the futurePlan/forking paths are Linux-only, so
  ## collapsing isLinux() into isUnix() would enable them on macOS.
  expect_type(isLinux(), "logical")
  expect_false(isLinux() && isMac())
  if (isLinux()) expect_true(isUnix())
  if (isMac()) expect_true(isUnix())

  ## The reason these are functions rather than inline .Platform checks: tests
  ## can override them, so platform-specific branches are reachable everywhere.
  local_mocked_bindings(isWindows = function() TRUE, isUnix = function() FALSE,
                        isLinux = function() FALSE)
  expect_true(isWindows())
  expect_false(isUnix())
  expect_false(isLinux())
})

test_that("mocked platform helpers reach package-internal callers", {
  testInit()

  ## The mock must apply inside the package namespace, not just this frame --
  ## otherwise migrating .Platform checks to helpers would buy nothing.
  probe <- function() if (isWindows()) "win" else "nix"
  environment(probe) <- asNamespace("reproducible")

  expect_identical(probe(), if (isWindows()) "win" else "nix")

  local_mocked_bindings(isWindows = function() TRUE)
  expect_identical(probe(), "win")
})
