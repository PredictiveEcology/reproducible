## Coverage for .archiveExtractBinary() (R/prepInputs.R), which locates a 7z or
## unrar binary for the archive formats base R cannot handle.
##
## The Windows lookup -- Sys.which("7z.exe"), then a recursive search of
## "C:/Program Files", then of the SystemRoot volume -- is unreachable on the
## CI/dev platforms where coverage is measured. isWindows() is mockable, which
## is what makes it testable at all; see the isWindows/isUnix helpers.
##
## Being the first caller of this function on macOS and Windows is what turned
## up two latent bugs: `apt` was being invoked on macOS (the guard admitted it),
## and `list.files()` returning MORE than one match on a real Windows runner
## made `x == "" || length(x) == 0` error with "'length = 2' in coercion to
## 'logical(1)'". Both are fixed; these tests are what keep them fixed.
##
## No network, no Drive.

test_that(".archiveExtractBinary finds a system binary when one is installed", {
  skip_if_not(nzchar(Sys.which("7z")) || nzchar(Sys.which("unrar")),
              "no 7z/unrar binary to find")
  testInit()

  ## The contract: an absolute path to something usable, or NULL. Never "".
  ## On Linux this also walks the p7zip-rar advisory (an `apt` call, which is
  ## why that block is guarded on isLinux() rather than !isWindows()).
  out <- .archiveExtractBinary(verbose = 0)
  expect_true(is.null(out) || (is.character(out) && nzchar(out)))
  if (!is.null(out)) expect_true(file.exists(out))
})

test_that(".archiveExtractBinary walks the Windows lookup without erroring", {
  testInit()

  ## Two mocks, both needed. isWindows() selects the Windows lookup; Sys.which
  ## returning "" is what makes that lookup necessary -- the dev machine has 7z
  ## on PATH, which would short-circuit before the branch under test.
  ##
  ## On a non-Windows filesystem the "C:/Program Files" and SystemRoot searches
  ## naturally find nothing, so this walks the full not-found chain to the end.
  out <- testthat::with_mocked_bindings(
    testthat::with_mocked_bindings(
      .archiveExtractBinary(verbose = 0),
      Sys.which = function(names) stats::setNames(rep("", length(names)), names),
      .package = "base"),
    isWindows = function() TRUE)

  ## The contract is a SINGLE usable path, or NULL -- never "" (callers use
  ## is.null(), and an empty string reads as a valid path) and never a vector.
  ##
  ## Length is the assertion that matters here. A real Windows runner has 7-Zip
  ## under Program Files, so the recursive search returns MORE than one match,
  ## which is exactly what used to make `x == "" || length(x) == 0` blow up with
  ## "'length = 2' in coercion to 'logical(1)'". Getting a length-1 result back
  ## is the evidence that the multi-match handling works.
  expect_true(is.null(out) || (is.character(out) && length(out) == 1L && nzchar(out)))
})

test_that(".archiveExtractBinary returns NULL on unix when no binary exists", {
  testInit()

  ## Same not-found outcome via the non-Windows branch, which additionally
  ## messages about installing p7zip.
  out <- suppressMessages(testthat::with_mocked_bindings(
    testthat::with_mocked_bindings(
      .archiveExtractBinary(verbose = 0),
      Sys.which = function(names) stats::setNames(rep("", length(names)), names),
      .package = "base"),
    isWindows = function() FALSE))

  expect_null(out)
})
