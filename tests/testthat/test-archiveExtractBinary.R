## Coverage for .archiveExtractBinary() (R/prepInputs.R), which locates a 7z or
## unrar binary for the archive formats base R cannot handle.
##
## The Windows lookup -- Sys.which("7z.exe"), then a recursive search of
## "C:/Program Files", then of the SystemRoot volume -- is unreachable on the
## CI/dev platforms where coverage is measured. isWindows() is mockable, which
## is what makes it testable at all; see the isWindows/isUnix helpers.
##
## No network, no Drive.

test_that(".archiveExtractBinary finds a system binary when one is installed", {
  skip_if_not(nzchar(Sys.which("7z")) || nzchar(Sys.which("unrar")),
              "no 7z/unrar binary to find")
  testInit()

  ## The contract: an absolute path to something usable, or NULL. Never "".
  out <- .archiveExtractBinary(verbose = 0)
  expect_true(is.null(out) || (is.character(out) && nzchar(out)))
  if (!is.null(out)) expect_true(file.exists(out))
})

test_that(".archiveExtractBinary returns NULL on Windows when no binary exists", {
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

  ## NULL, not "" -- callers test with is.null(), and an empty string would slip
  ## through as a valid-looking path.
  expect_null(out)
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
