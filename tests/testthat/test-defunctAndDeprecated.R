## Coverage for the retired entry points that are still exported.
##
## These stay in the NAMESPACE so that old code fails with a message naming the
## replacement rather than "could not find function". That message IS the
## contract, so it is what gets asserted -- if one were quietly turned into a
## working function again, or the replacement it names were renamed, these
## catch it.
##
## Cheap to keep: every one of these is a one-line body.
##
## No network, no Drive.

test_that("defunct functions error and name their replacement", {
  testInit()

  ## The gdal* trio was folded into the postProcessTo pipeline.
  expect_error(gdalProject(), "defunct")
  expect_error(gdalProject(), "postProcessTo")
  expect_error(gdalResample(), "projectTo")
  expect_error(gdalMask(), "maskTo")

  ## CacheV2 was the old second-generation entry point; Cache is now the only one.
  expect_error(CacheV2(), "defunct")
  expect_error(CacheV2(), "Cache")

  ## objSizeSession was dropped in favour of lobstr.
  expect_error(objSizeSession(), "defunct")
  expect_error(objSizeSession(), "lobstr")
})

test_that(".file.move warns as deprecated but still moves the file", {
  testInit()

  from <- file.path(tmpdir, "from.txt")
  to <- file.path(tmpdir, "to.txt")
  writeLines("payload", from)

  ## Deprecated, not defunct: it must keep working, so old callers get a warning
  ## rather than a broken move.
  expect_warning(suppressMessages(.file.move(from, to)), "deprecated")

  ## A move, not a copy: the destination has the content and the source is gone.
  expect_true(file.exists(to))
  expect_identical(readLines(to, warn = FALSE), "payload")
  expect_false(file.exists(from))
})
