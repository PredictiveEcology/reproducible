## Coverage for the archive-handling helpers in R/prepInputs.R.
##
## prepInputs()'s archive path is the largest uncovered block in the file, but
## none of it actually needs the network: a two-file zip built in tempdir()
## exercises extension dispatch, listing, and the corrupt-archive recovery.
##
## No network, no Drive.

## A small real zip. Built by cd-ing in, so the archive holds bare filenames
## rather than absolute paths -- which is what the listing assertions check.
mkZip <- function(dir, files = c("a.txt", "b.txt")) {
  owd <- setwd(dir)
  on.exit(setwd(owd), add = TRUE)
  for (f in files) writeLines(f, f)
  zipfile <- file.path(dir, "test.zip")
  suppressWarnings(utils::zip(zipfile, files, flags = "-q"))
  zipfile
}

test_that(".whichExtractFn dispatches on extension and rejects unknown ones", {
  testInit()

  zipfile <- mkZip(tmpdir)

  ## A known archive extension yields a function plus its arguments, which the
  ## caller then does.call()s.
  out <- .whichExtractFn(zipfile, args = NULL)
  expect_type(out, "list")
  expect_true(all(c("fun", "args") %in% names(out)))
  expect_true(is.function(out$fun))

  ## An unknown extension is refused, and the message lists what IS supported --
  ## the only way a caller can tell what to convert to.
  err <- tryCatch(.whichExtractFn(file.path(tmpdir, "x.qqq"), args = NULL),
                  error = function(e) conditionMessage(e))
  expect_match(err, "can only deal with archives")
  for (ext in knownArchiveExtensions) expect_match(err, ext, fixed = TRUE)

  ## NULL archive is "nothing to extract", not an error: prepInputs calls this
  ## unconditionally, before it knows whether there is an archive at all.
  expect_null(.whichExtractFn(NULL, args = NULL))
  expect_null(.whichExtractFn(NA, args = NULL))
})

test_that(".listFilesInArchive lists archive contents", {
  testInit()

  zipfile <- mkZip(tmpdir)
  files <- .listFilesInArchive(zipfile)

  expect_setequal(files, c("a.txt", "b.txt"))
  ## Bare names, not absolute paths -- prepInputs matches targetFile against
  ## these, so a path prefix would break the match.
  expect_false(any(grepl("^/", files)))
})

test_that(".listFilesInArchive deletes a corrupt archive and returns NULL", {
  testInit()

  ## Anything <= 10 bytes cannot be a real archive; it is treated as a failed or
  ## truncated download, removed, and reported -- so the next call re-fetches
  ## rather than failing on the same bad file forever.
  corrupt <- file.path(tmpdir, "corrupt.zip")
  writeLines("x", corrupt)
  expect_true(file.size(corrupt) <= 10)

  mess <- capture_messages(out <- .listFilesInArchive(corrupt))

  expect_null(out)
  expect_false(file.exists(corrupt))
  expect_true(any(grepl("appears corrupted", mess)))
})

test_that("knownArchiveExtensions and its subsets stay consistent", {
  testInit()

  ## These drive .whichExtractFn's dispatch: the "internal" ones can be handled
  ## by base R, the rest need the archive package or a system binary. A member
  ## of a subset that is not in the parent would be unreachable.
  expect_true(all(knownInternalArchiveExtensions %in% knownArchiveExtensions))
  expect_true(all(knownSystemArchiveExtensions %in% knownArchiveExtensions))
  expect_true("zip" %in% knownInternalArchiveExtensions)
})
