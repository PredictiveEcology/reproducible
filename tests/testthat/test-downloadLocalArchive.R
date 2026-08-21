## Coverage for downloadFile()'s "we already have the archive" shortcut
## (R/download.R, inside `if (missingNeededFiles)` -> `if (!is.null(archive))`).
##
## The point of that block is to NOT hit the network: when a local copy of the
## archive is on disk and holds everything asked for, the download is skipped
## and the files come out of the local archive instead. Every test here passes a
## deliberately unresolvable url, so if the shortcut stopped working these would
## fail by attempting a real download rather than silently passing. `.invalid`
## is IANA-reserved, so it cannot resolve even behind wildcard DNS.
##
## NOTE ON ENTRY POINT: these drive the exported downloadFile() directly rather
## than prepInputs(). Going through prepInputs does NOT reach this block --
## preProcess runs pp_resolve_needed_files() (which extracts from a local
## archive via .tryExtractFromArchive) a full phase before pp_download(), so by
## the time downloadFile() is called nothing is missing and the shortcut is
## skipped. Verified by tracing .listFilesInArchive: 0 calls via prepInputs,
## 2 via downloadFile(). downloadFile() is exported, so this is public API.
##
## No network, no Drive.

## A two-file zip; returns its path.
mkZip <- function(dir, files = c("a.txt", "b.txt")) {
  checkPath(dir, create = TRUE)
  owd <- setwd(dir); on.exit(setwd(owd), add = TRUE)
  for (f in files) writeLines(f, f)
  suppressWarnings(utils::zip("t.zip", files, flags = "-q"))
  normalizePath(file.path(dir, "t.zip"))
}

## dest holding a valid CHECKSUMS.txt for `files`, but not the files themselves
## -- i.e. "we have been here before, the payload is gone, the archive remains".
seedChecksums <- function(dest, srcDir, files) {
  checkPath(dest, create = TRUE)
  file.copy(file.path(srcDir, files), dest)
  Checksums(path = dest, write = TRUE,
            files = file.path(dest, files), verbose = 0)
  unlink(file.path(dest, files))
}

badUrl <- "https://example.invalid/does-not-exist.zip"

test_that("downloadFile extracts from a local archive instead of downloading", {
  testInit()

  src <- file.path(tmpdir, "src")
  arch <- mkZip(src)
  dest <- file.path(tmpdir, "dest")
  seedChecksums(dest, src, c("a.txt", "b.txt"))
  needed <- file.path(dest, c("a.txt", "b.txt"))

  out <- downloadFile(
    archive = arch, targetFile = "a.txt", neededFiles = needed,
    destinationPath = dest, quick = FALSE,
    checksumFile = file.path(dest, "CHECKSUMS.txt"),
    checkSums = Checksums(path = dest, write = FALSE, files = needed, verbose = 0),
    url = badUrl, needChecksums = 0, preDigest = NULL,
    .tempPath = tempdir2(rndstr(1, 6))
  )

  ## No download was attempted -- an unresolvable url would have errored.
  expect_type(out, "list")
  ## The files came out of the local archive.
  expect_true(all(file.exists(needed)))
  expect_identical(readLines(needed[1], warn = FALSE), "a.txt")
  ## The success path narrows `archive` to the copies that existed locally.
  expect_identical(basename(unlist(out$archive)), "t.zip")
})

test_that("downloadFile reports the archive it used and leaves checksums intact", {
  testInit()

  src <- file.path(tmpdir, "src2")
  arch <- mkZip(src)
  dest <- file.path(tmpdir, "dest2")
  seedChecksums(dest, src, c("a.txt", "b.txt"))
  needed <- file.path(dest, c("a.txt", "b.txt"))

  out <- downloadFile(
    archive = arch, targetFile = "a.txt", neededFiles = needed,
    destinationPath = dest, quick = FALSE,
    checksumFile = file.path(dest, "CHECKSUMS.txt"),
    checkSums = Checksums(path = dest, write = FALSE, files = needed, verbose = 0),
    url = badUrl, needChecksums = 0, preDigest = NULL,
    .tempPath = tempdir2(rndstr(1, 6))
  )

  ## The returned contract other preProcess phases rely on.
  expect_true(all(c("needChecksums", "archive", "neededFiles",
                    "downloaded", "checkSums") %in% names(out)))
  ## CHECKSUMS.txt survives -- it is what made the shortcut usable.
  expect_true(file.exists(file.path(dest, "CHECKSUMS.txt")))
})

test_that("downloadFile falls through to the url when the archive lacks the files", {
  testInit()

  ## haveAll is FALSE: the archive exists but does not contain what was asked
  ## for, so the shortcut must NOT be taken. It has to fall through and try the
  ## url, which fails because the url is unresolvable -- that error IS the
  ## assertion, proving it did not wrongly satisfy the request from the archive.
  src <- file.path(tmpdir, "src3")
  arch <- mkZip(src)
  dest <- checkPath(file.path(tmpdir, "dest3"), create = TRUE)
  needed <- file.path(dest, "not-in-there.txt")

  expect_error(
    suppressWarnings(suppressMessages(downloadFile(
      archive = arch, targetFile = "not-in-there.txt", neededFiles = needed,
      destinationPath = dest, quick = FALSE,
      checksumFile = file.path(dest, "CHECKSUMS.txt"),
      checkSums = .emptyChecksumsResult,
      url = badUrl, needChecksums = 1, preDigest = NULL,
      .tempPath = tempdir2(rndstr(1, 6))
    )))
  )
})
