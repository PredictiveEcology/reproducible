## purge = 7 downloads a call's inputs again. "Delete the file and re-run" cannot be the way
## to do that: with reproducible.destinationPathShared set there are two local copies, and a
## stale one left in either is found and reused. Before, purge = 7 only dropped CHECKSUMS.txt
## entries, which were then rebuilt from the files already on disk -- nothing was downloaded,
## and a damaged local file was accepted as correct.

purge7FileUrl <- function(p) {
  p <- gsub("\\\\", "/", normPath(p))
  paste0("file://", ifelse(startsWith(p, "/"), "", "/"), p)
}
purge7Inode <- function(p) as.character(fs::file_info(p)$inode)
purge7Leftovers <- function(dirs) list.files(dirs, pattern = "purge7", all.files = TRUE)

test_that("purge = 7 downloads a changed or damaged file again", {
  testInit(opts = list(reproducible.overwrite = FALSE, reproducible.inputPaths = NULL,
                       reproducible.destinationPathShared = NULL,
                       reproducible.interactiveOnDownloadFail = FALSE))
  src <- checkPath(file.path(tmpdir, "remote"), create = TRUE)
  dest <- checkPath(file.path(tmpdir, "dest"), create = TRUE)
  remote <- file.path(src, "data.csv")
  local <- file.path(dest, "data.csv")
  writeLines("v1", remote)
  prep <- function(...)
    prepInputs(url = purge7FileUrl(remote), targetFile = "data.csv", destinationPath = dest,
               fun = NA, useCache = FALSE, ...)

  prep()
  expect_identical(readLines(local), "v1")

  ## the file at the url changes
  writeLines("v2", remote)
  expect_no_error(prep(purge = 7))
  expect_identical(readLines(local), "v2")
  ## and v2 is what CHECKSUMS.txt now records
  expect_no_error(prep())
  expect_identical(readLines(local), "v2")

  ## a damaged local copy is replaced, not accepted
  writeLines("damaged", local)
  expect_no_error(prep(purge = 7))
  expect_identical(readLines(local), "v2")
  expect_length(purge7Leftovers(dest), 0L)
})

test_that("purge = 7 replaces both copies with destinationPathShared, and only this call's files", {
  skip_on_os("windows") # inodes, and renaming a file that is open
  testInit(opts = list(reproducible.overwrite = FALSE, reproducible.inputPaths = NULL,
                       reproducible.interactiveOnDownloadFail = FALSE))
  src <- checkPath(file.path(tmpdir, "remote"), create = TRUE)
  dest <- checkPath(file.path(tmpdir, "dest"), create = TRUE)
  shared <- checkPath(file.path(tmpdir, "shared"), create = TRUE)
  withr::local_options(reproducible.destinationPathShared = shared)
  remote <- file.path(src, "data.csv")
  other <- file.path(src, "other.csv")
  writeLines("v1", remote)
  writeLines("keep", other)
  prep <- function(f, ...)
    prepInputs(url = purge7FileUrl(f), targetFile = basename(f), destinationPath = dest,
               fun = NA, useCache = FALSE, ...)

  prep(remote)
  prep(other)
  local <- file.path(dest, "data.csv")
  stashed <- file.path(shared, "data.csv")
  expect_true(file.exists(stashed))
  expect_identical(purge7Inode(local), purge7Inode(stashed))
  otherPaths <- c(file.path(dest, "other.csv"), file.path(shared, "other.csv"))
  otherInodes <- purge7Inode(otherPaths)

  ## a process still reading the old file
  con <- file(local, "r")
  withr::defer(close(con))

  writeLines("v2", remote)
  expect_no_error(prep(remote, purge = 7))
  expect_identical(readLines(local), "v2")
  expect_identical(readLines(stashed), "v2")
  ## one fresh inode, shared again -- the stash is not left holding the old one
  expect_identical(purge7Inode(local), purge7Inode(stashed))
  ## the old file was replaced, not truncated under its reader
  expect_identical(readLines(con), "v1")
  ## another input in the same destinationPath and stash is untouched
  expect_identical(purge7Inode(otherPaths), otherInodes)
  expect_length(purge7Leftovers(c(dest, shared)), 0L)
})

test_that("purge = 7 puts the previous copies back if the download fails", {
  withr::local_envvar(NOT_CRAN = "true") # a failed download would otherwise skip() on CRAN
  testInit(opts = list(reproducible.overwrite = FALSE, reproducible.inputPaths = NULL,
                       reproducible.destinationPathShared = NULL,
                       reproducible.interactiveOnDownloadFail = FALSE))
  src <- checkPath(file.path(tmpdir, "remote"), create = TRUE)
  dest <- checkPath(file.path(tmpdir, "dest"), create = TRUE)
  remote <- file.path(src, "data.csv")
  local <- file.path(dest, "data.csv")
  writeLines("v1", remote)
  prep <- function(...)
    prepInputs(url = purge7FileUrl(remote), targetFile = "data.csv", destinationPath = dest,
               fun = NA, useCache = FALSE, ...)
  prep()

  unlink(remote) # the url no longer answers
  expect_error(suppressWarnings(prep(purge = 7)))
  expect_identical(readLines(local), "v1")
  expect_length(purge7Leftovers(dest), 0L)
  ## CHECKSUMS.txt still describes the restored copy, so nothing needs downloading
  expect_no_error(prep())
  expect_identical(readLines(local), "v1")
})

test_that("purge = 7 downloads an archive again and re-extracts from it", {
  testInit(opts = list(reproducible.overwrite = FALSE, reproducible.inputPaths = NULL,
                       reproducible.destinationPathShared = NULL,
                       reproducible.interactiveOnDownloadFail = FALSE))
  src <- checkPath(file.path(tmpdir, "remote"), create = TRUE)
  dest <- checkPath(file.path(tmpdir, "dest"), create = TRUE)
  zipPath <- file.path(src, "arc.zip")
  mkZip <- function(content) {
    unlink(zipPath)
    writeLines(content, file.path(src, "a.csv"))
    owd <- setwd(src)
    on.exit(setwd(owd), add = TRUE)
    zipped <- utils::zip(zipPath, "a.csv", flags = "-q")
    skip_if_not(identical(zipped, 0L), "no zip utility")
  }
  mkZip("v1")
  prep <- function(...)
    prepInputs(url = purge7FileUrl(zipPath), targetFile = "a.csv", destinationPath = dest,
               fun = NA, useCache = FALSE, ...)
  prep()
  expect_identical(readLines(file.path(dest, "a.csv")), "v1")

  mkZip("v2")
  expect_no_error(prep(purge = 7))
  expect_identical(readLines(file.path(dest, "a.csv")), "v2")
  expect_length(purge7Leftovers(dest), 0L)
})

test_that("a Google Drive folder url fetches by CHECKSUMS.txt, and everything on purge = 7", {
  skip_if_not_installed("googledrive")
  testInit(opts = list(reproducible.inputPaths = NULL, reproducible.destinationPathShared = NULL))
  src <- checkPath(file.path(tmpdir, "remote"), create = TRUE)
  dest <- checkPath(file.path(tmpdir, "dest"), create = TRUE)
  writeLines("x1", file.path(src, "x.csv"))
  writeLines("y1", file.path(src, "y.csv"))
  ## no network: the folder is listed from a "mirror" whose files are local file urls
  local_mocked_bindings(
    .driveDirRemap = function(url, ...) "https://mirror.invalid/listing/",
    .bucketDirList = function(...)
      data.frame(name = c("x.csv", "y.csv"),
                 url = purge7FileUrl(file.path(src, c("x.csv", "y.csv"))),
                 stringsAsFactors = FALSE)
  )
  folder <- "https://drive.google.com/drive/folders/1AbCdEfGhIjKlMnOpQrStUvWxYz012345"
  fetch <- function(purge = FALSE)
    downloadRemote(url = folder, archive = NULL, targetFile = NULL,
                   checkSums = .emptyChecksumsResult, fileToDownload = NULL,
                   messSkipDownload = "", destinationPath = dest, overwrite = FALSE,
                   needChecksums = 0, .tempPath = tempdir2(rndstr(1, 6)), preDigest = NULL,
                   alsoExtract = NULL, purge = purge, verbose = 0)

  fetch()
  Checksums(dest, write = TRUE)

  writeLines("x2", file.path(src, "x.csv"))       # the remote copy of x changes
  writeLines("damaged", file.path(dest, "y.csv")) # the local copy of y is damaged
  fetch()
  expect_identical(readLines(file.path(dest, "x.csv")), "x1") # matches its entry: kept
  expect_identical(readLines(file.path(dest, "y.csv")), "y1") # fails its entry: fetched again

  fetch(purge = 7)
  expect_identical(readLines(file.path(dest, "x.csv")), "x2")
  ## the old rows are dropped so the fresh copies get recorded, not rejected
  cs <- read.table(file.path(dest, "CHECKSUMS.txt"), header = TRUE)
  expect_false(any(c("x.csv", "y.csv") %in% cs$file))
})

test_that("preProcess(overwrite) is accepted and ignored, with a one-time deprecation message", {
  testInit(opts = list(reproducible.inputPaths = NULL, reproducible.destinationPathShared = NULL))
  .pkgEnv$.deprecMsgEmitted <- setdiff(.pkgEnv$.deprecMsgEmitted, "preProcess(overwrite)")
  src <- checkPath(file.path(tmpdir, "remote"), create = TRUE)
  dest <- checkPath(file.path(tmpdir, "dest"), create = TRUE)
  writeLines("v1", file.path(src, "data.csv"))
  pp <- function()
    preProcess(url = purge7FileUrl(file.path(src, "data.csv")), targetFile = "data.csv",
               destinationPath = dest, fun = NA, overwrite = TRUE)

  expect_message(pp(), "deprecated and ignored")
  expect_no_message(pp(), message = "deprecated")
  expect_identical(readLines(file.path(dest, "data.csv")), "v1")
})
