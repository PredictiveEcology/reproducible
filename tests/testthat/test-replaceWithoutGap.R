## Replacing a file that other processes are reading must never leave its path missing.
## FireSense, 2026-09-29: two workers on one machine took the same input through purge = 7.
## One moved the file aside while it downloaded a new one; the other, reading it at that moment,
## stopped with "The file does not exist". linkOrCopy() had the same gap (unlink, then link).
##
## The reader is a separate R process that does nothing but look at the file, over and over.
## Separate processes, not forks: see helper-childProcess.R. No network: the "remote" is a file.

fileUrl <- function(p) {
  p <- gsub("\\\\", "/", normPath(p))
  paste0("file://", ifelse(startsWith(p, "/"), "", "/"), p)
}

## Starts a process that, until told to stop, checks that each of `paths` exists and can be read.
startReader <- function(paths, dir) {
  ready <- file.path(dir, "ready")
  stopFile <- file.path(dir, "stop")
  out <- file.path(dir, "out")
  script <- file.path(dir, "reader.R")
  writeLines(c(
    sprintf("paths <- c(%s)", paste0('"', paths, '"', collapse = ", ")),
    "n <- 0L; bad <- 0L",
    sprintf('file.create("%s")', ready),
    sprintf('while (!file.exists("%s")) {', stopFile),
    "  for (p in paths) {",
    "    n <- n + 1L",
    "    ok <- file.exists(p) && !inherits(try(readLines(p, warn = FALSE), silent = TRUE), 'try-error')",
    "    if (!ok) bad <- bad + 1L",
    "  }",
    "}",
    sprintf('writeLines(as.character(c(n, bad)), "%s")', out)), script)
  system2(file.path(R.home("bin"), "Rscript"), shQuote(script), wait = FALSE,
          stdout = file.path(dir, "reader.log"), stderr = file.path(dir, "reader.log"))
  for (i in 1:600) if (file.exists(ready)) break else Sys.sleep(0.05)
  stopifnot(file.exists(ready))
  list(stop = function() {
    file.create(stopFile)
    for (i in 1:600) if (file.exists(out)) break else Sys.sleep(0.05)
    stopifnot(file.exists(out))
    res <- as.integer(readLines(out))
    c(looks = res[1], missing = res[2])
  })
}

test_that("linkOrCopy replaces a file another process is reading without it going missing", {
  skip_on_cran()
  skip_on_os("windows")
  testInit()
  to <- file.path(tmpdir, "target.txt")
  writeLines("v0", to)
  reader <- startReader(to, checkPath(file.path(tmpdir, "reader"), create = TRUE))

  for (i in 1:400) {
    src <- file.path(tmpdir, paste0("src", i, ".txt"))
    writeLines(paste0("v", i), src)
    suppressMessages(linkOrCopy(src, to, verbose = 0))
    unlink(src)
  }
  seen <- reader$stop()

  expect_gt(seen[["looks"]], 0L)
  expect_identical(seen[["missing"]], 0L)
  expect_identical(readLines(to), "v400")
  ## no temporary files left beside it
  expect_identical(sort(dir(tmpdir, all.files = TRUE, no.. = TRUE, pattern = "target")), "target.txt")
})

test_that("linkOrCopy still replaces a target with different content, and keeps an identical one", {
  testInit()
  from <- file.path(tmpdir, "from.txt")
  to <- file.path(tmpdir, "to.txt")
  writeLines("new", from)
  writeLines("old", to)
  expect_true(all(suppressMessages(linkOrCopy(from, to, verbose = 0))))
  expect_identical(readLines(to), "new")

  ## identical content: the target is left as it was, so anything else linked to it keeps sharing it
  skip_on_os("windows")
  inode <- fs::file_info(to)$inode
  copy <- file.path(tmpdir, "copy.txt")
  writeLines("new", copy)
  expect_true(all(suppressMessages(linkOrCopy(copy, to, verbose = 0))))
  expect_identical(fs::file_info(to)$inode, inode)
  expect_length(dir(tmpdir, all.files = TRUE, pattern = "^\\.[^.]"), 0L) # no temporary files
})

test_that("purge = 7 re-downloads without the local copies ever going missing", {
  skip_on_cran()
  skip_on_os("windows")
  testInit(opts = list(reproducible.overwrite = FALSE, reproducible.inputPaths = NULL,
                       reproducible.interactiveOnDownloadFail = FALSE))
  src <- checkPath(file.path(tmpdir, "remote"), create = TRUE)
  dest <- checkPath(file.path(tmpdir, "dest"), create = TRUE)
  shared <- checkPath(file.path(tmpdir, "shared"), create = TRUE)
  withr::local_options(reproducible.destinationPathShared = shared)
  remote <- file.path(src, "data.csv")
  writeLines("v0", remote)
  prep <- function(...)
    prepInputs(url = fileUrl(remote), targetFile = "data.csv", destinationPath = dest,
               fun = NA, useCache = FALSE, verbose = -1, ...)
  prep()
  local <- file.path(dest, "data.csv")
  stashed <- file.path(shared, "data.csv")
  expect_true(file.exists(stashed))

  reader <- startReader(c(local, stashed), checkPath(file.path(tmpdir, "reader"), create = TRUE))
  for (i in 1:8) {
    writeLines(paste0("v", i), remote)
    prep(purge = 7)
  }
  seen <- reader$stop()

  expect_gt(seen[["looks"]], 0L)
  expect_identical(seen[["missing"]], 0L)
  expect_identical(readLines(local), "v8")
  expect_identical(readLines(stashed), "v8")
  expect_length(dir(c(dest, shared), pattern = "purge7", all.files = TRUE), 0L)
})

test_that("a Drive folder purge = 7 replaces the stash copies instead of unlinking them", {
  skip_if_not_installed("googledrive")
  skip_on_os("windows")
  testInit(opts = list(reproducible.inputPaths = NULL))
  src <- checkPath(file.path(tmpdir, "remote"), create = TRUE)
  dest <- checkPath(file.path(tmpdir, "dest"), create = TRUE)
  shared <- checkPath(file.path(tmpdir, "shared"), create = TRUE)
  withr::local_options(reproducible.destinationPathShared = shared)
  writeLines("x1", file.path(src, "x.csv"))
  local_mocked_bindings(
    .driveDirRemap = function(url, ...) "https://mirror.invalid/listing/",
    .bucketDirList = function(...)
      data.frame(name = "x.csv", url = fileUrl(file.path(src, "x.csv")), stringsAsFactors = FALSE)
  )
  fetch <- function(purge = FALSE)
    downloadRemote(url = "https://drive.google.com/drive/folders/1AbCdEfGhIjKlMnOpQrStUvWxYz012345",
                   archive = NULL, targetFile = NULL, checkSums = .emptyChecksumsResult,
                   fileToDownload = NULL, messSkipDownload = "", destinationPath = dest,
                   overwrite = FALSE, needChecksums = 0, .tempPath = tempdir2(rndstr(1, 6)),
                   preDigest = NULL, alsoExtract = NULL, purge = purge, verbose = 0)
  fetch()
  file.copy(file.path(dest, "x.csv"), file.path(shared, "x.csv"))
  reader <- startReader(file.path(shared, "x.csv"), checkPath(file.path(tmpdir, "reader"), create = TRUE))
  writeLines("x2", file.path(src, "x.csv"))
  fetch(purge = 7)
  seen <- reader$stop()
  expect_identical(seen[["missing"]], 0L)
  expect_identical(readLines(file.path(shared, "x.csv")), "x2")
  expect_identical(readLines(file.path(dest, "x.csv")), "x2")
})
