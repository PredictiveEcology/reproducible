test_that("destinationPathShared links archive-derived files across destinations", {
  skip_on_cran()
  testInit("terra")

  ## The shared stash is scoped per archive by .sharedDirsFor(), and runChecksums()
  ## reads back from that scoped directory. appendChecksumsTable() used to write its
  ## copy of the rows to the UNSCOPED shared root, where the read side never looks. An
  ## archive-derived file was therefore never found in the stash: preProcess
  ## re-downloaded, then hit the "already exists at <stash>" branch in downloadRemote(),
  ## which errors under the default overwrite = FALSE. Every study area sharing one
  ## destinationPathShared kept its own copy of every archive-derived input.
  src <- checkPath(file.path(tmpdir, "src"), create = TRUE)

  mkZip <- function(name, nested) {
    d <- file.path(src, name)
    inner <- if (nested) file.path(d, name) else d
    checkPath(inner, create = TRUE)
    writeBin(as.raw(rep(1:255, length.out = 1e5)), file.path(inner, paste0(name, ".bin")))
    owd <- setwd(d)
    on.exit(setwd(owd), add = TRUE)
    zipped <- utils::zip(file.path(src, paste0(name, ".zip")),
                         if (nested) name else paste0(name, ".bin"), flags = "-rq")
    skip_if_not(identical(zipped, 0L), "no zip utility")
    file.path(src, paste0(name, ".zip"))
  }

  ## fs, not `stat -c`: that spelling is GNU-only and returns nothing on macOS, where
  ## BSD stat wants `-f %l`. fs is already an Import and reports hard_links everywhere.
  nlink <- function(p) as.integer(fs::file_info(p)$hard_links)

  linksAcrossDestinations <- function(zip, label) {
    shared <- checkPath(file.path(tmpdir, paste0("shared_", label)), create = TRUE)
    withr::local_options(reproducible.destinationPathShared = shared)
    vapply(1:3, function(i) {
      dest <- checkPath(file.path(tmpdir, paste0(label, i)), create = TRUE)
      prepInputs(url = paste0("file://", zip), destinationPath = dest,
                 fun = NA, useCache = FALSE)
      f <- list.files(dest, pattern = "[.]bin$", recursive = TRUE, full.names = TRUE)
      expect_length(f, 1L)
      nlink(f[1L])
    }, integer(1L))
  }

  ## One inode in the stash, one extra link per destination: 2, 3, 4.
  ## Both archive shapes matter -- a top-level folder inside the zip is what
  ## CA_FAO_forest_2019.zip looks like, files at the root is CA_forest_VLCE2_2000.zip.
  expect_equal(linksAcrossDestinations(mkZip("nestarc", TRUE), "nested"), 2:4)
  expect_equal(linksAcrossDestinations(mkZip("flatarc", FALSE), "flat"), 2:4)
})

test_that("concurrent consumers of one archive converge on one inode in the shared stash", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("filelock")
  testInit()

  ## Filling destinationPathShared for one input is a multi-step transaction -- read
  ## CHECKSUMS.txt, download, extract, hardlink in, append rows -- and nothing serialised
  ## it. Every process read the stash before any of them had written it, so every process
  ## kept a private copy: 15 workers on one 0.78 GB input left 84 paths across 34 inodes
  ## (measured, 2026-09-09), which is the opposite of what the stash is for. A reader
  ## could also catch the stash half-written and match a CHECKSUMS.txt row whose file was
  ## not linked in yet, failing with "No archive exists with filename: <stash>/x.zip".
  ##
  ## Separate R processes, not parallel::mclapply: the defect is a filesystem race between
  ## processes, and forked children of one session share too much to exercise it.
  src <- checkPath(file.path(tmpdir, "src"), create = TRUE)
  inner <- checkPath(file.path(src, "arc", "arc"), create = TRUE)
  writeBin(as.raw(sample(0:255, 2e5, TRUE)), file.path(inner, "arc.bin"))
  owd <- setwd(file.path(src, "arc"))
  zipped <- utils::zip(file.path(src, "arc.zip"), "arc", flags = "-rq")
  setwd(owd)
  skip_if_not(identical(zipped, 0L), "no zip utility")

  shared <- checkPath(file.path(tmpdir, "shared"), create = TRUE)
  libs <- .libPaths()
  n <- 6L
  script <- file.path(tmpdir, "consumer.R")
  ## Under devtools/pkgload the installed reproducible is not the one being tested, and a
  ## fresh child process would silently load the installed one -- so the child loads the
  ## same source tree the parent did.
  devPath <- if (isNamespaceLoaded("pkgload") &&
                 isTRUE(pkgload::is_dev_package("reproducible")))
    normalizePath(getNamespaceInfo("reproducible", "path"), mustWork = FALSE)
  loadLine <- if (!is.null(devPath) && file.exists(file.path(devPath, "DESCRIPTION")))
    sprintf('pkgload::load_all("%s", quiet = TRUE)', devPath) else
      'suppressMessages(library(reproducible))'
  writeLines(c(
    sprintf('.libPaths(%s)', paste0("c(", paste0('"', libs, '"', collapse = ", "), ")")),
    loadLine,
    'a <- commandArgs(trailingOnly = TRUE)',
    sprintf('options(reproducible.destinationPathShared = "%s", reproducible.verbose = -1)', shared),
    'dest <- a[1]; dir.create(dest, recursive = TRUE, showWarnings = FALSE)',
    sprintf('prepInputs(url = "file://%s", destinationPath = dest, fun = NA, useCache = FALSE)',
            file.path(src, "arc.zip"))
  ), script)

  dests <- file.path(tmpdir, paste0("dest", seq_len(n)))
  logs  <- file.path(tmpdir, paste0("log", seq_len(n), ".txt"))
  Rscript <- file.path(R.home("bin"), "Rscript")
  ## Launched with wait = FALSE so they overlap; the marker line is how a finished
  ## process is told from one still running, since the exit status is not returned here.
  Map(function(d, lg) system2(Rscript, c(shQuote(script), shQuote(d)),
                              stdout = lg, stderr = lg, wait = FALSE), dests, logs)
  deadline <- Sys.time() + 300
  repeat {
    done <- file.exists(file.path(dests, "arc", "arc.bin")) |
      vapply(logs, function(lg) any(grepl("^Error", readLines(lg, warn = FALSE))), logical(1))
    if (all(done) || Sys.time() > deadline) break
    Sys.sleep(1)
  }

  got <- file.path(dests, "arc", "arc.bin")
  ## Every consumer gets its file -- concurrency is not an error.
  errs <- unlist(lapply(logs, function(lg) grep("^Error", readLines(lg, warn = FALSE), value = TRUE)))
  expect_length(errs, 0L)
  expect_true(all(file.exists(got)))
  ## ...and all of them, plus the stash copy, are the SAME inode: one file on disk.
  expect_equal(length(unique(fs::file_info(got)$inode)), 1L)
  expect_equal(unique(as.integer(fs::file_info(got)$hard_links)), n + 1L)
})
