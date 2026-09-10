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
  ## same source tree the parent did. An installed package has R/reproducible.rdb where a
  ## source tree has R/*.R; that is the difference, and it needs no extra dependency to
  ## ask (pkgload is not one of reproducible's).
  pkgPath <- normalizePath(getNamespaceInfo("reproducible", "path"), mustWork = FALSE)
  fromSource <- !file.exists(file.path(pkgPath, "R", "reproducible.rdb")) &&
    file.exists(file.path(pkgPath, "DESCRIPTION"))
  loadLine <- if (fromSource)
    sprintf('library(pkgload); load_all("%s", quiet = TRUE)', pkgPath) else
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

test_that("a destination arriving later does not replace the copy already in the stash", {
  skip_on_cran()
  testInit()

  ## linkOrCopy() unlinks an existing target before linking. Passing it a `to` that the
  ## stash already holds therefore DELETES the shared copy and replaces it with a link to
  ## the current caller's file -- orphaning every destination that had linked to the old
  ## inode, which the next caller then does to this one. On a 15-worker run
  ## CA_FAO_forest_2019.tif collected three inodes in twenty minutes, each stranded at
  ## nlink = 1, with only the most recent writer still sharing.
  ##
  ## Sequential, deliberately: this is not a race. One destination after another is enough,
  ## which is why the lock in the preceding test does not cover it.
  src <- checkPath(file.path(tmpdir, "src"), create = TRUE)
  inner <- checkPath(file.path(src, "keep", "keep"), create = TRUE)
  writeBin(as.raw(rep(7L, 2e5)), file.path(inner, "keep.bin"))
  owd <- setwd(file.path(src, "keep"))
  zipped <- utils::zip(file.path(src, "keep.zip"), "keep", flags = "-rq")
  setwd(owd)
  skip_if_not(identical(zipped, 0L), "no zip utility")

  shared <- checkPath(file.path(tmpdir, "shared"), create = TRUE)
  withr::local_options(reproducible.destinationPathShared = shared)

  inoOf <- function(p) as.character(fs::file_info(p)$inode)
  stashed <- NULL
  inodes <- character()
  for (i in 1:3) {
    dest <- checkPath(file.path(tmpdir, paste0("d", i)), create = TRUE)
    prepInputs(url = paste0("file://", file.path(src, "keep.zip")),
               destinationPath = dest, fun = NA, useCache = FALSE)
    if (is.null(stashed))
      stashed <- list.files(shared, pattern = "keep[.]bin$", recursive = TRUE,
                            full.names = TRUE)[1L]
    expect_true(file.exists(stashed))
    inodes <- c(inodes, inoOf(stashed))
  }

  ## The stash keeps ONE inode throughout -- it is never unlinked and rewritten...
  expect_length(unique(inodes), 1L)
  ## ...and nothing that linked to it earlier has been orphaned.
  expect_gte(as.integer(fs::file_info(stashed)$hard_links), 2L)
})

test_that("extracting into the shared stash keeps the copy other destinations are sharing", {
  skip_on_cran()
  testInit()

  ## extractFromArchive() unpacks to a temp dir and then moves the results to `exdir` with
  ## hardLinkOrCopy(). When the lookup has redirected destinationPath at the shared stash,
  ## `exdir` IS the stash -- and linkOrCopy() unlinks a target before linking to it. So the
  ## extraction replaced the stash file with a fresh inode holding the same bytes, and
  ## every destination hardlinked to the old inode was stranded as a private copy; the next
  ## extraction then stranded that one. On a 15-worker run CA_FAO_forest_2019.tif collected
  ## three inodes in twenty minutes, each at nlink = 1.
  ##
  ## Sequential, not concurrent: the lock added for the stash race does not cover this, and
  ## one destination after another is enough to show it.
  src <- checkPath(file.path(tmpdir, "src"), create = TRUE)
  inner <- checkPath(file.path(src, "arc", "arc"), create = TRUE)
  writeBin(as.raw(rep(3L, 2e5)), file.path(inner, "arc.bin"))
  owd <- setwd(file.path(src, "arc"))
  zipped <- utils::zip(file.path(src, "arc.zip"), "arc", flags = "-rq")
  setwd(owd)
  skip_if_not(identical(zipped, 0L), "no zip utility")

  shared <- checkPath(file.path(tmpdir, "shared"), create = TRUE)
  withr::local_options(reproducible.destinationPathShared = shared)
  stashed <- file.path(shared, "arc", "arc", "arc.bin")
  csf     <- file.path(shared, "arc", "CHECKSUMS.txt")

  inodes <- character()
  links  <- integer()
  for (i in 1:4) {
    dest <- checkPath(file.path(tmpdir, paste0("d", i)), create = TRUE)
    prepInputs(url = paste0("file://", file.path(src, "arc.zip")),
               destinationPath = dest, fun = NA, useCache = FALSE)
    ## every destination gets its file -- the guard must not skip the placement
    expect_true(file.exists(file.path(dest, "arc", "arc.bin")))
    expect_true(file.exists(stashed))
    inodes <- c(inodes, as.character(fs::file_info(stashed)$inode))
    links  <- c(links,  as.integer(fs::file_info(stashed)$hard_links))
    ## After the first pass, drop the row for the file while leaving the file itself: that
    ## is the state the affected stash was in -- populated but not findable -- and it is
    ## what sends later callers back through extraction and onto the stash path.
    if (i == 1L && file.exists(csf)) {
      x <- readLines(csf)
      writeLines(x[!grepl("arc\\.bin", x)], csf)
    }
  }

  ## One inode throughout: the stash file is never unlinked and rewritten...
  expect_length(unique(inodes), 1L)
  ## ...and each destination adds a link rather than orphaning the last.
  expect_equal(links, 2:5)
})

test_that("linkOrCopy handles a set with nothing to link", {
  skip_on_cran()
  testInit()

  ## file.link() errors with "no files to link from" on zero-length input rather than
  ## returning logical(0), so a set that is all directories reached it and threw. In
  ## extractFromArchive the throw was swallowed and retried until the extraction fallbacks
  ## ran out, surfacing as a misleading "Please install.packages('archive')".
  d <- checkPath(file.path(tmpdir, "onlyDirs"), create = TRUE)
  checkPath(file.path(d, "sub"), create = TRUE)
  to <- file.path(tmpdir, "dest", "sub")
  expect_error(linkOrCopy(file.path(d, "sub"), to, symlink = FALSE, verbose = -1), NA)
})
