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
