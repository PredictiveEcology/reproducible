## End-to-end coverage of prepInputs()' archive path, across every archive
## format base R can build.
##
## Driving prepInputs() rather than .callArchiveExtractFn() directly is
## deliberate: the internal takes an `args` list assembled several layers up, so
## calling it directly would mean reproducing prepInputs internals in the test.
## Going through the public entry point exercises the whole chain --
## .whichExtractFn -> .listFilesInArchive -> .callArchiveExtractFn -> extraction
## -> checksums -- and asserts what a caller actually gets.
##
## Archives are built on the fly rather than committed as fixtures: no binaries
## in git, nothing counting against package size, and the inputs are visible in
## the test. zip/tar/tar.gz/gz are exactly knownInternalArchiveExtensions, all
## creatable with base R. rar/7z/cab need external binaries that CI may lack, so
## the 7z case is guarded and rar/cab are left alone (rar cannot be *created*
## by any commonly available tool anyway -- unrar only extracts).
##
## No network, no Drive.

## Build a two-file archive of the given kind; returns its absolute path.
mkArchive <- function(root, kind) {
  d <- file.path(root, paste0("src-", kind))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  owd <- setwd(d)
  on.exit(setwd(owd), add = TRUE)

  writeLines("hello", "a.txt")
  writeLines("world", "b.txt")

  f <- switch(kind,
    zip   = { suppressWarnings(utils::zip("t.zip", c("a.txt", "b.txt"), flags = "-q")); "t.zip" },
    tar   = { utils::tar("t.tar", c("a.txt", "b.txt")); "t.tar" },
    targz = { utils::tar("t.tar.gz", c("a.txt", "b.txt"), compression = "gzip"); "t.tar.gz" },
    `7z`  = { system2("7z", c("a", "-bso0", "-bsp0", "t.7z", "a.txt", "b.txt")); "t.7z" }
  )
  normalizePath(file.path(d, f))
}

test_that("prepInputs extracts from every archive format base R can build", {
  testInit()

  ## fun = NA means "extract and return the path, do not load" -- the archive
  ## machinery is what is under test here, not the readers.
  for (kind in c("zip", "tar", "targz")) {
    root <- checkPath(file.path(tmpdir, kind), create = TRUE)
    arch <- mkArchive(root, kind)
    dest <- checkPath(file.path(root, "dest"), create = TRUE)

    out <- prepInputs(archive = arch, targetFile = "a.txt",
                      destinationPath = dest, fun = NA, verbose = -1)

    ## Returns the path to the requested file inside destinationPath.
    expect_identical(basename(as.character(out)), "a.txt")
    expect_true(file.exists(as.character(out)))
    expect_identical(readLines(as.character(out), warn = FALSE), "hello")

    ## The whole archive is extracted, not only the target.
    expect_true(all(c("a.txt", "b.txt") %in% dir(dest)))

    ## A CHECKSUMS.txt is written, which is what makes a second call cheap.
    expect_true("CHECKSUMS.txt" %in% dir(dest))
  }
})

test_that("prepInputs extracts a 7z archive", {
  skip_if_not(nzchar(Sys.which("7z")), "7z binary not available")
  testInit()

  root <- checkPath(file.path(tmpdir, "sevenz"), create = TRUE)
  arch <- mkArchive(root, "7z")
  skip_if_not(file.exists(arch), "7z archive could not be created")
  dest <- checkPath(file.path(root, "dest"), create = TRUE)

  ## 7z is in knownSystemArchiveExtensions: handled by a system call or the
  ## archive package rather than base R, so it takes a different branch.
  out <- prepInputs(archive = arch, targetFile = "a.txt",
                    destinationPath = dest, fun = NA, verbose = -1)

  expect_identical(basename(as.character(out)), "a.txt")
  expect_identical(readLines(as.character(out), warn = FALSE), "hello")
})

test_that("prepInputs re-run on the same archive is idempotent", {
  testInit()

  root <- checkPath(file.path(tmpdir, "again"), create = TRUE)
  arch <- mkArchive(root, "zip")
  dest <- checkPath(file.path(root, "dest"), create = TRUE)

  first <- prepInputs(archive = arch, targetFile = "a.txt",
                      destinationPath = dest, fun = NA, verbose = -1)
  ## The second call sees the checksums and the already-extracted files; it must
  ## return the same path and leave the content untouched.
  second <- prepInputs(archive = arch, targetFile = "a.txt",
                       destinationPath = dest, fun = NA, verbose = -1)

  expect_identical(as.character(second), as.character(first))
  expect_identical(readLines(as.character(second), warn = FALSE), "hello")
})

test_that("prepInputs loads through fun when the loader is given", {
  testInit()

  root <- checkPath(file.path(tmpdir, "load"), create = TRUE)
  d <- checkPath(file.path(root, "src"), create = TRUE)
  owd <- setwd(d)
  saveRDS(list(v = 1:3), "obj.rds")
  suppressWarnings(utils::zip("t.zip", "obj.rds", flags = "-q"))
  setwd(owd)
  arch <- normalizePath(file.path(d, "t.zip"))
  dest <- checkPath(file.path(root, "dest"), create = TRUE)

  out <- prepInputs(archive = arch, targetFile = "obj.rds",
                    destinationPath = dest, fun = "readRDS", verbose = -1)

  ## fun is applied, so the object comes back rather than a path.
  expect_type(out, "list")
  expect_identical(out$v, 1:3)
})

test_that("prepInputs errors when the targetFile is not in the archive", {
  testInit()

  root <- checkPath(file.path(tmpdir, "missing"), create = TRUE)
  arch <- mkArchive(root, "zip")
  dest <- checkPath(file.path(root, "dest"), create = TRUE)

  ## Asking for something the archive does not contain must fail rather than
  ## quietly returning one of the files it does contain.
  expect_error(
    suppressWarnings(prepInputs(archive = arch, targetFile = "not-there.txt",
                                destinationPath = dest, fun = NA, verbose = -1))
  )
})
