## Coverage for .guessFileExtension() (R/preProcess.R) and lockFile() (R/GPT2.R).
##
## .guessFileExtension identifies a downloaded file by magic number when the URL
## gave no usable extension -- that is how prepInputs decides whether something
## is an archive at all. lockFile serialises concurrent writers of the same
## cacheId on the file-backed backend.
##
## No network, no Drive.

test_that(".guessFileExtension identifies archives by magic number", {
  skip_on_os("windows")
  skip_if_not(nzchar(Sys.which("file")), "`file` binary not available")
  testInit()

  ## Built with absolute paths -- no setwd(), whose restore races testInit()'s
  ## tempdir cleanup. Only the magic number matters here, not the archive's
  ## internal paths.
  plain <- file.path(tmpdir, "plain.txt")
  writeLines("hello", plain)
  suppressWarnings(utils::zip(file.path(tmpdir, "z.zip"), plain, flags = "-q"))
  utils::tar(file.path(tmpdir, "t.tar"), plain)

  ## Recognised container formats come back with a leading dot, ready to paste
  ## onto a filename.
  expect_identical(.guessFileExtension(file.path(tmpdir, "z.zip")), ".zip")
  expect_identical(.guessFileExtension(file.path(tmpdir, "t.tar")), ".tar")

  ## Anything it cannot identify as an archive is NULL, not a guess -- prepInputs
  ## treats NULL as "not an archive" and uses the file as-is.
  expect_null(.guessFileExtension(file.path(tmpdir, "plain.txt")))
})

test_that(".guessFileExtension returns NULL rather than erroring on the Windows path", {
  testInit()

  writeLines("hello", file.path(tmpdir, "plain.txt"))

  ## The Windows branch shells out to a cygwin `file.exe` that will not exist
  ## here. It is wrapped in tryCatch precisely so a missing tool yields NULL
  ## instead of failing the download. Reachable only because the branch tests
  ## isWindows() rather than .Platform directly.
  local_mocked_bindings(isWindows = function() TRUE)
  expect_null(.guessFileExtension(file.path(tmpdir, "plain.txt")))
})

test_that("lockFile takes a lock on the file-backed backend", {
  skip_if_not_installed("filelock")
  testInit()

  withr::local_options(reproducible.useDBI = FALSE)
  cp <- checkPath(file.path(tmpdir, "cache"), create = TRUE)

  lock <- lockFile(cp, "abc123")

  ## A real lock object comes back, and the lock file is named for the cacheId,
  ## so two processes computing DIFFERENT cacheIds never block each other.
  expect_s3_class(lock, "filelock_lock")
  expect_true(any(grepl("abc123", dir(CacheStorageDir(cp)))))

  ## The storage dir is created if absent -- lockFile is called before anything
  ## else has necessarily written there.
  expect_true(dir.exists(CacheStorageDir(cp)))

  filelock::unlock(lock)
})

test_that("lockFile is a no-op under the DBI backend", {
  skip_if_not_installed("RSQLite")
  testInit()

  withr::local_options(reproducible.useDBI = TRUE)
  skip_if_not(useDBI())
  cp <- checkPath(file.path(tmpdir, "cacheDBI"), create = TRUE)

  ## SQLite does its own locking, so no file lock is taken and no lock file is
  ## left behind for a later run to trip over.
  expect_null(lockFile(cp, "abc123"))
})
