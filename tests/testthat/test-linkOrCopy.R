## Coverage for linkOrCopy() in R/preProcess.R.
##
## linkOrCopy() is how preProcess/prepInputs put a file where the caller asked
## for it without duplicating bytes when it can be avoided. It prefers a hard
## link and falls back to copying when it must -- across filesystems, or when
## linking is unsupported.
##
## Deliberately NOT asserted here: whether any given call links or copies. That
## depends on the filesystem, and both outcomes are correct. What a caller
## depends on is that the file arrives with the right content, which is what is
## pinned.
##
## No network, no Drive.

test_that("linkOrCopy places the file with its content", {
  testInit()

  src <- file.path(tmpdir, "src.txt")
  writeLines("payload", src)

  for (sym in c(TRUE, FALSE)) {
    to <- file.path(tmpdir, paste0("out-", sym, ".txt"))
    res <- suppressMessages(linkOrCopy(src, to, symlink = sym, verbose = 0))

    expect_true(isTRUE(all(res)))
    expect_true(file.exists(to))
    expect_identical(readLines(to, warn = FALSE), "payload")
  }
})

test_that("linkOrCopy creates missing destination directories", {
  testInit()

  src <- file.path(tmpdir, "src.txt")
  writeLines("payload", src)

  ## The destination directory does not exist yet; the caller should not have to
  ## pre-create it.
  to <- file.path(tmpdir, "newdir", "nested", "out.txt")
  expect_false(dir.exists(dirname(to)))

  suppressMessages(linkOrCopy(src, to, symlink = FALSE, verbose = 0))

  expect_true(file.exists(to))
  expect_identical(readLines(to, warn = FALSE), "payload")
})

test_that("linkOrCopy is a no-op from a file to itself", {
  testInit()

  src <- file.path(tmpdir, "self.txt")
  writeLines("payload", src)

  ## Copying a file onto itself would truncate it on some paths, so this case is
  ## short-circuited. The file must survive intact -- this is the assertion that
  ## matters, not the return value.
  suppressMessages(linkOrCopy(src, src, verbose = 0))

  expect_true(file.exists(src))
  expect_identical(readLines(src, warn = FALSE), "payload")
})

test_that("linkOrCopy reports failure for a missing source without creating anything", {
  testInit()

  missing <- file.path(tmpdir, "does-not-exist.txt")
  to <- file.path(tmpdir, "out.txt")

  res <- suppressMessages(linkOrCopy(missing, to, verbose = 0))

  ## FALSE rather than an error: callers treat this as "not available here" and
  ## fall through to the next source (download, other inputPaths).
  expect_false(isTRUE(all(res)))
  ## And crucially it must not leave an empty file behind, which would later
  ## look like a successful download.
  expect_false(file.exists(to))
})

test_that("linkOrCopy handles several files at once", {
  testInit()

  srcs <- file.path(tmpdir, c("m1.txt", "m2.txt"))
  for (i in seq_along(srcs)) writeLines(paste0("payload", i), srcs[i])
  tos <- file.path(tmpdir, "multi", c("m1.txt", "m2.txt"))

  suppressMessages(linkOrCopy(srcs, tos, symlink = FALSE, verbose = 0))

  expect_true(all(file.exists(tos)))
  expect_identical(readLines(tos[1], warn = FALSE), "payload1")
  expect_identical(readLines(tos[2], warn = FALSE), "payload2")
})

test_that("linkOrCopy falls back to copying when hard-linking fails", {
  testInit()

  ## The fallback normally only fires across filesystems, which a test cannot
  ## arrange portably. Mocking file.link to fail reaches it deterministically --
  ## and that IS the real-world case: a destination on another mount.
  src <- file.path(tmpdir, "src.txt")
  writeLines("payload", src)
  to <- file.path(tmpdir, "copied.txt")

  suppressMessages(
    testthat::with_mocked_bindings(
      linkOrCopy(src, to, symlink = FALSE, verbose = 0),
      file.link = function(...) FALSE, .package = "base")
  )

  ## What a caller depends on: the bytes arrive regardless of the mechanism.
  expect_true(file.exists(to))
  expect_identical(readLines(to, warn = FALSE), "payload")
})

test_that("linkOrCopy copies every file when hard-linking fails for a vector", {
  testInit()

  ## The fallback path indexes with `from[!result]`, so a partial/whole vector
  ## failure must still deliver all of them -- an off-by-one here would silently
  ## drop inputs.
  srcs <- file.path(tmpdir, c("m1.txt", "m2.txt", "m3.txt"))
  for (i in seq_along(srcs)) writeLines(paste0("payload", i), srcs[i])
  tos <- file.path(tmpdir, "multi", c("m1.txt", "m2.txt", "m3.txt"))

  suppressMessages(
    testthat::with_mocked_bindings(
      linkOrCopy(srcs, tos, symlink = FALSE, verbose = 0),
      file.link = function(...) FALSE, .package = "base")
  )

  expect_true(all(file.exists(tos)))
  expect_identical(readLines(tos[3], warn = FALSE), "payload3")
})

test_that("linkOrCopy tries a symlink before copying on unix", {
  skip_on_os("windows")
  testInit()

  ## With symlink = TRUE and hard-linking unavailable, the symlink branch runs
  ## before the copy fallback. Either outcome delivers the content; that is what
  ## is asserted, not which mechanism won.
  src <- file.path(tmpdir, "src.txt")
  writeLines("payload", src)
  to <- file.path(tmpdir, "linked.txt")

  suppressMessages(
    testthat::with_mocked_bindings(
      linkOrCopy(src, to, symlink = TRUE, verbose = 0),
      file.link = function(...) FALSE, .package = "base")
  )

  expect_true(file.exists(to))
  expect_identical(readLines(to, warn = FALSE), "payload")
})
