test_that("checkPath: normPath and normPathRel consistency", {
  # Use the following here instead of above because it fails on Mac without this.
  testInit()
  # tmpdir <- tempdir2("test_normPath")
  # tmpdir <- normalizePath(tmpdir, winslash = "/", mustWork = FALSE)

  withr::local_dir(tmpdir)
  # cwd <- getwd()
  # setwd(tmpdir)

  # don't use checkPath here because we are testing normPath!

  paths <- list(
    "./aaa/zzz",
    "./aaa/zzz/",
    ".//aaa//zzz",
    ".//aaa//zzz/",
    ".\\aaa\\zzz",
    ".\\aaa\\zzz\\",
    paste0(tmpdir, "/aaa/zzz"), # nolint
    paste0(tmpdir, "/aaa/zzz/"), # nolint
    file.path(tmpdir, "aaa", "zzz")
  )

  checked <- normPath(paths)
  expect_equal(length(unique(checked)), 1)

  checkedRel <- normPathRel(paths)
  if (identical(.Platform$OS.type, "windows")) {
    ## Windows create absolute paths
    expect_equal(length(unique(checkedRel)), 2)
  } else {
    ## non-existent paths kept relative on other platforms
    expect_equal(length(unique(checkedRel)), 2)
  }

  # These don't exist ... added May 10, 2023 after discovering that *nix-alikes don't make
  #   absolute paths with normalizePath when file or dir doesn't exist
  # Also empty path should return "", not be normalized
  pathsToCheck <- c("", "hi", "~/Hi", "/home/emcintir/testing", NA, "c:/Eliot")
  out <- normPath(pathsToCheck)
  expect_identical(sum(isAbsolutePath(out)), 4L)

  out <- isAbsolutePath(pathsToCheck)
  expect_identical(c(FALSE, FALSE, TRUE, TRUE, FALSE, TRUE), unname(out))

  outRel <- normPathRel(pathsToCheck)
  if (!identical(.Platform$OS.type, "windows")) {
    ## Windows create absolute paths;
    ## non-existent paths kept relative on other platforms
    expect_identical(isAbsolutePath(outRel), isAbsolutePath(pathsToCheck))
  }

  # extra checks for missing/NA/NULL
  expect_equal(normPath(), character(0))
  expect_equal(normPathRel(), character(0))

  expect_true(all(is.na(normPath(list(NA, NA_character_)))))
  expect_true(all(is.na(normPathRel(list(NA, NA_character_)))))

  expect_equal(normPath(NULL), character(0))
  expect_equal(normPathRel(NULL), character(0))
})

test_that("checkPath: checkPath consistency", {
  testInit()
  withr::local_dir(tmpdir)

  dir.create("aaa/zzz", recursive = TRUE, showWarnings = FALSE)
  paths <- list(
    "./aaa/zzz",
    "./aaa/zzz/",
    ".//aaa//zzz",
    ".//aaa//zzz/",
    ".\\aaa\\zzz",
    ".\\aaa\\zzz\\",
    paste0(tmpdir, "/aaa/zzz"), # nolint
    paste0(tmpdir, "/aaa/zzz/"), # nolint
    file.path(tmpdir, "aaa", "zzz")
  )

  checked <- lapply(paths, checkPath, create = FALSE)
  expect_equal(length(unique(checked)), 1)
  # unlink(tmpdir, recursive = TRUE)

  # extra checks for missing/NA/NULL
  expect_error(checkPath(), "Invalid path: no path specified.")
  expect_error(checkPath(NULL), "Invalid path: cannot be NULL.")
  expect_error(checkPath(NA_character_), "Invalid path: cannot be NA.")

  # Case where it is an existing file
  f1 <- tempfile()
  expect_true(file.create(f1)) ## TRUE
  expect_true(file.exists(f1)) ## TRUE

  withr::local_options("reproducible.verbose" = 1)
  expect_message(checkPath(f1), "is an existing file")
})
