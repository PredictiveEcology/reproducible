test_that("checkPath: normPath consistency", {
  cwd <- getwd()

  # don't use checkPath here because we are testing normPath!
  tmpdir <- tempdir2("test_normPath")
  tmpdir <- normalizePath(tmpdir, winslash = "/", mustWork = FALSE)

  setwd(tmpdir)

  on.exit({
    setwd(cwd)
    unlink(tmpdir, recursive = TRUE)
  }, add = TRUE) # nolint

  paths <- list("./aaa/zzz",
                "./aaa/zzz/",
                ".//aaa//zzz",
                ".//aaa//zzz/",
                ".\\aaa\\zzz",
                ".\\aaa\\zzz\\",
                paste0(tmpdir, "/aaa/zzz"), # nolint
                paste0(tmpdir, "/aaa/zzz/"), # nolint
                file.path(tmpdir, "aaa", "zzz"))

  checked <- normPath(paths)
  expect_equal(length(unique(checked)), 1)

  # extra checks for missing/NA/NULL
  expect_equal(normPath(), character())
  expect_true(all(is.na(normPath(list(NA, NA_character_)))))
  expect_equal(normPath(NULL), character())
})

test_that("checkPath: checkPath consistency", {
  currdir <- getwd()

  # don't use checkPath here because we are testing checkPath
  tmpdir <- tempdir2("test_checkPath")

  on.exit({
    setwd(currdir)
    unlink(tmpdir, recursive = TRUE)
  }, add = TRUE) # nolint
  setwd(tmpdir)

  dir.create("aaa/zzz", recursive = TRUE, showWarnings = FALSE)
  paths <- list("./aaa/zzz",
                "./aaa/zzz/",
                ".//aaa//zzz",
                ".//aaa//zzz/",
                ".\\aaa\\zzz",
                ".\\aaa\\zzz\\",
                paste0(tmpdir, "/aaa/zzz"), # nolint
                paste0(tmpdir, "/aaa/zzz/"), # nolint
                file.path(tmpdir, "aaa", "zzz"))

  checked <- lapply(paths, checkPath, create = FALSE)
  expect_equal(length(unique(checked)), 1)
  unlink(tmpdir, recursive = TRUE)

  # extra checks for missing/NA/NULL
  expect_error(checkPath(), "Invalid path: no path specified.")
  expect_error(checkPath(NULL), "Invalid path: cannot be NULL.")
  expect_error(checkPath(NA_character_), "Invalid path: cannot be NA.")

  # Case where it is an existing fle
  f1 <- tempfile()
  expect_true(file.create(f1)) ## TRUE
  expect_true(file.exists(f1)) ## TRUE

  expect_message(a <- checkPath(f1), "is an existing file")

})
