test_that("filesUsedTrace parses a trace file: successful opens under folders only", {
  skip_on_os("windows") # a strace trace holds Linux paths ("/..."); a Windows temp dir is "C:/..."
  root <- withr::local_tempdir()
  inputs <- file.path(root, "inputs")
  cache <- file.path(root, "cache")
  other <- file.path(root, "other")
  dir.create(inputs); dir.create(cache); dir.create(other)
  writeLines("a", file.path(inputs, "a.txt"))
  writeLines("bb", file.path(inputs, "b.txt"))
  writeLines("ccc", file.path(cache, "c.rds"))
  writeLines("dddd", file.path(other, "d.txt"))
  writeLines("e", file.path(inputs, "unused.txt"))
  dir.create(file.path(inputs, "sub"))
  link <- file.path(root, "link")
  skip_if_not(file.symlink(inputs, link), "symlinks not supported")

  traceFile <- file.path(root, "trace.txt")
  writeLines(c(
    sprintf('1 openat(AT_FDCWD, "%s", O_RDONLY) = 3', file.path(inputs, "a.txt")),
    sprintf('1 newfstatat(AT_FDCWD, "%s", {st_mode=S_IFREG|0644, ...}, 0) = 0', file.path(link, "b.txt")),
    sprintf('2 openat(AT_FDCWD, "%s", O_RDONLY) = 4', file.path(cache, "c.rds")),
    sprintf('2 openat(AT_FDCWD, "%s", O_RDONLY) = 4', file.path(inputs, "a.txt")),
    sprintf('1 openat(AT_FDCWD, "%s", O_RDONLY) = 5', file.path(other, "d.txt")),
    sprintf('1 openat(AT_FDCWD, "%s", O_RDONLY) = -1 ENOENT (No such file or directory)', file.path(inputs, "unused.txt")),
    sprintf('1 newfstatat(AT_FDCWD, "%s", {st_mode=S_IFDIR|0755, ...}, 0) = 0', file.path(inputs, "sub")),
    sprintf('1 openat(AT_FDCWD, "%s", O_RDONLY) = 6', file.path(inputs, "missing.txt")),
    '1 openat(AT_FDCWD, "relative.txt", O_RDONLY) = 7',
    "1 +++ exited with 0 +++"
  ), traceFile)

  out <- filesUsedTrace(traceFile = traceFile, folders = c(link, cache))
  expect_s3_class(out, "data.frame")
  expect_identical(names(out), c("path", "folder", "size"))
  inputsN <- normalizePath(inputs)
  cacheN <- normalizePath(cache)
  expect_identical(out$path, sort(c(file.path(inputsN, "a.txt"), file.path(inputsN, "b.txt"),
                                    file.path(cacheN, "c.rds"))))
  expect_identical(out$folder, c(cacheN, inputsN, inputsN))
  expect_identical(out$size, file.size(out$path))

  none <- filesUsedTrace(traceFile = traceFile, folders = other)
  expect_identical(none$path, normalizePath(file.path(other, "d.txt")))
  empty <- filesUsedTrace(traceFile = traceFile, folders = file.path(inputs, "sub"))
  expect_identical(nrow(empty), 0L)
  expect_identical(names(empty), c("path", "folder", "size"))
})

test_that("filesUsedTrace traces a real Rscript run", {
  skip_on_cran()
  skip_if(!identical(Sys.info()[["sysname"]], "Linux") || !nzchar(Sys.which("strace")), "needs Linux and strace")
  root <- withr::local_tempdir()
  inputs <- file.path(root, "inputs")
  dir.create(inputs)
  writeLines("a", file.path(inputs, "used.txt"))
  writeLines("b", file.path(inputs, "unused.txt"))
  script <- file.path(root, "s.R")
  writeLines(c("args <- commandArgs(trailingOnly = TRUE)",
               "readLines(file.path(args[1], 'used.txt'))"), script)
  out <- filesUsedTrace(script, args = inputs, folders = inputs)
  expect_identical(out$path, normalizePath(file.path(inputs, "used.txt")))
  expect_identical(out$folder, normalizePath(inputs))
})

test_that("filesUsedTrace stops clearly without strace", {
  withr::local_envvar(PATH = withr::local_tempdir())
  expect_error(filesUsedTrace("x.R", folders = tempdir()), "strace")
})
