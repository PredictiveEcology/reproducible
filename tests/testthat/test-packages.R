test_that("package-related functions work", {

  repos <- getOption("repos")[1]
  if ( is.null(repos) | any(repos == "") ) {
    repos <- "https://cran.rstudio.com"
    options(repos = repos)
  }

  packageDir <- normalizePath(file.path(tempdir(), "test5"), winslash = "/", mustWork = FALSE)
  Require("crayon", libPath = packageDir)
  expect_true(any(grepl(pattern = "package:crayon", search())))
  expect_true(require("crayon", lib.loc = packageDir))

  vers <- readLines(file.path(packageDir, "crayon", "DESCRIPTION"))
  vers <- vers[grepl(vers, pattern = "^Version: ")]
  version <- sub(vers, pattern = "^Version: ", replacement = "")

  packageVersionFile <- file.path(packageDir, ".packageVersion.txt")
  pkgSnapshot(libPath=packageDir, packageVersionFile)
  expect_true(file.exists(packageVersionFile))

  # test wrong version
  aa <- data.frame(instPkgs="crayon", instVers = "1.3.2", stringsAsFactors = FALSE)
  write.table(file = packageVersionFile, aa, row.names = FALSE)

  Require("crayon", libPath = packageDir, packageVersionFile = packageVersionFile)
  iv <- data.frame(installed.packages(lib.loc = packageDir), stringsAsFactors = FALSE)
  expect_true(iv[iv$Package=="crayon","Version"]=="1.3.2")
  expect_false(iv[iv$Package=="crayon","Version"]==version)


  Require("achubaty/meow", libPath = packageDir, install_githubArgs = list(force = TRUE, dependencies = TRUE))
  expect_true(any(grepl(pattern = "package:meow", search())))


})
