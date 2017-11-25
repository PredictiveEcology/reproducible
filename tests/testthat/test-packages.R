test_that("package-related functions work", {

  #repos <- getOption("repos")[1]
  #if ( is.null(repos) | any(repos == "") ) {
    repos <- "https://cran.rstudio.com"
    options(repos = repos)
  #}

  packageDir <- normalizePath(file.path(tempdir(), "test5"), winslash = "/", mustWork = FALSE)
  Require("crayon", libPath = packageDir, standAlone = TRUE)
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

  Require("crayon", libPath = packageDir, packageVersionFile = packageVersionFile,
          standAlone = TRUE)
  iv <- data.frame(installed.packages(lib.loc = packageDir), stringsAsFactors = FALSE)
  #expect_true(iv[iv$Package=="crayon","Version"]=="1.3.4")
  expect_true(iv[iv$Package=="crayon","Version"]==version)


  Require("achubaty/meow", libPath = packageDir,
          install_githubArgs = list(force = TRUE, dependencies = TRUE),
          standAlone = TRUE)
  expect_true(any(grepl(pattern = "package:meow", search())))


  unlink(packageDir, recursive = TRUE, force = TRUE)

  # Use standAlone = FALSE
  Require("covr", libPath = packageDir, standAlone = FALSE)
  packageVersionFile <- file.path(packageDir, ".packageVersion2.txt")
  pkgSnapshot(libPath=packageDir, packageVersionFile, standAlone = FALSE)
  installed <- data.table::fread(packageVersionFile)
  expect_true(NROW(installed)==20)
  packageDirList <- dir(packageDir)
  expect_true(packageDirList %in% installed$instPkgs)
  expect_false(all(installed$instPkgs %in% packageDirList))
  expect_true(any(installed$instPkgs %in% packageDirList))
  installedNotInLibPath <- installed[!(installed$instPkgs %in% packageDirList),]
  inBase <- unlist(installedVersions(installedNotInLibPath$instPkgs, libPath = .libPaths()[length(.libPaths())]))
  inBaseDT <- na.omit(data.table::data.table(instPkgs=names(inBase), instVers=inBase))
  merged <- installed[inBaseDT, on = c("instPkgs", "instVers"), nomatch=0]

  # This test that the installed versions in Base are the same as the ones that are expected in packageVersionFile,
  #   which is the ones that were used when looking for the dependencies of covr during Require call
  expect_that(identical(merged, unique(merged)))

})
