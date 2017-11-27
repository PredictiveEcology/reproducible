test_that("package-related functions work", {

  oldRepos <- getOption("repos")
  repos <- "https://cran.rstudio.com"
  options(repos = repos)

  packageDir <- normalizePath(file.path(tempdir(), "test5"), winslash = "/", mustWork = FALSE)
  suppressWarnings(Require("crayon", libPath = packageDir, standAlone = TRUE))
  a <- getOption("repos")
  saveRDS(a, file = paste0("c:/Eliot/",paste(sample(LETTERS,3),collapse=""),".rds"))
  expect_true(any(grepl(pattern = "package:crayon", search())))
  expect_true(require("crayon", lib.loc = packageDir))

  vers <- readLines(file.path(packageDir, "crayon", "DESCRIPTION"))
  vers <- vers[grepl(vers, pattern = "^Version: ")]
  version <- sub(vers, pattern = "^Version: ", replacement = "")

  packageVersionFile <- file.path(packageDir, ".packageVersion.txt")
  pkgSnapshot(libPath=packageDir, packageVersionFile)
  expect_true(file.exists(packageVersionFile))

  # keep wrong version that is already installed, and loaded
  aa <- data.frame(instPkgs="crayon", instVers = "1.3.2", stringsAsFactors = FALSE)
  write.table(file = packageVersionFile, aa, row.names = FALSE)

  suppressWarnings(Require("crayon", libPath = packageDir, packageVersionFile = packageVersionFile,
          standAlone = TRUE))
  iv <- data.frame(installed.packages(lib.loc = packageDir), stringsAsFactors = FALSE)
  #expect_true(iv[iv$Package=="crayon","Version"]=="1.3.4")
  expect_true(iv[iv$Package=="crayon","Version"]==version)


  versionCovr <- "3.0.0"
  aa <- data.frame(instPkgs="covr", instVers = versionCovr, stringsAsFactors = FALSE)
  write.table(file = packageVersionFile, aa, row.names = FALSE)

  Require("covr", libPath = packageDir, packageVersionFile = packageVersionFile,
          standAlone = FALSE)
  iv <- data.frame(installed.packages(lib.loc = packageDir), stringsAsFactors = FALSE)
  #expect_true(iv[iv$Package=="crayon","Version"]=="1.3.4")
  save(iv, versionCovr, packageDir, packageVersionFile, file = "c:/Eliot/Line41.rdata")
  expect_true(iv[iv$Package=="covr","Version"]==versionCovr)


  Require("achubaty/meow", libPath = packageDir,
          install_githubArgs = list(force = TRUE, dependencies = TRUE),
          standAlone = TRUE)
  expect_true(any(grepl(pattern = "package:meow", search())))


  try(detach("package:covr", unload = TRUE))
  try(detach("package:meow", unload = TRUE))

  # zTree is a random small package that has 1 dependency that is NOT in base -- plyr
  Require("plyr") # make sure it is installed in personal library -- not packageDir -- THis should install Rcpp too, if needed
  suppressWarnings(Require("zTree", libPath = packageDir, standAlone = FALSE))
  expect_true(is.na(installedVersions("Rcpp", packageDir))) # should

  srch <- search()
  save(srch, file = "c:/Eliot/SearchPath.rdata")
  Require("zTree", libPath = packageDir, standAlone = TRUE) # with standAlone TRUE, both plyr & Rcpp need to be installed in packageDir
  expect_true(!is.na(installedVersions("Rcpp", packageDir)))

  packageVersionFile <- file.path(packageDir, ".packageVersion2.txt")
  pkgSnapshot(libPath=packageDir, packageVersionFile, standAlone = FALSE)
  installed <- data.table::fread(packageVersionFile)
  pkgDeps <- tools::package_dependencies("zTree", recursive = TRUE)
  save(installed, versionCovr, packageDir, packageVersionFile, pkgDeps, file = "c:/Eliot/Line68.rdata")
  expect_true(NROW(installed)==(1 + length(unlist(tools::package_dependencies("zTree", recursive = TRUE)))))


  # Check that the snapshot works even if packages aren't in packageDir, i.e., standAlone is FALSE, or there are base packages
  allInstalled <- c("zTree", "achubaty/meow", "crayon", "covr")
  allInstalledNames <- unlist(lapply(strsplit(allInstalled, "/"), function(x) x[length(x)]))
  suppressWarnings(Require(allInstalled, libPath = packageDir))
  packageVersionFile <- file.path(packageDir, ".packageVersion3.txt")
  pkgSnapshot(libPath=packageDir, packageVersionFile, standAlone = FALSE)
  installed <- data.table::fread(packageVersionFile)
  expect_true(NROW(unique(installed, by="instPkgs")) ==
                   length(unique(c(allInstalledNames, unlist(tools::package_dependencies(allInstalledNames, recursive = TRUE))))))

  packageDirList <- dir(packageDir)
  expect_true(all(packageDirList %in% installed$instPkgs))
  expect_false(all(installed$instPkgs %in% packageDirList))
  expect_true(any(installed$instPkgs %in% packageDirList))
  installedNotInLibPath <- installed[!(installed$instPkgs %in% packageDirList),]
  inBase <- unlist(installedVersions(installedNotInLibPath$instPkgs, libPath = .libPaths()[length(.libPaths())]))
  inBaseDT <- na.omit(data.table::data.table(instPkgs=names(inBase), instVers=inBase))
  merged <- installed[inBaseDT, on = c("instPkgs", "instVers"), nomatch=0]

  # This test that the installed versions in Base are the same as the ones that are expected in packageVersionFile,
  #   which is the ones that were used when looking for the dependencies of covr during Require call
  expect_true(identical(merged, unique(merged)))

  try(detach("package:covr", unload = TRUE))
  try(detach("package:meow", unload = TRUE))
  try(detach("package:zTree", unload = TRUE))
  try(detach("package:plyr", unload = TRUE))
  suppressWarnings(try(detach("package:crayon", unload = TRUE)))

  unlink(packageDir, recursive = TRUE, force = TRUE)

})
