test_that("package-related functions work", {
  repos <- getOption("repos", "https://cran.rstudio.com")
  if (is.null(repos) | any(repos == "" | "@CRAN@" %in% repos)) {
    repos <- "https://cran.rstudio.com"
  }
  if (length(repos) > 1) repos <- repos[(names(repos) %in% "CRAN")]

  packageDir <- normalizePath(file.path(tempdir(), "test5"), winslash = "/", mustWork = FALSE)
  packageDir1 <- normalizePath(file.path(tempdir(), "test6"), winslash = "/", mustWork = FALSE)
  suppressWarnings(Require("TimeWarp", libPath = packageDir1, standAlone = TRUE))
  expect_true(any(grepl(pattern = "package:TimeWarp", search())))
  expect_true(require("TimeWarp", lib.loc = packageDir1))

  vers <- readLines(file.path(packageDir1, "TimeWarp", "DESCRIPTION"))
  vers <- vers[grepl(vers, pattern = "^Version: ")]
  version <- sub(vers, pattern = "^Version: ", replacement = "")

  packageVersionFile <- file.path(packageDir1, ".packageVersion.txt")
  pkgSnapshot(libPath = packageDir1, packageVersionFile)
  expect_true(file.exists(packageVersionFile))

  # keep wrong version that is already installed, and loaded
  aa <- data.frame(instPkgs = "TimeWarp", instVers = "1.0.12", stringsAsFactors = FALSE)
  write.table(file = packageVersionFile, aa, row.names = FALSE)

  aa <- capture_warnings(Require("TimeWarp", libPath = packageDir1,
                                 packageVersionFile = packageVersionFile,
                                 standAlone = TRUE))
  iv <- data.frame(installed.packages(lib.loc = packageDir1), stringsAsFactors = FALSE)
  expect_true(iv[iv$Package == "TimeWarp", "Version"] == version)
  expect_true(startsWith(prefix = "Can't install", aa))

  # Load a different package
  versionlatdiag <- "0.2-2"
  aa <- data.frame(instPkgs = "latdiag", instVers = versionlatdiag, stringsAsFactors = FALSE)
  write.table(file = packageVersionFile, aa, row.names = FALSE)

  suppressWarnings(Require("latdiag", libPath = packageDir, packageVersionFile = packageVersionFile,
          standAlone = FALSE))
  iv <- data.frame(installed.packages(lib.loc = packageDir), stringsAsFactors = FALSE)
  expect_true(iv[iv$Package == "latdiag", "Version"] == versionlatdiag)

  Require("achubaty/meow", libPath = packageDir,
          install_githubArgs = list(force = TRUE, dependencies = c("Depends", "Imports")),
          standAlone = TRUE)
  expect_true(any(grepl(pattern = "package:meow", search())))

  # Holidays is a random small package that has 1 dependency that is NOT in base -- TimeWarp
  # make sure it is installed in personal library -- not packageDir;
  # this should install TimeWarp too, if needed
  Require("TimeWarp")
  suppressWarnings(Require("Holidays", libPath = packageDir, standAlone = FALSE))
  expect_true(is.na(installedVersions("TimeWarp", packageDir))) # should

  # with standAlone TRUE, both TimeWarp
  Require("Holidays", libPath = packageDir, standAlone = TRUE)
  expect_true(!is.na(installedVersions("TimeWarp", packageDir)))

  packageVersionFile <- file.path(packageDir, ".packageVersion2.txt")
  pkgSnapshot(libPath = packageDir, packageVersionFile, standAlone = FALSE)
  installed <- data.table::fread(packageVersionFile)
  pkgDeps <- sort(c("Holidays", unique(unlist(pkgDep("Holidays", recursive = TRUE,
                                                     libPath = packageDir)))))
  expect_true(all(sort(installed$instPkgs) == pkgDeps))

  # Check that the snapshot works even if packages aren't in packageDir,
  # i.e., standAlone is FALSE, or there are base packages
  allInstalled <- c("Holidays", "achubaty/meow", "latdiag")
  allInstalledNames <- unlist(lapply(strsplit(allInstalled, "/"), function(x) x[length(x)]))
  Require(allInstalled, libPath = packageDir)
  packageVersionFile <- file.path(packageDir, ".packageVersion3.txt")
  pkgSnapshot(libPath = packageDir, packageVersionFile, standAlone = FALSE)
  installed <- data.table::fread(packageVersionFile)

  pkgDeps <- sort(c(allInstalledNames, unique(unlist(pkgDep(libPath = packageDir,
                                                            allInstalledNames,
                                                            recursive = TRUE)))))

  expect_true(all(sort(unique(installed$instPkgs)) == sort(unique(pkgDeps))))

  packageDirList <- dir(packageDir)
  expect_true(all(packageDirList %in% installed$instPkgs))
  expect_false(all(installed$instPkgs %in% packageDirList))
  expect_true(any(installed$instPkgs %in% packageDirList))
  installedNotInLibPath <- installed[!(installed$instPkgs %in% packageDirList), ]
  inBase <- unlist(installedVersions(installedNotInLibPath$instPkgs,
                                     libPath = .libPaths()[length(.libPaths())]))
  inBaseDT <- na.omit(data.table::data.table(instPkgs = names(inBase), instVers = inBase))
  merged <- installed[inBaseDT, on = c("instPkgs", "instVers"), nomatch = 0]

  # This test that the installed versions in Base are the same as the ones that
  # are expected in packageVersionFile, which is the ones that were used when
  # looking for the dependencies of latdiag during Require call
  expect_true(identical(merged, unique(merged)))

  try(detach("package:meow", unload = TRUE))
  try(detach("package:Holidays", unload = TRUE))

  ## Test passing package as unquoted name
  expect_silent(Require(TimeWarp, libPath = packageDir1, standAlone = TRUE))

  unlink(packageDir, recursive = TRUE, force = TRUE)
})
