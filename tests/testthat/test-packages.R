test_that("package-related functions work", {

  skip_on_cran()

  testInitOut <- testInit()
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  packageDir <- tmpdir
  packageDir1 <- tmpCache
  defaultFilesInPackageDir <- dir(packageDir)
  defaultFilesInPackageDir1 <- dir(packageDir1)
  on.exit({
    unlink(packageDir, recursive = TRUE)
    unlink(packageDir1, recursive = TRUE)
  }, add = TRUE)
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
  if (!grepl(pattern = "3.5", x = R.Version()[["version.string"]])) {
    expect_true(iv[iv$Package == "latdiag", "Version"] == versionlatdiag)
  }
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
  expect_true(all(pkgDeps %in% installed$instPkgs))

  # Check that the snapshot works even if packages aren't in packageDir,
  # i.e., standAlone is FALSE, or there are base packages
  allInstalled <- c("Holidays", "achubaty/meow", "latdiag")
  allInstalledNames <- unlist(lapply(strsplit(allInstalled, "/"), function(x) x[length(x)]))
  Require(allInstalled, libPath = packageDir)
  packageVersionFile <- file.path(packageDir, ".packageVersion3.txt")
  pkgSnapshot(libPath = packageDir, packageVersionFile, standAlone = FALSE)
  installed <- unique(data.table::fread(packageVersionFile))

  pkgDeps <- sort(c(allInstalledNames, unique(unlist(pkgDep(libPath = packageDir,
                                                            allInstalledNames,
                                                            recursive = TRUE)))))

  expect_true(all(unique(pkgDeps) %in% unique(installed$instPkgs)))

  packageDirList <- dir(packageDir)
  packageDirList <- packageDirList[!packageDirList %in% defaultFilesInPackageDir]
  expect_true(all(packageDirList %in% installed$instPkgs))
  expect_false(all(installed$instPkgs %in% packageDirList))
  expect_true(any(installed$instPkgs %in% packageDirList))
  installedNotInLibPath <- installed[!(installed$instPkgs %in% packageDirList), ]
  inBase <- unlist(installedVersions(installedNotInLibPath$instPkgs,
                                     libPath = .libPaths()[length(.libPaths())]))
  inBaseDT <- na.omit(data.table::data.table(instPkgs = names(inBase), instVers = inBase))
  inBaseDT <- unique(inBaseDT)
  merged <- installed[inBaseDT, on = c("instPkgs", "instVers"), nomatch = 0]

  # This test that the installed versions in Base are the same as the ones that
  # are expected in packageVersionFile, which is the ones that were used when
  # looking for the dependencies of latdiag during Require call
  expect_true(identical(merged, unique(merged)))

  try(detach("package:meow", unload = TRUE))
  try(detach("package:Holidays", unload = TRUE))
  try(detach("package:TimeWarp", unload = TRUE))

  ## Test passing package as unquoted name
  expect_true(Require(TimeWarp, libPath = packageDir1, standAlone = TRUE))

  #unlink(packageDir, recursive = TRUE, force = TRUE)

  #}
})

test_that("package-related functions work", {
  skip_on_cran()
  skip_on_appveyor()

  testInitOut <- testInit(libraries = c("data.table", "versions"))
  on.exit({
    try(testOnExit(testInitOut))
  }, add = TRUE)

  unlink(dir(tmpdir, full.names = TRUE, all.files = TRUE), recursive = TRUE)
  # wrong packages arg
  expect_error(Require(1), "packages should be")

  # # Try to cause fail
  # warns <- capture_warnings(Require("testsdfsd"))
  # expect_true(any(grepl("there is no", warns)))
  #
  # packageVersionFile <- file.path(tmpdir, ".packageVersion.txt")
  #
  # Require("TimeWarp", libPath = tmpdir, standAlone = TRUE)
  # pkgVers <- as.data.table(pkgSnapshot(libPath=tmpdir, packageVersionFile, standAlone = TRUE))
  #
  # pkgVers <- pkgVers[instPkgs=="TimeWarp",]
  # pkgVers[, instVers:= "1.0-7"]
  #
  # fwrite(pkgVers, file = packageVersionFile)
  #
  # try(detach("package:TimeWarp", unload = TRUE))
  # Mess <- capture_messages(Require("TimeWarp", libPath = tmpdir,
  #                                  packageVersionFile = packageVersionFile, standAlone = TRUE))
  # expect_true(any(grepl("Already have", Mess)))
  # expect_true(any(grepl("Trying to install", Mess)))

})
