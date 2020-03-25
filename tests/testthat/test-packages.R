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
  # ._installPackages_1 <<- ._installPackages_2 <<- ._installPackages_3 <<- ._Require_1 <<- 1
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
  versionlatdiag <- "0.2-4"
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
  skip_on_os("mac")
  # Alex debug on mac
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

  # Alex debug on mac
  expect_true(all(unique(pkgDeps) %in% unique(installed$instPkgs)))

  packageDirList <- dir(packageDir)
  packageDirList <- packageDirList[!packageDirList %in% defaultFilesInPackageDir]
  # Alex debug on mac
  expect_true(all(packageDirList %in% installed$instPkgs))
  expect_false(all(installed$instPkgs %in% packageDirList))
  # Alex debug on mac
  expect_true(any(installed$instPkgs %in% packageDirList))
  installedNotInLibPath <- installed[!(installed$instPkgs %in% packageDirList), ]
  inBase <- unlist(installedVersions(installedNotInLibPath$instPkgs,
                                     libPath = .libPaths()[length(.libPaths())]))
  inBaseDT <- na.omit(data.table::data.table(instPkgs = names(inBase), instVers = inBase))
  inBaseDT <- unique(inBaseDT)
  # Alex debug on mac
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

test_that("test pkgDep", {
    testInitOut <- testInit()
    on.exit({
      testOnExit(testInitOut)
    }, add = TRUE)

    N <- 4
    aTime <- system.time(for (i in 1:N) aStart <- pkgDep("reproducible", refresh = TRUE))
    bTime <- system.time(for (i in 1:N) bStart <- pkgDep("reproducible", refresh = FALSE))
    expect_identical(aStart,bStart)
    expect_true(aTime[3] > bTime[3])

    df <- expand.grid(suggests = c(TRUE, FALSE),
                      #depends = c(TRUE, FALSE),
                      imports = c(TRUE, FALSE),
                      linkingTo = c(TRUE, FALSE)
                      )
    #df <- data.frame(df, count = apply(df, 1, sum) - df$depends)

    # Test "refresh"
    a <- list()
    b <- list()
    pkg <- "reproducible"
    out <- lapply(seq(NROW(df)), function(n) {
      f <- df[n,]
      n <- paste(as.logical(f), collapse = "_")
      a[[n]] <<- pkgDep(pkg, refresh = TRUE, suggests = f$suggests, imports = f$imports, #depends = f$depends,
                        linkingTo = f$linkingTo,
                        recursive = FALSE)
      b[[n]] <<- pkgDep(pkg, refresh = FALSE, suggests = f$suggests, imports = f$imports, #depends = f$depends,
                        linkingTo = f$linkingTo,
                        recursive = FALSE)
      expect_identical(a[[n]],b[[n]])
    })

    # out <- lapply(seq(NROW(df)), function(n) {
    #   f1 <- df[n,]
    #   names(f1) <- colnames(df)
    #
    #   subDF <- expand.grid(lapply(f1, function(x) unique(c(FALSE, x))))
    #   dfCompare <- subDF[sapply(seq_len(NROW(subDF)), function(x) !identical(as.logical(subDF[x,]), as.logical(f1))),]
    #
    #   lapply(as.numeric(rownames(dfCompare)), function(x) {
    #     x <- paste(as.logical(dfCompare[x,]), collapse = "_")
    #     expect_true(length(a[[n]][[1]]) > length(a[[x]][[1]]))
    #   })
    # })

    a2 <- pkgDep("reproducible", refresh = TRUE, suggests = TRUE, imports = FALSE, linkingTo = FALSE)
    b2 <- pkgDep("reproducible", refresh = FALSE, suggests = TRUE, imports = FALSE, linkingTo = FALSE)
    expect_identical(a2,b2)

    a3 <- pkgDep("reproducible", refresh = TRUE, suggests = TRUE, imports = FALSE, linkingTo = FALSE, depends = FALSE)
    b3 <- pkgDep("reproducible", refresh = FALSE, suggests = TRUE, imports = FALSE, linkingTo = FALSE, depends = FALSE)
    expect_identical(a3,b3)

    a4 <- pkgDep("reproducible", refresh = TRUE, suggests = FALSE, imports = FALSE, linkingTo = FALSE, depends = FALSE)
    b4 <- pkgDep("reproducible", refresh = FALSE, suggests = FALSE, imports = FALSE, linkingTo = FALSE, depends = FALSE)
    expect_identical(a4,b4)

    # rebuild recursive manually
    d <- list()
    d[[1]] <- pkgDep("reproducible", refresh = TRUE, recursive = FALSE)
    b2 <- pkgDep("reproducible", refresh = FALSE, recursive = FALSE)
    expect_identical(d[[1]],b2)

    i <- 1
    while (length(d[[i]]) > 0) {
      d[[i+1]] <- unique(unname(unlist(lapply(d[[i]], pkgDep, recursive = FALSE))))
      i <- i + 1
    }

    e2 <- sort(unique(unlist(c(b2,d))))
    expect_identical(e2, sort(aStart$reproducible))

  })

test_that("test pkgDep2", {
  testInitOut <- testInit()
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  a <- pkgDep2("reproducible", recursive = TRUE, suggests = FALSE, depends = TRUE,
               imports = TRUE, sort = FALSE)
  b <- pkgDep("reproducible", recursive = FALSE, suggests = FALSE, depends = TRUE,
              imports = TRUE)
  expect_identical(names(a), b$reproducible)

  a <- pkgDep2("reproducible", recursive = TRUE, suggests = FALSE, depends = TRUE,
               imports = TRUE, sort = FALSE)
  b <- pkgDep("reproducible", recursive = TRUE, suggests = FALSE, depends = TRUE,
              imports = TRUE)
  expect_identical(sort(unique(c(names(a), unique(unlist(a))))), sort(b$reproducible))


})

test_that("package-related functions work", {
  skip_on_cran()
  skip_on_appveyor()
  skip_on_travis()

  testInitOut <- testInit(libraries = c("data.table", "versions"))
  on.exit({
    try(testOnExit(testInitOut))
  }, add = TRUE)

  testthat::with_mock(
    "isInteractive" = function() {
      FALSE
    },
    {
      a <- Require(paste0("glue (>=", packageVersion("glue"), ")"),
                   #libPath = tmpCache,
                   standAlone = FALSE)
    })
  testthat::with_mock(
    "isInteractive" = function() {
      FALSE
    },
    {
      expect_true(a)
    })

  mess <- capture_messages(err <- capture_error(b <- Require(c("SpaDES.core (>=0.9)",
            "PredictiveEcology/reproducible@messagingOverhaul (>= 4.1.1)",
            "achubaty/amc@development (>=0.1.5)",
            "data.table (>=100.0)",
            paste0("digest (>=", packageVersion("digest"),")"),
            "PredictiveEcology/LandR (>= 0.0.2)"))))
  expect_true(sum(grepl("following packages", mess)) == 1)
  expect_true(sum(grepl("Please manually", err)) == 1)

  mess <- capture_messages(
    err <- capture_error(
      b <- Require(c("fastdigest (>=0.0.0.9)",
                     "PredictiveEcology/reproducible@messagingOverhaul (>= 0.0.0.9)",
                     "achubaty/amc@development (>=0.0.0.9)",
                     "data.table (>=0.0.0.9)",
                     paste0("digest (>=", packageVersion("digest"),")"),
                     "PredictiveEcology/LandR (>= 0.0.0.9)"))))
  expect_true(sum(grepl("following packages", mess)) == 0)
  expect_true(sum(grepl("Please manually", err)) == 0)
  expect_true(all(b))

  testthat::with_mock(
    "isInteractive" = function() {
      FALSE
    },
    {
      expect_error(a <- Require("glue (>=1000.3.1)", libPath = tmpCache, standAlone = TRUE))
    })
  testthat::with_mock(
    "isInteractive" = function() {
      FALSE
    },
    {
      a <- Require(c("glue (>=0.3.1)", "fpCompare"), libPath = tmpCache, standAlone = TRUE)
    })
  testthat::with_mock(
    "isInteractive" = function() {
      FALSE
    },
    {
      expect_true(length(a) == 2)
    })
  testthat::with_mock(
    "isInteractive" = function() {
      FALSE
    },
    {
      expect_true(all(a))
    }
  )
  testthat::with_mock(
    ".readline" = function(prompt) {
      3
    },
    {
      warn <- capture_warnings(
        mess <- capture_messages(a <- Require("glue (>=1.3.1)",
                                              libPath = tmpdir, standAlone = TRUE))
      )
      expect_true(sum(grepl("Not installing", mess))==1)
    }
  )

})
