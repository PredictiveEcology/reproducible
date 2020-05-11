test_that("package-related functions work (part 1)", {
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

test_that("package-related functions work (part 2)", {
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

test_that("package-related functions work (part 3)", {
  skip("Require() needs reworking") ## TODO: temporarily skip these broken tests
  skip_on_cran()
  skip_on_appveyor()
  skip_on_travis()
  testInitOut <- testInit(libraries = c("data.table"))#,
                          #opts = list("reproducible.Require.install" = FALSE))
  tmpdir <- checkPath(file.path("~", "TempLib3"), create = TRUE) # need a persistent folder ## TODO: use tempdir
  on.exit({
    try(testOnExit(testInitOut))
  }, add = TRUE)

  repo <- getCRANrepos()
  opts <- options(repos = repo)
  on.exit(opts, add = TRUE)

  origLibPaths <- .libPaths()
  on.exit(
    .libPaths(origLibPaths)
  )
  .libPaths(tmpdir)
  # ._test111 <<- 1
  # pedev::rmDotUnderline()
  ######## NEW FULL
  pkgs <- list(
    c("SpaDES.core (>=0.9)", "PredictiveEcology/reproducible@messagingOverhaul (>= 4.1.1)",
      "achubaty/amc@development (>=0.1.5)", "data.table (>=100.0)",
      "digest (>=0.6.23)", "PredictiveEcology/LandR@development (>= 1.0.2)", "versions (>=0.3)",
      "fastdigest (>=0.0.0.9)", "PredictiveEcology/reproducible@development (>= 0.0.0.9)",
      "achubaty/amc@development (>=0.0.0.9)", "data.table (>=0.0.0.9)",
      "PredictiveEcology/LandR@development(>= 0.0.0.9)", "fastdigest (>=1000.0.0.8)",
      "fastdigest", "quickPlot", "testthat",
      "PredictiveEcology/reproducible@messagingOverhaul (>= 0.0.0.9)",
      "PredictiveEcology/reproducible@messagingOverhaul (>= 0.0.0.10)",
      "PredictiveEcology/reproducible@development (>= 1110.0.0.9)",
      "PredictiveEcology/reproducible@development (>= 0.0.0.10)",
      "PredictiveEcology/reproducible@development (>= 0.0.0.11)"
    ),
    c("SpaDES.core (>=0.9)",
      "PredictiveEcology/reproducible@messagingOverhaul (>= 4.1.1)",
      "achubaty/amc@development (>=0.1.5)", "data.table (>=100.0)",
      paste0("digest (>=", packageVersion("digest"), ")"),
      "PredictiveEcology/LandR@development (>= 1.0.2)"),
    c("fastdigest (>=0.0.0.9)",
      "PredictiveEcology/reproducible@development (>= 0.0.0.9)",
      "achubaty/amc@development (>=0.0.0.9)", "data.table (>=0.0.0.9)",
      paste0("digest (>=", packageVersion("digest"), ")"),
      "PredictiveEcology/LandR@development(>= 0.0.0.9)"),
    # Multiple conflicting version numbers, and with NO version number
    c("fastdigest (>=0.0.0.8)", "fastdigest (>=0.0.0.9)", "fastdigest", "quickPlot", "testthat"),
    c("fastdigest (>=1000.0.0.8)", "fastdigest (>=0.0.0.9)", "fastdigest", "quickPlot", "testthat"),
    c("fastdigest (>=0.0.0.9)",
      "PredictiveEcology/reproducible@messagingOverhaul (>= 0.0.0.9)",
      "PredictiveEcology/reproducible@messagingOverhaul (>= 0.0.0.10)",
      "PredictiveEcology/reproducible@development (>= 1110.0.0.9)",
      "achubaty/amc@development (>=0.0.0.9)",
      "data.table (>=0.0.0.9)",
      paste0("digest (>=", packageVersion("digest"),")"),
      "PredictiveEcology/LandR@development(>= 0.0.0.9)"),
    "glue (>=1000.3.1)",
    c("glue (>=0.3.1)", "fpCompare"),
    "glue (>=1.3.1)"
  )

  ip <- as.data.table(installed.packages(noCache = TRUE))[[1]]
  if ("LandR" %in% ip)
    remove.packages("LandR")
  out1 <- capture.output({
    out <- unloadRandom(unique(extractPkgName(unlist(pkgs))),
                        keepDepsOf = c("reproducible", "devtools"),
                        num = 10)
  })
  options("reproducible.Require.install" = TRUE)
  Sys.setenv("R_REMOTES_UPGRADE" = "never")
  i <- 0
  for (pkg in sample(pkgs)) {
    i <- i + 1
    #._Require_3 <<- ._installPackages_1 <<-
    #   ._installPackages_4 <<- 1
    # Remove another package, but only after SpaDES.core is no long being Require-d
    # if (!any(grepl("SpaDES.core", pkg))) {
    #   kdo <- c("reproducible")
    #   if (any(grepl("amc", pkg)))
    #     kdo <- c("amc", kdo)
    #   out1 <- capture.output(out <- unloadRandom(pkg, keepDepsOf = kdo))
    #   message("removed ", paste(out, collapse = ", "))
    # }
    suppressWarnings(rm(list = "outFromRequire", inherits = FALSE))

    ipPre <- as.data.table(installed.packages(noCache = TRUE))[[1]]
    # ._installPackages_1 <<- ._installPackages_2 <<- ._installPackages_3 <<- ._Require_1 <<- 1
    err <- capture_error({
      warn <- capture_warnings({
        mess <- capture_messages({
          outFromRequire <- Require(pkg, repos = repo, standAlone = FALSE)
        })
      })
    })
    if (length(err) == 0) {
      ipPost <- as.data.table(installed.packages(noCache = TRUE))[[1]]
      if (!"PredictiveEcology/LandR@development (>= 1.0.2)" %in% pkg &&
          "PredictiveEcology/LandR@development(>= 0.0.0.9)" %in% pkg) {
        expect_true("LandR" %in% ipPost)
      } else {
        expect_false("LandR" %in% ipPost)
      }
      if (!exists("warn", inherits = FALSE))
        warn <- character()
      if (!exists("mess", inherits = FALSE))
        mess <- character()
      if (length(warn) == 0)
        warn <- ""
      if (length(mess) == 0)
        mess <- ""
      dealWithWarns(c(mess, warn), outFromRequire)
    }
    toDetachAndRm <- c("amc", "LandR")
    for (j in toDetachAndRm) {
      out <- capture.output(try(detach(paste0("package:",j), unload = TRUE), silent = TRUE))
      out <- capture.output(try(remove.packages(j), silent = TRUE))
    }
  }

  # if (FALSE) {
  #
  #   for (lp in rev(c(origLibPaths[1], tmpdir))) {
  #     # To emulate a near vanilla install, make a hard link to reproducible dependencies
  #     .libPaths(lp)
  #     if (lp == origLibPaths[1])
  #       options("reproducible.Require.install" = TRUE)
  #     else {
  #       options("reproducible.Require.install" = FALSE)
  #       pkgDirs <- dir(setdiff(origLibPaths, tail(.libPaths(), 1)), full.names = TRUE)
  #       pkgDirs <- pkgDirs[grepl("curl", pkgDirs)]
  #       #pkgDirs <- pkgDirs[basename(pkgDirs) %in% reproducibleDeps$reproducible]
  #       pkgDirs2 <- lapply(pkgDirs, dir, recursive = TRUE, full.names = TRUE)
  #
  #       newPaths <- lapply(origLibPaths[-length(origLibPaths)], function(lp) {
  #         lapply(pkgDirs2, function(pk) gsub(lp, tmpdir, pk))
  #       })
  #       allDirs <- unique(dirname(unlist(newPaths)))
  #       out <- lapply(basename(pkgDirs), function(p) {
  #         dir.create(file.path(tmpdir, p))
  #       })
  #
  #       warn <- capture_warnings(out2 <- lapply(allDirs, dir.create, recursive = TRUE))
  #       warn2 <- capture_warnings(out <- file.link(unlist(pkgDirs2), unlist(newPaths)))
  #
  #     }
  #
  #
  #     ######## NEW FULL
  #     pkgs <- c("SpaDES.core (>=0.9)", "PredictiveEcology/reproducible@messagingOverhaul (>= 4.1.1)",
  #               "achubaty/amc@development (>=0.1.5)", "data.table (>=100.0)",
  #               "digest (>=0.6.23)", "PredictiveEcology/LandR@development (>= 1.0.2)", "versions (>=0.3)",
  #               "fastdigest (>=0.0.0.9)", "PredictiveEcology/reproducible@development (>= 0.0.0.9)",
  #               "achubaty/amc@development (>=0.0.0.9)", "data.table (>=0.0.0.9)",
  #               "PredictiveEcology/LandR@development(>= 0.0.0.9)", "fastdigest (>=1000.0.0.8)",
  #               "fastdigest", "quickPlot", "testthat",
  #               "PredictiveEcology/reproducible@messagingOverhaul (>= 0.0.0.9)",
  #               "PredictiveEcology/reproducible@messagingOverhaul (>= 0.0.0.10)",
  #               "PredictiveEcology/reproducible@development (>= 1110.0.0.9)",
  #               "PredictiveEcology/reproducible@development (>= 0.0.0.10)",
  #               "PredictiveEcology/reproducible@development (>= 0.0.0.11)"
  #     )
  #     Require(pkgs, repos = repo, standAlone = FALSE)
  #
  #     # There is no point in installing dependencies if the package version is insufficient
  #
  #     ip <- installed.packages(noCache = TRUE)
  #     assignInNamespace("isInteractive", ns = "reproducible", function() {FALSE})
  #     suppressWarnings(rm(a))
  #     warn <- capture_warnings(
  #       a <- Require(paste0("versions (>=", packageVersion("versions"), ")"),
  #                    repos = repo,
  #                    #libPath = tmpCache,
  #                    standAlone = FALSE)
  #     )
  #
  #     noPackageWarn <- any(grepl("there is no package", warn))
  #     if ("curl" %in% ip[,1]) {
  #       if (noPackageWarn) {
  #         expect_false(a)
  #       } else {
  #         expect_true(a)
  #       }
  #     }
  #
  #
  #
  #     ################################################################
  #     ################################################################
  #     pkgs <- c("SpaDES.core (>=0.9)",
  #               "PredictiveEcology/reproducible@messagingOverhaul (>= 4.1.1)",
  #               "achubaty/amc@development (>=0.1.5)",
  #               "data.table (>=100.0)",
  #               paste0("digest (>=", packageVersion("digest"),")"),
  #               "PredictiveEcology/LandR@development (>= 1.0.2)")
  #     mess <- capture_messages({
  #       warn <- capture_warnings({
  #         err <- capture_error({
  #           b <- Require(repos = repo, pkgs)})
  #       })
  #     })
  #     cat(mess, file = "c:/Eliot/tmp/testMess.txt")
  #     cat(as.character(err), file = "c:/Eliot/tmp/testErr.txt")
  #
  #     noPackageWarn <- any(grepl("there is no package", warn))
  #     if (any(noPackageWarn)) {
  #       noPackage <- strsplit(warn, "there is no package")
  #       noPackage <- gsub("^.* \'(.*)\'.*", "\\1", noPackage[[1]])
  #       noPackage <- noPackage[nzchar(noPackage)]
  #       expect_true(sum(b) == sum(!names(b) %in% noPackage))
  #     } else {
  #
  #       expect_true(sum(grepl("following packages", mess)) == 1)
  #       if (any(!extractPkgName(pkgs) %in% ip[,1])) {
  #         if (sum(grepl("not sufficiently", mess))==1) {
  #           expect_true(sum(grepl("Please manually", err)) == 1)
  #         } else
  #           expect_true(sum(grepl("Please manually", err)) == 0)
  #       } else {
  #         expect_true(sum(grepl("Please manually", err)) == 1)
  #       }
  #     }
  #
  #     ################################################################
  #     ################################################################
  #     pkgs <- c("fastdigest (>=0.0.0.9)",
  #               "PredictiveEcology/reproducible@development (>= 0.0.0.9)",
  #               "achubaty/amc@development (>=0.0.0.9)",
  #               "data.table (>=0.0.0.9)",
  #               paste0("digest (>=", packageVersion("digest"),")"),
  #               "PredictiveEcology/LandR@development(>= 0.0.0.9)")
  #
  #     suppressWarnings(rm(b))
  #     warn <- capture_warnings(
  #       mess <- capture_messages({
  #         err <- capture_error({
  #           b <- Require(repos = repo, pkgs)
  #         })
  #       })
  #     )
  #
  #     noPackageWarn <- grepl("there is no package", warn)
  #     if (any(noPackageWarn)) {
  #       noPackage <- strsplit(warn, "there is no package")
  #       noPackage <- gsub("^.* \'(.*)\'.*", "\\1", noPackage[[1]])
  #       noPackage <- noPackage[nzchar(noPackage)]
  #       expect_true(sum(b) == sum(!names(b) %in% noPackage))
  #     } else {
  #       ip <- installed.packages(.libPaths())
  #       pkgNames <- extractPkgName(pkgs)
  #       deps <- pkgDep(pkgNames, recursive = TRUE)
  #       out <- unlist(Map(d = deps, n = names(deps), function(d, n) {
  #         all(d %in% ip[,1]) && n %in% c(gsub("^.*:", "", search()), ip[, 1])
  #       }))
  #       out[["reproducible"]] <- TRUE
  #       expect_true(sum(b) == sum(out))
  #     }
  #
  #     ################################################################
  #     # Multiple conflicting version numbers, and with NO version number
  #     pkgs <- c("fastdigest (>=0.0.0.8)", "fastdigest (>=0.0.0.9)", "fastdigest", "quickPlot", "testthat")
  #     pkgName <- unique(extractPkgName(pkgs))
  #     loadedPkgs <- unlist(lapply(pkgName, isNamespaceLoaded))
  #     sumIL <- sum(installedAndLoaded(pkgName))
  #     warn <- capture_warnings(
  #       mess <- capture_messages(
  #         b <- Require(repos = repo, pkgs))
  #     )
  #     if (sum(loadedPkgs)) {
  #       expect_true(sum(b) == sumIL)
  #     }
  #
  #     ################################################################
  #     ################################################################
  #     pkgs <- c("fastdigest (>=1000.0.0.8)", "fastdigest (>=0.0.0.9)", "fastdigest",
  #               "quickPlot", "testthat")
  #     mess <- capture_messages(
  #       err <- capture_error(
  #         b <- Require(repos = repo, pkgs)
  #       ))
  #     pkgName <- unique(extractPkgName(pkgs))
  #     loadedPkgs <- unlist(lapply(pkgName, isNamespaceLoaded))
  #     sumIL <- sum(installedAndLoaded(pkgName))
  #
  #     expect_true(sum(b) == sumIL)
  #
  #     expect_true(sum(grepl("following packages", mess)) == 1)
  #     expect_true(sum(grepl("Please manually", err)) == 1)
  #
  #     # Same as above for GitHub packages
  #     pkgs <- c("fastdigest (>=0.0.0.9)",
  #               "PredictiveEcology/reproducible@messagingOverhaul (>= 0.0.0.9)",
  #               "PredictiveEcology/reproducible@messagingOverhaul (>= 0.0.0.10)",
  #               "PredictiveEcology/reproducible@development (>= 1110.0.0.9)",
  #               "achubaty/amc@development (>=0.0.0.9)",
  #               "data.table (>=0.0.0.9)",
  #               paste0("digest (>=", packageVersion("digest"),")"),
  #               "PredictiveEcology/LandR@development(>= 0.0.0.9)")
  #     suppressWarnings(rm(b, mess, warn, a))
  #     warn <- capture_warnings(
  #       mess <- capture_messages(
  #         err <- capture_error(
  #           b <- Require(repos = repo, pkgs))))
  #
  #     expect_true(sum(grepl("following packages", mess)) == 1)
  #
  #     pkgName <- unique(extractPkgName(pkgs))
  #     loadedPkgs <- unlist(lapply(pkgName, isNamespaceLoaded))
  #     sumIL <- sum(installedAndLoaded(pkgName))
  #
  #
  #     if (any(grepl("not sufficiently", mess)) && getOption("reproducible.Require.upgrade") != 3) {
  #       expect_true(grepl("Please manually", err)  )
  #     }
  #
  #     expect_true(sum(b) == sumIL)
  #
  #
  #     pkgs <- c("fastdigest (>=0.0.0.9)",
  #               "PredictiveEcology/reproducible@development (>= 0.0.0.9)",
  #               "PredictiveEcology/reproducible@development (>= 0.0.0.10)",
  #               "PredictiveEcology/reproducible@development (>= 0.0.0.11)",
  #               "achubaty/amc@development (>=0.0.0.9)",
  #               "data.table (>=0.0.0.9)",
  #               paste0("digest (>=", packageVersion("digest"),")"),
  #               "PredictiveEcology/LandR@development(>= 0.0.0.9)")
  #     suppressWarnings(rm(b))
  #     warn <- capture_warnings(
  #       mess <- capture_messages(
  #         err <- capture_error(
  #           b <- Require(repos = repo, pkgs))))
  #
  #     if (any(grepl("package or namespace load failed", mess))) {
  #       expect_true(sum(grepl("following packages", mess)) == 0)
  #     } else {
  #       expect_true(sum(grepl("following packages", mess)) == 1)
  #     }
  #
  #     pkgNames <- unique(extractPkgName(pkgs))
  #     pkgNames <- pkgNames[pkgNames %in% c(gsub("^.*:", "", search()), ip[, 1])]
  #     theGrep <- grepl("package or namespace load failed", mess)
  #     if (any(theGrep)) {
  #       cantLoad <- unique(gsub("^.*failed for '(.*)' in .*$", "\\1", mess[theGrep]))
  #       if (length(cantLoad))
  #         expect_true(all(setdiff(pkgNames, cantLoad) %in% names(b)[b]))
  #     } else {
  #       expect_true(all(pkgNames %in% names(b)[b]))
  #     }
  #
  #
  #
  #     pkgs <- "glue (>=1000.3.1)"
  #     err <- capture_error(a <- Require(repos = repo,
  #                                       pkgs, libPath = tmpCache, standAlone = TRUE))
  #     expect_true(grepl("manually install", err))
  #
  #     pkgs <- c("glue (>=0.3.1)", "fpCompare")
  #     warn <- capture_warnings(
  #       mess <- capture_messages(
  #         a <- Require(repos = repo,
  #                      pkgs, libPath = tmpCache, standAlone = TRUE)
  #       ))
  #
  #     theGrep <- grepl("cannot be unloaded", mess)
  #     if (any(theGrep)) {
  #       pkgNotLoaded <- gsub("^.*Package \'(.*)\' version.*$", "\\1", mess[theGrep])
  #       expect_true(sum(a) == sum(a[!names(a) %in% pkgNotLoaded]))
  #     } else {
  #       pkgNames <- unique(extractPkgName(pkgs))
  #       pkgNames <- pkgNames[pkgNames %in% c(gsub("^.*:", "", search()), ip[, 1])]
  #       expect_true(all(pkgNames %in% names(a)[a]))
  #     }
  #
  #     pkgs <- "glue (>=1.3.1)"
  #     pkgName <- extractPkgName(pkgs)
  #     currentVers <- packageVersion(pkgName)
  #     ap <- available.packages()
  #     pkgNames <- ap[ap[,1] %in% pkgName,2]
  #     names(pkgName) <- pkgName
  #     loadedPkgs <- lapply(pkgName, isNamespaceLoaded)
  #
  #     ._test111 <<- 1
  #     err <- capture_error(
  #       warn <- capture_warnings({
  #         mess <- capture_messages({
  #           a <- Require(repos = repo,
  #                        pkgs, libPath = tmpdir, standAlone = TRUE)
  #         })
  #       })
  #     )
  #
  #     if (length(loadedPkgs)) {
  #       expect_true(sum(grepl("Not installing", mess)) ==  length(loadedPkgs))
  #     } else {
  #       pkgNames <- unique(extractPkgName(pkgs))
  #       pkgNames <- pkgNames[pkgNames %in% c(gsub("^.*:", "", search()), ip[, 1])]
  #       expect_true(all(pkgNames %in% names(a)[a]))
  #     }
  #   }
  # }
})

test_that("package topoSort", {
  testInitOut <- testInit(libraries = c("data.table", "versions"))
  on.exit({
    try(testOnExit(testInitOut))
  }, add = TRUE)

  vals <- c("fastdigest",
            #"reproducible",
            "glue",
            "data.table",
            "digest",
            "quickPlot")
  correctOrder <- c("fastdigest", "quickPlot", "glue", "data.table", "digest")
  names(vals) <- vals
  out <- pkgDepTopoSort(vals, reverse = TRUE, returnFull = FALSE,
                 useAllInSearch = FALSE)
  expect_true(is.list(out))
  expect_true(any(names(out) != names(vals))) # reordered
  expect_true(all(names(out) == correctOrder)) # reordered

  out <- pkgDepTopoSort(vals, reverse = TRUE, returnFull = FALSE, useAllInSearch = TRUE)
  inSrch <- setdiff(search(), .defaultPackages)
  inSrch <- setdiff(gsub("package:", "", inSrch), vals)
  toCheck <- c(vals, inSrch)
  expect_true(all(names(out) %in% toCheck))

  out <- pkgDepTopoSort(vals, reverse = FALSE, returnFull = FALSE, useAllInSearch = FALSE)
  expect_true(all(names(out) %in% vals))
  expect_true(length(out$quickPlot) == 3)
  expect_true(sum(unlist(lapply(out, function(x) length(x) == 0))) == 4)

  out1 <- pkgDepTopoSort(vals, reverse = TRUE, returnFull = TRUE, useAllInSearch = FALSE)
  expect_true(all(unlist(lapply(vals, function(v) length(out1[[v]] >= length(out[[v]]))))))

  out <- pkgDep(vals, topoSort = TRUE)
  expect_true(tail(names(out), 1) == "quickPlot")
})
