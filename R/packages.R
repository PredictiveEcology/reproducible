#' Repeatability-safe install and load packages, optionally with specific versions
#'
#' This is an "all in one" function that will run \code{install.packages} for
#' CRAN packages, \code{devtools::install_github} for GitHub.com packages and
#' will install specific versions of each package if
#' there is a \code{packageVersionFile} supplied.
#' Plus, when \code{packages} is provided as a character vector, or a
#' \code{packageVersionFile} is supplied, all package dependencies
#' will be first assessed for \code{unique(dependencies)} so the same package is
#' not installed multiple times. Finally \code{library} is called on the
#' \code{packages}. If packages are already installed (\code{packages} supplied),
#' and their version numbers are exact (when \code{packageVersionFile} is supplied),
#' then the "install" component will be skipped very quickly with a message.
#'
#' @note This function will use \code{Cache} internally to determine the dependencies
#' of all \code{packages}. The cache repository will be inside the \code{libPath},
#' and will be named \code{.cache}. This will speed up subsequent calls to \code{Require}
#' dramatically.
#' It will not take into account version numbers for this
#' caching step. If package versions are updated manually by the user, then this cached
#' element should be wiped, using \code{notOlderThan = Sys.time()}.
#'
#' @export
#' @importFrom tools package_dependencies
#' @importFrom desc desc_get
#' @importFrom devtools install_github
#' @param packages Character vector of packages to install via
#'        \code{install.packages}, then load (i.e., with \code{library})
#' @param packageVersionFile If provided, then this will override all \code{install.package}
#'        calls with \code{versions::install.versions}
#' @param libPath The library path where all packages should be installed, and looked for to load
#'        (i.e., call \code{library})
#' @inheritParams Cache
#' @param notOlderThan Time or Date. The \code{Cache} is used internally for
#'                     \code{tools::package_dependencies} and for already installed packages that
#'                     came from github. To purge the cache: \code{notOlderThan = Sys.time()}
#' @param install_githubArgs List of optional named arguments, passed to install_github
#' @param install.packagesArgs List of optional named arguments, passed to install.packages
#'
#' @examples
#' \dontrun{
#'
#' # simple usage, like conditional install.packages then library
#' Require("stats") # analogous to require(stats), but slower because it checks for
#'                  #   pkg dependencies, and installs them, if missing
#' tempPkgFolder <- file.path(tempdir(), "Packages")
#' Require("crayon", libPath = tempPkgFolder) # install.packages first, then library
#'
#' # make a package version snapshot
#' packageVersionFile <- file.path(tempPkgFolder, ".packageVersion.txt")
#' pkgSnapshot(libPath=tempPkgFolder, packageVersionFile)
#'
#' Require("crayon", packageVersionFile = packageVersionFile)
#'
#' # Create mismatching versions -- desired version is older than current installed
#' # This will install the older version, overwriting the newer version
#' desiredVersion <- data.frame(instPkgs="crayon", instVers = "1.3.2", stringsAsFactors = FALSE)
#' write.table(file = packageVersionFile, desiredVersion, row.names = FALSE)
#' Require("crayon", packageVersionFile = packageVersionFile)
#'
#' # Mutual dependencies, only installs once -- e.g., httr
#' Require(c("cranlogs", "covr"), libPath = tempPkgFolder)
#'
#' }
#'
Require <- function(packages, packageVersionFile, libPath = .libPaths()[1],
                    notOlderThan = NULL, install_githubArgs = list(),
                    install.packagesArgs = list()) {
  githubPkgs <- grep("\\/", packages, value = TRUE)
  githubPkgNames <- sapply(strsplit(githubPkgs, split = "/|@" ), function(x) x[2])
  if(length(githubPkgs)) {
    packages[packages %in% githubPkgs] <- githubPkgNames
  }
  if(!dir.exists(libPath)) dir.create(libPath)
  libPath <- normalizePath(libPath, winslash = "/") # the system call requires this

  # Two parts -- one if there is a packageVersionFile -- This calls the external file installVersions
  #           -- two if there is no packageVersionFile
  if(!missing(packageVersionFile)) {
    Sys.setlocale("LC_ALL", "C") # required to deal with non English characters in Author names
    aa <- installVersions(githubPkgs, packageVersionFile = packageVersionFile,
                          libPath = libPath)
  } else {
    cacheRepo <- file.path(libPath, ".cache")
    deps <- unlist(Cache(tools::package_dependencies, packages, recursive = TRUE,
                         cacheRepo = cacheRepo, notOlderThan = notOlderThan))
    if(length(githubPkgs)) {
      pkgPaths <- file.path(libPath, githubPkgNames)
      fileExists <- file.exists(pkgPaths)
      if(any(fileExists)) {
        if(!is.null(install_githubArgs$dependencies)) {
          if(isTRUE(install_githubArgs$dependencies)) {
            gitPkgDeps <- unlist(Cache(lapply, pkgPaths[fileExists], function(p) {
              lapply(c("Imports", "Suggests", "Depends"), function(type) {
                strsplit(desc::desc_get(key=type,  p), split = ",.{0,2} +")
              })
            }, cacheRepo = cacheRepo, notOlderThan = notOlderThan))
            gitPkgDeps <- unname(unlist(lapply(strsplit(gitPkgDeps, split = "\n| "), function(x) x[1])))
            deps <- unique(c(deps, gitPkgDeps))
            deps <- deps[deps != "R"]
          }
        }
      }
    }
    allPkgsNeeded <- unique(c(deps, packages))
    installedPkgs <- sapply(libPath, dir)
    basePkgs <- sapply(.libPaths()[length(.libPaths())], dir)
    needInstall <- allPkgsNeeded[!(allPkgsNeeded %in% unique(unlist(installedPkgs)))]
    needInstall <- needInstall[!(needInstall %in% basePkgs)]
    if(length(needInstall)) {
      gitPkgs <- githubPkgs[githubPkgNames %in% needInstall]
      if(length(gitPkgs)) {

        oldLibPaths <- .libPaths()
        .libPaths(c(libPath, oldLibPaths))
        args <- append(install_githubArgs,list(dependencies = FALSE, upgrade_dependencies = FALSE,
                                               force = TRUE, local = FALSE)) # use xforce = TRUE because we have already eliminated
        sapply(gitPkgs, function(pk) {
          args <- append(args, list(pk))
          # the cases where we have the correct version; install_github uses a
          # local database hidden somewhere that won't let the same package be installed
          # twice, even if in different libPaths
          args <- args[!duplicated(names(args))]
          do.call(install_github, args)
          # with_libpaths doesn't work because it will look for ALL packages there; can't download without curl
        })
        .libPaths(oldLibPaths)
        Require(unlist(gitPkgs), libPath = libPath, notOlderThan = notOlderThan,
                install_githubArgs = install_githubArgs,
                install.packagesArgs = install.packagesArgs) # This sends it back in with all the install_github calls completed
        return(NULL)
      }
      # RSTudio won't install packages if they are loaded. THe algorithm is faulty and it won't let things install even when they are not loaded.
      #do.call(install.packages, append(list(needInstall, lib = libPath, dependencies = FALSE), install.packagesArgs))
      repos <- getOption("repos")[1]
      if ( is.null(repos) | any(repos == "") ) {
        repos <- "https://cran.rstudio.com"
      }
      lapply(needInstall, function(pkg){
        system(paste0(file.path(R.home(), "bin", "R"),
                      " --vanilla -e do.call(install.packages,list('",pkg,"',lib='",
                      libPath,"',dependencies=FALSE,repos='",repos,"'))"), wait=TRUE)

      })
    }

  }

  oldLibPath <- .libPaths()
  .libPaths(libPath)
  packagesLoaded <- suppressMessages(unlist(lapply(packages, function(p) {
    try(require(p, character.only = TRUE), silent = TRUE)
    })))
  .libPaths(oldLibPath)
  if(any(!packagesLoaded)) {
    message("Simultaneous package versions being used. Can only load first version(s) loaded in this session:\n",
            packages[!packagesLoaded])
    packagesLoaded2 <- unlist(lapply(packages[!packagesLoaded], function(p) {
      try(require(p, character.only = TRUE))
    }))


  }
  return(invisible(packagesLoaded))
}



#' NA-aware comparison of two vectors
#'
#' Copied from \url{http://www.cookbook-r.com/Manipulating_data/Comparing_vectors_or_factors_with_NA/}.
#' This function returns TRUE wherever elements are the same, including NA's,
#' and false everywhere else.
#'
#' @export
#' @param v1 A vector
#' @param v2 A vector
#'
#' @examples
#' a <- c(NA, 1, 2, NA)
#' b <- c(1, NA, 2, NA)
#' compareNA(a,b)
compareNA <- function(v1,v2) {
  same <- (v1 == v2)  |  (is.na(v1) & is.na(v2))
  same[is.na(same)] <- FALSE
  return(same)
}

#' A shortcut to create a .libPaths() with only 2 folders
#'
#' This will remove all but the top level of .libPaths(), which should be the base packages
#' installed with R, and adds a second directory, the \code{libPath}.
#'
#' @return
#' Invisibly the new \code{.libPaths()}
#' @param libPath A path that will be the new .libPaths()[1]
#' @export
#' @examples
#' \dontrun{
#' newLibPaths("testPackages")
#' .libPaths() # new .libPaths
#' }
newLibPaths <- function(libPath) {
  .libPaths(.libPaths()[length(.libPaths())])
  suppressWarnings(dir.create(libPath))
  .libPaths(libPath)
  invisible(.libPaths())
}


#' Determine versions all installed pacakges
#'
#' This code is taken very directly from the \code{installed.versions} function in the
#' \code{versions} package, but uses an \code{Rcpp} alternative
#' to readLines for speed. It will be anywhere from 2x to 10x faster than the \code{installed.versions}
#' \code{versions} function. This is also many times faster (1000x ? for the 1 package
#' case) than \code{utils::installed.packages}
#' especially if only a subset of "all" packages in libPath are desired.
#' @export
#' @param pkgs Character vector of packages to determine which version is installed in the \code{libPath}.
#' @inheritParams installVersions
#' @examples
#' installedVersions("reproducible", .libPaths()[1])
installedVersions <- function (pkgs, libPath) {
  if (missing(libPath) || is.null(libPath)) {
    libPath <- .libPaths()[1L]
    if (length(.libPaths()) > 1L)
      message(sprintf(ngettext(length(pkgs), "Checking package in %s\n(as %s is unspecified)",
                               "Checking packages in %s\n(as %s is unspecified)"),
                      sQuote(libPath), sQuote("libPath")), domain = NA)
  }
  if (length(pkgs) > 1) {
    ans <- lapply(pkgs, installedVersions, libPath)
    names(ans) <- pkgs
    return(ans)
  } else if (length(pkgs)==0)  {
    return(character())
  }
  desc_path <- sprintf("%s/%s/DESCRIPTION", libPath, pkgs)
  if (!file.exists(desc_path)) {
    return(NA)
  }
  else {
    lines <- readLinesRcpp(desc_path);
    lines <- strsplit(lines, split = "\n")[[1]];
    vers_line <- lines[grep("^Version: *", lines)]
    vers <- gsub("Version: ", "", vers_line)
    return(vers)
  }
}

#' Install exact package versions from a package version text file & GitHub
#'
#' This uses CRAN, CRAN archives, or MRAN (accessed via \code{versions::install.versions})
#' for remote repositories.
#' This will attempt to install all packages in the \code{packageVersionFile},
#' with their exact version described in that file. For GitHub packages, it will
#' use \code{\link[devtools]{install_github}}. This will be called internally by
#' \code{Require}, and so often doesn't need to be used by a user.
#'
#' Because of potential conflicts with loaded packages, this function will run
#' \code{install.packages} in a separate R process.
#'
#' @export
#' @param gitHubPackages Character vectors indicating repository/packageName@branch
#' @inheritParams Require
#' @param packageVersionFile Path to the package version file, defaults to
#'        the \code{.packageVersions.txt}.
#' @importFrom versions install.versions
#' @importFrom data.table setDT data.table setnames
#' @importFrom utils read.table available.packages installed.packages
#' @examples
#' \dontrun{
#' # requires the packageVersionFile -- this doesn't work -- safer to use Require
#' installVersions("PredictiveEcology/reproducible@development")
#'
#' # make a package version snapshot
#' packageVersionFile <- file.path(tempPkgFolder, ".packageVersion.txt")
#' pkgSnapshot(libPath=tempPkgFolder, packageVersionFile)
#'
#' tempPkgFolder <- file.path(tempdir(), "Packages")
#' Require("crayon", libPath = tempPkgFolder) # install.packages first, then library
#'
#' # install a specific version
#' # make a package version snapshot
#' packageVersionFile <- file.path(tempPkgFolder, ".packageVersion.txt")
#' pkgSnapshot(libPath=tempPkgFolder, packageVersionFile)
#'
#' installVersions("crayon", packageVersionFile = packageVersionFile)
#'
#' }
installVersions <- function(gitHubPackages, packageVersionFile = ".packageVersions.txt",
                            libPath = .libPaths()[1]) {
  if(file.exists(packageVersionFile)) {
    libPath <- normalizePath(libPath, winslash = "/") # the system call requires this
    message("Reading ", packageVersionFile)
    libPathListFiles <- dir(libPath)
    allPkgsDESC <- file.info(file.path(libPath, libPathListFiles, "DESCRIPTION"))
    .snap <- file.path(libPath, ".snapshot.RDS")
    needSnapshot <- FALSE
    needInstalledVersions <- FALSE
    installedVersionsFile <- file.path(libPath, ".installedVersions.RDS")
    if (!file.exists(installedVersionsFile)) {
      needSnapshot <- TRUE
      needInstalledVersions <- TRUE
    }
    if (file.exists(.snap)) {
      if(!(identical(readRDS(file = .snap), allPkgsDESC))) {
        needSnapshot <- TRUE
        needInstalledVersions <- TRUE
      }
    } else {
      needSnapshot <- TRUE
      needInstalledVersions <- TRUE
    }
    if(needSnapshot) {
      saveRDS(allPkgsDESC, file = .snap)
    }

    if(needInstalledVersions) {
      instVers <- installedVersions(libPathListFiles, libPath)
      saveRDS(instVers, file = installedVersionsFile)
    } else {
      instVers <- readRDS(file = installedVersionsFile)
    }

    instVers <- lapply(instVers, sub, pattern="\\r|\\n", replacement = "")
    if(length(instVers)!=length(libPathListFiles)) stop("Package folder, ", .libPaths()[1],
                                                        " has become corrupt. Please manually delete .snapshot.RDS and .installedVersions.RDS")
    inst <- data.frame(havePkgs=libPathListFiles, haveVers=unlist(instVers), stringsAsFactors = FALSE)
    supposedToBe <- read.table(packageVersionFile, header = TRUE, stringsAsFactors = FALSE)
    together <- merge(supposedToBe, inst, by.x="instPkgs",by.y="havePkgs")
    needPkgs <- setdiff(supposedToBe$instPkgs,inst$havePkgs)
    needVers <- which(!compareNA(together$instVers, together$haveVers))
    wh1 <- supposedToBe[which(supposedToBe$instPkgs %in% needPkgs),]
    wh2 <- together[needVers,]
    whPkgsNeeded <- rbind(wh1, wh2[,c("instPkgs","instVers")]) #sort(unique(c(wh1[,"instPkgs"],wh2[,"instPkgs"])))
    if(nrow(whPkgsNeeded)) {
      packages <- whPkgsNeeded[,"instPkgs"]
      if(length(gitHubPackages)) {
        ghPackages <- sapply(strsplit(sapply(strsplit(gitHubPackages, split="/"), function(x) x[2]), split = "@"),function(x) x[1])
      } else {
        ghPackages <- character(0)
      }
      pkgsOmitted <- (ghPackages %in% packages)

      whPkgsNeededFromCran <- whPkgsNeeded[!(packages %in% ghPackages),]
      whPkgsNeededGH <- gitHubPackages[pkgsOmitted]
      if(length(whPkgsNeededGH)) {
        message("Needing to install ", length(whPkgsNeededGH), " packages from GitHub.com:\n",
              "  ", paste(whPkgsNeededGH, collapse = ", "))
      }
      if(nrow(whPkgsNeededFromCran)) {
        neededCRANpkgs <- paste(paste0(apply(whPkgsNeededFromCran, 1, paste, collapse=" ("),")"), collapse=", ")
        message("Needing to install ", nrow(whPkgsNeededFromCran),
              " packages from MRAN or CRAN:\n",
              "  ", neededCRANpkgs)
      }
      failed <- data.frame(instPkgs = character(), instVers = character())
      if(nrow(whPkgsNeededFromCran)) {
        avail <- available.packages()
        wh <- avail[,"Package"] %in% whPkgsNeededFromCran[,"instPkgs"]
        whPkgsAvailFromCran <- data.frame(avail[wh,c("Package", "Version"), drop = FALSE], stringsAsFactors = FALSE)

        colnames(whPkgsAvailFromCran) <- c("instPkgs", "instVers")

        setDT(whPkgsAvailFromCran)
        setDT(whPkgsNeededFromCran)

        tryCRANarchive <- whPkgsNeededFromCran[!whPkgsAvailFromCran, on = c("instPkgs", "instVers")]
        canInstDirectFromCRAN <- whPkgsAvailFromCran[whPkgsNeededFromCran, nomatch=0, on = c("instPkgs", "instVers")]

        if(nrow(canInstDirectFromCRAN)) {
          repos <- getOption("repos")[1]
          if ( is.null(repos) | any(repos == "") ) {
            repos <- "https://cran.rstudio.com"
          }
          lapply(canInstDirectFromCRAN$instPkgs, function(pkg){
            system(paste0(file.path(R.home(), "bin", "R"), " --vanilla -e do.call(install.packages,list('",pkg,
                          "',lib='",libPath,"',dependencies=FALSE,repos='",repos,"'))"), wait=TRUE)

          })

          AP <- installed.packages(lib.loc = libPath)
          actuallyInstalled <- data.table(AP[(AP[,"Package"] %in% canInstDirectFromCRAN$instPkgs),
                                             c("Package", "Version"),drop=FALSE])
          setnames(actuallyInstalled, old = c("Package", "Version"), new= c("instPkgs", "instVers"))

          setDT(actuallyInstalled)
          failed <- rbind(failed, canInstDirectFromCRAN[actuallyInstalled, nomatch=0, on = c("instPkgs", "instVers")])
        }

        if(nrow(tryCRANarchive)) {
          repos <- getOption("repos")[1]
          if ( is.null(repos) | any(repos == "") ) {
            repos <- "https://cran.rstudio.com"
          }
          packageURLs <- file.path(repos[length(repos)],"src/contrib/Archive",
                                   tryCRANarchive$instPkgs,
                                   paste0(tryCRANarchive$instPkgs,"_",
                                          tryCRANarchive$instVers,".tar.gz"))

          lapply(packageURLs, function(pkg){
            system(paste0(file.path(R.home(), "bin", "R"), " --vanilla -e install.packages('",pkg,
                          "',lib='",libPath,"',dependencies=FALSE,repos=NULL,type='source')"), wait=TRUE)

          })

          AP <- installed.packages(lib.loc = libPath)
          actuallyInstalled <- data.table(AP[(AP[,"Package"] %in% tryCRANarchive$instPkgs),c("Package", "Version"),drop=FALSE])
          setnames(actuallyInstalled, old = c("Package", "Version"), new = c("instPkgs", "instVers"))
          failed <- rbind(failed, tryCRANarchive[!actuallyInstalled, on = c("instPkgs", "instVers")])
        }

        if(nrow(failed)) {
          message("Trying MRAN install of ",failed$Package)
          multiSource <- paste0(file.path(R.home(), "bin", "R"), " --vanilla -e versions::install.versions('",
                                failed$Package,"','",failed$Version,
                                "',lib='",libPath,"',dependencies=FALSE,type='source')")
          lapply(multiSource, system, wait=TRUE)
        }


      }
      if(length(whPkgsNeededGH)) {
        whPkgsNeededGHNames <- ghPackages[pkgsOmitted]
        lapply(whPkgsNeededGH, function(pkg) {
          oldLibPaths <- .libPaths()
          .libPaths(c(libPath, oldLibPaths))
          devtools::install_github(pkg, upgrade_dependencies = FALSE, local = FALSE,
                                   force = TRUE)
          .libPaths(oldLibPaths)

        })
      }
    } else {
      message("All packages are correct versions")
    }
  } else {
    message("There is no package version file named ", packageVersionFile, ".\n",
            "No package versions installed.")
  }
}

#' Take a snapshot of all the packages and version numbers
#'
#' This can be used later by \code{installVersions} to install or re-install the correct versions.
#'
#' @export
#' @param packageVersionFile A filename to save the packages and their currently
#'        installed version numbers. Defaults to \code{".packageVersions.txt"}.
#' @param libPath The path to the local library where packages are installed.
#'        Defaults to the .libPaths()[1]
#' @details
#' A file is written with the package names and versions of all packages within \code{libPath}.
#' This can later be passed to \code{Require}.
#'
#' @inheritParams Require
#' @importFrom utils write.table
#' @examples
#' pkgSnapFile <- tempfile()
#' pkgSnapshot(pkgSnapFile, .libPaths()[1])
#' data.table::fread(pkgSnapFile)
#'
pkgSnapshot <- function(packageVersionFile, libPath) {
  if(missing(libPath)) libPath <- .libPaths()[1]
  if(missing(packageVersionFile)) packageVersionFile <- ".packageVersions.txt"
  instPkgs <- dir(libPath)
  instVers <- installedVersions(instPkgs, libPath = libPath)
  inst <- data.frame(instPkgs, instVers=unlist(instVers), stringsAsFactors = FALSE)
  write.table(inst, file = packageVersionFile, row.names = FALSE)
}

