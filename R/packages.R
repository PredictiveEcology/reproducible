#' Install and load packages, optionally with specific package versions
#'
#' This is an "all in one" function that will run \code{install.packages} for
#' CRAN packages, \code{devtools::install_github} for GitHub.com packages and
#' \code{versions::install.versions} if there is a \code{packageVersionFile} supplied.
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
#' @param install_githubArgs List of optional named arguments, passed to install_github
#' @param install.packagesArgs List of optional named arguments, passed to install.packages
Require <- function(packages, packageVersionFile, libPath = .libPaths()[1],
                    notOlderThan = NULL, install_githubArgs = list(),
                    install.packagesArgs = list()) {
  githubPkgs <- grep("\\/", packages, value = TRUE)
  githubPkgNames <- sapply(strsplit(githubPkgs, split = "/|@" ), function(x) x[2])
  if(length(githubPkgs)) {
    packages[packages %in% githubPkgs] <- githubPkgNames
  }
  if(!dir.exists(libPath)) dir.create(libPath)
  if(!missing(packageVersionFile)) {
    aa <- instPkgs(githubPkgs, packageVersionFile = packageVersionFile,
                           libPath = libPath)
  } else {
    cacheRepo <- file.path(libPath, ".cache")
    deps <- unlist(Cache(tools::package_dependencies, packages, recursive = TRUE,
                         cacheRepo = cacheRepo, notOlderThan = notOlderThan))

    if(length(githubPkgs)) {
      pkgPaths <- file.path(libPath, githubPkgNames)
      fileExists <- file.exists(pkgPaths)
      if(any(fileExists)) {
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
        sapply(gitPkgs, function(pk) {
          args <- append(install_githubArgs,list(pk, dependencies = FALSE, upgrade_dependencies = FALSE,
                                                 force = TRUE, local = FALSE)) # use force = TRUE because we have already eliminated
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
      do.call(install.packages, append(list(needInstall, lib = libPath, dependencies = FALSE), install.packagesArgs))
      #install.packages(needInstall, lib = libPath, dependencies = FALSE)
    }

  }


  packagesLoaded <- lapply(packages, library, character.only = TRUE)
  return(invisible(packagesLoaded))
}



#' NA-aware comparison of two vectors
#'
#' Copied from \link{http://www.cookbook-r.com/Manipulating_data/Comparing_vectors_or_factors_with_NA/}.
#' This function returns TRUE wherever elements are the same, including NA's,
#' and false everywhere else.
#'
#' @export
#' @param v1 A vector
#' @param v2 A vector
#'
#' @export
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
installedVersions <- function (pkgs, libPath, notOlderThan = NULL) {
  if (missing(libPath) || is.null(libPath)) {
    libPath <- .libPaths()[1L]
    if (length(.libPaths()) > 1L)
      message(sprintf(ngettext(length(pkgs), "Checking package in %s\n(as %s is unspecified)",
                               "Checking packages in %s\n(as %s is unspecified)"),
                      sQuote(libPath), sQuote("libPath")), domain = NA)
  }
  if (length(pkgs) > 1) {
    ans <- Cache(lapply, pkgs, installedVersions, libPath, cacheRepo = file.path(libPath, ".cache"),
                 notOlderThan = notOlderThan)
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

    # sha_line <- lines[grep("^RemoteSha: *", lines)]
    # if(length(sha_line)) {
    #   sha <- gsub("RemoteSha: ", "", sha_line)
    #   attr(vers, "SHA") <- sha
    # }
    return(vers)
  }
}

#' Install exact package versions from a package version text file & GitHub
#'
#' This uses MRAN for remote repository, accessed via \code{versions::install.versions}.
#' This will attempt to install all packages in the \code{packageVersionFile},
#' with their exact version described in that file. For GitHub packages, it will
#' use \code{\link[devtools]{install_github}}
#'
#' @export
#' @param gitHubPackages Character vectors indicating repository/packageName@branch
#' @inheritParams Require
#' @param packageVersionFile Path to the package version file, defaults to
#'        the \code{.packageVersions.txt}.
#' @importFrom versions install.versions
instPkgs <- function(gitHubPackages, packageVersionFile = ".packageVersions.txt",
                     libPath = .libPaths()[1]) {
  if(file.exists(packageVersionFile)) {
    message("Reading ", packageVersionFile)
    instPkgs <- dir(libPath)
    instVers <- installedVersions(instPkgs, libPath)
    inst <- data.frame(instPkgs, instVers=unlist(instVers), stringsAsFactors = FALSE)
    supposedToBe <- read.table(packageVersionFile, header = TRUE, stringsAsFactors = FALSE)
    together <- merge(supposedToBe, inst, by="instPkgs")
    needInst1 <- setdiff(supposedToBe$instPkgs,inst$instPkgs)
    needInst2 <- which(!compareNA(together$instVers.x, together$instVers.y))
    wh1 <- which(supposedToBe$instPkgs %in% needInst1)
    wh2 <- needInst2
    whPkgsNeeded <- sort(unique(c(wh1,wh2)))
    if(length(whPkgsNeeded)) {
      packages <- supposedToBe$instPkgs[whPkgsNeeded]
      ghPackages <- sapply(strsplit(sapply(strsplit(gitHubPackages, split="/"), function(x) x[2]), split = "@"),function(x) x[1])
      pkgsOmitted <- (ghPackages %in% packages)

      whPkgsNeededFromCran <- whPkgsNeeded[!(packages %in% ghPackages)]
      whPkgsNeededGH <- gitHubPackages[pkgsOmitted]
      message("Needing to install ", length(whPkgsNeededGH), " packages from GitHub.com:\n",
              "  ", paste(whPkgsNeededGH, collapse = ", "))
      message("Needing to install ", length(supposedToBe$instPkgs[whPkgsNeededFromCran]),
              " packages from MRAN or CRAN:\n",
              "  ", paste(supposedToBe$instPkgs[whPkgsNeededFromCran], collapse = ", "))
      if(length(whPkgsNeededFromCran)) {
          message("Installing from MRAN failed.\n",
                  "Trying source installs from CRAN Archives")
          avail <- available.packages()
          wh <- avail[,"Package"] %in% supposedToBe[whPkgsNeededFromCran,"instPkgs"]
          canInstDirectFromCRAN <- merge(
            data.frame(avail[wh,c("Package", "Version"), drop = FALSE], stringsAsFactors = FALSE),
            supposedToBe[whPkgsNeededFromCran,],
            by.x = c("Package", "Version"), by.y = c("instPkgs", "instVers"))


          if(nrow(canInstDirectFromCRAN)) {
            install.packages(canInstDirectFromCRAN$Package,
                             dependencies = FALSE, lib = libPath)
          }

          stillNotYet <- dplyr::anti_join(supposedToBe[whPkgsNeededFromCran,], canInstDirectFromCRAN,
                                          by = c("instPkgs"="Package", "instVers"="Version"))
          if(nrow(stillNotYet)) {
            packageURLs <- file.path(options()$repos[length(options()$repos)],"src/contrib/Archive",
                                     stillNotYet[,"instPkgs"],
                                     paste0(stillNotYet[,"instPkgs"],"_",
                                            stillNotYet[,"instVers"],".tar.gz"))
          }

          lapply(packageURLs, function(pack) {
            install.packages(pack, repos = NULL, type = "source", lib = libPath,
                                      dependencies = FALSE)
          })

          # instReport <- tryCatch(install.versions(supposedToBe$instPkgs[whPkgsNeededFromCran],
          #                  supposedToBe$instVers[whPkgsNeededFromCran], dependencies = FALSE),
          #     error = function(x) FALSE)
          # if(!instReport) {

         # return(supposedToBe[whPkgsNeededFromCran,"Package"])
        #}
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
    message("There is no package version file named ", packageVersionFile)
  }
}

#' Take a snapshot of all the packages and version numbers
#'
#' This can be used later by \code{instPkgs} to install or re-install the correct versions.
#'
#' @export
#' @param packageVersionFile A filename to save the packages and their currently
#'        installed version numbers. Defaults to \code{".packageVersions.txt"}.
#' @param libPath The path to the local library where packages are installed.
#'        Defaults to the .libPaths()[1]
#' @inheritParams Require
pkgSnapshot <- function(packageVersionFile, libPath, notOlderThan = NULL) {
  if(missing(libPath)) libPath <- .libPaths()[1]
  if(missing(packageVersionFile)) packageVersionFile <- ".packageVersions.txt"
  instPkgs <- dir(libPath)
  instVers <- installedVersions(instPkgs, libPath, notOlderThan = notOlderThan)
  # shas <- lapply(instVers, function(vers) {
  #   attr(vers, "SHA")
  # })
  # shas <- shas[!unlist(lapply(shas, is.null))]
  # if(length(shas)) {
  #   instVers[names(shas)] <- shas
  # }
  inst <- data.frame(instPkgs, instVers=unlist(instVers), stringsAsFactors = FALSE)
  write.table(inst, file = packageVersionFile, row.names = FALSE)
}

