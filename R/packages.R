#' Install and load packages
#'
#' This is an "all in one" function that will run \code{install.packages} for
#' CRAN packages and \code{devtools::install_github} for GitHub.com packages, plus
#' all their dependencies with \code{unique(dependencies)} so the same package is
#' not installed multiple times, and finally \code{library} is called on the
#' \code{packages}.
#'
#' @export
#' @importFrom tools package_dependencies
#' @importFrom desc desc_get
#' @param packages Character vector of packages to install via
#'        \code{install.packages}, then load (i.e., with \code{library})
Require <- function(packages, forceInstall = FALSE, ...) {
  githubPkgs <- grep("\\/", packages, value = TRUE)
  githubPkgNames <- sapply(strsplit(githubPkgs, split = "/|@" ), function(x) x[2])
  if(length(githubPkgs)) {
    packages[packages %in% githubPkgs] <- githubPkgNames
  }
  cacheRepo <- file.path(.libPaths()[1], ".cache")
  deps <- unlist(Cache(tools::package_dependencies, packages, recursive = TRUE,
                       cacheRepo = cacheRepo, notOlderThan = list(...)$notOlderThan))

  if(length(githubPkgs)) {
    gitPkgDeps <- unlist(Cache(lapply, file.path(.libPaths()[1], githubPkgNames), function(p) {
      lapply(c("Imports", "Suggests", "Depends"), function(type) {
        strsplit(desc::desc_get(key=type,  p), split = ",.{0,2} +")
      })
    }, cacheRepo = cacheRepo, notOlderThan = list(...)$notOlderThan))
    gitPkgDeps <- unname(unlist(lapply(strsplit(gitPkgDeps, split = "\n| "), function(x) x[1])))
    deps <- unique(c(deps, gitPkgDeps))
    deps <- deps[deps != "R"]
  }
  allPkgsNeeded <- unique(c(deps, packages))
  installedPkgs <- sapply(.libPaths(), dir)
  needInstall <- allPkgsNeeded[!(allPkgsNeeded %in% unique(unlist(installedPkgs)))]
  if(length(needInstall)) {
    gitPkgs <- githubPkgs[githubPkgNames %in% needInstall]
    if(length(gitPkgs)) {
      sapply(gitPkgs, install_github, dependencies = FALSE)
      Require(unlist(needInstall))#[!(needInstall %in% githubPkgNames[githubPkgNames %in% needInstall])])
      return(NULL)
    }
    install.packages(needInstall, dependencies = FALSE)
  }

  packagesLoaded <- lapply(packages, library, character.only = TRUE)
  return(invisible(packagesLoaded))
}



#' NA-aware Comparison of two vectors
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

#' A shortcut to create a .libPaths() with 2 folders
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
#' \code{versions} function
#' @export
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
#' @export
#' @param gitHubPackages Character vectors indicating repository/packageName@branch
#' @param packageVersionFile Path to the package version file, defaults to
#'        the \code{.packageVersions.txt}.
#' @param libPath A folder in which all packages should be installed. Default is the first package in
#'        \code{.libPaths()}
#' @importFrom versions install.versions
instPkgs <- function(gitHubPackages, packageVersionFile = ".packageVersions.txt",
                     libPath = .libPaths()[1]) {
  libPath <- if(missing(libPath)) .libPaths()[1] else libPath
  #if(missing(packageVersionFile)) packageVersionFile <- ".packageVersions.txt"
  # packageVersionFile <- file.path(libPath, packageVersionFile)
  if(file.exists(packageVersionFile)) {
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
      if(length(whPkgsNeededFromCran))
        install.versions(supposedToBe$instPkgs[whPkgsNeededFromCran],
                         supposedToBe$instVers[whPkgsNeededFromCran], dependencies = FALSE)
      if(length(whPkgsNeededGH)) {
        whPkgsNeededGHNames <- ghPackages[pkgsOmitted]
        lapply(whPkgsNeededGH, function(pkg) {
          devtools::install_github(pkg, upgrade_dependencies = FALSE)
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
pkgSnapshot <- function(packageVersionFile, libPath) {
  if(missing(libPath)) libPath <- .libPaths()[1]
  if(missing(packageVersionFile)) packageVersionFile <- ".packageVersions.txt"
  instPkgs <- dir(libPath)
  instVers <- installedVersions(instPkgs, libPath)
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

