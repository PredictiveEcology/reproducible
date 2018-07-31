if (getRversion() >= "3.1.0") {
  utils::globalVariables(c(".SD", "instPkgs"))
}

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
#' \code{standAlone} will either put the \code{Require}d packages and their dependencies
#' \emph{all} within the libPath (if \code{TRUE}) or if \code{FALSE} will only install
#' packages and their dependencies that are otherwise
#' not installed in \code{.libPaths()}, i.e., the personal or base library paths. Any
#' packages or dependencies that are not yet installed will be installed in \code{libPath}.
#' Importantly, a small hidden file (named \code{._packageVersionsAuto.txt})
#' will be saved in \code{libPath} that will store the \emph{information} about
#' the packages and their dependencies, even if the version used is located in
#' \code{.libPaths()}, i.e., not the \code{libPath} provided.
#' This hidden file will be used if a user runs \code{pkgSnapshot},
#' enabling a new user to rebuild the entire dependency chain, without having to
#' install all packages in an isolated directory (as does \pkg{packrat}).
#' This will save potentially a lot of time and disk space, and yet maintain reproducibility.
#' \emph{NOTE}: since there is only one hidden file in a \code{libPath}, any call to
#' \code{pkgSnapshot} will make a snapshot of the most recent call to \code{Require}.
#'
#' To build a snapshot of the desired packages and their versions, first run \code{Require}
#' with all packages, then \code{pkgSnapshot}. If a \code{libPath} is used, it must be used
#' in both functions.
#'
#' This function works best if all required packages are called within one \code{Require}
#' call, as all dependencies can be identified together, and all package versions will be saved
#' automatically (with \code{standAlone = TRUE} or \code{standAlone = FALSE}),
#' allowing a call to \code{pkgSnapshot} when a more permanent record of versions can be made.
#'
#' @note This function will use \code{memoise} internally to determine the dependencies
#' of all \code{packages}. This will speed up subsequent calls to \code{Require}
#' dramatically. However, it will not take into account version numbers for this
#' memoised step. If package versions are updated manually by the user, then this cached
#' element should be wiped, using \code{forget = TRUE}.
#'
#' @export
#' @importFrom devtools install_github
#' @importFrom utils install.packages
#' @param packages Character vector of packages to install via
#'        \code{install.packages}, then load (i.e., with \code{library}). If it is
#'        one package, it can be unquoted (as in \code{require})
#' @param packageVersionFile If provided, then this will override all \code{install.package}
#'        calls with \code{versions::install.versions}
#' @param libPath The library path where all packages should be installed, and looked for to load
#'        (i.e., call \code{library})
#' @param repos The remote repository (e.g., a CRAN mirror), passed to either
#'              \code{install.packages}, \code{install_github} or \code{installVersions}.
#' @param install_githubArgs List of optional named arguments, passed to install_github
#' @param install.packagesArgs List of optional named arguments, passed to install.packages
#' @param standAlone Logical. If \code{TRUE}, all packages will be installed and loaded strictly
#'                   from the \code{libPaths} only. If \code{FALSE}, all \code{.libPaths} will
#'                   be used to find the correct versions. This can be create dramatically faster
#'                   installs if the user has a substantial number of the packages already in their
#'                   personal library. In the case of \code{TRUE}, there will be a hidden file
#'                   place in the \code{libPath} directory that lists all the packages
#'                   that were needed during the \code{Require} call. Default \code{FALSE} to
#'                   minimize package installing.
#' @param forget Internally, this function identifies package dependencies using a memoised
#'               function for speed on reuse. But, it may be inaccurate in some cases,
#'               if packages were installed manually by a user. Set this to \code{TRUE} to
#'               refresh that dependency calculation.
#'
#'
#' @examples
#' \dontrun{
#' # simple usage, like conditional install.packages then library
#' Require("stats") # analogous to require(stats), but slower because it checks for
#'                  #   pkg dependencies, and installs them, if missing
#' tempPkgFolder <- file.path(tempdir(), "Packages")
#'
#' # use standAlone, means it will put it in libPath, even if it already exists
#' #   in another local library (e.g., personal library)
#' Require("crayon", libPath = tempPkgFolder, standAlone = TRUE)
#'
#' # make a package version snapshot
#' packageVersionFile <- file.path(tempPkgFolder, ".packageVersion.txt")
#' pkgSnapshot(libPath=tempPkgFolder, packageVersionFile)
#'
#' # confirms that correct version is installed
#' Require("crayon", packageVersionFile = packageVersionFile)
#'
#' # Create mismatching versions -- desired version is older than current installed
#' # This will try to install the older version, overwriting the newer version
#' desiredVersion <- data.frame(instPkgs="crayon", instVers = "1.3.2", stringsAsFactors = FALSE)
#' write.table(file = packageVersionFile, desiredVersion, row.names = FALSE)
#' # won't work because newer crayon is loaded
#' Require("crayon", packageVersionFile = packageVersionFile)
#'
#' # unload it first
#' detach("package:crayon", unload = TRUE)
#'
#' # run again, this time, correct "older" version installs in place of newer one
#' Require("crayon", packageVersionFile = packageVersionFile)
#'
#' # Mutual dependencies, only installs once -- e.g., httr
#' Require(c("cranlogs", "covr"), libPath = tempPkgFolder)
#'
#' }
#'
Require <- function(packages, packageVersionFile, libPath = .libPaths()[1], # nolint
                    install_githubArgs = list(),       # nolint
                    install.packagesArgs = list(), standAlone = FALSE,      # nolint
                    repos = getOption("repos"), forget = FALSE) {

  # Convert a single name to a character
  subpackages <- substitute(packages)
  if (is.name(subpackages)) { # single, non quoted object
    subpackages <- deparse(subpackages)
    if (!exists(subpackages, envir = parent.frame())) # if it does exist, then it is not a name, but an object
      packages <- subpackages
  }

  if (!is.character(packages)) {
    stop("packages should be a character vector or a name")
  }

  if (!is.null(packages)) {
    githubPkgs <- grep("\\/", packages, value = TRUE)
    githubPkgNames <- sapply(strsplit(githubPkgs, split = "/|@"), function(x) x[2])
    if (length(githubPkgs)) {
      packages[packages %in% githubPkgs] <- githubPkgNames
    }
    if (!dir.exists(libPath)) dir.create(libPath)
    libPath <- normalizePath(libPath, winslash = "/") # the system call requires this

    if (standAlone) {
      # include only base if standAlone
      nonLibPathPaths <- .libPaths()[length(.libPaths())]
      ips <- unname(installed.packages(priority = "high")[, "Package"])
      nonLibPathPkgs <- ips # unlist(lapply(nonLibPathPaths, dir))
    } else {
      nonLibPathPaths <- setdiff(.libPaths(), libPath)
      nonLibPathPkgs <- unique(unlist(lapply(nonLibPathPaths, dir)))
    }

    # Two parts: 1) if there is a packageVersionFile (this calls the external file installVersions)
    #            2) if there is no packageVersionFile
    if (!missing(packageVersionFile)) {
      Sys.setlocale(locale = "C") # required to deal with non English characters in Author names
      aa <- installVersions(githubPkgs, packageVersionFile = packageVersionFile,
                            libPath = libPath, standAlone = standAlone, repos = repos)
      Sys.setlocale(locale = "")
      allPkgsNeeded <- aa$instPkgs
    } else {
      aa <- .installPackages(packages, githubPkgs = githubPkgs, githubPkgNames = githubPkgNames,
                             install_githubArgs = install_githubArgs,
                             nonLibPathPkgs = nonLibPathPkgs, libPath = libPath,
                             standAlone = standAlone, forget = forget)
      allPkgsNeeded <- aa$allPkgsNeeded
      if (standAlone) {
        libPathListFiles <- unlist(lapply(unique(c(libPath, .libPaths()[length(.libPaths())])),
                                          dir, full.names = TRUE))
      } else {
        libPathListFiles <- unlist(lapply(unique(c(libPath, .libPaths())), dir, full.names = TRUE))
      }
      libPathListFiles <- libPathListFiles[basename(libPathListFiles) %in% allPkgsNeeded]
      currentVersions <- installedVersionsQuick(libPathListFiles, libPath, standAlone = standAlone,
                             basename(libPathListFiles))
      if (is.null(names(currentVersions))) names(currentVersions) <- allPkgsNeeded
    }

    autoFile <- file.path(libPath, "._packageVersionsAuto.txt")
    if (is.null(aa$haveVers)) {
      pkgsToSnapshot <- pickFirstVersion(names(currentVersions), unlist(currentVersions))
      .pkgSnapshot(pkgsToSnapshot$instPkgs, pkgsToSnapshot$instVers, packageVersionFile = autoFile)
    } else {
      .pkgSnapshot(aa$instPkgs, aa$haveVers, packageVersionFile = autoFile)
      pkgSnapshot <- aa
    }

    oldLibPath <- .libPaths()
    if (standAlone) .libPaths(libPath) else .libPaths(c(libPath, .libPaths()))
    packagesLoaded <- unlist(lapply(packages, function(p) {
      try(require(p, character.only = TRUE))
    }))
    .libPaths(oldLibPath)
    if (any(!packagesLoaded)) {
      message("Simultaneous package versions being used.",
              " Can only load first version(s) loaded in this session:\n",
              paste(packages[!packagesLoaded], collapse = ", "))
      packagesLoaded2 <- unlist(lapply(packages[!packagesLoaded], function(p) {
        try(require(p, character.only = TRUE, quietly = TRUE))
      }))


    }
  } else {
    packagesLoaded <- NULL
  }

  return(invisible(packagesLoaded))
}

#' \code{NA}-aware comparison of two vectors
#'
#' Copied from
#' \url{http://www.cookbook-r.com/Manipulating_data/Comparing_vectors_or_factors_with_NA/}.
#' This function returns \code{TRUE} wherever elements are the same, including \code{NA}'s,
#' and \code{FALSE} everywhere else.
#'
#' @export
#' @param v1 A vector
#' @param v2 A vector
#'
#' @examples
#' a <- c(NA, 1, 2, NA)
#' b <- c(1, NA, 2, NA)
#' compareNA(a, b)
#'
compareNA <- function(v1, v2) {
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

#' Determine versions all installed packages
#'
#' This code is adapted from \code{\link[versions]{installed.versions}},
#' but uses an \code{Rcpp} alternative to \code{readLines} for speed.
#' It will be anywhere from 2x to 10x faster than the
#' \code{\link[versions]{installed.versions}} function.
#' This is also many times faster than \code{utils::installed.packages},
#' especially if only a subset of "all" packages in \code{libPath} are desired
#' (1000x ? for the 1 package case).
#'
#' @param packages Character vector of packages to determine which version is
#'                 installed in the \code{libPath}.
#'
#' @export
#' @inheritParams installVersions
#' @examples
#' installedVersions("reproducible", .libPaths()[1])
#'
installedVersions <- function(packages, libPath) {
  if (missing(libPath) || is.null(libPath)) {
    libPath <- .libPaths()[1L]
    if (length(.libPaths()) > 1L)
      message(sprintf(ngettext(length(packages), "Checking package in %s\n(as %s is unspecified)",
                               "Checking packages in %s\n(as %s is unspecified)"),
                      sQuote(libPath), sQuote("libPath")), domain = NA)
  }
  if (length(packages) > 1) {
    if (length(packages) == length(libPath)) {
      ans <- lapply(seq_along(packages), function(x) installedVersions(packages[x], libPath[x]))
    } else {
      ans <- lapply(packages, installedVersions, libPath)
    }
    names(ans) <- packages
    return(ans)
  } else if (length(packages) == 0)  {
    return(character())
  }
  desc_path <- sprintf("%s/%s/DESCRIPTION", libPath, packages) # nolint
  if (!file.exists(desc_path)) {
    return(NA)
  } else {
    lines <- readLinesRcpp(desc_path);
    Sys.setlocale(locale = "C") # required to deal with non English characters in Author names
    on.exit(Sys.setlocale(locale = ""))
    vers_line <- lines[grep("^Version: *", lines)] # nolint
    vers <- gsub("Version: ", "", vers_line)
    return(vers)
  }

}

pkgDepRaw <- function(packages, libPath, recursive = TRUE, depends = TRUE,
                      imports = TRUE, suggests = FALSE, linkingTo = TRUE,
                      repos = getOption("repos")) {
  if (missing(libPath) || is.null(libPath)) {
    libPath <- .libPaths()#[1L]
  }

  if (length(libPath) > 1) {
    ans <- list()
    # Using loop next allows the ability to break out of search
    #  if initial .libPaths have the package
    for (lp in libPath) {
       ans1 <- pkgDep(packages, lp)
       ans <- append(ans, list(ans1))
       if (all(unlist(lapply(ans, function(x) all(unlist(lapply(x, is.character))))))) {
         break
       }
    }
    if (length(packages) == 1) {
      ans <- list(ans)
    } else {
      #invert the list, so by package name
      ans <- lapply(names(ans[[1]]), function(nam) {
        ll2 <- lapply(ans, function(x) x[[nam]])
      })
    }
    names(ans) <- packages

    ll2 <- lapply(ans, function(x) {
      ll1 <- unique(na.omit(unlist(x)))
      attr(ll1, "na.action") <- NULL
      attr(ll1, "class") <- NULL
      ll1
    })

    # package_dependencies and PkgDep will differ under the following circumstances
    # 1. github packages are not detected using tools::package_dependencies
    # 2. package_dependencies does not detect the dependencies of base packages,
    #    e.g,. methods depends on stats and graphics
    notInstalled <- unlist(lapply(ll2, function(y) length(y) == 0 & is.logical(y)))
    ll2[notInstalled] <- NA
    if (any(notInstalled)) {

      paste(names(ll2[notInstalled]), collapse = ", ")
      repos <- getCRANrepos(repos)

      availPackagesDb <- available.packagesMem(repos = repos)
      ll3 <- package_dependenciesMem(names(ll2[notInstalled]), db = availPackagesDb,
                                     recursive = recursive)
      # the previous line will miss base packages
      ll3 <- lapply(ll3, function(x) {
        unique(c(x, unlist(pkgDep(x, libPath = unique(c(libPath, .libPaths())),
                                  recursive = recursive))))
      })

      ll2[notInstalled] <- ll3
    }
    return(ll2)
  }

  if (length(packages) > 1) {
    if (length(packages) == length(libPath)) {
      ans <- lapply(seq_along(packages), function(x) pkgDep(packages[x], libPath[x]))
    } else {
      ans <- lapply(packages, pkgDep, libPath)
    }
    names(ans) <- packages
    return(ans)
  } else if (length(packages) == 0)  {
    return(character())
  }

  desc_path <- sprintf("%s/%s/DESCRIPTION", libPath, packages) # nolint
  if (!file.exists(desc_path)) {
    return(NA)
  } else {
    lines <- readLinesRcpp(desc_path)
    Sys.setlocale(locale = "C") # required to deal with non English characters in Author names
    on.exit(Sys.setlocale(locale = ""))
    deps_line <- grep("^Depends: *", lines) # nolint
    sugg_line <- grep("^Suggests: *", lines) # nolint
    imports_line <- grep("^Imports: *", lines) # nolint
    linkingTo_line <- grep("^LinkingTo: *", lines) # nolint
    colon_line <- grep(": *", lines) # nolint

    needed <- character()
    if (imports) {
      if (length(imports_line)) {
      imports_lines <- imports_line:(colon_line[which(colon_line %in% imports_line) + 1] - 1) # nolint
      imprts <- paste(lines[imports_lines], collapse = "")
      imprts <- gsub("Imports: ", "", imprts)
      imprts <- strsplit(imprts, split = ", *")
      needed <- c(needed, imprts[[1]])
    }}

    if (depends) {
      if (length(deps_line)) {
      deps_lines <- deps_line:(colon_line[which(colon_line %in% deps_line) + 1] - 1) # nolint
      deps <- paste(lines[deps_lines], collapse = "")
      deps <- gsub("Depends: ", "", deps)
      deps <- strsplit(deps, split = ", *")
      needed <- c(needed, deps[[1]])
    }}

    if (suggests) {
      if (length(sugg_line)) {
      sugg_lines <- sugg_line:(colon_line[which(colon_line %in% sugg_line) + 1] - 1) # nolint
      sugg <- paste(lines[sugg_lines], collapse = "")
      sugg <- gsub("Suggests: ", "", sugg)
      sugg <- strsplit(sugg, split = ", *")
      needed <- c(needed, sugg[[1]])
    }}

    if (linkingTo) {
      if (length(linkingTo_line)) {
        linkingTo_lines <- linkingTo_line:(colon_line[which(colon_line %in% linkingTo_line) + 1] - 1) # nolint
        link <- paste(lines[linkingTo_lines], collapse = "")
        link <- gsub("LinkingTo: ", "", link)
        link <- strsplit(link, split = ", *")
        needed <- c(needed, link[[1]])
      }}

    needed <- grep("^R[\\( ]", needed, value = TRUE, invert = TRUE)

    if (length(needed)) {
      # hasVersionNumber <- regmatches(needed, gregexpr(pattern = "(?<=\\().*?(?=\\))",
      #                                                 needed, perl = TRUE))[[1]]
      hasVersionNumber <- unlist(lapply(needed, function(x) {
        regmatches(x, gregexpr(pattern = "(?<=\\().*?(?=\\))", x, perl = TRUE))[[1]]
      }))
      if (length(hasVersionNumber)) {
        for (pat in hasVersionNumber) {
          needed <- sub(pattern = paste0("\\(", pat, "\\)"), needed, replacement = "")
        }
        needed <- gsub(needed, pattern = " *", replacement = "")
      }
    }

    if (recursive) {
      # note that recursive searching must search in all libPaths, not just current one
      # like miniCRAN::pkgDep not recursive on Suggests
      needed2 <- pkgDep(needed, libPath = unique(c(libPath, .libPaths())),
                                  depends = depends, imports = imports, suggests = FALSE)
      needed <- na.omit(unique(c(needed, unlist(needed2))))
      attr(needed, "na.action") <- NULL
      attr(needed, "class") <- NULL
    }
    return(needed)
  }
}

#' Determine package dependencies, first looking at local filesystem
#'
#' This is intended to replace \code{\link[tools]{package_dependencies}} or
#' \code{pkgDep} in the \pkg{miniCRAN} package, but with modifications for speed.
#' It will first check local package directories in \code{libPath}, and it if
#' the function cannot find the packages there, then it will use
#' \code{\link[tools]{package_dependencies}}.
#'
#' @note \code{package_dependencies} and \code{pkgDep} will differ under the following
#' circumstances:
#' \enumerate{
#'   \item GitHub packages are not detected using \code{tools::package_dependencies};
#'   \item \code{tools::package_dependencies} does not detect the dependencies of base packages
#'     among themselves, \emph{e.g.}, \code{methods} depends on \code{stats} and \code{graphics}.
#' }
#'
#' @inheritParams tools::package_dependencies
#' @inheritParams Require
#' @param depends Logical. Include packages listed in "Depends". Default TRUE.
#' @param imports Logical. Include packages listed in "Imports". Default TRUE.
#' @param suggests Logical. Include packages listed in "Suggests". Default FALSE.
#' @param linkingTo Logical. Include packages listed in "LinkingTo". Default TRUE.
#' @param recursive Logical. Should dependencies of dependencies be searched, recursively.
#'                  NOTE Dependencies of suggests will not be recursive. Default TRUE.
#' @export
#' @importFrom memoise memoise
#' @rdname pkgDep
#'
#' @examples
#' pkgDep("crayon")
pkgDep <- memoise(pkgDepRaw)

pkgDep2 <- memoise(pkgDepRaw)

#' Memoised version of package_dependencies
#'
#' This have a 6 minute memory time window.
#'
#' @importFrom memoise memoise timeout
#' @importFrom tools package_dependencies
#' @inheritParams tools::package_dependencies
#' @rdname package_dependenciesMem
package_dependenciesMem <- memoise(tools::package_dependencies, ~timeout(360)) # nolint

#' Memoised version of available.packages
#'
#' This have a 6 minute memory time window.
#' @inheritParams utils::available.packages
#' @keywords internal
available.packagesMem <- function(contriburl, method, fields, type, filters, repos) {# This will be replaced upon first calls to
  stop("This function is for internal use only")
  return(invisible(NULL))
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
#' @importFrom RCurl url.exists
#' @importFrom data.table setDT data.table setnames rbindlist
#' @importFrom utils read.table available.packages installed.packages install.packages
#' @examples
#' \dontrun{
#' # requires the packageVersionFile -- this doesn't work -- safer to use Require
#' installVersions("PredictiveEcology/reproducible@development")
#'
#' # make a package version snapshot -- this will be empty because no packages in directory
#' tempPkgFolder <- file.path(tempdir(), "Packages")
#' dir.create(tempPkgFolder)
#' packageVersionFile <- file.path(tempPkgFolder, ".packageVersion.txt")
#' pkgSnapshot(libPath=tempPkgFolder, packageVersionFile)
#'
#' Require("crayon", libPath = tempPkgFolder) # install.packages first, then library
#'
#' # install a specific version
#' # make a package version snapshot
#' packageVersionFile <- file.path(tempPkgFolder, ".packageVersion.txt")
#' pkgSnapshot(libPath=tempPkgFolder, packageVersionFile, standAlone = FALSE)
#'
#' installVersions("crayon", packageVersionFile = packageVersionFile)
#'
#' }
installVersions <- function(gitHubPackages, packageVersionFile = ".packageVersions.txt",
                            libPath = .libPaths()[1], standAlone = FALSE,
                            repos = getOption("repos")["CRAN"]) {

  if (file.exists(packageVersionFile)) {
    libPath <- normalizePath(libPath, winslash = "/") # the system call requires this
    message("Reading ", packageVersionFile)
    if (standAlone) {
      libPathListFiles <- dir(libPath, full.names = TRUE)
    } else {
      libPathListFiles <- unlist(lapply(unique(c(libPath, .libPaths())), dir, full.names = TRUE))
    }

    supposedToBe <- data.table::fread(packageVersionFile, header = TRUE)
    supposedToBe <- unique(supposedToBe, by = c("instPkgs", "instVers"))
    data.table::setkeyv(supposedToBe, c("instPkgs", "instVers"))
    supposedToBe <- supposedToBe[, list(instVers = max(instVers)), by = "instPkgs"]

    uniqued <- !duplicated(basename(libPathListFiles))
    libPathListFilesUniqued <- libPathListFiles[uniqued];
    libPathListFiles <- libPathListFilesUniqued[basename(libPathListFilesUniqued) %in%
                                                  supposedToBe$instPkgs]
    libPathListFilesBase <- basename(libPathListFiles)

    instVers <- installedVersionsQuick(libPathListFiles, libPath, standAlone,
                                               libPathListFilesBase)

    if (length(instVers) != length(libPathListFilesBase)) {
      stop("Package folder, ", libPath, " has become corrupt.",
           " Please manually delete .snapshot.RDS and .installedVersions.RDS")
    }

    inst <- pickFirstVersion(libPathListFilesBase, unlist(instVers))
    data.table::setnames(inst, "instVers", "haveVers")

    together <- inst[supposedToBe, on = c(instPkgs = "instPkgs")]

    needPkgs <- setdiff(supposedToBe$instPkgs, inst$instPkgs)

    eq <- together$haveVers == together$instVers
    needVersEqual <- which(!eq)

    # Here test that the installed version is greater than required one
    isLoaded <- unlist(lapply(together$instPkgs[needVersEqual], isNamespaceLoaded))
    if (!is.null(isLoaded)) {
      canInstall <- together[needVersEqual[!isLoaded], ]
      cantInstall <- together[needVersEqual[isLoaded], ]
    } else {
      canInstall <- together[0, ]
      cantInstall <- together[needVersEqual, ]
    }

    gte <- together$haveVers[needVersEqual] < together$instVers[needVersEqual]
    gte <- is.na(gte) | gte

    needVers <- needVersEqual[gte]
    wh1 <- supposedToBe[which(supposedToBe$instPkgs %in% needPkgs), ]
    wh2 <- together[needVers, ]
    if (NROW(canInstall)) {
      message("Already have ", paste(canInstall$instPkgs, collapse = ", "), " version(s) ",
              paste(canInstall$haveVers, collapse = ", "),
              ". Trying to install requested version(s) ", paste(canInstall$instVers,
                                                                 collapse = ", "))
      wh2 <- merge(canInstall, wh2, all.x = TRUE)
    }
    if (NROW(cantInstall)) {
      warning("Can't install ", paste(cantInstall$instPkgs, collapse = ", "), " version(s) ",
              paste(cantInstall$instVers, collapse = ", "),
              " because version(s) ", paste(cantInstall$haveVers, collapse = ", "),
              " is already loaded.",
              " Try restarting R or unloading that package.")
    }

    whPkgsNeeded <- rbindlist(list(wh1, wh2[, list(instPkgs, instVers)]))
    whPkgsNeeded <- unique(whPkgsNeeded, by = c("instPkgs", "instVers"))

    if (nrow(whPkgsNeeded)) {
      internetExists <- url.exists("www.google.com")

      packages <- whPkgsNeeded[, "instPkgs"]
      if (length(gitHubPackages)) {
        ghPackages <- sapply(strsplit(sapply(strsplit(gitHubPackages, split = "/"),
                                             function(x) x[2]), split = "@"), function(x) x[1])
      } else {
        ghPackages <- character(0)
      }
      pkgsOmitted <- (ghPackages %in% packages)

      whPkgsNeededFromCran <- whPkgsNeeded[!(packages %in% ghPackages), ]
      whPkgsNeededGH <- gitHubPackages[pkgsOmitted]
      if (length(whPkgsNeededGH)) {
        message("Needing to install ", length(whPkgsNeededGH), " packages from GitHub.com:\n",
                "  ", paste(whPkgsNeededGH, collapse = ", "))
      }
      if (nrow(whPkgsNeededFromCran)) {
        neededCRANpkgs <- paste(paste0(apply(whPkgsNeededFromCran, 1, paste,
                                             collapse = " ("), ")"), collapse = ", ")
        message("Needing to install ", nrow(whPkgsNeededFromCran),
              " packages from MRAN or CRAN:\n",
              "  ", neededCRANpkgs)
      }
      failed <- data.frame(instPkgs = character(), instVers = character())

      repos <- getCRANrepos(repos)

      rpath <- file.path(R.home(), "bin", "R")

      rtests <- Sys.getenv("R_TESTS")
      isEmptyRtests <- nchar(rtests) == 0
      if (!isEmptyRtests) {
        Sys.setenv(R_TESTS = "")
        on.exit(Sys.setenv(R_TESTS = rtests), add = TRUE)
      }

      if (nrow(whPkgsNeededFromCran)) {
        if (internetExists) {
          avail <- available.packages(repos = repos)
        } else {
          avail <- cbind(Package = character(), Version = character())
        }

        wh <- avail[, "Package"] %in% whPkgsNeededFromCran[, "instPkgs"]
        whPkgsAvailFromCran <- data.frame(avail[wh, c("Package", "Version"), drop = FALSE],
                                          stringsAsFactors = FALSE)

        colnames(whPkgsAvailFromCran) <- c("instPkgs", "instVers")

        setDT(whPkgsAvailFromCran)
        setDT(whPkgsNeededFromCran)

        tryCRANarchive <- whPkgsNeededFromCran[!whPkgsAvailFromCran, on = c("instPkgs", "instVers")]
        canInstDirectFromCRAN <- whPkgsAvailFromCran[whPkgsNeededFromCran, nomatch = 0,
                                                     on = c("instPkgs", "instVers")]

        if (NROW(canInstDirectFromCRAN)) {
          if (internetExists) {
            lapply(canInstDirectFromCRAN$instPkgs, function(pkg) {
              system(paste0(rpath, " --quiet --vanilla -e \"do.call(install.packages,list('",
                            pkg, "', lib='", libPath, "', dependencies = FALSE,  = '", repos,
                            "'))\""), wait = TRUE)
            })
          } else {
            message("No connection to the internet. Can't install packages.")
          }

          AP <- installed.packages(lib.loc = libPath) # nolint
          actuallyInstalled <- data.table(AP[(AP[, "Package"] %in% canInstDirectFromCRAN$instPkgs),
                                             c("Package", "Version"), drop = FALSE])
          setnames(actuallyInstalled, old = c("Package", "Version"),
                   new = c("instPkgs", "instVers"))

          setDT(actuallyInstalled)
          failed <- rbind(failed, canInstDirectFromCRAN[!actuallyInstalled,
                                                        on = c("instPkgs", "instVers")])
        }

        if (NROW(tryCRANarchive) > 0 & all(!is.na(tryCRANarchive$instPkgs))) {
          archiveReposAttempts <- 0
          archiveReposSuccess <- FALSE
          while (any(!archiveReposSuccess)) {
            packageURLs <- file.path(repos, "src/contrib/Archive", tryCRANarchive$instPkgs,
                                     paste0(tryCRANarchive$instPkgs, "_",
                                            tryCRANarchive$instVers, ".tar.gz"))
            for (pkg in packageURLs) {
              if (url.exists(pkg)) {
                system(paste0(rpath, " --quiet --vanilla -e \"install.packages('", pkg,
                              "', lib='", libPath,
                              "', dependencies = FALSE, repos = NULL, type = 'source')\""), wait = TRUE)
                archiveReposSuccess <- TRUE
                break
              }
            }
            if (!any(archiveReposSuccess)) {
              archiveReposAttempts <- archiveReposAttempts + 1
              repos <- getCRANrepos(repos)
            }
            if (archiveReposAttempts > 1) archiveReposSuccess <- TRUE
          }

          AP <- installed.packages(lib.loc = libPath) # nolint
          actuallyInstalled <- data.table(AP[(AP[, "Package"] %in% tryCRANarchive$instPkgs),
                                             c("Package", "Version"), drop = FALSE])
          setnames(actuallyInstalled, old = c("Package", "Version"),
                   new = c("instPkgs", "instVers"))
          failed <- rbind(failed, tryCRANarchive[!actuallyInstalled,
                                                 on = c("instPkgs", "instVers")])
        }

        if (nrow(failed)) {
          if (internetExists) {

            message("Trying MRAN install of ", paste(failed$instPkgs, collapse = ", "))
            type <- if (.Platform$OS.type == "windows") "win.binary" else "source"

            multiSource <- paste0(rpath, " --quiet --vanilla -e \"versions::install.versions('",
                                  failed$instPkgs, "','", failed$instVers,
                                  "',lib='", libPath, "',dependencies=FALSE,type='", type, "')\"")
            lapply(multiSource, system, wait = TRUE)
          } else {
            message("No connection to the internet. Can't install packages.")
          }
        }
      }

      if (length(whPkgsNeededGH)) {
        whPkgsNeededGHNames <- ghPackages[pkgsOmitted]
        lapply(whPkgsNeededGH, function(pkg) {
          oldLibPaths <- .libPaths()
          .libPaths(c(libPath, oldLibPaths))
          devtools::install_github(pkg, upgrade_dependencies = FALSE, local = FALSE, force = TRUE)
          .libPaths(oldLibPaths)
        })
      }
    } else {
      if (length(needVersEqual)) {
        tog <- together[needVersEqual, ]
        message(paste(tog$instPkgs, collapse = ", "),
                " version incorrect, but wrong version loaded. Cannot install.")
        message("Keeping installed version.")
        colnames(tog) <- c("Package", "Installed Version", "Requested Version")
        rownames(tog) <- NULL
        print(tog)
        message("If this version is ok, you can update ", packageVersionFile,
                " using pkgSnapshot().",
                " If not ok, unload ", paste(tog$Package, collapse = ", "),
                ", uninstall the newer version(s),",
                " or restart R, ensuring package(s) don't load") # nolint
      } else {
        message("All packages are correct versions.")
        if (!is.null(isLoaded)) {
          message("However, because a package was already loaded in the namespace,",
                  " the desired version may not be correctly loaded.")
        }
      }
      message("")
    }
  } else {
    message("There is no package version file named ", packageVersionFile, ".\n",
            "No package versions installed.")
  }
  return(together)
}

#' Internal function to install packages
#'
#' @inheritParams Require
#' @param repos The remote repository (e.g., a CRAN mirror), passed to \code{install.packages},
#' @param githubPkgs Character vector of github repositories and packages, in the
#'                   form \code{repository/package@branch}, with branch being optional.
#' @param githubPkgNames Character vector of the package names, i.e., just the R package name.
#' @param nonLibPathPkgs Character vector of all installed packages that are in \code{.libPaths},
#'                       but not in \code{libPath}. This would normally include a listing of
#'                       base packages, but may also include other library paths if
#'                       \code{standAlone} if \code{FALSE}
#' @importFrom data.table data.table setDT setnames
#' @importFrom memoise forget is.memoised memoise
#' @importFrom utils assignInMyNamespace available.packages install.packages installed.packages read.table
#' @importFrom versions install.versions
#' @rdname installPackages
#' @examples
#' \dontrun{
#'   .installPackages("crayon")
#' }
.installPackages <- function(packages, repos = getOption("repos"),
                             githubPkgs = character(0), githubPkgNames,
                             nonLibPathPkgs = character(0), install_githubArgs, # nolint
                             install.packagesArgs = list(), # nolint
                             libPath = .libPaths()[1], standAlone = standAlone,
                             forget = FALSE) {

  if (!is.memoised(available.packagesMem)) {
    assignInMyNamespace("available.packagesMem", memoise(available.packages, ~timeout(360))) # nolint
  }

  forget(pkgDep)
  if (forget) forget(pkgDep2)
  deps <- unlist(pkgDep2(packages, unique(c(libPath, .libPaths())),
                                              recursive = TRUE))
  if (length(deps) == 0) deps <- NULL
  allPkgsNeeded <- na.omit(unique(c(deps, packages)))

  if (missing(githubPkgNames)) {
    githubPkgNames <- sapply(strsplit(githubPkgs, split = "/|@"), function(x) x[2])
  }

  libPathPkgs <- unlist(lapply(libPath, dir))
  needInstall <- allPkgsNeeded[!(allPkgsNeeded %in% unique(libPathPkgs))]
  needInstall <- needInstall[!(needInstall %in% nonLibPathPkgs)]
  if (length(needInstall)) {
    internetExists <- url.exists("www.google.com")
    gitPkgs <- githubPkgs[githubPkgNames %in% needInstall]
    if (length(gitPkgs)) {
      oldLibPaths <- .libPaths()
      .libPaths(unique(c(libPath, oldLibPaths)))

      # use xforce = TRUE because we have already eliminated
      args <- append(install_githubArgs, list(dependencies = NA,
                                              upgrade_dependencies = TRUE,
                                              force = TRUE))#, local = FALSE))
      if (internetExists) {
        sapply(gitPkgs, function(pk) {
        args <- append(args, list(pk))
        # the cases where we have the correct version; install_github uses a
        # local database hidden somewhere that won't let the same package be installed
        # twice, even if in different libPaths
        args <- args[!duplicated(names(args))]

        do.call(install_github, args)
        # with_libpaths doesn't work because it will look for ALL packages there;
        # can't download without curl
      })
      } else {
        message("No connection to the internet. Can't install packages.")
      }
      .libPaths(oldLibPaths)

      # This sends it back in with all the install_github calls which may or may not
      #   include dependencies correctly -- especially with standAlone = TRUE
      #   Basically, install_github needs packages like curl, which will likely be
      #   in the user's personal library, so, we can't omit personal library from
      #   .libPath(), but that means that dependencies may not be installed in
      #   libPath if they exist in the personal library. Sending it back into function
      #   will work
      Require(unique(c(needInstall[!(needInstall %in% githubPkgNames)], gitPkgs)),
              libPath = libPath, forget = TRUE,
              install_githubArgs = install_githubArgs, standAlone = standAlone,
              install.packagesArgs = install.packagesArgs)
    } else {
      # RStudio won't install packages if they are loaded. The algorithm is faulty
      # and it won't let things install even when they are not loaded.
      repos <- getCRANrepos(repos)

      rpath <- file.path(R.home(), "bin", "R")
      rtests <- Sys.getenv("R_TESTS")
      isEmptyRtests <- nchar(rtests) == 0
      if (!isEmptyRtests) {
        Sys.setenv(R_TESTS = "")
        on.exit(Sys.setenv(R_TESTS = rtests), add = TRUE)
      }

      if (internetExists) {
        aa <- lapply(needInstall[!(needInstall %in% githubPkgNames)], function(pkg) {
          syscall <- paste0("--quiet --vanilla -e \"utils::install.packages('", pkg,
                            "',dependencies=FALSE,lib='", libPath, "',repos=c('",
                            paste(repos, collapse = "','"), "'))\"")
          system(paste(rpath, syscall), wait = TRUE)
        })
      } else {
        message("No connection to the internet. Can't install packages.")
      }

    }
  }

  return(invisible(list(instPkgs = needInstall, allPkgsNeeded = allPkgsNeeded)))
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
pkgSnapshot <- function(packageVersionFile, libPath, standAlone = FALSE) {
  if (missing(libPath)) {
    if (standAlone) libPath <- .libPaths()[1] else libpath <- .libPaths()
  } else {
    if (!standAlone) libPath <- unique(c(libPath, .libPaths()))
  }
  if (missing(packageVersionFile)) packageVersionFile <- ".packageVersions.txt"

  isAbsolute <-
    if (regexpr("^~", packageVersionFile) != -1L) {
      TRUE
    } else if (regexpr("^.:(/|\\\\)", packageVersionFile) != -1L) {
      TRUE
    } else {
      components <- strsplit(packageVersionFile, split = "[/\\]")[[1L]]
      if (length(components) == 0L) {
        FALSE
      } else {
      (components[1L] == "")
      }
    }

  if (!isAbsolute) packageVersionFile <- file.path(libPath[1], basename(packageVersionFile))

  autoFile <- file.path(libPath[1], "._packageVersionsAuto.txt")
  if (!standAlone & file.exists(autoFile)){
      file.copy(autoFile, packageVersionFile, overwrite = TRUE)
      out <- data.table::fread(autoFile)
  } else {
    if (!file.exists(autoFile) & !standAlone) {
      message("There is no ", autoFile,
              " and standAlone is FALSE. This snapshot will not be accurate",
              " because it will include all packages in ",
              paste(libPath, collapse = ", "))
    }
    instPkgs <- dir(libPath)
    instVers <- unlist(lapply(libPath, function(lib)
                    na.omit(unlist(installedVersions(instPkgs, libPath = lib)))))
    if (length(instVers) == 1) names(instVers) <- instPkgs

    out <- .pkgSnapshot(names(instVers), instVers, packageVersionFile)
  }
  out
}

pickFirstVersion <- function(instPkgs, instVers) {
  out <- data.table(instPkgs = instPkgs, instVers = instVers)
  out <- unique(out, by = c("instPkgs", "instVers"))
  out <- out[, .SD[1], by = "instPkgs"] # pick one in libPath, because that is
                                        # the one used, if there are more than 1 copy
  out
}

installedVersionsQuick <- function(libPathListFiles, libPath, standAlone = FALSE,
                                   libPathListFilesBase) {
  allPkgsDESC <- file.info(file.path(libPathListFiles, "DESCRIPTION"))
  .snap <- file.path(libPath, ".snapshot.RDS")
  needSnapshot <- FALSE
  needInstalledVersions <- FALSE
  installedVersionsFile <- file.path(libPath, ".installedVersions.RDS")
  if (!file.exists(installedVersionsFile)) {
    needSnapshot <- TRUE
    needInstalledVersions <- TRUE
  }
  if (file.exists(.snap)) {
    if (!(identical(readRDS(file = .snap), allPkgsDESC))) {
      needSnapshot <- TRUE
      needInstalledVersions <- TRUE
    }
  } else {
    needSnapshot <- TRUE
    needInstalledVersions <- TRUE
  }
  if (needSnapshot) {
    saveRDS(allPkgsDESC, file = .snap)
  }

  if (needInstalledVersions) {
    if (standAlone) {
      instVers <- installedVersions(libPathListFilesBase, libPath)
    } else {
      instVers <- installedVersions(libPathListFilesBase, dirname(libPathListFiles))
    }
    saveRDS(instVers, file = installedVersionsFile)
  } else {
    instVers <- readRDS(file = installedVersionsFile)
  }

  return(instVers)
}
