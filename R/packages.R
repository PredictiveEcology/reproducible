if (getRversion() >= "3.1.0") {
  utils::globalVariables(c(
    ".SD", "instPkgs", "..colsToKeep", "..colsToKeepNow",
    "Account", "availableOnCRAN", "availableOnGitHub", "AvailableVersion", "AvailableVersionCRAN",
    "Branch", "colsToKeep", "compareVersionAvail", "compareVersionAvailGH", "correctVersion",
    "correctVersionAvail", "correctVersionAvailGH", "currentInstalled", "fullGit", "githubPkgName",
    "hasMinVersion", "hasVersionSpec", "inequality", "installed", "installFrom", "isGH",
    "minVersion", "newColNames", "notCorrectVersions", "Package", "packageFullName",
    "Repo", "RepoWBranch", "upgrade", "Version", "versionOnGH", "versionSpec"
  ))
}

#' Repeatability-safe install and load packages, optionally with specific versions
#'
#' \lifecycle{maturing}
#'
#' This is an "all in one" function that will run \code{install.packages} for
#' CRAN packages, \code{remotes::install_github} for \url{https://github.com/} packages and
#' will install specific versions of each package if there is a
#' \code{packageVersionFile} supplied. Plus, when \code{packages} is provided as
#' a character vector, or a \code{packageVersionFile} is supplied, all package
#' dependencies will be first assessed for \code{unique(dependencies)} so the
#' same package is not installed multiple times. Finally \code{library} is
#' called on the \code{packages}. If packages are already installed
#' (\code{packages} supplied), and their version numbers are exact (when
#' \code{packageVersionFile} is supplied), then the "install" component will be
#' skipped very quickly with a message.
#'
#' \code{standAlone} will either put the \code{Require}d packages and their
#' dependencies \emph{all} within the libPath (if \code{TRUE}) or if
#' \code{FALSE} will only install packages and their dependencies that are
#' otherwise not installed in \code{.libPaths()}, i.e., the personal or base
#' library paths. Any packages or dependencies that are not yet installed will
#' be installed in \code{libPath}. Importantly, a small hidden file (named
#' \code{._packageVersionsAuto.txt}) will be saved in \code{libPath} that will
#' store the \emph{information} about the packages and their dependencies, even
#' if the version used is located in \code{.libPaths()}, i.e., not the
#' \code{libPath} provided. This hidden file will be used if a user runs
#' \code{pkgSnapshot}, enabling a new user to rebuild the entire dependency
#' chain, without having to install all packages in an isolated directory (as
#' does \pkg{packrat}). This will save potentially a lot of time and disk space,
#' and yet maintain reproducibility. \emph{NOTE}: since there is only one hidden
#' file in a \code{libPath}, any call to \code{pkgSnapshot} will make a snapshot
#' of the most recent call to \code{Require}.
#'
#' To build a snapshot of the desired packages and their versions, first run
#' \code{Require} with all packages, then \code{pkgSnapshot}.
#'  If a \code{libPath} is used, it must be used in both functions.
#'
#' This function works best if all required packages are called within one
#' \code{Require} call, as all dependencies can be identified together, and all
#' package versions will be saved automatically (with \code{standAlone = TRUE}
#' or \code{standAlone = FALSE}), allowing a call to \code{pkgSnapshot} when a
#' more permanent record of versions can be made.
#'
#' @note This function will use \code{memoise} internally to determine the
#'   dependencies of all \code{packages}. This will speed up subsequent calls to
#'   \code{Require} dramatically. However, it will not take into account version
#'   numbers for this memoised step. #If package versions are updated manually by
#'   #the user, then this cached element should be wiped, using \code{forget =
#'   #TRUE}.
#'
#' @param packages Character vector of packages to install via
#'        \code{install.packages}, then load (i.e., with \code{library}). If it is
#'        one package, it can be unquoted (as in \code{require})
#' @param packageVersionFile If provided, then this will override all \code{install.package}
#'        calls with \code{versions::install.versions}
#' @param libPath The library path where all packages should be installed, and looked for to load
#'        (i.e., call \code{library})
#' @param repos The remote repository (e.g., a CRAN mirror), passed to either
#'              \code{install.packages}, \code{install_github} or \code{installVersions}.
#' @param install_githubArgs List of optional named arguments, passed to \code{install_github}.
#' @param install.packagesArgs List of optional named arguments, passed to \code{install.packages}.
#' @param standAlone Logical. If \code{TRUE}, all packages will be installed and loaded strictly
#'                   from the \code{libPaths} only. If \code{FALSE}, all \code{.libPaths} will
#'                   be used to find the correct versions. This can be create dramatically faster
#'                   installs if the user has a substantial number of the packages already in their
#'                   personal library. In the case of \code{TRUE}, there will be a hidden file
#'                   place in the \code{libPath} directory that lists all the packages
#'                   that were needed during the \code{Require} call. Default \code{FALSE} to
#'                   minimize package installing.
#' #param forget Internally, this function identifies package dependencies using a memoised
#' #               function for speed on reuse. But, it may be inaccurate in some cases,
#' #               if packages were installed manually by a user. Set this to \code{TRUE} to
#' #               refresh that dependency calculation.
#'
#' @export
#' @importFrom remotes install_github
#' @importFrom utils install.packages capture.output
#' @importFrom testthat capture_warnings capture_messages capture_error
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
#' }
#'
Require <- function(packages, packageVersionFile, libPath = .libPaths()[1], # nolint
                    install_githubArgs = list(),       # nolint
                    install.packagesArgs = list(), standAlone = FALSE,      # nolint
                    repos = getOption("repos")){#}, forget = FALSE) {

  browser(expr = exists("._Require_1"))
  ##################################################################
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
    #githubPkgNames <- extractPkgGitHub(githubPkgs)

    ##################################################################
    # Set up libPath
    origLibPaths <- setLibPaths(libPath, standAlone)
    on.exit({.libPaths(origLibPaths)}, add = TRUE)
    # Two parts: 1) if there is a packageVersionFile (this calls the external file installVersions)
    #            2) if there is no packageVersionFile
    if (!missing(packageVersionFile)) {
      Sys.setlocale(locale = "C") # required to deal with non English characters in Author names
      aa <- installVersions(githubPkgs, packageVersionFile = packageVersionFile,
                            libPath = libPath, standAlone = standAlone, repos = repos)
      Sys.setlocale(locale = "")
      allPkgsNeeded <- aa$instPkgs
    } else {
      aa <- .installPackages(packages, #githubPkgs = githubPkgs,
                             #githubPkgNames = githubPkgNames,
                             install_githubArgs = install_githubArgs,
                             #nonLibPathPkgs = nonLibPathPkgs,
                             libPath = libPath,
                             standAlone = standAlone)#, forget = forget)
      allPkgsNeeded <- aa$allPkgsNeeded
      libPathListFiles <- .libPathListFiles(standAlone, libPath)

      libPathListFiles <- libPathListFiles[basename(libPathListFiles) %in% allPkgsNeeded]
      currentVersionsDT <- as.data.table(installed.packages())
      currentVersionsDT <- currentVersionsDT[Package %in% allPkgsNeeded]
      currentVersionsDT <- currentVersionsDT[, .SD[which.min(match(libPath, .libPaths()))], by = "Package"]
      currentVersions <- currentVersionsDT$Version
      names(currentVersions) <- currentVersionsDT$Package
    #   currentVersions <- installedVersionsQuick(libPathListFiles, libPath, standAlone = standAlone,
    #                          basename(libPathListFiles))
    #   if (is.null(names(currentVersions)))
    #     if (identical(length(currentVersions), length(allPkgsNeeded))) {
    #       names(currentVersions) <- allPkgsNeeded
    #     }
    }

    collapsedLibPath <- gsub("\\/", replacement = "_", libPath)
    collapsedLibPath <- gsub(":_", "_", collapsedLibPath)
    pathToRequireFolder <- file.path(getOption("reproducible.cachePath"), ".Require",
                                     collapsedLibPath)
    autoFile <- paste0(pathToRequireFolder, "._packageVersionsAuto.txt")
    if (is.null(aa$haveVers)) {
      if (length(currentVersions)) {
        checkPath(dirname(autoFile), create = TRUE)
        # pkgsToSnapshot <- pickFirstVersion(names(currentVersions), unlist(currentVersions))
        .pkgSnapshot(names(currentVersions), currentVersions, packageVersionFile = autoFile)
      }
    } else {
      .pkgSnapshot(aa$instPkgs, aa$haveVers, packageVersionFile = autoFile)
      pkgSnapshot <- aa
    }

    # oldLibPath <- .libPaths()
    # if (standAlone) .libPaths(libPath) else .libPaths(c(libPath, .libPaths()))
    # on.exit({
    #   .libPaths(oldLibPath)
    # }, add = TRUE)

    # Actual package loading
    browser(expr = exists("._Require_3"))
    packages <- rev(names(pkgDepTopoSort(extractPkgName(packages), reverse = TRUE, returnFull = FALSE)))
    packages <- unique(packages)
    names(packages) <- packages
    unloadedPkgs <- logical()
    if (length(aa$notAvailableAnywhere)) {
      unloadedPkgs <- rep(FALSE, length(aa$notAvailableAnywhere))
      names(unloadedPkgs) <- aa$notAvailableAnywhere
      packages <- packages[-which(packages %in% aa$notAvailableAnywhere)]
      message("These packages not sufficient version or not installed; skipping loading: ",
              paste(aa$notAvailableAnywhere, collapse = ", "))
    }
    # if (NROW(aa$pkgDTMV)) {
    #   dt1 <- aa$pkgDTMV
    #   if ("correctVersion" %in% names(aa$pkgDTMV)) {
    #     dt1 <- aa$pkgDTMV[correctVersion == FALSE]
    #
    #   } else {
    #     dt1 <- aa$pkgDTMV
    #     dt1[is.na(currentInstalled), correctVersion := FALSE]
    #     dt1 <- dt1[correctVersion == FALSE]
    #   }
    #   if (NROW(dt1)) {
    #     packagesNoLoadableBCNotMinVers <- packages[!packages %in% dt1[availableOnCRAN == TRUE | availableOnGitHub == TRUE]$Package]
    #     packages <- packages[packages %in% dt1[availableOnCRAN == TRUE | availableOnGitHub == TRUE]$Package]
    #   }}
    #
    # if (length(packagesNoLoadableBCNotMinVers))
    #   message("These packages not sufficient version or not installed; skipping loading: ",
    #           paste(packagesNoLoadableBCNotMinVers, collapse = ", "))
    browser(expr = exists("._test111"))
    loadedNS <- unlist(lapply(packages, isNamespaceLoaded))
    baseRPackages <- dir(tail(.libPaths(),1))
    loadedNS1 <- setdiff(names(loadedNS), baseRPackages)
    loadedNS <- loadedNS[loadedNS1]
    if (sum(loadedNS)) {
      loaded <- names(loadedNS)[loadedNS]
      names(loaded) <- loaded
      versionsAboutToLoad <- unlist(lapply(loaded, function(l)
        DESCRIPTIONFileVersion(file.path(.libPaths()[1], l, "DESCRIPTION"))), recursive = FALSE)
      versionsLoaded <- unlist(lapply(loaded, function(l) as.character(packageVersion(l))), recursive = FALSE)

      compareV <- data.table(Package = names(versionsAboutToLoad), versionsAboutToLoad, versionsLoaded)
      compareV <- compareV[numeric_version(versionsAboutToLoad) != numeric_version(versionsLoaded)]
      compareV <- compareV[Package != "reproducible"]
      if (NROW(compareV)) {
        messageDF(compareV)
        stop(call. = FALSE,
             "There are package versions already loaded that conflict with package versions in ",
             ".libPaths(). Please detach and unload (e.g., restart R) and resolve this e.g., remove ",
             "old packages, change .libPaths() etc.")
      }

    }

    co <- capture.output(type = "message",
      err <- capture_error(
        warns <- capture_warnings(
          mess <- capture_messages(#type = "message", {
            # mess <- capture.output(type = "message", {
            packagesLoaded <- unlist(lapply(packages, function(p) {
              try(require(p, character.only = TRUE))
            }))
          )
        )
      )
    )

    message(paste(mess, collapse = "\n"))
    if (!is.null(packagesLoaded)) {
      if (any(!packagesLoaded)) {
        warns2 <- capture_warnings(
          packagesLoaded2 <- unlist(lapply(packages[!packagesLoaded], function(p) {
            try(require(p, character.only = TRUE, quietly = TRUE))
          }))
        )
        if (length(warns) > 0 || length(warns2) || length(co)) {
          warning(paste(unique(c(warns, warns2, co)), collapse = "\n"))
        }
        # } else {
        #   if (length(mess) > 0)
        #     message("Simultaneous package versions being used.",
        #             " Can only load first version(s) loaded in this session:\n",
        #             paste(packages[!packagesLoaded], collapse = ", "))
        # }

      }
    }
    packagesLoaded <- c(packagesLoaded, unloadedPkgs)
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


#' Determine versions all installed packages
#'
#' This code is adapted from \code{\link[versions]{installed.versions}}.
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
      ans <- lapply(seq_along(packages), function(x) unlist(installedVersions(packages[x], libPath[x])))
    } else {
      ans <- lapply(packages, function(x) unlist(installedVersions(x, libPath)))
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
    vers <- DESCRIPTIONFileVersion(desc_path)
    # lines <- readLines(desc_path);
    # Sys.setlocale(locale = "C") # required to deal with non English characters in Author names
    # on.exit(Sys.setlocale(locale = ""))
    # vers_line <- lines[grep("^Version: *", lines)] # nolint
    # vers <- gsub("Version: ", "", vers_line)
    vers <- list(vers)
    names(vers) <- packages
    return(vers)
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
#' @inheritParams Cache
#' @param depends Logical. Include packages listed in "Depends". Default \code{TRUE}.
#' @param imports Logical. Include packages listed in "Imports". Default \code{TRUE}.
#' @param suggests Logical. Include packages listed in "Suggests". Default \code{FALSE}.
#' @param linkingTo Logical. Include packages listed in "LinkingTo". Default \code{TRUE}.
#' @param recursive Logical. Should dependencies of dependencies be searched, recursively.
#'                  NOTE: Dependencies of suggests will not be recursive. Default \code{TRUE}.
#' @param keepVersionNumber Logical. If \code{TRUE}, then the package dependencies returned
#'   will include version number. Default is \code{FALSE}
#' @param refresh There is an internal type of caching. If the results are wrong, likely
#'   set \code{refresh = TRUE}.
#' @export
#' @importFrom memoise memoise
#' @rdname pkgDep
#'
#' @examples
#' pkgDep("crayon")
pkgDep <- function(packages, libPath, recursive = TRUE, depends = TRUE,
                   imports = TRUE, suggests = FALSE, linkingTo = TRUE,
                   topoSort = FALSE, repos = getOption("repos"), refresh = FALSE,
                   verbose = getOption("reproducible.verbose"),
                   keepVersionNumber = FALSE) {
  if (all(c(!depends, !imports, !suggests, !linkingTo))) {
    names(packages) <- packages
    needed <- lapply(packages, function(x) character())
    return(needed)
  }
  typeString <- paste("depends"[depends], "imports"[imports],
                      "suggests"[suggests], "linkingTo"[linkingTo], sep = "_")
  if (isTRUE(refresh)) {
   .pkgEnv$.depsAll[["recursive"]][[typeString]] <- NULL
   .pkgEnv$.depsAll[["nonRecursive"]][[typeString]] <- NULL
  }

  if (missing(libPath) || is.null(libPath)) {
    libPath <- .libPaths()#[1L]
  }

  if (length(libPath) > 1) {
    ans <- list()
    # Using loop next allows the ability to break out of search
    #  if initial .libPaths have the package
    for (lp in libPath) {
      # message("  Searching in ", lp)
      ans1 <- pkgDep(packages, lp, recursive = recursive,
                      depends = depends, imports = imports, suggests = suggests,
                      linkingTo = linkingTo,
                      refresh = FALSE)
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

    # package_dependencies and pkgDep will differ under the following circumstances
    # 1. github packages are not detected using tools::package_dependencies
    # 2. package_dependencies does not detect the dependencies of base packages,
    #    e.g,. methods depends on stats and graphics
    notInstalled <- unlist(lapply(ll2, function(y) length(y) == 0 & is.logical(y)))
    ll2[notInstalled] <- NA
    if (any(notInstalled)) {

      paste(names(ll2[notInstalled]), collapse = ", ")
      repos <- getCRANrepos(repos)

      if (!is.memoised(available.packagesMem)) {
        assignInMyNamespace("available.packagesMem", memoise(available.packages, ~timeout(360))) # nolint
      }

      parentFramePackages <- tryCatch(get("packages", envir = parent.frame()), error = function(x) NULL)

      if (!is.null(parentFramePackages))
        message(paste(parentFramePackages, collapse = ", "), " dependencies: ")
      message("  ", paste(names(ll2[notInstalled]), collapse = ", "),
              " not installed locally; check for dependencies on CRAN")
      availPackagesDb <- available.packagesMem(repos = repos)
      ll3 <- package_dependenciesMem(names(ll2[notInstalled]), db = availPackagesDb,
                                     recursive = recursive)
      if (recursive) {
        .pkgEnv$.depsAll[["recursive"]][[typeString]] <- append(.pkgEnv$.depsAll[["recursive"]][[typeString]], ll3)
      } else {
        .pkgEnv$.depsAll[["nonRecursive"]][[typeString]] <- append(.pkgEnv$.depsAll[["nonRcursive"]][[typeString]], ll3)
      }
      # the previous line will miss base packages
      ll3 <- lapply(ll3, function(x) {
        unique(c(x, unlist(pkgDep(x, libPath = unique(c(libPath, .libPaths())),
                                  recursive = recursive,
                                  depends = depends, imports = imports, suggests = FALSE, # don't propagate suggests
                                  linkingTo = linkingTo,
                                  refresh = FALSE))))
      })

      ll2[notInstalled] <- ll3
    }
    if (isTRUE(topoSort)) {
      browser(expr = exists("._pkgDep_1", envir = .GlobalEnv))
      needed <- names(ll2)
      names(needed) <- needed
      ll2 <- pkgDepTopoSort(needed, deps = ll2)
    }

    return(ll2)
  }

  if (length(packages) > 1) {
    if (length(packages) == length(libPath)) {
      ans <- lapply(seq_along(packages), function(x) pkgDep(packages[x], libPath[x],
                                                            recursive = recursive,
                                                            depends = depends, imports = imports, suggests = suggests,
                                                            linkingTo = linkingTo,
                                                            refresh = FALSE))
    } else {
      ans <- lapply(packages, pkgDep, libPath, recursive = recursive,
                    depends = depends, imports = imports, suggests = suggests,
                    linkingTo = linkingTo,
                    refresh = FALSE)
    }
    names(ans) <- packages
    return(ans)
  } else if (length(packages) == 0)  {
    return(character())
  }

  if (recursive) {
    if (isTRUE(packages %in% names(.pkgEnv$.depsAll[["recursive"]][[typeString]]))) {
      if (!is.null(.pkgEnv$.depsAll[["recursive"]][[typeString]][[packages]])) {
        return(.pkgEnv$.depsAll[["recursive"]][[typeString]][[packages]])
      }
    }
  } else {
    if (isTRUE(packages %in% names(.pkgEnv$.depsAll[["nonRecursive"]][[typeString]]))) {
      if (!is.null(.pkgEnv$.depsAll[["nonRecursive"]][[typeString]][[packages]]))
        return(.pkgEnv$.depsAll[["nonRecursive"]][[typeString]][[packages]])
    }
  }

  desc_path <- sprintf("%s/%s/DESCRIPTION", libPath, packages) # nolint
  if (!file.exists(desc_path)) {
    return(NA)
  } else {
    lines <- readLines(desc_path)
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
      if (!isTRUE(keepVersionNumber))
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
      if (verbose) message(packages)
      needed <- unique(needed)
      namesSP <- names(.pkgEnv$.depsAll[["nonRecursive"]][[typeString]])
      oldNeeded <- character()
      if (!is.null(namesSP)) {
        oldNeeded <- unlist(needed[needed %in% namesSP])
        needed <- needed[!needed %in% namesSP]
      }
      if (verbose) {
        if (length(needed) > 0)
          message("      Recursive: ", paste(needed, collapse = ","))
        if (length(oldNeeded) > 0)
          message("        Skipped: ", paste(oldNeeded, collapse = ","))
      }
      .packages <- list(character())
      names(.packages) <- packages
      names(needed) <- needed
      .needed <- lapply(needed, function(x) NULL)
      .pkgEnv$.depsAll[["nonRecursive"]][[typeString]] <- c(.pkgEnv$.depsAll[["nonRecursive"]][[typeString]], .needed)
      .pkgEnv$.depsAll[["nonRecursive"]][[typeString]][[packages]] <- unique(c(needed, oldNeeded))
      .pkgEnv$.depsAll[["recursive"]][[typeString]] <- c(.pkgEnv$.depsAll[["recursive"]][[typeString]], .needed)

      if (length(needed) > 0) {
        uniqueLibPaths <- unique(c(libPath, .libPaths()))
        needed2 <- pkgDep(needed, libPath = uniqueLibPaths, recursive = recursive,
                          depends = depends, imports = imports, suggests = FALSE,
                          linkingTo = linkingTo,
                          refresh = FALSE)
        needed <- na.omit(unique(c(needed, unlist(needed2)))) # collapses recursive on non-recursive
      }
      if (length(oldNeeded) > 0) { # just because we don't need to find its depenencies, doesn't mean it isn't needed
        needed <- unique(c(needed, oldNeeded, unlist(.pkgEnv$.depsAll[["recursive"]][[typeString]][oldNeeded])))
      }
      .pkgEnv$.depsAll[["recursive"]][[typeString]][[packages]] <- needed # recursive
      attr(needed, "na.action") <- NULL
      attr(needed, "class") <- NULL
      #}
    }
    return(needed)
  }
}

#' @description
#' \code{pkgDep2} is a convenience wrapper of \code{pkgDep} that "goes one level in",
#' i.e., the first order dependencies, and runs the \code{pkgDep} on those.
#' @rdname pkgDep
#' @export
#' @param sorted Logical. If \code{TRUE}, the default, the packages will be sorted in
#'   the returned list from most number of dependencies to least.
#' @examples
#' pkgDep2("reproducible")
pkgDep2 <- function(packages, recursive = TRUE, depends = TRUE,
                    imports = TRUE, suggests = FALSE, linkingTo = TRUE,
                    repos = getOption("repos"), refresh = FALSE,
                    verbose = getOption("reproducible.verbose"),
                    sorted = TRUE) {
  a <- lapply(pkgDep(packages, recursive = FALSE, depends = depends, imports = imports, suggests = suggests,
                     linkingTo = linkingTo)[[1]],
              recursive = recursive,
              pkgDep, depends = depends, imports = imports, suggests = suggests,
              linkingTo = linkingTo
  )
  a <- unlist(a, recursive = FALSE)
  if (sorted) {
    ord <- order(sapply(a, function(x) length(x)), decreasing = TRUE)
    a <- a[ord]
  }
  return(a)
}

#' Memoised version of \code{package_dependencies}
#'
#' This has a 6 minute memory time window.
#'
#' @inheritParams tools::package_dependencies
#'
#' @param db character matrix as from \code{\link[utils]{available.packages}}
#'
#' @importFrom memoise memoise timeout
#' @importFrom tools package_dependencies
package_dependenciesMem <- memoise::memoise(tools::package_dependencies, ~timeout(360)) # nolint

#' Memoised version of \code{available.packages}
#'
#' This has a 6 minute memory time window.
#' This will be replaced upon first calls to.
#'
#' @inheritParams utils::available.packages
#'
#' @importFrom utils available.packages
#' @keywords internal
available.packagesMem <- function(contriburl, method, fields, type, filters, repos) {
  stop("This function is for internal use only.")
  return(invisible(NULL))
}

#' Install exact package versions from a package version text file & GitHub
#'
#' @inheritParams Require
#' @param gitHubPackages Character vectors indicating \code{repository/packageName@branch}
#' @param packageVersionFile Path to the package version file, defaults to
#'        the \file{.packageVersions.txt}.
#'
#' This uses CRAN, CRAN archives, or MRAN (accessed via \code{versions::install.versions})
#' for remote repositories.
#' This will attempt to install all packages in the \code{packageVersionFile},
#' with their exact version described in that file. For GitHub packages, it will
#' use \code{\link[remotes]{install_github}}. This will be called internally by
#' \code{Require}, and so often doesn't need to be used by a user.
#'
#' Because of potential conflicts with loaded packages, this function will run
#' \code{install.packages} in a separate R process.
#'
#' @export
#' @importFrom data.table data.table rbindlist setDT setnames
#' @importFrom remotes install_github
#' @importFrom utils available.packages install.packages installed.packages read.table
#' @importFrom versions install.versions
#' @examples
#' \dontrun{
#' # requires the packageVersionFile -- this doesn't work -- safer to use Require
#' installVersions("PredictiveEcology/reproducible@development")
#'
#' # make a package version snapshot -- this will be empty because no packages in directory
#' tempPkgFolder <- file.path(tempdir(), "Packages")
#' dir.create(tempPkgFolder)
#' packageVersionFile <- file.path(tempPkgFolder, ".packageVersion.txt")
#' pkgSnapshot(libPath = tempPkgFolder, packageVersionFile)
#'
#' Require("crayon", libPath = tempPkgFolder) # install.packages first, then library
#'
#' # install a specific version
#' # make a package version snapshot
#' packageVersionFile <- file.path(tempPkgFolder, ".packageVersion.txt")
#' pkgSnapshot(libPath=tempPkgFolder, packageVersionFile, standAlone = FALSE)
#'
#' installVersions("crayon", packageVersionFile = packageVersionFile)
#' }
installVersions <- function(gitHubPackages, packageVersionFile = ".packageVersions.txt",
                            libPath = .libPaths()[1], standAlone = FALSE,
                            repos = getOption("repos")["CRAN"]) {

  if (file.exists(packageVersionFile)) {
    libPath <- normalizePath(libPath, winslash = "/") # the system call requires this
    message("Reading ", packageVersionFile)
    libPathListFiles <- .libPathListFiles(standAlone, libPath)
    # if (standAlone) {
    #   libPathListFiles <- dir(libPath, full.names = TRUE)
    # } else {
    #   libPathListFiles <- unlist(lapply(unique(c(libPath, .libPaths())), dir, full.names = TRUE))
    # }

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
    #nas <- unlist(lapply(instVers, is.na))
    instVers <- instVers[libPathListFilesBase]

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
      internetExists <- internetExists()

      packages <- whPkgsNeeded[, "instPkgs"]
      if (length(gitHubPackages)) {
        ghPackages <- extractPkgGitHub(gitHubPackages)
        # ghPackages <- sapply(strsplit(sapply(strsplit(gitHubPackages, split = "/"),
        #                                      function(x) x[2]), split = "@"), function(x) x[1])
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
                            pkg, "', lib='", libPath, "', dependencies = FALSE, repos = '", repos,
                            "'))\""), wait = TRUE)
            })
          } else {
            message("No connection to the internet. Can't install packages.")
          }

          AP <- installed.packages() # nolint
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
              urlex <- if (requireNamespace("RCurl", quietly = TRUE)) # cleaner with RCurl, but not required
                RCurl::url.exists(pkg)
              else {
                message(RCurlMess)
                TRUE
              }

              if (urlex) {
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

          AP <- installed.packages() # nolint
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
            type <- if (isWindows()) "win.binary" else "source"

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
          browser()
          oldLibPaths <- .libPaths()
          .libPaths(c(libPath, oldLibPaths))
          remotes::install_github(pkg, upgrade_dependencies = FALSE, local = FALSE, force = TRUE)
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
#' @param nonLibPathPkgs Character vector of all installed packages that are in \code{.libPaths},
#'                       but not in \code{libPath}. This would normally include a listing of
#'                       base packages, but may also include other library paths if
#'                       \code{standAlone} if \code{FALSE}
#' @importFrom data.table data.table setDT setnames
#' @importFrom memoise is.memoised memoise
#' @importFrom utils assignInMyNamespace available.packages install.packages installed.packages
#' @importFrom utils read.table compareVersion
#' @importFrom versions install.versions
#' @rdname installPackages
#'
#' @examples
#' \dontrun{
#'   .installPackages("crayon")
#' }
.installPackages <- function(packages, repos = getOption("repos"),
                             nonLibPathPkgs = character(0), install_githubArgs, # nolint
                             install.packagesArgs = list(), # nolint
                             libPath = .libPaths(), standAlone = standAlone) {

  browser(expr = exists("._installPackages_1"))

  pkgNames <- extractPkgName(packages)
  # Check if installed
  origLibPaths <- setLibPaths(libPath, standAlone)
  on.exit({.libPaths(origLibPaths)}, add = TRUE)

  installedPkgsCurrent <- installed.packages()
  installedPkgsCurrent <- as.data.table(installedPkgsCurrent[, c("Package", "LibPath", "Version"), drop = FALSE])

  messFromDeps <- capture_messages(
    depsAll <- pkgDep(unique(pkgNames), #lib = .libPaths()[-length(.libPaths())],
                      recursive = TRUE))
  deps <- unique(unlist(depsAll))
  baseRPackages <- dir(tail(.libPaths(),1)) # remove base packages
  deps <- setdiff(deps, baseRPackages)

  if (length(deps) == 0) deps <- NULL

  pkgDT <- data.table(Package = c(pkgNames, deps), packageFullName = c(packages, deps))
  pkgDT[, hasMinVersion := grepl(.grepVersionNumber, packageFullName)]
  pkgDT[, installed := Package %in% installedPkgsCurrent$Package]
  pkgDT[, `:=`(availableOnCRAN = NA, AvailableVersionCRAN = NA, versionOnGH = NA,
               currentInstalled = NA, correctVersionAvailGH = NA,
               correctVersionAvail = NA, minVersion = NA, upgrade = NA)]
  notAvailableAnywhere <- character()

  # # Check min version
  upgrades <- character()
  pkgDT <- installedPkgsCurrent[pkgDT, on = "Package"]

  colsToKeep <- c("Package", "Version", "hasMinVersion", "minVersion", "AvailableVersionCRAN",
                  "correctVersionAvail", "installed", "packageFullName", "versionOnGH", "correctVersionAvailGH",
                  "upgrade")
  newColNames <- c("Package", "currentInstalled", "hasMinVersion", "neededVersion", "currentOnCRAN",
                   "availableOnCRAN", "installed", "packageFullName", "currentOnGH", "availableOnGitHub", "upgrade")

  if (NROW(pkgDT)) {
    pkgDT[ , githubPkgName := extractPkgGitHub(packageFullName)]
    pkgDT[, isGH := !is.na(githubPkgName)]
    pkgDT[isGH == TRUE, fullGit := trimVersionNumber(packageFullName)]
    pkgDT[isGH == TRUE, Account := gsub("^(.*)/.*$", "\\1", fullGit)]
    pkgDT[isGH == TRUE, RepoWBranch := gsub("^(.*)/(.*)@*.*$", "\\2", fullGit)]
    pkgDT[isGH == TRUE, Repo := gsub("^(.*)@(.*)$", "\\1", RepoWBranch)]
    pkgDT[isGH == TRUE, Branch := "master"]
    pkgDT[isGH == TRUE & grepl("@", RepoWBranch), Branch := gsub("^.*@(.*)$", "\\1", RepoWBranch)]
    pkgDT[isGH == TRUE, Package := githubPkgName]
    set(pkgDT, NULL, c("RepoWBranch", "fullGit"), NULL)

    pkgDTNoMV <- pkgDT[hasMinVersion == FALSE]
    pkgDTNoMV[isGH == TRUE, correctVersionAvailGH := TRUE]

    pkgDTMV <- pkgDT[hasMinVersion == TRUE][, minVersion := gsub(grepExtractPkgs, "\\2", packageFullName)]
    pkgDTMV[hasMinVersion == TRUE, inequality := gsub(grepExtractPkgs, "\\1", packageFullName)]

    # any duplicates with different minimum version number to be dealt with here
    pkgDTMV <- pkgDTMV[pkgDTMV[, list(minVersion = as.character(max(package_version(minVersion)))),
                               by = "Package"],
                       on = c("Package", "minVersion")]


    if (NROW(pkgDTMV)) {
      # pkgDTMV[ , githubPkgName := extractPkgGitHub(packageFullName)]
      # pkgDTMV[, isGH := !is.na(githubPkgName)]
      # pkgDTMV[isGH == TRUE, fullGit := trimVersionNumber(packageFullName)]
      # pkgDTMV[isGH == TRUE, Account := gsub("^(.*)/.*$", "\\1", fullGit)]
      # pkgDTMV[isGH == TRUE, RepoWBranch := gsub("^(.*)/(.*)@*.*$", "\\2", fullGit)]
      # pkgDTMV[isGH == TRUE, Repo := gsub("^(.*)@(.*)$", "\\1", RepoWBranch)]
      # pkgDTMV[isGH == TRUE, Branch := "master"]
      # pkgDTMV[isGH == TRUE & grepl("@", RepoWBranch), Branch := gsub("^.*@(.*)$", "\\1", RepoWBranch)]
      # pkgDTMV[isGH == TRUE, Package := githubPkgName]
      # set(pkgDTMV, NULL, c("RepoWBranch", "fullGit"), NULL)
      pkgDTMV[, compareVersion := .compareVersionV(Version, minVersion)]
      pkgDTMV[, correctVersion := .evalV(.parseV(text = paste(compareVersion, inequality, "0")))]

      notCorrectVersions <- pkgDTMV[correctVersion == FALSE]
      areGH <- pkgDTMV[correctVersion == FALSE & isGH == TRUE]$Package

      if (NROW(notCorrectVersions)) {#} && sum(notCorrectVersions$installed) > 0) {
        if (!is.memoised(available.packagesMem)) {
          assignInMyNamespace("available.packagesMem", memoise(available.packages, ~timeout(360))) # nolint
        }
        apm <- available.packagesMem()
        apm <- as.data.table(apm[, c("Package", "Version")])
        setnames(apm, "Version", "AvailableVersionCRAN")
        notCorrectVersions <- apm[notCorrectVersions, on = "Package"]
        notCorrectVersions[, compareVersionAvail := .compareVersionV(AvailableVersionCRAN, minVersion)]
        notCorrectVersions[, correctVersionAvail := .evalV(.parseV(text = paste(compareVersionAvail, inequality, "0")))]


        if (any(notCorrectVersions$isGH)) {
          notCorrectVersions[isGH == TRUE,
                             url := file.path("https://raw.githubusercontent.com", Account,
                                              Repo, Branch, "DESCRIPTION", fsep = "/")
                             ]
          ua <- httr::user_agent(getOption("reproducible.useragent"))
          notCorrectVersions[isGH == TRUE, {
            versionOnGH := {
              destFile <- tempfile()
              suppressWarnings(
                httr::GET(url, ua,
                          httr::write_disk(destFile, overwrite = TRUE)) ## TODO: overwrite?
              )
              DESCRIPTIONFileVersion(destFile)
            }
          }, by = c("Package", "Branch")]

          notCorrectVersions[, compareVersionAvailGH := .compareVersionV(versionOnGH, minVersion)]
          notCorrectVersions[, correctVersionAvailGH :=
                               .evalV(.parseV(text = paste(compareVersionAvailGH, inequality, "0")))]
          #colsToKeep <- c(colsToKeep, "versionOnGH", "correctVersionAvailGH")
          #newColNames <- c(newColNames, "currentOnGH", "availableOnGitHub")

        }

        # If correct version is available, and it wasn't already installed, remove it from here and
        #   let the "normal" install mechanism take place - Don't ask about these
        notInstalledCorrectAvail <- notCorrectVersions[(installed == FALSE &
                                                          (correctVersionAvail == TRUE | correctVersionAvailGH == TRUE)),
                                                       ..colsToKeep]
        upgradeAvailable <- notCorrectVersions[(installed == TRUE &
                                                  (correctVersionAvail == TRUE | correctVersionAvailGH == TRUE)),
                                               ..colsToKeep]
        upgradeAvailable[, upgrade := FALSE]
        impossibleToInstall <- notCorrectVersions[!(installed == FALSE &
                                                      (correctVersionAvail == TRUE | correctVersionAvailGH == TRUE)),
                                                  ..colsToKeep]

        setnames(impossibleToInstall, old = colsToKeep, new = newColNames )
        setnames(notInstalledCorrectAvail, old = colsToKeep, new = newColNames )
        setnames(upgradeAvailable, old = colsToKeep, new = newColNames )
        notAvailableAnywhere <- impossibleToInstall$Package
        if (length(notAvailableAnywhere)) {
          message(magenta("These are not upgradable to required versions: ", paste(notAvailableAnywhere, collapse = ", ")))
          message(magenta(" -->  Please manually install these packages or check minimum version requirements"))
          messageDF(impossibleToInstall, colour = "magenta")
        }
        if (NROW(upgradeAvailable)) {

          availableOnGH <- upgradeAvailable[availableOnGitHub == TRUE]$Package
          availableOnCRAN <- upgradeAvailable[availableOnCRAN == TRUE]$Package
          upgradeAvailable[availableOnGitHub == TRUE, installFrom := "GitHub"]
          upgradeAvailable[availableOnCRAN == TRUE, installFrom := "CRAN"]
          if (NROW(upgradeAvailable)) {
            message(crayon::green("These are upgradable: "))
            messageDF(upgradeAvailable, colour = "green")
          }

          if (length(availableOnGH))
            capture_messages(message("Packages available on GitHub: ", paste(availableOnGH, collapse = ", ")))
          if (length(availableOnCRAN))
            capture_messages(message("Packages available on CRAN: ", paste(availableOnCRAN, collapse = ", ")))

          upgradeAvailable <- upgradeAvailable[!is.na(installFrom)]
          if (NROW(upgradeAvailable)) {

            vals <- c("All", "CRAN packages only", "None", upgradeAvailable[installFrom == "CRAN"]$Package,
                      upgradeAvailable[installFrom == "GitHub"]$Package)

            df2 <- data.frame( row.names = NULL, stringsAsFactors = FALSE, "Upgrade" = vals)
            row.names(df2) <- paste0(seq(NROW(df2)), ":")
            messageDF(df2)
            out <- if (isInteractive() && is.null(getOption("reproducible.Require.upgrade"))) {
              as.numeric(.readline("Pick a number to upgrade: "))
            } else {
              getOption("reproducible.Require.upgrade", 3)
            }
            if (out > NROW(df2)) stop("Please choose one of the options")
            choice <- df2[out,]
            if (identical(choice, "None") || is.na(out)) {
              message("Not installing/upgrading ",paste(upgradeAvailable$Package, collapse = ", "),
                      "; this may cause undesired effects")
            } else {
              upgrades <- df2[-(1:3),]
              upgradesGit <- upgradeAvailable[installFrom == "GitHub"]$Package
              if (identical(out, 2)) {
                upgrades <- upgrades[!upgrades %in% upgradesGit]
              } else if (out > 3) {
                upgrades <- upgrades[as.numeric(out) - 3]
              }
            }
            missingPkg <- upgradeAvailable[is.na(currentInstalled)]
            if (NROW(missingPkg)) {
              packages <- c(packages, missingPkg$Package)
            }

            upgrades <- trimVersionNumber(upgrades)
            upgradeAvailable[, upgrade := Package %in% upgrades]
            upgradeAvailable <- upgradeAvailable[upgrade == TRUE]
          }
        }
        pkgDTMV <- rbindlist(list(notInstalledCorrectAvail, upgradeAvailable), fill = TRUE, use.names = TRUE)
      }
    }
    pkgDT <- rbindlist(list(pkgDTMV, pkgDTNoMV), fill = TRUE, use.names = TRUE)
    pkgDT <- pkgDT[!duplicated(pkgDT$Package)] # now that version numbering is dealt with
  } else {
    pkgDTMV <- Copy(pkgDT) # if empty pkgDT --> need this in pkgDTMV also
  }
  pkgDT <- pkgDT[, ..colsToKeep]
  setnames(pkgDT, old = colsToKeep, new = newColNames )

  allPkgsNeeded <- pkgDT$Package
  # allPkgsNeeded <- na.omit(unique(c(deps, packages, upgrades)))

  browser(expr = exists("._installPackages_4"))
  baseRPackages <- dir(tail(.libPaths(),1))
  dontNeedRowInd1 <- if ("correctVersion" %in% names(pkgDT)) {
    pkgDT[, which(installed == TRUE & hasMinVersion == TRUE & correctVersion == TRUE)]
  } else {
    integer()
  }
  dontNeedRowInd2 <- pkgDT[, which(installed == TRUE & hasMinVersion == FALSE)]
  dontNeedRowInd <- unique(c(dontNeedRowInd1, dontNeedRowInd2))
  needInstall <- if (length(dontNeedRowInd)) {
    pkgDT[-dontNeedRowInd]$Package # needInstall[!(needInstall %in% nonLibPathPkgs)]
  } else {
    pkgDT$Package
  }
  needInstall <- needInstall[!needInstall %in% baseRPackages]
  names(needInstall) <- needInstall

  loadedNS <- unlist(lapply(needInstall, isNamespaceLoaded))
  if (sum(loadedNS)) {
    loaded <- names(loadedNS)[loadedNS]
    cantLoad <- unlist(Map(dep = depsAll, nam = names(depsAll),
        function(dep, nam) any(loaded %in% c(nam, dep))))
    browser(expr = exists("._installPackages_5"))
    pkgToInstallManually <- pkgDT[installed == FALSE | upgrade == TRUE][Package %in% loaded]
    message("Package(s) already loaded into RAM: (",paste(loaded, collapse = ", "),"). This may cause installation to fail ",
            "and will cause loading of ", paste(names(cantLoad)[cantLoad], collapse = ", "), " to fail.")
    cran <- pkgToInstallManually[availableOnCRAN == TRUE | is.na(availableOnCRAN)]$Package
    gh <- trimVersionNumber(pkgToInstallManually[availableOnGitHub == TRUE]$packageFullName)
    mess <- character()
    if (length(gh))
      mess <- paste0("devtools::install_github(c('",paste(gh, collapse = "', '"),"'))")
    if (length(cran))
      mess <- c(mess, paste0("install.packages(c('",paste(cran, collapse = "', '"),"'))"))

    stop("  Skipping install because these packages are loaded: ",paste(cantLoad, collapse = ", "),
         ";\n Please install these manually in a clean session with e.g.:\n ",
         paste(mess, collapse = "\n"), call. = FALSE)
  }
  if (length(needInstall) && getOption("reproducible.Require.install", TRUE)) {
    internetExists <- internetExists()
    #if (missing(githubPkgs)) {
      githubPkgs <- character()
      githubPkgNames <- githubPkgs
      if (any(names(pkgDT) %in% "availableOnGitHub")) {
        githubPkgNames <- pkgDT[availableOnGitHub == TRUE]$Package
        githubPkgs <- unlist(lapply(githubPkgNames, function(ghpn)
          trimVersionNumber(grep(ghpn, pkgDT$packageFullName, value = TRUE))))
      }
    #}
    if (length(githubPkgs)) {
      #oldLibPaths <- .libPaths()
      #.libPaths(unique(c(libPath, oldLibPaths)))

      # use xforce = TRUE because we have already eliminated
      args <- append(install_githubArgs, list(dependencies = NA,
                                              # upgrade_dependencies = TRUE,
                                              force = TRUE))#, local = FALSE))
      if (isTRUE(standAlone)) {
        oldLibPaths2 <- .libPaths()
        .libPaths(libPath)
      } else {
        oldLibPaths2 <- .libPaths()
        .libPaths(unique(libPath, .libPaths()))
      }
      on.exit(.libPaths(oldLibPaths2), add = TRUE)
      if (internetExists) {
        sapply(githubPkgs, function(pk) {
          args <- append(args, list(pk))
          # the cases where we have the correct version; install_github uses a
          # local database hidden somewhere that won't let the same package be installed
          # twice, even if in different libPaths
          args <- args[!duplicated(names(args))]

          warn <- capture_warnings(do.call(install_github, args))
          if (any(grepl("cannot remove prior installation", warn))) {
            stop(paste(warn, collapse = "\n"), "\n --> Try to remove lock then rerun Require")
          }

          browser(expr = exists("._installPackages_3"))

          # with_libpaths doesn't work because it will look for ALL packages there;
          # can't download without curl
        })
      } else {
        message("No connection to the internet. Can't install packages.")
      }
      # .libPaths(oldLibPaths)

      # This sends it back in with all the install_github calls which may or may not
      #   include dependencies correctly -- especially with standAlone = TRUE
      #   Basically, install_github needs packages like curl, which will likely be
      #   in the user's personal library, so, we can't omit personal library from
      #   .libPath(), but that means that dependencies may not be installed in
      #   libPath if they exist in the personal library. Sending it back into function
      #   will work
      ip <- as.data.table(installed.packages(noCache = TRUE))[[1]]
      packagesAgain <- setdiff(unique(c(needInstall[!(needInstall %in% githubPkgNames)], githubPkgNames)),ip)
      Require(packagesAgain,
              libPath = libPath, #forget = TRUE,
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
        dop <- tools::dependsOnPkgs(needInstall, recursive = TRUE)
        needUnload <- dop[unlist(lapply(dop, function(p) isNamespaceLoaded(p)))]
        needDetach <- dop[dop %in% gsub("package:", "", search())]
        if (isTRUE(any(grepl("SpaDES|^reproducible|^devtools$", unique(c(needUnload, needDetach))))))
          message("Because '",paste(needInstall, collapse = "', '"), "' or a package that uses it (",
                  paste(unique(c(needUnload, needDetach)), collapse = ", "),") is/are currently loaded, ",
               "it may be necessary to restart R and install it/them",
               "' manually in a clean session")
        if (FALSE) { # an attempt to unload things first -- can't unload reproducible or else rest of function fails
          moreToUnload <- TRUE
          while (moreToUnload) {
            needUnload <- dop[unlist(lapply(dop, function(p) isNamespaceLoaded(p)))]
            needDetach <- dop[dop %in% gsub("package:", "", search())]
            if (length(needDetach)) {
              message("Will attemp to detach and unload ", paste0(needDetach, collapse = ", "))
              out1 <- lapply(rev(needDetach), function(p)
                try(detach(paste0("package:", p), character.only = TRUE, unload = TRUE), silent = TRUE))
            }
            if (length(c(needUnload, needDetach))) {
              message("Will attemp to unload ", paste0(needUnload, collapse = ", "))
              out2 <- lapply(rev(needUnload), function(p) try(unloadNamespace(p), silent = TRUE))
            }
            out1 <- unlist(lapply(out1, function(x) is(x, "try-error")))
            out2 <- unlist(lapply(out2, function(x) is(x, "try-error")))
            moreToUnload <- any(c(out1, out2))
          }
        }
        needInstall2 <- needInstall[!(needInstall %in% githubPkgNames)]
        names(needInstall2) <- needInstall2
        aa <- lapply(needInstall2, function(pkg) {
          syscall <- paste0("--quiet --vanilla -e \"utils::install.packages('", pkg,
                            "',dependencies=FALSE,lib='", .libPaths()[1], "',repos=c('",
                            paste(repos, collapse = "','"), "'))\"")
          system(paste(rpath, syscall), wait = TRUE)
        })
        if (any(unlist(aa) != 0)) {
          ip <- as.data.table(installed.packages(noCache = TRUE))[[1]]
          needInstall2 <- setdiff(needInstall, ip)
          aa <- install.packages(needInstall2)
        }
      } else {
        message("No connection to the internet. Can't install packages.")
      }

    }
  } else {
    if (length(needInstall))
      message("Skipping installation of missing packages because options('reproducible.Require.install' = FALSE)")
  }

  return(invisible(list(instPkgs = needInstall, allPkgsNeeded = allPkgsNeeded,
                        pkgDTMV = pkgDTMV, notAvailableAnywhere = notAvailableAnywhere)))
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
  browser(expr = exists("aaaa"))
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

  collapsedLibPath <- gsub("\\/", replacement = "_", libPath[1])
  pathToRequireFolder <- file.path(getOption("reproducible.cachePath"), ".Require")
  autoFile <- file.path(pathToRequireFolder, paste0(collapsedLibPath))
  autoFile <- gsub(":_", "_", autoFile) # on Windows, rm the c: in the middle
  if (!isAbsolute) packageVersionFile <- paste0(autoFile, "_", basename(packageVersionFile))

  autoFile <- paste0(autoFile, "._packageVersionsAuto.txt")
  if (!standAlone & file.exists(autoFile)) {
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
    instPkgs <- instPkgs[!instPkgs %in% basename2(c(CacheStorageDir("."),
                                                    CacheDBFile(".", drv = NULL, conn = NULL)))]
    instVers <- unlist(lapply(libPath, function(lib)
                    na.omit(unlist(unname(installedVersions(instPkgs, libPath = lib))))))
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
  allPkgsDESC <- allPkgsDESC[order(rownames(allPkgsDESC)),]
  collapsedLibPath <- gsub("\\/", replacement = "_", libPath)
  collapsedLibPath <- gsub(":_", "_", collapsedLibPath) # on Windows, rm the c: in the middle

  pathToRequireFolder <- file.path(getOption("reproducible.cachePath"), ".Require")
  if (!dir.exists(pathToRequireFolder)) {
    checkPath(pathToRequireFolder, create = TRUE)
  }
  # .snap <- file.path(libPath, ".snapshot.RDS")
  .snapFilePath <- file.path(pathToRequireFolder, paste0(collapsedLibPath, ".snapshot.RDS"))

  needSnapshot <- FALSE
  needInstalledVersions <- FALSE
  #fromInstalledVersionsObj <- getFromNamespace(".installedVersions", "reproducible")
  installedVersionsFile <- file.path(getOption("reproducible.cachePath"),
                                     ".Require",
                                     paste0(collapsedLibPath, ".installedVersions.RDS"))

  #installedVersionsFile <- file.path(libPath, ".installedVersions.RDS")
  if (!file.exists(installedVersionsFile)) {
  # if (NROW(fromInstalledVersionsObj) == 0) {
    needSnapshot <- TRUE
    needInstalledVersions <- TRUE
  }

  fromDotSnap <- if (file.exists(.snapFilePath)) {
    readRDS(.snapFilePath)
  } else {
    data.frame()
  }
  inDotSnap <- fromDotSnap[dirname(rownames(fromDotSnap)) %in% libPathListFiles,]
  inDotSnap <- inDotSnap[order(rownames(inDotSnap)),]

  if (!(identical(inDotSnap, allPkgsDESC))) {
    needSnapshot <- TRUE
    needInstalledVersions <- TRUE
  }
  #} else {
  #  needSnapshot <- TRUE
  #  needInstalledVersions <- TRUE
  #}
  if (needSnapshot) {
    allPkgsDESC <- unique(rbind(fromDotSnap, allPkgsDESC))
    allPkgsDESC <- allPkgsDESC[order(rownames(allPkgsDESC)),]
    #assignInMyNamespace(x = ".snap", allPkgsDESC)
    saveRDS(allPkgsDESC, file = .snapFilePath)
  }

  if (needInstalledVersions) {
    if (standAlone) {
      instVers <- installedVersions(libPathListFilesBase, libPath)
      names(instVers) <- libPathListFilesBase
    } else {
      instVers <- installedVersions(libPathListFilesBase, dirname(libPathListFiles))
    }
    #assignInMyNamespace(".installedVersions", instVers)
    saveRDS(instVers, file = installedVersionsFile)
  } else {
    #instVers <- getFromNamespace(".installedVersions", "reproducible")
    instVers <- readRDS(file = installedVersionsFile)
  }

  return(instVers)
}

.snap <- data.frame()
.installedVersions <- list()

.libPathListFiles <- function(standAlone, libPath = .libPaths()) {
  origLibPaths <- setLibPaths(libPath, standAlone)
  on.exit({.libPaths(origLibPaths)}, add = TRUE)
  unlist(lapply(.libPaths(), dir, full.names = TRUE))
}


internetExists <- function() {
  if (requireNamespace("RCurl", quietly = TRUE)) {
    out <- RCurl::url.exists("www.google.com")
  } else {
    message(RCurlMess)
    if (.Platform$OS.type == "windows") {
      ipmessage <- system("ipconfig", intern = TRUE)
    } else {
      ipmessage <- system("ifconfig", intern = TRUE)
    }
    validIP <- "((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)[.]){3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)"
    linesWithIP <- grep(validIP, ipmessage)
    out <- if (length(linesWithIP)) {
      any(linesWithIP) && all(!grepl("127.0.0.1", ipmessage[linesWithIP]))
    } else {
      FALSE
    }
    out
  }
  return(out)
}

RCurlMess <- paste0("install.packages('RCurl') may give a more reliable detection ",
"of internet connection")

extractPkgGitHub <- function(pkgs) {
  unlist(lapply(strsplit(trimVersionNumber(pkgs), split = "/|@"), function(x) x[2]))
  #sapply(strsplit(sapply(strsplit(pkgs, split = "/"),
  #                       function(x) x[2]), split = "@"), function(x) x[1])
}

extractPkgName <- function(pkgs) {
  pkgNames <- trimVersionNumber(pkgs)
  withGitName <- extractPkgGitHub(pkgNames)
  isNAWithGitName <- is.na(withGitName)
  if (any(!isNAWithGitName)) {
    stripped <- unlist(lapply(strsplit(pkgNames[!isNAWithGitName], split = "/|@"), function(x) x[2]))
    pkgNames[!isNAWithGitName] <- stripped
  }
  pkgNames
}

.grepVersionNumber <- " *\\(.*"
.grepGitHubInfo <- "/|@"

#' @export
#' @rdname installPackages
trimVersionNumber <- function(packages) {
  gsub(.grepVersionNumber, "", packages)
}

trimGitHubInfo <- function(packages) {
  gsub(.grepGitHubInfo, "", packages)
}

.readline <- function(prompt) {
  readline(prompt)
}

grepExtractPkgs <- ".*\\((>*=*) *(.*)\\)"

.compareVersionV <- Vectorize(compareVersion)
.evalV <- Vectorize(eval, vectorize.args = "expr")
.parseV <- Vectorize(parse, vectorize.args = "text")

DESCRIPTIONFileVersion <- function(file) {
  lines <- readLines(file);
  Sys.setlocale(locale = "C") # required to deal with non English characters in Author names
  on.exit(Sys.setlocale(locale = ""))
  vers_line <- lines[grep("^Version: *", lines)] # nolint
  gsub("Version: ", "", vers_line)
}


#' Reverse package depends
#'
#' This is a wrapper around \code{tools::dependsOnPkgs},
#' but with the added option of \code{sorted}, which
#' will sort them such that the packages at the top will have
#' the least number of dependencies that are in \code{pkgs}.
#' This is essentially a topological sort, but it is done
#' heuristically. This can be used to e.g., \code{detach} or
#' \code{unloadNamespace} packages in order so that they each
#' of their dependencies are detached or unloaded first.
#' @param pkgs A vector of package names to evaluate their
#'   reverse depends (i.e., the packages that \emph{use} each
#'   of these packages)
#' @param deps An optional named list of (reverse) dependencies.
#'   If not supplied, then \code{tools::dependsOnPkgs(..., recursive = TRUE)}
#'   will be used
#' @param topoSort Logical. If \code{TRUE}, the default, then
#'   the returned list of packages will be in order with the
#'   least number of dependencies listed in \code{pkgs} at
#'   the top of the list.
#' @param useAllInSearch Logical. If \code{TRUE}, then all non-core
#' R packages in \code{search()} will be appended to \code{pkgs}
#' to allow those to also be identified
#' @param returnFull Logical. If \code{TRUE}, then the full reverse
#'   dependencies will be returned; if \code{FALSE}, the default,
#'   only the reverse dependencies that are found within the \code{pkgs}
#'   (and \code{search()} if \code{useAllInSearch = TRUE}) will be returned.
#'
#' @export
#' @rdname pkgDep
#' @return
#' A possibly ordered, named (with packages as names) list where list elements
#' are either full reverse depends.
#'
pkgDepTopoSort <- function(pkgs, deps, reverse = FALSE, topoSort = TRUE, useAllInSearch = FALSE,
                      returnFull = TRUE) {

  if (isTRUE(useAllInSearch)) {
    if (missing(deps)) {
      a <- search()
      a <- setdiff(a, .defaultPackages)
      a <- gsub("package:", "", a)
      pkgs <- unique(c(pkgs, a))
    } else {
      message("deps is provided; useAllInSearch will be set to FALSE")
    }
  }

  names(pkgs) <- pkgs
  if (missing(deps)) {
    aa <- if (isTRUE(reverse)) {
      lapply(pkgs, tools::dependsOnPkgs, recursive = TRUE)
    } else {
      pkgDep(pkgs, recursive = TRUE)
    }

  }
  else
    aa <- deps
  bb <- list()
  cc <- lapply(pkgs, function(x) character())

  if (isTRUE(topoSort)) {
    notInOrder <- TRUE
    isCorrectOrder <- logical(length(aa))
    i <- 1
    newOrd <- numeric(0)
    for (i in seq_along(aa)) {
      dif <- setdiff(seq_along(aa), newOrd)
      for (j in dif) {
        overlapFull <- aa[[j]] %in% names(aa)[-i]
        overlap <- aa[[j]] %in% names(aa)[dif]
        overlapPkgs <- aa[[j]][overlapFull]
        isCorrectOrder <- !any(overlap)
        if (isCorrectOrder) {
          # bb[names(aa)[j]] <- list(overlapPkgs)
          cc[j] <- list(overlapPkgs)
          newOrd <- c(newOrd, j)
          i <- i + 1
          break
        }
      }
    }
    aa <- aa[newOrd]
    cc <- cc[newOrd]
  }
  out <- if (isTRUE(returnFull)) {
    aa
  } else {
    cc
  }
  return(out)
}

.defaultPackages <- c(".GlobalEnv", "tools:rstudio", "package:stats", "package:graphics",
                      "package:grDevices", "package:utils", "package:datasets", "package:methods",
                      "Autoloads", "package:base", "devtools_shims")

setLibPaths <- function(libPath = .libPaths(), standAlone = FALSE) {
  out <- lapply(libPath, checkPath, create = TRUE)
  libPath <- normalizePath(libPath, winslash = "/") # the system call requires this

  origLibPaths <- .libPaths()
  if (standAlone) .libPaths(c(libPath, tail(.libPaths(), 1))) else .libPaths(c(libPath, libPath, .libPaths()))
  return(invisible(origLibPaths))
}