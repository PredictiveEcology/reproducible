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
#' will be saved in \code{libPath} that will store the \emph{information}
#' about the packages and their dependencies, even if they were already installed in
#' \code{.libPaths()}. This hidden file will be used if a user runs \code{pkgSnapshot}, enabling
#' a new user to rebuild the entire dependency chain, without having to install all packages
#' in an isolated directory (as does packrat). This will save potentially a lot of time and
#' disk space, and yet maintain reproducibility. \emph{NOTE}: since there is only one hidden file in a
#' \code{libPath}, any call to \code{pkgSnapshot} will make a snapshot of the most recent call
#' to \code{Require}.
#'
#' This function works best if all required packages are called within one \code{Require}
#' call, as all dependencies can identified together, and all package versions will be saved
#' automatically (with \code{standAlone = TRUE} or \code{standAlone = FALSE}),
#' allowing a call to \code{pkgSnapshot} when a more permanent record of versions can be made.
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
#' @param standAlone Logical. If \code{TRUE}, all packages will be installed and loaded strictly
#'                   from the \code{libPaths} only. If \code{FALSE}, all \code{.libPaths} will
#'                   be used to find the correct versions. This can be create dramatically faster
#'                   installs if the user has a substantial number of the packages already in their
#'                   personal library. In the case of \code{TRUE}, there will be a hidden file
#'                   place in the \code{libPath} directory that lists all the packages that were needed
#'                   during the Require call.
#'
#'
#' @examples
#' \dontrun{
#'
#' # simple usage, like conditional install.packages then library
#' Require("stats") # analogous to require(stats), but slower because it checks for
#'                  #   pkg dependencies, and installs them, if missing
#' tempPkgFolder <- file.path(tempdir(), "Packages")
#' Require("crayon", libPath = tempPkgFolder, standAlone = TRUE) # install.packages first, then library
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
Require <- function(packages, packageVersionFile, libPath = .libPaths()[1],
                    notOlderThan = NULL, install_githubArgs = list(),
                    install.packagesArgs = list(), standAlone = FALSE) {

  githubPkgs <- grep("\\/", packages, value = TRUE)
  githubPkgNames <- sapply(strsplit(githubPkgs, split = "/|@" ), function(x) x[2])
  if(length(githubPkgs)) {
    packages[packages %in% githubPkgs] <- githubPkgNames
  }
  if(!dir.exists(libPath)) dir.create(libPath)
  libPath <- normalizePath(libPath, winslash = "/") # the system call requires this

  if(standAlone) { # include only base if standAlone
    nonLibPathPaths <- .libPaths()[length(.libPaths())]
    nonLibPathPkgs <- unlist(lapply(nonLibPathPaths, dir))
  } else {
    nonLibPathPaths <- setdiff(.libPaths(), libPath)
    nonLibPathPkgs <- unique(unlist(lapply(nonLibPathPaths, dir)))
  }


  # Two parts -- one if there is a packageVersionFile -- This calls the external file installVersions
  #           -- two if there is no packageVersionFile
  if(!missing(packageVersionFile)) {
    Sys.setlocale(locale = "C") # required to deal with non English characters in Author names
    aa <- installVersions(githubPkgs, packageVersionFile = packageVersionFile,
                          libPath = libPath, standAlone = standAlone)
    Sys.setlocale(locale = "")
    allPkgsNeeded <- aa$instPkgs
  } else {
    cacheRepo <- file.path(libPath, ".cache")
    aa <- .installPackages(packages, githubPkgs=githubPkgs, githubPkgNames = githubPkgNames,
                           install_githubArgs = install_githubArgs, nonLibPathPkgs = nonLibPathPkgs,
                           libPath = libPath, standAlone = standAlone, cacheRepo = cacheRepo,
                           notOlderThan = notOlderThan)
    allPkgsNeeded <- aa$allPkgsNeeded
  }
  currentVersions <- na.omit(unlist(lapply(unique(c(libPath, nonLibPathPaths)),
                                           function(pk)
                                             installedVersions(allPkgsNeeded, libPath = pk))))
  if(length(currentVersions)==1) names(currentVersions) <- allPkgsNeeded
  autoFile <- file.path(libPath, "._packageVersionsAuto.txt")
  if(NROW(currentVersions)) {
    pkgsToSnapshot <- data.table(instPkgs = names(currentVersions), instVers = currentVersions)

    # if(file.exists(autoFile)) {
    #   aaa <- data.table::fread(autoFile, header = TRUE)
    #   pkgsToSnapshot <- unique(data.table::rbindlist(list(pkgsToSnapshot, aaa)), by = "instPkgs")
    # }
    .pkgSnapshot(pkgsToSnapshot$instPkgs, pkgsToSnapshot$instVers, packageVersionFile = autoFile)
  }

  oldLibPath <- .libPaths()
  if(standAlone) .libPaths(libPath) else .libPaths(c(libPath, .libPaths()))
  packagesLoaded <- unlist(lapply(packages, function(p) {
    try(require(p, character.only = TRUE))
    }))
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
#' @param packages Character vector of packages to determine which version is installed in the \code{libPath}.
#' @inheritParams installVersions
#' @examples
#' installedVersions("reproducible", .libPaths()[1])
installedVersions <- function (packages, libPath) {
  if (missing(libPath) || is.null(libPath)) {
    libPath <- .libPaths()[1L]
    if (length(.libPaths()) > 1L)
      message(sprintf(ngettext(length(packages), "Checking package in %s\n(as %s is unspecified)",
                               "Checking packages in %s\n(as %s is unspecified)"),
                      sQuote(libPath), sQuote("libPath")), domain = NA)
  }
  if (length(packages) > 1) {
    if(length(packages)==length(libPath)) {
      ans <- lapply(seq_along(packages), function(x) installedVersions(packages[x], libPath[x]))
    } else {
      ans <- lapply(packages, installedVersions, libPath)
    }
    names(ans) <- packages
    return(ans)
  } else if (length(packages)==0)  {
    return(character())
  }
  desc_path <- sprintf("%s/%s/DESCRIPTION", libPath, packages)
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

#' Determine package dependencies, first looking at local filesystem
#'
#' This is intended to replace \code{\link[tools]{package_dependencies}} or
#' \code{\link[miniCRAN]{pkgDep}}, but with modfications for speed. It will first check
#' local package directory(ies) in \code{libPath}, and it if the function cannont find
#' the packages there, then it will use \code{\link[tools]{package_dependencies}}.
#'
#' @inheritParams tools::package_dependencies
#' @inheritParams Require
#' @param depends Logical. Include packages listed in "Depends". Default TRUE.
#' @param imports Logical. Include packages listed in "Imports". Default TRUE.
#' @param suggests Logical. Include packages listed in "Suggests". Default FALSE.
#' @param recursive Logical. Should dependencies of dependencies be searched, recursively.
#'                  NOTE Dependencies of suggests will not be recursive. Default TRUE.
#' @rdname pkgDep
#' @examples
#' pkgDep("crayon")
pkgDepRaw <- function (packages, libPath, recursive = TRUE, depends = TRUE, imports = TRUE, suggests = FALSE) {
  if (missing(libPath) || is.null(libPath)) {
    libPath <- .libPaths()#[1L]
    # if (length(.libPaths()) > 1L) {
    #   message(sprintf(ngettext(length(packages), "Checking package in %s\n(as %s is unspecified)",
    #                            "Checking packages in %s\n(as %s is unspecified)"),
    #                   sQuote(libPath), sQuote("libPath")), domain = NA)
    # }
  }

  if (length(libPath) > 1) {
    ans <- list()
    for(lp in libPath) {
       ans1 <- pkgDep(packages, lp)
       ans <- append(ans, list(ans1))
       if(all(unlist(lapply(ans, function(x) all(unlist(lapply(x, is.character))))))) {
         break
       }
    }
    # This next line is the 1 liner to replace above... but above allows the ability to break out of search
    #  if initial .libPaths have the package
    #ans <- lapply(libPath, function(lp) pkgDep(packages, lp))

    if(length(packages)==1) {
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

    notInstalled <- unlist(lapply(ll2, function(y) length(y)==0 & is.logical(y)))
    ll2[notInstalled] <- NA
    if(any(notInstalled)) {
      message(paste(names(ll2[notInstalled]), collapse = ", "),
              " not installed locally, checking on CRAN for dependencies.")
      ll3 <- tools::package_dependencies(names(ll2[notInstalled]), recursive = TRUE)
      ll2[notInstalled] <- ll3
    }
    return(ll2)
  }

  if (length(packages) > 1) {
    if(length(packages)==length(libPath)) {
      ans <- lapply(seq_along(packages), function(x) pkgDep(packages[x], libPath[x]))
    } else {
      ans <- lapply(packages, pkgDep, libPath)
    }
    names(ans) <- packages
    return(ans)
  } else if (length(packages)==0)  {
    return(character())
  }


  desc_path <- sprintf("%s/%s/DESCRIPTION", libPath, packages)
  if (!file.exists(desc_path)) {
    return(NA)
  }
  else {
    lines <- readLinesRcpp(desc_path)
    lines <- strsplit(lines, split = "\n")[[1]];
    deps_line <- grep("^Depends: *", lines)
    sugg_line <- grep("^Suggests: *", lines)
    imports_line <- grep("^Imports: *", lines)
    colon_line <- grep(": *", lines)

    needed <- character()
    if(imports){
      if(length(imports_line)) {
      imports_lines <- imports_line:(colon_line[which(colon_line %in% imports_line) + 1]-1)
      imprts <- paste(lines[imports_lines], collapse = "")
      imprts <- gsub("Imports: ", "", imprts)
      imprts <- strsplit(imprts, split = ", *")
      needed <- c(needed, imprts[[1]])
    }}

    if(depends){
      if(length(deps_line)) {
      deps_lines <- deps_line:(colon_line[which(colon_line %in% deps_line) + 1]-1)
      deps <- paste(lines[deps_lines], collapse = "")
      deps <- gsub("Depends: ", "", deps)
      deps <- strsplit(deps, split = ", *")
      needed <- c(needed, deps[[1]])
      }}

    if(suggests){
      if(length(sugg_line)) {
      sugg_lines <- sugg_line:(colon_line[which(colon_line %in% sugg_line) + 1]-1)
      sugg <- paste(lines[sugg_lines], collapse = "")
      sugg <- gsub("Suggests: ", "", sugg)
      sugg <- strsplit(sugg, split = ", *")
      needed <- c(needed, sugg[[1]])
      }}

    needed <- grep("^R ", needed, value = TRUE, invert = TRUE)
    if(length(needed)) {
      #hasVersionNumber <- regmatches(needed, gregexpr(pattern = "(?<=\\().*?(?=\\))", needed, perl = TRUE))[[1]]
      hasVersionNumber <- unlist(lapply(needed, function(x) regmatches(x, gregexpr(pattern = "(?<=\\().*?(?=\\))", x, perl = TRUE))[[1]]))
      if(length(hasVersionNumber)) {
        for(pat in hasVersionNumber) {
          needed <- sub(pattern = pat, needed, replacement = "")
        }
        needed <- gsub(needed, pattern = " *\\(\\)", replacement = "")
      }
    }

    if(recursive) {
      # note that recursive searching must search in all libPaths, not just current one
      needed2 <- pkgDep(needed, libPath = unique(c(libPath, .libPaths())),
                                  depends = depends, imports = imports, suggests = FALSE) # like miniCRAN::pkgDep not recursive on Suggests
      needed <- na.omit(unique(c(needed, unlist(needed2))))
      attr(needed, "na.action") <- NULL
      attr(needed, "class") <- NULL
    }
    return(needed)
  }
}

#' @rdname pkgDep
#' @export
#' @import memoise memoise
pkgDep <- memoise::memoise(pkgDepRaw)

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
#' @importFrom utils read.table available.packages installed.packages install.packages
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
                            libPath = .libPaths()[1], standAlone = FALSE) {
  if(file.exists(packageVersionFile)) {
    libPath <- normalizePath(libPath, winslash = "/") # the system call requires this
    message("Reading ", packageVersionFile)
    if(standAlone) {
      libPathListFiles <- dir(libPath, full.names = TRUE)
    } else {
      libPathListFiles <- unlist(lapply(unique(c(libPath, .libPaths())), dir, full.names = TRUE))
    }
    libPathListFilesBase <- basename(libPathListFiles)
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
      if(standAlone) {
        instVers <- installedVersions(libPathListFilesBase, libPath)
      } else {
        instVers <- installedVersions(libPathListFilesBase, dirname(libPathListFiles))
      }
      saveRDS(instVers, file = installedVersionsFile)
    } else {
      instVers <- readRDS(file = installedVersionsFile)
    }

    instVers <- lapply(instVers, sub, pattern="\\r|\\n", replacement = "")
    if(length(instVers)!=length(libPathListFilesBase)) stop("Package folder, ", libPath,
                                                        " has become corrupt. Please manually delete .snapshot.RDS and .installedVersions.RDS")
    inst <- data.frame(havePkgs=libPathListFilesBase, haveVers=unlist(instVers), stringsAsFactors = FALSE)
    supposedToBe <- read.table(packageVersionFile, header = TRUE, stringsAsFactors = FALSE)
    together <- merge(supposedToBe, inst, by.x="instPkgs",by.y="havePkgs")
    needPkgs <- setdiff(supposedToBe$instPkgs,inst$havePkgs)
    eq <- compareNA(together$instVers, together$haveVers)
    needVersEqual <- which(!eq)

    # Here test that the installed version is greater than required one
    isLoaded <- unlist(lapply(together$instPkgs[needVersEqual], isNamespaceLoaded))
    if(!is.null(isLoaded)) {
      #canInstall <- together[needVersEqual[!eq],]
      canInstall <- together[needVersEqual[!eq & !isLoaded],]
    } else {
      canInstall <- together[0,]
    }

    gte <- together$instVers[needVersEqual] >= together$haveVers[needVersEqual]
    gte <- is.na(gte) | gte

    needVers <- needVersEqual[gte]
    wh1 <- supposedToBe[which(supposedToBe$instPkgs %in% needPkgs),]
    wh2 <- together[needVers,]
    if(NROW(canInstall)) {
      wh2 <- merge(canInstall, wh2, all.x = TRUE)
    }
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
        #repos <- getOption("repos")
        if ( is.null(repos) | any(repos == "" | "@CRAN@" %in% repos) ) {
          options("repos" = "https://cran.rstudio.com")
        }

        avail <- available.packages()

        wh <- avail[,"Package"] %in% whPkgsNeededFromCran[,"instPkgs"]
        whPkgsAvailFromCran <- data.frame(avail[wh,c("Package", "Version"), drop = FALSE], stringsAsFactors = FALSE)

        colnames(whPkgsAvailFromCran) <- c("instPkgs", "instVers")

        setDT(whPkgsAvailFromCran)
        setDT(whPkgsNeededFromCran)


        tryCRANarchive <- whPkgsNeededFromCran[!whPkgsAvailFromCran, on = c("instPkgs", "instVers")]
        canInstDirectFromCRAN <- whPkgsAvailFromCran[whPkgsNeededFromCran, nomatch=0, on = c("instPkgs", "instVers")]

        if(NROW(canInstDirectFromCRAN)) {
          #repos <- getOption("repos")
          if ( is.null(repos) | any(repos == "" | "@CRAN@" %in% repos) ) {
            repos <- "https://cran.rstudio.com"
          }

          if(length(repos)>1) repos <- repos[["CRAN"]]

          rpath <- file.path(R.home(), "bin", "R")

          rtests <- Sys.getenv("R_TESTS")
          isEmptyRtests <- nchar(rtests)==0
          if(!isEmptyRtests) {
            Sys.setenv(R_TESTS="")
            on.exit(Sys.setenv(R_TESTS=rtests), add = TRUE)
          }
          lapply(canInstDirectFromCRAN$instPkgs, function(pkg){
              system(paste0(file.path(R.home(), "bin", "R"), " --quiet --vanilla -e \"do.call(install.packages,list('",pkg,
                            "',lib='",libPath,"',dependencies=FALSE,repos='",repos,"'))\""), wait=TRUE)

          })

          AP <- installed.packages(lib.loc = libPath)
          actuallyInstalled <- data.table(AP[(AP[,"Package"] %in% canInstDirectFromCRAN$instPkgs),
                                             c("Package", "Version"),drop=FALSE])
          setnames(actuallyInstalled, old = c("Package", "Version"), new= c("instPkgs", "instVers"))

          setDT(actuallyInstalled)
          failed <- rbind(failed, canInstDirectFromCRAN[!actuallyInstalled, on = c("instPkgs", "instVers")])
        }

        if(NROW(tryCRANarchive)>0 & all(!is.na(tryCRANarchive$instPkgs))) {
          #repos <- getOption("repos")
          if ( is.null(repos) | any(repos == "" | "@CRAN@" %in% repos) ) {
            repos <- "https://cran.rstudio.com"
          }
          if(length(repos)>1) repos <- repos[(names(repos) %in% "CRAN")]

          rtests <- Sys.getenv("R_TESTS")
          isEmptyRtests <- nchar(rtests)==0
          if(!isEmptyRtests) {
            Sys.setenv(R_TESTS="")
            on.exit(Sys.setenv(R_TESTS=rtests), add = TRUE)
          }

          packageURLs <- file.path(repos[length(repos)],"src/contrib/Archive",
                                   tryCRANarchive$instPkgs,
                                   paste0(tryCRANarchive$instPkgs,"_",
                                          tryCRANarchive$instVers,".tar.gz"))
          lapply(packageURLs, function(pkg){
            system(paste0(file.path(R.home(), "bin", "R"), " --quiet --vanilla -e \"install.packages('",pkg,
                          "',lib='",libPath,"',dependencies=FALSE,repos=NULL,type='source')\""), wait=TRUE)

          })

          AP <- installed.packages(lib.loc = libPath)
          actuallyInstalled <- data.table(AP[(AP[,"Package"] %in% tryCRANarchive$instPkgs),c("Package", "Version"),drop=FALSE])
          setnames(actuallyInstalled, old = c("Package", "Version"), new = c("instPkgs", "instVers"))
          failed <- rbind(failed, tryCRANarchive[!actuallyInstalled, on = c("instPkgs", "instVers")])
        }

        if(nrow(failed)) {
          message("Trying MRAN install of ",paste(failed$instPkgs, collapse=", "))
          type <- if(.Platform$OS.type=="windows") "win.binary" else "source"

          multiSource <- paste0(file.path(R.home(), "bin", "R"), " --quiet --vanilla -e \"versions::install.versions('",
                                failed$instPkgs,"','",failed$instVers,
                                "',lib='",libPath,"',dependencies=FALSE,type='",type,"')\"")
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
      if(length(needVersEqual)) {
        tog <- together[needVersEqual,]
        message(paste(tog$instPkgs, collapse = ", ")," version incorrect, but wrong version loaded. Cannot install.")
        message("Keeping installed version")
        colnames(tog) <- c("Package", "Requested Version", "Installed Version")
        rownames(tog) <- NULL
        print(tog)
        message("If this version is ok, you can update ", packageVersionFile," using pkgSnapshot()")

      } else {
        message("All packages are correct versions")
        if(!is.null(isLoaded)) {
          message("However, because a package was already loaded in the namespace, the desired version",
                  "may not be correctly loaded")
        }

      }
      message("")

    }
  } else {
    message("There is no package version file named ", packageVersionFile, ".\n",
            "No package versions installed.")
  }
}

#' Internal function to install packages
#'
#' @inheritParams Require
#' @param repos The remote repository (e.g., a CRAN mirror), passed to \code{install.packages}
#' @param githubPkgs Character vector of github repositoriess and packages, given in the form of
#'                   repository/package@branch, with branch being optional.
#' @param githubPkgNames Character vector of the package names, i.e., just the R package name.
#' @param nonLibPathPkgs Character vector of all installed packages that are in .libPaths, but
#'                       not in \code{libPath}. This would normally include a listing of
#'                       base packages, but may also include other library paths if \code{standAlone}
#'                       if \code{FALSE}
#' @param cacheRepo A path to a cache repository to create or use. Used solely internally while
#'                  running \code{pkgDep}.
#' @importFrom versions install.versions
#' @importFrom data.table setDT data.table setnames
#' @importFrom utils read.table available.packages installed.packages install.packages
#' @rdname installPackages
#' @examples
#' \dontrun{
#'   .installPackages("crayon")
#' }
.installPackages <- function(packages, repos = getOption("repos"),
                             githubPkgs = character(0), githubPkgNames,
                             nonLibPathPkgs = character(0), install_githubArgs,
                             install.packagesArgs = list(),
                             libPath = .libPaths()[1], standAlone = standAlone,
                             cacheRepo = cacheRepo, notOlderThan = notOlderThan) {
  memoise::forget(pkgDep)
  deps <- unlist(Cache(pkgDep, packages, unique(c(libPath, .libPaths())), recursive = TRUE,
                 cacheRepo = cacheRepo, notOlderThan = notOlderThan))
  # browser()
  # if(any(is.na(deps)) & all(!(packages %in% basename(githubPkgs)))) {# means we don't have a local copy yet
  #   browser()
  #   # These package_dependencies and installedPkdDeps will differ under the following circumstances
  #   # 1. github packages are not detected using tools::package_dependencies
  #   # 2. package_dependencies does not detect the dependencies of base packages, e.g,. methods depends on stats and graphics
  #   deps <- unlist(Cache(tools::package_dependencies, names(deps[is.na(deps)]), recursive = TRUE,
  #                         cacheRepo = cacheRepo, notOlderThan = notOlderThan))
  # }

  if(length(deps)==0) deps <- NULL
  allPkgsNeeded <- na.omit(unique(c(deps, packages)))
  names(deps) <- deps
  if(missing(githubPkgNames)) githubPkgNames <- sapply(strsplit(githubPkgs, split = "/|@" ), function(x) x[2])

  libPathPkgs <- unlist(lapply(libPath, dir))
  needInstall <- allPkgsNeeded[!(allPkgsNeeded %in% unique(unlist(libPathPkgs)))]
  needInstall <- needInstall[!(needInstall %in% nonLibPathPkgs)]
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
      Require(unlist(gitPkgs), libPath = libPath, notOlderThan = Sys.time(),
              install_githubArgs = install_githubArgs, standAlone = standAlone,
              install.packagesArgs = install.packagesArgs) # This sends it back in with all the install_github calls completed
      return(NULL)
    }
    # RSTudio won't install packages if they are loaded. THe algorithm is faulty and it won't let things install even when they are not loaded.
    repos <- getOption("repos")
    if ( is.null(repos) | any(repos == "" | "@CRAN@" %in% repos) ) {
      repos <- "https://cran.rstudio.com"
    }

    rpath <- file.path(R.home(), "bin", "R")
    rtests <- Sys.getenv("R_TESTS")
    isEmptyRtests <- nchar(rtests)==0
    if(!isEmptyRtests) {
      Sys.setenv(R_TESTS="")
      on.exit(Sys.setenv(R_TESTS=rtests), add = TRUE)
    }

    aa <- lapply(needInstall, function(pkg){
      syscall <- paste0("--quiet --vanilla -e \"utils::install.packages('",pkg,
                        "',dependencies=FALSE,lib='",libPath,"',repos=c('",paste(repos, collapse = "','"),"'))\"")
      system(paste(rpath, syscall), wait = TRUE)
    })
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
pkgSnapshot <- function(packageVersionFile, libPath, standAlone = TRUE) {
  if(missing(libPath)) {
    if(standAlone) libPath <- .libPaths()[1] else libpath <- .libPaths()
  } else {
    if(!standAlone) libPath <- unique(c(libPath, .libPaths()))
  }
  if(missing(packageVersionFile)) packageVersionFile <- ".packageVersions.txt"

  autoFile <- file.path(libPath[1], "._packageVersionsAuto.txt")
  if(!standAlone & file.exists(autoFile)) {
    file.copy(autoFile, packageVersionFile, overwrite = TRUE)
  } else {
    instPkgs <- dir(libPath)
    instVers <- installedVersions(instPkgs, libPath = libPath)
    .pkgSnapshot(instPkgs, instVers, packageVersionFile)
  }
}


.pkgSnapshot <- function(instPkgs, instVers, packageVersionFile = "._packageVersionsAuto.txt") {
  inst <- data.frame(instPkgs, instVers=unlist(instVers), stringsAsFactors = FALSE)
  write.table(inst, file = packageVersionFile, row.names = FALSE)
}



invertList <- function(ls) { # @Josh O'Brien
  # get sub-elements in same order
  x <- lapply(ls, `[`, names(ls[[1]]))
  # stack and reslice
  ll <- apply(do.call(rbind, x), 2, as.list)
  lapply(ll, function(xx) unique(unlist(xx)))
}
