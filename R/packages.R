utils::globalVariables(c(
  ".SD", "..colsToKeep", "Account", "availableOnCRAN", "availableOnGitHub", "AvailableVersion",
  "Branch", "compareVersionAvail", "compareVersionAvailGH", "correctVersion", "correctVersionAvail",
  "correctVersionAvailGH", "fullGit", "githubPkgName", "inequality", "instPkgs", "isGH",
  "minVersion", "Package", "Repo", "RepoWBranch", "Version", "versionOnGH"
))


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


.readline <- function(prompt) {
  readline(prompt)
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
#' @param reverse Logical. If \code{TRUE}, then this will use \code{tools::pkgDependsOn}
#'   to determine which packages depend on the \code{pkgs}
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
#' @importFrom Require pkgDep
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
