#' @title
#' Alternative to readLines that is faster
#' @description
#' This alternative is from \url{https://gist.github.com/hadley/6353939}
#'
#' @param path Path to text file to read.
#' @return
#' Similar to \code{readLines}. It may not return identical results.
#'
#' @export
#' @examples
#' readLinesRcpp(system.file(package = "reproducible", "DESCRIPTION"))
#' @rdname readLinesRcpp
readLinesRcpp <- function(path) {
  Sys.setlocale(locale = "C") # required to deal with non English characters in Author names
  on.exit(Sys.setlocale(locale = ""))
  strsplit(readLinesRcppInternal(path), split = "[{\r}]*\n")[[1]]
}

.pkgSnapshot <- function(instPkgs, instVers, packageVersionFile = "._packageVersionsAuto.txt") {
  inst <- data.frame(instPkgs, instVers = unlist(instVers), stringsAsFactors = FALSE)
  write.table(inst, file = packageVersionFile, row.names = FALSE)
  inst
}

#' @importFrom utils chooseCRANmirror
getCRANrepos <- function(repos = NULL) {
  if (is.null(repos)) {
    repos <- getOption("repos")["CRAN"]
  }

  # still might be imprecise repository, specifically ""
  if (isTRUE("" == repos)) {
    repos <- "@CRAN@"
  }

  # if @CRAN@, and non interactive session
  if (isTRUE("@CRAN@" %in% repos)) {
    cranRepo <- Sys.getenv("CRAN_REPO")
    repos <- if (nzchar(cranRepo)) {
      cranRepo
    } else {
      if (isInteractive()) {
        chooseCRANmirror() ## sets repo option
        getOption("repos")["CRAN"]
      } else {
        "https://cloud.R-project.org"
      }
    }
  }

  return(repos)
}

#' Add a prefix or suffix to the basename part of a file path
#'
#' Prepend (or postpend) a filename with a prefix (or suffix).
#' If the directory name of the file cannot be ascertained from its path,
#' it is assumed to be in the current working directory.
#'
#' @param f       A character string giving the name/path of a file.
#' @param prefix  A character string to prepend to the filename.
#' @param suffix  A character string to postpend to the filename.
#'
#' @author Jean Marchal and Alex Chubaty
#' @export
#' @importFrom tools file_ext file_path_sans_ext
#' @rdname prefix
#'
#' @examples
#' # file's full path is specified (i.e., dirname is known)
#' myFile <- file.path("~/data", "file.tif")
#' .prefix(myFile, "small_")    ## "/home/username/data/small_file.tif"
#' .suffix(myFile, "_cropped") ## "/home/username/data/myFile_cropped.shp"
#'
#' # file's full path is not specified
#' .prefix("myFile.shp", "small")    ## "./small_myFile.shp"
#' .suffix("myFile.shp", "_cropped") ## "./myFile_cropped.shp"
#'
.prefix <- function(f, prefix = "") {
  file.path(dirname(f), paste0(prefix, basename(f)))
}

#' @export
#' @name suffix
#' @rdname prefix
.suffix <- function(f, suffix = "") {
  file.path(dirname(f), paste0(tools::file_path_sans_ext(basename(f)), suffix,
                               ".", tools::file_ext(f)))
}

#' Identify which formals to a function are not in the current ...
#'
#' This is for advanced use.
#' @keywords internal
#' @export
#' @param fun A function
#' @param ... The ... from inside a function. Will be ignored if \code{dots} is
#'        provided explicitly.
#' @param dots Optional. If this is provided via say dots = list(...),
#'             then this will cause the \code{...} to be ignored.
.formalsNotInCurrentDots <- function(fun, ..., dots) {
  if (!missing(dots)) {
    out <- names(dots)[!(names(dots) %in% names(formals(fun)))]
  } else {
    out <- names(list(...))[!(names(list(...)) %in% names(formals(fun)))]
  }
  out
}

rndstr <- function(n = 1, len = 8) {
  unlist(lapply(character(n), function(x) {
    x <- paste0(sample(c(0:9, letters, LETTERS), size = len,
                       replace = TRUE), collapse = "")
  }))
}

isInteractive <- function() interactive()

#' A version of \code{base::basename} that is \code{NULL} resistant
#'
#' Returns \code{NULL} if x is \code{NULL}, otherwise, as \code{basename}.
#'
#' @param x A character vector of paths
#' @export
#' @return Same as \code{\link[base]{basename}}
#'
basename2 <- function (x) {
  if (is.null(x)) {
    NULL
  }
  else {
    basename(x)
  }
}

#' A wrapper around \code{try} that retries on failure
#'
#' This is useful for functions that are "flaky", such as
#' \code{curl}, which may fail for unknown reasons that do not
#' persist.
#' @param ... Passed to \code{try}
#' @param sleep Passed to \code{Sys.sleep}, the default
#'   delay in seconds bewteen repeated tries
#' @param retries Numeric. The maximum number of retries.
#' @export
#' @details
#' The same as \code{try}
#'
retry <- function(..., sleep = 0.5, retries = 5) {
  failed <- 1
  while(failed > 0  && failed < retries) {
    downloadResults <- try(...)
    if (is(downloadResults, "try-error")) {
      failed <- failed + 1
      print("retrying")
      if (failed >= retries)
        stop("Failed uploading to GoogleDrive")
      Sys.sleep(sleep)
    } else {
      failed <- 0
    }
  }
  downloadResults

}
