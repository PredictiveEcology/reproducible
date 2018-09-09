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
#' @author Jean Marchal & Alex Chubaty
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

#' @keywords internal
.formalsNotInCurrentDots <- function(fun, ...) {
  names(list(...))[!(names(list(...)) %in% names(formals(fun)))]
}

rndstr <- function(n = 1, len = 8) {
  unlist(lapply(character(n), function(x) {
    x <- paste0(sample(c(0:9, letters, LETTERS), size = len,
                       replace = TRUE), collapse = "")
  }))
}


isInteractive <- function() interactive()
