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
  strsplit(readLinesRcppInternal(path), split = "\n")[[1]]
}

.pkgSnapshot <- function(instPkgs, instVers, packageVersionFile = "._packageVersionsAuto.txt") {
  inst <- data.frame(instPkgs, instVers = unlist(instVers), stringsAsFactors = FALSE)
  write.table(inst, file = packageVersionFile, row.names = FALSE)
}

getCRANrepos <- function(repos = NULL) {
  if (is.null(repos) | any(repos == "" | "@CRAN@" %in% repos)) {
    repos <- "https://cran.rstudio.com"
  }
  #if (length(repos) > 1) repos <- repos[(names(repos) %in% "CRAN")]
  return(repos)
}
