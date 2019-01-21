#' The \code{reproducible} package
#'
#' Built on top of \pkg{git2r} and \pkg{archivist}, this package aims at making
#' high-level, robust, machine and OS independent tools for making deeply
#' reproducible and reusable content in R. This extends beyond the package
#' management utilities of \pkg{packrat} and \pkg{checkpoint} by including
#' tools for caching, and accessing GitHub repositories.
#'
#' @section Package options:
#'
#' \code{reproducible} has the following \code{\link{options}} to configure behaviour.
#' See \code{\link{reproducibleOptions}}
#'
#' @import methods
#' @importFrom Rcpp evalCpp
#' @useDynLib reproducible
"_PACKAGE"
