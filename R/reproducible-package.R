#' The \code{reproducible} package
#'
#' Built on top of \code{git2r} and \code{archivist}, this package aims at making
#' high-level, robust, machine and OS independent tools for making deeply
#' reproducible and reusable content in R. This extends beyond the package
#' management utilities of \code{packrat} and \code{checkpoint} by including
#' tools for caching, and accessing GitHub repositories.
#'
#' @section Package options:
#'
#' \code{reproducible} has the following \code{\link{options}} to configure behaviour:
#'
#' \itemize{
#'   \item \code{reproducible.quick}: Default is \code{FALSE}. This means that
#'   all hashing will be run on full objects but only file.size for any file paths.
#'   If \code{TRUE}, then full objects and file content will be hashed. Because
#'   the hash will be different between quick = TRUE and quick = FALSE, hashing
#'   will effectively be independent between the two states.
#'
#'   \item \code{reproducible:verbose}: Default is \code{FALSE}. This is the
#'   normal setting. If set to \code{TRUE} then every Cache call will show a summary
#'   of the objects being cached, their object.size and the time it took to digest
#'   them and also the time it took to run the call and save the call to the
#'   cache repository or load the cached copy from the repository.
#'
#'   \item \code{reproducible.cachePath}: The default path for repositories if not
#'   passed as an argument. The default is the tempdir() of the session.
#'
#' }
#'
#' @importFrom Rcpp evalCpp
#' @useDynLib reproducible
"_PACKAGE"
