#' Check global git config file
#'
#' Defunct.
#'
#' @author Alex Chubaty
#' @keywords internal
#' @name .checkGitConfig
#' @rdname checkGitConfig
#'
.checkGitConfig <- function() {
  .Defunct(msg = ".checkGitConfig is not sufficiently tested or developed to be useful")
}

#' Clone, fetch, and checkout from GitHub.com repositories
#'
#' Defunct.
#'
#' @param repo Repository address in the format \code{username/repo[/subdir][@ref|#pull]}.
#'   Alternatively, you can specify subdir and/or ref using the respective parameters (see below);
#'   if both is specified, the values in repo take precedence.
#'
#' @param localRepoPath Character string. The path into which the git repo should be
#'                      cloned, fetched, and checked out from.
#'
#' @param cred Character string. Either the name of the environment variable
#'             that contains the GitHub PAT or filename of the GitHub private key file.
#'
#' @param ...  Additional arguments passed to \code{git2r} functions.
#'
#' @return Invisibly returns a git_repository class object, defined in \pkg{git2r}.
#'
#' @author Eliot McIntire and Alex Chubaty
#' @export
#' @importFrom utils getFromNamespace
#' @rdname checkoutVersion
#'
#' @examples
#' \dontrun{
#'   tmpDir <- tempfile("")
#'   dir.create(tmpDir)
#'   repo <- "PredictiveEcology/reproducible"
#'
#'   ## get latest from master branch
#'   localRepo <- checkoutVersion("PredictiveEcology/reproducible",
#'                                localRepoPath = tmpDir)
#'   git2r::summary(localRepo)
#'   unlink(tmpDir, recursive = TRUE)
#'
#'   ## get latest from development branch
#'   localRepo <- checkoutVersion(paste0(repo, "@", "development"), localRepoPath = tmpDir)
#'   git2r::summary(localRepo)
#'   unlink(tmpDir, recursive = TRUE)
#'
#'   ## get a particular commit by sha
#'   sha <- "8179e1910e7c617fdeacad0f9d81323e6aad57c3"
#'   localRepo <- checkoutVersion(paste0(repo, "@", sha), localRepoPath = tmpDir)
#'   git2r::summary(localRepo)
#'   unlink(tmpDir, recursive = TRUE)
#'
#'   rm(localRepo, repo)
#' }
#'
checkoutVersion <- function(repo, localRepoPath = ".", cred = "", ...) {
  .Defunct(msg = "checkoutVersions is not sufficiently tested or developed to be useful")
}
