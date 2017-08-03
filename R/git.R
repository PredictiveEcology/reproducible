#' Check global git config file
#'
#' In general, interacting with remote repositories using \pkg{git2r} won't work
#' if a users's \file{.gitconfig} file rewrites HTTPS urls to SSH.
#'
#' @author Alex Chubaty
#' @importFrom git2r config
#' @keywords internal
#' @name .checkGitConfig
#' @rdname checkGitConfig
#'
.checkGitConfig <- function() {
  gitConfig <- unlist(git2r::config(global = TRUE))
  usingSSH <- any(grepl("url.ssh://git@github.com/.insteadof", names(gitConfig)))
  if (usingSSH) {
    stop("A .gitconfig file is rewriting HTTPS urls to SSH,",
         " which breaks some functionality because 'git2r' can't handle SSH remotes.\n",
         "Please [temporarily] disable this option.")
  }
  return(invisible(TRUE))
}

#' Clone, fetch, and checkout from GitHub.com repositories
#'
#' In reproducible research, not only do packages and R version have to be
#' consistent, but also specific versions of version controlled scripts.
#' This function allows a simple way to create an exactly copy locally of a git
#' repository. It can use ssh keys (including GitHub deploy keys) or GitHub
#' Personal Access Tokens.
#'
#' @inheritParams devtools::install_github
#'
#' @param localRepoPath Character string. The path into which the git repo should be
#'                      cloned, fetched, and checked out from.
#'
#' @param cred Character string. Either the name of the environment variable
#'             that contains the GitHub PAT or filename of the GitHub private key file.
#'
#' @param ...  Additional arguments passed to \code{git2r} functions.
#'
#' @return Invisibly returns a repository class object, defined in
#'         \code{\link[git2r]{git_repository-class}}
#'
#' @author Eliot McIntire and Alex Chubaty
#' @export
#' @import devtools
#' @importFrom git2r checkout clone commit cred_token cred_ssh_key head init lookup
#'                   remote_add remote_set_url remote_url repository status
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
  .checkGitConfig()

  localRepoPath <- normalizePath(path.expand(localRepoPath), mustWork = FALSE)

  .parse_git_repo <- utils::getFromNamespace("parse_git_repo", "devtools") # nolint
  params <- .parse_git_repo(repo)
  gitHash <- if (is.null(params$ref)) "master" else params$ref

  repoName <- params$repo
  repoAcct <- params$username

  ghPrivateKeyFile <- if (file.exists(cred)) cred else NULL

  cred <- if (is.null(ghPrivateKeyFile)) {
     cred_token(cred)
   } else {
     cred_ssh_key(publickey = paste0(ghPrivateKeyFile, ".pub"), privatekey = ghPrivateKeyFile)
   }

  httpsURL <- paste0("https://github.com/", repoAcct, "/", repoName, ".git")
  sshURL <- paste0("ssh://git@github.com:", repoAcct, "/", repoName, ".git")
  needSSH <- class(cred) == "cred_ssh_key"
  urls <- c(httpsURL, sshURL)
  url1 <- ifelse(needSSH, sshURL, httpsURL)

  pathExists <- dir.exists(localRepoPath)
  pathIsRepo <- if (pathExists) {
    is(tryCatch(repository(localRepoPath), error = function(e) FALSE),
       "git_repository")
  } else {
    FALSE
  }

  if (!pathIsRepo) {
    repo1 <- git2r::clone(url1, path.expand(localRepoPath), credentials = cred, ...)
  } else if (pathExists && pathIsRepo) {
    repo0 <- repository(localRepoPath, discover = TRUE)
    if (!(remote_url(repo0) %in% urls)) {
      stop("Local repository exists and remote URLs do not match:\n",
           "  requested repo: ", url1, "\n",
           "  localRepoPath: ", remote_url(repo0))
    }
    repo1 <- repo0
  } else {
    repo1 <- git2r::init(localRepoPath)
    remote_add(repo1, name = repo, url = url1)

    ## if repo's remote url uses SSH, 'git2r' wonn't work -- must change it temporarily
    isSSH <- !any(grepl(httpsURL, remote_url(repo1)))
    if (xor(isSSH, needSSH)) {
      remote_set_url(repo, "origin", url = url1)
    }
  }

  tryCatch(git2r::checkout(lookup(repo1, gitHash)), error = function(x) {
    checkout(repo1, gitHash, ...)
  })

  ## switch back to using SSH for remote url if it was previously set
  isSSH <- !any(grepl(httpsURL, remote_url(repo1)))
  if (xor(isSSH, needSSH)) {
    remote_set_url(repo1, "origin", url = setdiff(urls, url1))
  }

  return(invisible(repo1))
}
