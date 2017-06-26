#' Clone, pull, & checkout from github.com repositories
#'
#' In reproducible research, not only do packages and R version have to be
#' consistent, but also specific versions of version controlled scripts.
#' This function allows a simple way to create an exactly copy locally of a git
#' repository. It can use ssh keys (including GitHub deploy keys) or GitHub
#' Personal Access Tokens.
#'
#' @inheritParams devtools::install_github
#'
#' @import devtools
#' @importFrom git2r checkout clone commit cred_token cred_ssh_key head init
#'                   lookup pull remote_set_url remote_url revparse_single status
#' @importFrom utils getFromNamespace
#'
#' @param localRepoPath Character string. The path into which the git repo should be
#'                      cloned, pulled, and checked out from.
#'
#' @param cred Character string. Either the name of the environment variable
#'             that contains the GitHub PAT or filename of the GitHub private key file.
#'
#' @return Invisibly returns a repository class object, defined in
#'         \code{\link[git2r]{git_repository-class}}
#'
checkoutVersion <- function(repo, localRepoPath = ".", cred = "") {
  .parse_git_repo <- utils::getFromNamespace("parse_git_repo", "devtools")
  params <- .parse_git_repo(repo)
  gitHash <- if (is.null(params$ref)) "master" else params$ref

  repositoryName <- params$repo
  repositoryAccount <- params$username

  githubPrivateKeyFile <- if (file.exists(cred)) cred else NULL

   if (is.null(githubPrivateKeyFile)) {
     cred <- cred_token(cred)
   } else {
     cred <- cred_ssh_key(publickey = paste0(githubPrivateKeyFile, ".pub"),
                          privatekey = githubPrivateKeyFile)
   }

  pathExists <- suppressWarnings(file.exists(normalizePath(path.expand(localRepoPath))))
  httpsURL <- paste0("https://github.com/", repositoryAccount, "/", repositoryName, ".git")
  sshURL <- paste0("git@github.com:", repositoryAccount, "/", repositoryName, ".git")

  needSSH <- class(cred) == "cred_ssh_key"
  urls <- c(httpsURL, sshURL)
  url1 <- ifelse(needSSH, sshURL, httpsURL)

  if (!(pathExists)) {
    # using "~" in a path doesn't seem to work correctly. Must use path.expand to
    #  give an absolute path in this case.
    clone(url1, path.expand(localRepoPath), branch = gitHash, credentials = cred)
  }

  # If repo is set to using ssh, git2r package doesn't work -- must change it
  repo <- init(localRepoPath)
  isSSH <- any(grepl(httpsURL, remote_url(repo)))
  needSwitchURL <- xor(isSSH, needSSH)
  if (needSwitchURL) {
    remote_set_url(repo, "origin", url = url1)
  }

  if (gitHash %in% c("development", "master")) git2r::pull(repo, cred)

  tryCatch(git2r::checkout(lookup(repo, gitHash)), error = function(x) {
    checkout(repo, gitHash)
  })

  if (needSwitchURL) {
    remote_set_url(repo, "origin", url = setdiff(urls, url1))
  }

  return(invisible(repo))
}
