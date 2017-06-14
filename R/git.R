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
#' @importFrom git2r checkout clone commit cred_token cred_ssh_key head init is_local
#'                   lookup pull remote_set_url remote_url revparse_single status
#'
#' @param localRepoPath Character string. The path into which the git repo should be
#'                      cloned, pulled, and checked out from.
#'
#' @param cred Character string. Either the name of the environment variable
#'             that contains the GitHub PAT or filename of the GitHub private key file.
#'
#' @return Invisibly returns a repository class object, defined in
#' \code{\link[git2r]{git_repository-class}}
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

  pathExists <- suppressWarnings(file.exists(normalizePath(localRepoPath)))
  httpsURL <- paste0("https://github.com/", repositoryAccount, "/", repositoryName, ".git")
  sshURL <- paste0("git@github.com:", repositoryAccount, "/", repositoryName, ".git")

  if (!(pathExists)) {
    # using "~" in a path doesn't seem to work correctly. Must use path.expand to
    #  give an absolute path in this case.
    clone(httpsURL, path.expand(localRepoPath), branch = gitHash, credentials = cred)
  }

  # If repo is set to using ssh, git2r package doesn't work -- must change it
  repo <- init(localRepoPath)
  remoteWasHTTPS <- any(grepl(httpsURL, remote_url(repo)))
  if (!remoteWasHTTPS)
    remote_set_url(repo, "origin", url = httpsURL)

  # # Get specific LandWeb version
  # hasUncommittedFiles <- sum(sapply(status(repo), length))>0
  # if(hasUncommittedFiles) {
  #   lastCommit <- revparse_single(repo, "HEAD")
  #   git2r::add(repo, unlist(status(repo)$unstaged))
  #   tempCommit <- commit(repo, "testing")
  # } else {
  #   lastCommit <- NULL
  # }

  if (gitHash %in% c("development", "master")) git2r::pull(repo, cred)

  tryCatch(git2r::checkout(lookup(repo, gitHash)), error = function(x) {
    checkout(repo, gitHash)
  })

  if (!remoteWasHTTPS) {
    remote_set_url(repo, "origin", url = sshURL)
  }

  return(invisible(repo))
}

#' Checkout the development branch of a repository
#'
#' @param checkoutCondition NEEDS DESCRIPTION
#'
#' @author Eliot Mcintire
#' @docType methods
#' @export
#' @importFrom git2r checkout reset remote_set_url
#'
checkoutDev <- function(checkoutCondition) {
  checkout(checkoutCondition$repo, "development")
  if (checkoutCondition$hasUncommittedFiles) {
    reset(checkoutCondition$lastCommit, reset_type = "soft")
  }
  if (!checkoutCondition$remoteWasHTTPS) {
    remote_set_url(checkoutCondition$repo, "origin", url = checkoutCondition$sshURL)
  }
}

#' Use 'RoboCopy' on Windows to intelligently copy files
#'
#' @param from source directory
#' @param to   destination directory
#' @param options RoboCopy options (default \code{c("xo", "e")}).
#' @param files files to copy
robocopy <- function(from, to, options = c("xo", "e"), files) {
  if (Sys.info()["sysname"] == "Windows") {
    system(paste("robocopy", normalizePath(from),
                 normalizePath(to), paste(paste0("/", options), collapse = " ")))
  } else {
    stop("RoboCopy is only available on Windows. On other operating systems, use 'rsync' if it's available instead.")
  }
}
