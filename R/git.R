#' Clone, pull, & checkout from github.com repositories
#'
#' In reproducible research, not only do packages and R version have to be
#' consistent, but also specific versions of version controlled scripts. This
#' function allows a simple way to create an exactly copy locally of a git
#' repository. It can use ssh keys (including github deploy keys) or GitHub
#' Personal Access Tokens.
#'
#' @inheritParams devtools::install_github
#' @import devtools
#' @importFrom git2r checkout remote_set_url pull remote_url lookup
#' @importFrom git2r cred_token cred_ssh_key clone init
#'
#' @param localRepoPath Character string. The path into which the git repo should be
#'        cloned, pulled, and checked out from.
#' @param cred Character string. Either the name of the environment variable
#'             that contains the GitHub PAT or filename of the github Private Key File.
#' @return Invisibly returns a repository class object, defined in
#' \code{\link[git2r]{git_repository}}
checkoutVersion <- function(repo, localRepoPath=".", cred = "") {

  params <- devtools:::parse_git_repo(repo)
  gitHash <- if(is.null(params$ref)) "master" else params$ref
  repositoryName <- params$repo
  repositoryAccount <- params$username

  githubPrivateKeyFile <- if(file.exists(cred)) cred else NULL

   if(is.null(githubPrivateKeyFile)) {
     cred <- git2r::cred_token(cred)
   } else {
     cred <- git2r::cred_ssh_key(publickey = paste0(githubPrivateKeyFile,".pub"),
                                 privatekey = githubPrivateKeyFile)
   }

  pathExists <- suppressWarnings(file.exists(normalizePath(path.expand(localRepoPath))))
  httpsURL <- paste0("https://github.com/",repositoryAccount,"/",repositoryName,".git")
  sshURL <- paste0("git@github.com:",repositoryAccount,"/",repositoryName,".git")

  needSSH <- class(cred)=="cred_ssh_key"
  urls <- c(httpsURL, sshURL)
  url1 <- ifelse(needSSH, sshURL, httpsURL)

  if(!(pathExists)) {
    # using "~" in a path doesn't seem to work correctly. Must use path.expand to
    #  give an absolute path in this case.
    git2r::clone(url1, path.expand(localRepoPath), branch=gitHash, credentials=cred)

  }

  # If repo is set to using ssh, git2r package doesn't work -- must change it
  repo <- git2r::init(localRepoPath)

  isSSH <- any(grepl(sshURL, git2r::remote_url(repo)))
  #needHTTPS <- any(grepl(httpsURL, git2r::remote_url(repo))) & !(class(cred)=="cred_ssh_key")
  needSwitchURL <- xor(isSSH, needSSH)

  if(needSwitchURL)
    git2r::remote_set_url(repo, "origin", url=url1)

  # # Get specific LandWeb version
  # hasUncommittedFiles <- sum(sapply(status(repo), length))>0
  # if(hasUncommittedFiles) {
  #   lastCommit <- revparse_single(repo, "HEAD")
  #   git2r::add(repo, unlist(status(repo)$unstaged))
  #   tempCommit <- commit(repo, "testing")
  # } else {
  #   lastCommit <- NULL
  # }

  if(gitHash %in% c("development", "master")) git2r::pull(repo, cred)

  tryCatch(git2r::checkout(lookup(repo, gitHash)), error=function(x)
    git2r::checkout(repo, gitHash))

  if(needSwitchURL)
    git2r::remote_set_url(repo, "origin", url=setdiff(urls, url1))

  return(invisible(repo))
}

