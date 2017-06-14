#' Clone, pull, & checkout from github.com repositories
#'
#' In reproducible research, not only do packages and R version have to be
#' consistent, but also specific versions of version controlled scripts. This
#' function allows a simple way to create an exactly copy locally of a git
#' repository. It can use ssh keys (including github deploy keys) or GitHub
#' Personal Access Tokens.
#'
#' @inheritParams devtools::install_github
#'
#' @param localRepoPath Character string. The path into which the git repo should be
#'        cloned, pulled, and checked out from.
#' @param cred Character string. Either the name of the environment variable
#'             that contains the GitHub PAT or filename of the github Private Key File.
#'
checkoutVersion <- function(repo, localRepoPath=".", cred = "") {

  browser()
  params <- devtools:::parse_git_repo(repo)
  gitHash <- params$ref
  repositoryName <- params$repo
  repositoryAccount <- params$username

   if(!nzchar(cred)) {
     cred <- git2r::cred_token(cred)
   } else {
     cred <- git2r::cred_ssh_key(publickey = paste0(githubPrivateKeyFile,".pub"),
                                 privatekey = githubPrivateKeyFile)

   }

  pathExists <- tryCatch(SpaDES::checkPath(localRepoPath), error=function(x) NULL)
  httpsURL <- paste0("https://github.com/",repositoryAccount,"/",repositoryName,".git")
  sshURL <- paste0("git@github.com:",repositoryAccount,"/",repositoryName,".git")

  if(is.null(pathExists)) {
    git2r::clone(httpsURL, localRepoPath, branch=gitHash, credentials=cred)
  }

  # If repo is set to using ssh, git2r package doesn't work -- must change it
  repo <- git2r::init(localRepoPath)
  remoteWasHTTPS <- any(grepl(httpsURL, git2r::remote_url(repo)))
  if(!remoteWasHTTPS)
    git2r::remote_set_url(repo, "origin", url=httpsURL)

  # Get specific LandWeb version
  hasUncommittedFiles <- sum(sapply(status(repo), length))>0
  if(hasUncommittedFiles) {
    lastCommit <- revparse_single(repo, "HEAD")
    git2r::add(repo, unlist(status(repo)$unstaged))
    tempCommit <- commit(repo, "testing")
  } else {
    lastCommit <- NULL
  }

  if(gitHash %in% c("development", "master")) git2r::pull(repo, cred)

  tryCatch(git2r::checkout(lookup(repo, gitHash)), error=function(x)
    git2r::checkout(repo, gitHash))


  return(list(repo=repo, hasUncommittedFiles=hasUncommittedFiles, lastCommit=lastCommit,
              remoteWasHTTPS=remoteWasHTTPS, sshURL=sshURL))
}


checkoutDev <- function(checkoutCondition) {
  checkout(checkoutCondition$repo, "development")
  if(checkoutCondition$hasUncommittedFiles) git2r::reset(checkoutCondition$lastCommit,
                                                         reset_type = "soft")
  if(!checkoutCondition$remoteWasHTTPS)
    remote_set_url(checkoutCondition$repo, "origin", url=checkoutCondition$sshURL)

}


robocopy <- function(from, to, options=c("xo", "e"), files) {
  system(paste("robocopy", normalizePath(from),
               normalizePath(to), paste(paste0("/",options), collapse=" ")))
}
