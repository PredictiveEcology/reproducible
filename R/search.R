#' Search up the full scope for functions
#'
#' @description
#' This is like \code{base::search} but when used inside a function, it will
#' show the full scope (see figure in the section \emph{Binding environments}
#' on \url{http://adv-r.had.co.nz/Environments.html}).
#' This full search path will be potentially much longer than
#' just \code{search()} (which always starts at \code{.GlobalEnv}).
#'
#' \code{searchFullEx} shows an example function that is inside this package
#' whose only function is to show the Scope of a package function.
#'
#' @description
#'
#' @param env The environment to start searching at. Default is
#'                calling environment, i.e., \code{parent.frame()}
#' @param simplify Logical. Should the output be simplified to character,
#'                 where possible
#' @return
#' Similar to \code{readLines}. It may not return identical results.
#'
#' @export
#' @examples
#' seeScope <- function() {
#'   searchFull()
#' }
#' seeScope()
#' searchFull()
#'
#' @rdname search
searchFull <- function(env = parent.frame(), simplify = TRUE) {
  envs <- list()
  counter <- 0
  while(!identical(parent.env(env), emptyenv())) {
    counter = counter + 1
    envs[[counter]] <- parent.env(env)
    env <- envs[[counter]]
  }
  if (simplify) {
    lapply(envs, function(x) if (!is.null(attr(x, "name"))) environmentName(x) else x)
  } else {
    envs
  }
}

#' @export
#' @examples
#' searchFullEx()
#' @rdname search
searchFullEx <- function() {
  searchFull()
}
