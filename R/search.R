#' Search up the full scope for functions
#'
#' @description
#' This is like `base::search` but when used inside a function, it will
#' show the full scope (see figure in the section *Binding environments*
#' on <http://adv-r.had.co.nz/Environments.html>).
#' This full search path will be potentially much longer than
#' just `search()` (which always starts at `.GlobalEnv`).
#'
#' `searchFullEx` shows an example function that is inside this package
#' whose only function is to show the Scope of a package function.
#'
#' @param env The environment to start searching at. Default is
#'                calling environment, i.e., `parent.frame()`
#' @param simplify Logical. Should the output be simplified to character,
#'                 if possible (usually it is not possible because environments
#'                 don't always coerce correctly)
#' @return
#' A list of environments that is the actual search path, unlike `search()`
#' which only prints from `.GlobalEnv` up to `base` through user attached
#' packages.
#'
#' @export
#' @rdname search
#' @seealso [base::search()]
#'
#' @examples
#' seeScope <- function() {
#'   searchFull()
#' }
#' seeScope()
#' searchFull()
searchFull <- function(env = parent.frame(), simplify = TRUE) {
  envs <- list()
  counter <- 0
  while (!identical(parent.env(env), emptyenv())) {
    counter <- counter + 1
    envs[[counter]] <- parent.env(env)
    env <- envs[[counter]]
  }
  if (simplify) {
    lapply(envs, function(x) if (!is.null(attr(x, "name"))) environmentName(x) else x)
  } else {
    envs
  }
}

#' @details
#' `searchFullEx` can be used to show an example of the use of `searchFull`.
#'
#' @export
#' @examples
#' searchFullEx()
#' @rdname search
searchFullEx <- function() {
  searchFull()
}
