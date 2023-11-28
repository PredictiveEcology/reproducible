#' Create a list with names from object names
#'
#' This is a convenience wrapper around `newList <- list(a = 1); names(newList) <- "a"`.
#'
#' @param ... Any elements to add to a list, as in `base::list`
#' @details
#' This will return a named list, where names are the object names, captured
#' internally in the function and assigned to the list. If a user manually supplies
#' names, these will be kept (i.e., not overwritten by the object name).
#' @export
#' @examples
#' a <- 1
#' b <- 2
#' d <- 3
#' (newList <- listNamed(a, b, dManual = d)) # "dManual" name kept
#'
#'
listNamed <- function(...) {
  dotsSUB <- as.list(substitute(list(...)))[-1]
  names <- vapply(dotsSUB, deparse, FUN.VALUE = character(1))
  ll <- list(...)
  existingNames <- names(dotsSUB)
  if (!is.null(existingNames)) {
    nonEmptyNames <- nzchar(existingNames)
    names[nonEmptyNames] <- existingNames[nonEmptyNames]
  }
  names(ll) <- names
  ll
}
