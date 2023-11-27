
list2 <- function(...) {
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
