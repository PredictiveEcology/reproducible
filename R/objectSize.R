
#' Recursive object.size
#'
#' This has methods for various types of things that may not correctly report
#' their object.size using \code{object\.size}. Also, for lists and environments,
#' it will return the object.size separately for each element.
#'
#' @param x An object
#' @param quick Logical. Only some methods use this. e.g.,
#'              \code{Path} class objects. In which case, \code{file.size} will be
#'              used instead of \code{object.size}.
#'
#' @export
#' @rdname objectSize
#' @keywords internal
#' @examples
#' a <- new.env()
#' a$b <- 1:10
#' a$d <- 1:10
#'
#' objectSize(a) # all the elements in the environment
#' object.size(a) # different - only measuring the environment as an object
objectSize <- function(x, quick) {
  UseMethod("objectSize", x)
}

#' @export
#' @rdname objectSize
objectSize.list <- function(x, quick = getOption("reproducible.quick", FALSE)) {
  lapply(x, function(y) objectSize(y))
}

#' @export
#' @rdname objectSize
#' @importFrom utils object.size
objectSize.environment <- function(x, quick = getOption("reproducible.quick", FALSE)) {
  xName <- deparse(substitute(x))
  os <- objectSize(as.list(x))
  names(os) <- paste0(xName, "$", names(os))
  osCur <- list(object.size(x))
  names(osCur) <- xName
  os <- append(os, osCur)
}

#' @export
#' @rdname objectSize
objectSize.default <- function(x, quick = getOption("reproducible.quick", FALSE)) {
  object.size(x)
}

#' @export
#' @rdname objectSize
objectSize.Path <- function(x, quick = getOption("reproducible.quick", FALSE)) {
  if (quick) {
    object.size(x)
  } else {
    file.size(x)
  }
}
