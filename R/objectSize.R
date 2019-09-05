
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
#' @param ... Other arguments passed to methods, e.g., \code{objSize.function}
#'   has \code{enclosingEnvs}
#'
#' @export
#' @rdname objSize
#' @keywords internal
#' @examples
#' a <- new.env()
#' a$b <- 1:10
#' a$d <- 1:10
#'
#' objSize(a) # all the elements in the environment
#' object.size(a) # different - only measuring the environment as an object
objSize <- function(x, quick, ...) {
  UseMethod("objSize", x)
}

#' @export
#' @rdname objSize
objSize.list <- function(x, quick = getOption("reproducible.quick", FALSE),
                         ...) {
  lapply(x, function(y) objSize(y, ...))
}

#' @export
#' @rdname objSize
#' @importFrom utils object.size
objSize.environment <- function(x, quick = getOption("reproducible.quick", FALSE), ...) {
  xName <- deparse(substitute(x))
  os <- objSize(as.list(x, all.names = TRUE), ...)
  if (length(os) > 0)
    names(os) <- paste0(xName, "$", names(os))
  osCur <- list(object.size(x))
  names(osCur) <- xName
  os <- append(os, osCur)
  return(os)
}

#' @export
#' @rdname objSize
objSize.default <- function(x, quick = getOption("reproducible.quick", FALSE), ...) {
  object.size(x)
}

#' @export
#' @rdname objSize
objSize.Path <- function(x, quick = getOption("reproducible.quick", FALSE), ...) {
  if (quick) {
    object.size(x)
  } else {
    file.size(x)
  }
}

#' @export
#' @rdname objSize
#' @details
#' For functions, a user can include the enclosing environment as described
#' \url{https://www.r-bloggers.com/using-closures-as-objects-in-r/} and
#' \url{http://adv-r.had.co.nz/memory.html}.
#' It is not entirely clear which estimate is better.
#' However, if the enclosing environment is the \code{.GlobalEnv}, it will
#' not be included even though \code{enclosingEnvs = TRUE}.
objSize.function <- function(x, quick = getOption("reproducible.quick", FALSE),
                             enclosingEnvs = TRUE, ...) {
  varName <- deparse(substitute(x))
  if (isTRUE(enclosingEnvs) && (!identical(.GlobalEnv, environment(x)))) {
    x <- mget(ls(envir = environment(x)), envir = environment(x))
    x <- lapply(x, functionToChar)
    x <- list(list("fnEnclosingEnv" = x))
    names(x) <- varName
  } else {
    x <- functionToChar(x)
  }
  objSize(x)
}

functionToChar <- function(x) {
  if (is.list(x) || is.environment(x)) {
    x <- lapply(x, functionToChar)
  } else {
    isFun <- is.function(x)
    if (isTRUE(isFun))
      x <- format(x)
  }
  x
}
