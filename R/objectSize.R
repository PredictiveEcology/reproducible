
#' Recursive object.size
#'
#' This has methods for various types of things that may not correctly report
#' their object.size using \code{object\.size}. Also, for lists and environments,
#' it will return the object.size separately for each element. These are estimates
#' only, and could be inaccurate. Alternative, similar functions include
#' \code{object.size} and \code{pryr::object_size}. See Details for the special case of
#' functions and their enclosing environments.
#'
#' @param x An object
#' @param quick Logical. Only some methods use this. e.g.,
#'              \code{Path} class objects. In which case, \code{file.size} will be
#'              used instead of \code{object.size}.
#' @param .prevEnvirs For internal account keeping to identify and prevent duplicate counting
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
#'
#' object.size(prepInputs) # only the function, without its enclosing environment
#' objSize(prepInputs)     # the function, plus its enclosing environment
#'
#' # Size of all packages; includes their imported functions
#' a <- objSizeSession(1)
#' print(a, units = "auto")
#'
#' os1 <- object.size(as.environment("package:reproducible"))
#' os2 <- objSize(as.environment("package:reproducible"))
#' (os1) # very small -- just the environment container
#' sum(unlist(os2)) # around 31 MB, with all functions, objects
#'                  # and imported functions
objSize <- function(x, quick, enclosingEnvs, .prevEnvirs) {
  UseMethod("objSize", x)
}

#' @export
#' @rdname objSize
objSize.list <- function(x, quick = getOption("reproducible.quick", FALSE),
                        enclosingEnvs = TRUE, .prevEnvirs = list()) {
  TandC <- grepl(".__[TC]__", names(x))
  if (sum(TandC) > 0)
    x <- x[!TandC]
  osList <- lapply(x, function(y) {
    if (!is.function(y)) {
      os <- objSize(y, quick = quick, enclosingEnvs = enclosingEnvs,
                    .prevEnvirs = .prevEnvirs)
    } else {
      doneAlready <- lapply(.prevEnvirs, function(pe) identical(pe, environment(y)))
      .prevEnvirs <<- unique(append(.prevEnvirs, environment(y)))
      if (!any(unlist(doneAlready))) {
        os <- objSize(y, quick = quick, enclosingEnvs =enclosingEnvs,
                      .prevEnvirs = .prevEnvirs)
      } else {
        os <- NULL
      }
    }
    return(os)
  })
  if (length(osList) > 0)
    osList <- osList[!unlist(lapply(osList, function(x) is.null(x) || length(x) == 0))]
  osList
}

#' @export
#' @rdname objSize
#' @importFrom utils object.size
objSize.environment <- function(x, quick = getOption("reproducible.quick", FALSE),
                                enclosingEnvs = TRUE, .prevEnvirs = list()) {
  xName <- deparse(substitute(x))
  print(format(x))
  os <- objSize(as.list(x, all.names = TRUE), enclosingEnvs = enclosingEnvs,
                .prevEnvirs = .prevEnvirs)
  if (length(os) > 0)
    names(os) <- paste0(xName, "$", names(os))
  osCur <- list(object.size(x))
  names(osCur) <- xName
  os <- append(os, osCur)
  return(os)
}

#' @export
#' @rdname objSize
objSize.default <- function(x, quick = getOption("reproducible.quick", FALSE),
                            enclosingEnvs = TRUE, .prevEnvirs = list()) {
  object.size(x)
}

#' @export
#' @rdname objSize
objSize.Path <- function(x, quick = getOption("reproducible.quick", FALSE),
                         enclosingEnvs = TRUE, .prevEnvirs = list()) {
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
                             enclosingEnvs = TRUE, .prevEnvirs = list()) {
  varName <- deparse(substitute(x))
  if (isTRUE(enclosingEnvs) && (!identical(.GlobalEnv, environment(x)))) {
    if (is.primitive(x)) {
      os <- list(object.size(x))
    } else {
      x <- mget(ls(envir = environment(x)), envir = environment(x))
      os <- lapply(x, function(xx) object.size(xx))
    }
  } else {
    os <- object.size(x)
  }
  return(os)

}

#' @rdname objSize
#' @param sumLevel Numeric, indicating at which depth in the list of objects should the
#'   object sizes be summed (summarized). Default is \code{Inf}, meaning no sums. Currently,
#'   the only option other than Inf is 1: \code{objSizeSession(1)},
#'   which gives the size of each package.
#' @export
#' @details \code{objSizeSession} will give the size of the whole session, including loaded packages.
#' Because of the difficulties in calculating the object size of \code{base}
#' and \code{methods} packages and \code{Autoloads}, these are
#' omitted.
objSizeSession <- function(sumLevel = Inf, enclosingEnvs = TRUE, .prevEnvirs = list()) {
  srch <- search()
  srch <- setdiff(srch, c("package:base", "package:methods", "Autoloads"))
  names(srch) <- srch
  os <- lapply(srch, function(x) {
    doneAlready <- lapply(.prevEnvirs, function(pe) identical(pe, as.environment(x)))
    .prevEnvirs <<- unique(append(.prevEnvirs, as.environment(x)))
    out <- if (!any(unlist(doneAlready))) {
      try(objSize(as.environment(x), enclosingEnvs = enclosingEnvs,
                  .prevEnvirs = .prevEnvirs))
    } else {
      NULL
    }
    return(out)
  })
  if (sumLevel == 1) {
    os <- lapply(os, function(x) {
      osIn <- sum(unlist(x))
      class(osIn) <- "object_size"
      osIn
    })
  } else if (sumLevel == 0) {
    os <- sum(unlist(os))
    class(os) <- "object_size"
    os
  }

  return(os)
}

