#' Wrapper around \code{lobstr::obj_size}
#'
#' This will return the result from \code{lobstr::obj_size}, i.e., a \code{lobstr_bytes}
#' which is a \code{numeric}. If \code{quick = FALSE}, it will also have an attribute,
#' "objSize", which will
#' be a list with each element being the \code{objSize} of the individual elements of \code{x}.
#' This is particularly useful if \code{x} is a \code{list} or \code{environment}.
#' However, because of the potential for shared memory, the sum of the individual
#' elements will generally not equal the value returned from this function.
#'
#' @param x An object
#' @param quick Logical. If \code{FALSE}, then an attribute, "objSize" will be added to
#'   the returned value, with each of the elements' object size returned also.
#' @param ...  Additional arguments (currently unused), enables backwards compatible use.
#'
#' @export
#' @rdname objSize
#'
#' @examples
#' library(utils)
#'
#' foo <- new.env()
#' foo$b <- 1:10
#' foo$d <- 1:10
#'
#' objSize(foo) # all the elements in the environment
#' object.size(foo) # different - only measuring the environment as an object
#'
#' object.size(prepInputs) # only the function, without its enclosing environment
#' objSize(prepInputs)     # the function, plus its enclosing environment
#'
#' # Size of all packages; includes their imported functions
#' \dontrun{
#'   bar <- objSizeSession(1)
#'   print(bar, units = "auto")
#' }
#'
#' os1 <- object.size(as.environment("package:reproducible"))
#' os2 <- objSize(as.environment("package:reproducible"))
#' (os1) # very small -- just the environment container
#' sum(unlist(os2)) # around 13 MB, with all functions, objects
#'                  # and imported functions
#'
#' @importFrom utils object.size
#' @details
#' For functions, a user can include the enclosing environment as described
#' \url{https://www.r-bloggers.com/2015/03/using-closures-as-objects-in-r/} and
#' \url{http://adv-r.had.co.nz/memory.html}.
#' It is not entirely clear which estimate is better.
#' However, if the enclosing environment is the \code{.GlobalEnv}, it will
#' not be included even though \code{enclosingEnvs = TRUE}.
#'
#' @export
objSize <- function(x, quick = TRUE, ...) {
  UseMethod("objSize", x)
}

#' @export
#' @importFrom lobstr obj_size
objSize.default <- function(x, quick = TRUE, ...) {
  out <- obj_size(x)
  if (!quick)
    attr(out, "objSize") <- list(obj = out)
  out
}

#' @export
#' @importFrom lobstr obj_size
objSize.list <- function(x, quick = TRUE, ...) {
  os <- obj_size(x)
  if (!quick) {
    out <- lapply(x, lobstr::obj_size, quick = quick)
    attr(os, "objSize") <- out
  }
  os
}

#' @export
#' @importFrom lobstr obj_size
objSize.Path <- function(x, quick = TRUE, ...) {
  if (quick) {
    os <- obj_size(x)
  } else {
    os <- file.size(x)
  }

  if (!quick) {
    out <- lapply(x, lobstr::obj_size, quick = quick)
    attr(os, "objSize") <- out
  }
  os
}

#' @export
#' @importFrom lobstr obj_size
objSize.environment <- function(x, quick = TRUE, ...) {
  os <- obj_size(x)
  if (!quick) {
    out <- lapply(as.list(x, all.names = TRUE), lobstr::obj_size, quick = quick)
    attr(os, "objSize") <- out
  }
  os
}


#' @param sumLevel Numeric, indicating at which depth in the list of objects should the
#'   object sizes be summed (summarized). Default is \code{Inf}, meaning no sums. Currently,
#'   the only option other than Inf is 1: \code{objSizeSession(1)},
#'   which gives the size of each package.
#' @param enclosingEnvs Logical indicating whether to include enclosing environments.
#'                      Default \code{TRUE}.
#' @param .prevEnvirs For internal account keeping to identify and prevent duplicate counting
#'
#'
#' @details \code{objSizeSession} will give the size of the whole session, including loaded packages.
#' Because of the difficulties in calculating the object size of \code{base}
#' and \code{methods} packages and \code{Autoloads}, these are omitted.
#'
#' @export
#' @rdname objSize
objSizeSession <- function(sumLevel = Inf, enclosingEnvs = TRUE, .prevEnvirs = list()) {
  srch <- search()
  srch <- setdiff(srch, c("package:base", "package:methods", "Autoloads"))
  names(srch) <- srch
  os <- lapply(srch, function(x) {
    doneAlready <- lapply(.prevEnvirs, function(pe)
      tryCatch(identical(pe, as.environment(x)), error = function(e) FALSE))
    # Update the object in the function so next lapply has access to the updated version
    .prevEnvirs <<- unique(append(.prevEnvirs, as.environment(x)))
    out <- if (!any(unlist(doneAlready))) {
      xAsEnv <- as.environment(x)
      if (!identical(xAsEnv, globalenv())) {
        xAsEnv <- tryCatch(asNamespace(gsub("package:", "", x)), error = function(x) xAsEnv)
      }
      tryCatch(
        objSize(xAsEnv, enclosingEnvs = enclosingEnvs,
                  .prevEnvirs = .prevEnvirs)
        , error = function(x) NULL,
              warning = function(y) NULL
        )
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

