#' Recursive \code{object.size}
#'
#' This has methods for various types of things that may not correctly report
#' their object size using \code{object.size}.
#' Also, for lists and environments, it will return the object size separately for each element.
#' These are estimates only, and could be inaccurate.
#' Alternative, similar functions include \code{object.size} and \code{pryr::object_size}.
#' See Details for the special case of functions and their enclosing environments.
#'
#' @param x An object
#' @param enclosingEnvs Logical indicating whether to include enclosing environments.
#'                      Default \code{TRUE}.
#' @param quick Logical. Only some methods use this. e.g.,
#'              \code{Path} class objects. In which case, \code{file.size} will be
#'              used instead of \code{object.size}.
#' @param .prevEnvirs For internal account keeping to identify and prevent duplicate counting
#' @param ...  Additional arguments (currently unused)
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
objSize <- function(x, quick, enclosingEnvs, .prevEnvirs, ...) {
  UseMethod("objSize", x)
}

#' @export
#' @importFrom utils object.size
#' @rdname objSize
#' @details
#' For functions, a user can include the enclosing environment as described
#' \url{https://www.r-bloggers.com/2015/03/using-closures-as-objects-in-r/} and
#' \url{http://adv-r.had.co.nz/memory.html}.
#' It is not entirely clear which estimate is better.
#' However, if the enclosing environment is the \code{.GlobalEnv}, it will
#' not be included even though \code{enclosingEnvs = TRUE}.
#'
objSize.default <- function(x, quick = getOption("reproducible.quick", FALSE),
                            enclosingEnvs = TRUE, .prevEnvirs = list(), ...) {
  if (any(inherits(x, "SpatVector"), inherits(x, "SpatRaster"))) {
    if (!requireNamespace("terra") && getOption("reproducible.useTerra", FALSE))
      stop("Please install terra package")
      os <- object.size(terra::wrap(x))
  } else {
    varName <- deparse(substitute(x))

    hasEnclEnv <- if ((is(x, "function") && !is.primitive(x)) ||
                      is(x, "formula") || is(x, "expression")) {
      TRUE
    } else {
      FALSE
    }

    os <- object.size(x)

    if (hasEnclEnv) {
      browser()
      curEnvYYY <- environment(x)
      osEnv <- objSize(curEnvYYY, quick = quick, enclosingEnvs = enclosingEnvs, .prevEnvirs = .prevEnvirs)
      names(osEnv) <- gsub("curEnvYYY", paste0(varName, "_enclEnvir"), names(osEnv))
      os <- list(os)
      names(os) <- varName
      os <- append(os, osEnv)
    }

  }
  total <- sum(unlist(os))
  class(total) <- "object_size"
  attr(os, "total") <- noquote(format(total, units = "auto"))
  os
}

#' @export
#' @rdname objSize
objSize.list <- function(x, quick = getOption("reproducible.quick", FALSE),
                         enclosingEnvs = TRUE, .prevEnvirs = list(), ...) {
  TandC <- grepl(".__[TC]__", names(x))
  if (sum(TandC) > 0)
    x <- x[!TandC]
  osList <- lapply(x, function(yyyy) {

    hasEnclEnv <- (is(yyyy, "function") && !is.primitive(yyyy)) ||
                      is(yyyy, "formula") || is(yyyy, "expression")
    isEnv <- is(yyyy, "environment")
    if (hasEnclEnv) {
      browser()
      localEnv <- environment(yyyy)
      if (length(.prevEnvirs) == 0) {
        doneAlready <- list(FALSE)
      } else {
        doneAlready <- lapply(.prevEnvirs, function(pe) identical(pe, localEnv))
      }
      .prevEnvirs <<- unique(append(.prevEnvirs, localEnv))

      browser()
      if (!any(unlist(doneAlready))) {
        os <- objSize(yyyy, quick, enclosingEnvs)#, .prevEnvirs)
      } else {
        os <- NULL
      }
    } else {
      os <- object.size(yyyy)
    }



    # This strips the `yyyy` from the naming... automatically done with the lapply
    if (length(names(os)) > 0) {
      names(os) <- gsub("yyyy(_)*", "", names(os))
    }
    return(os)
  })
  if (length(osList) > 0)
    osList <- osList[!unlist(lapply(osList, function(x) is.null(x) || length(x) == 0))]

  total <- sum(unlist(osList))
  class(total) <- "object_size"
  attr(osList, "total") <- noquote(format(total, units = "auto"))

  osList
}

#' @export
objsize <- function(x, quick, enclosingEnvs, .prevEnvirs, ...) {
  UseMethod("objsize", x)
}

#' @export
#' @importFrom utils object.size
objsize.default <- function(x, quick = getOption("reproducible.quick", FALSE),
                            enclosingEnvs = TRUE, .prevEnvirs = list(), ...) {

  .prevEnvirs <- list()
  os <- objsizeInner(x, outerEnv = environment())

  total <- sum(unlist(os))
  class(total) <- "object_size"
  attr(os, "total") <- noquote(format(total, units = "auto", digits = 2))

  return(os)

}

#' @export
objsize.simList <- function(x, quick = getOption("reproducible.quick", FALSE),
                            enclosingEnvs = TRUE, .prevEnvirs = list(), ...) {

  aa <- objsize(x@.xData, quick, enclosingEnvs, .prevEnvirs)

  theSimEnvItself <- names(aa) == "x@.xData"
  names(aa)[theSimEnvItself] <- "simEnv"
  aa <- lapply(aa, function(a) {
    ss <- sum(unlist(a))
    class(ss) <- "object_size"
    ss
  })
  names(aa) <- gsub("x@.xData\\$*", "", names(aa))
  simSlots <- grep("^\\.envir$|^\\.xData$", slotNames(x), value = TRUE, invert = TRUE)
  names(simSlots) <- simSlots
  otherParts <- objsize(lapply(simSlots, function(slotNam) slot(x, slotNam)))
  other <- lapply(otherParts, function(op) {
    ss <- sum(unlist(op))
    class(ss) <- "object_size"
    ss
  })

  aa <- list(sim = aa, other = other)
  total <- sum(unlist(aa))
  class(total) <- "object_size"
  attr(aa, "total") <- noquote(format(total, units = "auto"))

  return(aa)
}

objsizeInner <- function(x, quick, outerEnv) {

  if (!is.null(x)) {
    os <- 0
    xEnv <- environment(x)
    hasEnv <- !is.null(xEnv)
    isEnv <- is.environment(x)
    if (hasEnv || isEnv) {
      peHere <- if (hasEnv) xEnv else x
      if (hasEnv)
        osHasEnv <- object.size(x)
      osEnv <- sum(object.size(peHere), object.size(attributes(peHere)))
      isGlobal <- identical(.GlobalEnv, peHere)

      if (!isGlobal) {
        peOuter <- get(".prevEnvirs", envir = outerEnv)
        doneAlready <- lapply(peOuter, function(pe) identical(pe, peHere))

        if (!any(unlist(doneAlready))) {
          newPe <- append(peOuter, peHere)
          assign(".prevEnvirs", newPe, outerEnv)
          if (length(ls(peHere, all.names = TRUE)) > 0)
            x <- as.list(peHere, all.names = TRUE)
        }
      }
    }

    if (is(x, "list")) {
      anyNested <- lapply(x, function(xInner) is(xInner, "list") || is(xInner, "environment"))
      if (any(unlist(anyNested))) {
        osInner <- if (length(x) > 1)
          lapply(x, function(xx) objsizeInner(xx, quick, outerEnv))
        else
          list(objsizeInner(x[[1]], quick, outerEnv))
      } else {
        osInner <- object.size(x)
      }
      os <- osInner
      if (exists("osEnv", inherits = FALSE))
        os <- append(list(osEnv), os)
      if (exists("osHasEnv", inherits = FALSE))
        os <- append(list(osHasEnv), os)

    } else {
      os <- object.size(x)
    }
  } else {
    os <- NULL
  }

  return(os)


}

#' @export
#' @importFrom utils object.size
#' @rdname objSize
objSize.environment <- function(x, quick = getOption("reproducible.quick", FALSE),
                                enclosingEnvs = TRUE, .prevEnvirs = list(), ...) {
  xName <- deparse(substitute(x))

  if (length(.prevEnvirs) == 0) {
    doneAlready <- list(FALSE)
  } else {
    doneAlready <- lapply(.prevEnvirs, function(pe) identical(pe, x))
  }
  .prevEnvirs <- unique(append(.prevEnvirs, x))

  lsEnv <- ls(x, all.names = TRUE)

  if (!any(unlist(doneAlready)) && length(lsEnv) > 0) {
    browser()
    asList <- as.list(x, all.names = TRUE)
    os <- objSize(asList, enclosingEnvs = enclosingEnvs,
            .prevEnvirs = .prevEnvirs)

  } else {
    os <- NULL
  }
  if (length(os) > 0)
    names(os) <- paste0(xName, "$", names(os))
  osCur <- list(object.size(x))
  names(osCur) <- xName
  os <- append(os, osCur)

  total <- sum(unlist(os))
  class(total) <- "object_size"
  attr(os, "total") <- noquote(format(total, units = "auto"))
  return(os)
}

#' @export
#' @importFrom utils object.size
#' @rdname objSize
objSize.Path <- function(x, quick = getOption("reproducible.quick", FALSE),
                         enclosingEnvs = TRUE, .prevEnvirs = list(), ...) {
  if (quick) {
    object.size(x)
  } else {
    file.size(x)
  }
}


#' @param sumLevel Numeric, indicating at which depth in the list of objects should the
#'   object sizes be summed (summarized). Default is \code{Inf}, meaning no sums. Currently,
#'   the only option other than Inf is 1: \code{objSizeSession(1)},
#'   which gives the size of each package.
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

#' Determine if an environment is a top level environment
#'
#' Here, we define that as .GlobalEnv, any namespace, emptyenv,
#' or baseenv. This is useful to determine the effective size
#' of an R function, due to R including the objects from enclosing
#' environments
#'
#' @param x Any environment
#'
#' @return
#' A logical. \code{FALSE} if it is not one of the "Top Level Environments",
#' \code{TRUE} otherwise.
#' @export
isTopLevelEnv <- function(x) {
  identical(.GlobalEnv, x) ||
       isNamespace(x) ||
       identical(emptyenv(), x) ||
       identical(baseenv(), x)
}
