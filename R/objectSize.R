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

#' @export
objSize <- function(x, quick, enclosingEnvs, .prevEnvirs, ...) {
  UseMethod("objSize", x)
}

#' @export
#' @importFrom utils object.size
#' @importFrom data.table setorderv
objSize.default <- function(x, quick = getOption("reproducible.quick", FALSE),
                            enclosingEnvs = TRUE, .prevEnvirs = list(), ...) {

  .prevEnvirs <- list()
  # .ord <- 0

  # .objSizeObj <- list()

  parentObjName <- deparse(substitute(x))
  if (is.environment(x)) {
    envName <- environmentName(x)
    if (nchar(envName) > 0) parentObjName <- envName
  }

  browser()
  os <- obSizeInner(x, quick = quick,
          parentObjName = parentObjName, parentAddress = "")
  # os <- objsizeInner(x, quick = quick, outerEnv = environment(),
  #                    parentObjName = parentObjName, parentAddress = "")

  browser()
  os3 <- data.table(object = names(os), bytes = unlist(os))
  # set(os4, NULL, "object", names(os))
  #
  # # browser()
  # os2 <- rbindlist(.objSizeObj)
  # setorderv(os2, "order")
  # #
  # keepCols <- c("parentName", "os")
  # os3 <- unique(os2, by = "add")
  # # setnames(os3, old = keepCols, c("object", "bytes"))
  #
  # if (NROW(os3) > 1) {
  #   osTop <- os3[1]
  #   setnames(osTop, "os", "bytes")
  #
  #   addsByLevel <- list(os3$add[1])
  #   indices <- list(1)
  #   for(lev in 2:NROW(os3)) {
  #     index <- which(os3$parentAdd %in% addsByLevel[[lev - 1]])
  #     val <- os3$add[index]
  #     if (length(val) == 0) break
  #     addsByLevel[[lev]] <- val
  #     indices[[lev]] <- index
  #   }
  #
  #   if (lev > 2) {
  #     os5 <- os3[indices[[lev - 1]]]
  #     set(os5, NULL, "V1", 0)
  #     for (lev2 in rev(3:length(indices))) {
  #       os4 <- os5[, sum(c(V1, os), na.rm = TRUE), by = "parentAdd"]
  #       os5 <- os4[os3[indices[[lev2 - 1]]], on = c("parentAdd" = "add")]
  #       set(os5, NULL, "parentAdd", NULL)
  #       setnames(os5, "i.parentAdd", "parentAdd")
  #     }
  #     os5[!is.na(V1), os := V1 + os]
  #     setnames(os5, "os", "bytes")
  #     os6 <- rbindlist(list(osTop, os5), fill = TRUE)
  #     if (is.na(os6$parentName[1]))
  #       os6[1, parentName := "Top"]
  #     os3 <- os6
  #   }
  #
  # } else {
  #   setnames(os3, "os", "bytes")
  # }

  #colsToDel <- setdiff(colnames(os3), c("parentName", "bytes"))
  #set(os3, NULL, colsToDel, NULL)

  # setnames(os3, old = "parentName", "object")
  total <- sum(unlist(os3$bytes), na.rm = TRUE)
  class(total) <- "object_size"

  bb <- list(objects = os3, total = total)

  return(bb)

}

sizeInner <- function(os) {
  if (length(os) > 1) {
    out <- lapply(os, sizeInner)
  } else {
    out <- as.list(os)
  }
  out
}

addressInner <- function(os) {
  if (length(os) > 1) {
    out <- lapply(os, addressInner)
  } else {
    out <- list(attr(os, "add"))
  }
  out
}

objsizeInner <- function(x, quick, outerEnv, parentObjName, parentAddress) {
  xClass <- class(x)[1]
  # .ord <- get(".ord", envir = outerEnv)
  # .ord <- .ord + 1
  # assign(".ord", .ord, envir = outerEnv)

  add <- if (quick) rndstr(1) else .address(x)
  if (!is.null(x)) {
    os <- 0

    xEnv <- environment(x)
    hasEnv <- !is.null(xEnv)
    isEnv <- is.environment(x)
    if (hasEnv || isEnv) {
      peHere <- if (hasEnv) xEnv else x
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

    # if ( (is(x, "list") && !quick) || (quick & (isEnv || hasEnv)) && length(x) > 1) {
    if ( (is(x, "list") && (!quick || identical(outerEnv, parent.frame()))) && length(x) > 1) {

      if (length(x) > 0) {
        parentObjNames <- if (is.null(names(x))) {
          paste0("y", seq(x))
        } else {
          names(x)
        }
        osInner <-
          Map(xx = x, parentObjName = parentObjNames,
              function(xx, parentObjName) objsizeInner(xx, quick = quick, outerEnv,
                                                       parentObjName = parentObjName,
                                                       parentAddress = add))
        os <- osInner
      }
      if (exists("osEnv", inherits = FALSE)) {
        attr(osEnv, "add") <- add
        osEnvList <- list(osEnv)
        names(osEnvList) <- parentObjName
        os <- append(osEnvList, os)

      }

    } else {
      if (is(x, "simList_")) browser()
      os <- list(object.size(x))
      if (is.null(names(os)))
        names(os) <- parentObjName
      if (hasEnv) {
        os <- list(unlist(os) + object.size(attributes(x)))
      }
    }

    if (length(os) == 0) {
      os <- list(0)
    }

    # if (quick) {
    #   if (is.null(names(os)))
    #     varNames <- "yy"
    #   else
    #     varNames <- names(unlist(os))
    #   oses <- unlist(os)
    # } else {
    #   if (is.null(names(os)))
    #     varNames <- "yy"
    #   else
    #     varNames <- names(unlist(os))[1]
    #   oses <- unlist(os)[1]
    # }


    #.objSizeObj <- get(".objSizeObj", envir = outerEnv)
    if (is.null(os)) os <- 0

    # if (quick) {
    #   .objSizeObj <- append(.objSizeObj,
    #                         list(list(parentName = parentObjName, varName = varNames,
    #                                         os = oses, add = add, parentAdd = parentAddress,
    #                                         class = xClass, order = .ord)))
    # } else {
    #   .objSizeObj <- append(.objSizeObj,
    #                         list(list(parentName = parentObjName, varName = varNames,
    #                                   os = oses, add = add, parentAdd = parentAddress,
    #                                   class = xClass, order = .ord)))
    # }
    # assign(".objSizeObj", .objSizeObj, envir = outerEnv)


  } else {
    os <- list(0)
  }
  attr(os, "add") <- add

  if (identical(outerEnv, parent.frame())) {
    ad <- unlist(addressInner(os))
    si <- unlist(sizeInner(os))
    lengths <- lapply(os, function(x) length(unlist(x)))
    lengths[lengths == 0] <- 1
    lengths <- rep(names(lengths), lengths)
    dt <- data.table(object = lengths, object2 = names(ad),
                     bytes = si[names(ad)], address = ad)
    dt <- unique(dt, by = "address")
    browser()
    dt[is.na(bytes), bytes := 0]
    dt <- dt[, list(bytes = sum(bytes)), by = "object"]

    bytesTop <- unlist(lapply(x, object.size)[dt$object])
    dt[, bytes2 := bytesTop[object]]
    dt[, bytes := max(bytes, bytes2, na.rm = TRUE), by = seq(NROW(dt))]

    os <- as.list(dt$bytes)
    names(os) <- dt$object
  }

  return(os)


}


obSizeInner <- local({
  i <- 1
  .prevEnvirs <- list(.GlobalEnv, environment())
  function(x, quick, parentObjName, parentAddress) {
    i <<- i + 1

    print(i)
    out <- 0

    # if (is.function(x)) browser()
    if (!is.null(x)) {
      parentEnv <- environment(x)
      needObjSizeXFun <- FALSE
      osThisX <- list(object.size(x))
      names(osThisX) <- parentObjName
      if (!is.null(parentEnv)) { # functions, formulas
        attr(parentEnv, "name") <- parentObjName
        browser()
        out <- obSizeInner(parentEnv, quick = quick, parentObjName = parentObjName,
                    parentAddress = parentAddress)
        out <- append()
        #xFun <- x # keep the function
        #x <- parentEnv
        # needObjSizeXFun <- TRUE
      }
      if (is.environment(x)) {
        if (all(!unlist(lapply(.prevEnvirs, function(pe) identical(x, pe))))) {
          needObjSizeXFun <- TRUE
          # xFun <- x # keep the function
          .prevEnvirs <<- append(.prevEnvirs, x)
          if (!is.list(x))
            x <- as.list(x, all.names = TRUE)
        }
      }
      if (is(x, "list") && length(x) > 0) { # recursion branch for envs
        parentObjNames <- names(x)
        if (is.null(parentObjNames)) {
          parentObjNames <- paste0("v", seq(x))
          names(x) <- parentObjNames
        }
        out <- Map(x = x, parentObjName = parentObjNames,
                   MoreArgs = list(quick = quick,
                                   parentAddress = parentAddress),
                   obSizeInner)
        #browser()
        #out <- append(osThisX, out)
      } else {
        out <- object.size(x)
      }
    }
    return(out)
  }
  })

#' Get memory address using .Internal(inspect(obj))
#'
#' Not intended for user use.
#'
#' @param obj An R object
address <- function(obj) {
  objName <- deparse(substitute(obj))
  if (is(obj, "list")) {
    out <- lapply(obj, address)
    outer <- capture.output(.Internal(inspect(obj)))
    outer <- substr(outer[1], 1, 19)
    out <- append(list(outer), out)
  } else {
    out <- capture.output(.Internal(inspect(obj)))
    out <- substr(out, 1, 19)
  }
  out
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

.address <- function(x) {
  add <- capture.output(.Internal(inspect(x)))
  add <- paste0(substr(add[1], 2, 3),
                substr(add[1], 9, 19))
  add
}

