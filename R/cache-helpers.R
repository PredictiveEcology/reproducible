################################################################################
#' Add extra tags to an archive based on class
#'
#' This is a generic definition that can be extended according to class.
#'
#' @return A character vector of new tags.
#'
#' @param object Any R object.
#'
#' @author Eliot McIntire
#' @docType methods
#' @export
#' @importFrom archivist showLocalRepo rmFromLocalRepo
#' @rdname tagsByClass
#'
setGeneric(".tagsByClass", function(object) {
  standardGeneric(".tagsByClass")
})

#' @export
#' @rdname tagsByClass
setMethod(
  ".tagsByClass",
  signature = "ANY",
  definition = function(object) {
    NULL
})

################################################################################
#' Create a custom cache message by class
#'
#' This is a generic definition that can be extended according to class.
#'
#' @return Nothing; called for its messaging side effect.
#'
#' @param object Any R object.
#' @param functionName A character string indicating the function name
#'
#' @author Eliot McIntire
#' @export
#' @docType methods
#' @rdname cacheMessage
#'
setGeneric(".cacheMessage", function(object, functionName) {
  standardGeneric(".cacheMessage")
})

#' @export
#' @rdname cacheMessage
setMethod(
  ".cacheMessage",
  signature = "ANY",
  definition = function(object, functionName) {
    message("loading cached result from previous ", functionName, " call.")
})

################################################################################
#' Determine object size of all objects inside environments
#'
#' This is a generic definition that can be extended according to class.
#'
#' @return A numeric, the result of object.size for all objects in environments.
#'
#' @param object Any R object.
#'
#' @export
#' @docType methods
#' @rdname objSizeInclEnviros
#' @author Eliot McIntire
setGeneric(".objSizeInclEnviros", function(object) {
  standardGeneric(".objSizeInclEnviros")
})

#' @export
#' @rdname objSizeInclEnviros
setMethod(
  ".objSizeInclEnviros",
  signature = "ANY",
  definition = function(object) {
    object.size(object)
})

#' @export
#' @rdname objSizeInclEnviros
setMethod(
  ".objSizeInclEnviros",
  signature = "environment",
  definition = function(object) {
    object.size(as.list(object, all.names = TRUE))
})

################################################################################
#' Add tags to object
#'
#' This is a generic definition that can be extended according to class.
#' This function and methods should do "deep" copy for archiving purposes.
#'
#' @inheritParams Cache
#'
#' @param object Any R object.
#'
#' @param FUN A function
#'
#' @return New object with tags attached.
#'
#' @author Eliot McIntire
#' @docType methods
#' @export
#' @rdname addTagsToOutput
#'
setGeneric(".addTagsToOutput", function(object, outputObjects, FUN) { # nolint
  standardGeneric(".addTagsToOutput")
})

#' @export
#' @rdname addTagsToOutput
setMethod(
  ".addTagsToOutput",
  signature = "ANY",
  definition = function(object, outputObjects, FUN) { # nolint
    object
})

################################################################################
#' Check for cache repository info in ...
#'
#' This is a generic definition that can be extended according to class.
#'
#' @param object A list of all elements in the call to Cache
#'
#' @return A character string with a path to a cache repository.
#'
#' @author Eliot McIntire
#' @docType methods
#' @export
#' @importFrom archivist showLocalRepo rmFromLocalRepo
#' @rdname checkCacheRepo
#'
setGeneric(".checkCacheRepo", function(object) {
  standardGeneric(".checkCacheRepo")
})

#' @export
#' @rdname checkCacheRepo
setMethod(
  ".checkCacheRepo",
  signature = "ANY",
  definition = function(object) {
    stop("must supply a cacheRepo argument")
})

################################################################################
#' Make any modifications to object recovered from cacheRepo
#'
#' This is a generic definition that can be extended according to class.
#'
#' @inheritParams Cache
#'
#' @param object Any R object
#'
#' @return The object, modified
#'
#' @author Eliot McIntire
#' @docType methods
#' @export
#' @importFrom archivist showLocalRepo rmFromLocalRepo
#' @rdname prepareOutput
#'
setGeneric(".prepareOutput", function(object, cacheRepo, ...) {
  standardGeneric(".prepareOutput")
})

#' @export
#' @rdname prepareOutput
setMethod(
  ".prepareOutput",
  signature = "RasterLayer",
  definition = function(object, cacheRepo, ...) {
    prepareFileBackedRaster(object, repoDir = cacheRepo)
})

#' @export
#' @rdname prepareOutput
setMethod(
  ".prepareOutput",
  signature = "ANY",
  definition = function(object, cacheRepo, ...) {
    if (is.character(object)) {
      if (length(object) == 1) {
        # need something to attach tags to if it is actually NULL
        if (object == "Null") object <- NULL
      }
    }
    object
})

#' A set of helpers for Cache
#'
#' These are internal only.
#'
#' @param FUN A function
#' @param ... passing the ... from outer function, which will include potential
#'        arguments to the FUN
#' @param overrideCall A character string indicating a different (not "Cache") function
#'        name to search for. Mostly so that this works with deprecated "cache".
#' @note If the function cannot figure out a clean function name, it returns "internal"
#'
#' @author Eliot Mcintire
#' @docType methods
#' @importFrom methods selectMethod showMethods
#' @keywords internal
#' @rdname cacheHelper
#'
getFunctionName <- function(FUN, ..., overrideCall) { # nolint
  if (isS4(FUN)) {
    # Have to extract the correct dispatched method
    firstElems <- strsplit(showMethods(FUN, inherited = TRUE, printTo = FALSE), split = ", ")
    firstElems <- lapply(firstElems, function(x) {
      y <- strsplit(x, split = "=")
      unlist(lapply(y, function(z) z[1]))
    })
    firstElems <- firstElems[!unlist(lapply(firstElems, is.null))] # remove nulls
    firstElems <- firstElems[!unlist(lapply(firstElems, function(x) {
      any(grepl(x, pattern = "inherited")) # remove "nulls" inherited
    }))]
    firstElems <- firstElems[!unlist(lapply(firstElems, function(x) {
      any(grepl(x, pattern = "\\(inherited")) # remove "nulls" inherited
    }))]
    firstElems <- firstElems[!unlist(lapply(firstElems, function(x) {
      any(grepl(x, pattern = "^Function:"))  # remove "nulls" inherited
    }))]

    sigArgs <- lapply(unique(firstElems), function(x) {
      FUN@signature %in% x
    })
    signat <- unlist(sigArgs[unlist(lapply(sigArgs, function(y) any(y)))])

    matchedCall <- as.list(
      match.call(FUN, do.call(call, append(list(name = FUN@generic),
                                           list(...)))))
    matchedCall <- matchedCall[nzchar(names(matchedCall))]
    matchedCall <- matchedCall[na.omit(match(names(matchedCall), FUN@signature[signat]))]

    signatures <- rep("missing", (sum(signat))) # default is "missing"
    names(signatures) <- FUN@signature[signat]
    classMatchedCall <- sapply(matchedCall, class)

    # update "missing" with ones that aren't missing
    signatures[names(classMatchedCall)] <- classMatchedCall

    ## TO DO: need to get the method the dispatch correct
    methodUsed <- selectMethod(FUN, optional = TRUE, signature = signatures)
    .FUN <- methodUsed@.Data  # nolint
    functionName <- FUN@generic
  } else {
    if (!missing(overrideCall)) {
      functionCall <- grep(sys.calls(), pattern = paste0("^", overrideCall), value = TRUE)
    } else {
      functionCall <- grep(sys.calls(),
                           pattern = "^Cache|^SpaDES::Cache|^reproducible::Cache", value = TRUE)
    }
    if (length(functionCall)) {
      # for() loop is a work around for R-devel that produces a different final call in the
      # sys.calls() stack which is NOT .Method ... and produces a Cache(FUN = FUN...)
      for (fns in rev(functionCall)) {
        if (!missing(overrideCall)) {
          matchedCall <- match.call(get(overrideCall), parse(text = fns))
          functionName <- matchedCall$FUN
        } else {
          matchedCall <- match.call(Cache, parse(text = fns))
          functionName <- matchedCall$FUN
        }
        functionName <- deparse(functionName)
        if (functionName != "FUN") break
      }
    } else {
      functionName <- ""
    }
    .FUN <- FUN  # nolint
  }
  .FUN <- format(FUN)  # nolint

  # if it can't deduce clean name (i.e., still has a "(" in it), return "internal"
  if (isTRUE(grepl(functionName, pattern = "\\(")))
    functionName <- "internal"

  return(list(functionName = functionName, .FUN = .FUN))
}

#' @exportClass Path
#' @rdname Path-class
setClass("Path", slots = c(.Data = "character"), contains = "character",
         prototype = NA_character_)

#' Coerce a character string to a class "Path"
#'
#' It is often difficult to impossible do know algorithmically whether a
#' character string is a valid path. In the case where it is en existing
#' file, \code{file.exists} can work. But if it is not yet existing, e.g.,
#' for a \code{save}, it is difficult to know if it is a valid path.
#' This allows a user to specify that their character string is indeed
#' a file path. Thus, methods that require only a file path can be
#' dispatched correctly.
#' @export
#' @rdname Path-class
#' @param obj A character string to convert to a Path
asPath <- function(obj) {
  UseMethod("asPath", obj)
}

#' @export
#' @importFrom methods is
#' @rdname Path-class
asPath.character <- function(obj) {  # nolint
  class(obj) <- c("Path", is(obj))
  return(obj)
}

#' @export
#' @importFrom methods new
setAs(from = "character", to = "Path", function(from) {
  new("Path", from)
})
