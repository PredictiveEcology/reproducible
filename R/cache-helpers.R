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
#' @export
#' @rdname tagsByClass
#' @examples
#' .tagsByClass(character()) # Nothing interesting. Other packages will make methods
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
#' @param fromMemoise Logical. If \code{TRUE}, the message will be about
#'        recovery from memoised copy
#'
#' @author Eliot McIntire
#' @export
#' @rdname cacheMessage
#' @examples
#' a <- 1
#' .cacheMessage(a, "mean")
#'
setGeneric(".cacheMessage", function(object, functionName,
                                     fromMemoise = getOption("reproducible.useMemoise", TRUE)) {
  standardGeneric(".cacheMessage")
})

#' @export
#' @rdname cacheMessage
setMethod(
  ".cacheMessage",
  signature = "ANY",
  definition = function(object, functionName,
                        fromMemoise) {
    if (isTRUE(fromMemoise)) {
      message(crayon::blue("  loading memoised result from previous ", functionName, " call.",
                           sep = ""))
    } else if (!is.na(fromMemoise)) {
      message(crayon::blue("  loading cached result from previous ", functionName, " call, ",
                           "adding to memoised copy", sep = ""))
    } else {
      message(crayon::blue("  loading cached result from previous ", functionName, " call.",
                           sep = ""))
    }
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
#' @author Eliot McIntire
#' @export
#' @rdname objSizeInclEnviros
#' @examples
#' a <- new.env()
#' a$b <- 1:10
#' object.size(a)
#' .objSizeInclEnviros(a) # much larger
#'
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
#' @param preDigestByClass A list, usually from \code{.preDigestByClass}
#'
#' @return New object with tags attached.
#'
#' @author Eliot McIntire
#' @export
#' @rdname addTagsToOutput
#'
setGeneric(".addTagsToOutput", function(object, outputObjects, FUN, preDigestByClass) { # nolint
  standardGeneric(".addTagsToOutput")
})

#' @export
#' @rdname addTagsToOutput
setMethod(
  ".addTagsToOutput",
  signature = "ANY",
  definition = function(object, outputObjects, FUN, preDigestByClass) { # nolint
    object
})

################################################################################
#' Any miscellaneous things to do before \code{.robustDigest} and after \code{FUN} call
#'
#' The default method for \code{preDigestByClass} and simply returns \code{NULL}.
#' There may be methods in other packages.
#'
#' @inheritParams Cache
#'
#' @param object Any R object.
#'
#' @return A list with elements that will likely be used in \code{.postProcessing}
#'
#' @author Eliot McIntire
#' @export
#' @rdname preDigestByClass
#' @examples
#' a <- 1
#' .preDigestByClass(a) # returns NULL in the simple case here.
#'
setGeneric(".preDigestByClass", function(object) { # nolint
  standardGeneric(".preDigestByClass")
})

#' @export
#' @rdname preDigestByClass
setMethod(
  ".preDigestByClass",
  signature = "ANY",
  definition = function(object) { # nolint
    NULL
  })


################################################################################
#' Check for cache repository info in ...
#'
#' This is a generic definition that can be extended according to class.
#' Normally, \code{checkPath} can be called directly, but does not have class-specific methods.
#'
#' @param object An R object
#' @param create Logical. If TRUE, then it will create the path for cache.
#'
#' @return A character string with a path to a cache repository.
#'
#' @author Eliot McIntire
#' @export
#' @importFrom archivist showLocalRepo rmFromLocalRepo
#' @rdname checkCacheRepo
#' @examples
#' a <- "test"
#' .checkCacheRepo(a) # no cache repository supplied
#'
setGeneric(".checkCacheRepo", function(object, create = FALSE) {
  standardGeneric(".checkCacheRepo")
})

#' @export
#' @rdname checkCacheRepo
setMethod(
  ".checkCacheRepo",
  signature = "ANY",
  definition = function(object, create) {
    cacheRepo <- tryCatch(checkPath(object, create), error = function(x) {
      cacheRepo <- if (isTRUE(nzchar(getOption("reproducible.cachePath")))) {
        message("No cacheRepo supplied. Using value in getOption('reproducible.cachePath')")
        getOption("reproducible.cachePath", tempdir())
      } else {
        message("No cacheRepo supplied. Using tempdir()")
        tempdir()
      }
      checkPath(path = cacheRepo, create = create)
    })
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
#' @export
#' @importFrom archivist showLocalRepo rmFromLocalRepo
#' @rdname prepareOutput
#' @examples
#' a <- 1
#' .prepareOutput(a) # does nothing
#'
#' b <- "Null"
#' .prepareOutput(b) # converts to NULL
#'
#' # For rasters, it is same as .prepareFileBackedRaster
#' try(archivist::createLocalRepo(tempdir()))
#'
#' library(raster)
#' r <- raster(extent(0,10,0,10), vals = 1:100)
#'
#' # write to disk manually -- will be in tempdir()
#' r <- writeRaster(r, file = tempfile())
#'
#' # copy it to the cache repository
#' r <- .prepareOutput(r, tempdir())
setGeneric(".prepareOutput", function(object, cacheRepo, ...) {
  standardGeneric(".prepareOutput")
})

#' @export
#' @rdname prepareOutput
setMethod(
  ".prepareOutput",
  signature = "RasterLayer",
  definition = function(object, cacheRepo, ...) {
    # with this call to .prepareFileBackedRaster, it is from the same function call as a previous time
    #  overwrite is ok
    .prepareFileBackedRaster(object, repoDir = cacheRepo, ...)
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


#####################################
################################################################################
#' Add an attribute to an object indicating which named elements change
#'
#' This is a generic definition that can be extended according to class.
#'
#' @param object Any R object returned from a function
#' @param preDigest The full, element by element hash of the input arguments to that same function,
#' e.g., from \code{.robustDigest}
#' @param origArguments These are the actual arguments (i.e., the values, not the names) that
#'        were the source for \code{preDigest}
#' @param ... Anything passed to methods.
#'
#' @return The object, modified
#'
#' @author Eliot McIntire
#' @export
#' @importFrom archivist showLocalRepo rmFromLocalRepo
#' @rdname addChangedAttr
#' @examples
#' a <- 1
#' .addChangedAttr(a) # does nothing because default method is just a pass through
setGeneric(".addChangedAttr", function(object, preDigest, origArguments, ...) {
  standardGeneric(".addChangedAttr")
})

#' @export
#' @rdname addChangedAttr
setMethod(
  ".addChangedAttr",
  signature = "ANY",
  definition = function(object, preDigest, origArguments, ...) {
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
#' @param isPipe Logical. If the call to getFunctionName is coming from a pipe, there is more
#'               information available. Specifically, ._lhs which is already a call.
#' @note If the function cannot figure out a clean function name, it returns "internal"
#'
#' @author Eliot McIntire
#' @importFrom methods selectMethod showMethods
#' @keywords internal
#' @rdname cacheHelper
getFunctionName <- function(FUN, originalDots, ...,
                            overrideCall, isPipe) { # nolint
  callIndex <- numeric()
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

    if (isPipe) {
      matchedCall <- as.list(
        match.call(FUN, list(...)$._lhs) # already a call
      )
    } else {
      matchedCall <- as.list(
        match.call(FUN, call(name = FUN@generic, list(...)))
      )
    }

    matchedCall <- matchedCall[nzchar(names(matchedCall))]
    matchedCall <- matchedCall[na.omit(match(names(matchedCall), FUN@signature[signat]))]
    matchedCall <- lapply(matchedCall, eval)

    signatures <- rep("missing", (sum(signat))) # default is "missing"
    names(signatures) <- FUN@signature[signat]
    classMatchedCall <- sapply(matchedCall, class)

    # update "missing" with ones that aren't missing
    signatures[names(classMatchedCall)] <- classMatchedCall

    ## TO DO: need to get the method the dispatch correct
    methodUsed <- selectMethod(FUN, optional = TRUE, signature = signatures)
    functionName <- FUN@generic
    FUN <- methodUsed@.Data  # nolint
  } else {
    scalls <- sys.calls()
    if (!missing(overrideCall)) {
      callIndices <- .grepSysCalls(scalls, pattern = paste0("^", overrideCall))
      functionCall <- scalls[callIndices]
    } else {
      callIndices <- .grepSysCalls(scalls, pattern = "^Cache|^SpaDES::Cache|^reproducible::Cache")
      # The next line takes too long to grep if scalls has enormous objects
      # callIndices <- grep(scalls, pattern = "^Cache|^SpaDES::Cache|^reproducible::Cache")
      functionCall <- scalls[callIndices]
    }
    if (length(functionCall)) {
      # for() loop is a work around for R-devel that produces a different final call in the
      # sys.calls() stack which is NOT .Method ... and produces a Cache(FUN = FUN...)
      for (callIndex in rev(callIndices)) {
        if (!missing(overrideCall)) {
          env <- sys.frames()[[callIndices]]
          matchedCall <- match.call(get(overrideCall, envir = env), scalls[[callIndex]])#parse(text = callIndex))
          forms <- tryCatch("FUN" %in% formalArgs(overrideCall), error = function(x) NULL)
          if (!is.null(forms)) {
            functionName <- matchedCall$FUN
          } else {
            functionName <- matchedCall[[2]]
          }
        } else {
          matchedCall <- match.call(Cache, scalls[[callIndex]])#parse(text = callIndex))
          functionName <- matchedCall$FUN
        }
        functionName <- deparse(functionName, width.cutoff = 300)
        if (all(functionName != c("FUN"))) break
      }
    } else {
      functionName <- ""
    }
    .FUN <- FUN  # nolint
  }

  if (is(FUN, "function")) {
    .FUN <- format(FUN)  # nolint
  } else {
    .FUN <- NULL # nolint
  }

  # if it can't deduce clean name (i.e., still has a "(" in it), return NA
  if (isTRUE(grepl(functionName, pattern = "\\(")))
    functionName <- NA_character_

  return(list(functionName = functionName, .FUN = .FUN))#, callIndex = callIndex))
}

#' @exportClass Path
#' @rdname Path-class
setClass("Path", slots = c(.Data = "character"), contains = "character",
         prototype = NA_character_)

#' Coerce a character string to a class "Path"
#'
#' Allows a user to specify that their character string is indeed a filepath.
#' Thus, methods that require only a filepath can be dispatched correctly.
#'
#' It is often difficult or impossible to know algorithmically whether a
#' character string corresponds to a valid filepath.
#' In the case where it is en existing file, \code{file.exists} can work.
#' But if it does not yet exist, e.g., for a \code{save}, it is difficult to know
#' whether it is a valid path before attempting to save to the path.
#'
#' This function can be used to remove any ambiguity about whether a character
#' string is a path. It is primarily useful for achieving repeatability with Caching.
#' Essentially, when Caching, arguments that are character strings should generally be
#' digested verbatim, i.e., it must be an exact copy for the Cache mechanism
#' to detect a candidate for recovery from the cache.
#' Paths, are different. While they are character strings, there are many ways to
#' write the same path. Examples of identical meaning, but different character strings are:
#' path expanding of \code{~} vs. not, double back slash vs. single forward slash,
#' relative path vs. absolute path.
#' All of these should be assessed for their actual file or directory location,
#' NOT their character string. By converting all character string that are actual
#' file or directory paths with this function, then \code{Cache} will correctly assess
#' the location, NOT the character string representation.
#'
#' @param obj A character string to convert to a \code{Path}.
#' @param nParentDirs A numeric indicating the number of parent directories starting
#'                    from basename(obj) = 0 to keep for the digest
#'
#' @export
#' @rdname Path-class
#'
#' @examples
#' tmpf <- tempfile(fileext = ".csv")
#' file.exists(tmpf)    ## FALSE
#' tmpfPath <- asPath(tmpf)
#' is(tmpf, "Path")     ## FALSE
#' is(tmpfPath, "Path") ## TRUE
#'
asPath <- function(obj, nParentDirs = 0) {
  UseMethod("asPath", obj)
}

#' @export
#' @importFrom methods is
#' @rdname Path-class
asPath.character <- function(obj, nParentDirs = 0) {  # nolint
  class(obj) <- c("Path", is(obj))
  attr(obj, "nParentDirs") <- nParentDirs
  return(obj)
}

#' If using \code{as("string", "Path")}, there is no option to pass \code{nParentDirs}.
#' So, using \code{asPath} directly (e.g., \code{asPath("string", 0))}) is preferred.
#' @export
#' @importFrom methods new
#' @rdname Path-class
#' @name asPath
setAs(from = "character", to = "Path", function(from) {
  asPath(from, 0)
})

################################################################################
#' Clear erroneous archivist artifacts
#'
#' Stub artifacts can result from several causes. The most common being
#' erroneous removal of a file in the SQLite database. This can be caused
#' sometimes if an archive object is being saved multiple times by multiple
#' threads. This function will clear entries in the SQLite database which
#' have no actual file with data.
#'
#' @return Invoked for its side effect on the \code{repoDir}.
#'
#' @param repoDir A character denoting an existing directory of the repository for
#' which metadata will be returned. If \code{NULL} (default), it will use the
#' \code{repoDir} specified in \code{archivist::setLocalRepo}.
#'
#' @author Eliot McIntire
#' @export
#' @importFrom archivist showLocalRepo rmFromLocalRepo
#' @rdname clearStubArtifacts
#'
#' @examples
#' tmpDir <- file.path(tempdir(), "reproducible_examples", "clearStubArtifacts")
#'
#' lapply(c(runif, rnorm), function(f) {
#'   reproducible::Cache(f, 10, cacheRepo = tmpDir)
#' })
#'
#' # clear out any stub artifacts
#' showCache(tmpDir)
#'
#' file2Remove <- dir(file.path(tmpDir, "gallery"), full.name = TRUE)[1]
#' file.remove(file2Remove)
#' showCache(tmpDir) # repository directory still thinks files are there
#'
#' # run clearStubArtifacts
#' suppressWarnings(clearStubArtifacts(tmpDir))
#' showCache(tmpDir) # stubs are removed
#'
#' # cleanup
#' clearCache(tmpDir)
#' unlink(tmpDir, recursive = TRUE)
#'
setGeneric("clearStubArtifacts", function(repoDir = NULL) {
  standardGeneric("clearStubArtifacts")
})

#' @export
#' @rdname clearStubArtifacts
setMethod(
  "clearStubArtifacts",
  definition = function(repoDir) {
    md5hashInBackpack <- showLocalRepo(repoDir = repoDir)$md5hash
    listFiles <- dir(file.path(repoDir, "gallery")) %>%
      strsplit(".rda") %>%
      unlist()
    toRemove <- !(md5hashInBackpack %in% listFiles)
    md5hashInBackpack[toRemove] %>%
      sapply(., rmFromLocalRepo, repoDir = repoDir)
    return(invisible(md5hashInBackpack[toRemove]))
})

#' Copy the file-backing of a file-backed Raster* object
#'
#' Rasters are sometimes file-based, so the normal save and copy and assign
#' mechanisms in R don't work for saving, copying and assigning.
#' This function creates an explicit file copy of the file that is backing the raster,
#' and changes the pointer (i.e., \code{filename(object)}) so that it is pointing
#' to the new file.
#'
#' @param obj The raster object to save to the repository.
#'
#' @inheritParams Cache
#'
#' @param repoDir Character denoting an existing directory in which an artifact will be saved.
#'
#' @param overwrite Logical. Should the raster be saved to disk, overwriting existing file.
#'
#' @param ... passed to \code{archivist::saveToRepo}
#'
#' @return A raster object and its newly located file backing.
#'         Note that if this is a legitimate archivist repository, the new location
#'         will be a subdirectory called \file{rasters/} of \file{repoDir/}.
#'         If this is not a repository, the new location will be within \code{repoDir}.
#'
#' @author Eliot McIntire
#' @export
#' @importFrom digest digest
#' @importFrom raster filename dataType inMemory nlayers writeRaster
#' @importFrom methods is selectMethod slot slot<-
#' @rdname prepareFileBackedRaster
#' @examples
#' library(raster)
#' archivist::createLocalRepo(tempdir())
#'
#' r <- raster(extent(0,10,0,10), vals = 1:100)
#'
#' # write to disk manually -- will be in tempdir()
#' r <- writeRaster(r, file = tempfile())
#'
#' # copy it to the cache repository
#' r <- .prepareFileBackedRaster(r, tempdir())
#'
#' r # now in "rasters" subfolder of tempdir()
#'
#'
.prepareFileBackedRaster <- function(obj, repoDir = NULL, overwrite = FALSE, ...) {
  isRasterLayer <- TRUE
  isStack <- is(obj, "RasterStack")
  repoDir <- checkPath(repoDir, create = TRUE)
  isRepo <- all(c("backpack.db", "gallery") %in% list.files(repoDir))

  if (inMemory(obj)) {
    isFilebacked <- FALSE
    if (isTRUE(any(raster::is.factor(obj)))) {
      fileExt <- ".grd"
    } else {
      fileExt <- ".tif"
    }
    curFilename <- basename(tempfile(pattern = "raster", fileext = fileExt, tmpdir = ""))
  } else {
    isFilebacked <- TRUE
    if (is(obj, "RasterLayer") || is(obj, "RasterBrick")) {
      curFilename <- normalizePath(filename(obj), winslash = "/", mustWork = FALSE)
    } else  {
      curFilenames <- unlist(lapply(obj@layers, function(x)
        normalizePath(filename(x), winslash = "/", mustWork = FALSE)))
      curFilename <- unique(curFilenames)
    }
  }

  if (any(!file.exists(curFilename)) & isFilebacked & isRasterLayer) {

    # File is in wrong folder, usually the result of a copy of cache bewteen 2 machines
    splittedFilenames <- strsplit(curFilename, split = basename(repoDir))
    trySaveFilename <- if (length(splittedFilenames) == 1) {
      normalizePath(
        file.path(repoDir, splittedFilenames[[1]][[length(splittedFilenames[[1]])]]),
        winslash = "/", mustWork = FALSE)
    } else {
      splittedFilenames2 <- lapply(splittedFilenames, function(x) {
        ifelse(length(x), x[length(x)], "")
      })

      normalizePath(file.path(repoDir, splittedFilenames2), winslash = "/", mustWork = FALSE)
    }
    if (any(!file.exists(trySaveFilename))) {
      stop("The following file-backed rasters are supposed to be on disk ",
           "but appear to have been deleted:\n",
           paste("    ", curFilename, collapse = "\n"),
           ". The most likely reason is that two functions had the same output ",
           "and one of them was removed with clearCache(...). ",
           "The best solution to this is never have two functions create the same ",
           "file-backed raster.")
    } else {
      slot(slot(obj, "file"), "name") <- saveFilename <- curFilename <- trySaveFilename
    }
  } else {
    saveFilename <- if (isRepo) {
      file.path(repoDir, "rasters", basename(curFilename))
    } else {
      file.path(repoDir, basename(curFilename))
    }

    saveFilename <- normalizePath(saveFilename, winslash = "/", mustWork = FALSE)
  }

  sameFilenames <- saveFilename == curFilename
  if (any(sameFilenames)) {
    if (!overwrite) {
      saveFilename[sameFilenames] <- unlist(lapply(seq_along(curFilename[sameFilenames]),
             function(x) {
               if (file.exists(saveFilename[x])) {
                 nextNumericName(saveFilename[x])
               } else {
                 saveFilename[x]
               }
             }))
    }
  }
  # filenames are not the same
  if (any(saveFilename != curFilename)) {
    if (isFilebacked) {
      shouldCopy <- rep(TRUE, length(curFilename))
      if (any(shouldCopy)) {
        pathExists <- dir.exists(dirname(saveFilename))
        if (any(!pathExists)) {
          dirname(saveFilename) %>%
            unique() %>%
            sapply(., dir.create, recursive = TRUE)
        }

        if (any(saveFilename %>% grepl(., pattern = "[.]grd$"))) {
          copyFile(from = curFilename, to = saveFilename, overwrite = TRUE, silent = TRUE)
          griFilename <- sub(saveFilename, pattern = "[.]grd$", replacement = ".gri")
          curGriFilename <- sub(curFilename, pattern = "[.]grd$", replacement = ".gri")
          copyFile(from = curGriFilename, to = griFilename, overwrite = TRUE, silent = TRUE)
        } else {
          saveFilename <- unlist(lapply(seq_along(curFilename),
                                        function(x) {
                                          # change filename if it already exists
                                          if (file.exists(saveFilename[x])) {
                                            saveFilename[x] <- nextNumericName(saveFilename[x])
                                          }
                                          copyFile(to = saveFilename[x],
                                                   overwrite = TRUE,
                                                   from = curFilename[x], silent = TRUE)
                                        }))
        }
      }
      # for a stack with independent Raster Layers (each with own file)
      if (length(curFilename) > 1) {
        for (i in seq_along(curFilename)) {
          slot(slot(slot(obj, "layers")[[i]], "file"), "name") <- saveFilename[i]
        }
      } else {
        if (!isStack) {
          slot(slot(obj, "file"), "name") <- saveFilename
        } else {
          for (i in seq_len(nlayers(obj))) {
            whFilename <- match(basename(saveFilename), basename(curFilenames))
            slot(slot(obj@layers[[i]], "file"), "name") <- saveFilename[whFilename]
          }
        }
      }
    } else {
      checkPath(dirname(saveFilename), create = TRUE) #SpaDES dependency
      if (!inMemory(obj)) {
        obj <- writeRaster(obj, filename = saveFilename, datatype = dataType(obj))
      }
    }
  }

  return(obj)
}


#' Copy a file using \code{robocopy} on Windows and \code{rsync} on Linux/macOS
#'
#' This is replacement for \code{file.copy}, but for one file at a time.
#' The additional feature is that it will use \code{robocopy} (on Windows) or
#' \code{rsync} on Linux or Mac, if they exist.
#' It will default back to \code{file.copy} if none of these exists.
#' If there is a possibility that the file already exists, then this function
#' should be very fast as it will do "update only", i.e., nothing.
#'
#' @param from The source file.
#'
#' @param to The new file.
#'
#' @param useRobocopy For Windows, this will use a system call to \code{robocopy}
#'        which appears to be much faster than the internal \code{file.copy} function.
#'        Uses \code{/MIR} flag. Default \code{TRUE}.
#'
#' @param overwrite Passed to \code{file.copy}
#'
#' @param delDestination Logical, whether the destination should have any files deleted,
#' if they don't exist in the source. This is \code{/purge} for robocopy and --delete for
#' rsync.
#'
#' @param create Passed to \code{checkPath}.
#'
#' @param silent Should a progress be printed.
#'
#' @inheritParams base::file.copy
#'
#' @author Eliot McIntire and Alex Chubaty
#' @export
#' @rdname copyFile
#'
#' @examples
#' tmpDirFrom <- file.path(tempdir(), "example_fileCopy_from")
#' tmpDirTo <- file.path(tempdir(), "example_fileCopy_to")
#' tmpFile <- tempfile("file", tmpDirFrom, ".csv")
#' dir.create(tmpDirFrom)
#' f1 <- normalizePath(tmpFile, mustWork = FALSE)
#' f2 <- normalizePath(file.path(tmpDirTo, basename(tmpFile)), mustWork = FALSE)
#'
#' write.csv(data.frame(a = 1:10, b = runif(10), c = letters[1:10]), f1)
#' copyFile(f1, f2)
#' file.exists(f2) ## TRUE
#' identical(read.csv(f1), read.csv(f2)) ## TRUE
#'
#' unlink(tmpDirFrom, recursive = TRUE)
#' unlink(tmpDirTo, recursive = TRUE)
#'
copyFile <- function(from = NULL, to = NULL, useRobocopy = TRUE,
                     overwrite = TRUE, delDestination = FALSE,
                     #copyRasterFile = TRUE, clearRepo = TRUE,
                     create = TRUE, silent = FALSE) {
  origDir <- getwd()
  useFileCopy <- identical(dirname(from), dirname(to))

  lapply(unique(dirname(to)), checkPath, create = create)

  os <- tolower(Sys.info()[["sysname"]])
  .onLinux <- .Platform$OS.type == "unix" && unname(os) == "linux"
  if (!useFileCopy) {
    if (os == "windows") {
      if (!isTRUE(unique(dir.exists(to)))) toDir <- dirname(to) # extract just the directory part
      robocopyBin <- tryCatch(Sys.which("robocopy"), warning = function(w) NA_character_)

      robocopy <-  if (silent) {
        paste0(robocopyBin, " /purge"[delDestination], " /ETA /XJ /XO /NDL /NFL /NJH /NJS \"",  # nolint
               normalizePath(dirname(from), mustWork = TRUE, winslash = "\\"), "\" \"",
               normalizePath(toDir, mustWork = FALSE, winslash = "\\"),  "\" ",
               basename(from))
      } else {
        paste0(robocopyBin, " /purge"[delDestination], " /ETA /XJ /XO \"", # nolint
               normalizePath(dirname(from), mustWork = TRUE, winslash = "\\"), "\" \"",
               normalizePath(toDir, mustWork = FALSE, winslash = "\\"), "\" ",
               basename(from))
      }

      useFileCopy <- if (useRobocopy && !is.na(robocopyBin)) {
        suppressWarnings(tryCatch(system(robocopy, intern = TRUE), error = function(x) TRUE))
      } else {
        TRUE
      }
      if (isTRUE(any(grepl("ERROR", useFileCopy)))) {
        useFileCopy <- TRUE
      }

      # means that the file didn't get copied because it is actually the same directory
      if (any(!nzchar(useFileCopy))) {
        useFileCopy <- TRUE
      }
    } else if ( (.onLinux) ) { # nolint
      if (!identical(basename(from), basename(to))) {
        # rsync can't handle file renaming on copy
        useFileCopy <- TRUE
      } else {
        if (!dir.exists(to)) toDir <- dirname(to) # extract just the directory part
        rsyncBin <- tryCatch(Sys.which("rsync"), warning = function(w) NA_character_)
        opts <- if (silent) " -a " else " -avP "
        rsync <- paste0(rsyncBin, " ", opts, " --delete "[delDestination],
                        normalizePath(from, mustWork = TRUE), " ",
                        normalizePath(toDir, mustWork = FALSE), "/")

        useFileCopy <- tryCatch(system(rsync, intern = TRUE), error = function(x) TRUE)
      }
    } else {
      useFileCopy <- TRUE
    }
  }
  if (isTRUE(useFileCopy)) {
    lapply(unique(dirname(to)), checkPath, create = create)
    file.copy(from = from, to = to, overwrite = overwrite, recursive = FALSE)
  }

  setwd(origDir)
  return(invisible(to))
}

#' @rdname cacheHelper
#' @importFrom fastdigest fastdigest
#' @importFrom methods slotNames
#' @importFrom digest digest
#' @importFrom raster res crs extent
.digestRasterLayer <- function(object, length, algo, quick) {
  # metadata -- only a few items of the long list because one thing (I don't recall)
  #  doesn't cache consistently
  sn <- slotNames(object@data)
  sn <- sn[!(sn %in% c("min", "max", "haveminmax", "names", "isfactor",
                       "dropped", "nlayers", "fromdisk", "inmemory", "offset", "gain"))]
  dataSlotsToDigest <- lapply(sn, function(s) slot(object@data, s))
  dig <- fastdigest(append(list(dim(object), res(object), crs(object),
                         extent(object)), dataSlotsToDigest)) # don't include object@data -- these are volatile
  if (nzchar(object@file@name)) {
    # if the Raster is on disk, has the first length characters;
    filename <- if (isTRUE(endsWith(basename(object@file@name), suffix = ".grd"))) {
      sub(object@file@name, pattern = ".grd$", replacement = ".gri")
    } else {
      object@file@name
    }
    # there is no good reason to use depth = 0, 1, or 2 or more -- but I think 2 is *more* reliable
    dig2 <- .robustDigest(asPath(filename, 2), length = length, quick = quick, algo = algo)
    dig <- c(dig, dig2)
  }
  dig <- fastdigest(dig)
}

#' Recursive copying of nested environments, and other "hard to copy" objects
#'
#' When copying environments and all the objects contained within them, there are
#' no copies made: it is a pass-by-reference operation. Sometimes, a deep copy is
#' needed, and sometimes, this must be recursive (i.e., environments inside
#' environments).
#'
#' @param object  An R object (likely containing environments) or an environment.
#'
#' @param filebackedDir A directory to copy any files that are backing R objects,
#'                      currently only valid for \code{Raster} classes. Defaults
#'                      to \code{tempdir()}, which is unlikely to be very useful.
#'
#' @param ... Only used for custom Methods
#'
#' @author Eliot McIntire
#' @export
#' @importFrom data.table copy
#' @rdname Copy
#' @seealso \code{\link{.robustDigest}}
#'
#' @examples
#' e <- new.env()
#' e$abc <- letters
#' e$one <- 1L
#' e$lst <- list(W = 1:10, X = runif(10), Y = rnorm(10), Z = LETTERS[1:10])
#' ls(e)
#'
#' # 'normal' copy
#' f <- e
#' ls(f)
#' f$one
#' f$one <- 2L
#' f$one
#' e$one ## uh oh, e has changed!
#'
#' # deep copy
#' e$one <- 1L
#' g <- Copy(e)
#' ls(g)
#' g$one
#' g$one <- 3L
#' g$one
#' f$one
#' e$one
#'
setGeneric("Copy", function(object, filebackedDir = tempdir(), ...) {
  standardGeneric("Copy")
})

#' @rdname Copy
setMethod(
  "Copy",
  signature(object = "ANY"),
  definition = function(object, filebackedDir, ...) {
    return(object)
})

#' @rdname Copy
setMethod("Copy",
          signature(object = "data.table"),
          definition = function(object, ...) {
            data.table::copy(object)
})

#' @rdname Copy
setMethod("Copy",
          signature(object = "environment"),
          definition = function(object,  filebackedDir, ...) {
            listVersion <- Copy(as.list(object, all.names = TRUE),  filebackedDir, ...)
            as.environment(listVersion)
})

#' @rdname Copy
setMethod("Copy",
          signature(object = "list"),
          definition = function(object,  filebackedDir, ...) {
            lapply(object, function(x) Copy(x, filebackedDir, ...))
})

#' @rdname Copy
setMethod("Copy",
          signature(object = "data.frame"),
          definition = function(object,  filebackedDir, ...) {
            object
})

#' @rdname Copy
setMethod("Copy",
          signature(object = "Raster"),
          definition = function(object, filebackedDir, ...) {
            object <- .prepareFileBackedRaster(object, repoDir = filebackedDir)
})

################################################################################
#' Sort or order any named object with dotted names and underscores first
#'
#' Internal use only. This exists so Windows, Linux, and Mac machines can have
#' the same order after a sort. It will put dots and underscores first
#' (with the sort key based on their second character, see examples.
#' It also sorts lower case before upper case.
#'
#' @param obj  An arbitrary R object for which a \code{names} function
#'              returns a character vector.
#'
#' @return The same object as \code{obj}, but sorted with .objects first.
#'
#' @author Eliot McIntire
#' @export
#' @rdname sortDotsUnderscoreFirst
#'
#' @examples
#' items <- c(A = "a", Z = "z", `.D` = ".d", `_C` = "_C")
#' .sortDotsUnderscoreFirst(items)
#'
#' # dots & underscore (using 2nd character), then all lower then all upper
#' items <- c(B = "Upper", b = "lower", A = "a", `.D` = ".d", `_C` = "_C")
#' .sortDotsUnderscoreFirst(items)
#'
#' # with a vector
#' .sortDotsUnderscoreFirst(c(".C", "_B", "A")) # _B is first
#'
.sortDotsUnderscoreFirst <- function(obj) {
  obj[.orderDotsUnderscoreFirst(obj)]
}

#' @export
#' @rdname sortDotsUnderscoreFirst
.orderDotsUnderscoreFirst <- function(obj) {

  if (!is.null(names(obj))) {
    namesObj <- names(obj)
  } else {
    namesObj <- obj
  }

  namesObj <- gsub(namesObj, pattern = "\\.|_", replacement = "aa")
  allLower <- tolower(namesObj) == namesObj
  namesObj[allLower] <- paste0("abALLLOWER", namesObj[allLower])

  onesChanged <- startsWith(namesObj, prefix = "a")
  namesObj[!onesChanged] <- paste0("ZZZZZZZZZ", namesObj[!onesChanged])

  order(namesObj)
}

################################################################################
#' Attach debug info to return for Cache
#'
#' Internal use only. Attaches an attribute to the output, usable for
#' debugging the Cache.
#'
#' @param obj  An arbitrary R object.
#' @param preDigest  A list of hashes.
#' @param ...  Dots passed from Cache
#'
#' @return The same object as \code{obj}, but with 2 attributes set.
#'
#' @author Eliot McIntire
#' @importFrom data.table setattr
#' @rdname debugCache
#'
.debugCache <- function(obj, preDigest, ...) {
  setattr(obj, "debugCache1", list(...))
  setattr(obj, "debugCache2", preDigest)
  obj
}

# loadFromLocalRepoMem <- memoise::memoise(loadFromLocalRepo)

.getOtherFnNamesAndTags <- function(scalls) {
  if (is.null(scalls)) {
    scalls <- sys.calls()
  }

  otherFns <- .grepSysCalls(scalls, pattern = paste0("(test_code)|(with_reporter)|(force)|",
                                             "(eval)|(::)|(\\$)|(\\.\\.)|(standardGeneric)|",
                                             "(Cache)|(tryCatch)|(doTryCatch)"))
  if (length(otherFns)) {
    otherFns <- unlist(lapply(scalls[-otherFns], function(x) {
      tryCatch(as.character(x[[1]]), error = function(y) "")
    }))
    otherFns <- otherFns[nzchar(otherFns)]
    otherFns <- otherFns[!startsWith(otherFns, prefix = ".")]
    otherFns <- paste0("otherFunctions:", otherFns)
  } else {
    otherFns <- character()
  }

  # Figure out if it is in a .parseModule call, if yes, then extract the module
  doEventFrameNum <- .grepSysCalls(scalls, "\\.parseModule")
  #doEventFrameNum <- which(startsWith(as.character(scalls), prefix = ".parseModule"))
  if (length(doEventFrameNum)) {
    module <- get("m", envir = sys.frame(doEventFrameNum[2])) # always 2
    otherFns <- c(paste0("module:", module), otherFns)
  }
  unique(otherFns)
}

#' @importFrom tools file_path_sans_ext file_ext
nextNumericName <- function(string) {
  theExt <- file_ext(string)
  saveFilenameSansExt <- file_path_sans_ext(string)
  finalNumericPattern <- "_[[:digit:]]*$"
  allSimilarFilesInDir <- dir(dirname(saveFilenameSansExt), pattern = basename(saveFilenameSansExt))
  allSimilarFilesInDirSansExt <- if (length(allSimilarFilesInDir) == 0) {
    unique(saveFilenameSansExt)
  } else {
    unique(file_path_sans_ext(allSimilarFilesInDir))
  }
  alreadyHasNumeric <- grepl(allSimilarFilesInDirSansExt, pattern = finalNumericPattern)
  if (isTRUE(any(alreadyHasNumeric))) {
    splits <- strsplit(allSimilarFilesInDirSansExt[alreadyHasNumeric], split = "_")
    highestNumber <- max(unlist(lapply(splits, function(split) as.numeric(tail(split,1)))),
                         na.rm = TRUE)
    preNumeric <- unique(unlist(lapply(splits, function(spl) paste(spl[-length(spl)], collapse = "_"))))

    out <- paste0(dirname(saveFilenameSansExt), preNumeric, "_", highestNumber + 1) # keep rndstr in here, so that both streams keep same rnd number state
  } else {
    out <- paste0(saveFilenameSansExt, "_1")
  }

  paste0(out, ".", theExt)
}

#' Internal function
#'
#' A faster way of grepping the system call stack than just
#' \code{grep(sys.calls(), pattern = "test")}
#'
#' @keywords internal
#' @export
#' @rdname grepSysCalls
#' @param sysCalls The return from sys.calls()
#' @param pattern Character, passed to grep
#' @return
#' Numeric vector, equivalent to return from \code{grep(sys.calls(), pattern = "test")},
#' but faster if \code{sys.calls()} is very big.
.grepSysCalls <- function(sysCalls, pattern) {
  scallsFirstElement <- lapply(sysCalls, function(x) x[1])
  grep(scallsFirstElement, pattern = pattern)
}
