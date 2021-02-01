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
#' @inheritParams Cache
#'
#' @author Eliot McIntire
#' @export
#' @rdname cacheMessage
#' @examples
#' a <- 1
#' .cacheMessage(a, "mean")
#'
setGeneric(".cacheMessage", function(object, functionName,
                                     fromMemoise = getOption("reproducible.useMemoise", TRUE),
                                     verbose = getOption("reproducible.verbose", 1)) {
  standardGeneric(".cacheMessage")
})

#' @export
#' @rdname cacheMessage
setMethod(
  ".cacheMessage",
  signature = "ANY",
  definition = function(object, functionName, fromMemoise, verbose = getOption("reproducible.verbose", 1)) {
    if (isTRUE(fromMemoise)) {
      messageCache(.loadedCacheMsg(.loadedMemoisedResultMsg, functionName), verbose = verbose)
    } else if (!is.na(fromMemoise)) {
      messageCache(.loadedCacheMsg(.loadedCacheResultMsg, functionName),
                           "adding to memoised copy...", sep = "", verbose = verbose)
    } else {
      messageCache(.loadedCacheMsg(.loadedCacheResultMsg, functionName), verbose = verbose)
    }
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
#' @inheritParams Cache
#'
#' @return A character string with a path to a cache repository.
#'
#' @author Eliot McIntire
#' @export
#' @rdname checkCacheRepo
#' @examples
#' a <- "test"
#' .checkCacheRepo(a) # no cache repository supplied
#'
setGeneric(".checkCacheRepo", function(object, create = FALSE,
                                       verbose = getOption("reproducible.verbose", 1)) {
  standardGeneric(".checkCacheRepo")
})

#' @export
#' @rdname checkCacheRepo
setMethod(
  ".checkCacheRepo",
  signature = "ANY",
  definition = function(object, create, verbose = getOption("reproducible.verbose", 1)) {
    cacheRepo <- tryCatch(checkPath(object, create), error = function(x) {
      cacheRepo <- if (isTRUE(nzchar(getOption("reproducible.cachePath")[1]))) {
        tmpDir <- .reproducibleTempCacheDir()
        # Test whether the user has accepted the default. If yes, then give message.
        #  If no, then user is aware and doesn't need a message
        if (any(identical(normPath(tmpDir), normPath(getOption("reproducible.cachePath"))))) {
          messageCache("No cacheRepo supplied and getOption('reproducible.cachePath') is inside a temporary directory;\n",
                  "  this will not persist across R sessions.", verbose = verbose)
        }
        getOption("reproducible.cachePath", tmpDir)
      } else {
        messageCache("No cacheRepo supplied. Using ",.reproducibleTempCacheDir(), verbose = verbose)
        .reproducibleTempCacheDir()
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
#' @rdname prepareOutput
#' @examples
#' a <- 1
#' .prepareOutput(a) # does nothing
#'
#' b <- "Null"
#' .prepareOutput(b) # converts to NULL
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
#' @importFrom Require normPath
#' @importFrom RSQLite SQLite
setMethod(
  ".prepareOutput",
  signature = "Raster",
  definition = function(object, cacheRepo, drv = getOption("reproducible.drv", RSQLite::SQLite()),
                        conn = getOption("reproducible.conn", NULL), ...) {
    # with this call to .prepareFileBackedRaster, it is from the same function call as a previous time
    #  overwrite is ok
    # .prepareFileBackedRaster(object, repoDir = cacheRepo, drv = drv, conn = conn, ...)
    # browser(expr = exists("._prepareOutputs_1"))
    if (isTRUE(fromDisk(object))) {
      fns <- Filenames(object, allowMultiple = FALSE)
      fpShould <- normPath(file.path(cacheRepo, "rasters"))
      isCorrect <- unlist(lapply(normPath(file.path(fpShould, basename(fns))),
                                 function(x) any(grepl(x, fns))))
      if (!any(isCorrect)) {
        if (is(object, "RasterStack")) {
          # browser(expr = exists("._prepareOutputs_2"))
          for (i in seq(nlayers(object))) {
            object@layers[[i]]@file@name <- gsub(dirname(object@layers[[i]]@file@name),
                                                 fpShould, object@layers[[i]]@file@name)
          }
        } else {
          object@file@name <- gsub(unique(dirname(fns)), fpShould, fns)
        }
      }
    }
    object
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
#' @param ... passing the \code{...} from outer function, which will include potential
#'        arguments to the \code{FUN}
#' @param overrideCall A character string indicating a different (not "Cache") function
#'        name to search for. Mostly so that this works with deprecated "cache".
#' @param isPipe Logical. If the call to \code{getFunctionName} is coming from a pipe, there is more
#'               information available. Specifically, \code{._lhs} which is already a call.
#' @note If the function cannot figure out a clean function name, it returns "internal"
#'
#' @author Eliot McIntire
#' @importFrom methods selectMethod showMethods
#' @importFrom utils head
#' @keywords internal
#' @rdname cache-helpers
getFunctionName <- function(FUN, originalDots, ..., overrideCall, isPipe) { # nolint
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
    ff <- match(names(matchedCall), FUN@signature[signat])
    matchedCall <- matchedCall[ff[!is.na(ff)]]
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
      callIndices <- .grepSysCalls(scalls,
                                   pattern = "^Cache|^SpaDES::Cache|^reproducible::Cache|^cloudCache")
      callIndicesDoCall <- .grepSysCalls(scalls, pattern = "^do.call")
      doCall1st2Elements <- lapply(scalls[callIndicesDoCall], function(x) x[1:2])
      callIndicesDoCall <- callIndicesDoCall[grep("Cache", doCall1st2Elements)]
      # The next line takes too long to grep if scalls has enormous objects
      # callIndices <- grep(scalls, pattern = "^Cache|^SpaDES::Cache|^reproducible::Cache")
      callIndices <- unique(sort(c(callIndices, callIndicesDoCall)))
      functionCall <- scalls[callIndices]
    }
    if (length(functionCall)) {
      # for() loop is a work around for R-devel that produces a different final call in the
      # sys.calls() stack which is NOT .Method ... and produces a Cache(FUN = FUN...)
      for (callIndex in head(rev(callIndices), 2)) {
        if (!missing(overrideCall)) {
          env <- sys.frames()[[callIndex]]
          matchedCall <- match.call(get(overrideCall, envir = env), scalls[[callIndex]])#parse(text = callIndex))
          forms <- tryCatch("FUN" %in% formalArgs(overrideCall), error = function(x) NULL)
          if (!is.null(forms)) {
            functionName <- matchedCall$FUN
          } else {
            functionName <- matchedCall[[2]]
          }
        } else {
          foundCall <- FALSE
          if (exists("callIndicesDoCall", inherits = FALSE))
            if (length(callIndicesDoCall) > 0) {
              if (callIndex %in% callIndicesDoCall) {
                mcDoCall <- match.call(do.call, scalls[[callIndex]])
                for (i in 1:2) {
                  fnLookup <- try(eval(mcDoCall$args, envir = sys.frames()[[callIndex - i]]), silent = TRUE)
                  if (!is(fnLookup, "try-error"))
                    break
                }
                functionName <- if (isTRUE("FUN" %in% names(fnLookup)))
                  fnLookup$FUN
                else
                  fnLookup[[1]]

                foundCall <- TRUE
              }
            }
          if (!foundCall) {
            matchedCall <- try(match.call(Cache, scalls[[callIndex]]), silent = TRUE)#parse(text = callIndex))
            functionName <- if (!is(matchedCall, "try-error"))
              matchedCall$FUN
            else
              ""
          }
        }
        functionName <- if (is(functionName, "name")) {
          deparse(functionName, width.cutoff = 300)
        } else {
          "FUN"
        }

        if (all(functionName != c("FUN")) && all(functionName != c("NULL"))) break
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
setClass("Path", slots = c(.Data = "character"), contains = "character", prototype = NA_character_)

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

#' @export
#' @rdname Path-class
asPath.null <- function(obj, nParentDirs = 0) {  # nolint
  return(NULL)
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

# Old one
.prepareFileBackedRaster2 <- function(obj, repoDir = NULL, overwrite = FALSE,
                                     drv = getOption("reproducible.drv", RSQLite::SQLite()),
                                     conn = getOption("reproducible.conn", NULL),
                                     ...) {
  isRasterLayer <- TRUE
  isBrick <- is(obj, "RasterBrick")
  isStack <- is(obj, "RasterStack")
  repoDir <- checkPath(repoDir, create = TRUE)
  isRepo <- CacheIsACache(cachePath = repoDir, drv = drv, conn = conn)

  ## check which files are backed
  numFiles <- sum(nchar(unique(Filenames(obj)))>0)
  allInOneFile <- allInOneFile(obj)
  # if (isStack && numFiles > 0) {
  #   innerFilenames <- unlist(lapply(obj@layers, filename))
  #   allInOneFile <- isTRUE(sum(nchar(innerFilenames)>0) == length(obj@layers))
  # }

  whichInMemory <- if (!isStack) {
    im <- inMemory(obj)
    if (isBrick) {
      if (isTRUE(im))
        im <- rep(im, raster::nlayers(obj))
    }
    im
  } else {
    sapply(obj@layers, inMemory)
  }
  whichHasValues <- if (!isStack) {
    hasValues(obj)
  } else {
    sapply(obj@layers, hasValues)
  }

  isFilebacked <- !(whichInMemory | !whichHasValues)
  # if (isStack && any(isFilebacked)) {
  #   if (allInOneFile) {
  #      isFilebacked <- rep(TRUE, numFiles)
  #   }
  # }

  ## create a storage vector of file names to be filled
  curFilename <- if (isBrick) {
    rep("", raster::nlayers(obj))
  } else {
    rep("", length(isFilebacked))
  }

  if (any(!isFilebacked)) {
    fileExt <- if (!isStack) {
      raster::is.factor(obj)
    } else {
      sapply(obj@layers, raster::is.factor)
    }

    fileExt <- ifelse(fileExt, ".grd", ".tif")
    tempName <- basename(tempfile(pattern = "raster", fileext = fileExt, tmpdir = ""))
    curFilename[!isFilebacked] <- tempName[!isFilebacked]
  }
  if (any(isFilebacked)) {
    if (isTRUE(allInOneFile)) {
      curFiles <- normPath(Filenames(obj, allowMultiple = FALSE))
    } else {
      curFiles <- normPath(Filenames(obj, allowMultiple = FALSE)[isFilebacked])
    }
    curFilename[isFilebacked] <- curFiles
    # if (is(obj, "RasterLayer") || is(obj, "RasterBrick") ||
    #     (is(obj, "RasterStack") && numFiles == 1)) {
    #   curFilename <- normalizePath(Filenames(obj), winslash = "/", mustWork = FALSE)
    # } else  {
    #   curFilenames <- unlist(lapply(obj@layers, function(x)
    #     normalizePath(filename(x), winslash = "/", mustWork = FALSE)))
    #   curFilename[isFilebacked] <- curFilenames[isFilebacked]
    # }
  }

  ## check for files that should've been backed, but don't exist
  if (any(!file.exists(curFilename) & isFilebacked & isRasterLayer)) {
    badFileNames <- curFilename[!file.exists(curFilename) & isFilebacked]

    # File is in wrong folder, usually the result of a copy of cache between 2 machines
    splittedFilenames <- strsplit(badFileNames, split = basename(repoDir))
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
           paste("    ", badFileNames, collapse = "\n"),
           ". The most likely reason is that two functions had the same output ",
           "and one of them was removed with clearCache(...). ",
           "The best solution to this is never have two functions create the same ",
           "file-backed raster.")
    } else {
      if (!isStack) {
        slot(slot(obj, "file"), "name") <- saveFilename <- curFilename <- trySaveFilename
      } else {
        curFilename[!file.exists(curFilename) & isFilebacked] <- trySaveFilename
        saveFilename <- curFilename
        for (i in seq_len(nlayers(obj))) {
          slot(slot(obj@layers[[i]], "file"), "name") <- saveFilename[i]
        }
      }
    }
  } else {
    saveFilename <- if (isRepo) {
      file.path(repoDir, "rasters", basename(curFilename))
    } else {
      file.path(repoDir, basename(curFilename))
    }
    saveFilename <- normPath(saveFilename)
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
  # filenames are not the same, check if backed, act accordingly

  if (any(saveFilename != curFilename)) {
    notSameButBacked <- saveFilename != curFilename & isFilebacked

    if (any(notSameButBacked)) {
      ## deal only with files that have been backed
      saveFilename2 <- saveFilename[notSameButBacked]
      curFilename2 <- curFilename[notSameButBacked]

      pathExists <- dir.exists(dirname(saveFilename2))
      if (any(!pathExists)) {
        dirname(saveFilename2) %>%
          unique() %>%
          sapply(., dir.create, recursive = TRUE)
      }

      saveFilename2 <- sapply(seq_along(curFilename2), function(x) {
        curFilenameBase <- filePathSansExt(curFilename2[x])
        curFilename <- dir(dirname(curFilename2[x]), pattern = paste0(basename(curFilenameBase), "\\."),
                           full.names = TRUE)
        exts <- fileExt(curFilename)
        saveFilenamesBase <- filePathSansExt(saveFilename2[x])
        saveFilenames <- paste0(saveFilenamesBase, ".", exts)


        # browser(expr = exists("._prepareFileBackedRaster_2"))
        # change filename if it already exists
        if (any(file.exists(saveFilenames))) {
          saveFilenames <- nextNumericName(saveFilenames)
        }
        outFL <- suppressWarnings(file.link(to = saveFilenames,
                  # overwrite = TRUE,
                  from = curFilename))
        if (any(!outFL)) {
          copyFile(to = saveFilenames[!outFL],
                  overwrite = TRUE,
                  from = curFilename[!outFL], silent = TRUE)

        }
        saveFilenames[match(fileExt(curFilename2[x]), fileExt(saveFilenames))]
      })

      # for a stack with independent Raster Layers (each with own file)
      obj <- updateFilenameSlots2(obj, curFilename2, saveFilename2, isStack)
      # if (length(curFilename2) > 1) {
      #   for (i in seq_along(curFilename2)) {
      #     slot(slot(slot(obj, "layers")[[i]], "file"), "name") <- saveFilename2[i]
      #   }
      # } else {
      #   if (!isStack) {
      #     slot(slot(obj, "file"), "name") <- saveFilename2
      #   } else {
      #     for (i in seq_len(nlayers(obj))) {
      #       whFilename <- match(basename(saveFilename2), basename(curFilename2))
      #       slot(slot(obj@layers[[i]], "file"), "name") <- saveFilename2[whFilename]
      #     }
      #   }
      # }

      ## update saveFilename
      saveFilename[notSameButBacked] <- saveFilename2[notSameButBacked]
    }
    if (any(!notSameButBacked)) {
      ## deal with files that haven't been backed
      checkPath(unique(dirname(saveFilename[!notSameButBacked])), create = TRUE)
      if (any(!whichInMemory[!notSameButBacked])) {
        if (!isStack) {
          obj <- writeRaster(obj, filename = saveFilename[!notSameButBacked], datatype = dataType(obj))
        } else {
          for (i in which(!notSameButBacked)) {
            obj@layers[[i]] <- writeRaster(obj@layers[[i]], filename = saveFilename[i], datatype = dataType(obj@layers[[i]]))
          }
        }
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
#' @author Eliot McIntire and Alex Chubaty
#' @rdname copyFile
#'
#' @examples
#' tmpDirFrom <- file.path(tempdir(), "example_fileCopy_from")
#' tmpDirTo <- file.path(tempdir(), "example_fileCopy_to")
#' tmpFile1 <- tempfile("file1", tmpDirFrom, ".csv")
#' tmpFile2 <- tempfile("file2", tmpDirFrom, ".csv")
#' checkPath(tmpDirFrom, create = TRUE)
#' f1 <- normalizePath(tmpFile1, mustWork = FALSE)
#' f2 <- normalizePath(tmpFile2, mustWork = FALSE)
#' t1 <- normalizePath(file.path(tmpDirTo, basename(tmpFile1)), mustWork = FALSE)
#' t2 <- normalizePath(file.path(tmpDirTo, basename(tmpFile2)), mustWork = FALSE)
#'
#' write.csv(data.frame(a = 1:10, b = runif(10), c = letters[1:10]), f1)
#' write.csv(data.frame(c = 11:20, d = runif(10), e = letters[11:20]), f2)
#' copyFile(c(f1, f2), c(t1, t2))
#' file.exists(t1) ## TRUE
#' file.exists(t2) ## TRUE
#' identical(read.csv(f1), read.csv(f2)) ## FALSE
#' identical(read.csv(f1), read.csv(t1)) ## TRUE
#' identical(read.csv(f2), read.csv(t2)) ## TRUE
#'
#' unlink(tmpDirFrom, recursive = TRUE)
#' unlink(tmpDirTo, recursive = TRUE)
#'
copySingleFile <- function(from = NULL, to = NULL, useRobocopy = TRUE,
                           overwrite = TRUE, delDestination = FALSE,
                           #copyRasterFile = TRUE, clearRepo = TRUE,
                           create = TRUE, silent = FALSE) {
  if (any(length(from) != 1, length(to) != 1)) stop("from and to must each be length 1")
  origDir <- getwd()
  useFileCopy <- identical(dirname(from), dirname(to))

  lapply(unique(dirname(to)), checkPath, create = create)

  os <- tolower(Sys.info()[["sysname"]])
  .onLinux <- .Platform$OS.type == "unix" && unname(os) == "linux"
  if (!useFileCopy) {
    if (isWindows() && isTRUE(file.size(from) > 1e6)) {
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
        # rsync command can't handle spaces in dirnames -- must protect them
        toDir <- gsub("\ ", "\\ ", toDir, fixed = TRUE)
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

#' @export
#' @rdname copyFile
copyFile <- Vectorize(copySingleFile, vectorize.args = c("from", "to"))

#' @importFrom methods slotNames
#' @importFrom digest digest
#' @importFrom raster res crs extent
#' @rdname cache-helpers
.digestRasterLayer <- function(object, length, algo, quick) {
  # metadata -- only a few items of the long list because one thing (I don't recall)
  #  doesn't cache consistently
  if (isTRUE(getOption("reproducible.useNewDigestAlgorithm") < 2)) {
    return(.digestRasterLayer2(object, length, algo, quick))
  }

  isRasterStack <- is(object, "RasterStack")
  if (!isRasterStack) {
    objList <- list(object)
  } else {
    objList <- object@layers
  }
  dig <- lapply(objList, function(object) {
    sn <- slotNames(object@data)
    sn <- sn[!(sn %in% c(#"min", "max", "haveminmax", "names", "isfactor",
      "dropped", "nlayers", "fromdisk", "inmemory"
      #"offset", "gain"
    ))]
    dataSlotsToDigest <- lapply(sn, function(s) slot(object@data, s))
    if (isTRUE(getOption("reproducible.useNewDigestAlgorithm") > 0))
      dig <- .robustDigest(append(list(dim(object), res(object), crs(object),
                                       extent(object)), dataSlotsToDigest), length = length, quick = quick,
                           algo = algo) # don't include object@data -- these are volatile
    else {
      if (!requireNamespace("fastdigest", quietly = TRUE))
        stop(requireNamespaceMsg("fastdigest", "to use options('reproducible.useNewDigestAlgorithm' = FALSE"))
      dig <- fastdigest::fastdigest(append(list(dim(object), res(object), crs(object),
                                                extent(object)), dataSlotsToDigest)) # don't include object@data -- these are volatile
    }

    # Legend
    sn <- slotNames(object@legend)
    legendSlotsToDigest <- lapply(sn, function(s) slot(object@legend, s))
    if (isTRUE(getOption("reproducible.useNewDigestAlgorithm") > 0))
      dig2 <- .robustDigest(legendSlotsToDigest, length = length, quick = quick,
                            algo = algo) # don't include object@data -- these are volatile
    else {
      if (!requireNamespace("fastdigest", quietly = TRUE))
        stop(requireNamespaceMsg("fastdigest", "to use options('reproducible.useNewDigestAlgorithm' = FALSE"))
      dig2 <- fastdigest::fastdigest(legendSlotsToDigest) # don't include object@data -- these are volatile
    }
    dig <- c(dig, dig2)

    sn <- slotNames(object@file)
    sn <- sn[!(sn %in% c("name"))]
    fileSlotsToDigest <- lapply(sn, function(s) slot(object@file, s))
    if (isTRUE(getOption("reproducible.useNewDigestAlgorithm") > 0))
      digFile <- .robustDigest(fileSlotsToDigest, length = length, quick = quick,
                               algo = algo) # don't include object@file -- these are volatile
    else {
      if (!requireNamespace("fastdigest", quietly = TRUE))
        stop(requireNamespaceMsg("fastdigest", "to use options('reproducible.useNewDigestAlgorithm' = FALSE"))
      digFile <- fastdigest::fastdigest(fileSlotsToDigest) # don't include object@file -- these are volatile
    }

    dig <- c(dig, digFile)
  })

  fns <- Filenames(object, allowMultiple = FALSE)
  if (length(fns[nchar(fns) > 0])) {
    # if the Raster is on disk, has the first length characters;
    isGrd <- endsWith(basename(fns), suffix = ".grd")
    if (isTRUE(any(isGrd))) {
      fns[isGrd] <- sub(fns[isGrd], pattern = ".grd$", replacement = ".gri")
    }
    # # there is no good reason to use depth = 0, 1, or 2 or more -- but I think 2 is *more* reliable
    dig2 <- .robustDigest(asPath(fns, 2), length = length, quick = quick, algo = algo)
    dig <- c(dig, unname(dig2))
  }

  if (isTRUE(getOption("reproducible.useNewDigestAlgorithm") > 0))
    dig <- .robustDigest(unlist(dig), length = length, quick = quick, algo = algo)
  else {
    if (!requireNamespace("fastdigest", quietly = TRUE))
      stop(requireNamespaceMsg("fastdigest", "to use options('reproducible.useNewDigestAlgorithm' = FALSE"))
    dig <- fastdigest::fastdigest(dig)
  }
  dig
}

.digestRasterLayer2 <- function(object, length, algo, quick) {
  # metadata -- only a few items of the long list because one thing (I don't recall)
  #  doesn't cache consistently
  sn <- slotNames(object@data)
  sn <- sn[!(sn %in% c(#"min", "max", "haveminmax", "names", "isfactor",
    "dropped", "nlayers", "fromdisk", "inmemory"
    #"offset", "gain"
  ))]
  dataSlotsToDigest <- lapply(sn, function(s) slot(object@data, s))
  if (isTRUE(getOption("reproducible.useNewDigestAlgorithm")))
    dig <- .robustDigest(append(list(dim(object), res(object), crs(object),
                                     extent(object)), dataSlotsToDigest), length = length, quick = quick,
                         algo = algo) # don't include object@data -- these are volatile
  else {
    if (!requireNamespace("fastdigest", quietly = TRUE))
      stop(requireNamespaceMsg("fastdigest", "to use options('reproducible.useNewDigestAlgorithm' = FALSE"))
    dig <- fastdigest::fastdigest(append(list(dim(object), res(object), crs(object),
                                              extent(object)), dataSlotsToDigest)) # don't include object@data -- these are volatile
  }

  # Legend
  sn <- slotNames(object@legend)
  legendSlotsToDigest <- lapply(sn, function(s) slot(object@legend, s))
  if (isTRUE(getOption("reproducible.useNewDigestAlgorithm")))
    dig2 <- .robustDigest(legendSlotsToDigest, length = length, quick = quick,
                          algo = algo) # don't include object@data -- these are volatile
  else {
    if (!requireNamespace("fastdigest", quietly = TRUE))
      stop(requireNamespaceMsg("fastdigest", "to use options('reproducible.useNewDigestAlgorithm' = FALSE"))
    dig2 <- fastdigest::fastdigest(legendSlotsToDigest) # don't include object@data -- these are volatile
  }
  dig <- c(dig, dig2)

  sn <- slotNames(object@file)
  sn <- sn[!(sn %in% c("name"))]
  fileSlotsToDigest <- lapply(sn, function(s) slot(object@file, s))
  if (isTRUE(getOption("reproducible.useNewDigestAlgorithm")))
    digFile <- .robustDigest(fileSlotsToDigest, length = length, quick = quick,
                             algo = algo) # don't include object@file -- these are volatile
  else {
    if (!requireNamespace("fastdigest", quietly = TRUE))
      stop(requireNamespaceMsg("fastdigest", "to use options('reproducible.useNewDigestAlgorithm' = FALSE"))
    digFile <- fastdigest::fastdigest(fileSlotsToDigest) # don't include object@file -- these are volatile
  }

  dig <- c(dig, digFile)
  if (nzchar(object@file@name)) {
    # if the Raster is on disk, has the first length characters;
    filename <- if (isTRUE(endsWith(basename(object@file@name), suffix = ".grd"))) {
      sub(object@file@name, pattern = ".grd$", replacement = ".gri")
    } else {
      object@file@name
    }
    # there is no good reason to use depth = 0, 1, or 2 or more -- but I think 2 is *more* reliable
    dig2 <- .robustDigest(asPath(filename, 2), length = length, quick = quick, algo = algo)
    dig <- c(dig, unname(dig2))
  }

  if (isTRUE(getOption("reproducible.useNewDigestAlgorithm")))
    dig <- .robustDigest(unlist(dig), length = length, quick = quick, algo = algo)
  else {
    if (!requireNamespace("fastdigest", quietly = TRUE))
      stop(requireNamespaceMsg("fastdigest", "to use options('reproducible.useNewDigestAlgorithm' = FALSE"))
    dig <- fastdigest::fastdigest(dig)
  }
  dig
}


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

  if (is.character(namesObj)) {
    namesObj <- gsub(namesObj, pattern = "\\.|_", replacement = "aa")
    allLower <- tolower(namesObj) == namesObj
    namesObj[allLower] <- paste0("abALLLOWER", namesObj[allLower])

    onesChanged <- startsWith(namesObj, prefix = "a")
    namesObj[!onesChanged] <- paste0("ZZZZZZZZZ", namesObj[!onesChanged])

    out <- order(namesObj)
  } else {
    out <- seq_along(obj)
  }
  out
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
.debugCache <- function(obj, preDigest, ...) {
  setattr(obj, "debugCache1", list(...))
  setattr(obj, "debugCache2", preDigest)
  obj
}

# loadFromLocalRepoMem <- memoise::memoise(loadFromLocalRepo)

#' @keywords internal
.getOtherFnNamesAndTags <- function(scalls) {
  if (is.null(scalls)) {
    scalls <- sys.calls()
  }

  patt <- paste(.defaultOtherFunctionsOmit, collapse = ")|(")
  otherFns <- .grepSysCalls(
    scalls,
    pattern = patt)
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

#' @keywords internal
nextNumericName <- function(string) {
  theExt <- fileExt(string)
  saveFilenameSansExt <- filePathSansExt(string)
  finalNumericPattern <- "_[[:digit:]]+$"
  allSimilarFilesInDir <- dir(dirname(saveFilenameSansExt), pattern = basename(saveFilenameSansExt))
  allSimilarFilesInDirSansExt <- if (length(allSimilarFilesInDir) == 0) {
    unique(saveFilenameSansExt)
  } else {
    unique(filePathSansExt(allSimilarFilesInDir))
  }
  alreadyHasNumeric <- grepl(allSimilarFilesInDirSansExt, pattern = finalNumericPattern)
  if (isTRUE(any(alreadyHasNumeric))) {
    splits <- strsplit(allSimilarFilesInDirSansExt[alreadyHasNumeric], split = "_")
    highestNumber <- max(unlist(lapply(splits, function(split) as.numeric(tail(split,1)))),
                         na.rm = TRUE)
    preNumeric <- unique(unlist(lapply(splits, function(spl) paste(spl[-length(spl)], collapse = "_")))) #nolint
    ## keep rndstr in here (below), so that both streams keep same rnd number state
    out <- file.path(dirname(saveFilenameSansExt), paste0(preNumeric, "_", highestNumber + 1))
  } else {
    out <- paste0(saveFilenameSansExt, "_1")
  }

  paste0(out, ".", theExt)
}

#' Grep system calls
#'
#' A faster way of grepping the system call stack than just
#' \code{grep(sys.calls(), pattern = "test")}
#'
#' @param sysCalls The return from \code{sys.calls()}
#' @param pattern Character, passed to grep
#' @return
#' Numeric vector, equivalent to return from \code{grep(sys.calls(), pattern = "test")},
#' but faster if \code{sys.calls()} is very big.
#'
#' @export
#' @keywords internal
#' @rdname grepSysCalls
.grepSysCalls <- function(sysCalls, pattern) {
  scallsFirstElement <- lapply(sysCalls, function(x) x[1])
  grep(scallsFirstElement, pattern = pattern)
}

#' @importFrom raster fromDisk
dealWithRasters <- function(obj, cachePath, drv, conn) {
  # browser(expr = exists("._dealWithRasters_1"))
  outputToSaveIsList <- is(obj, "list") # is.list is TRUE for anything, e.g., data.frame. We only want "list"
  if (outputToSaveIsList) {
    rasters <- unlist(lapply(obj, is, "Raster"))
  } else {
    rasters <- is(obj, "Raster")
  }
  if (any(rasters)) {
    objOrig <- obj
    atts <- attributes(obj)
    # browser(expr = exists("._dealWithRasters_2"))
    if (outputToSaveIsList) {
      obj[rasters] <- lapply(obj[rasters], function(x)
        .prepareFileBackedRaster(x, repoDir = cachePath, overwrite = FALSE, drv = drv, conn = conn))
      isFromDisk <- any(unlist(lapply(obj, function(x)
        if (is(x, "Raster")) fromDisk(x) else FALSE)))
    } else {
      obj <- .prepareFileBackedRaster(obj, repoDir = cachePath,
                                      overwrite = FALSE, drv = drv, conn = conn)
      isFromDisk <- fromDisk(obj)
    }

    # have to reset all these attributes on the rasters as they were undone in prev steps
    atts$tags <- c(atts$tags, paste("fromDisk", sep = ":", isFromDisk))
    setattr(obj, "tags", atts$tags)
    .setSubAttrInList(obj, ".Cache", "newCache", atts$.Cache$newCache)
    setattr(obj, "call", atts$call)

    if (!identical(attr(obj, ".Cache")$newCache, atts$.Cache$newCache))
      stop("attributes are not correct 6")
    if (!identical(attr(obj, "call"), atts$call))
      stop("attributes are not correct 7")
    if (!identical(attr(obj, "tags"), atts$tags))
      stop("attributes are not correct 8")

    if (!is.null(atts[["function"]])) {
      setattr(obj, "function", atts[["function"]])
      if (!identical(attr(obj, "function"), atts[["function"]]))
        stop("There is an unknown error 04")
    }
    if (isFromDisk) {
      if (isTRUE(getOption("reproducible.useNewDigestAlgorithm") < 2)) {
        obj <- list(origRaster = objOrig, cacheRaster = obj)
      } else {
        obj <- list(origRaster = Filenames(objOrig), cacheRaster = obj)
      }

    }

  }
  obj
}

.loadedCacheResultMsg <- "loaded cached result from previous"

.loadedMemoisedResultMsg <- "loaded memoised result from previous"

.loadedCacheMsg <- function(root, functionName) {
  paste0("     ", root," ", functionName, " call, ")
}

updateFilenameSlots <- function(obj, curFilenames, newFilenames, isStack = NULL) {
  if (isTRUE(getOption("reproducible.useNewDigestAlgorithm") < 2)) {
    return(updateFilenameSlots2(obj, curFilenames, newFilenames, isStack))
  }

  if (length(curFilenames) > 1) {
    for (i in seq_along(curFilenames)) {
      slot(slot(slot(obj, "layers")[[i]], "file"), "name") <- newFilenames[i]
    }
  } else {
    if (is.null(isStack)) isStack <- is(obj, "RasterStack")
    if (!isStack) {
      slot(slot(obj, "file"), "name") <- newFilenames
    } else {
      # aiof <- allInOneFile(obj)

      # if (isTRUE(aiof)) {
      #   slot(obj, "filename") <- newFilenames
      # } else {
        for (i in seq_len(nlayers(obj))) {
          whFilename <- match(withoutFinalNumeric(basename(newFilenames)),
                              withoutFinalNumeric(basename(curFilenames)))
          slot(slot(obj@layers[[i]], "file"), "name") <- newFilenames[whFilename]
        }
      # }


    }
  }
  obj
}

updateFilenameSlots2 <- function(obj, curFilenames, newFilenames, isStack = NULL) {
  whichNotGri <- grep("\\.gri$", curFilenames, invert = TRUE)
  curFilenamesNotGri <- curFilenames[whichNotGri]
  newFilenamesNotGri <- newFilenames[whichNotGri]
  if (length(curFilenamesNotGri) > 1 ) {
    for (i in seq_along(curFilenamesNotGri)) {
      slot(slot(slot(obj, "layers")[[i]], "file"), "name") <- newFilenamesNotGri[i]
    }
  } else {
    if (is.null(isStack)) isStack <- is(obj, "RasterStack")
    if (!isStack) {
      slot(slot(obj, "file"), "name") <- newFilenamesNotGri
    } else {
      for (i in seq_len(nlayers(obj))) {
        if (fromDisk(obj[[i]])) {
          whFilename <- match(withoutFinalNumeric(basename(newFilenamesNotGri)),
                              withoutFinalNumeric(basename(curFilenamesNotGri)))
          slot(slot(obj@layers[[i]], "file"), "name") <- newFilenamesNotGri[whFilename]
        }
      }
    }
  }
  obj
}

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
#' @param repoDir Character denoting an existing directory in which an artifact will be saved.
#'
#' @param overwrite Logical. Should the raster be saved to disk, overwriting existing file.
#'
#' @param ... Not used
#'
#' @return A raster object and its newly located file backing.
#'         Note that if this is a legitimate Cache repository, the new location
#'         will be a subdirectory called \file{rasters/} of \file{repoDir/}.
#'         If this is not a repository, the new location will be within \code{repoDir}.
#'
#' @author Eliot McIntire
#' @export
#' @importFrom digest digest
#' @importFrom methods is selectMethod slot slot<-
#' @importFrom raster dataType filename hasValues inMemory nlayers writeRaster
#' @inheritParams Cache
#' @rdname prepareFileBackedRaster
#' @examples
#' library(raster)
#' # make a cache repository
#' a <- Cache(rnorm, 1)
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
.prepareFileBackedRaster <- function(obj, repoDir = NULL, overwrite = FALSE,
                                     drv = getOption("reproducible.drv", RSQLite::SQLite()),
                                     conn = getOption("reproducible.conn", NULL),
                                     ...) {
  if (isTRUE(getOption("reproducible.useNewDigestAlgorithm") < 2)) {
    return(.prepareFileBackedRaster2(obj, repoDir = repoDir, overwrite = overwrite,
                              drv = drv, conn = conn, ...))
  }
  fnsAll <- Filenames(obj)
  fnsShort <- Filenames(obj, FALSE)
  if (!all(nchar(fnsAll) == 0)) {
    repoDir <- checkPath(repoDir, create = TRUE)
    isRepo <- CacheIsACache(cachePath = repoDir, drv = drv, conn = conn)
    # thoseWithGRI <- endsWith(fnsAll, "gri")
    fns <- fnsAll
    FB <- nchar(fns) > 0
    ########################
    if (any(!file.exists(fns[FB]))) {
      FBshort <- nchar(fnsShort) > 0
      fnsOnly <- fnsShort[FBshort]
      badFileNames <- fnsOnly[!file.exists(fnsOnly)]

      trySaveFilename <- badFileNames
      if (any(grepl(basename(repoDir), badFileNames))) {
        # File is in wrong folder, usually the result of a copy of cache between 2 machines
        splittedFilenames <- strsplit(badFileNames, split = basename(repoDir))
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
      }
      if (any(!file.exists(trySaveFilename))) {
        stop("The following file-backed rasters are supposed to be on disk ",
             "but appear to have been deleted:\n",
             paste("    ", badFileNames, collapse = "\n"),
             ". The most likely reason is that two functions had the same output ",
             "and one of them was removed with clearCache(...). ",
             "The best solution to this is never have two functions create the same ",
             "file-backed raster.")
      } else {
        obj <- updateFilenameSlots(obj, curFilenames = fnsOnly, trySaveFilename)
        fnsAll <- fns <- Filenames(obj)
      }
    }
    #################


    saveFilename <- fns
    bn <- basename(fns)
    bnFB <- bn[FB]


    saveFilename[FB] <- if (isRepo) {
      file.path(repoDir, "rasters"[isRepo], bnFB)
    } else {
      file.path(repoDir, bnFB)
    }
    dirForNewFiles <- unique(dirname(saveFilename[FB]))
    checkPath(dirForNewFiles, create = TRUE)
    saveFilename <- normPath(saveFilename)
    saveFilenamePreNumeric <- saveFilename
    exist <- file.exists(saveFilename)
    if (any(exist)) {
      saveFilename[exist] <- nextNumericName(saveFilename[exist])
    }
    FBAll <- nchar(fnsAll) > 0
    out <- Map(from = fnsAll[FBAll], to = saveFilename[FBAll],
        function(from, to) {
      linkTry <- suppressWarningsSpecific(file.link(from = from, to = to),
                                      falseWarnings = "already exists")
      if (!linkTry)
        linkTry <- copyFile(from = from, to = to, overwrite = TRUE, silent = TRUE)
      linkTry
    })

    # FBshort <- nchar(fnsShort) > 0
    saveFilenamesToUpdateSlot <- saveFilename[basename(saveFilenamePreNumeric) %in%
                                                basename(fnsShort)]
    obj <- updateFilenameSlots(obj, fnsShort, saveFilenamesToUpdateSlot)

  }
  obj
}

allInOneFile <- function(obj) {
  aiof <- TRUE
  if (is(obj, "RasterStack")) {
    aiof <- FALSE
    numFiles <- sum(nchar(unique(Filenames(obj)))>0)
    if (numFiles > 0) {
      innerFilenames <- unlist(lapply(obj@layers, filename))
      aiof <- isTRUE(sum(nchar(innerFilenames)>0) == length(obj@layers))
    }
  }
  aiof
}

withoutFinalNumeric <- function(string) {
  ext <- fileExt(string)
  string1 <- filePathSansExt(string)
  woNumeric <- gsub("^(.+)\\_[[:digit:]]+$", "\\1", string1)
  paste0(woNumeric, ".", ext)
}
