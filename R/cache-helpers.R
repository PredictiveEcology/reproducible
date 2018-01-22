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
    message(crayon::blue("  loading cached result from previous ", functionName, " call.",
                         sep = ""))
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
#' This is a generic definition that can be extended according to class. Normally,
#' checkPath can be called directly, but does not have class-specific methods.
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
#'
setGeneric(".checkCacheRepo", function(object, create=FALSE) {
  standardGeneric(".checkCacheRepo")
})

#' @export
#' @rdname checkCacheRepo
setMethod(
  ".checkCacheRepo",
  signature = "ANY",
  definition = function(object, create) {
    cacheRepo <- tryCatch(checkPath(object, create), error = function(x) {
        message("No cacheRepo supplied. Using tempdir()")
        tempdir()
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
    .prepareFileBackedRaster(object, repoDir = cacheRepo)
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
#' @param isPipe Logical. If the call to getFunctionName is coming from a pipe, there is more
#'               information available. Specifically, ._lhs which is already a call.
#' @note If the function cannot figure out a clean function name, it returns "internal"
#'
#' @author Eliot McIntire
#' @importFrom methods selectMethod showMethods
#' @keywords internal
#' @rdname cacheHelper
getFunctionName <- function(FUN, ..., overrideCall, isPipe) { # nolint
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
        functionName <- deparse(functionName, width.cutoff = 300)
        if (all(functionName != "FUN")) break
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
#' path exanding of \code{~} vs. not, double back slash vs. single forward slash,
#' relative path vs. absolute path.
#' All of these should be assessed for their actual file or directory location,
#' NOT their character string. By converting all character string that are actual
#' file or directory paths with this function, then \code{Cache} will correctly assess
#' the location, NOT the character string representation.
#'
#' @param obj A character string to convert to a \code{Path}.
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
#' @rdname Path-class
#' @name asPath
setAs(from = "character", to = "Path", function(from) {
  new("Path", from)
})

################################################################################
#' Clear erroneous archivist artifacts
#'
#' Stub artifacts can result from several causes. The most common being
#' erroneous removal of a file in the sqlite database. This can be caused
#' sometimes if an archive object is being saved multiple times by multiple
#' threads. This function will clear entries in the sqlite database which
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
#'
.prepareFileBackedRaster <- function(obj, repoDir = NULL) {
  isRasterLayer <- TRUE
  isStack <- is(obj, "RasterStack")
  repoDir <- checkPath(repoDir, create = TRUE)
  isRepo <- if (!all(c("backpack.db", "gallery") %in% list.files(repoDir))) {
    FALSE
  } else {
    TRUE
  }

  if (!inMemory(obj)) {
    isFilebacked <- TRUE
    if (is(obj, "RasterLayer")) {
      curFilename <- normalizePath(filename(obj), winslash = "/", mustWork = FALSE)
    } else  {
      curFilenames <- unlist(lapply(obj@layers, function(x)
        normalizePath(filename(x), winslash = "/", mustWork = FALSE)))
      curFilename <- unique(curFilenames)
    }
  } else {
    isFilebacked <- FALSE
    if (is.factor(obj)) {
      fileExt <- ".grd"
    } else {
      fileExt <- ".tif"
    }
    curFilename <- basename(tempfile(pattern = "raster", fileext = fileExt, tmpdir = ""))
  }

  if (any(!file.exists(curFilename)) & isFilebacked & isRasterLayer) {
    splittedFilenames <- strsplit(curFilename, split = basename(repoDir))
    trySaveFilename <- if (length(splittedFilenames) == 1) {
      normalizePath(
        file.path(repoDir, splittedFilenames[[1]][[length(splittedFilenames[[1]])]]),
        winslash = "/", mustWork = FALSE)
    } else {
      normalizePath(
        file.path(repoDir, splittedFilenames),
        winslash = "/", mustWork = FALSE)
    }
    if (any(!file.exists(trySaveFilename))) {
      stop("The raster that is supposed to be on disk with this or these filename(s) ",
           curFilename, " has been deleted. It must be recreated e.g.,",
           "try showCache(userTags = \"writeRaster\"), examine that and possibly -- with",
           "caution -- clearCache(userTags = \"writeRaster\")")
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
          suppressWarnings(
            lapply(seq_along(curFilename),
                   function(x) copyFile(to = saveFilename[x],
                                        overwrite = TRUE,
                                        from = curFilename[x], silent = TRUE)))
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


#' Copy a file using \code{Robocopy} on Windows and \code{rsync} on Linux/macOS
#'
#' This is replacement for \code{file.copy}, but for one file at a time.
#' The additional feature is that it will use Robocopy (on Windows) or
#' rsync on Linux or Mac, if they exist. It will default back to \code{file.copy}
#' if none of these exists.
#' This will generally copy a large file faster using \code{Robocopy} on Windows,
#' and using \code{rsync} on macOS and Linux. In particular, if there is a possibility
#' that the file already exists, then this function should be very fast as it
#' will do "update only", i.e., nothing.
#'
#' @param from The source file.
#'
#' @param to The new file.
#'
#' @param useRobocopy For Windows, this will use a system call to \code{Robocopy}
#'        which appears to be much faster than the internal \code{file.copy} function.
#'        Uses \code{/MIR} flag. Default \code{TRUE}.
#'
#' @param overwrite Passed to \code{file.copy}
#'
#' @param delDestination Logical, whether the destination should have any files deleted,
#' if they don't exist in the source. This is \code{/purge} for RoboCopy and --delete for
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
                     #copyRasterFile=TRUE, clearRepo=TRUE,
                     create = TRUE, silent = FALSE) {
  origDir <- getwd()
  useFileCopy <- FALSE

  checkPath(dirname(to), create = create)

  os <- tolower(Sys.info()[["sysname"]])
  if (os == "windows") {
    if (!dir.exists(to)) to <- dirname(to) # extract just the directory part
    robocopyBin <- tryCatch(Sys.which("robocopy"), warning = function(w) NA_character_)

    robocopy <-  if (silent) {
      paste0(robocopyBin, " /purge"[delDestination], " /ETA /XJ /XO /NDL /NFL /NJH /NJS ",  # nolint
             normalizePath(dirname(from), mustWork = TRUE, winslash = "\\"), " ",
             normalizePath(to, mustWork = FALSE, winslash = "\\"),  " ",
             basename(from))
    } else {
      paste0(robocopyBin, " /purge"[delDestination], " /ETA /XJ /XO ", # nolint
             normalizePath(dirname(from), mustWork = TRUE, winslash = "\\"), " ",
             normalizePath(to, mustWork = FALSE, winslash = "\\"), " ",
             basename(from))
    }

    useFileCopy <- if (useRobocopy && !is.na(robocopyBin)) {
      suppressWarnings(tryCatch(system(robocopy, intern = TRUE), error = function(x) TRUE))
    } else {
      TRUE
    }
  } else if ( (os == "linux") || (os == "darwin") ) { # nolint
    if (!dir.exists(to)) to <- dirname(to) # extract just the directory part
    rsyncBin <- tryCatch(Sys.which("rsync"), warning = function(w) NA_character_)
    opts <- if (silent) " -a " else " -avP "
    rsync <- paste0(rsyncBin, " ", opts, " --delete "[delDestination],
                    normalizePath(from, mustWork = TRUE), " ",
                    normalizePath(to, mustWork = FALSE), "/")

    useFileCopy <- tryCatch(system(rsync, intern = TRUE), error = function(x) TRUE)
  } else {
    useFileCopy <- TRUE
  }
  if (isTRUE(useFileCopy)) {
    dir.create(dirname(to))
    file.copy(from = from, to = to, overwrite = overwrite, recursive = FALSE)
  }

  setwd(origDir)
  return(invisible(to))
}

#' @rdname cacheHelper
#' @importFrom fastdigest fastdigest
#' @importFrom digest digest
#' @importFrom raster res crs extent
.digestRaster <- function(object, compareRasterFileLength, algo) {
  if (nzchar(object@file@name)) {
    dig <- fastdigest(list(dim(object), res(object), crs(object),
                           extent(object), object@data))
    # if the Raster is on disk, has the first compareRasterFileLength characters;
    dig <- fastdigest(
      append(dig, digest::digest(file = object@file@name,
                                 length = compareRasterFileLength,
                                 algo = algo)))
  } else {
    dig <- fastdigest(object)
  }
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
#' Internal use only. This exists so Windows, *nux, Mac machines can have
#' the same order after a sort. It will put dots and underscores first
#' (with the sort key based on their second character, see examples. It also
#' sorts lower case before upper case
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

loadFromLocalRepoMem <- memoise::memoise(loadFromLocalRepo)


.getOtherFnNamesAndTags <- function(scalls) {
  if (is.null(scalls)) {
    scalls <- sys.calls()
  }

  otherFns <- grepl(scalls, pattern = c("(test_code)|(with_reporter)|(force)|(eval)|(::)|(\\$)|(\\.\\.)|(standardGeneric)|(Cache)|(tryCatch)|(doTryCatch)"))
  otherFns <- unlist(lapply(scalls[!otherFns], function(x) tryCatch(as.character(x[[1]]), error = function(y) "")))
  otherFns <- otherFns[nzchar(otherFns)]
  otherFns <- otherFns[!startsWith(otherFns, prefix = ".")]
  otherFns <- paste0("otherFunctions:", otherFns)

  # FIgure out if it is in a .parseModule call, if yes, then extract the module
  doEventFrameNum <- which(startsWith(as.character(scalls), prefix = ".parseModule"))
  if (length(doEventFrameNum)) {
    module <- get("m", envir = sys.frame(doEventFrameNum[2])) # always 2
    otherFns <- c(paste0("module:",module), otherFns)
  }
  unique(otherFns)
}

