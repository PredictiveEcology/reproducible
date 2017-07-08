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
#' @docType methods
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
#' @author Eliot McIntire
#' @docType methods
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
#' @docType methods
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
#' Any miscellaneous things to do before robustDigest and after FUN call
#'
#' The default method for \code{preDigestByClass} and simply returns NULL. 
#' There may be methods in other packages. 
#' 
#' @inheritParams Cache
#'
#' @param object Any R object.
#'
#' @param FUN A function
#'
#' @return A list with elements that will likely be used in \code{.postProcessing}
#'
#' @author Eliot McIntire
#' @docType methods
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
#' @docType methods
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
      match.call(FUN, do.call(call, append(list(name = FUN@generic), list(...))))
    )
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


################################################################################
#' Clear erroneous archivist artifacts
#'
#' When an archive object is being saved, if this is occurring at the same time
#' as another process doing the same thing, a stub of an artifact may occur. This
#' function will clear those stubs.
#'
#' @return Done for its side effect on the repoDir
#'
#' @param repoDir A character denoting an existing directory of the Repository for
#' which metadata will be returned. If it is set to NULL (by default), it
#' will use the repoDir specified in \code{archivist::setLocalRepo}.
#'
#' @export
#' @importFrom archivist showLocalRepo rmFromLocalRepo
#' @docType methods
#' @rdname clearStubArtifacts
#' @author Eliot McIntire
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
#' and changes the pointer (i.e., filename(object)) so that it is pointing to the new
#' file.
#'
#' @param obj The raster object to save to the repository.
#'
#' @inheritParams Cache
#'
#' @param repoDir Character denoting an existing directory in which an artifact will be saved.
#'
#' @param ... passed to \code{archivist::saveToRepo}
#'
#' @return A raster object and its newly located file backing. Note that if this is a
#' legitimate archivist repository,
#' the new location will be in a subfolder called "rasters" of \code{repoDir}.
#' If this is not a repository, then the new file location will placed in \code{repoDir}.
#'
#' @author Eliot McIntire
#' @docType methods
#' @export
#' @importFrom digest digest
#' @importFrom raster filename dataType inMemory writeRaster nlayers
#' @importFrom methods slot is selectMethod slot<-
#' @rdname prepareFileBackedRaster
#'
prepareFileBackedRaster <- function(obj, repoDir = NULL, compareRasterFileLength = 1e6, ...) {
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
        winslash = "/")
    } else {
      normalizePath(
        file.path(repoDir, splittedFilenames),
        winslash = "/")
    }
    if (any(!file.exists(trySaveFilename))) {
      stop("please rename raster that thinks is on disk with this or these filename(s) ",
           curFilename, " or rerun cache.")
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
          copyFile(to = saveFilename, overwrite = TRUE, from = curFilename, silent = TRUE)
          griFilename <- sub(saveFilename, pattern = "[.]grd$", replacement = ".gri")
          curGriFilename <- sub(curFilename, pattern = "[.]grd$", replacement = ".gri")
          copyFile(to = griFilename, overwrite = TRUE, from = curGriFilename, silent = TRUE)
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
#' This will copy an individual file faster using \code{Robocopy} on Windows,
#' and using \code{rsync} on macOS and Linux.
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
#' if they don't exist in the source. This is \code{/purge}.
#'
#' @param create Passed to \code{checkLazyDir}.
#'
#' @param silent Should a progress be printed.
#'
#' @inheritParams base::file.copy
#'
#' @author Eliot McIntire
#' @docType methods
#' @export
#' @rdname copyFile
#'
copyFile <- function(from = NULL, to = NULL, useRobocopy = TRUE,
                     overwrite = TRUE, delDestination = FALSE,
                     #copyRasterFile=TRUE, clearRepo=TRUE,
                     create = TRUE, silent = FALSE, recursive = TRUE) {
  origDir <- getwd()
  useFileCopy <- FALSE

  if (!dir.exists(to)) to <- dirname(to) # extract just the directory part
  os <- tolower(Sys.info()[["sysname"]])
  if (os == "windows") {
    robocopyBin <- tryCatch(system("where robocopy", intern = TRUE),
                            warning = function(w) NA_character_)

    robocopy <-  if (silent) {
      paste0(robocopyBin, "", "/purge"[delDestination], " /ETA /NDL /NFL /NJH /NJS ",  # nolint
             normalizePath(dirname(from), mustWork = TRUE, winslash = "\\"), "\\ ",
             normalizePath(to, mustWork = FALSE, winslash = "\\"),  " ", basename(from))
    } else {
      paste0(robocopyBin, " ", "/purge"[delDestination], " /ETA /xo ", # nolint
             normalizePath(from, mustWork = TRUE, winslash = "\\"), "\\ ",
             normalizePath(to, mustWork = FALSE, winslash = "\\"), " /E"[recursive], " ",
             basename(from))
    }

    useFileCopy <- if (useRobocopy && !is.na(robocopyBin)) {
      suppressWarnings(tryCatch(system(robocopy, intern = TRUE), error = function(x) TRUE))
    } else {
      TRUE
    }
  } else if ( (os == "linux") || (os == "darwin") ) { # nolint
    rsyncBin <- tryCatch(system("which rsync", intern = TRUE),
                         warning = function(w) NA_character_)
    opts <- if (silent) " -a " else " -avP "
    rsync <- paste0(rsyncBin, " ", opts, " --delete "[delDestination],
                    normalizePath(from, mustWork = TRUE), " ",
                    normalizePath(to, mustWork = FALSE), "/")

    useFileCopy <- tryCatch(system(rsync, intern = TRUE), error = function(x) TRUE)
  }
  if (isTRUE(useFileCopy))
    file.copy(from = from, to = to, overwrite = overwrite, recursive = FALSE)

  setwd(origDir)
  return(invisible(to))
}

#' @rdname cacheHelper
#' @importFrom raster res crs extent
digestRaster <- function(object, compareRasterFileLength, algo) {
  dig <- fastdigest::fastdigest(list(dim(object), res(object), crs(object),
                                     extent(object), object@data))
  if (nzchar(object@file@name)) {
    # if the Raster is on disk, has the first compareRasterFileLength characters;
    dig <- fastdigest(
      append(dig, digest::digest(file = object@file@name,
                                 length = compareRasterFileLength,
                                 algo = algo)))
  }
}


#' Recursive copying of nested environments, and other "hard to copy" objects
#'
#' When copying environments and all the objects contained within them, there are
#' no copies made: it is a pass-by-reference operation. Sometimes, a deep copy is
#' needed, and sometimes, this must be recursive (i.e., environments inside
#' environments)
#'
#' @param object  An R object (likely containing environments) or an environment
#' @param filebackedDir A directory to copy any files that are backing R objects,
#'                      currently only valid for \code{Raster} classes. Defaults
#'                      to \code{tempdir()}, which is unlikely to be very useful.
#' @param ... Only used for custom Methods
#'
#' @author Eliot McIntire
#' @docType methods
#' @export
#' @importFrom data.table copy
#' @rdname Copy
#' @seealso \code{\link{robustDigest}}
#'
setGeneric("Copy", function(object, filebackedDir=tempdir(), ...) {
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
            object <- prepareFileBackedRaster(object, repoDir = filebackedDir)
})

################################################################################
#' Sort a any named object with dotted names first
#'
#' Internal use only. This exists so Windows and Linux machines can have
#' the same order after a sort.
#'
#' @param obj  An arbitrary R object for which a \code{names} function
#'              returns a character vector.
#'
#' @return The same object as \code{obj}, but sorted with .objects first.
#'
#' @author Eliot McIntire
#' @docType methods
#' @export
#' @rdname sortDotsUnderscoreFirst
#'
sortDotsUnderscoreFirst <- function(obj) {
  names(obj) <- gsub(names(obj), pattern = "\\.", replacement = "DOT")
  names(obj) <- gsub(names(obj), pattern = "_", replacement = "US")
  allLower <- which(tolower(names(obj)) == names(obj))
  names(obj)[allLower] <- paste0("ALLLOWER", names(obj)[allLower])
  obj[order(names(obj))]
}

################################################################################
#' Attach debug info to return for Cache
#'
#' Internal use only. Attaches an attribute to the output, useable for
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
#' @docType methods
#' @rdname debugCache
#'
.debugCache <- function(obj, preDigest, ...) {
  setattr(obj, "debugCache1", list(...))
  setattr(obj, "debugCache2", preDigest)
  obj
}
