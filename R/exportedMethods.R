## non-exported attribute stuff -------------------------------------------------
attributesReassign <- function(atts, obj) {
  attsNames <- setdiff(names(atts), knownAtts)
  if (length(attsNames))
    for (att in attsNames) {
      if (is.null(attr(obj, att))) {
        attr(obj, att) <- atts[[att]]
      }
    }
  obj
}

knownAtts <- c("cpp", "class", "attributes", "values", "definition", "pnt", "ptr")

## non-exported tag stuff -------------------------------------------------------

parseTags <- function(tags) {
  out <- strsplit(tags, ":")
  tags2 <- lapply(out, function(x) x[[1]])
  tags1 <- as.data.table(do.call(rbind, tags2))
  vals <- vapply(seq_len(NROW(tags1)), function(i) {
    gsub(paste0(tags1[[1]][i], ":"), "", tags[i])
  }, FUN.VALUE = character(1))
  set(tags1, NULL, "tagValue", vals)
  setnames(tags1, "V1", "tagKey")
  tags1
}

tagFilesToLoad <- "filesToLoad"
tagRelToWhere <- "relToWhere"
tagOrigFilename <- "origFilename"
tagOrigRelName <- "origRelName"

tagsSpatRaster <- function(obj = NULL, relToWhere = NULL, relName = NULL, cls = NULL,
                           whLayers = NULL, layerNams = NULL, obj2 = NULL) {
  fe <- if (is.null(obj)) NULL else tools::file_ext(obj)
  c(
    attr(obj, "tags"),
    paste0(tagRelToWhere, ":", names(relToWhere)),
    paste0(tagOrigFilename, ":", basename2(obj)),
    paste0(tagOrigRelName, ":", relName),
    paste0("fromDisk:", TRUE),
    paste0("class:", cls),
    paste0("fileFormat:", fe),
    paste0("saveRawFile:", TRUE),
    paste0("loadFun:", "terra::rast"),
    paste0("whLayers:", whLayers),
    paste0("layerNames:", layerNams),
    paste0(tagFilesToLoad, ":", basename2(obj2))
  )
}

## non-exported path functions --------------------------------------------------

absoluteBase <- function(relToWhere, cachePath, ...) {
  if (identical(relToWhere, "cachePath") && !is.null(cachePath)) {
    ab <- cachePath
  } else if (identical(relToWhere, "getwd")) {
    ab <- getwd()
  } else {
    possRelPaths <- modifyListPaths(cachePath, ...)
    if (relToWhere %in% names(possRelPaths)) {
      ab <- try(possRelPaths[[relToWhere]])
    } else {
      ab <- try(possRelPaths[[1]])
    }
    if (is(ab, "try-error")) browser()
  }

  ab
}

#' @importFrom utils modifyList
modifyListPaths <- function(cachePath, ...) {
  possRelPaths <- list()
  if (!missing(cachePath))
    possRelPaths$cachePath <- cachePath
  dots <- list(...)
  if (length(dots)) {
    if (is(dots[[1]], "list")) {
      if (is.null(names(dots[[1]])))
        stop("wrapSpatRaster and unwrapSpatRaster require named list passed to dots")
      possRelPaths <- modifyList(dots[[1]], possRelPaths)
    }
  }
  possRelPaths <- append(possRelPaths, list(getwd = getwd()))
}

relativeToWhat <- function(file, cachePath, ...) {
  possRelPaths <- modifyListPaths(cachePath, ...)

  foundAbs <- FALSE
  dirnameFile <- dirname(file)
  whSame <- rep(FALSE, length(file))
  pc <- rep("", length(file))
  for (nams in names(possRelPaths)) {
    pc[!whSame] <- mapply(fn = file[!whSame], function(fn)
      fs::path_common(c(dirname(fn), possRelPaths[[nams]]))
    )

    out <- sapply(possRelPaths[[nams]], fs::path_rel, path = pc) |> as.character()
    whSame <- pc == dirnameFile
    if (all(whSame)) {
      out <- list(out)
      names(out) <- nams
      foundAbs <- TRUE
      break
    }
  }
  if (isFALSE(foundAbs)) {
    for (nams in names(possRelPaths)) {
      out <- dirname(file)
      names(out) <- ""
      if (FALSE) { # this is for rebuilding relative against
        # poss <- fs::path_common(c(file, possRelPaths[nams]))
        # if (!identical(poss, possRelPaths[nams])) {
        #   fileRel <- makeRelative(file, poss)
        #   rel <- makeRelative(possRelPaths[nams], poss)
        #   relWithDots <- rep("..", length(strsplit(rel, "/|\\\\")[[1]]))
        #   poss <- file.path(paste(relWithDots, collapse = "/"), dirname(fileRel))
        #   out <- poss
        #   out <- list(out)
        #   names(out) <- nams
        #   foundAbs <- TRUE
        #   break
        # }
        # names(out) <- nams
      }
    }
  }

  out
}

## non-exported wrap functions --------------------------------------------------

wrapSpatRaster <- function(obj, cachePath, ...) {
  fns <- Filenames(obj, allowMultiple = FALSE)

  cls <- class(obj)
  fnsMulti <- Filenames(obj, allowMultiple = TRUE)
  obj2 <- asPath(Filenames(obj, allowMultiple = FALSE))
  nlyrsInFile <- as.integer(terra::nlyr(terra::rast(fns)))
  layerNams <- paste(names(obj), collapse = layerNamesDelimiter)

  ## A file-backed raster can:
  ## 1) not be using all the layers in the file; and
  ## 2) have layer names renamed.
  whLayers <- seq_along(names(obj))
  if (!identical(nlyrsInFile, length(names(obj)))) {
    rr <- terra::rast(fns);
    objDigs <- unlist(lapply(layerNams, function(ln) .robustDigest(obj[[ln]][])))
    digs <- character()
    whLayers <- integer()

    # don't need to go through all layers if the current file has only some; run through from start
    for (ln in seq_len(terra::nlyr(rr))) {
      digs[ln] <- .robustDigest(rr[[ln]][])
      if (digs[ln] %in% objDigs)
        whLayers <- c(ln, whLayers)
      if (all(digs %in% objDigs))
        break
    }
  }
  obj <- asPath(fnsMulti)

  relToWhere <- relativeToWhat(obj, cachePath, ...)
  # if ("" %in% names(relToWhere)) {
  #   # absBase <- browser()
  # }
  # absBase <- absoluteBase(names(relToWhere), cachePath, ...)
  relPath <- unname(unlist(relToWhere))
  relName <- file.path(relPath, basename2(obj))

  tags <- tagsSpatRaster(obj, relToWhere, relName, cls, whLayers, layerNams, obj2)
  attr(obj, "tags") <- tags

  # c(
  #   attr(obj, "tags"),
  #   paste0(tagRelToWhere, ":", names(relToWhere)),
  #   paste0(tagOrigFilename, ":", basename2(obj)),
  #   paste0(tagOrigRelName, ":", relName),
  #   # paste0("origDirname:", dirname(obj)),
  #   paste0("fromDisk:", TRUE),
  #   paste0("class:", cls),
  #   paste0("fileFormat:", tools::file_ext(obj)),
  #   paste0("saveRawFile:", TRUE),
  #   paste0("loadFun:", "terra::rast"),
  #   paste0("whLayers:", whLayers),
  #   paste0("layerNames:", layerNams),
  #   paste0(tagFilesToLoad, ":", basename2(obj2))
  # )
  obj
}

unwrapSpatRaster <- function(obj, cachePath, ...) {
  fns <- Filenames(obj)
  if (isTRUE(any(nchar(fns) > 0))) {
    tags <- attr(obj, "tags")
    if (!is.null(tags)) {
      if (!is.null(cachePath)) {
        filenameInCache <- CacheStoredFile(cachePath,
                                           # cacheId = tools::file_path_sans_ext(basename(obj)),
                                           obj = obj
        )
        feObjs <- file.exists(obj)
        if (any(feObjs))
          unlink(obj[feObjs])
        # fnToLoad <- fns
        newFiles <- remapFilenames(fns, tags, cachePath, ...)
        fromFiles <- unlist(filenameInCache)
      } else {
        newFiles <- remapFilenames(tags = tags, cachePath = cachePath, ...)
        fromFiles <- unlist(fns) # fnToLoad <- newFiles$newName
      }
      hardLinkOrCopy(fromFiles, newFiles$newName, verbose = 0)
      # tags <- parseTags(tags)
      # origRelName <- extractFromCache(tags, tagOrigRelName)
      # origFilename <- extractFromCache(tags, tagOrigFilename) # tv[tk == tagOrigFilename]
      # relToWhere <- extractFromCache(tags, "relToWhere")
      # # possPaths <- modifyListPaths(cachePath, ...)
      # absBase <- absoluteBase(relToWhere, cachePath, ...)
      # newName <- file.path(absBase, origRelName)

      # if (FALSE) {
      #   isAbs <- isAbsolutePath(origRelName)
      #   if (any(isAbs) || is.null(cachePath)) { # means that it had a specific path, not just relative
      #     newName2 <- file.path(normPath(extractFromCache(tags, "origDirname")), origFilename)
      #   } else {
      #     newName2 <- file.path(cachePath, origRelName)
      #   }
      # }

      # if (!identical(newName, newName2)) browser()

      # whFiles <- newFiles$newName[match(basename(extractFromCache(tags, tagFilesToLoad)), origFilename)]

      # if (!is.null(cachePath)) {
      #   filenameInCache <- CacheStoredFile(cachePath,
      #                                      # cacheId = tools::file_path_sans_ext(basename(obj)),
      #                                      obj = obj
      #   )
      #   feObjs <- file.exists(obj)
      #   if (any(feObjs))
      #     unlink(obj[feObjs])
      #   fnToLoad <- fns
      #   hardLinkOrCopy(unlist(filenameInCache), fns, verbose = 0)
      # } else {
      #   fnToLoad <- newFiles$newName
      #   hardLinkOrCopy(unlist(fns), newFiles$newName, verbose = 0)
      # }

      obj <- eval(parse(text = extractFromCache(newFiles$tagsParsed, "loadFun")))(newFiles$whFiles)
      possNames <- strsplit(extractFromCache(newFiles$tagsParsed, "layerNames"), split = layerNamesDelimiter)[[1]]
      namsObjs <- names(obj)
      if (!identical(possNames, namsObjs)) {
        whLayers <- as.integer(extractFromCache(newFiles$tagsParsed, "whLayers"))
        if (length(whLayers) != length(namsObjs)) {
          obj <- obj[[whLayers]]
        }
      }

      # names can be wrong e.g., with "nextNumericName" ... habitatQuality_1 instead of habitatQuality.
      #  Should use the one without the `nextNumericName`
      names(obj) <- possNames
    }
  }
  obj
}

unwrapRaster <- function(obj, cachePath, cacheId) {
  origFilenames <- if (is(obj, "Raster")) {
    Filenames(obj) # This is legacy piece which allows backwards compatible
  } else {
    obj$origRaster
  }

  filesExist <- file.exists(origFilenames)
  cacheFilenames <- Filenames(obj)
  filesExistInCache <- file.exists(cacheFilenames)
  if (any(!filesExistInCache)) {
    fileTails <- gsub("^.+(rasters.+)$", "\\1", cacheFilenames)
    correctFilenames <- file.path(cachePath, fileTails)
    filesExistInCache <- file.exists(correctFilenames)
    if (all(filesExistInCache)) {
      cacheFilenames <- correctFilenames
    } else {
      stop("File-backed raster files in the cache are corrupt for cacheId: ", cacheId)
    }
  }
  out <- hardLinkOrCopy(cacheFilenames[filesExistInCache],
                        origFilenames[filesExistInCache],
                        overwrite = TRUE
  )

  newOutput <- updateFilenameSlots(obj$cacheRaster,
                                   Filenames(obj, allowMultiple = FALSE),
                                   newFilenames = grep("\\.gri$", origFilenames, value = TRUE, invert = TRUE)
  )
  obj <- newOutput
  obj <- .setSubAttrInList(obj, ".Cache", "newCache", FALSE)
  obj
}

## misc non-exported stuff ------------------------------------------------------
grepStartsTwoDots <- "^\\.\\."

## exported generics and functions ----------------------------------------------

### .tagsByClass ----------------------------------------------------------------

#' Exported generics and methods
#'
#' There are a number of generics that are exported for other packages to use.
#' These are listed below. They are not intended for use by normal users; rather,
#' they are made available for package developers to build specific methods.
#'
#' @details
#' The methods should do as described below.
#'
#' `.tagsByClass` should return a character vector, with a single colon (":") dividing
#' two parts: the tag type and tag value, for the specific class.
#'
#' @return
#' `.tagsByClass` default method returns `NULL`.
#'
#'
#' @param object Any R object.
#'
#' @author Eliot McIntire
#' @export
#' @name ExportedMethods
#' @aliases .tagsByClass
#' @rdname exportedMethods
#' @examples
#' .tagsByClass(character())
#'
setGeneric(".tagsByClass", function(object) {
  standardGeneric(".tagsByClass")
})

#' @export
#' @rdname exportedMethods
setMethod(
  ".tagsByClass",
  signature = "ANY",
  definition = function(object) {
    NULL
  }
)

### .cacheMessage ---------------------------------------------------------------

#' @details
#' `.cacheMessage` should make a call to `message` that gives information about
#' the loaded cached object being returned.
#'
#' @return `.cacheMessage`: nothing; called for its messaging side effect, which,
#' by default, just edits the name of the function into a generic "loaded cached result"
#' message.
#'
#' @param object Any R object.
#' @param functionName A character string indicating the function name
#' @param fromMemoise Logical. If `TRUE`, the message will be about
#'        recovery from memoised copy
#' @inheritParams Cache
#'
#' @author Eliot McIntire
#' @export
#' @rdname exportedMethods
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
#' @rdname exportedMethods
setMethod(
  ".cacheMessage",
  signature = "ANY",
  definition = function(object, functionName, fromMemoise, verbose = getOption("reproducible.verbose", 1)) {
    postMess <- NULL
    whMessage <- .message$LoadedCacheResult()
    if (isTRUE(fromMemoise)) {
      whMessage <- .message$LoadedCacheResult(2)
    } else if (fromMemoise %in% FALSE) {
      if (isTRUE(getOption("reproducible.useMemoise")))
        postMess <- paste0(" ", .message$AddingToMemoised)
    }
    baseMess <- .message$LoadedCache(whMessage, .messageFunctionFn(functionName)) # ELIOT HERE
    if (!is.null(postMess))
      baseMess <- paste0(baseMess, postMess)
    messageCache(baseMess, verbose = verbose)
    return(invisible(whMessage))
  }
)

### .cacheMessageObjectToRetrieve -------------------------------------------------------------

#' @param fullCacheTableForObj The data.table entry from the Cache database for only
#' this `cacheId`, e.g., via `showCache()`.
#' @inheritParams Cache
#' @inheritParams .unwrap
#'
#' @details
#' `.objecxtToRetrieveMessage` is the messaging for recovering an object from Cache.
#'
#' @export
#' @rdname exportedMethods
.cacheMessageObjectToRetrieve <- function(functionName, fullCacheTableForObj, cachePath, cacheId, verbose) {
  objSize <- as.numeric(tail(extractFromCache(fullCacheTableForObj, elem = "file.size"), 1))
  class(objSize) <- "object_size"
  bigFile <- isTRUE(objSize > 1e6)

  fileFormat <- unique(extractFromCache(fullCacheTableForObj, elem = "fileFormat")) # can have a single tif for many entries

  messageCache(.message$ObjToRetrieveFn(functionName), ", ",
               #             messageCache("...(Object to retrieve (fn: ", .messageFunctionFn(functionName), ", ",
               basename2(CacheStoredFile(cachePath, cacheId, format = fileFormat)),
               ")",
               if (bigFile) " is large: ",
               if (bigFile) format(objSize, units = "auto"),
               " ... ",
               verbose = verbose
  )
}

#' @details
#' `.addTagsToOutput` should add one or more attributes to an object, named either
#' `"tags"`, `"call"` or `"function"`. It may be wise to do a "deep" copy within this
#' method, but it may not be necessary.
#'
#' @inheritParams Cache
#'
#' @param object Any R object.
#'
#' @param FUN A function
#'
#' @param preDigestByClass A list, usually from `.preDigestByClass`
#'
#' @return `.addTagsToOutput`: The inputted object but with tags attached.
#'
#' @author Eliot McIntire
#' @export
#' @rdname exportedMethods
#'
setGeneric(".addTagsToOutput", function(object, outputObjects, FUN, preDigestByClass) { # nolint
  standardGeneric(".addTagsToOutput")
})

#' @export
#' @rdname exportedMethods
setMethod(
  ".addTagsToOutput",
  signature = "ANY",
  definition = function(object, outputObjects, FUN, preDigestByClass) { # nolint
    object
  }
)


### .preDigestByClass ---------------------------------------------------------------------------

#' `preDigestByClass`: A function called to do any miscellaneous things to
#' do before `.robustDigest` and after `FUN` call
#'
#' @param object Any R object.
#'
#' @return `.preDigestByClass`: A list with elements that have a difficult time
#'   being digested correctly, e.g., an S4 object with some elements removed and
#'   handled for digesting purposes. The default method for `preDigestByClass` and
#'   simply returns `NULL`.
#'
#' @author Eliot McIntire
#' @export
#' @rdname exportedMethods
#' @examples
#' a <- 1
#' .preDigestByClass(a) # returns NULL in the simple case here.
#'
setGeneric(".preDigestByClass", function(object) { # nolint
  standardGeneric(".preDigestByClass")
})

#' @export
#' @rdname exportedMethods
setMethod(
  ".preDigestByClass",
  signature = "ANY",
  definition = function(object) { # nolint
    NULL
  }
)

### .checkCacheRepo ---------------------------------------------------------------------------

#' `.checkCacheRepo`: normally, `checkPath` can be called directly, and if more
#' friendly, but that does not have class-specific methods.
#'
#' @param object An R object
#' @param create Logical. If TRUE, then it will create the path for cache.
#' @inheritParams Cache
#'
#' @return `.checkCacheRepo`: A character string with a path to a cache repository.
#'
#' @author Eliot McIntire
#' @export
#' @rdname exportedMethods
#' @examples
#' a <- normalizePath(file.path(tempdir(), "test"), mustWork = FALSE)
#' .checkCacheRepo(a, create = TRUE)
#'
setGeneric(".checkCacheRepo", function(object, create = FALSE,
                                       verbose = getOption("reproducible.verbose", 1)) {
  standardGeneric(".checkCacheRepo")
})

#' @export
#' @rdname exportedMethods
#' @include messages.R
setMethod(
  ".checkCacheRepo",
  signature = "ANY",
  definition = function(object, create, verbose = getOption("reproducible.verbose", 1)) {
    cachePath <- tryCatch(checkPath(object, create), error = function(x) {
      cachePath <- if (isTRUE(nzchar(getOption("reproducible.cachePath")[1]))) {
        tmpDir <- .reproducibleTempCacheDir()
        # Test whether the user has accepted the default. If yes, then give message.
        #  If no, then user is aware and doesn't need a message
        if (any(grepl(normPath(tmpDir), normPath(getOption("reproducible.cachePath")))) ||
            any(grepl(normPath(tempdir()), normPath(getOption("reproducible.cachePath"))))) {
          messageCache(.message$NoCachePathSupplied, " and getOption('reproducible.cachePath') is inside a temporary directory;\n",
                       "  this will not persist across R sessions.",
                       verbose = verbose
          )
        }
        getOption("reproducible.cachePath", tmpDir)
      } else {
        messageCache(.message$NoCachePathSupplied, ". Using ", .reproducibleTempCacheDir(), verbose = verbose)
        .reproducibleTempCacheDir()
      }
      checkPath(path = cachePath, create = create)
    })
  }
)

### .prepareOutput ----------------------------------------------------------------------------

#'  @details
#'  `.prepareOutput` allows a package developer to do whatever is needed to prepare
#'  the output on recovery from the cache repository. In other words,
#'  there are cases where the object being cached may have some modifications for
#'  writing to file/database that are not part of the object when returned in
#'  R. Example is a `NULL` entry. A class may need this to be stored on disk,
#'  but `NULL` does not store reliably on disk. So this would convert the
#'  `"NULL"` (character string, stored on disk) to a `NULL` returned to user.
#'  @inheritParams Cache
#'
#' @param object Any R object
#'
#' @return `.prepareOutput`: The object, modified
#'
#' @author Eliot McIntire
#' @export
#' @rdname exportedMethods
#' @examples
#' a <- 1
#' .prepareOutput(a) # does nothing
#'
#' b <- "NULL"
#' .prepareOutput(b) # converts to NULL
#'
#' if (requireNamespace("terra", quietly = TRUE)) {
#'   r <- terra::rast(terra::ext(0, 10, 0, 10), vals = 1:100)
#'
#'   # write to disk manually -- will be in tempdir()
#'   r <- terra::writeRaster(r, filename = tempfile(fileext = ".tif"))
#'
#'   # copy it to the cache repository
#'   r <- .prepareOutput(r, tempdir())
#' }
setGeneric(".prepareOutput", function(object, cachePath, ...) {
  standardGeneric(".prepareOutput")
})

#' @export
#' @rdname exportedMethods
setMethod(
  ".prepareOutput",
  signature = "ANY",
  definition = function(object, cachePath, ...) {
    if (is.character(object)) {
      if (length(object) == 1) {
        # need something to attach tags to if it is actually NULL
        if (identical(object, "NULL")) object <- NULL
      }
    }
    object
  }
)

### .addChangedAttr ---------------------------------------------------------------------------

#' @details
#' `.addChangedAttr` should return the same object, with this a very specific
#' attribute added: it must be named ".Cache", and have a sub-list element named
#' "changed", which must be a logical, `TRUE` or `FALSE`, to describe whether the object
#' has changed or not since last attempt to cache it. This is mostly useful when there
#' are only one or a few sub-elements of e.g., a large list, that are changed. `Cache`
#' will be able to only recover the changed parts, to reduce time required to
#' complete a call to `Cache`.
#'
#' @param object Any R object returned from a function
#' @param preDigest The full, element by element hash of the input arguments to that same function,
#' e.g., from `.robustDigest`
#' @param origArguments These are the actual arguments (i.e., the values, not the names) that
#'        were the source for `preDigest`
#' @param ... Anything passed to methods.
#'
#' @return `.addChangedAttr`: the object, with an attribute ".Cache" and sub-element, "changed",
#'   added set to either `TRUE` or `FALSE`
#'
#' @author Eliot McIntire
#' @export
#' @rdname exportedMethods
#' @examples
#' a <- 1
#' .addChangedAttr(a) # does nothing because default method is just a pass through
setGeneric(".addChangedAttr", function(object, preDigest, origArguments, ...) {
  standardGeneric(".addChangedAttr")
})

#' @export
#' @rdname exportedMethods
setMethod(
  ".addChangedAttr",
  signature = "ANY",
  definition = function(object, preDigest, origArguments, ...) {
    object
  }
)

#' Remap file names
#'
#' Update file path metadata for file-backed objects (e.g., `SpatRasters`).
#' Useful when moving saved objects between projects or machines.
#'
#' @param obj (optional) object whose file path metadata will be remapped
#' @param tags cache tags `data.table` object
#' @param cachePath character string specifying the path to the cache directory or `NULL`
#' @param ... Additional path arguments, passed to `absoluteBase()` and `modifyListPaths()`
#'
#' @export
#' @importFrom fs path_join path_norm
remapFilenames <- function(obj, tags, cachePath, ...) {
  tags <- parseTags(tags)
  origFilename <- extractFromCache(tags, tagOrigFilename) # tv[tk == tagOrigFilename]

  if (missing(obj)) {
    origRelName <- extractFromCache(tags, tagOrigRelName) |> fs::path_norm()
    relToWhere <- extractFromCache(tags, "relToWhere")

    ## NOTE: extractFromCache() is looking for specific tags which may not exist if saved
    ## using earlier versions of the package, and cannot be restored.
    if (is.null(relToWhere) || length(relToWhere) == 0) {
      stop("remapFileNames() cannot restore objects saved using a previous version of 'reproducible'.")
    }

    possRelPaths <- modifyListPaths(cachePath, ...)
    if (relToWhere %in% names(possRelPaths)) {
      absBase <- absoluteBase(relToWhere, cachePath, ...)
    } else {
      absBase <- possRelPaths[[1]]
      isOutside <- grepl(grepStartsTwoDots, origRelName)
      if (any(isOutside)) {
        ## means the relative path is "outside" of something;
        ## strip all ".." if relToWhere doesn't exist
        while (any(grepl(grepStartsTwoDots, origRelName))) {
          origRelName <- gsub(paste0(grepStartsTwoDots, "|(\\\\|/)"), "", origRelName)
        }
      }
    }
    newName <- vapply(origRelName, function(x) {
      if (fs::is_absolute_path(x)) {
        x
      } else {
        fs::path_join(c(absBase, x)) |> fs::path_norm()
      }
    }, character(1))
  } else {
    newName <- obj
  }

  whFiles <- newName[match(basename(extractFromCache(tags, tagFilesToLoad)), origFilename)]
  list(newName = newName, whFiles = whFiles, tagsParsed = tags)
}

#' @details
#' `updateFilenameSlots`: this exists because when copying file-backed rasters, the
#' usual mechanism of `writeRaster` can be very slow. This function allows
#' for a user to optionally create a hard link to the old file, give it a new
#' name, then update the filename slot(s) in the `Raster*` class object. This
#' can be 100s of times faster for large rasters.
#'
#' @return
#' `updateFilenameSlots`: The original object, but with its internal file pointer
#' updated to the `newFilenames`
#'
#' @export
#' @keywords internal
#' @param obj An object. This function only has useful methods for `Raster*`,
#'   with all other classes being simply a pass-through
#' @param curFilenames An optional character vector of filenames currently existing
#'   and that are pointed to in the obj. If omitted, will take from the `obj`
#'   using `Filenames(obj)`
#' @param newFilenames An optional character vector of filenames to use instead of
#'   the curFilenames. This can also be a single directory, in which case the
#'   renaming will be given:
#'   `file.path(newFilenames, basename(Filenames(obj, allowMultiple = FALSE)))`
#' @rdname exportedMethods
updateFilenameSlots <- function(obj, curFilenames, newFilenames, isStack = NULL) {
  UseMethod("updateFilenameSlots")
}

#' @rdname exportedMethods
#' @export
#' @keywords internal
updateFilenameSlots.default <- function(obj, curFilenames, newFilenames, isStack = NULL, ...) {
  if (inherits(obj, "Raster")) {
    if (missing(curFilenames)) {
      curFilenames <- Filenames(obj, allowMultiple = FALSE)
    }

    if (missing(newFilenames)) stop("newFilenames can't be missing: either new filenames or a single directory")
    # if newFilenames is a directory
    areDirs <- dir.exists(newFilenames)
    if (any(areDirs) && length(newFilenames) == 1) {
      newFilenames <- file.path(newFilenames, basename(curFilenames))
    }

    if (length(curFilenames) > 1) {
      for (i in seq_along(curFilenames)) {
        if (is.list(obj)) {
          slot(slot(obj[[i]], "file"), "name") <- newFilenames[i]
        } else {
          slot(slot(slot(obj, "layers")[[i]], "file"), "name") <- newFilenames[i]
        }
      }
    } else {
      if (is.null(isStack)) isStack <- is(obj, "RasterStack")
      if (!isStack) {
        slot(slot(obj, "file"), "name") <- newFilenames
      } else {
        if (length(newFilenames) == 1) {
          newFilenames <- rep(newFilenames, nlayers2(obj))
        }
        for (i in seq_len(nlayers2(obj))) {
          whFilename <- unique(match(
            withoutFinalNumeric(basename(newFilenames)),
            withoutFinalNumeric(basename(curFilenames))
          ))
          isNAwhFn <- is.na(whFilename)
          if (any(isNAwhFn)) {
            whFilename <- i
          }
          slot(slot(obj@layers[[i]], "file"), "name") <- newFilenames[whFilename]
        }
        # }
      }
    }
  }
  obj
}

#' @rdname exportedMethods
#' @export
#' @keywords internal
updateFilenameSlots.list <- function(obj, ...) {
  areRasters <- vapply(obj, is, "RasterLayer", FUN.VALUE = logical(1))
  if (all(areRasters)) {
    .requireNamespace("raster", stopOnFALSE = TRUE)
    # a separate option for list of RasterLayers because curFilename will be
    #   as long as all the filenames because there is a method for lists;
    #   passing this to updateFilaneSlots will fail if it is one RasterLayer
    #   at a time
    out <- updateFilenameSlots(raster::stack(obj), ...)
    out <- raster::unstack(out)
  } else {
    out <- lapply(obj, function(o) {
      updateFilenameSlots(o, ...)
    })
  }
  out
}

#' @rdname exportedMethods
#' @export
#' @keywords internal
updateFilenameSlots.environment <- function(obj, ...) {
  if (is.null(names(obj))) {
    names(obj) <- as.character(seq(obj))
  }
  lapply(obj, function(o) {
    updateFilenameSlots(as.list(o), ...)
  })
}

#' @details
#' `makeMemoiseable` and `unmakeMemoisable` methods are run during `Cache`. The
#' methods should address any parts that will not successfully work for memoising,
#' most notably, `data.table` or other pass-by-reference objects likely need to
#' be deep copied, so that the memoised version doesn't get changed after being
#' stashed in the RAM cache (i.e., memoised)
#'
#' @details
#' `makeMemoiseable` and `unmakeMemoisable` are, by default, just a pass through
#' for most class. `reproducible` only has a method for `data.table` class objects.
#'
#' @param x  An object to make memoisable.
#'           See individual methods in other packages.
#' @return The same object, but with any modifications, especially
#' dealing with saving of environments, which memoising doesn't handle
#' correctly in some cases.
#'
#' @export
#' @rdname exportedMethods
makeMemoisable <- function(x) {
  UseMethod("makeMemoisable")
}

#' @export
#' @rdname exportedMethods
makeMemoisable.default <- function(x) {
  x
}

#' @export
#' @rdname exportedMethods
makeMemoisable.data.table <- function(x) {
  data.table::copy(x)
}

#' @export
#' @rdname exportedMethods
unmakeMemoisable <- function(x) {
  UseMethod("unmakeMemoisable")
}

#' @export
#' @rdname exportedMethods
unmakeMemoisable.default <- function(x) {
  x
}

#' Grep system calls
#'
#' A faster way of grepping the system call stack than just
#' `grep(sys.calls(), pattern = "test")`
#'
#' @param sysCalls The return from `sys.calls()`
#' @param pattern Character, passed to grep
#' @return
#' Numeric vector, equivalent to return from `grep(sys.calls(), pattern = "test")`,
#' but faster if `sys.calls()` is very big.
#'
#' @keywords internal
#' @export
#' @rdname grepSysCalls
.grepSysCalls <- function(sysCalls, pattern) {
  scallsFirstElement <- lapply(sysCalls, function(x) x[1])
  grep(scallsFirstElement, pattern = pattern)
}

#' Deal with class for saving to and loading from Cache or Disk
#'
#' This generic and some methods will do whatever is required to prepare an object for
#' saving to disk (or RAM) via e.g., `saveRDS`. Some objects (e.g., `terra`'s `Spat*`)
#' cannot be saved without first wrapping them. Also, file-backed objects are similar.
#'
#' @param obj Any arbitrary R object.
#' @param ... Arguments passed to methods; default does not use anything in `...`.
#' @inheritParams Cache
#' @inheritParams loadFromCache
#'
#' @return Returns an object that can be saved to disk e.g., via `saveRDS`.
#'
#' @export
#' @rdname dotWrap
.wrap <- function(obj, cachePath, preDigest,  drv = getDrv(getOption("reproducible.drv", NULL)),
                  conn = getOption("reproducible.conn", NULL),
                  verbose = getOption("reproducible.verbose"), outputObjects  = NULL, ...) {
  UseMethod(".wrap")
}

#' @export
#' @rdname dotWrap
.wrap.list <- function(obj, cachePath, preDigest, drv = getDrv(getOption("reproducible.drv", NULL)),
                       conn = getOption("reproducible.conn", NULL),
                       verbose = getOption("reproducible.verbose"), outputObjects = NULL, ...) {

  if (!is.null(outputObjects)) {
    allObjs <- names(obj)
    nullify <- setdiff(allObjs, outputObjects)
    obj[nullify] <- NULL
  }

  attrsOrig <- attributes(obj)
  obj <- lapply(obj, .wrap, preDigest = preDigest, cachePath = cachePath, drv = drv,
                conn = conn, verbose = verbose, ...)
  hasTagAttr <- lapply(obj, function(x) attr(x, "tags"))
  tagAttr <- unname(unlist(hasTagAttr)) # this removed name
  if (length(tagAttr)) {
    if (is.null(attrsOrig[["tags"]])) {
      newList <- tagAttr
    } else {
      newList <- try(c(attrsOrig[["tags"]], tagAttr))
    }
    attrsOrig[["tags"]] <- newList
  }
  if (!is.null(attrsOrig)) {
    for (tt in intersect(names(attrsOrig), c(".Cache", "tags", "call")))
      attr(obj, tt) <- attrsOrig[[tt]]
  }
  obj
}

#' @export
#' @rdname dotWrap
.wrap.environment <- function(obj, cachePath, preDigest, drv = getDrv(getOption("reproducible.drv", NULL)),
                              conn = getOption("reproducible.conn", NULL),
                              verbose = getOption("reproducible.verbose"), outputObjects = NULL, ...) {

  if (!is.null(outputObjects)) {
    allObjs <- ls(obj)
    nullify <- setdiff(allObjs, outputObjects)
    rm(list = nullify, envir = obj)
  }

  if (length(ls(obj, all.names = T)) > 0) {
    obj2 <- as.list(obj, all.names = TRUE)
    out <- .wrap(obj2, cachePath = cachePath, preDigest = preDigest, drv = drv,
                 conn = conn, verbose = verbose, outputObjects = outputObjects, ...)
    # obj <- Copy(obj)
    obj2 <- list2envAttempts(out, obj)
    if (!is.null(obj2)) obj <- obj2

  }
  obj
}

#' @export
#' @rdname dotWrap
#' @examples
#' # For SpatExtent
#' if (requireNamespace("terra")) {
#'   ex <- terra::ext(c(0, 2, 0, 3))
#'   exWrapped <- .wrap(ex)
#'   ex1 <- .unwrap(exWrapped)
#' }
#'
.wrap.default <- function(obj, cachePath, preDigest, drv = getDrv(getOption("reproducible.drv", NULL)),
                          conn = getOption("reproducible.conn", NULL),
                          verbose = getOption("reproducible.verbose"), ...) {
  rasters <- is(obj, "Raster")
  atts <- attributes(obj)
  reassignAtts <- TRUE
  if (any(rasters)) {
    .requireNamespace("raster", stopOnFALSE = TRUE)
    objOrig <- obj
    obj <- .prepareFileBackedRaster(obj,
                                    repoDir = cachePath,
                                    overwrite = FALSE, drv = drv, conn = conn
    )
    isFromDisk <- raster::fromDisk(obj)

    # have to reset all these attributes on the rasters as they were undone in prev steps
    hasFromDisk <- if (!is.null(atts$tags)) startsWith(atts$tags, "fromDisk") else FALSE

    if (any(hasFromDisk)) {
      atts$tags[hasFromDisk] <- paste("fromDisk", sep = ":", isFromDisk)
    } else {
      atts$tags <- c(atts$tags, paste("fromDisk", sep = ":", isFromDisk))
    }

    attr(obj, "tags") <- atts$tags
    obj <- .setSubAttrInList(obj, ".Cache", "newCache", atts$.Cache$newCache)
    attr(obj, "call") <- atts$call

    if (!identical(attr(obj, ".Cache")$newCache, atts$.Cache$newCache)) {
      stop("attributes are not correct 6")
    }
    if (!identical(attr(obj, "call"), atts$call)) {
      stop("attributes are not correct 7")
    }
    if (!identical(attr(obj, "tags"), atts$tags)) {
      stop("attributes are not correct 8")
    }

    if (!is.null(atts[["function"]])) {
      attr(obj, "function") <- atts[["function"]]
      if (!identical(attr(obj, "function"), atts[["function"]])) {
        stop("There is an unknown error 04")
      }
    }
    if (isFromDisk) {
      obj <- list(origRaster = Filenames(objOrig), cacheRaster = obj)
      attr(obj, "tags") <- c(
        attributes(obj$cacheRaster)$tags,
        paste0("origRaster:", obj$origRaster),
        paste0("cacheRaster:", Filenames(obj))
      )
    }
  }

  if (is(obj, "SpatVectorCollection")) {
    obj <- .wrap(as.list(obj))
    class(obj) <- "PackedSpatVectorCollection"
  }

  if (any(inherits(obj, c("SpatVector", "SpatRaster", "SpatExtent", "data.table")))) {
    if (!requireNamespace("terra", quietly = TRUE)) {
      stop("Please install terra package")
    }
    messageCache("wrapping terra object for saving...", verboseLevel = 2, verbose = verbose)
    # attrs <- attr(obj, ".Cache")

    # next is for terra objects --> terra::wrap is ridiculously slow for SpatVector objects; use
    #   custom version in reproducible where here
    useWrap <- TRUE
    if (inherits(obj, "SpatRaster")) {
      if (all(nzchar(Filenames(obj)))) {
        useWrap <- FALSE
        obj <- wrapSpatRaster(obj, cachePath, ...)
      }
    } else if (is(obj, "SpatVector") && !missing(cachePath)) {
      useWrap <- FALSE
      obj <- wrapSpatVector(obj)
    } else if (is(obj, "SpatExtent")) {
      obj <- list(
        xmin = terra::xmin(obj), xmax = terra::xmax(obj),
        ymin = terra::ymin(obj), ymax = terra::ymax(obj)
      )
      attr(obj, "class") <- "PackedSpatExtent2"
      useWrap <- FALSE
      reassignAtts <- FALSE
    }
    if (is(obj, "data.table")) {
      obj <- data.table::copy(obj)
      useWrap <- FALSE
    }

    if (useWrap) {
      obj <- terra::wrap(obj)
    } # let method dispatch work

    # attr(obj, ".Cache") <- attrs

    messageCache("\b Done!", verboseLevel = 2, verbose = verbose)
  }

  # put attributes back on the potentially packed object
  if (isTRUE(reassignAtts))
    obj <- attributesReassign(atts, obj)

  obj
}

#' @export
#' @rdname dotWrap
.unwrap.default <- function(obj, cachePath, cacheId,
                            drv = getDrv(getOption("reproducible.drv", NULL)),
                            conn = getOption("reproducible.conn", NULL), ...) {
  atts <- attributes(obj)
  if (any(inherits(obj, "PackedSpatVectorCollection"))) {
    obj <- lapply(obj, .unwrap)
    obj <- terra::svc(obj)
  }
  if (any(inherits(obj, c("PackedSpatVector", "PackedSpatRaster", "PackedSpatExtent2")))) {
    if (!requireNamespace("terra")) stop("Please install.packages('terra')")
    if (any(inherits(obj, "PackedSpatVector"))) {
      obj <- terra::vect(obj)
    } else if (any(inherits(obj, "PackedSpatRaster"))) {
      obj <- terra::rast(obj)
    } else if (any(inherits(obj, "PackedSpatExtent2"))) {
      obj <- terra::ext(unlist(obj))
    }
  } else if (any(inherits(obj, "data.table"))) {
    obj <- data.table::copy(obj)
  } else if (is(obj, "Path")) {
    obj <- unwrapSpatRaster(obj, cachePath, ...)
  }
  # put attributes back on the potentially packed object
  obj <- attributesReassign(atts, obj)

  obj
}

#' @export
#' @param cacheId Used strictly for messaging. This should be the cacheId of the object being recovered.
#' @rdname dotWrap
.unwrap <- function(obj, cachePath, cacheId,
                    drv = getDrv(getOption("reproducible.drv", NULL)),
                    conn = getOption("reproducible.conn", NULL), ...) {
  UseMethod(".unwrap")
}

#' @export
#' @rdname dotWrap
.unwrap.environment <- function(obj, cachePath, cacheId,
                                drv = getDrv(getOption("reproducible.drv", NULL)),
                                conn = getOption("reproducible.conn", NULL), ...) {
  # the as.list doesn't get everything. But with a simList, this is OK; rest will stay
  atts <- attributes(obj)
  # if (!is.null(obj$fireSense_dataPrepFit$.objects$studyAreaUnion)) browser()
  objList <- as.list(obj, all.names = TRUE) # don't overwrite everything, just the ones in the list part; but need .mods, .objects etc.

  outList <- .unwrap(objList, cachePath = cachePath, cacheId = cacheId, drv = drv, conn = conn, ...)
  output2 <- list2envAttempts(outList, obj) # don't return it if the list2env retured nothing (a normal environment situation; not simList)
  if (!is.null(output2)) obj <- output2

  # put attributes back on the potentially packed object
  obj <- attributesReassign(atts, obj)

  obj
}

#' @export
#' @rdname dotWrap
.unwrap.list <- function(obj, cachePath, cacheId,
                         drv = getDrv(getOption("reproducible.drv", NULL)),
                         conn = getOption("reproducible.conn", NULL), ...) {
  atts <- attributes(obj)
  anyNames <- names(obj)
  isSpatVector <- if (is.null(anyNames)) FALSE else all(names(obj) %in% spatVectorNamesForCache)
  if (isTRUE(isSpatVector)) {
    obj <- unwrapSpatVector(obj)
  } else {
    isRaster <- isTRUE("cacheRaster" %in% names(obj))
    if (isRaster) {
      obj <- unwrapRaster(obj, cachePath, cacheId)
    } else {
      obj <- lapply(obj, function(out) {
        .unwrap(out, cachePath, cacheId, drv, conn, ...)
      })
    }
  }
  # put attributes back on the potentially packed object
  obj <- attributesReassign(atts, obj)

  obj
}
