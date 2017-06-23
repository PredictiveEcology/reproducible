if (getRversion() >= "3.1.0") {
  utils::globalVariables(c(".", "artifact", "createdDate", "tagKey", "tagValue"))
}

################################################################################
#' Cache method that accommodates environments, S4 methods, Rasters
#'
#' This function takes elements from \code{\link[archivist]{cache}}, with
#' four very critical modifications:
#' 1) the \code{archivist} package detects different environments as different;
#' 2) it also does not detect S4 methods correctly due to method inheritance;
#' 3) it does not detect objects that have file-base storage of information
#' (specifically \code{\link[raster]{RasterLayer-class}} objects).
#' 4) the default hashing method is relatively slow
#' This version of the \code{Cache} function accommodates those four special,
#' though quite common, cases by:
#' 1) converting any environments into list equivalents;
#' 2) identifying the dispatched S4 method (including those made through
#' inheritance) before \code{\link[fastdigest]{fastdigest}} is called so the correct
#' method is being cached;
#' and 3) by running \code{\link[digest]{digest}} on the linked file. Currently,
#' only file-backed \code{Raster*} objects are digested (e.g., not \code{ff} objects,
#' or any other R object where the data is in a file, rather than RAM object).
#' 4) We use \code{\link[fastdigest]{fastdigest}} internally when the object is
#' in RAM (i.e., not for file-backed objects0) which appears to be up to
#' 10x faster than \code{\link[digest]{digest}}.
#'
#' \code{Cache} (uppercase C) is used here so that it is not confused with and does
#' not mask the \code{archivist::cache} function.
#'
#'
#' @note As indicated above, several objects require pre-treatment before
#' caching will work as expected. The function \code{robustDigest} accommodates this.
#' It is an S4 generic, meaning that developers can produce their own methods for
#' different classes of objects. Currently, there are methods for several types
#' of classes. See \code{\link{robustDigest}} .
#'
#' See \code{\link{robustDigest}} for other specifics for other classes.
#'
#' @inheritParams archivist::cache
#' @inheritParams archivist::saveToLocalRepo
#' @include cache-helpers.R
#'
#' @param objects Character vector of objects to be digested. This is only applicable
#'                if there is a list, environment or simList with named objects
#'                within it. Only this/these objects will be considered for caching,
#'                i.e., only use a subset of
#'                the list, environment or simList objects.
#'
#' @param outputObjects Optional character vector indicating which objects to
#'                      return. This is only relevant for \code{simList} objects
#'
#' @param cacheRepo A repository used for storing cached objects.
#'                  This is optional if \code{Cache} is used inside a SpaDES module.
#'
#' @param compareRasterFileLength Numeric. Optional. When there are Rasters, that
#'        have file-backed storage, this is passed to the length arg in \code{digest}
#'        when determining if the Raster file is already in the database.
#'        Note: uses \code{\link[digest]{digest}} for file-backed Raster.
#'        Default 1e6. Passed to \code{prepareFileBackedRaster}.
#'
#' @param debugCache Logical. If \code{TRUE}, then the returned object from the Cache
#'        function will have two attributes, "debugCache1" and "debugCache2" which
#'        are the entire list(...) and that same object, but
#'        after all "robustDigest" calls, at the moment that it is digested using
#'        \code{fastdigest}, respectively. This \code{attr(mySimOut, "debugCache2")} can
#'        then be compared to
#'        a subsequent call and individual items within the object
#'        \code{attr(mySimOut, "debugCache1")} can be compared.
#' @inheritParams digest::digest
#' @param digestPathContent Logical. Should arguments that are of class "Path"
#'                          (see examples below) have their name digested
#'                          (FALSE, the default), or their
#'                          file contents (TRUE)
#'
#' @return As with \code{\link[archivist]{cache}}, the return is either the return
#' value of the function call or the cached version (i.e., the result from a previous
#' call to this same cached function with identical arguments).
#'
#' If \code{Cache} is called within a SpaDES module, then the cached entry will automatically
#' get 3 extra \code{userTags}: eventTime, eventType, and moduleName. These can then be used in
#' \code{clearCache} to selectively remove cached objects by eventTime, eventType or moduleName.
#'
#' \code{Cache} will add a tag to the artifact in the database
#' called \code{accessed} which will assign the time that it was
#' accessed, either read or write. That way, artifacts can be shown (\code{showCache})
#' or removed \code{clearCache} selectively based on their accessed
#'  dates, rather than only by their
#' creation dates. See example in \code{\link{clearCache}}.
#'
#' @note In general, it is expected that caching will only be used when stochasticity
#' is not relevant, or if a user has achieved sufficient stochasticity (e.g., via
#' sufficient number of calls to \code{experiment}) such that no new explorations
#' of stochastic outcomes are required. It will also be very useful in a
#' reproducible work flow
#'
#' If a function has a path argument, there is some ambiguity about what should be
#' done. Possiblities: digest the string as is (this will be very system specific,
#' meaning a Cache will not work if copied between systems or directories), digest
#' the basename(path), or digest the contents of the file. If paths are passed in as
#' is (i.e,. character string), the result will not be entirely predictable. Instead,
#' one should use the wrapper function asPath(path), and one should decide whether
#' one wants to digest the content of the file (\code{digestPathContent = TRUE})
#' or just the filename (\code{(digestPathContent = FALSE)}). See example.

#'
#' @seealso \code{\link[archivist]{cache}}, \code{\link{robustDigest}}
#'
#' @author Eliot McIntire
#' @docType methods
#' @export
#' @importClassesFrom raster RasterBrick
#' @importClassesFrom raster RasterLayer
#' @importClassesFrom raster RasterLayerSparse
#' @importClassesFrom raster RasterStack
#' @importClassesFrom sp Spatial
#' @importClassesFrom sp SpatialLines
#' @importClassesFrom sp SpatialLinesDataFrame
#' @importClassesFrom sp SpatialPixels
#' @importClassesFrom sp SpatialPixelsDataFrame
#' @importClassesFrom sp SpatialPoints
#' @importClassesFrom sp SpatialPointsDataFrame
#' @importClassesFrom sp SpatialPolygons
#' @importClassesFrom sp SpatialPolygonsDataFrame
#' @importFrom archivist cache loadFromLocalRepo saveToLocalRepo showLocalRepo
#' @importFrom digest digest
#' @importFrom fastdigest fastdigest
#' @importFrom magrittr %>%
#' @importFrom stats na.omit
#' @importFrom utils object.size
#' @rdname cache
#'
#' @examples
#' \dontrun{
#'
#' library(raster)
#' tmpdir <- tempdir()
#' ras <- raster(extent(0,100,0,100), res = 1,
#'               vals = sample(1:5, replace=TRUE, size = 1e4),
#'               crs = "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGS84")
#'
#' # A slow operation, like GIS operation
#' notCached <- projectRaster(ras, crs=crs(ras), res = 5, cacheRepo=tmpdir) #
#' cached <- Cache(projectRaster, ras, crs=crs(ras), res = 5, cacheRepo=tmpdir) #
#' # 2nd time is much faster
#' reRun <- Cache(projectRaster, ras, crs=crs(ras), res = 5, cacheRepo=tmpdir) #
#' all.equal(notCached, reRun) # TRUE meaning the recovered cached version is same
#'                             # as notCached version
#'
#' #Paths -- are character strings, it will take 2 complete passes to before
#' #  a cached copy is used when it is a save event (read or load is different)
#' tmpdir <- file.path(tempdir(), "test")
#' obj <- 1:10
#' Cache(saveRDS, obj, file="filename.rdata", cacheRepo = tmpdir)
#' Cache(saveRDS, obj, file="filename.rdata", cacheRepo = tmpdir)
#' Cache(saveRDS, obj, file="filename.rdata", cacheRepo = tmpdir) # cached copy is loaded
#' # vs. which takes only 1 complete time before cached copy is loaded
#' Cache(saveRDS, obj, file=asPath("filename1.rdata"), cacheRepo = tmpdir)
#' Cache(saveRDS, obj, file=asPath("filename1.rdata"), cacheRepo = tmpdir) #cached copy is loaded
#'
#' }
#'
setGeneric("Cache", signature = "...",
           function(FUN, ..., notOlderThan = NULL,
                    objects = NULL, outputObjects = NULL, algo = "xxhash64",
                    cacheRepo = NULL, compareRasterFileLength = 1e6,
                    userTags = c(), digestPathContent = FALSE,
                    debugCache = FALSE) {
             archivist::cache(cacheRepo, FUN, ..., notOlderThan, algo, userTags = userTags)
})

#' @export
#' @rdname cache
setMethod(
  "Cache",
  definition = function(FUN, ..., notOlderThan, objects, outputObjects,
                        algo, cacheRepo, compareRasterFileLength, userTags,
                        digestPathContent, debugCache) {
    tmpl <- list(...)

    if (missing(notOlderThan)) notOlderThan <- NULL

    # if a simList is in ...
    # userTags added based on object class
    userTags <- c(userTags, unlist(lapply(tmpl, .tagsByClass)))

    # get cacheRepo if not supplied
    if (is.null(cacheRepo)) cacheRepo <- .checkCacheRepo(tmpl)

    if (is(try(archivist::showLocalRepo(cacheRepo), silent = TRUE), "try-error")) {
      suppressWarnings(archivist::createLocalRepo(cacheRepo))
    }

    # get function name and convert the contents to text so digestible
    functionDetails <- getFunctionName(FUN, ...)
    tmpl$.FUN <- functionDetails$.FUN # put in tmpl for digesting

    # remove things in the Cache call that are not relevant to Caching
    if (!is.null(tmpl$progress)) if (!is.na(tmpl$progress)) tmpl$progress <- NULL

    # Do the digesting
    preDigest <- lapply(tmpl, robustDigest, objects = objects,
                        compareRasterFileLength = compareRasterFileLength,
                        algo = algo,
                        digestPathContent = digestPathContent)
    outputHash <- fastdigest(preDigest)

    # compare outputHash to existing Cache record
    localTags <- showLocalRepo(cacheRepo, "tags")
    isInRepo <- localTags[localTags$tag == paste0("cacheId:", outputHash), , drop = FALSE]

    # If it is in the existing record:
    if (NROW(isInRepo) > 0) {
      lastEntry <- max(isInRepo$createdDate)
      lastOne <- order(isInRepo$createdDate, decreasing = TRUE)[1]

      # make sure the notOlderThan is valid, if not, exit this loop
      if (is.null(notOlderThan) || (notOlderThan < lastEntry)) {
        out <- loadFromLocalRepo(isInRepo$artifact[lastOne],
                                 repoDir = cacheRepo, value = TRUE)
        # Class-specific message
        .cacheMessage(out, functionDetails$functionName)

        suppressWarnings(archivist::addTagsRepo(isInRepo$artifact[lastOne],
                               repoDir = cacheRepo,
                               tags = paste0("accessed:", Sys.time())))

        # This allows for any class specific things
        out <- .prepareOutput(out, cacheRepo, ...)

        return(out)
      }
    }

    # RUN the function call
    output <- do.call(FUN, list(...))

    # Delete previous version if notOlderThan violoated --
    #   but do this AFTER new run on prev line, in case function call
    #   makes it crash, or user interrupts long function call and wants
    #   a previous verions
    if (nrow(isInRepo) > 0) {
      if (notOlderThan >= lastEntry) {
        # flush it if notOlderThan is violated
        suppressWarnings(rmFromLocalRepo(isInRepo$artifact[lastOne], repoDir = cacheRepo))
      }
    }

    isNullOutput <- if (is.null(output)) TRUE else FALSE # need something to attach tags to if it is actually NULL
    if (isNullOutput) output <- "NULL"

    attr(output, "tags") <- paste0("cacheId:", outputHash)
    attr(output, "call") <- ""
    if (isS4(FUN)) attr(output, "function") <- FUN@generic

    # Can make new methods by class to add tags to outputs
    outputToSave <- .addTagsToOutput(output, outputObjects, FUN)

    # This is for write conflicts to the SQLite database, i.e., keep trying until it is
    # written
    written <- FALSE
    outputToSaveIsList <- is.list(outputToSave)
    if (outputToSaveIsList) {
      rasters <- unlist(lapply(outputToSave, is, "Raster"))
    } else {
      rasters <- is(outputToSave, "Raster")
    }
    if (any(rasters)) {
      if (outputToSaveIsList) {
        outputToSave[rasters] <- lapply(outputToSave[rasters], function(x)
          prepareFileBackedRaster(x, repoDir = cacheRepo))#, archiveData = TRUE,
      } else {
        outputToSave <- prepareFileBackedRaster(outputToSave, repoDir = cacheRepo)#, archiveData = TRUE,
      }
      attr(outputToSave, "tags") <- attr(output, "tags")
      attr(outputToSave, "call") <- attr(output, "call")
      if (isS4(FUN))
        attr(outputToSave, "function") <- attr(output, "function")
      output <- outputToSave
    }
    if (debugCache) {
      attr(output, "debugCache1") <- attr(outputToSave, "debugCache1") <- list(...)
      attr(output, "debugCache2") <- attr(outputToSave, "debugCache2") <- tmpl
    }

    while (!written) {
      objSize <- .objSizeInclEnviros(outputToSave)
      userTags <- c(userTags,
                    paste0("function:",functionDetails$functionName),
                    paste0("object.size:", objSize),
                    paste0("accessed:", Sys.time()))
      saved <- suppressWarnings(try(saveToLocalRepo(outputToSave, repoDir = cacheRepo,
                                   artifactName = "Cache",
                                   archiveData = FALSE, archiveSessionInfo = FALSE,
                                   archiveMiniature = FALSE, rememberName = FALSE, silent = TRUE,
                                   userTags = userTags),
                   silent = TRUE))

      # This is for simultaneous write conflicts. SQLite on Windows can't handle them.
      written <- if (is(saved, "try-error")) {
        Sys.sleep(0.05)
        FALSE
      } else {
        TRUE
      }
    }

    if (isNullOutput) return(NULL) else return(output)
})

################################################################################
#' Create reproducible digests of objects in R
#'
#' Not all aspects of R objects are captured by current hashing tools in R (e.g.
#' \code{digest::digest}, \code{fastdigest::fastdigest}, \code{knitr} caching,
#' \code{archivist::cache}). This is mostly because many objects have "transient"
#' (e.g., functions have environments), or "disk-backed" features. This function
#' allows for these accommodations to be made and uses \code{\link[fastdigest]{fastdigest}}
#' internally.  Since
#' the goal of using reproducibility is to have tools that are not session specific,
#' this function
#' attempts to strip all session specific information so that the fastdigest
#' works between sessions and operating systems. It is tested under many
#' conditions and object types, there are bound to be others that don't
#' work correctly.
#'
#' @section Classes:
#'
#' \code{Raster*} objects have the potential for disk-backed storage. If
#' the object in the R session is cached using \code{archivist::cache}, only
#' the header component will be assessed for caching. Thus, objects like this
#' require more work. Also, because \code{Raster*} can have a built-in representation
#' for having their data content located on disk, this format will be maintained
#' if the raster already is file-backed, i.e., to create .tif or .grd backed rasters,
#' use writeRaster first, then Cache. The .tif or .grd will be copied to the "raster"
#' subdirectory of the \code{cacheRepo}.
#' Their RAM representation (as an R object) will still be in the usual "gallery" directory.
#' For \code{inMemory} raster objects, they will remain as binary .rdata files.
#'
#' Functions (which are contained within environments) are
#' converted to a text representation via a call to \code{format(FUN)}.
#'
#' Objects contained within a list or environment are recursively hashed
#' using \code{\link[fastdigest]{fastdigest}}, while removing all references to
#' environments.
#'
#' Character strings are first assessed with \code{dir.exists} and \code{file.exists}
#' to check for paths. If they are found to be paths, then the path is hashed with
#' only its filename via \code{basename(filename)}.
#'
#' @param object an object to digest.
#'
#' @param objects Optional character vector indicating which objects are to
#'                be considered while making digestible. This is only relevant
#'                if the object being passed is an environment or list or the like.
#' @inheritParams Cache
#'
#' @return A hash i.e., digest of the object passed in.
#'
#' @seealso \code{\link[archivist]{cache}}.
#' @seealso \code{\link[fastdigest]{fastdigest}}.
#' @importFrom digest digest
#' @importFrom fastdigest fastdigest
#' @docType methods
#' @keywords internal
#' @rdname robustDigest
#' @author Eliot McIntire
#' @export
setGeneric("robustDigest", function(object, objects,
                                    compareRasterFileLength = 1e6,
                                    algo = "xxhash64",
                                    digestPathContent = FALSE) {
  standardGeneric("robustDigest")
})


#' @rdname robustDigest
#' @exportMethod robustDigest
setMethod(
  "robustDigest",
  signature = "ANY",
  definition = function(object, compareRasterFileLength, algo, digestPathContent) {
    fastdigest(object)
})

#' @import parallel
setOldClass("cluster")

#' @rdname robustDigest
#' @exportMethod robustDigest
setMethod(
  "robustDigest",
  signature = "cluster",
  definition = function(object, compareRasterFileLength, algo, digestPathContent) {
    fastdigest(NULL)
})

#' @rdname robustDigest
#' @exportMethod robustDigest
setMethod(
  "robustDigest",
  signature = "function",
  definition = function(object, compareRasterFileLength, algo, digestPathContent) {
    fastdigest(format(object))
})

#' @rdname robustDigest
#' @exportMethod robustDigest
setMethod(
  "robustDigest",
  signature = "expression",
  definition = function(object, compareRasterFileLength, algo, digestPathContent) {
    fastdigest(format(object))
})

#' @rdname robustDigest
#' @exportMethod robustDigest
setMethod(
  "robustDigest",
  signature = "character",
  definition = function(object, compareRasterFileLength, algo, digestPathContent) {
    if (any(unlist(lapply(object, dir.exists)))) {
      unlist(lapply(object, function(x) {
        if (dir.exists(x)) {
          fastdigest::fastdigest(basename(x))
        } else {
          fastdigest::fastdigest(x)
        }
      }))
    } else if (any(unlist(lapply(object, file.exists)))) {
      unlist(lapply(object, function(x) {
        if (file.exists(x)) {
          digest::digest(file = x, length = compareRasterFileLength, algo = algo)
        } else {
          fastdigest::fastdigest(x)
        }
      }))
    } else {
      fastdigest::fastdigest(object)
    }
})

#' @rdname robustDigest
#' @exportMethod robustDigest
setMethod(
  "robustDigest",
  signature = "Path",
  definition = function(object, compareRasterFileLength, algo, digestPathContent) {
    if (digestPathContent) {
      lapply(object, function(x) {
        if (file.exists(x)) {
          digest::digest(file = x,length = compareRasterFileLength, algo = algo)
        } else {
          fastdigest::fastdigest(basename(x))
        }
      })
    } else {
      fastdigest::fastdigest(basename(object))
    }
})

#' @rdname robustDigest
#' @exportMethod robustDigest
setMethod(
  "robustDigest",
  signature = "environment",
  definition = function(object, compareRasterFileLength, algo, digestPathContent) {
    robustDigest(as.list(object, all.names = TRUE),
                 compareRasterFileLength = compareRasterFileLength,
                 algo = algo, digestPathContent = digestPathContent)
})

#' @rdname robustDigest
#' @exportMethod robustDigest
setMethod(
  "robustDigest",
  signature = "list",
  definition = function(object, compareRasterFileLength, algo, digestPathContent) {
    lapply(sortDotsUnderscoreFirst(object), function(x) {
      robustDigest(object = x,
                   compareRasterFileLength = compareRasterFileLength,
                   algo = algo, digestPathContent = digestPathContent)
      })
})

#' @rdname robustDigest
#' @exportMethod robustDigest
setMethod(
  "robustDigest",
  signature = "Raster",
  definition = function(object, compareRasterFileLength, algo, digestPathContent) {
    if (is(object, "RasterStack") | is(object, "RasterBrick")) {
      dig <- suppressWarnings(
        list(dim(object), res(object), crs(object), extent(object),
             lapply(object@layers, function(yy) {
               digestRaster(yy, compareRasterFileLength, algo)
             })
        )
      )
      if (nzchar(object@filename, keepNA = TRUE)) {
        # if the Raster is on disk, has the first compareRasterFileLength characters;
        # uses digest::digest on the file
        dig <- append(dig, digest(file = object@filename, length = compareRasterFileLength))
      }
    } else {
      dig <- suppressWarnings(digestRaster(object, compareRasterFileLength, algo))
    }
    return(dig)
})

#' @rdname robustDigest
#' @exportMethod robustDigest
setMethod(
  "robustDigest",
  signature = "Spatial",
  definition = function(object, compareRasterFileLength, algo, digestPathContent) {
    if (is(object, "SpatialPoints")) {
      aaa <- as.data.frame(object)
    } else {
      aaa <- suppressMessages(broom::tidy(object))
    }

    # The following Rounding is necessary to make digest equal on linux and windows
    for (i in names(aaa)) {
      if (!is.integer(aaa[, i])) {
        if (is.numeric(aaa[, i]))
          aaa[,i] <- round(aaa[, i], 4)
      }
    }

    dig <- fastdigest::fastdigest(aaa)
    return(dig)
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
    listFiles <- dir(file.path(repoDir, "gallery")) %>% strsplit(".rda") %>% unlist()
    toRemove <- !(md5hashInBackpack %in% listFiles)
    md5hashInBackpack[toRemove] %>%
      sapply(., rmFromLocalRepo, repoDir = repoDir)
    return(invisible(md5hashInBackpack[toRemove]))
})

#' Deprecated functions
#' @export
#' @importFrom archivist showLocalRepo rmFromLocalRepo
#' @inheritParams Cache
#' @docType methods
#' @rdname reproducible-deprecated
setGeneric("cache", signature = "...",
           function(cacheRepo = NULL, FUN, ..., notOlderThan = NULL,
                    objects = NULL, outputObjects = NULL, algo = "xxhash64") {
             archivist::cache(cacheRepo, FUN, ..., notOlderThan, algo)
           })

#' @export
#' @rdname reproducible-deprecated
setMethod(
  "cache",
  definition = function(cacheRepo, FUN, ..., notOlderThan, objects,
                        outputObjects, algo) {
    .Deprecated("Cache", package = "reproducible",
                msg = paste0("cache from SpaDES and reproducible is deprecated.\n",
                             "Use Cache with capital C if you want the robust Cache function.\n",
                             "e.g., Cache(",getFunctionName(FUN, ..., overrideCall = "cache")$functionName,
                             ", ", paste(list(...), collapse = ", "), ")"))
    Cache(FUN = FUN, ..., notOlderThan = notOlderThan, objects = objects,
          outputObjects = outputObjects, algo = algo, cacheRepo = cacheRepo)
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
#' @importFrom methods slot is selectMethod showMethods slot<-
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

  if (any(saveFilename != curFilename)) { # filenames are not the same
    if (isFilebacked) {
      shouldCopy <- rep(TRUE, length(curFilename))
      # if (any(file.exists(saveFilename))) {
      #   if (!(compareRasterFileLength == Inf)) {
      #     shouldCopy <- lapply(seq_along(saveFilename), function(ind) {
      #       if (digest(file = saveFilename[ind], length = compareRasterFileLength) ==
      #           digest(file = curFilename[ind], length = compareRasterFileLength)) {
      #         shouldCopy <- FALSE
      #       }
      #     })
      #     if (digest(file = saveFilename, length = compareRasterFileLength) ==
      #         digest(file = curFilename, length = compareRasterFileLength)) {
      #       shouldCopy <- FALSE
      #     }
      #   } else {
      #     shouldCopy = TRUE
      #   }
      # }
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
      if (length(curFilename) > 1) { # for a stack with independent Raster Layers (each with own file)
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
    # } else {
    #   if(isStack) {
    #     slot(obj, "filename")
    #   }
    #   saveFilename <- slot(slot(obj, "file"), "name")
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
    robocopy_bin <- tryCatch(system("where robocopy", intern = TRUE),
                             warning = function(w) NA_character_)

    robocopy <-  if (silent) {
      paste0(robocopy_bin, "", "/purge"[delDestination], " /ETA /NDL /NFL /NJH /NJS ",
             normalizePath(dirname(from), mustWork = TRUE, winslash = "\\"), "\\ ",
             normalizePath(to, mustWork = FALSE, winslash = "\\"),  " ", basename(from))
    } else {
      paste0(robocopy_bin, " ", "/purge"[delDestination], " /ETA /xo ",
             normalizePath(from, mustWork = TRUE, winslash = "\\"), "\\ ",
             normalizePath(to, mustWork = FALSE, winslash = "\\"), " /E"[recursive], " " ,
             basename(from))
    }

    useFileCopy <- if (useRobocopy && !is.na(robocopy_bin)) {
      suppressWarnings(tryCatch(system(robocopy, intern = TRUE), error = function(x) TRUE))
    } else {
      TRUE
    }
  } else if ((os == "linux") || (os == "darwin")) {
    rsync_bin <- tryCatch(system("which rsync", intern = TRUE),
                          warning = function(w) NA_character_)
    opts <- if (silent) " -a " else " -avP "
    rsync <- paste0(rsync_bin, " ", "--delete "[delDestination],
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


#' Recursive copying of nested environments
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
setMethod("Copy",
          signature(object = "ANY"),
          definition = function(object, filebackedDir, ...) {
            # make an outer copy
            if (is.environment(object)) {
              object <- as.list(object, all.names = TRUE)
              wasEnv <- TRUE
            } else {
              wasEnv <- FALSE
            }

            if (is.list(object))
              object <- lapply(object, function(x) Copy(x, filebackedDir, ...))

            if (wasEnv)
              object <- as.environment(object)
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
