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
#' @include robustDigest.R
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
           function(FUN, ..., notOlderThan = NULL,  # nolint
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
  definition = function(FUN, ..., notOlderThan, objects, outputObjects,  # nolint
                        algo, cacheRepo, compareRasterFileLength, userTags,
                        digestPathContent, debugCache) {
    tmpl <- list(...)
    
    # if (!is(FUN, "function")) stop("Can't understand the function provided to Cache. \n",
    #                                "Did you write it in the form: ",
    #                                "Cache(function, functionArguments)?")

    if (missing(notOlderThan)) notOlderThan <- NULL

    # if a simList is in ...
    # userTags added based on object class
    userTags <- c(userTags, unlist(lapply(tmpl, .tagsByClass)))

    # get cacheRepo if not supplied
    if (is.null(cacheRepo)) { 
      cacheRepo <- .checkCacheRepo(tmpl, create = TRUE)
    } else {
      cacheRepo <- checkPath(cacheRepo, create = TRUE)
    }

    if (is(try(archivist::showLocalRepo(cacheRepo), silent = TRUE), "try-error")) {
      suppressWarnings(archivist::createLocalRepo(cacheRepo))
    }

    # get function name and convert the contents to text so digestible
    functionDetails <- getFunctionName(FUN, ...)
    tmpl$.FUN <- functionDetails$.FUN # put in tmpl for digesting  # nolint

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
    isInRepo <- localTags[localTags$tag == paste0("cacheId:", outputHash), , drop = FALSE] # nolint

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

    # need something to attach tags to if it is actually NULL
    isNullOutput <- if (is.null(output)) TRUE else FALSE
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
          prepareFileBackedRaster(x, repoDir = cacheRepo))
      } else {
        outputToSave <- prepareFileBackedRaster(outputToSave, repoDir = cacheRepo)
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
                    paste0("function:", functionDetails$functionName),
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


#' Deprecated functions
#' @export
#' @importFrom archivist showLocalRepo rmFromLocalRepo
#' @inheritParams Cache
#' @docType methods
#' @rdname reproducible-deprecated
setGeneric("cache", signature = "...",
           function(cacheRepo = NULL, FUN, ..., notOlderThan = NULL,  # nolint
                    objects = NULL, outputObjects = NULL, algo = "xxhash64") {
             archivist::cache(cacheRepo, FUN, ..., notOlderThan, algo)
           })

#' @export
#' @rdname reproducible-deprecated
setMethod(
  "cache",
  definition = function(cacheRepo, FUN, ..., notOlderThan, objects,  # nolint
                        outputObjects, algo) {
    .Deprecated("Cache", package = "reproducible",
                msg = paste0(
                  "cache from SpaDES and reproducible is deprecated.\n",
                  "Use Cache with capital C if you want the robust Cache function.\n",
                  "e.g., Cache(", getFunctionName(FUN, ..., overrideCall = "cache")$functionName,
                  ", ", paste(list(...), collapse = ", "), ")"
                )
    )
    Cache(FUN = FUN, ..., notOlderThan = notOlderThan, objects = objects,
          outputObjects = outputObjects, algo = algo, cacheRepo = cacheRepo)
})
