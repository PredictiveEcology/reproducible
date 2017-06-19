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
#' This version of the \code{Cache} function accommodates those three special,
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
#' While \code{Cache} is built to work with any R function, we have also built
#' several accommodations within the \code{SpaDES} package context. Principally,
#' the \code{simList} class has an environment as one of
#' its slots, but also many system specific paths.
#' Some of the details of the \code{simList}-specific features of this \code{Cache}
#' function include:
#' We remove all elements that have an environment as part of their attributes.
#' This is generally functions that are loaded from the modules,
#' but also the \code{.envir} slot in the \code{simList}.
#' Functions are formatted to text before running \code{fastdigest}.
#'
#' Cache (capital C) is a short cut to using SpaDES::cache (which is
#' being deprecated). It has the added benefit that if no cacheRepo is
#' specified, it will choose a smart option. If called
#' from inside a SpaDES module, \code{Cache} will use the cacheRepo from a call
#' to \code{cachePath(sim)}, taking the sim from the call stack. Similarly, if no
#' \code{cacheRepo} is specified, then it will use \code{getOption("spades.cachePath") }, which
#' will, by default, be a temporary location with no persistence between R sessions!
#' To persist between sessions, use \code{SpaDES::setPaths()} every session.
#'
#' \code{Cache} (uppercase C) is also defined so that it is not confused with the
#' \code{archivist::cache} function which will not work in a SpaDES context.
#' If a user would like to use \code{cache} (lowercase c), then it must be
#' always prefixed with \code{SpaDES::cache(  )} so that it does not accidentally
#' call the archivist package version of cache.
#'
#' @section Caching as part of SpaDES:
#'
#' SpaDES has several levels of caching. Each level can be used to a modeler's
#' advantage; and, all can be -- and are often -- used concurrently.
#'
#' @section \code{spades} or \code{experiment}:
#'
#' And entire call
#' to \code{spades} or \code{experiment} can be cached. This will have the effect
#' of eliminating any stochasticity in the model as the output will simply be
#' the cached version of the \code{simList}. This is likely most useful in
#' situations where reproducibility is more important than "new" stochasticity
#' (e.g., building decision support systems, apps, final version of a manuscript).
#'
#' @section Module-level caching:
#'
#' If the parameter \code{.useCache} in the module's metadata
#' is set to TRUE, then the \code{doEvent.moduleName}
#' will be cached. That means that every time that module
#' is called from within a spades or experiment call, \code{Cache} will be called. Only
#' the objects inside the \code{simList} that correspond to the \code{inputObjects} of the
#' module and the \code{outputObjects} from the module (as specified in the module
#' metadata) will be assessed for caching
#' inputs or output, respectively.
#'
#' In general use, module level caching would be mostly useful for modules that have
#' no stochasticity, such as data-preparation modules, GIS modules etc.
#'
#' @section Event-level caching:
#'
#' If the parameter \code{.useCache} in the module's metadata
#' is set to a character or character vector,
#' then that or those event(s) will be cached. That means that every time the event
#' is called from within a spades or experiment call, \code{Cache} will be called.
#' Only
#' the objects inside the \code{simList} that correspond to the \code{inputObjects} or the
#' \code{outputObjects} as defined in the module metadata  will be assessed for caching
#' inputs or output, respectively. The fact that all and only the named \code{inputObjects}
#' and \code{outputObjects} are cached and returned may be inefficient (i.e., it may
#' cache more objects than are necessary) for individual events.
#'
#' Similar to module-level caching, event-level caching would be mostly
#' useful for events that have
#' no stochasticity, such as data-preparation events, GIS events etc.
#'
#' @section Function-level caching:
#'
#' Any function can be cached using:
#' \code{Cache(FUN = functionName, ...)}
#' or
#' \code{cache(cacheRepo = cacheDirectory, FUN = functionName, ...)}.
#' This will be a slight change to a function call, such as:
#' \code{projectRaster(raster, crs = crs(newRaster))}
#' to
#' \code{Cache(projectRaster, raster, crs = crs(newRaster))}.
#'
#' @note Several objects require pre-treatment before successful caching will
#' work. \code{Raster*} objects have the potential for disk-backed storage. If
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
#' @seealso \code{\link[archivist]{cache}}, \code{\link{robustDigest}}
#' @export
#' @importFrom archivist cache loadFromLocalRepo saveToLocalRepo showLocalRepo
#' @importFrom digest digest
#' @importFrom magrittr %>%
#' @importFrom stats na.omit
#' @importFrom fastdigest fastdigest
#' @importFrom utils object.size
#' @importClassesFrom raster RasterLayer
#' @importClassesFrom raster RasterLayerSparse
#' @importClassesFrom raster RasterStack
#' @importClassesFrom raster RasterBrick
#' @importClassesFrom sp Spatial
#' @importClassesFrom sp SpatialLines
#' @importClassesFrom sp SpatialLinesDataFrame
#' @importClassesFrom sp SpatialPixels
#' @importClassesFrom sp SpatialPixelsDataFrame
#' @importClassesFrom sp SpatialPoints
#' @importClassesFrom sp SpatialPointsDataFrame
#' @importClassesFrom sp SpatialPolygons
#' @importClassesFrom sp SpatialPolygonsDataFrame
#' @docType methods
#' @rdname cache
#' @author Eliot McIntire
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
#'
#' # if using with SpaDES
#' mySim <- simInit(times = list(start = 0.0, end = 5.0),
#'                  params = list(.globals = list(stackName = "landscape", burnStats = "testStats")),
#'                  modules = list("randomLandscapes", "fireSpread"),
#'                  paths = list(modulePath = system.file("sampleModules", package = "SpaDES")))
#'
#'   # This functionality can be achieved within a spades call
#'   # compare caching ... run once to create cache
#'   system.time(outSim <- spades(Copy(mySim), cache = TRUE, notOlderThan = Sys.time(),
#'                                .plotInitialTime = NA))
#'   # compare... second time is fast
#'   system.time(outSimCached <- spades(Copy(mySim), cache = TRUE, .plotInitialTime = NA))
#'   all.equal(outSim, outSimCached)
#'
#'   # Function caching
#'   ras <- raster(extent(0,1e3,0,1e3),res = 1)
#'   system.time(map <- Cache(gaussMap, ras, cacheRepo = cachePath(mySim),
#'                            notOlderThan = Sys.time()))
#'   # second time much faster
#'   system.time(mapCached <- Cache(gaussMap, ras, cacheRepo = cachePath(mySim)))
#'
#'   # They are the same
#'   all.equal(map, mapCached)
#'
#'   # Module-level
#'   # In this example, we will use the cache on the randomLandscapes module
#'   # This means that each subsequent call to spades will result in identical
#'   # outputs from the randomLandscapes module (only!).
#'   # This would be useful when only one random landscape is needed
#'   # simply for trying something out, or putting into production code
#'   # (e.g., publication, decision support, etc.)
#'   params(mySim)$randomLandscapes$.useCache <- TRUE
#'   system.time(randomSim <- spades(Copy(mySim), .plotInitialTime = NA,
#'                                  notOlderThan = Sys.time(), debug = TRUE))
#'
#'   # user  system elapsed
#'   # 1.26    0.25    7.00
#'   # Vastly faster
#'   system.time(randomSimCached <- spades(Copy(mySim), .plotInitialTime = NA,
#'                                  debug = TRUE))
#'    # user  system elapsed
#'    # 0.22    0.00    0.24
#'    # Test that only layers produced in randomLandscapes are identical, not fireSpread
#'    layers <- list("DEM","forestAge", "habitatQuality", "percentPine","Fires")
#'    same <- lapply(layers, function(l) identical(randomSim$landscape[[l]],
#'                                         randomSimCached$landscape[[l]]))
#'    names(same) <- layers
#'    print(same) # Fires is not same because it is not in the randomLandscape module that was cached
#'
#'    # Note - one can access cached items manually (rather than simply
#'    #    rerunning the same Cache function again)
#'    if (requireNamespace("archivist")) {
#'      # examine the cache
#'      showCache(mySim)
#'      # get the RasterLayer that was produced with the gaussMap function:
#'      map <- showCache(mySim, userTags = "gaussMap")$artifact %>%
#'        archivist::loadFromLocalRepo(repoDir = cachePath(mySim), value = TRUE)
#'    }
#' }
#'
setGeneric("Cache", signature = "...",
           function(FUN, ..., notOlderThan = NULL,
                    objects = NULL, outputObjects = NULL, algo = "xxhash64",
                    cacheRepo = NULL, compareRasterFileLength = 1e6,
                    userTags = c(), debugCache = FALSE) {
             archivist::cache(cacheRepo, FUN, ..., notOlderThan, algo, userTags = userTags)
           })

#' @export
#' @rdname cache
setMethod(
  "Cache",
  definition = function(FUN, ..., notOlderThan, objects, outputObjects,
                        algo, cacheRepo, compareRasterFileLength, userTags, debugCache) {
    tmpl <- list(...)

    if (missing(notOlderThan)) notOlderThan <- NULL

    # if a simList is in ...
    # userTags added based on object class
    userTags <- c(userTags, unlist(lapply(tmpl, tagsByClass)))

    # get cacheRepo if not supplied
    if(is.null(cacheRepo)) cacheRepo <- checkCacheRepo(tmpl)


    if (is(try(archivist::showLocalRepo(cacheRepo), silent = TRUE), "try-error")) {
      archivist::createLocalRepo(cacheRepo)
    }

    # get function name and convert the contents to text so digestible
    functionDetails <- getFunctionName(FUN, ...)
    tmpl$.FUN <- functionDetails$.FUN # put in tmpl for digesting

    # remove things in the Cache call that are not relevant to Caching
    if (!is.null(tmpl$progress)) if (!is.na(tmpl$progress)) tmpl$progress <- NULL

    # Do the digesting
    preDigest <- lapply(tmpl, recursiveRobustDigest, objects = objects,
                        compareRasterFileLength=compareRasterFileLength,
                        algo=algo)
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
        cacheMessage(out, functionDetails$functionName)

        archivist::addTagsRepo(isInRepo$artifact[lastOne],
                               repoDir = cacheRepo,
                               tags = paste0("accessed:", Sys.time()))

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
        rmFromLocalRepo(isInRepo$artifact[lastOne], repoDir = cacheRepo)
      }
    }

    isNullOutput <- if (is.null(output)) TRUE else FALSE # need something to attach tags to if it is actually NULL
    if (isNullOutput) output <- "NULL"

    attr(output, "tags") <- paste0("cacheId:", outputHash)
    attr(output, "call") <- ""
    if (isS4(FUN)) attr(output, "function") <- FUN@generic

    # Can make new methods by class to add tags to outputs
    outputToSave <- addTagsToOutput(output, outputObjects, FUN)

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
      objSize <- objSizeInclEnviros(outputToSave)
      userTags <- c(userTags,
                    paste0("function:",functionDetails$functionName),
                    paste0("object.size:", objSize),
                    paste0("accessed:", Sys.time()))
      saved <- try(saveToLocalRepo(outputToSave, repoDir = cacheRepo,
                                   artifactName="Cache",
                                   archiveData = FALSE, archiveSessionInfo = FALSE,
                                   archiveMiniature = FALSE, rememberName = FALSE, silent = TRUE,
                                   userTags = userTags),
                   silent = TRUE)

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
#' Remove any reference to environments or filepaths in objects
#'
#' Using \code{\link[fastdigest]{fastdigest}} will include every detail of an object, including
#' environments of functions, including those that are session-specific. Since
#' the goal of using fastdigest::fastdigest is not session specific, this function
#' attempts to strip all session specific information so that the fastdigest
#' works between sessions and operating systems. It is tested under many
#' conditions and object types, there are bound to be others that don't
#' work correctly.
#'
#' This is primarily for internal use only. Especially when
#' caching a \code{simList}.
#'
#' This is a derivative of the class \code{simList}, except that all references
#' to local environments are removed.
#' Specifically, all functions (which are contained within environments) are
#' converted to a text representation via a call to \code{format(FUN)}.
#' Also the objects that were contained within the \code{.envir} slot are hashed
#' using \code{\link[fastdigest]{fastdigest}}.
#' The \code{paths} slot is not used (to allow comparison across platforms); it's
#' not relevant where the objects are gotten from, so long as they are the same.
#' The \code{.envir} slot is emptied (\code{NULL}).
#' The object is then converted to a \code{simList_} which has a \code{.list} slot.
#' The hashes of the objects are then placed in that \code{.list} slot.
#'
#' @param object an object to convert to a 'digestible' state
#'
#' @param objects Optional character vector indicating which objects are to
#'                be considered while making digestible.
#' @inheritParams Cache
#'
#' @return A simplified version of the \code{simList} object, but with no
#'         reference to any environments, or other session-specific information.
#'
#' @seealso \code{\link[archivist]{cache}}.
#' @seealso \code{\link[digest]{digest}}.
#' @importFrom digest digest
#' @importFrom fastdigest fastdigest
#' @docType methods
#' @keywords internal
#' @rdname robustDigest
#' @author Eliot McIntire
#' @export
setGeneric("robustDigest", function(object, objects,
                                      compareRasterFileLength = 1e6,
                                      algo = "xxhash64") {
  standardGeneric("robustDigest")
})


#' @rdname robustDigest
#' @exportMethod robustDigest
setMethod(
  "robustDigest",
  signature = "ANY",
  definition = function(object) {
    fastdigest(object)
  })


#' @import parallel
setOldClass("cluster")

#' @rdname robustDigest
#' @exportMethod robustDigest
setMethod(
  "robustDigest",
  signature = "cluster",
  definition = function(object) {
    fastdigest(NULL)
  })

#' @rdname robustDigest
#' @exportMethod robustDigest
setMethod(
  "robustDigest",
  signature = "function",
  definition = function(object) {
    fastdigest(format(object))
  })

#' @rdname robustDigest
#' @exportMethod robustDigest
setMethod(
  "robustDigest",
  signature = "expression",
  definition = function(object) {
    fastdigest(format(object))
  })

#' @rdname robustDigest
#' @exportMethod robustDigest
setMethod(
  "robustDigest",
  signature = "character",
  definition = function(object, compareRasterFileLength, algo) {
    if (any(unlist(lapply(object, dir.exists)))) {
      fastdigest::fastdigest(basename(object))
    } else if(any(unlist(lapply(object, file.exists)))) {
      digest::digest(file = object,
                     length = compareRasterFileLength,
                     algo = algo)
    } else {
      fastdigest::fastdigest(object)
    }
  })



#' @rdname robustDigest
#' @exportMethod robustDigest
setMethod(
  "robustDigest",
  signature = "environment",
  definition = function(object, objects) {
    recursiveRobustDigest(object, objects)
  })

#' @rdname robustDigest
#' @exportMethod robustDigest
setMethod(
  "robustDigest",
  signature = "list",
  definition = function(object) {
    recursiveRobustDigest(object)
  })

#' @rdname robustDigest
#' @exportMethod robustDigest
setMethod(
  "robustDigest",
  signature = "Raster",
  definition = function(object, compareRasterFileLength, algo) {

    if (is(object, "RasterStack") | is(object, "RasterBrick")) {
      dig <- suppressWarnings(
        list(dim(object), res(object), crs(object), extent(object),
             lapply(object@layers, function(yy) {
               digestRaster(yy, compareRasterFileLength, algo)
             })
        )
      )
      if (nzchar(object@filename, keepNA=TRUE)) {
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
  definition = function(object, compareRasterFileLength, algo) {
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
    .Deprecated("Cache", package="reproducible",
                msg=paste0("cache from SpaDES and reproducible is deprecated.\n",
                          "Use Cache with capital C if you want the robust Cache function.\n",
                          "e.g., Cache(",getFunctionName(FUN, ..., overrideCall = "cache")$functionName,
                          ", ", paste(list(...), collapse=", "), ")"))
    Cache(FUN = FUN, ..., notOlderThan = notOlderThan, objects = objects,
          outputObjects = outputObjects, algo = algo, cacheRepo = cacheRepo)
  })

#' Alternative to \code{archivist::saveToRepo} for rasters
#'
#' Rasters are sometimes file-based, so the normal save mechanism doesn't work.
#' This function creates an explicit save of the file that is backing the raster,
#' in addition to saving the object metadata in the \code{archivist} repository database.
#'
#' @param obj The raster object to save to the repository.
#'
#' @inheritParams Cache
#'
#' @param repoDir Character denoting an existing directory in which an artifact will be saved.
#'
#' @param ... passed to \code{archivist::saveToRepo}
#'
#' @return A raster object and its file backing will be passed to the archivist repository.
#'
#' @importFrom digest digest
#' @importFrom raster filename dataType inMemory writeRaster nlayers
#' @importFrom methods slot is selectMethod showMethods slot<-
#'
#' @docType methods
#' @author Eliot McIntire
#' @rdname prepareFileBackedRaster
#'
prepareFileBackedRaster <- function(obj, repoDir = NULL, compareRasterFileLength = 1e6, ...) {
  isRasterLayer <- TRUE
  isStack <- is(obj, "RasterStack")
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
    saveFilename <- file.path(repoDir, "rasters", basename(curFilename)) %>%
      normalizePath(., winslash = "/", mustWork = FALSE)
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
#' @docType methods
#' @author Eliot McIntire
#' @rdname copyFile
#'
copyFile <- function(from = NULL, to = NULL, useRobocopy = TRUE,
                     overwrite = TRUE, delDestination = FALSE,
                     #copyRasterFile=TRUE, clearRepo=TRUE,
                     create = TRUE, silent = FALSE, recursive = TRUE) {

  origDir <- getwd()
  useFileCopy <- FALSE
  #checkPath(to, create=TRUE)#SpaDES dependency
  if (!dir.exists(to)) to <- dirname(to) # extract just the directory part
  os <- tolower(Sys.info()[["sysname"]])
  if (os == "windows") {
    if (useRobocopy) {
      if (silent) {
        suppressWarnings(useFileCopy <- tryCatch(
          system(paste0("robocopy ", "/purge"[delDestination], " /ETA /NDL /NFL /NJH /NJS ",
                        normalizePath(dirname(from), winslash = "\\"),
                        "\\ ", normalizePath(to, winslash = "\\"),
                        " ", basename(from)), intern = TRUE),
          error = function(x) TRUE)
        )
      } else {
        useFileCopy <- tryCatch(
          system(paste0("robocopy ", "/purge"[delDestination], " /ETA /xo ",
                        normalizePath(from, winslash = "\\"), "\\ ",
                        normalizePath(to, winslash = "\\"), " ", "/E"[recursive], " " , basename(from)), intern = TRUE),
          error = function(x) TRUE)
        # system(paste0("robocopy /E ","/purge"[delDestination]," /ETA ", normalizePath(fromDir, winslash = "\\"),
        #               "\\ ", normalizePath(toDir, winslash = "\\"), "\\"))
      }
    } else {
      useFileCopy <- TRUE
    }
  } else if (os == "linux") {
    if (silent) {
      useFileCopy <- tryCatch(system(paste0("rsync -a ", "--delete "[delDestination], from, " ", to, "/"),
                                     intern = TRUE), error = function(x) TRUE)
    } else {
      useFileCopy <- tryCatch(system(paste0("rsync -avP ", "--delete "[delDestination], from, " ", to, "/"),
                                     intern = TRUE), error = function(x) TRUE)
    }
  } else if (os == "darwin") {
    useFileCopy <- TRUE
  }
  if (isTRUE(useFileCopy))
    file.copy(from = from, to = to, overwrite = overwrite, recursive = FALSE)

  setwd(origDir)
  return(invisible(to))
}


#' Custom tools for digesting objects
#'
#' For reproducibility, there are many features or attributes of objects that must
#' be removed e.g., environments have unique labels, rasters have several infrequently
#' used slots and elements that are not perfectly maintained with manipulation.
#' These customDigest functions attempt to deal with some of the types of problems.
#' In conjunction with \code{\link{robustDigest}}, these are helpers to create
#' consisten cache results.
#'
#' @importFrom fastdigest fastdigest
#' @rdname cacheHelper
#' @author Eliot McIntire
#' @seealso \code{\link{robustDigest}}
#' @inheritParams robustDigest
recursiveRobustDigest <- function(object, objects, compareRasterFileLength, algo) {
  if(is.environment(object)|is.list(object)) {
    lapply(as.list(object, all.names=TRUE), recursiveRobustDigest,
           objects=objects, compareRasterFileLength = compareRasterFileLength,
           algo=algo) # need hidden objects too
  } else {
    robustDigest(object, objects, compareRasterFileLength, algo)
  }
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

