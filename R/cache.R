if (getRversion() >= "3.1.0") {
  utils::globalVariables(c(".", "artifact", "createdDate", "tagKey", "tagValue"))
}

################################################################################
#' Cache method that accommodates environments, S4 methods, Rasters
#'
#' @details
#' Caching R objects using \code{\link[archivist]{cache}} has four important limitations:
#' \enumerate{
#'   \item the \code{archivist} package detects different environments as different;
#'   \item it also does not detect S4 methods correctly due to method inheritance;
#'   \item it does not detect objects that have file-base storage of information
#'         (specifically \code{\link[raster]{RasterLayer-class}} objects);
#'   \item the default hashing algorithm is relatively slow.
#' }
#' This version of the \code{Cache} function accommodates those four special,
#' though quite common, cases by:
#' \enumerate{
#'   \item converting any environments into list equivalents;
#'   \item identifying the dispatched S4 method (including those made through
#'         inheritance) before hashing so the correct method is being cached;
#'   \item by hashing the linked file, rather than the Raster object.
#'         Currently, only file-backed \code{Raster*} objects are digested
#'         (e.g., not \code{ff} objects, or any other R object where the data
#'         are on disk instead of in RAM);
#'   \item using \code{\link[fastdigest]{fastdigest}} internally when the object
#'         is in RAM, which can be up to ten times faster than
#'         \code{\link[digest]{digest}}. Note that file-backed objects are still
#'         hashed using \code{\link[digest]{digest}}.
#' }
#'
#' If \code{Cache} is called within a SpaDES module, then the cached entry will automatically
#' get 3 extra \code{userTags}: \code{eventTime}, \code{eventType}, and \code{moduleName}.
#' These can then be used in \code{clearCache} to selectively remove cached objects
#' by \code{eventTime}, \code{eventType} or \code{moduleName}.
#'
#' \code{Cache} will add a tag to the artifact in the database called \code{accessed},
#' which will assign the time that it was accessed, either read or write.
#' That way, artifacts can be shown (using \code{showCache}) or removed (using
#' \code{clearCache}) selectively, based on their access dates, rather than only
#' by their creation dates. See example in \code{\link{clearCache}}.
#' \code{Cache} (uppercase C) is used here so that it is not confused with, and does
#' not mask, the \code{archivist::cache} function.
#'
#' @section Filepaths:
#' If a function has a path argument, there is some ambiguity about what should be
#' done. Possibilities include:
#' \enumerate{
#'   \item hash the string as is (this will be very system specific, meaning a
#'         \code{Cache} call will not work if copied between systems or directories);
#'   \item hash the \code{basename(path)};
#'   \item hash the contents of the file.
#' }
#' If paths are passed in as is (i.e,. character string), the result will not be predictable.
#' Instead, one should use the wrapper function \code{asPath(path)}, which sets the
#' class of the string to a \code{Path}, and one should decide whether one wants
#' to digest the content of the file (using \code{digestPathContent = TRUE}),
#' or just the filename (\code{(digestPathContent = FALSE)}). See examples.
#'
#' @section Stochasticity:
#' In general, it is expected that caching will only be used when stochasticity
#' is not relevant, or if a user has achieved sufficient stochasticity (e.g., via
#' sufficient number of calls to \code{experiment}) such that no new explorations
#' of stochastic outcomes are required. It will also be very useful in a
#' reproducible workflow.
#'
#' @section \code{sideEffect}:
#' If \code{sideEffect} is not \code{FALSE}, then metadata about any files that
#' added to \code{sideEffect} will be added as an attribute to the cached copy.
#' Subsequent calls to this function
#'        will assess for the presence of the new files in the \code{sideEffect} location.
#'        If the files are identical (\code{quick = FALSE}) or their file size is
#'        identical (\code{quick = TRUE}), then the cached copy of the function will
#'        be returned (and no files changed). If there are missing or incorrect files,
#'        then the function will re-run. This will accommodate the situation where the
#'        function call is identical, but somehow the side effect files were modified.
#'        If \code{sideEffect} is logical, then the function will check the
#'        \code{cacheRepo}; if it is a path, then it will check the path. The function will
#'        assess whether the files to be downloaded are found locally
#'        prior to download. If it fails the local test, then it will try to recover from a
#'        local copy if (\code{makeCopy} had been set to \code{TRUE} the first time
#'        the function was run. Currently, local recovery will only work if\code{makeCOpy} was
#'        set to \code{TRUE} the first time \code{Cache}
#'        was run). Default is \code{FALSE}.
#'
#' @note As indicated above, several objects require pre-treatment before
#' caching will work as expected. The function \code{.robustDigest} accommodates this.
#' It is an S4 generic, meaning that developers can produce their own methods for
#' different classes of objects. Currently, there are methods for several types
#' of classes. See \code{\link{.robustDigest}}.
#'
#' See \code{\link{.robustDigest}} for other specifics for other classes.
#'
#' @inheritParams archivist::cache
#' @inheritParams archivist::saveToLocalRepo
#' @include cache-helpers.R
#' @include robustDigest.R
#'
#' @param FUN Either a function or an unevaluated function call (e.g., using
#'            \code{quote}.
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
#'        Default 1e6. Passed to \code{.prepareFileBackedRaster}.
#'
#' @param omitArgs Optional character string of arguments in the FUN to omit from the digest.
#'
#' @param classOptions Optional list. This will pass into \code{.robustDigest} for
#'        specific classes. Should be options that the \code{.robustDigest} knows what
#'        to do with.
#'
#' @param debugCache Character or Logical. Either \code{"complete"} or \code{"quick"} (uses
#'        partial matching, so "c" or "q" work). \code{TRUE} is
#'        equivalent to \code{"complete"}.
#'        If \code{"complete"}, then the returned object from the Cache
#'        function will have two attributes, \code{debugCache1} and \code{debugCache2},
#'        which are the entire \code{list(...)} and that same object, but after all
#'        \code{.robustDigest} calls, at the moment that it is digested using
#'        \code{fastdigest}, respectively. This \code{attr(mySimOut, "debugCache2")}
#'        can then be compared to a subsequent call and individual items within
#'        the object \code{attr(mySimOut, "debugCache1")} can be compared.
#'        If \code{"quick"}, then it will return the same two objects directly,
#'        without evalutating the \code{FUN(...)}.
#'
#' @param sideEffect Logical or path. Deteremines where the function will look for
#'        new files following function completion. See Details.
#'        \emph{NOTE: this argument is experimental and may change in future releases.}
#'
#' @param makeCopy Logical. If \code{sideEffect = TRUE}, and \code{makeCopy = TRUE},
#'        a copy of the downloaded files will be made and stored in the \code{cacheRepo}
#'        to speed up subsequent file recovery in the case where the original copy
#'        of the downloaded files are corrupted or missing. Currently only works when
#'        set to \code{TRUE} during the first run of \code{Cache}. Default is \code{FALSE}.
#'        \emph{NOTE: this argument is experimental and may change in future releases.}
#'
#' @param quick Logical. If \code{sideEffect = TRUE}, setting this to \code{TRUE},
#'        will hash the file's metadata (i.e., filename and file size) instead of
#'        hashing the contents of the file(s). If set to \code{FALSE} (default),
#'        the contents of the file(s) are hashed.
#'        \emph{NOTE: this argument is experimental and may change in future releases.}
#'
#' @inheritParams digest::digest
#'
#' @param digestPathContent Logical. Should arguments that are of class \code{Path}
#'                          (see examples below) have their name digested
#'                          (\code{FALSE}), or their file contents (\code{TRUE}; default).
#'
#' @return As with \code{\link[archivist]{cache}}, returns the value of the
#' function call or the cached version (i.e., the result from a previous call
#' to this same cached function with identical arguments).
#'
#' @seealso \code{\link[archivist]{cache}}, \code{\link{.robustDigest}}
#'
#' @author Eliot McIntire
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
#' @example inst/examples/example_Cache.R
#'
setGeneric(
  "Cache", signature = "...",
  function(FUN, ..., notOlderThan = NULL, objects = NULL, outputObjects = NULL, # nolint
           algo = "xxhash64", cacheRepo = NULL, compareRasterFileLength = 1e6,
           userTags = c(), digestPathContent = TRUE, omitArgs = NULL,
           classOptions = list(),
           debugCache = character(),
           sideEffect = FALSE, makeCopy = FALSE, quick = FALSE) {
    archivist::cache(cacheRepo, FUN, ..., notOlderThan, algo, userTags = userTags)
})

#' @export
#' @rdname cache
setMethod(
  "Cache",
  definition = function(FUN, ..., notOlderThan, objects, outputObjects,  # nolint
                        algo, cacheRepo, compareRasterFileLength, userTags,
                        digestPathContent, omitArgs, classOptions,
                        debugCache, sideEffect, makeCopy, quick) {
    tmpl <- list(...)
    originalDots <- tmpl
    isPipe <- isTRUE(!is.null(tmpl$._pipe))

    # If passed with 'quote'
    if (!is.function(FUN)) {
      parsedFun <- parse(text = FUN)
      evaledParsedFun <- eval(parsedFun[[1]])
      if (is.function(evaledParsedFun)) {
        tmpFUN <- evaledParsedFun
        mc <- match.call(tmpFUN, FUN)
        FUN <- tmpFUN # nolint
        originalDots <- append(originalDots, as.list(mc[-1]))
        tmpl <- append(tmpl, as.list(mc[-1]))
      }
      functionDetails <- list(functionName = as.character(parsedFun[[1]]))
    } else {
      if (!isPipe) {
        functionDetails <- getFunctionName(FUN, ..., isPipe = isPipe)

        # i.e., if it did extract the name
        if (functionDetails$functionName != "internal") {
          if (is.primitive(FUN)) {
            tmpl <- list(...)
          } else {
            tmpl <- as.list(
              match.call(FUN, as.call(list(FUN, ...))))[-1]
          }
        }
      } else {
        functionDetails <- list()
      }
    }

    # get function name and convert the contents to text so digestible
    functionDetails$.FUN <- format(FUN) # nolint

    if (isPipe) {
      if (!is.call(tmpl$._lhs)) {
        # usually means it is the result of a pipe
        tmpl$._pipeFn <- "constant" # nolint
      }

      pipeFns <- paste(lapply(tmpl$._rhss, function(x) x[[1]]), collapse = ", ") %>%
        paste(tmpl$._pipeFn, ., sep = ", ") %>%
        gsub(., pattern = ", $", replacement = "") %>%
        paste0("'", ., "' pipe sequence")

      functionDetails$functionName <- pipeFns
      if (is.function(FUN)) {
        firstCall <- match.call(FUN, tmpl$._lhs)
        tmpl <- append(tmpl, lapply(as.list(firstCall[-1]), function(x) {
          eval(x, envir = tmpl$._envir)
        }))
      } else {
        tmpl <- append(tmpl, as.list(FUN))
      }

      for (fns in seq_along(tmpl$._rhss)) {
        functionName <- as.character(tmpl$._rhss[[fns]][[1]])
        FUN <- eval(parse(text = functionName)) # nolint
        if (is.primitive(FUN)) {
          otherCall <- tmpl$._rhss[[fns]]
        } else {
          otherCall <- match.call(definition = FUN, tmpl$._rhss[[fns]])
        }
        tmpl[[paste0("functionName", fns)]] <- as.character(tmpl$._rhss[[fns]][[1]])
        tmpl[[paste0(".FUN", fns)]] <-
          eval(parse(text = tmpl[[paste0("functionName", fns)]]))
        tmpl <- append(tmpl, as.list(otherCall[-1]))
      }
    }

    tmpl$.FUN <- functionDetails$.FUN # put in tmpl for digesting  # nolint

    if (!is(FUN, "function")) {
      scalls <- sys.calls()
      if (any(startsWith(as.character(scalls), "function_list[[k"))) {
        srch <- search()
        whereRepro <- which(endsWith(srch, "reproducible")) - 1
        if (whereRepro > 1) {
          srchNum <- seq_len(whereRepro)
          for (sr in srchNum) {
            masker <- exists("%>%", srch[sr], inherits = FALSE)
            if (masker) break
          }
        }
        if (masker) {
          stop("It looks like the pipe (%>%) from package:reproducible is masked by ", srch[sr],
               ". Please make sure library(reproducible) is after library(",
               gsub(srch[sr], pattern = "package:", replacement = ""), ")",
               call. = FALSE)
        } else {
          stop("Is the %>% from reproducible masked?")
        }

      } else {
        stop("Can't understand the function provided to Cache.\n",
             "Did you write it in the form: ",
             "Cache(function, functionArguments)?")
      }
    } else {
      scalls <- NULL
    }

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

    if (sideEffect != FALSE) if (isTRUE(sideEffect)) sideEffect <- cacheRepo

    if (is(try(archivist::showLocalRepo(cacheRepo), silent = TRUE), "try-error")) {
      suppressWarnings(archivist::createLocalRepo(cacheRepo))
    }

    # List file prior to cache
    if (sideEffect != FALSE) {
      if (isTRUE(sideEffect)) {
        priorRepo <- list.files(cacheRepo, full.names = TRUE)
      } else {
        priorRepo <- list.files(sideEffect, full.names = TRUE)
      }
    }

    # remove things in the Cache call that are not relevant to Caching
    if (!is.null(tmpl$progress)) if (!is.na(tmpl$progress)) tmpl$progress <- NULL

    # Do the digesting
    dotPipe <- startsWith(names(tmpl), "._") # don't digest the dotPipe elements as they are already
                                             # extracted individually into tmpl list elements
    preDigestByClass <- lapply(seq_along(tmpl[!dotPipe]), function(x) {
      .preDigestByClass(tmpl[!dotPipe][[x]])
    })
    preDigest <- lapply(tmpl[!dotPipe], .robustDigest, objects = objects,
                        compareRasterFileLength = compareRasterFileLength,
                        algo = algo,
                        digestPathContent = digestPathContent,
                        classOptions = classOptions)

    if (!is.null(omitArgs)) {
      preDigest <- preDigest[!(names(preDigest) %in% omitArgs)]
    }

    if (length(debugCache)) {
      if (!is.na(pmatch(debugCache, "quick")))
        return(list(hash = preDigest, content = list(...)))
    }

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
        output <- loadFromLocalRepoMem(isInRepo$artifact[lastOne],
                                 repoDir = cacheRepo, value = TRUE)
        # Class-specific message
        .cacheMessage(output, functionDetails$functionName)

        suppressWarnings(
          archivist::addTagsRepo(isInRepo$artifact[lastOne],
                                 repoDir = cacheRepo,
                                 tags = paste0("accessed:", Sys.time()))
        )

        if (sideEffect != FALSE) {
          #if(isTRUE(sideEffect)) {
            needDwd <- logical(0)
            fromCopy <- character(0)
            cachedChcksum <- attributes(output)$chcksumFiles

            if (!is.null(cachedChcksum)) {
              for (x in cachedChcksum) {
                chcksumName <- sub(":.*", "", x)
                chcksumPath <- file.path(sideEffect, basename(chcksumName))

                if (file.exists(chcksumPath)) {
                  checkDigest <- TRUE
                } else {
                  checkCopy <- file.path(cacheRepo, "gallery", basename(chcksumName))
                  if (file.exists(checkCopy)) {
                    chcksumPath <- checkCopy
                    checkDigest <- TRUE
                    fromCopy <- c(fromCopy, basename(chcksumName))
                  } else {
                    checkDigest <- FALSE
                    needDwd <- c(needDwd, TRUE)
                  }
                }

                if (checkDigest) {
                  if (quick) {
                    sizeCurrent <- lapply(chcksumPath, function(z) {
                      list(basename(z), file.size(z))
                    })
                    chcksumFls <- lapply(sizeCurrent, function(z) {
                      digest::digest(z, algo = algo)
                    })
                  } else {
                    chcksumFls <- lapply(chcksumPath, function(z) {
                      digest::digest(file = z, algo = algo)
                    })
                  }
                  # Format checksum from current file as cached checksum
                  currentChcksum <- paste0(chcksumName, ":", chcksumFls)

                  # List current files with divergent checksum (or checksum missing)
                  if (!currentChcksum %in% cachedChcksum) {
                    needDwd <- c(needDwd, TRUE)
                  } else {
                    needDwd <- c(needDwd, FALSE)
                  }
                }
              }
            #}
            } else {
            message("  There was no record of files in sideEffects")
          }
          if (any(needDwd)) {
            do.call(FUN, list(...))
          }

          if (NROW(fromCopy)) {
            repoTo <- file.path(cacheRepo, "gallery")
            lapply(fromCopy, function(x) {
              file.copy(from = file.path(repoTo, basename(x)),
                        to = file.path(cacheRepo), recursive = TRUE)
            })
          }
        }

        # This allows for any class specific things
        output <- .prepareOutput(output, cacheRepo, ...)

        if (length(debugCache)) {
          if (!is.na(pmatch(debugCache, "complete")) | isTRUE(debugCache))
            output <- .debugCache(output, preDigest, ...)
        }
        return(output)
      }
    }

    # RUN the function call
    if (isPipe) {
      output <- eval(tmpl$._pipe, envir = tmpl$._envir)
    } else {
      output <- do.call(FUN, originalDots)
    }

    # Delete previous version if notOlderThan violated --
    #   but do this AFTER new run on previous line, in case function call
    #   makes it crash, or user interrupts long function call and wants
    #   a previous version
    if (nrow(isInRepo) > 0) {
      # flush it if notOlderThan is violated
      if (notOlderThan >= lastEntry) {
        suppressWarnings(rmFromLocalRepo(isInRepo$artifact[lastOne], repoDir = cacheRepo))
      }
    }

    # need something to attach tags to if it is actually NULL
    isNullOutput <- if (is.null(output)) TRUE else FALSE
    if (isNullOutput) output <- "NULL"

    attr(output, "tags") <- paste0("cacheId:", outputHash)
    attr(output, "call") <- ""

    if (sideEffect != FALSE) {
      if (isTRUE(sideEffect)) {
        postRepo <- list.files(cacheRepo, full.names = TRUE)
      } else {
        postRepo <- list.files(sideEffect, full.names = TRUE)
      }
      dwdFlst <- setdiff(postRepo, priorRepo)
      if (length(dwdFlst > 0)) {
        if (quick) {
          sizecurFlst <- lapply(dwdFlst, function(x) {
            list(basename(x), file.size(file.path(x)))
          })
          cachecurFlst <- lapply(sizecurFlst, function(x) {
            digest::digest(x, algo = algo)
          })
        } else {
          cachecurFlst <- lapply(dwdFlst, function(x) {
            digest::digest(file = x, algo = algo)
          })
        }

        cacheName <- file.path(basename(sideEffect), basename(dwdFlst), fsep = "/")
        attr(output, "chcksumFiles") <- paste0(cacheName, ":", cachecurFlst)

        if (makeCopy) {
          repoTo <- file.path(cacheRepo, "gallery")
          checkPath(repoTo, create = TRUE)
          lapply(dwdFlst, function(x) {
            file.copy(from = x,
                      to = file.path(repoTo), recursive = TRUE)
          })
        }
      }
    }

    if (isS4(FUN)) attr(output, "function") <- FUN@generic

    # Can make new methods by class to add tags to outputs
    outputToSave <- .addTagsToOutput(output, outputObjects, FUN,
                                     preDigestByClass)


    # extract other function names that are not the ones the focus of the Cache call
    otherFns <- .getOtherFnNamesAndTags(scalls = scalls)

    # This is for write conflicts to the SQLite database
    #   (i.e., keep trying until it is written)
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
          .prepareFileBackedRaster(x, repoDir = cacheRepo))
      } else {
        outputToSave <- .prepareFileBackedRaster(outputToSave, repoDir = cacheRepo)
      }
      attr(outputToSave, "tags") <- attr(output, "tags")
      attr(outputToSave, "call") <- attr(output, "call")
      if (isS4(FUN))
        attr(outputToSave, "function") <- attr(output, "function")
      output <- outputToSave
    }
    if (length(debugCache)) {
      if (!is.na(pmatch(debugCache, "complete"))) {
        output <- .debugCache(output, preDigest, ...)
        outputToSave <- .debugCache(outputToSave, preDigest, ...)
      }
    }

    while (!written) {
      objSize <- .objSizeInclEnviros(outputToSave)
      userTags <- c(userTags,
                    paste0("function:", functionDetails$functionName),
                    paste0("object.size:", objSize),
                    paste0("accessed:", Sys.time()),
                    paste0("otherFunctions:", otherFns))
      saved <- suppressWarnings(try(
        saveToLocalRepo(outputToSave, repoDir = cacheRepo, artifactName = "Cache",
                        archiveData = FALSE, archiveSessionInfo = FALSE,
                        archiveMiniature = FALSE, rememberName = FALSE,
                        silent = TRUE, userTags = userTags),
        silent = TRUE
      ))

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


#' Pipe that is Cache-aware
#'
#' A pipe that works with Cache. The code for this is based on a verbatim copy from
#' \url{https://github.com/tidyverse/magrittr/blob/master/R/pipe.R} on Sep 8, 2017,
#' based on commit 81c2e2410ebb7c560a2b4a8384ef5c20946373c3, with enhancements
#' to make it Cache-aware.
#' This version is a drop-in replacement for \code{\link[magrittr]{\%>\%}} and will
#' work identically when there is no Cache. To use this, simply add \code{\%>\% Cache()}
#' to a pipe sequence. This can be in the middle or at the end. See examples. It has
#' been tested with multiple Cache calls within the same (long) pipe.
#'
#' If there is a Cache in the pipe,
#' the behaviour of the pipe is altered. In the magrittr pipe, each step of the
#' pipe chain is evaluated one at a time, in sequence. This will not allow any useful
#' type of caching. Here, if there is a call to \code{Cache} in the pipe sequence,
#' the entire pipe chain before the call to \code{Cache} will have its arguments
#' examined and digested. This digest is compared to the cache repository database.
#' If there is an identical pipe sequence in the Cache respository, then it will return
#' the final result of the entire pipe up to the Cache call. If there is no
#' identical copy in the cache repository, then it will evaluate the pipe sequence as per
#' normal, caching the return value at the point of the \code{Cache} call
#' into the cache repository for later use.
#'
#' @name pipe
#' @importFrom utils getFromNamespace
#' @inheritParams magrittr::`%>%`
#' @importFrom magrittr freduce
#' @export
#' @rdname pipe
#' @examples
#' tmpdir <- file.path(tempdir(), "testCache")
#' checkPath(tmpdir, create = TRUE)
#' a <- rnorm(10, 16) %>% mean() %>% prod(., 6)
#' b <- rnorm(10, 16) %>% mean() %>% prod(., 6) %>% Cache(cacheRepo = tmpdir)
#' d <- rnorm(10, 16) %>% mean() %>% prod(., 6) %>% Cache(cacheRepo = tmpdir)
#' all.equal(b,d) # TRUE
#' all.equal(a,d) # different because 'a' uses a unique rnorm, 'd' uses the Cached rnorm
#'
#' # Can put Cache in the middle of a pipe -- f2 and f4 use "cached result" until Cache
#' f1 <- rnorm(10, 16) %>% mean() %>% prod(., runif(1)) %>% Cache(cacheRepo = tmpdir)
#' f2 <- rnorm(10, 16) %>% mean() %>% prod(., runif(1)) %>% Cache(cacheRepo = tmpdir)
#' f3 <- rnorm(10, 16) %>% mean() %>% Cache(cacheRepo = tmpdir) %>% prod(., runif(1))
#' f4 <- rnorm(10, 16) %>% mean() %>% Cache(cacheRepo = tmpdir) %>% prod(., runif(1))
#' all.equal(f1, f2) # TRUE because the runif is before the Cache
#' all.equal(f3, f4) # different because the runif is after the Cache
#'
#' unlink(tmpdir, recursive = TRUE)
#'
`%>%` <- function(lhs, rhs) {
  # magrittr code below
  parent <- parent.frame()
  env <- new.env(parent = parent)
  mc <- match.call()
  chain_parts <- getFromNamespace("split_chain", ns = "magrittr")(mc, env = env) # nolint
  pipes <- chain_parts[["pipes"]]
  rhss <- chain_parts[["rhss"]]
  lhs <- chain_parts[["lhs"]]
  env[["_function_list"]] <- lapply(1:length(rhss), function(i) {
    getFromNamespace("wrap_function", ns = "magrittr")(rhss[[i]], pipes[[i]], parent)
  })
  env[["_fseq"]] <- `class<-`(eval(quote(function(value) {
    freduce(value, `_function_list`)
  }), env, env), c("fseq", "function"))
  env[["freduce"]] <- freduce
  if (getFromNamespace("is_placeholder", ns = "magrittr")(lhs)) {
    env[["_fseq"]]
  } else {

    # reproducible package code here until end of if statement
    whCache <- startsWith(as.character(rhss), "Cache")

    if (any(whCache)) {
      if (sum(whCache) > 1) whCache[-min(which(whCache))] <- FALSE
      whPreCache <- whCache
      whPreCache[seq(which(whCache), length(whCache))] <- TRUE

      cacheCall <- match.call(Cache, rhss[whCache][[1]])
      cacheArgs <- lapply(cacheCall, function(x) x)
      cacheArgs <- cacheArgs[names(cacheArgs) != "FUN"][-1] # remove FUN and Cache (i.e., the -1)

      args <- list(eval(lhs[[1]]),
        ._pipe = parse(text = paste(c(lhs, rhss[!whPreCache]), collapse = " %>% ")),
        ._pipeFn = as.character(lhs[[1]]),
        ._lhs = quote(lhs),
        ._rhss = quote(rhss[!whPreCache]),
        ._envir = parent)
      args <- append(args, lapply(cacheArgs, eval, envir = parent, enclos = parent))

      result <- withVisible(do.call("Cache", args))

      if (!identical(whPreCache, whCache)) {
        # If Cache call is not at the end of the pipe
        postCacheCall <- parse(text = paste(c(result$value, rhss[(!whCache) & whPreCache]),
                                            collapse = " %>% "))
        result <- withVisible(eval(postCacheCall, envir = parent, enclos = parent))
      }
    } else {
      # end reproducible package code

      # magrittr code below
      env[["_lhs"]] <- eval(lhs, parent, parent)
      result <- withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
    }

    if (getFromNamespace("is_compound_pipe", ns = "magrittr")(pipes[[1L]])) {
      eval(call("<-", lhs, result[["value"]]), parent,
           parent)
    } else {
      if (result[["visible"]])
        result[["value"]]
      else invisible(result[["value"]])
    }
  }
}
