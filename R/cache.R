if (getRversion() >= "3.1.0") {
  utils::globalVariables(c(".", "artifact", "createdDate", "deeperThan3", "differs",
                           "fun", "hash", "N", "tag", "tagKey", "tagValue"))
}

.reproEnv <- new.env(parent = asNamespace("reproducible"))

#' Cache method that accommodates environments, S4 methods, Rasters, & nested caching
#'
#' @details
#' Caching R objects using \code{\link[archivist]{cache}} has five important limitations:
#' \enumerate{
#'   \item the \code{archivist} package detects different environments as different;
#'   \item it also does not detect S4 methods correctly due to method inheritance;
#'   \item it does not detect objects that have file-base storage of information
#'         (specifically \code{\link[raster]{RasterLayer-class}} objects);
#'   \item the default hashing algorithm is relatively slow.
#'   \item heavily nested function calls may want Cache arguments to propagate through
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
#'   \item Cache will save arguments passed by user in a hidden environment. Any
#'         nested Cache functions will use arguments in this order 1) actual arguments
#'         passed at each Cache call, 2) any inherited arguments from an outer Cache
#'         call, 3) the default values of the Cache function. See section on \emph{Nested
#'         Caching}.
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
#' @section Nested Caching:
#' Commonly, Caching is nested, i.e., an outer function is wrapped in a \code{Cache}
#' function call, and one or more inner functions are also wrapped in a \code{Cache}
#' function call. A user \emph{can} always specify arguments in every Cache function
#' call, but this can get tedious and can be prone to errors. The normal way that
#' \emph{R} handles arguments is it takes the user passed arguments if any, and
#' default arguments for all those that have no user passed arguments. We have inserted
#' a middle step. The order or precedence for any given \code{Cache} function call is
#' 1. user arguments, 2. inherited arguments, 3. default arguments. At this time,
#' the top level \code{Cache} arguments will propagate to all inner functions unless
#' each individual \code{Cache} call has other arguments specified, i.e., "middle"
#' nested \code{Cache} function calls don't propagate their arguments to further "inner"
#' \code{Cache} function calls.  See example.
#'
#' \code{userTags} is unique of all arguments: its values will be appended to the
#' inherited \code{userTags}.
#'
#' @section Caching Speed:
#' Caching speed may become a critical aspect of a final product. For example,
#' if the final product is a shiny app, rerunning the entire project may need
#' to take less then a few seconds at most. There are 3 arguments that affect
#' Cache speed: \code{quick}, \code{length}, and
#' \code{algo}. \code{quick} is passed to \code{.robustDigest}, which currently
#' only affects \code{Path} and \code{Raster*} class objects. In both cases, \code{quick}
#' means that little or no disk-based information will be assessed.
#'
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
#' to digest the content of the file (using \code{quick = FALSE}),
#' or just the filename (\code{(quick = TRUE)}). See examples.
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
#'                if there is a list, environment (or similar) named objects
#'                within it. Only this/these objects will be considered for caching,
#'                i.e., only use a subset of
#'                the list, environment or similar objects.
#'
#' @param outputObjects Optional character vector indicating which objects to
#'                      return. This is only relevant for list, environment (or similar) objects
#'
#' @param cacheRepo A repository used for storing cached objects.
#'                  This is optional if \code{Cache} is used inside a SpaDES module.
#'
#' @param length Numeric. If the element passed to Cache is a \code{Path} class
#'        object (from e.g., \code{asPath(filename)}) or it is a \code{Raster} with file-backing, then this will be
#'        passed to \code{digest::digest}, essentially limiting the number of bytes
#'        to digest (for speed). This will only be used if \code{quick = FALSE}.
#'
#' @param compareRasterFileLength Being deprecated; use \code{length}.
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
#' @param sideEffect Logical or path. Determines where the function will look for
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
#' @param quick Logical. If \code{TRUE},
#'        little or no disk-based information will be assessed, i.e., mostly its
#'        memory content. This is relevant for objects of class \code{character},
#'        \code{Path} and \code{Raster} currently. For class \code{character}, it is ambiguous
#'        whether this represents a character string or a vector of file paths. The function
#'        will assess if it is a path to a file or directory first. If not, it will treat
#'        the object as a character string. If it is known that character strings should
#'        not be treated as paths, then \code{quick = TRUE} will be much faster, with no loss
#'        of information. If it is file or directory, then it will digest the file content,
#'        or \code{basename(object)}. For class \code{Path} objects, the file's metadata
#'        (i.e., filename and file size)
#'        will be hashed instead of the file contents if \code{quick = TRUE}.
#'        If set to \code{FALSE} (default),
#'        the contents of the file(s) are hashed.
#'        If \code{quick = TRUE}, \code{length} is ignored. \code{Raster} objects are treated
#'        as paths, if they are file-backed.
#'
#' @param verbose Logical. This will output much more information about the internals of
#'        Caching, which may help diagnose Caching challenges.
#'
#'
#' @param cacheId Character string. If passed, this will override the calculated hash
#'        of the inputs, and return the result from this cacheId in the cacheRepo.
#'        Setting this is equivalent to manually saving the output of this function, i.e.,
#'        the object will be on disk, and will be recovered in subsequent
#'        This may help in some particularly finicky situations
#'        where Cache is not correctly detecting unchanged inputs. This will guarantee
#'        the object will be identical each time; this may be useful in operational code.
#'
#' @param useCache Logical. If \code{FALSE}, then the entire Caching mechanism is bypassed
#'                 and the function is evaluated as if it was not being Cached.
#'                 Default is \code{getOption("reproducible.useCache")}),
#'                 which is \code{FALSE} by default, meaning use the Cache mechanism. This
#'                 may be useful to turn all Caching on or off in very complex scripts and
#'                 nested functions.
#'
#' @param showSimilar A logical or numeric. Useful for debugging.
#'        If \code{TRUE} or \code{1}, then if the Cache
#'        does not find an identical archive in the cacheRepo, it will report (via message)
#'        the next most similar archive, and indicate which argument(s) is/are different.
#'        If a number larger than \code{1}, then it will report the N most similar archived
#'        objects.
#'
#'
#'
#' @inheritParams digest::digest
#'
#' @param digestPathContent Being deprecated. Use \code{quick}.
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
#' @importFrom archivist createLocalRepo addTagsRepo
#' @importFrom digest digest
#' @importFrom data.table setDT := setkeyv .N .SD
#' @importFrom fastdigest fastdigest
#' @importFrom magrittr %>%
#' @importFrom stats na.omit
#' @importFrom utils object.size tail
#' @rdname cache
#'
#' @example inst/examples/example_Cache.R
#'
setGeneric(
  "Cache", signature = "...",
  function(FUN, ..., notOlderThan = NULL, objects = NULL, outputObjects = NULL, # nolint
           algo = "xxhash64", cacheRepo = NULL, length = 1e6,
           compareRasterFileLength, userTags = c(),
           digestPathContent, omitArgs = NULL,
           classOptions = list(), debugCache = character(),
           sideEffect = FALSE, makeCopy = FALSE,
           quick = getOption("reproducible.quick", FALSE),
           verbose = getOption("reproducible.verbose", FALSE), cacheId = NULL,
           useCache = getOption("reproducible.useCache", TRUE),
           showSimilar = NULL) {
    archivist::cache(cacheRepo, FUN, ..., notOlderThan, algo, userTags = userTags)
  })

#' @export
#' @rdname cache
setMethod(
  "Cache",
  definition = function(FUN, ..., notOlderThan, objects, outputObjects,  # nolint
                        algo, cacheRepo, length, compareRasterFileLength, userTags,
                        digestPathContent, omitArgs, classOptions,
                        debugCache, sideEffect, makeCopy, quick, cacheId, useCache,
                        showSimilar) {
    if (!useCache) {
      message(crayon::green("useCache is FALSE, skipping Cache.",
                            "To turn Caching on, use options(reproducible.useCache = TRUE)"))
      FUN(...)
    } else {

      if (verbose) {
        startCacheTime <- Sys.time()
      }

      if (missing(FUN)) stop("Cache requires the FUN argument")

      if (!missing(compareRasterFileLength)) {
        message("compareRasterFileLength argument being deprecated. Use 'length'")
        length <- compareRasterFileLength
      }
      if (!missing(digestPathContent)) {
        message("digestPathContent argument being deprecated. Use 'quick'.")
        quick <- !digestPathContent
      }

      # Arguments -- this puts arguments into a special reproducible environment
      if (R.version[['minor']] <= "4.0") {
        # match.call changed how it worked between 3.3.2 and 3.4.x MUCH SLOWER
        objs <- ls()[ls() %in% .namesCacheFormals]
        objs <- objs[match(.namesCacheFormals, objs)]# sort so same order as R > 3.4
        args <- mget(objs)
        forms <- lapply(.formalsCache, function(x) eval(x))
        objOverride <- unlist(lapply(objs, function(obj) identical(args[[obj]], forms[[obj]])))
        userCacheArgs <- objs[!objOverride]
        namesUserCacheArgs <- userCacheArgs
      } else {
        mc <- as.list(match.call(expand.dots = TRUE)[-1])
        namesMatchCall <- names(mc)
        userCacheArgs <- match(.namesCacheFormals, namesMatchCall)
        namesUserCacheArgs <- namesMatchCall[na.omit(userCacheArgs)]
        objOverride <- is.na(userCacheArgs)
      }

      if (any(!objOverride)) { # put into .reproEnv
        lsDotReproEnv <- ls(.reproEnv)
        namesMatchCallUserCacheArgs <- namesUserCacheArgs
        prevVals <- namesMatchCallUserCacheArgs %in% lsDotReproEnv

        # userTags is special because it gets appended
        prevUserTags <- if ("userTags" %in% namesMatchCallUserCacheArgs &&
                            "userTags" %in% lsDotReproEnv) {
          TRUE
        } else {
          FALSE
        }

        if (prevUserTags) {
          oldUserTags <- .reproEnv$userTags
          userTags <- c(userTags, .reproEnv$userTags)
          list2env(list(userTags = userTags), .reproEnv)
          on.exit({
            .reproEnv$userTags <- oldUserTags
          }, add = TRUE)
        }

        if (any(!prevVals)) {
          # don't override previous values -- except for userTags
          list2env(mget(namesUserCacheArgs[!prevVals]), .reproEnv)
          on.exit({
            # THe suppressWarnings is about objects that aren't there -- so far only happens
            #  when interrupting a process, which means it is spurious
            suppressWarnings(rm(list = namesUserCacheArgs, envir = .reproEnv))
            if (prevUserTags) {
              .reproEnv$userTags <- oldUserTags
            }
          }, add = TRUE)
        }
      }

      if (any(objOverride)) {
        # get from .reproEnv
        lsDotReproEnv <- ls(.reproEnv)
        prevVals <- .namesCacheFormals[objOverride] %in% lsDotReproEnv
        if (any(prevVals)) {
          list2env(mget(.namesCacheFormals[objOverride][prevVals], .reproEnv), environment())
        }
      }

      tmpl <- list(...)

      # get cacheRepo if not supplied
      if (is.null(cacheRepo)) {
        cacheRepo <- .checkCacheRepo(tmpl, create = TRUE)
      } else {
        cacheRepo <- checkPath(cacheRepo, create = TRUE)
      }

      # memoised part -- NA comes from next few lines -- if quick is NA, then it is a memoise event
      # if (!is.na(quick)) {
      #   if (getOption("reproducible.useMemoise")) {
      #     if (quick) {
      #       rawFunName <- deparse(substitute(FUN, env = whereInStack("Cache")))
      #       funName <- paste0("Cache_", rawFunName)
      #       if (!is.memoised(.memoisedCacheFuns[[funName]])) {
      #         .memoisedCacheFuns[[funName]] <- CacheMem
      #       }
      #       if (!is.null(notOlderThan)) {
      #         if (Sys.time() > notOlderThan) {
      #           forget(.memoisedCacheFuns[[funName]])
      #         }
      #       }
      #       mc <- match.call(expand.dots = TRUE)
      #       mc[[1]] <- as.name(funName)
      #       mc <- as.list(mc)
      #       mc$quick <- NA
      #       mc[[1]] <- NULL
      #       mc$userTags <- c(paste0("memoisedFunction:",funName), mc$userTags)
      #       out <- do.call(.memoisedCacheFuns[[funName]], mc)#)
      #       if (!is.null(out)) {
      #         if (!attr(out, "newCache")) {
      #           md5Hash <- searchInLocalRepo(pattern = attr(out, "tags"), repoDir = cacheRepo)
      #           suppressWarnings( # warning is about time zone
      #             archivist::addTagsRepo(md5Hash,
      #                                  repoDir = cacheRepo,
      #                                  tags = c(paste0("MemAccessed:", Sys.time())))
      #           )
      #           .cacheMessage("", paste("Memoised",rawFunName))
      #         }
      #       }
      #       return(out)
      #     }
      #   }
      # } else {
      #   # if it was NA, then it means TRUE, but memoised too
      #   quick <- TRUE
      # }

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
          if (!is.na(functionDetails$functionName)) {
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

      if (!is.null(functionDetails$functionName)) {
        if (functionDetails$functionName == "do.call") {
          possFunNames <- lapply(substitute(placeholderFunction(...))[-1],
                                                 deparse, backtick = TRUE)
          whatArg <- as.list(match.call(do.call, as.call(append(list(do.call), possFunNames))))$what
          functionDetails$functionName <- whatArg
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

      # This is for Pipe operator -- needs special consideration
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

      if (sideEffect != FALSE) if (isTRUE(sideEffect)) sideEffect <- cacheRepo

      suppressMessages(archivist::createLocalRepo(cacheRepo))

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
      if (!is.null(omitArgs)) {
        tmpl[omitArgs] <- NULL
      }

      # don't digest the dotPipe elements as they are already
      # extracted individually into tmpl list elements
      dotPipe <- startsWith(names(tmpl), "._")

      preDigestByClass <- lapply(seq_along(tmpl[!dotPipe]), function(x) {
        .preDigestByClass(tmpl[!dotPipe][[x]])
      })

      if (verbose) {
        startHashTime <- Sys.time()
      }
      preDigest <- lapply(tmpl[!dotPipe], function(x) {
        # remove the "newCache" attribute, which is irrelevant for digest
        if (!is.null(attr(x, ".Cache")$newCache)) attr(x, ".Cache")$newCache <- NULL
        .robustDigest(x, objects = objects,
                      length = length,
                      algo = algo,
                      quick = quick,
                      classOptions = classOptions)
      })
      preDigestUnlistTrunc <- unlist(.unlistToCharacter(preDigest, 3))

      if (verbose) {
        preDigestUnlist <- .unlistToCharacter(preDigest, 4)#recursive = TRUE)
        endHashTime <- Sys.time()
        verboseDF <- data.frame(
          functionName = functionDetails$functionName,
          component = "Hashing",
          elapsedTime = as.numeric(difftime(endHashTime, startHashTime, units = "secs")),
          units = "secs",
          stringsAsFactors = FALSE
        )

        hashObjectSize <- unlist(lapply(tmpl[!dotPipe], function(x) {
          objSize <- objSize(x, quick = quick)

        }))

        lengths <- unlist(lapply(preDigestUnlist, function(x) length(unlist(x))))
        hashDetails <- data.frame(
          objectNames = rep(names(preDigestUnlist), lengths),
          #objSize = rep(hashObjectSize, lengths),
          hashElements = names(unlist(preDigestUnlist)),
          hash = unname(unlist(preDigestUnlist)),
          stringsAsFactors = FALSE
        )
        preDigestUnlistNames <- unlist(lapply(strsplit(names(unlist(preDigestUnlist)), split = "\\."), #nolint
                                              function(x) paste0(tail(x, 2), collapse = ".")))
        hashObjectSizeNames <- unlist(lapply(strsplit(names(hashObjectSize), split = "\\$"),
                                             function(x) paste0(tail(x, 2), collapse = ".")))
        # hashObjectSizeNames <- unlist(lapply(strsplit(hashObjectSizeNames, split = "\\.y"),
        #                                      function(x) paste0(tail(x, 2), collapse = ".")))
        hashObjectSizeNames <- gsub("\\.y", replacement = "", hashObjectSizeNames)
        hashObjectSizeNames <- unlist(lapply(strsplit(hashObjectSizeNames, split = "\\."),
                                             function(x) paste0(tail(x, 2), collapse = ".")))
        hashDetails$objSize <- NA
        hashDetails$objSize[preDigestUnlistNames %in% hashObjectSizeNames] <-
          hashObjectSize[hashObjectSizeNames %in% preDigestUnlistNames]

        if (exists("hashDetails", envir = .reproEnv)) {
          .reproEnv$hashDetails <- rbind(.reproEnv$hashDetails, hashDetails)
        } else {
          .reproEnv$hashDetails <- hashDetails
          on.exit({
            assign("hashDetailsAll", .reproEnv$hashDetails, envir = .reproEnv)
            print(.reproEnv$hashDetails)
            message("The hashing details are available from .reproEnv$hashDetails")
            rm("hashDetails", envir = .reproEnv)
          }, add = TRUE)
        }

        if (exists("verboseTiming", envir = .reproEnv)) {
          verboseDF$functionName <- paste0("  ", verboseDF$functionName)
          .reproEnv$verboseTiming <- rbind(.reproEnv$verboseTiming, verboseDF)
        } else {
          .reproEnv$verboseTiming <- verboseDF
          on.exit({
            assign("cacheTimings", .reproEnv$verboseTiming, envir = .reproEnv)
            print(.reproEnv$verboseTiming)
            message("This object is also available from .reproEnv$cacheTimings")
            rm("verboseTiming", envir = .reproEnv)
          },
          add = TRUE)
        }
      }

      if (length(debugCache)) {
        if (!is.na(pmatch(debugCache, "quick")))
          return(list(hash = preDigest, content = list(...)))
      }

      outputHash <- fastdigest(preDigest)
      if (!is.null(cacheId)) {
        outputHashManual <- cacheId
        if (identical(outputHashManual, outputHash)) {
          message("cacheId is same as calculated hash")
        } else {
          message("cacheId is not same as calculated hash. Manually searching for cacheId:", cacheId)
        }
        outputHash <- outputHashManual
      }

      # compare outputHash to existing Cache record

      written <- 0
      while (written >= 0) {
        localTags <- suppressWarnings(try(showLocalRepo2(cacheRepo), silent = TRUE))
        #localTags <- suppressWarnings(try(showLocalRepo(cacheRepo, "tags"), silent = TRUE))
        written <- if (is(localTags, "try-error")) {
          Sys.sleep(sum(runif(written + 1,0.05, 0.2)))
          written + 1
        } else {
          -1
        }
      }

      isInRepo <- localTags[localTags$tag == paste0("cacheId:", outputHash), , drop = FALSE]

      # If it is in the existing record:

      if (NROW(isInRepo) > 0) {
        lastEntry <- max(isInRepo$createdDate)
        lastOne <- order(isInRepo$createdDate, decreasing = TRUE)[1]

        # make sure the notOlderThan is valid, if not, exit this loop
        if (is.null(notOlderThan) || (notOlderThan < lastEntry)) {
          if (verbose) {
            startLoadTime <- Sys.time()
          }

          fromMemoise <- NA
          if (getOption("reproducible.useMemoise")) {
            fromMemoise <-
              if (memoise::has_cache(.loadFromLocalRepoMem)(isInRepo$artifact[lastOne],
                                                            repoDir = cacheRepo, value = TRUE)) {
                TRUE
              } else {
                FALSE
              }
            loadFromMgs <- "Loading from memoise version of repo"
            output <- .loadFromLocalRepoMem(isInRepo$artifact[lastOne],
                                            repoDir = cacheRepo, value = TRUE)
            output <- unmakeMemoiseable(output)
            #if (is(output, "simList_")) output <- as(output, "simList")
          } else {
            loadFromMgs <- "Loading from repo"
            output <- loadFromLocalRepo(isInRepo$artifact[lastOne],
                                        repoDir = cacheRepo, value = TRUE)
          }

          if (verbose) {
            endLoadTime <- Sys.time()
            verboseDF <- data.frame(
              functionName = functionDetails$functionName,
              component = loadFromMgs,
              elapsedTime = as.numeric(difftime(endLoadTime, startLoadTime, units = "secs")),
              units = "secs",
              stringsAsFactors = FALSE
            )

            if (exists("verboseTiming", envir = .reproEnv)) {
              .reproEnv$verboseTiming <- rbind(.reproEnv$verboseTiming, verboseDF)
            }
            # on.exit({message("Loading from repo took ", format(endLoadTime - startLoadTime))},
            #   add = TRUE)

          }

          # Class-specific message
          .cacheMessage(output, functionDetails$functionName,
                        fromMemoise = fromMemoise)

          written <- 0
          while (written >= 0) {
            saved <- suppressWarnings(try(silent = TRUE,
                                          addTagsRepo(isInRepo$artifact[lastOne],
                                                      repoDir = cacheRepo,
                                                      tags = paste0("accessed:", Sys.time()))))
            written <- if (is(saved, "try-error")) {
              Sys.sleep(sum(runif(written + 1,0.05, 0.1)))
              written + 1
            } else {
              -1
            }
          }

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
          attr(output, ".Cache")$newCache <- FALSE

          if (verbose) {
            endCacheTime <- Sys.time()
            verboseDF <- data.frame(
              functionName = functionDetails$functionName,
              component = "Whole Cache call",
              elapsedTime = as.numeric(difftime(endCacheTime, startCacheTime, units = "secs")),
              units = "secs",
              stringsAsFactors = FALSE)

            if (exists("verboseTiming", envir = .reproEnv)) {
              .reproEnv$verboseTiming <- rbind(.reproEnv$verboseTiming, verboseDF)
            }
            # on.exit({message("Loading from repo took ", format(endLoadTime - startLoadTime))},
            #   add = TRUE)

          }

          # If it was a NULL, the cacheRepo stored it as "NULL" ... return it as NULL
          if (is.character(output))
            if (identical(as.character(output), "NULL"))
              output <- NULL

          return(output)
        }
      } else {
        # find similar -- in progress
        if (!is.null(showSimilar)) { # TODO: Needs testing
          setDT(localTags)
          userTags2 <- .getOtherFnNamesAndTags(scalls = scalls)
          userTags2 <- c(userTags2, paste("preDigest", names(preDigestUnlistTrunc), preDigestUnlistTrunc, sep = ":"))
          userTags3 <- c(userTags, userTags2)
          aa <- localTags[tag %in% userTags3][,.N, keyby = artifact]
          setkeyv(aa, "N")
          similar <- localTags[tail(aa, as.numeric(showSimilar)), on = "artifact"][N == max(N)]
          if (NROW(similar)) {
            similar2 <- similar[grepl("preDigest", tag)]
            cacheIdOfSimilar <- similar[grepl("cacheId", tag)]$tag
            cacheIdOfSimilar <- unlist(strsplit(cacheIdOfSimilar, split = ":"))[2]

            similar2[, `:=`(fun = unlist(lapply(strsplit(tag, split = ":"), function(xx) xx[[2]])),
                            hash = unlist(lapply(strsplit(tag, split = ":"), function(xx) xx[[3]])))]
            similar2[, differs := !(hash %in% preDigestUnlistTrunc), by = artifact]
            similar2[!(fun %in% names(preDigestUnlistTrunc)), differs := NA]
            similar2[(hash %in% "other"), deeperThan3 := TRUE]
            similar2[(hash %in% "other"), differs := NA]
            differed <- FALSE
            message("This call to cache differs from the next closest due to:")
            if (sum(similar2[differs %in% TRUE]$differs, na.rm = TRUE)) {
              differed <- TRUE
              message("... different ", paste(similar2[differs %in% TRUE]$fun, collapse = ", "))
            }

            if (length(similar2[is.na(differs)]$differs)) {
              differed <- TRUE
              message("... possible, unknown, differences in a nested list that is deeper than 3 in ",
                      paste(collapse = ", ", as.character(similar2[deeperThan3 == TRUE]$fun)))
            }
            missingArgs <- similar2[is.na(deeperThan3) & is.na(differs)]$fun
            if (length(missingArgs)) {
              differed <- TRUE
              message("... because of an ",
                      "argument currently not specified: ",
                      paste(as.character(missingArgs), collapse = ", "))

            }
            print(paste0("artifact with cacheId ", cacheIdOfSimilar))
            print(similar2[,c("fun", "differs")])

          } else {
            message("There is no similar item in the cacheRepo")
          }
        }
      }

      # RUN the function call
      if (verbose) {
        startRunTime <- Sys.time()
      }

      if (isPipe) {
        output <- eval(tmpl$._pipe, envir = tmpl$._envir)
      } else {
        output <- do.call(FUN, originalDots)
      }

      output <- .addChangedAttr(output, preDigest, origArguments = tmpl[!dotPipe],
                                 objects = objects, length = length,
                                 algo = algo, quick = quick, classOptions = classOptions, ...)

      if (verbose) {
        endRunTime <- Sys.time()
        verboseDF <- data.frame(
          functionName = functionDetails$functionName,
          component = paste("Running", functionDetails$functionName),
          elapsedTime = as.numeric(difftime(endRunTime, startRunTime, units = "secs")),
          units = "secs",
          stringsAsFactors = FALSE
        )

        if (exists("verboseTiming", envir = .reproEnv)) {
          .reproEnv$verboseTiming <- rbind(.reproEnv$verboseTiming, verboseDF)
        }

        # on.exit({message("Running ", functionDetails$functionName, " took ", format(endRunTime - startRunTime))},
        #         add = TRUE)

      }

      # Delete previous version if notOlderThan violated --
      #   but do this AFTER new run on previous line, in case function call
      #   makes it crash, or user interrupts long function call and wants
      #   a previous version
      if (nrow(isInRepo) > 0) {
        # flush it if notOlderThan is violated
        if (notOlderThan >= lastEntry) {
          suppressMessages(clearCache(userTags = isInRepo$artifact[lastOne], x = cacheRepo))
        }
      }

      # need something to attach tags to if it is actually NULL
      isNullOutput <- if (is.null(output)) TRUE else FALSE
      if (isNullOutput) output <- "NULL"

      attr(output, "tags") <- paste0("cacheId:", outputHash)
      attr(output, ".Cache")$newCache <- TRUE
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
              file.copy(from = x, to = file.path(repoTo), recursive = TRUE)
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

      outputToSaveIsList <- is.list(outputToSave)
      if (outputToSaveIsList) {
        rasters <- unlist(lapply(outputToSave, is, "Raster"))
      } else {
        rasters <- is(outputToSave, "Raster")
      }
      if (any(rasters)) {
        if (outputToSaveIsList) {
          outputToSave[rasters] <- lapply(outputToSave[rasters], function(x)
            .prepareFileBackedRaster(x, repoDir = cacheRepo, overwrite = FALSE))
        } else {
          outputToSave <- .prepareFileBackedRaster(outputToSave, repoDir = cacheRepo,
                                                   overwrite = FALSE)
        }
        attr(outputToSave, "tags") <- attr(output, "tags")
        attr(outputToSave, "call") <- attr(output, "call")
        attr(outputToSave, ".Cache")$newCache <- attr(output, ".Cache")$newCache
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

      if (verbose) {
        startSaveTime <- Sys.time()
      }

      # This is for write conflicts to the SQLite database
      #   (i.e., keep trying until it is written)

      objSize <- .objSizeInclEnviros(outputToSave)
      userTags <- c(userTags,
                    if (!is.na(functionDetails$functionName))
                      paste0("function:", functionDetails$functionName),
                    paste0("object.size:", objSize),
                    paste0("accessed:", Sys.time()),
                    paste0(otherFns),
                    paste("preDigest", names(preDigestUnlistTrunc),
                          preDigestUnlistTrunc, sep = ":"))

      written <- 0

      if (!isFALSE(getOption("reproducible.futurePlan")) && requireNamespace("future") &&
          (.Platform$OS.type != "windows")) {
        if (isTRUE(getOption("reproducible.futurePlan"))) {
          message('options("reproducible.futurePlan") is TRUE. Setting it to "multiprocess"\n',
                  'Please specify a plan by name, e.g., options("reproducible.futurePlan" = "multiprocess")')
          future::plan("multiprocess")
        } else {
          if (!is(future::plan(), getOption("reproducible.futurePlan"))) {
            thePlan <- getOption("reproducible.futurePlan")
            future::plan(thePlan)
          }
        }
        saved <- future::futureCall(FUN = writeFuture, args = list(written, outputToSave, cacheRepo, userTags),
                                    globals = list(written = written, saveToLocalRepo = archivist::saveToLocalRepo,
                                                   outputToSave = outputToSave,
                                                   cacheRepo = cacheRepo, userTags = userTags))
      } else {
        while (written >= 0) {
          saved <- suppressWarnings(try(silent = TRUE,
                                        saveToLocalRepo(
                                          outputToSave,
                                          repoDir = cacheRepo,
                                          artifactName = NULL,
                                          archiveData = FALSE,
                                          archiveSessionInfo = FALSE,
                                          archiveMiniature = FALSE,
                                          rememberName = FALSE,
                                          silent = TRUE,
                                          userTags = userTags
                                        )
          ))

          # This is for simultaneous write conflicts. SQLite on Windows can't handle them.
          written <- if (is(saved, "try-error")) {
            Sys.sleep(sum(runif(written + 1, 0.05, 0.1)))
            written + 1
          } else {
            -1
          }
        }

      }


      if (verbose) {
        endSaveTime <- Sys.time()
        verboseDF <-
          data.frame(
            functionName = functionDetails$functionName,
            component = "Saving to repo",
            elapsedTime = as.numeric(difftime(endSaveTime, startSaveTime, units = "secs")),
            units = "secs",
            stringsAsFactors = FALSE
          )

        if (exists("verboseTiming", envir = .reproEnv)) {
          .reproEnv$verboseTiming <- rbind(.reproEnv$verboseTiming, verboseDF)
        }
        # on.exit({message("Saving ", functionDetails$functionName, " to repo took ",
        #                  format(endSaveTime - startSaveTime))},
        #         add = TRUE)

      }

      if (verbose) {
        endCacheTime <- Sys.time()
        verboseDF <- data.frame(functionName = functionDetails$functionName,
                                component = "Whole Cache call",
                                elapsedTime = as.numeric(difftime(endCacheTime, startCacheTime,
                                                                  units = "secs")),
                                units = "secs",
                                stringsAsFactors = FALSE)

        if (exists("verboseTiming", envir = .reproEnv)) {
          .reproEnv$verboseTiming <- rbind(.reproEnv$verboseTiming, verboseDF)
        }
        # on.exit({message("Loading from repo took ", format(endLoadTime - startLoadTime))},
        #   add = TRUE)
      }

      if (isNullOutput) return(NULL) else return(output)
    }
  })

#' Deprecated functions
#' @export
#' @importFrom archivist showLocalRepo rmFromLocalRepo addTagsRepo
#' @importFrom stats runif
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

# .memoisedCacheFuns <- new.env(parent = asNamespace("reproducible"))
# CacheMem <- memoise::memoise(Cache)

.formalsCache <- formals(Cache)[-(1:2)]
.formalsCache[c("compareRasterFileLength", "digestPathContent")] <- NULL
.namesCacheFormals <- names(.formalsCache)[]

.loadFromLocalRepoMem2 <- function(md5hash, ...) {
  out <- loadFromLocalRepo(md5hash, ...)
  out <- makeMemoiseable(out)
  return(out)

}
.loadFromLocalRepoMem <- memoise::memoise(.loadFromLocalRepoMem2)

.unlistToCharacter <- function(l, max.level = 1) {
  if (max.level > 0) {
    lapply(l, function(l1) {
      if (is.character(l1)) {
        l1
      } else {
        if (is.list(l1)) {
          .unlistToCharacter(l1, max.level = max.level - 1)
        } else {
          "not list"
        }
      }
    })
  } else {
    "other"
  }
}

#' Generic method to make or unmake objects memoisable
#'
#' This is just a pass through for all clases in reproducible.
#' This generic is here so that downstream methods can be created.
#'
#' @param x  An object to make memoiseable.
#'           See individual methods in other packages.
#' @return The same object, but with any modifications, especially
#' dealing with saving of environments, which memoising doesn't handle
#' correctly in some cases.
#'
#' @export
#' @rdname makeMemoiseable
makeMemoiseable <- function(x) {
  UseMethod("makeMemoiseable")
}

#' @export
#' @rdname makeMemoiseable
makeMemoiseable.default <- function(x) {
  x
}

#' @export
#' @rdname makeMemoiseable
unmakeMemoiseable <- function(x) {
  UseMethod("unmakeMemoiseable")
}

#' @export
#' @rdname makeMemoiseable
unmakeMemoiseable.default <- function(x) {
  x
}


#' @inheritParams archivist showLocalRepo
#' @inheritParams fastdigest fastdigest
showLocalRepo2 <- function(repoDir) {
  aa <- showLocalRepo(repoDir) # much faster than showLocalRepo(repoDir, "tags")
  dig <- fastdigest(aa$md5hash)
  bb <- showLocalRepo3Mem(repoDir, dig)
  return(bb)
}

showLocalRepo3 <- function(repoDir, dig) {
  showLocalRepo(repoDir, "tags")
}

showLocalRepo3Mem <- memoise::memoise(showLocalRepo3)


#' Write to archivist repository, using \code{future::future}
#'
#' This will be used internally if \code{options("reproducible.futurePlan" = TRUE)}.
#' This is still experimental.
#'
#' @export
#' @param written Integer If zero or positive then it needs to be written still.
#'                Should be 0 to start.
#' @param outputToSave The R object to save to repository
#' @param cacheRepo The file path of the repository
#' @param userTags Character string of tags to attach to this \code{outputToSave} in
#'                 the \code{CacheRepo}
writeFuture <- function(written, outputToSave, cacheRepo, userTags) {
  while (written >= 0) {
    #future::plan(multiprocess)
    saved <- #suppressWarnings(try(silent = TRUE,
      saveToLocalRepo(
        outputToSave,
        repoDir = cacheRepo,
        artifactName = NULL,
        archiveData = FALSE,
        archiveSessionInfo = FALSE,
        archiveMiniature = FALSE,
        rememberName = FALSE,
        silent = TRUE,
        userTags = userTags
      )
    #))

    # This is for simultaneous write conflicts. SQLite on Windows can't handle them.
    written <- if (is(saved, "try-error")) {
      Sys.sleep(sum(runif(written + 1, 0.05, 0.1)))
      written + 1
    } else {
      -1
    }
  }
  return(saved)

}
