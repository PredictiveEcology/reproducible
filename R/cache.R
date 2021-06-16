utils::globalVariables(c(
  ".", "artifact", "createdDate", "deeperThan3", "differs", "fun", "hash",
  "i.hash", "iden", "N", "tag", "tagKey", "tagValue"
))

.reproEnv <- new.env(parent = asNamespace("reproducible"))

#' Cache method that accommodates environments, S4 methods, Rasters, & nested caching
#'
#' @description
#' \if{html}{\figure{lifecycle-maturing.svg}{options: alt="maturing"}}
#'
#' A function that can be used to wrap around other functions to cache function calls
#' for later use. This is normally most effective when the function to cache is
#' slow to run, yet the inputs and outputs are small. The benefit of caching, therefore,
#' will decline when the computational time of the "first" function call is fast and/or
#' the argument values and return objects are large. The default setting (and first
#' call to Cache) will always save to disk. The 2nd call to the same function will return
#' from disk. If the \code{options("reproducible.useMemoise" = TRUE)}, then the 3rd time
#' will recover the object from RAM and is normally much faster.
#'
#' @details
#'
#' There are other similar functions in the R universe. This version of Cache has
#' been used as part of a robust continuous workflow approach. As a result, we have
#' tested it with many "non-standard" R objects (e.g., RasterLayer objects) and
#' environments, which tend to be challenging for caching as they are always unique.
#'
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
#'   \item Uses \code{\link[digest]{digest}} (formerly fastdigest, which does
#'         not translate between operating systems).
#'         This is used for file-backed objects as well.
#'   \item Cache will save arguments passed by user in a hidden environment. Any
#'         nested Cache functions will use arguments in this order 1) actual arguments
#'         passed at each Cache call, 2) any inherited arguments from an outer Cache
#'         call, 3) the default values of the Cache function. See section on \emph{Nested
#'         Caching}.
#' }
#'
#' Caching R objects using \code{archivist::cache} has five important limitations:
#' \enumerate{
#'   \item the \pkg{archivist} package detects different environments as different;
#'   \item it also does not detect S4 methods correctly due to method inheritance;
#'   \item it does not detect objects that have file-based storage of information
#'         (specifically \code{\link[raster:Raster-classes]{RasterLayer-class}} objects);
#'   \item the default hashing algorithm is relatively slow.
#'   \item heavily nested function calls may want Cache arguments to propagate through
#' }
#'
#' As part of the SpaDES ecosystem of R packages, \code{Cache} can be used
#' within SpaDES modules. If it is, then the cached entry will automatically
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
#' @section quick:
#' The \code{quick} argument is attempting to sort out an ambiguity with character strings:
#' are they file paths or are they simply character strings. When \code{quick = TRUE},
#' \code{Cache} will treat these as character strings; when \code{quick = FALSE},
#' they will be attempted to be treated as file paths first; if there is no file, then
#' it will revert to treating them as character strings. If user passes a
#' character vector to this, then this will behave like \code{omitArgs}:
#' \code{quick = "file"} will treat the argument \code{"file"} as character string.
#'
#' The most often encountered situation where this ambiguity matters is in arguments about
#' filenames: is the filename an input pointing to an object whose content we want to
#' assess (e.g., a file-backed raster), or an output (as in saveRDS) and it should not
#' be assessed. If only run once, the output file won't exist, so it will be treated
#' as a character string. However, once the function has been run once, the output file
#' will exist, and \code{Cache(...)} will assess it, which is incorrect. In these cases,
#' the user is advised to use \code{quick = "TheOutputFilenameArgument"} to
#' specify the argument whose content on disk should not be assessed, but whose
#' character string should be assessed (distinguishing it from \code{omitArgs =
#' "TheOutputFilenameArgument"}, which will not assess the file content nor the
#' character string).
#'
#' This is relevant for objects of class \code{character}, \code{Path} and
#' \code{Raster} currently. For class \code{character}, it is ambiguous whether
#' this represents a character string or a vector of file paths. If it is known
#' that character strings should not be treated as paths, then \code{quick =
#' TRUE} will be much faster, with no loss of information. If it is file or
#' directory, then it will digest the file content, or \code{basename(object)}.
#' For class \code{Path} objects, the file's metadata (i.e., filename and file
#' size) will be hashed instead of the file contents if \code{quick = TRUE}. If
#' set to \code{FALSE} (default), the contents of the file(s) are hashed. If
#' \code{quick = TRUE}, \code{length} is ignored. \code{Raster} objects are
#' treated as paths, if they are file-backed.
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
#' @section useCache:
#' Logical or numeric. If \code{FALSE} or \code{0}, then the entire Caching
#' mechanism is bypassed and the
#' function is evaluated as if it was not being Cached. Default is
#' \code{getOption("reproducible.useCache")}), which is \code{TRUE} by default,
#' meaning use the Cache mechanism. This may be useful to turn all Caching on or
#' off in very complex scripts and nested functions. Increasing levels of numeric
#' values will cause deeper levels of Caching to occur. Currently, only implemented
#' in \code{postProcess}: to do both caching of inner \code{cropInputs}, \code{projectInputs}
#' and \code{maskInputs}, and caching of outer \code{postProcess}, use
#' \code{useCache = 2}; to skip the inner sequence of 3 functions, use \code{useCache = 1}.
#' For large objects, this may prevent many duplicated save to disk events.
#'
#' If \code{"overwrite"}
#' (which can be set with \code{options("reproducible.useCache" =
#' "overwrite")}), then the function invoke the caching mechanism but will purge
#' any entry that is matched, and it will be replaced with the results of the
#' current call.
#'
#' If \code{"devMode"}: The point of this mode is to facilitate using the Cache when
#' functions and datasets are continually in flux, and old Cache entries are
#' likely stale very often. In `devMode`, the cache mechanism will work as
#' normal if the Cache call is the first time for a function OR if it
#' successfully finds a copy in the cache based on the normal Cache mechanism.
#' It *differs* from the normal Cache if the Cache call does *not* find a copy
#' in the `cacheRepo`, but it does find an entry that matches based on
#' `userTags`. In this case, it will delete the old entry in the `cacheRepo`
#' (identified based on matching `userTags`), then continue with normal `Cache`.
#' For this to work correctly, `userTags` must be unique for each function call.
#' This should be used with caution as it is still experimental. Currently, if
#' \code{userTags} are not unique to a single entry in the cacheRepo, it will
#' default to the behaviour of \code{useCache = TRUE} with a message. This means
#' that \code{"devMode"} is most useful if used from the start of a project.
#'
#' @section \code{useCloud}:
#' This is a way to store all or some of the local Cache in the cloud.
#' Currently, the only cloud option is Google Drive, via \pkg{googledrive}.
#' For this to work, the user must be or be able to be authenticated
#' with \code{googledrive::drive_auth}. The principle behind this
#' \code{useCloud} is that it will be a full or partial mirror of a local Cache.
#' It is not intended to be used independently from a local Cache. To share
#' objects that are in the Cloud with another person, it requires 2 steps. 1)
#' share the \code{cloudFolderID$id}, which can be retrieved by
#' \code{getOption("reproducible.cloudFolderID")$id} after at least one Cache
#' call has been made. 2) The other user must then set their  \code{cacheFolderID} in a
#' \code{Cache\(..., reproducible.cloudFolderID = \"the ID here\"\)} call or
#' set their option manually
#' \code{options\(\"reproducible.cloudFolderID\" = \"the ID here\"\)}.
#'
#' If \code{TRUE}, then this Cache call will download
#'   (if local copy doesn't exist, but cloud copy does exist), upload
#'   (local copy does or doesn't exist and
#'   cloud copy doesn't exist), or
#'   will not download nor upload if object exists in both. If \code{TRUE} will be at
#'   least 1 second slower than setting this to \code{FALSE}, and likely even slower as the
#'   cloud folder gets large. If a user wishes to keep "high-level" control, set this to
#'   \code{getOption("reproducible.useCloud", FALSE)} or
#'   \code{getOption("reproducible.useCloud", TRUE)} (if the default behaviour should
#'   be \code{FALSE} or \code{TRUE}, respectively) so it can be turned on and off with
#'   this option. NOTE: \emph{This argument will not be passed into inner/nested Cache calls.})
#'
#' @section \code{sideEffect}:
#' If \code{sideEffect} is not \code{FALSE}, then metadata about any files that
#' added to \code{sideEffect} will be added as an attribute to the cached copy.
#' Subsequent calls to this function will assess for the presence of the new files in the
#' \code{sideEffect} location.
#' If the files are identical (\code{quick = FALSE}) or their file size is identical
#' (\code{quick = TRUE}), then the cached copy of the function will be returned
#' (and no files changed).
#' If there are missing or incorrect files, then the function will re-run.
#' This will accommodate the situation where the function call is identical, but somehow the side
#' effect files were modified.
#' If \code{sideEffect} is logical, then the function will check the \code{cacheRepo};
#' if it is a path, then it will check the path.
#' The function will assess whether the files to be downloaded are found locally prior to download.
#' If it fails the local test, then it will try to recover from a local copy if (\code{makeCopy}
#' had been set to \code{TRUE} the first time the function was run.
#' Currently, local recovery will only work if\code{makeCOpy} was set to \code{TRUE} the first time
#' \code{Cache} was run). Default is \code{FALSE}.
#'
#' @note As indicated above, several objects require pre-treatment before
#' caching will work as expected. The function \code{.robustDigest} accommodates this.
#' It is an S4 generic, meaning that developers can produce their own methods for
#' different classes of objects. Currently, there are methods for several types
#' of classes. See \code{\link{.robustDigest}}.
#'
#' See \code{\link{.robustDigest}} for other specifics for other classes.
#'
#' @include cache-helpers.R
#' @include robustDigest.R
#'
#' @param FUN Either a function or an unevaluated function call (e.g., using
#'            \code{quote}.
#' @param ... Arguments passed to \code{FUN}
#'
#' @param .objects Character vector of objects to be digested. This is only applicable
#'                if there is a list, environment (or similar) with named objects
#'                within it. Only this/these objects will be considered for caching,
#'                i.e., only use a subset of
#'                the list, environment or similar objects. In the case of nested list-type
#'                objects, this will only be applied outermost first.
#'
#' @param .cacheExtra A an arbitrary R object that will be included in the `CacheDigest`,
#'       but otherwise not passed into the \code{FUN}.
#'
#' @param outputObjects Optional character vector indicating which objects to
#'                      return. This is only relevant for list, environment (or similar) objects
#'
#' @param cacheRepo A repository used for storing cached objects.
#'                  This is optional if \code{Cache} is used inside a SpaDES module.
#'
#' @param length Numeric. If the element passed to Cache is a \code{Path} class
#'        object (from e.g., \code{asPath(filename)}) or it is a \code{Raster} with
#'        file-backing, then this will be
#'        passed to \code{digest::digest}, essentially limiting the number of bytes
#'        to digest (for speed). This will only be used if \code{quick = FALSE}.
#'        Default is \code{getOption("reproducible.length")}, which is set to \code{Inf}.
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
#'        partial matching, so "c" or "q" work). \code{TRUE} is equivalent to \code{"complete"}.
#'        If \code{"complete"}, then the returned object from the Cache
#'        function will have two attributes, \code{debugCache1} and \code{debugCache2},
#'        which are the entire \code{list(...)} and that same object, but after all
#'        \code{.robustDigest} calls, at the moment that it is digested using
#'        \code{digest}, respectively. This \code{attr(mySimOut, "debugCache2")}
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
#' @param userTags A character vector with descriptions of the Cache function call. These
#'   will be added to the Cache so that this entry in the Cache can be found using
#'   \code{userTags} e.g., via \code{\link{showCache}}.
#'
#' @param notOlderThan A time. Load an object from the Cache if it was created after this.
#'
#' @param quick Logical or character. If \code{TRUE},
#'        no disk-based information will be assessed, i.e., only
#'        memory content. See Details section about \code{quick} in \code{\link{Cache}}.
#'
#' @param verbose Numeric, -1 silent (where possible), 0 being very quiet,
#'        1 showing more messaging, 2 being more messaging, etc.
#'        Default is 1. Above 3 will output much more information about the internals of
#'        Caching, which may help diagnose Caching challenges. Can set globally with an
#'        option, e.g., \code{options('reproducible.verbose' = 0) to reduce to minimal}
#'
#' @param cacheId Character string. If passed, this will override the calculated hash
#'        of the inputs, and return the result from this cacheId in the cacheRepo.
#'        Setting this is equivalent to manually saving the output of this function, i.e.,
#'        the object will be on disk, and will be recovered in subsequent
#'        This may help in some particularly finicky situations
#'        where Cache is not correctly detecting unchanged inputs. This will guarantee
#'        the object will be identical each time; this may be useful in operational code.
#'
#' @param useCache Logical, numeric or \code{"overwrite"} or \code{"devMode"}. See details.
#'
#' @param useCloud Logical. See Details.
#'
#' @param cloudFolderID A googledrive dribble of a folder, e.g., using \code{drive_mkdir()}.
#'   If left as \code{NULL}, the function will create a cloud folder with name from last
#'   two folder levels of the \code{cacheRepo} path, :
#'   \code{paste0(basename(dirname(cacheRepo)), "_", basename(cacheRepo))}.
#'   This \code{cloudFolderID} will be added to \code{options("reproducible.cloudFolderID")},
#'   but this will not persist across sessions. If this is a character string, it will
#'   treat this as a folder name to create or use on GoogleDrive.
#'
#' @param showSimilar A logical or numeric. Useful for debugging.
#'        If \code{TRUE} or \code{1}, then if the Cache
#'        does not find an identical archive in the cacheRepo, it will report (via message)
#'        the next most similar archive, and indicate which argument(s) is/are different.
#'        If a number larger than \code{1}, then it will report the N most similar archived
#'        objects.
#'
#' @inheritParams digest::digest
#' @inheritParams DBI::dbConnect
#' @inheritParams DBI::dbWriteTable
#'
#' @param digestPathContent Being deprecated. Use \code{quick}.
#'
#' @return As with \code{archivist::cache}, returns the value of the
#' function call or the cached version (i.e., the result from a previous call
#' to this same cached function with identical arguments).
#'
#' @seealso \code{\link{showCache}}, \code{\link{clearCache}}, \code{\link{keepCache}},
#'   \code{\link{CacheDigest}}, \code{\link{movedCache}}, \code{\link{.robustDigest}},
#'   \code{\link{pipe}}
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
#' @importFrom DBI SQL
#' @importFrom digest digest
#' @importFrom data.table setDT := setkeyv .N .SD setattr
#' @importFrom glue glue_sql double_quote
#' @importFrom magrittr %>%
#' @importFrom utils object.size tail methods
#' @importFrom methods formalArgs
#' @rdname Cache
#'
#' @example inst/examples/example_Cache.R
#'
setGeneric(
  "Cache", # signature = "...",
  function(FUN, ..., notOlderThan = NULL,
           .objects = NULL, .cacheExtra = NULL,
           outputObjects = NULL, # nolint
           algo = "xxhash64", cacheRepo = NULL,
           length = getOption("reproducible.length", Inf),
           compareRasterFileLength, userTags = c(),
           digestPathContent, omitArgs = NULL,
           classOptions = list(), debugCache = character(),
           sideEffect = FALSE, makeCopy = FALSE,
           quick = getOption("reproducible.quick", FALSE),
           verbose = getOption("reproducible.verbose", 1), cacheId = NULL,
           useCache = getOption("reproducible.useCache", TRUE),
           useCloud = FALSE,
           cloudFolderID = getOption("reproducible.cloudFolderID", NULL),
           showSimilar = getOption("reproducible.showSimilar", FALSE),
           drv = getOption("reproducible.drv", RSQLite::SQLite()), conn = getOption("reproducible.conn", NULL)) {
    standardGeneric("Cache")
})

#' @export
#' @rdname Cache
setMethod(
  "Cache",
  definition = function(FUN, ..., notOlderThan, .objects = NULL, .cacheExtra = NULL,
                        outputObjects,  # nolint
                        algo, cacheRepo, length, compareRasterFileLength, userTags,
                        digestPathContent, omitArgs, classOptions,
                        debugCache, sideEffect, makeCopy, quick, verbose,
                        cacheId, useCache,
                        useCloud,
                        cloudFolderID,
                        showSimilar, drv, conn) {

    if (exists("._Cache_1")) browser() # to allow easier debugging of S4 class

    if (missing(FUN)) stop("Cache requires the FUN argument")

    # returns "modifiedDots", "originalDots", "FUN", "funName", which will
    #  have modifications under many circumstances, e.g., do.call, specific methods etc.
    fnDetails <- .fnCleanup(FUN = FUN, callingFun = "Cache", ...)

    FUN <- fnDetails$FUN
    modifiedDots <- fnDetails$modifiedDots
    originalDots <- fnDetails$originalDots
    skipCacheDueToNumeric <- is.numeric(useCache) && useCache <= (fnDetails$nestLevel)

    if (isFALSE(useCache) || isTRUE(0 == useCache) || skipCacheDueToNumeric) {
      nestedLev <- as.numeric(fnDetails$nestLevel)
      spacing <- paste(collapse = "",
                       rep("  ", nestedLev)
      )
      messageCache(spacing, "useCache is ", useCache,
                   "; skipping Cache on function ", fnDetails$functionName,
                   if (nestedLev > 0) paste0(" (currently running nested Cache level ", nestedLev + 1),
                   ")",
                   verbose = verbose)
      if (fnDetails$isDoCall) {
        do.call(modifiedDots$what, args = modifiedDots$args)
      } else {
        commonArgs <- .namesCacheFormals[.namesCacheFormals %in% formalArgs(FUN)]
        do.call(FUN, append(alist(...), modifiedDots[commonArgs]))
        # FUN(...) # using do.call fails on quoted arguments because it evaluates them
        # do.call(FUN, args = list(expr(modifiedDots)))
      }
    } else {
      startCacheTime <- verboseTime(verbose)

      if (!missing(compareRasterFileLength)) {
        messageCache("compareRasterFileLength argument being deprecated. Use 'length'",
                     verbose = verbose)
        length <- compareRasterFileLength
      }
      if (!missing(digestPathContent)) {
        messageCache("digestPathContent argument being deprecated. Use 'quick'.",
                     verbose = verbose)
        quick <- !digestPathContent
      }

      mced <- match.call(expand.dots = TRUE)
      nestedTags <- determineNestedTags(envir = environment(),
                                        mc = mced,
                                        userTags = userTags)
      userTags <- unique(c(userTags, .reproEnv$userTags))
      if (any(!nestedTags$objOverride)) {
        on.exit({
          if (any(!nestedTags$prevVals)) {
            # THe suppressWarnings is about objects that aren't there -- so far only happens
            #  when interrupting a process, which means it is spurious
            suppressWarnings(rm(list = nestedTags$namesUserCacheArgs,
                                envir = .reproEnv))
            if (nestedTags$prevUserTags)
              .reproEnv$userTags <- nestedTags$oldUserTags
          }
          if (nestedTags$prevUserTags) {
            .reproEnv$userTags <- nestedTags$oldUserTags
          }
        }, add = TRUE)
      }

      # get cacheRepo if not supplied
      cacheRepos <- getCacheRepos(cacheRepo, modifiedDots, verbose = verbose)
      cacheRepo <- cacheRepos[[1]]

      if (useDBI()) {
        if (is.null(conn)) {
          conn <- dbConnectAll(drv, cachePath = cacheRepo)
          RSQLite::dbClearResult(RSQLite::dbSendQuery(conn, "PRAGMA busy_timeout=5000;"))
          RSQLite::dbClearResult(RSQLite::dbSendQuery(conn, "PRAGMA journal_mode=WAL;"))
          on.exit({dbDisconnect(conn)}, add = TRUE)
        }
      }

      if (fnDetails$isPipe) {
        pipeRes <- .CachePipeFn1(modifiedDots, fnDetails, FUN)
        modifiedDots <- pipeRes$modifiedDots
        fnDetails <- pipeRes$fnDetails
      }

      modifiedDots$.FUN <- fnDetails$.FUN # put in modifiedDots for digesting  # nolint

      # This is for Pipe operator -- needs special consideration
      scalls <- if (!is(FUN, "function")) .CacheFn1(FUN, sys.calls()) else NULL

      # extract other function names that are not the ones the focus of the Cache call
      otherFns <- .getOtherFnNamesAndTags(scalls = scalls)

      if (missing(notOlderThan)) notOlderThan <- NULL

      # if a simList is in ...
      # userTags added based on object class
      userTags <- c(userTags, unlist(lapply(modifiedDots, .tagsByClass)))

      if (sideEffect != FALSE) if (isTRUE(sideEffect)) sideEffect <- cacheRepo

      # browser(expr = exists("._Cache_17"))
      conns <- list()
      on.exit({done <- lapply(conns, function(co) {
        if (!identical(co, conns[[1]])) {
          try(dbDisconnect(co), silent = TRUE)
        }})}, add = TRUE)
      isIntactRepo <- unlist(lapply(cacheRepos, function(cacheRepo) {
        # browser(expr = exists("._Cache_18"))
        conns[[cacheRepo]] <<- if (cacheRepo == cacheRepos[[1]]) {
          conn
        } else {
          dbConnectAll(drv, cachePath = cacheRepo)
        }
        CacheIsACache(cachePath = cacheRepo, drv = drv, create = TRUE,
                      conn = conns[[cacheRepo]])
      }))

      if (any(!isIntactRepo)) {
        if (useDBI())
          ret <- lapply(seq(cacheRepos)[!isIntactRepo], function(cacheRepoInd) {
            createCache(cacheRepos[[cacheRepoInd]], drv = drv, conn = conn,
                        force = isIntactRepo[cacheRepoInd])
          })
      }

      # List file prior to cache
      if (sideEffect != FALSE) {
        priorRepo <- list.files(sideEffect, full.names = TRUE)
      }

      # remove things in the Cache call that are not relevant to Caching
      if (!is.null(modifiedDots$progress))
        if (!is.na(modifiedDots$progress))
          modifiedDots$progress <- NULL

      # Do the digesting
      if (!is.null(omitArgs)) {
        modifiedDots[omitArgs] <- NULL
      }

      # don't digest the dotPipe elements as they are already
      # extracted individually into modifiedDots list elements
      dotPipe <- startsWith(names(modifiedDots), "._")
      preDigestByClass <- lapply(seq_along(modifiedDots[!dotPipe]), function(x) {
        .preDigestByClass(modifiedDots[!dotPipe][[x]])
      })

      startHashTime <- verboseTime(verbose)

      # remove some of the arguments passed to Cache, which are irrelevant for digest
      argsToOmitForDigest <- dotPipe | (names(modifiedDots) %in% .defaultCacheOmitArgs)

      preCacheDigestTime <- Sys.time()
      toDigest <- modifiedDots[!argsToOmitForDigest]
      if (!is.null(.cacheExtra)) {
        toDigest <- append(toDigest, list(.cacheExtra))
      }
      cacheDigest <- CacheDigest(toDigest, .objects = .objects,
                                 length = length, algo = algo, quick = quick,
                                 classOptions = classOptions)
      postCacheDigestTime <- Sys.time()
      elapsedTimeCacheDigest <- postCacheDigestTime - preCacheDigestTime

      preDigest <- cacheDigest$preDigest
      outputHash <- cacheDigest$outputHash

      # This does to depth 3
      preDigestUnlistTrunc <- unlist(
        .unlistToCharacter(preDigest, getOption("reproducible.showSimilarDepth", 3))
      )

      if (verbose > 3) {
        a <- .CacheVerboseFn1(preDigest, fnDetails,
                              startHashTime, modifiedDots, dotPipe, quick = quick,
                              verbose = verbose)
        on.exit({
          assign("cacheTimings", .reproEnv$verboseTiming, envir = .reproEnv)
          messageDF(.reproEnv$verboseTiming, colour = "blue")
          messageCache("This object is also available from .reproEnv$cacheTimings",
                       verbose = verbose)
          if (exists("verboseTiming", envir = .reproEnv))
            rm("verboseTiming", envir = .reproEnv)
        },
        add = TRUE)
      }

      if (length(debugCache)) {
        if (!is.na(pmatch(debugCache, "quick")))
          return(list(hash = preDigest, content = list(...)))
      }

      if (!is.null(cacheId)) {
        outputHashManual <- cacheId
        if (identical(outputHashManual, outputHash)) {
          messageCache("cacheId is same as calculated hash",
                       verbose = verbose)
        } else {
          messageCache("cacheId is not same as calculated hash. Manually searching for cacheId:", cacheId,
                       verbose = verbose)
        }
        outputHash <- outputHashManual
      }

      # compare outputHash to existing Cache record
      tries <- 1
      if (useCloud) {
        if (!requireNamespace("googledrive")) stop(requireNamespaceMsg("googledrive", "to use google drive files"))
        # Here, test that cloudFolderID exists and get obj details that matches outputHash, if present
        #  returns NROW 0 gdriveLs if not present
        #cloudFolderID <- checkAndMakeCloudFolderID(cloudFolderID)
        # browser(expr = exists("._Cache_2"))
        if (is.null(cloudFolderID))
          cloudFolderID <- cloudFolderFromCacheRepo(cacheRepo)
        if (is.character(cloudFolderID)) {
          cloudFolderID <- checkAndMakeCloudFolderID(cloudFolderID, create = TRUE,
                                                     overwrite = FALSE)
        }
        gdriveLs <- retry(quote(driveLs(cloudFolderID, pattern = outputHash,
                                        verbose = verbose)))
      }

      # Check if it is in repository
      needDisconnect <- FALSE
      while (tries <= length(cacheRepos)) {
        repo <- cacheRepos[[tries]]
        if (useDBI()) {
          # browser(expr = exists("._Cache_3"))
          dbTabNam <- CacheDBTableName(repo, drv = drv)
          if (tries > 1) {
            dbDisconnect(conn)
            conn <- dbConnectAll(drv, cachePath = repo)
          }
          qry <- glue::glue_sql("SELECT * FROM {DBI::SQL(double_quote(dbTabName))} where \"cacheId\" = ({outputHash})",
                                dbTabName = dbTabNam,
                                outputHash = outputHash,
                                .con = conn)
          res <- retry(retries = 15, exponentialDecayBase = 1.01,
                       quote(dbSendQuery(conn, qry)))
          isInRepo <- setDT(dbFetch(res))
          dbClearResult(res)
        }
        fullCacheTableForObj <- isInRepo
        if (NROW(isInRepo) > 1) isInRepo <- isInRepo[NROW(isInRepo),]
        if (NROW(isInRepo) > 0) {
          # browser(expr = exists("._Cache_4"))
          cacheRepo <- repo
          break
        }
        tries <- tries + 1
      }

      userTags <- c(userTags, if (!is.na(fnDetails$functionName))
        paste0("function:", fnDetails$functionName)
      )

      outputHashNew <- outputHash # Keep a copy of this because it may be replaced next, but we need to know old one

      # First, if this is not matched by outputHash, test that it is matched by
      #   userTags and in devMode
      needFindByTags <- identical("devMode", useCache) && NROW(isInRepo) == 0
      if (needFindByTags) {
        # browser(expr = exists("._Cache_5"))
        # It will not have the "localTags" object because of "direct db access" added Jan 20 2020
        if (!exists("localTags", inherits = FALSE)) #
          localTags <- showCache(repo, drv = drv, verboseMessaging = FALSE) # This is noisy
        devModeOut <- devModeFn1(localTags, userTags, scalls, preDigestUnlistTrunc, useCache, verbose, isInRepo, outputHash)
        outputHash <- devModeOut$outputHash
        isInRepo <- devModeOut$isInRepo
        needFindByTags <- devModeOut$needFindByTags
      }

      # Deal with overwrite, needFindByTags (which is related to "devMode")
      isInCloud <- FALSE
      if (useCloud && identical("overwrite", useCache)) {
        # browser(expr = exists("._Cache_16"))
        isInCloud <- isTRUE(any(gdriveLs$name %in% basename2(CacheStoredFile(cacheRepo, outputHash))))
      }

      if (identical("overwrite", useCache)  && (NROW(isInRepo) > 0 || isInCloud) || needFindByTags) {
        suppressMessages(clearCache(x = cacheRepo, userTags = outputHash, ask = FALSE,
                                    useCloud = ifelse(isTRUEorForce(useCloud), "force", FALSE),
                                    drv = drv, conn = conn,
                                    cloudFolderID = cloudFolderID))
        if (identical("devMode", useCache)) {
          userTagsSimple <- gsub(".*:(.*)", "\\1", userTags)
          isInRepo <- isInRepo[!isInRepo[[.cacheTableTagColName()]] %in% userTagsSimple, , drop = FALSE]
          outputHash <- outputHashNew
          messageCache("Overwriting Cache entry with userTags: '",paste(userTagsSimple, collapse = ", ") ,"'",
                       verbose = verbose)
        } else {
          # remove entries from the 2 data.frames of isInRep & gdriveLs
          if (useDBI()) {
            if (useCloud)
              gdriveLs <- gdriveLs[!gdriveLs$name %in% basename2(CacheStoredFile(cacheRepo, outputHash)),]
            isInRepo <- isInRepo[isInRepo[[.cacheTableHashColName()]] != outputHash, , drop = FALSE]
          } else {
            isInRepo <- isInRepo[isInRepo[[.cacheTableTagColName()]] != paste0("cacheId:", outputHash), , drop = FALSE]
          }
          messageCache("Overwriting Cache entry with function '",fnDetails$functionName ,"'",
                       verbose = verbose)
        }
      }

      # If it is in the existing record:
      if (NROW(isInRepo) > 0) {
        # make sure the notOlderThan is valid, if not, exit this loop
        lastEntry <- max(isInRepo$createdDate)
        lastOne <- order(isInRepo$createdDate, decreasing = TRUE)[1]
        if (is.null(notOlderThan) || (notOlderThan < lastEntry)) {
          # browser(expr = exists("._Cache_6"))
          objSize <- if (useDBI()) {
            as.numeric(tail(fullCacheTableForObj[["tagValue"]][
              fullCacheTableForObj$tagKey == "file.size"], 1))
          } else {
            file.size(CacheStoredFile(cacheRepo, isInRepo[[.cacheTableHashColName()]]))
          }
          class(objSize) <- "object_size"
          bigFile <- isTRUE(objSize > 1e6)
          messageCache("  ...(Object to retrieve (",
                                      basename2(CacheStoredFile(cacheRepo, isInRepo[[.cacheTableHashColName()]])),
                                      ")",
                                      if (bigFile) " is large: ",
                                      if (bigFile) format(objSize, units = "auto"),
                                      ")",
                       verbose = verbose)

          preLoadTime <- Sys.time()
          output <- try(.getFromRepo(FUN, isInRepo = isInRepo, notOlderThan = notOlderThan,
                                     lastOne = lastOne, cacheRepo = cacheRepo,
                                     fnDetails = fnDetails,
                                     modifiedDots = modifiedDots, debugCache = debugCache,
                                     verbose = verbose, sideEffect = sideEffect,
                                     quick = quick, algo = algo,
                                     preDigest = preDigest, startCacheTime = startCacheTime,
                                     drv = drv, conn = conn,
                                     ...), silent = TRUE)
          output <- dealWithRastersOnRecovery(output, cacheRepo = cacheRepo, cacheId = isInRepo$cacheId,
                                              drv = drv, conn = conn) # returns just the "original" filenames in metadata & copies file-backed
          postLoadTime <- Sys.time()
          elapsedTimeLoad <- postLoadTime - preLoadTime

          # browser(expr = exists("._Cache_7"))
          if (is(output, "try-error")) {
            cID <- if (useDBI())
              isInRepo[[.cacheTableHashColName()]]
            else
              gsub("cacheId:", "", isInRepo[[.cacheTableTagColName()]])
            stop(output, "\nError in trying to recover cacheID: ", cID,
                 "\nYou will likely need to remove that item from Cache, e.g., ",
                 "\nclearCache(userTags = '", cID, "')")
          }

          if (useDBI())
            .updateTagsRepo(outputHash, cacheRepo, "elapsedTimeLoad",
                         format(elapsedTimeLoad, units = "secs"),
                         add = TRUE,
                         drv = drv, conn = conn)
          if (useCloud) {
            # browser(expr = exists("._Cache_7b"))
            # Here, upload local copy to cloud folder
            cu <- try(retry(quote(isInCloud <- cloudUpload(isInRepo, outputHash, gdriveLs, cacheRepo,
                                                           cloudFolderID, output))))
            .updateTagsRepo(outputHash, cacheRepo, "inCloud", "TRUE", drv = drv, conn = conn)
          }

          return(output)
        }
      } else {
        # find similar -- in progress
        # browser(expr = exists("._Cache_8"))

        if (!is.null(showSimilar)) { # TODO: Needs testing
          if (!isFALSE(showSimilar)) {
            if (!exists("localTags", inherits = FALSE)) #
              localTags <- showCache(repo, drv = drv, verboseMessaging = FALSE) # This is noisy
            .findSimilar(localTags, showSimilar, scalls, preDigestUnlistTrunc,
                         userTags, functionName = fnDetails$functionName,
                         useCache = useCache, verbose = verbose)
          }
        }
      }

      startRunTime <- verboseTime(verbose)

      .CacheIsNew <- TRUE
      if (useCloud) {
        # browser(expr = exists("._Cache_9"))
        # Here, download cloud copy to local folder, skip the running of FUN
        newFileName <- CacheStoredFile(cacheRepo, outputHash) # paste0(outputHash,".rda")
        isInCloud <- gsub(gdriveLs$name,
                          pattern = paste0("\\.", fileExt(CacheStoredFile(cacheRepo, outputHash))),
                          replacement = "") %in% outputHash
        if (any(isInCloud)) {
          output <- cloudDownload(outputHash, newFileName, gdriveLs, cacheRepo, cloudFolderID,
                                  drv = drv)
          if (is.null(output)) {
            retry(quote(googledrive::drive_rm(gdriveLs[isInCloud,])))
            isInCloud[isInCloud] <- FALSE
          } else {
            .CacheIsNew <- FALSE
          }
        }
      }

      # check that it didn't come from cloud or failed to find complete cloud (i.e., output is NULL)
      # browser(expr = exists("._Cache_10"))
      elapsedTimeFUN <- NA
      if (!exists("output", inherits = FALSE) || is.null(output)) {
        # Run the FUN
        preRunFUNTime <- Sys.time()
        if (fnDetails$isPipe) {
          output <- eval(modifiedDots$._pipe, envir = modifiedDots$._envir)
        } else {
          # rlang attempts that are inadequate -- can't quite get the flexibility required to
          #   allow either Cache(rnorm(1)) or Cache(rnorm, 1) to work correctly. Can only get
          #   one or the other
          # FUNx1 <- rlang::enquo(FUN)
          # FUNx2 <- rlang::enquos(...)
          # rlang::eval_tidy(call2(FUNx1, !!!FUNx2))
          # theCall <- expr(FUN(!!!dots))
          # output <- eval_tidy(theCall)
          commonArgs <- .namesCacheFormals[.namesCacheFormals %in% formalArgs(FUN)]
          if (length(commonArgs) > 0) {
            messageCache("Cache and ", fnDetails$functionName, " have 1 or more common arguments: ", commonArgs,
                         "\nSending the argument(s) to both ", verboseLevel = 2, verbose = verbose)
          }
          output <- if (length(commonArgs) == 0) {
            FUN(...)
          } else {# the do.call mechanism is flawed because of evaluating lists; only use in rare cases
            do.call(FUN, append(alist(...), mget(commonArgs, inherits = FALSE)))
          }

        }
        postRunFUNTime <- Sys.time()
        elapsedTimeFUN <- postRunFUNTime - preRunFUNTime
      }

      output <- .addChangedAttr(output, preDigest, origArguments = modifiedDots[!dotPipe],
                                .objects = outputObjects, length = length,
                                algo = algo, quick = quick, classOptions = classOptions, ...)
      verboseDF1(verbose, fnDetails$functionName, startRunTime)

      # Delete previous version if notOlderThan violated --
      #   but do this AFTER new run on previous line, in case function call
      #   makes it crash, or user interrupts long function call and wants
      #   a previous version
      if (nrow(isInRepo) > 0) {
        # flush it if notOlderThan is violated
        if (notOlderThan >= lastEntry) {
          suppressMessages(clearCache(userTags = isInRepo[[.cacheTableHashColName()]][lastOne],
                                      x = cacheRepo,
                                      ask = FALSE, useCloud = useCloud, drv = drv, conn = conn,
                                      cloudFolderID = cloudFolderID))
        }
      }

      # need something to attach tags to if it is actually NULL
      isNullOutput <- if (is.null(output)) TRUE else FALSE
      if (isNullOutput) {
        output <- "NULL"
      }

      # browser(expr = identical(outputHash, "aa8b14f8ef51eddb"))
      .setSubAttrInList(output, ".Cache", "newCache", .CacheIsNew)
      setattr(output, "tags", paste0("cacheId:", outputHash))
      setattr(output, "call", "")
      # attr(output, "tags") <- paste0("cacheId:", outputHash)
      # attr(output, ".Cache")$newCache <- TRUE
      # attr(output, "call") <- ""
      if (!identical(attr(output, ".Cache")$newCache, .CacheIsNew))
        stop("attributes are not correct 3")
      if (!identical(attr(output, "call"), ""))
        stop("attributes are not correct 4")
      if (!identical(attr(output, "tags"), paste0("cacheId:", outputHash)))
        stop("attributes are not correct 5")

      # browser(expr = exists("._Cache_11"))
      if (sideEffect != FALSE) {
        output <- .CacheSideEffectFn2(sideEffect, cacheRepo, priorRepo, algo, output,
                                      makeCopy, quick)
      }

      if (isS4(FUN)) {
        setattr(output, "function", FUN@generic)
        #attr(output, "function") <- FUN@generic
        if (!identical(attr(output, "function"), FUN@generic))
          stop("There is an unknown error 03")
      }
      # Can make new methods by class to add tags to outputs
      if (useDBI()) {
        if (.CacheIsNew) {
          outputToSave <- dealWithRasters(output, cacheRepo, drv = drv, conn = conn)
          outputToSave <- .addTagsToOutput(outputToSave, outputObjects, FUN, preDigestByClass)
        } else {
          outputToSave <- .addTagsToOutput(output, outputObjects, FUN, preDigestByClass)
        }
      }

      # Remove from otherFunctions if it is "function"
      alreadyIn <- gsub(otherFns, pattern = "otherFunctions:", replacement = "") %in%
        as.character(attr(output, "function"))
      if (isTRUE(any(alreadyIn)))
        otherFns <- otherFns[!alreadyIn]

      if (!useDBI()) {
        # browser(expr = exists("._Cache_12"))
        outputToSaveIsList <- is(outputToSave, "list") # is.list is TRUE for anything, e.g., data.frame. We only want "list"
        if (outputToSaveIsList) {
          rasters <- unlist(lapply(outputToSave, is, "Raster"))
        } else {
          rasters <- is(outputToSave, "Raster")
        }
        if (any(rasters)) {
          if (outputToSaveIsList) {
            outputToSave[rasters] <- lapply(outputToSave[rasters], function(x)
              .prepareFileBackedRaster(x, repoDir = cacheRepo, overwrite = FALSE, drv = drv, conn = conn))
          } else {
            outputToSave <- .prepareFileBackedRaster(outputToSave, repoDir = cacheRepo,
                                                     overwrite = FALSE, drv = drv, conn = conn)
          }

          # have to reset all these attributes on the rasters as they were undone in prev steps
          setattr(outputToSave, "tags", attr(output, "tags"))
          .setSubAttrInList(outputToSave, ".Cache", "newCache", attr(output, ".Cache")$newCache)
          setattr(outputToSave, "call", attr(output, "call"))

          # attr(outputToSave, "tags") <- attr(output, "tags")
          # attr(outputToSave, "call") <- attr(output, "call")
          # attr(outputToSave, ".Cache")$newCache <- attr(output, ".Cache")$newCache
          if (!identical(attr(outputToSave, ".Cache")$newCache, attr(output, ".Cache")$newCache))
            stop("attributes are not correct 6")
          if (!identical(attr(outputToSave, "call"), attr(output, "call")))
            stop("attributes are not correct 7")
          if (!identical(attr(outputToSave, "tags"), attr(output, "tags")))
            stop("attributes are not correct 8")

          if (isS4(FUN)) {
            setattr(outputToSave, "function", attr(output, "function"))
            if (!identical(attr(outputToSave, "function"), attr(output, "function")))
              stop("There is an unknown error 04")
          }
          # attr(outputToSave, "function") <- attr(output, "function")
          # For Rasters, there will be a new name if file-backed ... it must be conveyed to output too
          output <- outputToSave
        }
      }
      if (length(debugCache)) {
        if (!is.na(pmatch(debugCache, "complete"))) {
          output <- .debugCache(output, preDigest, ...)
          outputToSave <- .debugCache(outputToSave, preDigest, ...)
        }
      }

      startSaveTime <- verboseTime(verbose)
      # This is for write conflicts to the SQLite database
      #   (i.e., keep trying until it is written)

      objSize <- sum(unlist(objSize(outputToSave)))
      resultHash <- ""
      linkToCacheId <- NULL
      if (objSize > 1e6) {
        resultHash <- CacheDigest(list(outputToSave), .objects = .objects)$outputHash
        qry <- glue::glue_sql("SELECT * FROM {DBI::SQL(double_quote(dbTabName))}",
                              dbTabName = dbTabNam,
                              .con = conn)
        res <- retry(retries = 15, exponentialDecayBase = 1.01,
                     quote(dbSendQuery(conn, qry)))
        allCache <- setDT(dbFetch(res))
        dbClearResult(res)
        if (NROW(allCache)) {
          alreadyExists <- allCache[allCache$tagKey == "resultHash" & allCache$tagValue %in% resultHash]
          if (NROW(alreadyExists)) {
            linkToCacheId <- alreadyExists[["cacheId"]][[1]]
          }
        }
      }

      userTags <- c(userTags,
                    paste0("class:", class(outputToSave)[1]),
                    paste0("object.size:", objSize),
                    paste0("accessed:", Sys.time()),
                    paste0("inCloud:", isTRUE(useCloud)),
                    paste0("resultHash:", resultHash),
                    paste0("elapsedTimeDigest:", format(elapsedTimeCacheDigest, units = "secs")),
                    paste0("elapsedTimeFirstRun:", format(elapsedTimeFUN, units = "secs")),
                    paste0(otherFns),
                    paste("preDigest", names(preDigestUnlistTrunc), preDigestUnlistTrunc, sep = ":")
      )

      written <- 0

      useFuture <- FALSE
      .onLinux <- .Platform$OS.type == "unix" && unname(Sys.info()["sysname"]) == "Linux"
      if (.onLinux) {
        if (!isFALSE(getOption("reproducible.futurePlan")) &&
            .requireNamespace("future", messageStart = "To use reproducible.futurePlan, ")) {
          useFuture <- TRUE
        }
      }
      if (useFuture) {
        if (exists("futureEnv", envir = .reproEnv))
          .reproEnv$futureEnv <- new.env(parent = emptyenv())

        if (isTRUE(getOption("reproducible.futurePlan"))) {
          messageCache('options("reproducible.futurePlan") is TRUE. Setting it to "multiprocess".\n',
                  'Please specify a plan by name, e.g.,\n',
                  '  options("reproducible.futurePlan" = "multiprocess")',
                  verbose = verbose)
          future::plan("multiprocess", workers = 2)
        } else {
          if (!is(future::plan(), getOption("reproducible.futurePlan"))) {
            thePlan <- getOption("reproducible.futurePlan")
            future::plan(thePlan, workers = 2)
          }
        }
        .reproEnv$futureEnv[[paste0("future_", rndstr(1,10))]] <-
          #saved <-
          future::futureCall(
            FUN = writeFuture,
            args = list(written, outputToSave, cacheRepo, userTags, drv, conn, cacheId, linkToCacheId),
            globals = list(written = written,
                           outputToSave = outputToSave,
                           cacheRepo = cacheRepo,
                           userTags = userTags,
                           drv = drv,
                           conn = conn,
                           cacheId = outputHash,
                           linkToCacheId = linkToCacheId)
          )
        if (is.null(.reproEnv$alreadyMsgFuture)) {
          messageCache("  Cache saved in a separate 'future' process. ",
                  "Set options('reproducible.futurePlan' = FALSE), if there is strange behaviour.",
                  "This message will not be shown again until next reload of reproducible",
                  verbose = verbose)
          .reproEnv$alreadyMsgFuture <- TRUE
        }
      } else {
        otsObjSize <- gsub(grep("object.size", userTags, value = TRUE),
                           pattern = "object.size:", replacement = "")
        otsObjSize <- as.numeric(otsObjSize)
        class(otsObjSize) <- "object_size"
        isBig <- otsObjSize > 1e7
        if (useDBI()) {
          # browser(expr = exists("._Cache_13"))
          outputToSave <- progressBarCode(
            saveToCache(cachePath = cacheRepo, drv = drv, userTags = userTags,
                        conn = conn, obj = outputToSave, cacheId = outputHash,
                        linkToCacheId = linkToCacheId),
            doProgress = isBig,
            message = c("Saving ","large "[isBig],"object (cacheId: ", outputHash, ") to Cache", ": "[isBig],
                        format(otsObjSize, units = "auto")[isBig]),
            verboseLevel = 2 - isBig, verbose = verbose,
            colour = getOption("reproducible.messageColourCache"))

          #  outputToSave <- saveToCache(cachePath = cacheRepo, drv = drv, userTags = userTags,
        #                              conn = conn, obj = outputToSave, cacheId = outputHash,
        #                              linkToCacheId = linkToCacheId)
        }
      }

      if (useCloud && .CacheIsNew) {
        # Here, upload local copy to cloud folder if it isn't already there
        # browser(expr = exists("._Cache_15"))
        cufc <- try(cloudUploadFromCache(isInCloud, outputHash, cacheRepo, cloudFolderID, ## TODO: saved not found
                                         outputToSave, rasters))
        if (is(cufc, "try-error"))
          .updateTagsRepo(outputHash, cacheRepo, "inCloud", "FALSE", drv = drv, conn = conn)
      }

      verboseDF2(verbose, fnDetails$functionName, startSaveTime)

      verboseDF3(verbose, fnDetails$functionName, startCacheTime)

      if (isNullOutput) return(NULL) else return(output)
    }
})

#' @keywords internal
.formalsCache <- formals(Cache)[-(1:2)]

#' @keywords internal
.formalsCache[c("compareRasterFileLength", "digestPathContent")] <- NULL

#' @keywords internal
.namesCacheFormals <- names(.formalsCache)[]

#' @keywords internal
.namesCacheFormalsSendToBoth <- intersect("verbose", names(.formalsCache)[])

#' @keywords internal
.loadFromLocalRepoMem <- function(md5hash, repoDir, ...) {
  if (useDBI()) {
    out <- loadFromCache(cachePath = repoDir, cacheId = md5hash)
  }
  out <- makeMemoisable(out)
  return(out)
}

#' @keywords internal
#.loadFromLocalRepoMem <- memoise::memoise(.loadFromLocalRepoMem2)

#' @keywords internal
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
#' This is just a pass through for all classes in \pkg{reproducible}.
#' This generic is here so that downstream methods can be created.
#'
#' @param x  An object to make memoisable.
#'           See individual methods in other packages.
#' @return The same object, but with any modifications, especially
#' dealing with saving of environments, which memoising doesn't handle
#' correctly in some cases.
#'
#' @export
#' @rdname makeMemoisable
makeMemoisable <- function(x) {
  UseMethod("makeMemoisable")
}

#' @export
#' @rdname makeMemoisable
makeMemoisable.default <- function(x) {
  x
}

#' @export
#' @rdname makeMemoisable
unmakeMemoisable <- function(x) {
  UseMethod("unmakeMemoisable")
}

#' @export
#' @rdname makeMemoisable
unmakeMemoisable.default <- function(x) {
  x
}

#' Write to cache repository, using \code{future::future}
#'
#' This will be used internally if \code{options("reproducible.futurePlan" = TRUE)}.
#' This is still experimental.
#'
#' @param written Integer. If zero or positive then it needs to be written still.
#'                Should be 0 to start.
#' @param outputToSave The R object to save to repository
#' @param cacheRepo The file path of the repository
#' @param userTags Character string of tags to attach to this \code{outputToSave} in
#'                 the \code{CacheRepo}
#'
#' @export
#' @inheritParams Cache
#' @inheritParams saveToCache
writeFuture <- function(written, outputToSave, cacheRepo, userTags,
                        drv = getOption("reproducible.drv", RSQLite::SQLite()),
                        conn = getOption("reproducible.conn", NULL),
                        cacheId, linkToCacheId = NULL) {
  counter <- 0
  # browser(expr = exists("._writeFuture_1"))
  if (!CacheIsACache(cachePath = cacheRepo, drv = drv, conn = conn)) {
    stop("That cacheRepo does not exist")
  }

  if (useDBI()) {
    if (missing(cacheId)) {
      cacheId <- .robustDigest(outputToSave)
    }
    output <- saveToCache(cachePath = cacheRepo, drv = drv, userTags = userTags,
                          conn = conn, obj = outputToSave, cacheId = cacheId,
                          linkToCacheId = linkToCacheId)
    saved <- cacheId
  }
  return(saved)
}

.fnCleanup <- function(FUN, ..., callingFun) {
  modifiedDots <- list(...)
  isPipe <- isTRUE(!is.null(modifiedDots$._pipe))
  originalDots <- modifiedDots

  # browser(expr = exists("._fnCleanup_1"))
  # If passed with 'quote'
  if (!is.function(FUN)) {
    parsedFun <- parse(text = FUN)
    evaledParsedFun <- eval(parsedFun[[1]], envir = modifiedDots)
    if (is.function(evaledParsedFun)) {
      tmpFUN <- evaledParsedFun
      mc <- match.call(tmpFUN, FUN)
      FUN <- tmpFUN # nolint
      originalDots <- append(originalDots, as.list(mc[-1]))
      modifiedDots <- append(modifiedDots, as.list(mc[-1]))
    }
    fnDetails <- list(functionName = as.character(parsedFun[[1]]))
  } else {
    if (!isPipe) {
      fnDetails <- getFunctionName(FUN, #...,
                                   originalDots = originalDots,
                                   isPipe = isPipe)

      # i.e., if it did extract the name
      if (!is.na(fnDetails$functionName)) {
        if (is.primitive(FUN)) {
          modifiedDots <- list(...)
        } else {
          nams <- names(modifiedDots)
          if (!is.null(nams)) {
            whHasNames <- nams != "" & !is.na(nams)
            whHasNames[is.na(whHasNames)] <- FALSE
            namedNames <- names(modifiedDots)[whHasNames]
            modifiedDotsArgsToUse <- namedNames[!namedNames %in% names(.formalsCache)]#  c("", names(formals(FUN)))
            modifiedDots <- append(modifiedDots[!whHasNames], modifiedDots[modifiedDotsArgsToUse])
          }
          modifiedDots <- as.list(
            match.call(FUN, as.call(append(list(FUN), modifiedDots))))[-1]
        }
      }
    } else {
      fnDetails <- list()
    }
  }

  isDoCall <- FALSE
  forms <- suppressWarnings(names(formals(FUN)))
  if (!is.null(fnDetails$functionName)) {
    if (!is.na(fnDetails$functionName)) {
      if (fnDetails$functionName == "do.call") {
        isDoCall <- TRUE
        possFunNames <- lapply(substitute(placeholderFunction(...))[-1],
                               deparse, backtick = TRUE)
        #doCallMatched <- as.list(match.call(modifiedDots$what, call = as.call(append(list(modifiedDots$what), modifiedDots$args))))
        doCallMatched <- as.list(match.call(do.call, as.call(append(list(do.call), possFunNames))))
        whatArg <- doCallMatched$what

        whArgs <- which(names(modifiedDots) %in% "args")
        doCallFUN <- modifiedDots$what

        forms <- names(formals(doCallFUN))
        if (isS4(doCallFUN)) {
          fnName <- doCallFUN@generic

          # Not easy to selectMethod -- can't have trailing "ANY" -- see ?selectMethod last
          #  paragraph of "Using findMethod()" which says:
          # "Notice also that the length of the signature must be what the corresponding
          #  package used. If thisPkg had only methods for one argument, only length-1
          # signatures will match (no trailing "ANY"), even if another currently loaded
          # package had signatures with more arguments.
          numArgsInSig <- try({
            suppressWarnings({
              info <- attr(utils::methods(fnName), "info")# from hadley/sloop package s3_method_generic
            })
            max(unlist(lapply(strsplit(rownames(info), split = ","), length) ) - 1)
          }, silent = TRUE)
          matchOn <- doCallFUN@signature[seq(numArgsInSig)]

          mc <- as.list(match.call(doCallFUN, as.call(append(fnName, modifiedDots[[whArgs]])))[-1])
          mc <- mc[!unlist(lapply(mc, is.null))]
          argsClasses <- unlist(lapply(mc, function(x) class(x)[1]))
          argsClasses <- argsClasses[names(argsClasses) %in% matchOn]
          missingArgs <- matchOn[!(matchOn %in% names(argsClasses))]

          missings <- rep("missing", length(missingArgs))
          names(missings) <- missingArgs
          argsClasses <- c(argsClasses, missings)

          forms <- names(formals(selectMethod(fnName, signature = argsClasses)))
          fnDetails$functionName <- fnName
        } else {
          classes <- try({
            suppressWarnings({
              info <- attr(utils::methods(whatArg), "info") # from hadley/sloop pkg s3_method_generic
            })
            classes <- unlist(lapply(strsplit(rownames(info), split = "\\."), function(x) x[[2]]))
            gsub("-method$", "", classes)
          }, silent = TRUE)
          if (is(classes, "try-error")) classes <- NA_character_
          mc <- as.list(match.call(doCallFUN, as.call(append(whatArg, modifiedDots[[whArgs]])))[-1])
          theClass <- classes[unlist(lapply(classes, function(x) inherits(mc[[1]], x)))]
          forms <- if (length(theClass)) {
            aa <- try(names(formals(paste0(whatArg, ".", theClass))))
            aa
          } else {
            if (all(is.na(classes))) {
              names(formals(doCallFUN))
            } else {
              names(formals(whatArg))
            }
          }
        }
      }
    }
  }
  # Determine if some of the Cache arguments are also arguments to FUN
  callingFunFormals <- if (callingFun == "Cache") .namesCacheFormals else names(formals(callingFun))
  if (isDoCall) {
    argNamesOfAllClasses <- forms
    fnDetails$.FUN <- format(doCallFUN) # nolint
    formalsInCallingAndFUN <- argNamesOfAllClasses[argNamesOfAllClasses %in% callingFunFormals]
  } else {
    fnDetails$.FUN <- format(FUN) # nolint
    formalsInCallingAndFUN <- forms[forms %in% callingFunFormals]
  }

  # If arguments to FUN and Cache are identical, pass them through to FUN
  if (length(formalsInCallingAndFUN)) {
    formalsInCallingAndFUN <- grep("\\.\\.\\.", formalsInCallingAndFUN, value = TRUE, invert = TRUE)
    commonArguments <- try(mget(formalsInCallingAndFUN, inherits = FALSE,
                                envir = parent.frame()),
                           silent = TRUE)
    if (!is(commonArguments, "try-error")) {
      if (isDoCall) {
        modifiedDots$args[formalsInCallingAndFUN] <- commonArguments
      } else {
        modifiedDots[formalsInCallingAndFUN] <- commonArguments
      }
    }
  }
  # browser(expr = exists("._fnCleanup_2"))
  return(append(fnDetails, list(originalDots = originalDots, FUN = FUN, isPipe = isPipe,
                                modifiedDots = modifiedDots, isDoCall = isDoCall,
                                formalArgs = forms)))
}

#' Set subattributes within a list by reference
#'
#' This uses \code{data.table::setattr}, but in the case where there is
#' only a single element within a list attribute.
#' @param object An arbitrary object
#' @param attr The attribute name (that is a list object) to change
#' @param subAttr The list element name to change
#' @param value The new value
#'
#' @export
#' @importFrom data.table setattr
#' @rdname setSubAttrInList
.setSubAttrInList <- function(object, attr, subAttr, value) {
  .CacheAttr <- attr(object, attr)
  if (is.null(.CacheAttr)) .CacheAttr <- list()
  .CacheAttr[[subAttr]] <- value
  setattr(object, attr, .CacheAttr)
}

#' The exact digest function that \code{Cache} uses
#'
#' This can be used by a user to pre-test their arguments before running
#' \code{Cache}, for example to determine whether there is a cached copy.
#'
#'
#' @param ... passed to \code{.robustDigest}; this is generally empty except
#'    for advanced use.
#' @param objsToDigest A list of all the objects (e.g., arguments) to be digested
#' @param calledFrom a Character string, length 1, with the function to
#'    compare with. Default is "Cache". All other values may not produce
#'    robust CacheDigest results.
#'
#' @inheritParams Cache
#'
#' @return
#' A list of length 2 with the \code{outputHash}, which is the digest
#' that Cache uses for \code{cacheId} and also \code{preDigest}, which is
#' the digest of each sub-element in \code{objsToDigest}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   a <- Cache(rnorm, 1)
#'   CacheDigest(list(rnorm, 1))
#'
#' }
CacheDigest <- function(objsToDigest, algo = "xxhash64", calledFrom = "Cache", quick = FALSE, ...) {
  if (identical("Cache", calledFrom)) {
    namesOTD <- names(objsToDigest)
    lengthChars <- nchar(namesOTD)
    if (!any(namesOTD == "FUN")) {
      zeroLength <- which(lengthChars == 0)
      if (sum(zeroLength ) > 0) {
        names(objsToDigest)[zeroLength[1]] <- ".FUN"
      }
    }
  }

  # need to omit arguments that are in Cache function call
  objsToDigest[names(objsToDigest) %in% .defaultCacheOmitArgs] <- NULL

  if (is.character(quick) || isTRUE(quick)) {
    quickObjs <- if (isTRUE(quick)) rep(TRUE, length(objsToDigest)) else
      names(objsToDigest) %in% quick
    objsToDigestQuick <- objsToDigest[quickObjs]
    objsToDigest <- objsToDigest[!quickObjs]

    preDigestQuick <- lapply(objsToDigestQuick, function(x) {
      # remove the "newCache" attribute, which is irrelevant for digest
      if (!is.null(attr(x, ".Cache")$newCache)) {
        .setSubAttrInList(x, ".Cache", "newCache", NULL)
        if (!identical(attr(x, ".Cache")$newCache, NULL)) stop("attributes are not correct 1")
      }
      .robustDigest(x, algo = algo, quick = TRUE, ...)
    })

  }


  preDigest <- lapply(objsToDigest, function(x) {
    # remove the "newCache" attribute, which is irrelevant for digest
    if (!is.null(attr(x, ".Cache")$newCache)) {
      .setSubAttrInList(x, ".Cache", "newCache", NULL)
      if (!identical(attr(x, ".Cache")$newCache, NULL)) stop("attributes are not correct 1")
    }
    .robustDigest(x, algo = algo, quick = FALSE, ...)
  })
  if (is.character(quick)) {
    preDigest <- append(preDigest, preDigestQuick)
  }

  res <- if (isTRUE(getOption("reproducible.useNewDigestAlgorithm") > 0)) {
    .robustDigest(unname(sort(unlist(preDigest))), algo = algo, quick = TRUE, ...)
  } else {
    if (!requireNamespace("fastdigest"))
      stop(requireNamespaceMsg("fastdigest", "to use options('reproducible.useNewDigestAlgorithm' = FALSE"))
    fastdigest::fastdigest(preDigest)
  }
  list(outputHash = res, preDigest = preDigest)
}

#' @importFrom data.table setDT setkeyv melt
#' @keywords internal
.findSimilar <- function(localTags, showSimilar, scalls, preDigestUnlistTrunc, userTags,
                         functionName,
                         useCache = getOption("reproducible.useCache", TRUE),
                         verbose = getOption("reproducible.verbose", TRUE)) {

  setDT(localTags)
  isDevMode <- identical("devMode", useCache)
  if (isDevMode) {
    showSimilar <- 1
  }
  # browser(expr = exists("._findSimilar_1"))
  # deal with tag
  userTags2 <- .getOtherFnNamesAndTags(scalls = scalls)
  userTags2 <- c(userTags2, paste("preDigest", names(preDigestUnlistTrunc),
                                  preDigestUnlistTrunc, sep = ":"))
  userTags3 <- c(userTags, userTags2)
  hashName <- .cacheTableHashColName()
  cn <- if (any(colnames(localTags) %in% "tag")) "tag" else "tagKey"

  if (!(cn %in% "tag")) {
    tag <- localTags[paste(tagKey , get(.cacheTableTagColName()), sep = ":"),
                     on = .cacheTableHashColName()][[hashName]]
  }
  aa <- localTags[tag %in% userTags3][,.N, keyby = hashName]
  setkeyv(aa, "N")
  similar <- if (NROW(aa) > 0) {
    localTags[tail(aa, as.numeric(showSimilar)), on = hashName][N == max(N)]
  } else {
    localTags
  }
  if (NROW(similar)) {
    if (cn %in% "tag") {
      similar2 <- similar[grepl("preDigest", tag)]
      cacheIdOfSimilar <- similar[grepl("cacheId", tag)][[.cacheTableTagColName("tag")]]
      cacheIdOfSimilar <- unlist(strsplit(cacheIdOfSimilar, split = ":"))[2]
      similar2[, `:=`(fun = unlist(lapply(strsplit(get(cn), split = ":"), function(xx) xx[[2]])),
                      hash = unlist(lapply(strsplit(get(cn), split = ":"), function(xx) xx[[3]])))]
    } else {
      Tag <- similar[paste(tagKey , get(.cacheTableTagColName()), sep = ":"),
                     on = .cacheTableHashColName()][[hashName]]
      similar2 <- similar[grepl("preDigest", Tag)]
      cacheIdOfSimilar <- unique(similar[[.cacheTableHashColName()]])
      similar2[, `:=`(fun = unlist(lapply(strsplit(get(.cacheTableTagColName()), split = ":"),
                                          function(xx) xx[[1]])),
                      hash = unlist(lapply(strsplit(get(.cacheTableTagColName()), split = ":"),
                                           function(xx) xx[[2]])))]
    }

    a <- setDT(as.list(preDigestUnlistTrunc))
    a <- melt(a, measure.vars = seq_along(names(a)), variable.name = "fun", value.name = "hash")

    similar2 <- similar2[a, on = "fun", nomatch = NA]
    similar2[, differs := (i.hash != hash)]

    similar2[!(fun %in% names(preDigestUnlistTrunc)), differs := NA]
    similar2[(hash %in% "other"), deeperThan3 := TRUE]
    similar2[(hash %in% "other"), differs := NA]
    differed <- FALSE
    if (isDevMode) {
      messageCache("    ------ devMode -------", verbose = verbose)
      messageCache("    This call to cache will replace", verbose = verbose)
    } else {
      # messageCache(" ------ showSimilar -------", verbose = verbose)
      messageCache("    (reproducible.showSimilar option is TRUE) This Cache call ",
                   if (!is.null(functionName)) paste0("with FUN arg of '",functionName,"' "),
                   "differs from", verbose = verbose)
    }
    messageCache(paste0("    the next closest cacheId ", cacheIdOfSimilar), verbose = verbose)

    if (sum(similar2[differs %in% TRUE]$differs, na.rm = TRUE)) {
      differed <- TRUE
      messageCache("    ... because of (a) different ", paste(similar2[differs %in% TRUE]$fun, collapse = ", "),
                   verbose = verbose)
    }

    if (length(similar2[is.na(differs) & deeperThan3 == TRUE]$differs)) {
      differed <- TRUE
      messageCache("    ... possible, unknown, differences in a nested list ",
                           "that is deeper than ",getOption("reproducible.showSimilarDepth", 3)," in ",
                           paste(collapse = ", ", as.character(similar2[deeperThan3 == TRUE]$fun)),
                   verbose = verbose)
    }
    missingArgs <- similar2[is.na(deeperThan3) & is.na(differs)]$fun
    if (length(missingArgs)) {
      differed <- TRUE
      messageCache("    ... because of (a) new argument(s): ",
                   paste(as.character(missingArgs), collapse = ", "), verbose = verbose)
    }
    if (isDevMode) {
      messageCache(" ------ end devMode -------", verbose = verbose)
    } #else {
      #messageCache(" ------ end showSimilar -------", verbose = verbose)
    #}

  } else {
    if (!identical("devMode", useCache))
      messageCache("There is no similar item in the cacheRepo", verbose = verbose)
  }
}


#' @keywords internal
.defaultCacheOmitArgs <- c("useCloud", "checksumsFileID", "cloudFolderID",
                           "notOlderThan", ".objects", "outputObjects", "algo", "cacheRepo",
                           "length", "compareRasterFileLength", "userTags", "digestPathContent",
                           "omitArgs", "classOptions", "debugCache", "sideEffect", "makeCopy",
                           "quick", "verbose", "cacheId", "useCache", "showSimilar", "cl")

#' @keywords internal
verboseTime <- function(verbose) {
  if (verbose > 3) {
    return(Sys.time())
  }
}

#' @keywords internal
verboseMessage1 <- function(verbose, userTags) {
  if (verbose > 2)
    messageCache("Using devMode; overwriting previous Cache entry with tags: ",
            paste(userTags, collapse = ", "),
            verbose = verbose)
  invisible(NULL)
}

#' @keywords internal
verboseMessage2 <- function(verbose) {
  if (verbose > 2)
    messageCache("Using devMode; Found entry with identical userTags, ",
            "but since it is very different, adding new entry",
            verbose = verbose)
  invisible(NULL)
}

#' @keywords internal
verboseMessage3 <- function(verbose, artifact) {
  if (length(unique(artifact)) > 1) {
    if (verbose > 2)
      messageCache("Using devMode, but userTags are not unique; defaulting to normal useCache = TRUE",
                   verbose = verbose)
  }
}

#' @keywords internal
verboseDF1 <- function(verbose, functionName, startRunTime) {
  if (verbose > 3) {
    endRunTime <- Sys.time()
    verboseDF <- data.frame(
      functionName = functionName,
      component = paste("Running", functionName),
      elapsedTime = as.numeric(difftime(endRunTime, startRunTime, units = "secs")),
      units = "secs",
      stringsAsFactors = FALSE
    )

    if (exists("verboseTiming", envir = .reproEnv)) {
      .reproEnv$verboseTiming <- rbind(.reproEnv$verboseTiming, verboseDF)
    }
  }
}

#' @keywords internal
verboseDF2 <- function(verbose, functionName, startSaveTime) {
  if (verbose > 3) {
    endSaveTime <- Sys.time()
    verboseDF <-
      data.frame(
        functionName = functionName,
        component = "Saving to repo",
        elapsedTime = as.numeric(difftime(endSaveTime, startSaveTime, units = "secs")),
        units = "secs",
        stringsAsFactors = FALSE
      )

    if (exists("verboseTiming", envir = .reproEnv)) {
      .reproEnv$verboseTiming <- rbind(.reproEnv$verboseTiming, verboseDF)
    }
  }
}

#' @keywords internal
verboseDF3 <- function(verbose, functionName, startCacheTime) {
  if (verbose > 3) {
    endCacheTime <- Sys.time()
    verboseDF <- data.frame(functionName = functionName,
                            component = "Whole Cache call",
                            elapsedTime = as.numeric(difftime(endCacheTime, startCacheTime,
                                                              units = "secs")),
                            units = "secs",
                            stringsAsFactors = FALSE)

    if (exists("verboseTiming", envir = .reproEnv)) {
      .reproEnv$verboseTiming <- rbind(.reproEnv$verboseTiming, verboseDF)
    }
  }
}

#' @keywords internal
determineNestedTags <- function(envir, mc, userTags) {
  argsNoNesting <- "useCloud"
  # if (R.version[['minor']] <= "4.0") {
  #   # match.call changed how it worked between 3.3.2 and 3.4.x MUCH SLOWER
  #   lsCurEnv <- ls(all.names = TRUE, envir = envir)
  #   objs <- lsCurEnv[lsCurEnv %in% .namesCacheFormals]
  #   objs <- objs[match(.namesCacheFormals, objs)]# sort so same order as R > 3.4
  #   args <- mget(objs, envir = envir)
  #   forms <- lapply(.formalsCache, function(x) eval(x))
  #   objOverride <- unlist(lapply(objs, function(obj) identical(args[[obj]], forms[[obj]])))
  #   userCacheArgs <- objs[!objOverride]
  #   namesUserCacheArgs <- userCacheArgs
  # } else {
  mc <- as.list(mc[-1])
  namesMatchCall <- names(mc)
  namesMatchCall <- namesMatchCall[!namesMatchCall %in% argsNoNesting]
  userCacheArgs <- match(.namesCacheFormals, namesMatchCall)
  namesUserCacheArgs <- namesMatchCall[userCacheArgs[!is.na(userCacheArgs)]]
  objOverride <- is.na(userCacheArgs)
  #}

  oldUserTags <- NULL
  prevUserTags <- FALSE
  prevValsInitial <- NULL
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
      # on.exit({
      #   .reproEnv$userTags <- oldUserTags
      # }, add = TRUE)
    }

    if (any(!prevVals)) {
      # don't override previous values -- except for userTags
      list2env(mget(namesUserCacheArgs[!prevVals], envir = envir), .reproEnv)
    }
    prevValsInitial <- prevVals
  }

  if (any(objOverride)) {
    # get from .reproEnv
    lsDotReproEnv <- ls(.reproEnv)
    prevVals <- .namesCacheFormals[objOverride] %in% lsDotReproEnv
    if (any(prevVals)) {
      list2env(mget(.namesCacheFormals[objOverride][prevVals], .reproEnv), envir = envir)
    }
  }

  return(list(oldUserTags = oldUserTags, namesUserCacheArgs = namesUserCacheArgs,
              prevVals = prevValsInitial, prevUserTags = prevUserTags,
              objOverride = objOverride))
}

getCacheRepos <- function(cacheRepo, modifiedDots, verbose = getOption("reproducible.verbose", 1)) {
  if (is.null(cacheRepo)) {
    cacheRepos <- .checkCacheRepo(modifiedDots, create = TRUE, verbose = verbose)
  } else {
    cacheRepos <- lapply(cacheRepo, function(repo) {
      repo <- checkPath(repo, create = TRUE)
    })
  }
  return(cacheRepos)
}

devModeFn1 <- function(localTags, userTags, scalls, preDigestUnlistTrunc, useCache, verbose,
                       isInRepo, outputHash) {
  # browser(expr = exists("._devModeFn1_1"))
  userTags <- gsub(".*:(.*)", "\\1", userTags)
  isInRepoAlt <- localTags[localTags[[.cacheTableTagColName("tag")]] %in% userTags, , drop = FALSE]
  data.table::setDT(isInRepoAlt)
  if (NROW(isInRepoAlt) > 0)
    isInRepoAlt <- isInRepoAlt[, iden := identical(sum(get(.cacheTableTagColName("tag"))
                                                       %in% userTags), length(userTags)),
                               by = eval(.cacheTableHashColName())][iden == TRUE]
  if (NROW(isInRepoAlt) > 0 && length(unique(isInRepoAlt[[.cacheTableHashColName()]])) == 1) {
    newLocalTags <- localTags[localTags[[.cacheTableHashColName()]] %in% isInRepoAlt[[.cacheTableHashColName()]],]
    tags1 <- grepl(paste0("(",
                          #paste("accessed", "cacheId", "class", "date", "format", "function", "inCloud",
                          #      "name", "object.size", "otherFunctions", "preDigest", "file.size",
                          #      sep = "|"),
                          paste(.defaultUserTags, collapse = "|"),
                          ")"),
                   newLocalTags[["tagKey"]])
    localTagsAlt <- newLocalTags[!tags1,]
    # browser(expr = exists("._devModeFn1_2"))

    if (all(localTagsAlt[[.cacheTableTagColName("tag")]] %in% userTags)) {
      mess <- capture.output(type = "output", {
        similars <- .findSimilar(newLocalTags, scalls = scalls,
                                 preDigestUnlistTrunc = preDigestUnlistTrunc,
                                 userTags = userTags,
                                 useCache = useCache,
                                 verbose = verbose)
      })
      similarsHaveNA <- sum(is.na(similars$differs))
      #similarsAreDifferent <- sum(similars$differs == TRUE, na.rm = TRUE)
      #likelyNotSame <- sum(similarsHaveNA, similarsAreDifferent)/NROW(similars)

      if (similarsHaveNA < 2) {
        verboseMessage1(verbose, userTags)
        if (useDBI()) {
          uniqueCacheId <- unique(isInRepoAlt[[.cacheTableHashColName()]])
          outputHash <- uniqueCacheId[uniqueCacheId %in% newLocalTags[[.cacheTableHashColName()]]]
        } else {
          outputHash <- gsub("cacheId:", "",
                             newLocalTags[newLocalTags[[.cacheTableHashColName()]] %in%
                                            isInRepoAlt[[.cacheTableHashColName()]] &
                                            startsWith(newLocalTags[[.cacheTableTagColName("tag")]],
                                                       "cacheId"), ][[.cacheTableTagColName()]])
        }
        isInRepo <- isInRepoAlt
      } else {
        verboseMessage2(verbose)
      }
    }
    needFindByTags <- TRUE # it isn't there
  } else {
    verboseMessage3(verbose, isInRepoAlt[[.cacheTableHashColName()]])
    needFindByTags <- FALSE # it isn't there
  }
  return(list(isInRepo = isInRepo, outputHash = outputHash, needFindByTags = needFindByTags))
}

cloudFolderFromCacheRepo <- function(cacheRepo)
  paste0(basename2(dirname(cacheRepo)), "_", basename2(cacheRepo))

.defaultUserTags <- c("function", "class", "object.size", "accessed", "inCloud",
                      "otherFunctions", "preDigest", "file.size", "cacheId",
                      "elapsedTimeDigest", "elapsedTimeFirstRun", "resultHash", "elapsedTimeLoad")

.defaultOtherFunctionsOmit <- c("(test_","with_reporter", "force", "Restart", "with_mock",
                                "eval", "::", "\\$", "\\.\\.", "standardGeneric",
                                "Cache", "tryCatch", "doTryCatch", "withCallingHandlers",
                                "FUN", "capture", "withVisible)")


dealWithRastersOnRecovery <- function(output, cacheRepo, cacheId,
                                      drv = getOption("reproducible.drv", RSQLite::SQLite()),
                                      conn = getOption("reproducible.conn", NULL)) {
  if (isTRUE(getOption("reproducible.useNewDigestAlgorithm") < 2)) {
    return(dealWithRastersOnRecovery2(output, cacheRepo, cacheId,
                               drv, conn))
  }

  if (is(output, "list") && !is.null(output$origRaster) && !is.null(output$cacheRaster)) {
    origFilenames <- if (is(output$origRaster, "Raster")) {
      Filenames(output$origRaster) # This is legacy piece which allows backwards compatible
    } else {
      output$origRaster
    }

    filesExist <- file.exists(origFilenames)
    cacheFilenames <- Filenames(output$cacheRaster)
    filesExistInCache <- file.exists(cacheFilenames)
    if (any(!filesExistInCache)) {
      fileTails <- gsub("^.+(rasters.+)$", "\\1", cacheFilenames)
      correctFilenames <- file.path(cacheRepo, fileTails)
      filesExistInCache <- file.exists(correctFilenames)
      if (all(filesExistInCache)) {
        cacheFilenames <- correctFilenames
      } else {
        stop("File-backed raster files in the cache are corrupt for cacheId: ", cacheId)
      }

    }
    out <- hardLinkOrCopy(cacheFilenames[filesExistInCache],
                          origFilenames[filesExistInCache], overwrite = TRUE)

    newOutput <- updateFilenameSlots(output$cacheRaster,
                                     Filenames(output$cacheRaster, allowMultiple = FALSE),
                                     newFilenames = grep("\\.gri$", origFilenames, value = TRUE, invert = TRUE))
    output <- newOutput
    .setSubAttrInList(output, ".Cache", "newCache", FALSE)
  }
  output
}

# This one is old, overly complicated; defunct
dealWithRastersOnRecovery2 <- function(output, cacheRepo, cacheId,
                                      drv = getOption("reproducible.drv", RSQLite::SQLite()),
                                      conn = getOption("reproducible.conn", NULL)) {
  # This function is because the user doesn't want the path of the file-backed raster to
  #   be in the cacheRepo --> they want it in its original file location
  #   If it is in both, take the one in the original location; if it has been deleted
  #   from the original location, then grab it from cache and put it in original place
  if (is(output, "list")) {
    if (identical(names(output), c("origRaster", "cacheRaster"))) {
      origFilenames <- Filenames(output$origRaster)
      cacheFilenames <- Filenames(output$cacheRaster)
      origStillExist <- file.exists(origFilenames)
      origFilenamesNeed <- origFilenames[!origStillExist]
      cacheFilenamesNeed <- cacheFilenames[!origStillExist]
      origFilenamesNeedDig <- origFilenames[origStillExist]
      cacheFilenamesNeedDig <- cacheFilenames[origStillExist]
      if (any(origStillExist)) {
        cacheFilenamesDig <- unlist(.robustDigest(asPath(cacheFilenamesNeedDig)))
        origFilenamesDig <- unlist(.robustDigest(asPath(origFilenamesNeedDig)))
        whichUnchanged <- cacheFilenamesDig == origFilenamesDig
        if (any(whichUnchanged)) {
          origFilenamesNeedDig <- origFilenamesNeedDig[!whichUnchanged]
          cacheFilenamesNeedDig <- cacheFilenamesNeedDig[!whichUnchanged]
        }
        cacheFilenamesNeed <- c(cacheFilenamesNeed, cacheFilenamesNeedDig)
        origFilenamesNeed <- c(origFilenamesNeed, origFilenamesNeedDig)
      }
      dirnamesRasters <- unique(dirname(dirname(cacheFilenamesNeed)))
      if (length(dirnamesRasters))
        if (!isTRUE(all.equal(dirnamesRasters, cacheRepo))) { # if this is a moved cache, the filenames in the cache will be wrong
          cacheFilenamesNeed2 <- gsub(dirnamesRasters, cacheRepo, cacheFilenamesNeed)
          wrongFilenames <- file.exists(cacheFilenamesNeed2)
          if (any(wrongFilenames)) {
            output$cacheRaster <- updateFilenameSlots(output$cacheRaster, cacheFilenamesNeed[wrongFilenames],
                                                      newFilenames = cacheFilenamesNeed2[wrongFilenames])
            fs <- saveFileInCacheFolder(output, cachePath = cacheRepo, cacheId = cacheId)
            cacheFilenamesNeed[wrongFilenames] <- cacheFilenamesNeed2[wrongFilenames]
          }

        }
      copyFile(from = cacheFilenamesNeed, to = origFilenamesNeed, overwrite = TRUE)
      output <- output$origRaster
      .setSubAttrInList(output, ".Cache", "newCache", FALSE)
    }
  }
  output
}
