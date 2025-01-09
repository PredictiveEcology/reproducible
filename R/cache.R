utils::globalVariables(c(
  ".", "artifact", "createdDate", "deeperThan3", "differs", "fun", "hash",
  "i.hash", "iden", "N", "tag", "tagKey", "tagValue"
))

.reproEnv <- new.env(parent = asNamespace("reproducible"))

#' Saves a wide variety function call outputs to disk and optionally RAM, for recovery later
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
#' from disk, unless `options("reproducible.useMemoise" = TRUE)`, then the 2nd time
#' will recover the object from RAM and is normally much faster (at the expense of RAM use).
#'
#' @details
#'
#' There are other similar functions in the R universe.
#' This version of Cache has been used as part of a robust continuous workflow approach.
#'  As a result, we have tested it with many "non-standard" R objects (e.g., `RasterLayer`,
#' `Spat*` objects) and environments (which are always unique, so do not cache readily).
#'
#' This version of the `Cache` function accommodates those four special,
#' though quite common, cases by:
#' \enumerate{
#'   \item converting any environments into list equivalents;
#'   \item identifying the dispatched S4 method (including those made through
#'         inheritance) before hashing so the correct method is being cached;
#'   \item by hashing the linked file, rather than the raster object.
#'         Currently, only file-backed `Raster*` or `Spat*` objects are digested
#'         (e.g., not `ff` objects, or any other R object where the data
#'         are on disk instead of in RAM);
#'   \item Uses [digest::digest()]
#'         This is used for file-backed objects as well.
#'   \item Cache will save arguments passed by user in a hidden environment. Any
#'         nested Cache functions will use arguments in this order: 1) actual arguments
#'         passed at each Cache call; 2) any inherited arguments from an outer Cache
#'         call; 3) the default values of the Cache function. See section on *Nested Caching*.
#' }
#'
#' `Cache` will add a tag to the entry in the cache database called `accessed`,
#' which will assign the time that it was accessed, either read or write.
#' That way, cached items can be shown (using `showCache`) or removed (using
#' `clearCache`) selectively, based on their access dates, rather than only
#' by their creation dates. See example in [clearCache()].
#'
#' @section Nested Caching:
#' Commonly, Caching is nested, i.e., an outer function is wrapped in a `Cache`
#' function call, and one or more inner functions are also wrapped in a `Cache`
#' function call. A user *can* always specify arguments in every Cache function
#' call, but this can get tedious and can be prone to errors. The normal way that
#' \R handles arguments is it takes the user passed arguments if any, and
#' default arguments for all those that have no user passed arguments. We have inserted
#' a middle step. The order or precedence for any given `Cache` function call is
#' 1. user arguments, 2. inherited arguments, 3. default arguments. At this time,
#' the top level `Cache` arguments will propagate to all inner functions unless
#' each individual `Cache` call has other arguments specified, i.e., "middle"
#' nested `Cache` function calls don't propagate their arguments to further "inner"
#' `Cache` function calls.  See example.
#'
#' `userTags` is unique of all arguments: its values will be appended to the
#' inherited `userTags`.
#'
#' @section quick:
#' The `quick` argument is attempting to sort out an ambiguity with character strings:
#' are they file paths or are they simply character strings. When `quick = TRUE`,
#' `Cache` will treat these as character strings; when `quick = FALSE`,
#' they will be attempted to be treated as file paths first; if there is no file, then
#' it will revert to treating them as character strings. If user passes a
#' character vector to this, then this will behave like `omitArgs`:
#' `quick = "file"` will treat the argument `"file"` as character string.
#'
#' The most often encountered situation where this ambiguity matters is in arguments about
#' filenames: is the filename an input pointing to an object whose content we want to
#' assess (e.g., a file-backed raster), or an output (as in saveRDS) and it should not
#' be assessed. If only run once, the output file won't exist, so it will be treated
#' as a character string. However, once the function has been run once, the output file
#' will exist, and `Cache(...)` will assess it, which is incorrect. In these cases,
#' the user is advised to use `quick = "TheOutputFilenameArgument"` to
#' specify the argument whose content on disk should not be assessed, but whose
#' character string should be assessed (distinguishing it from `omitArgs =
#' "TheOutputFilenameArgument"`, which will not assess the file content nor the
#' character string).
#'
#' This is relevant for objects of class `character`, `Path` and
#' `Raster` currently. For class `character`, it is ambiguous whether
#' this represents a character string or a vector of file paths. If it is known
#' that character strings should not be treated as paths, then `quick =
#' TRUE` is appropriate, with no loss of information. If it is file or
#' directory, then it will digest the file content, or `basename(object)`.
#' For class `Path` objects, the file's metadata (i.e., filename and file
#' size) will be hashed instead of the file contents if `quick = TRUE`. If
#' set to `FALSE` (default), the contents of the file(s) are hashed. If
#' `quick = TRUE`, `length` is ignored. `Raster` objects are
#' treated as paths, if they are file-backed.
#'
#' @section Caching Speed:
#' Caching speed may become a critical aspect of a final product. For example,
#' if the final product is a shiny app, rerunning the entire project may need
#' to take less then a few seconds at most.
#' There are 3 arguments that affect `Cache` speed: `quick`, `length`, and `algo`.
#' `quick` is passed to `.robustDigest`, which currently
#' only affects `Path` and `Raster*` class objects.
#' In both cases, `quick` means that little or no disk-based information will be assessed.
#'
#'
#' @section Filepaths:
#' If a function has a path argument, there is some ambiguity about what should be
#' done. Possibilities include:
#' \enumerate{
#'   \item hash the string as is (this will be very system specific, meaning a
#'         `Cache` call will not work if copied between systems or directories);
#'   \item hash the `basename(path)`;
#'   \item hash the contents of the file.
#' }
#' If paths are passed in as is (i.e,. character string), the result will not be predictable.
#' Instead, one should use the wrapper function `asPath(path)`, which sets the
#' class of the string to a `Path`, and one should decide whether one wants
#' to digest the content of the file (using `quick = FALSE`),
#' or just the filename (`(quick = TRUE)`). See examples.
#'
#' @section Stochasticity or randomness:
#' In general, it is expected that caching will only be used when randomness is not
#' desired, e.g., `Cache(rnorm(1))` is unlikely to be useful in many cases. However,
#' `Cache` captures the call that is passed to it, leaving all functions unevaluated.
#' As a result `Cache(glm, x ~ y, rnorm(1))` will not work as a means of forcing
#' a new evaluation each time, as the `rnorm(1)` is not evaluated before the call
#' is assessed against the cache database. To force a new call each time, evaluate
#' the randomness prior to the Cache call, e.g., `ran = rnorm(1)` then pass this
#' to `.cacheExtra`, e.g., `Cache(glm, x ~ y, .cacheExtra = ran)`
#'
#' @section `drv` and `conn`:
#' By default, `drv` uses an SQLite database. This can be sufficient for most cases.
#' However, if a user has dozens or more cores making requests to the Cache database,
#' it may be insufficient. A user can set up a different database backend, e.g.,
#' PostgreSQL that can handle multiple simultaneous read-write situations. See
#' \url{https://github.com/PredictiveEcology/SpaDES/wiki/Using-alternate-database-backends-for-Cache}.
#'
#'
#' @section `useCache`:
#' Logical or numeric. If `FALSE` or `0`, then the entire Caching
#' mechanism is bypassed and the
#' function is evaluated as if it was not being Cached. Default is
#' `getOption("reproducible.useCache")`), which is `TRUE` by default,
#' meaning use the Cache mechanism. This may be useful to turn all Caching on or
#' off in very complex scripts and nested functions. Increasing levels of numeric
#' values will cause deeper levels of Caching to occur (though this may not
#' work as expected in all cases). The following is no longer supported:
#' Currently, only implemented
#' in `postProcess`: to do both caching of inner `cropInputs`, `projectInputs`
#' and `maskInputs`, and caching of outer `postProcess`, use
#' `useCache = 2`; to skip the inner sequence of 3 functions, use `useCache = 1`.
#' For large objects, this may prevent many duplicated save to disk events.
#'
#' If `useCache = "overwrite"`
#' (which can be set with `options("reproducible.useCache" =
#' "overwrite")`), then the function invoke the caching mechanism but will purge
#' any entry that is matched, and it will be replaced with the results of the
#' current call.
#'
#' If `useCache = "devMode"`: The point of this mode is to facilitate using the Cache when
#' functions and datasets are continually in flux, and old Cache entries are
#' likely stale very often. In `devMode`, the cache mechanism will work as
#' normal if the Cache call is the first time for a function OR if it
#' successfully finds a copy in the cache based on the normal Cache mechanism.
#' It *differs* from the normal Cache if the Cache call does *not* find a copy
#' in the `cachePath`, but it does find an entry that matches based on
#' `userTags`. In this case, it will delete the old entry in the `cachePath`
#' (identified based on matching `userTags`), then continue with normal `Cache`.
#' For this to work correctly, `userTags` must be unique for each function call.
#' This should be used with caution as it is still experimental. Currently, if
#' `userTags` are not unique to a single entry in the cachePath, it will
#' default to the behaviour of `useCache = TRUE` with a message. This means
#' that `"devMode"` is most useful if used from the start of a project.
#'
#' @section `useCloud`:
#' This is experimental and there are many conditions under which this is known
#' to not work correctly. This is a way to store all or some of the local Cache in the cloud.
#' Currently, the only cloud option is Google Drive, via \pkg{googledrive}.
#' For this to work, the user must be or be able to be authenticated
#' with `googledrive::drive_auth`. The principle behind this
#' `useCloud` is that it will be a full or partial mirror of a local Cache.
#' It is not intended to be used independently from a local Cache. To share
#' objects that are in the Cloud with another person, it requires 2 steps. 1)
#' share the `cloudFolderID$id`, which can be retrieved by
#' `getOption("reproducible.cloudFolderID")$id` after at least one Cache
#' call has been made. 2) The other user must then set their  `cacheFolderID` in a
#' `Cache\(..., reproducible.cloudFolderID = \"the ID here\"\)` call or
#' set their option manually
#' `options\(\"reproducible.cloudFolderID\" = \"the ID here\"\)`.
#'
#' If `TRUE`, then this Cache call will download
#'   (if local copy doesn't exist, but cloud copy does exist), upload
#'   (local copy does or doesn't exist and
#'   cloud copy doesn't exist), or
#'   will not download nor upload if object exists in both. If `TRUE` will be at
#'   least 1 second slower than setting this to `FALSE`, and likely even slower as the
#'   cloud folder gets large. If a user wishes to keep "high-level" control, set this to
#'   `getOption("reproducible.useCloud", FALSE)` or
#'   `getOption("reproducible.useCloud", TRUE)` (if the default behaviour should
#'   be `FALSE` or `TRUE`, respectively) so it can be turned on and off with
#'   this option. NOTE: *This argument will not be passed into inner/nested Cache calls.*)
#'
#' @section Object attributes:
#' Users should be cautioned that object attributes may not be preserved, especially
#' in the case of objects that are file-backed, such as `Raster` or `SpatRaster` objects.
#' If a user needs to keep attributes, they may need to manually re-attach them to
#' the object after recovery. With the example of `SpatRaster` objects, saving
#' to disk requires `terra::wrap` if it is a memory-backed object. When running
#' `terra::unwrap` on this object, any attributes that a user had added are lost.
#'
#' @section `sideEffect`:
#' This feature is now deprecated. Do not use as it is ignored.
#'
#'
#'
#' @note As indicated above, several objects require pre-treatment before
#' caching will work as expected. The function `.robustDigest` accommodates this.
#' It is an S4 generic, meaning that developers can produce their own methods for
#' different classes of objects. Currently, there are methods for several types
#' of classes. See [.robustDigest()].
#'
#' @include cache-helpers.R
#' @include robustDigest.R
#'
#' @param FUN Either a function (e.g., `rnorm`), a function call (e.g., `rnorm(1)`),
#'             or an unevaluated function call (e.g., using `quote()`).
#'
#' @param ... Arguments passed to `FUN`, if `FUN` is not an expression.
#'
#' @param .objects Character vector of objects to be digested. This is only applicable
#'                if there is a list, environment (or similar) with named objects
#'                within it. Only this/these objects will be considered for caching,
#'                i.e., only use a subset of
#'                the list, environment or similar objects. In the case of nested list-type
#'                objects, this will only be applied outermost first.
#'
#' @param .cacheExtra A an arbitrary R object that will be included in the `CacheDigest`,
#'       but otherwise not passed into the `FUN`. If the user supplies a named list, then
#'       `Cache` will report which individual elements of `.cacheExtra` have changed
#'       when `options("reproducible.showSimilar" = TRUE)`. This can allow a user
#'       more control and understanding for debugging.
#'
#' @param .functionName A an arbitrary character string that provides a name that is different
#'       than the actual function name (e.g., "rnorm") which will be used for messaging. This
#'       can be useful when the actual function is not helpful for a user, such as `do.call`.
#'
#' @param outputObjects Optional character vector indicating which objects to
#'                      return. This is only relevant for list, environment (or similar) objects
#'
#' @param algo The digest algorithm to use. Default `xxhash64` (see [digest::digest()] for others).
#'
#' @param cacheRepo Same as `cachePath`, but kept for backwards compatibility.
#'
#' @param cachePath A repository used for storing cached objects.
#'                  This is optional if `Cache` is used inside a SpaDES module.
#' @param length Numeric. If the element passed to Cache is a `Path` class
#'        object (from e.g., `asPath(filename)`) or it is a `Raster` with
#'        file-backing, then this will be
#'        passed to `digest::digest`, essentially limiting the number of bytes
#'        to digest (for speed). This will only be used if `quick = FALSE`.
#'        Default is `getOption("reproducible.length")`, which is set to `Inf`.
#'
#' @param compareRasterFileLength Being deprecated; use `length`.
#'
#' @param omitArgs Optional character string of arguments in the FUN to omit from the digest.
#'
#' @param classOptions Optional list. This will pass into `.robustDigest` for
#'        specific classes. Should be options that the `.robustDigest` knows what
#'        to do with.
#'
#' @param debugCache Character or Logical. Either `"complete"` or `"quick"` (uses
#'        partial matching, so "c" or "q" work). `TRUE` is equivalent to `"complete"`.
#'        If `"complete"`, then the returned object from the Cache
#'        function will have two attributes, `debugCache1` and `debugCache2`,
#'        which are the entire `list(...)` and that same object, but after all
#'        `.robustDigest` calls, at the moment that it is digested using
#'        `digest`, respectively. This `attr(mySimOut, "debugCache2")`
#'        can then be compared to a subsequent call and individual items within
#'        the object `attr(mySimOut, "debugCache1")` can be compared.
#'        If `"quick"`, then it will return the same two objects directly,
#'        without evalutating the `FUN(...)`.
#'
#' @param makeCopy Now deprecated. Ignored if used.
#'
#' @param userTags A character vector with descriptions of the Cache function call. These
#'   will be added to the Cache so that this entry in the Cache can be found using
#'   `userTags` e.g., via [showCache()].
#'
#' @param notOlderThan A time. Load an object from the Cache if it was created after this.
#'
#' @param quick Logical or character. If `TRUE`,
#'        no disk-based information will be assessed, i.e., only
#'        memory content. See Details section about `quick` in [Cache()].
#'
#' @param verbose Numeric, -1 silent (where possible), 0 being very quiet,
#'        1 showing more messaging, 2 being more messaging, etc.
#'        Default is 1. Above 3 will output much more information about the internals of
#'        Caching, which may help diagnose Caching challenges. Can set globally with an
#'        option, e.g., `options('reproducible.verbose' = 0) to reduce to minimal`
#'
#' @param cacheId Character string. If passed, this will override the calculated hash
#'        of the inputs, and return the result from this `cacheId` in the `cachePath`.
#'        Setting this is equivalent to manually saving the output of this function, i.e.,
#'        the object will be on disk, and will be recovered in subsequent
#'        This may help in some particularly finicky situations
#'        where `Cache` is not correctly detecting unchanged inputs. This will guarantee
#'        the object will be identical each time; this may be useful in operational code.
#'
#' @param useCache Logical, numeric or `"overwrite"` or `"devMode"`. See details.
#'
#' @param useCloud Logical. See Details.
#'
#' @param cloudFolderID A googledrive dribble of a folder, e.g., using `drive_mkdir()`.
#'   If left as `NULL`, the function will create a cloud folder with name from last
#'   two folder levels of the `cachePath` path, :
#'   `paste0(basename(dirname(cachePath)), "_", basename(cachePath))`.
#'   This `cloudFolderID` will be added to `options("reproducible.cloudFolderID")`,
#'   but this will not persist across sessions. If this is a character string, it will
#'   treat this as a folder name to create or use on GoogleDrive.
#'
#' @param showSimilar A logical or numeric. Useful for debugging.
#'        If `TRUE` or `1`, then if the Cache
#'        does not find an identical archive in the `cachePath`, it will report (via message)
#'        the next most recent similar archive, and indicate which argument(s) is/are different.
#'        If a number larger than `1`, then it will report the N most recent similar archived
#'        objects.
#'
#' @param drv If using a database backend, `drv` must be an object that
#'   inherits from `DBIDriver` (e.g., `RSQLite::SQLite`).
#' @param conn an optional `DBIConnection` object, as returned by `dbConnect()`.
#'
#' @return Returns the value of the
#' function call or the cached version (i.e., the result from a previous call
#' to this same cached function with identical arguments).
#'
#' @seealso [showCache()], [clearCache()], [keepCache()],
#'   [CacheDigest()] to determine the digest of a given function or expression,
#'   as used internally within `Cache`, [movedCache()], [.robustDigest()], and
#'   for more advanced uses there are several helper functions,
#'   e.g., [rmFromCache()], [CacheStorageDir()]
#'
#' @author Eliot McIntire
#' @importFrom digest digest
#' @importFrom data.table setDT := setkeyv .N .SD
#' @importFrom utils object.size tail
#' @importFrom methods formalArgs
#' @export
#' @rdname Cache
#'
#' @example inst/examples/example_Cache.R
#'
Cache2 <-
  function(FUN, ..., notOlderThan = NULL,
           .objects = NULL, .cacheExtra = NULL, .functionName = NULL,
           outputObjects = NULL, # nolint
           algo = "xxhash64", cacheRepo = NULL,
           cachePath = NULL,
           length = getOption("reproducible.length", Inf),
           compareRasterFileLength, userTags = c(),
           omitArgs = NULL,
           classOptions = list(), debugCache = character(),
           # sideEffect = FALSE,
           makeCopy = FALSE,
           quick = getOption("reproducible.quick", FALSE),
           verbose = getOption("reproducible.verbose", 1), cacheId = NULL,
           useCache = getOption("reproducible.useCache", TRUE),
           useCloud = FALSE,
           cloudFolderID = NULL,
           showSimilar = getOption("reproducible.showSimilar", FALSE),
           drv = getDrv(getOption("reproducible.drv", NULL)),
           conn = getOption("reproducible.conn", NULL)) {

    if (is.null(cachePath)) {
      if (!is.null(cacheRepo)) {
        messageCache("The cacheRepo argument is being deprecated. Please use cachePath", verbose = verbose)
        cachePath <- cacheRepo
      }
    }

    userTagsOrig <- stats::na.omit(userTags) # keep to distinguish actual user supplied userTags

    CacheMatchedCall <- match.call(Cache)
    # Capture everything -- so not evaluated
    FUNcaptured <- substitute(FUN)
    dotsCaptured <- as.list(substitute(list(...))[-1])
    if (missing(FUNcaptured)) stop(.message$CacheRequiresFUNtxt())
    FUNbackup <- as.call(append(list(FUNcaptured), dotsCaptured))


    # returns "modifiedDots", "originalDots", "FUN", "funName", which will
    #  have modifications under many circumstances, e.g., do.call, specific methods etc.
    # Need the CacheMatchedCall so that args that are in both Cache and the FUN can be sent to both
    preCacheDigestTime <- Sys.time()
    fnDetails <- .fnCleanup(
      FUN = FUN, callingFun = "Cache", ..., .functionName = .functionName,
      FUNcaptured = FUNcaptured, CacheMatchedCall = CacheMatchedCall, omitArgs = omitArgs
    )
    # next line is (1 && 1) && 1 -- if it has :: or $ or [] e.g., fun$b, it MUST be length 3 for it to not be "captured function"
    isCapturedFUN <- isFALSE(isDollarSqBrPkgColon(FUNcaptured) &&
      length(FUNcaptured) == 3) &&
      length(dotsCaptured) == 0 && # no dots; likely not a captured function, unless it has no args
      (length(FUNcaptured) > 1) # Must have some args
    isSquiggly <- FALSE
    if (length(FUNcaptured) > 1) isSquiggly <- identical(as.name("{"), FUNcaptured[[1]])

    FUN <- fnDetails$FUN

    modifiedDots <- fnDetails$modifiedDots
    # originalDots <- fnDetails$originalDots
    skipCacheDueToNumeric <- is.numeric(useCache) && useCache <= (fnDetails$nestLevel)
    if (isFALSE(useCache) || isTRUE(0 == useCache) || skipCacheDueToNumeric) {
      .message$useCacheIsFALSE(nestLevel = fnDetails$nestLevel, functionName = fnDetails$functionName,
                               useCache, verbose)
      # nestedLev <- max(0, as.numeric(fnDetails$nestLevel)) ## nestedLev >= 0
      # spacing <- paste(collapse = "", rep("  ", nestedLev))
      # messageCache(spacing, "useCache is ", useCache,
      #   "; skipping Cache on function ", fnDetails$functionName,
      #   if (nestedLev > 0) paste0(" (currently running nested Cache level ", nestedLev + 1, ")"),
      #   verbose = verbose
      # )
      output <- evalTheFun(FUNcaptured, isCapturedFUN, FUNbackup,
        envir = parent.frame(),
        verbose, ...
      )
      # }
    } else {
      startCacheTime <- verboseTime(verbose, verboseLevel = 3)

      if (!missing(compareRasterFileLength)) {
        messageCache("compareRasterFileLength argument being deprecated. Use 'length'",
          verbose = verbose
        )
        length <- compareRasterFileLength
      }

      mced <- match.call(expand.dots = TRUE)
      nestedTags <- determineNestedTags(
        envir = environment(),
        mc = mced,
        userTags = userTags
      )
      userTags <- unique(c(userTags, .reproEnv$userTags))
      if (any(!nestedTags$objOverride)) {
        on.exit(
          {
            if (any(!nestedTags$prevVals)) {
              # THe suppressWarnings is about objects that aren't there -- so far only happens
              #  when interrupting a process, which means it is spurious
              suppressWarnings(rm(
                list = nestedTags$namesUserCacheArgs,
                envir = .reproEnv
              ))
              if (nestedTags$prevUserTags) {
                .reproEnv$userTags <- nestedTags$oldUserTags
              }
            }
            if (nestedTags$prevUserTags) {
              .reproEnv$userTags <- nestedTags$oldUserTags
            }
          },
          add = TRUE
        )
      }

      # get cachePath if not supplied
      cachePaths <- getCacheRepos(cachePath, modifiedDots, verbose = verbose)
      modifiedDots$.FUN <- fnDetails$.FUN # put in modifiedDots for digesting  # nolint
      scalls <- if (!is(FUN, "function")) try(.CacheFn1(FUN, sys.calls())) else NULL

      # extract other function names that are not the ones the focus of the Cache call
      otherFns <- .getOtherFnNamesAndTags(scalls = scalls)

      if (missing(notOlderThan)) notOlderThan <- NULL
      # if a simList is in ...
      # userTags added based on object class
      userTags <- c(userTags, unlist(lapply(modifiedDots, .tagsByClass)))

      if (isTRUE("sideEffect" %in% ...names())) {
        messageCache("sideEffect is deprecated; being ignored",
          verbose = verbose, verboseLevel = 0
        )
      }

      # Do the digesting
      if (!is.null(omitArgs)) {
        # recursive
        modifiedDots <- nullifyByArgName(modifiedDots, omitArgs)
        # modifiedDots[omitArgs] <- NULL
      }

      preDigestByClass <- lapply(
        seq_along(modifiedDots),
        function(x) {
          .preDigestByClass(modifiedDots[[x]])
        }
      )

      startHashTime <- verboseTime(verbose, verboseLevel = 3)

      # remove some of the arguments passed to Cache, which are irrelevant for digest
      argsToOmitForDigest <- names(modifiedDots) %in% .defaultCacheOmitArgs

      toDigest <- modifiedDots[!argsToOmitForDigest]
      if (!is.null(.cacheExtra)) {
        toDigest <- append(toDigest, list(.cacheExtra = .cacheExtra))
      }
      withCallingHandlers(
        cacheDigest <- CacheDigest(toDigest,
                                   .functionName = fnDetails$functionName,
                                   .objects = .objects,
                                   length = length, algo = algo, quick = quick,
                                   classOptions = classOptions, calledFrom = "Cache"
        ),
        error = function(e) {
          messageCache("Error occurred during Cache call of: ", .messageFunctionFn(fnDetails$functionName),
                       ". Call was:\n", paste0(head(format(FUNcaptured)), collapse = "\n"))
        })
      postCacheDigestTime <- Sys.time()
      elapsedTimeCacheDigest <- postCacheDigestTime - preCacheDigestTime

      preDigest <- cacheDigest$preDigest
      outputHash <- cacheDigest$outputHash

      # This does to depth 3
      preDigestUnlistTrunc <- unlist(
        .unlistToCharacter(preDigest, getOption("reproducible.showSimilarDepth", 3))
      )
      if (verbose > 3) {
        a <- verboseCacheMessage(preDigest, functionName = fnDetails$functionName,
          startHashTime, modifiedDots,
          quick = quick,
          verbose = verbose, verboseLevel = 3
        )
        on.exit(
          {
            assign("cacheTimings", .reproEnv$verboseTiming, envir = .reproEnv)
            messageDF(.reproEnv$verboseTiming, colour = "blue", verbose = verbose, verboseLevel = 3)
            messageCache("This object is also available from .reproEnv$cacheTimings",
              verbose = verbose, verboseLevel = 3
            )
            if (exists("verboseTiming", envir = .reproEnv)) {
              rm("verboseTiming", envir = .reproEnv)
            }
          },
          add = TRUE
        )
      }

      if (length(debugCache)) {
        if (!is.na(pmatch(debugCache, "quick"))) {
          return(list(hash = preDigest, content = list(...)))
        }
      }
      userConn <- !is.null(conn)

      conns <- checkConns(cachePaths, conn)
      # conns <- list()
      # if (!is.null(conn)) { # if the conn was passed by user
      #   if (!is.list(conn)) {
      #     conn <- list(conn)
      #   }
      #   if (!identical(length(cachePaths), length(conn))) {
      #     stop("conn and cachePath are both provided, but are different lengths which is not allowed")
      #   }
      #   names(conn) <- cachePaths
      #   conns <- conn
      # }
      for (cachePath in cachePaths) {
        # Need conn --> also need exclusive lock
        #if (useDBI()) {
          conns <- createConns(cachePath, conns, drv)
          # if (is.null(conns[[cachePath]])) {
          #   conns[[cachePath]] <- dbConnectAll(drv, cachePath = cachePath)
          #   RSQLite::dbClearResult(RSQLite::dbSendQuery(conns[[cachePath]], "PRAGMA busy_timeout=5000;"))
          #   RSQLite::dbClearResult(RSQLite::dbSendQuery(conns[[cachePath]], "PRAGMA journal_mode=WAL;"))
          # }
        #}

        # isIntactRepo <- CacheIsACache(
        #   cachePath = cachePath, drv = drv, create = TRUE,
        #   conn = conns[[cachePath]]
        # )
        # if (any(!isIntactRepo)) {
        #   ret <- createCache(cachePath,
        #     drv = drv, conn = conns[[cachePath]],
        #     force = isIntactRepo
        #   )
        # }

        # Need exclusive lock
        if (!useDBI()) {
          dtFile <- CacheDBFileSingle(cachePath = cachePath, cacheId = outputHash)
          lockFile <- file.path(CacheStorageDir(cachePath = cachePath), paste0(outputHash, suffixLockFile()))
          locked <- filelock::lock(lockFile)
          on.exit(
            {
              filelock::unlock(locked)
              if (file.exists(lockFile)) {
                unlink(lockFile)
              }
            },
            add = TRUE
          )
        }

        # Check if it is in repository
        inReposPoss <- searchInRepos(cachePath,
          outputHash = outputHash,
          drv = drv, conn = conns[[cachePath]]
        )
        if (cachePath == cachePaths[[1]] || NROW(inReposPoss$isInRepo)) {
          # keep important parts if it is first one, or if it has the object in the cacheRepo
          inRepos <- inReposPoss
          conn <- conns[[cachePath]]
          if (NROW(inReposPoss$isInRepo)) {
            break
          }
        }
      }
      on.exit(
        {
          if (useDBI()) {
            if (!isTRUE(userConn)) {
              done <- lapply(conns, function(co) {
                try(DBI::dbDisconnect(co), silent = TRUE)
              })
            }
          }
        },
        add = TRUE
      )

      # If user passes cacheId, including cacheId = "previous"
      if (!is.null(cacheId)) {
        sc <- cacheIdCheckInCache(cacheId, calculatedCacheId = outputHash, .functionName, verbose)
        outputHashPossible <- attr(sc, "cacheId")
        if (!is.null(outputHashPossible)) outputHash <- outputHashPossible
        if (NROW(sc) > 0) {
          # outputHash <- unique(sc$cacheId) # cacheId
          inRepos$fullCacheTableForObj <- sc
          inRepos$isInRepo <- sc[1, ]
          # outputHash <- sc[["cacheId"]][1]
        }
      }

      # if (!is.null(cacheId)) {
      #   if  (identical(cacheId, "previous")) {
      #     sc <- showCache(fun = .functionName, verbose = -2)
      #     if (NROW(sc)) {
      #       messageCache("cacheId is 'previous' meaning it will recover the most recent ",
      #                    "cache item (accessed) that matches on .functionName: ",
      #                    .messageFunctionFn(.functionName), "\nPlease ensure ",
      #                    "the function name is precise enough for this behaviour", verbose = verbose)
      #       outputHashNew <- data.table::setorderv(sc[tagKey == "accessed"], "tagValue", order = -1L)
      #       outputHash <- outputHashNew$cacheId[1]
      #       inRepos$isInRepo <- outputHashNew[1, ]
      #       inRepos$fullCacheTableForObj <- showCacheFast(cacheId = outputHash)
      #     }
      #   } else {
      #     outputHashManual <- cacheId
      #     if (identical(outputHashManual, outputHash)) {
      #       messageCache("cacheId is same as calculated hash",
      #                    verbose = verbose
      #       )
      #     } else {
      #       messageCache(.message$cacheIdNotSameTxt(cacheId), verbose = verbose)
      #       sc <- showCacheFast(cacheId = outputHashManual)
      #       if (NROW(sc))
      #         inRepos$isInRepo <- sc[1,]
      #     }
      #     outputHash <- outputHashManual
      #   }
      #
      # }

      isInRepo <- inRepos$isInRepo
      # dbTabNam <- inRepos$dbTabName
      fullCacheTableForObj <- inRepos$fullCacheTableForObj
      cachePath <- inRepos$cachePath # i.e., if there was > 1, then we now know which one

      # compare outputHash to existing Cache record
      if (useCloud) {
        .requireNamespace("googledrive",
          stopOnFALSE = TRUE,
          messageStart = "to use google drive files"
        )
        # Here, test that cloudFolderID exists and get obj details that matches outputHash, if present
        #  returns NROW 0 gdriveLs if not present
        if (is.null(cloudFolderID)) {
          cloudFolderID <- cloudFolderFromCacheRepo(cachePath)
        }
        if (is.character(cloudFolderID)) {
          cloudFolderID <- checkAndMakeCloudFolderID(cloudFolderID, cachePath = cachePath,
            create = TRUE, overwrite = FALSE
          )
        }
        gdriveLs <- retry(quote(driveLs(cloudFolderID,
          pattern = outputHash,
          verbose = verbose
        )))
      }

      userTags <- c(userTags, if (!is.na(fnDetails$functionName)) {
        paste0("function:", fnDetails$functionName)
      })

      outputHashNew <- outputHash # Keep a copy of this because it may be replaced next, but we need to know old one

      # First, if this is not matched by outputHash, test that it is matched by
      #   userTags and in devMode
      needFindByTags <- identical("devMode", useCache) && NROW(isInRepo) == 0
      if (needFindByTags) {
        # It will not have the "localTags" object because of "direct db access" added Jan 20 2020
        if (!exists("localTags", inherits = FALSE)) { #
          localTags <- showCache(cachePath, drv = drv, verbose = FALSE)
        } # This is noisy
        devModeOut <- devModeFn1(
          localTags, userTags, userTagsOrig, scalls,
          preDigestUnlistTrunc, useCache, verbose, isInRepo, outputHash
        )
        outputHash <- devModeOut$outputHash
        isInRepo <- devModeOut$isInRepo
        needFindByTags <- devModeOut$needFindByTags
      }

      # Deal with overwrite, needFindByTags (which is related to "devMode")
      isInCloud <- FALSE
      if (useCloud) {
        isInCloud <- any(grepl(outputHash, gdriveLs$name))
      }

      if (identical("overwrite", useCache) && (NROW(isInRepo) > 0 || isInCloud) || needFindByTags) {
        suppressMessages(clearCache(
          x = cachePath, userTags = outputHash, ask = FALSE,
          useCloud = ifelse(isTRUEorForce(useCloud), "force", FALSE),
          drv = drv, conn = conn,
          cloudFolderID = cloudFolderID
        ))
        if (identical("devMode", useCache)) {
          userTagsSimple <- gsub(".*:(.*)", "\\1", userTags)
          isInRepo <- isInRepo[!isInRepo[[.cacheTableTagColName()]] %in% userTagsSimple, , drop = FALSE]
          outputHash <- outputHashNew
          .message$overwriting(userTagsSimple, type = c("userTags"), verbose)
          # messageCache("Overwriting Cache entry with userTags: '", paste(userTagsSimple, collapse = ", "), "'",
          #   verbose = verbose
          # )
        } else {
          # remove entries from the 2 data.frames of isInRep & gdriveLs
          if (useCloud) {
            gdriveLs <- gdriveLs[!gdriveLs$name %in% basename2(CacheStoredFile(cachePath, outputHash)), ]
          }
          isInRepo <- isInRepo[isInRepo[[.cacheTableHashColName()]] != outputHash, , drop = FALSE]
          .message$overwriting(fnDetails$functionName, type = c("function"), verbose)
          # messageCache("Overwriting Cache entry with function '", fnDetails$functionName, "'",
          #   verbose = verbose
          # )
        }
      }

      # It is in the cloud, but not local
      if (useCloud) {
        if (isInCloud && NROW(isInRepo) == 0) {
          # Here, download cloud copy to local folder, skip the running of FUN
          newFileName <- gdriveLs$name[isInCloud] # paste0(outputHash,".rda")
          inReposPoss <- cloudDownload(outputHash, newFileName, gdriveLs, cachePath, cloudFolderID,
            drv = drv, conn = conn, verbose = verbose
          )
          isInRepo <- inReposPoss$isInRepo
          fullCacheTableForObj <- inReposPoss$fullCacheTableForObj
          if (is.null(isInRepo)) {
            retry(quote(googledrive::drive_rm(gdriveLs[isInCloud, ])))
            isInCloud[isInCloud] <- FALSE
          } else {
            .CacheIsNew <- FALSE
          }
        }
      }

      # If it is in the existing record:
      if (NROW(isInRepo) > 0) {
        # make sure the notOlderThan is valid, if not, exit this loop
        lastEntry <- # as.POSIXct(
          max(isInRepo$createdDate) # ) # + 1 # This is necessary for very fast functions; basically, allow at least 1 second before refreshing
        lastOne <- order(isInRepo$createdDate, decreasing = TRUE)[1]
        if (is.null(notOlderThan) || (notOlderThan <= lastEntry)) {
          out <- returnObjFromRepo(
            isInRepo = isInRepo, notOlderThan = notOlderThan,
            fullCacheTableForObj = fullCacheTableForObj, cachePath = cachePath,
            verbose = verbose, FUN = FUN, fnDetails = fnDetails, modifiedDots = modifiedDots,
            debugCache = debugCache, # sideEffect = sideEffect,
            quick = quick, algo = algo, preDigest = preDigest,
            startCacheTime = startCacheTime, drv = drv, conn = conn,
            outputHash = outputHash, useCloud = useCloud, gdriveLs = gdriveLs,
            cloudFolderID = cloudFolderID,
            lastEntry = lastEntry, lastOne = lastOne, ...
          )
          if (!is.null(out))
            out <- addCacheAttr(out, .CacheIsNew = FALSE, outputHash, FUN)
          if (!is(out, "try-error"))
            return(out)
        }
      } # else {
      # find similar
      if (!is.null(showSimilar)) { # TODO: Needs testing
        if (!isFALSE(showSimilar)) {
          if (!exists("localTags", inherits = FALSE)) { #
            localTags <- showCache(cachePath, drv = drv, verbose = FALSE)
          } # This is noisy
          .findSimilar(localTags, showSimilar, scalls, preDigestUnlistTrunc,
                       userTags, userTagsOrig,
                       functionName = fnDetails$functionName,
                       useCache = useCache, verbose = verbose
          )
        }
      }
      # }

      startRunTime <- verboseTime(verbose, verboseLevel = 3)

      .CacheIsNew <- TRUE

      # check that it didn't come from cloud or failed to find complete cloud (i.e., output is NULL)
      elapsedTimeFUN <- NA
      if (!exists("output", inherits = FALSE) || is.null(output)) {
        # Run the FUN
        preRunFUNTime <- Sys.time()
        output <- evalTheFun(FUNcaptured, isCapturedFUN, FUNbackup,
                             envir = parent.frame(),
                             verbose, ...
        )
        postRunFUNTime <- Sys.time()
        elapsedTimeFUN <- postRunFUNTime - preRunFUNTime
      }

      output <- .addChangedAttr(output, preDigest,
                                origArguments = modifiedDots,
                                .objects = outputObjects, length = length,
                                algo = algo, quick = quick, classOptions = classOptions, ...
      )
      verboseDF1(verbose, fnDetails$functionName, startRunTime)

      # Delete previous version if notOlderThan violated --
      #   but do this AFTER new run on previous line, in case function call
      #   makes it crash, or user interrupts long function call and wants
      #   a previous version
      if (NROW(isInRepo) > 0) {
        # flush it if notOlderThan is violated
        if (isTRUE(notOlderThan >= lastEntry)) {
          suppressMessages(clearCache(
            userTags = isInRepo[[.cacheTableHashColName()]][lastOne],
            x = cachePath,
            ask = FALSE, useCloud = useCloud, drv = drv, conn = conn,
            cloudFolderID = cloudFolderID
          ))
        }
      }

      # need something to attach tags to if it is actually NULL
      isNullOutput <- if (is.null(output)) TRUE else FALSE
      if (isNullOutput) {
        output <- "NULL"
      }

      output <- addCacheAttr(output, .CacheIsNew, outputHash, FUN)

      # Can make new methods by class to add tags to outputs
      if (.CacheIsNew) {
        outputToSave <- .wrap(output, cachePath, preDigest = preDigest,
                              outputObjects = outputObjects,
                              drv = drv, conn = conn, cacheId = outputHash, verbose = verbose)
        if (isTRUE(is.character(outputToSave)) && isTRUE(!is.character(output)))
          outputToSave <- asPath(outputToSave)
        output <- .CopyCacheAtts(outputToSave, output)
        # .wrap added tags; these should be transfered to output
        #          outputToSave <- .addTagsToOutput(outputToSave, outputObjects, FUN, preDigestByClass)
        #          output <- .addTagsToOutput(outputToSave, outputObjects, FUN, preDigestByClass)
      }

      # Remove from otherFunctions if it is "function"
      alreadyIn <- gsub(otherFns, pattern = "otherFunctions:", replacement = "") %in%
        as.character(attr(output, "function"))
      if (isTRUE(any(alreadyIn))) {
        otherFns <- otherFns[!alreadyIn]
      }


      if (length(debugCache) && .CacheIsNew) {
        if (!is.na(pmatch(debugCache, "complete"))) {
          output <- .debugCache(output, preDigest, ...)
          outputToSave <- .debugCache(outputToSave, preDigest, ...)
        }
      }

      startSaveTime <- verboseTime(verbose, verboseLevel = 3)
      # This is for write conflicts to the SQLite database
      #   (i.e., keep trying until it is written)

      objSize <- if (getOption("reproducible.objSize", TRUE)) sum(objSize(outputToSave)) else NA

      resultHash <- ""
      linkToCacheId <- NULL
      if (isTRUE(objSize > 1e6)) {
        resultHash <- CacheDigest(outputToSave,
                                  .objects = .objects,
                                  length = length, algo = algo, quick = quick,
                                  classOptions = classOptions, calledFrom = "Cache"
        )$outputHash
        allCache <- showCache(cachePath, verbose = -2)
        if (NROW(allCache)) {
          alreadyExists <- allCache[allCache$tagKey == "resultHash" & allCache$tagValue %in% resultHash]
          if (NROW(alreadyExists)) {
            linkToCacheId <- alreadyExists[["cacheId"]][[1]]
          }
        }
      }

      fns <- Filenames(outputToSave)
      userTags <- c(
        userTags,
        paste0("class:", class(outputToSave)[1]),
        paste0("object.size:", format(as.numeric(objSize))),
        paste0("accessed:", Sys.time()),
        paste0("inCloud:", isTRUE(useCloud)),
        paste0("fromDisk:", isTRUE(any(nchar(fns) > 0))),
        paste0("resultHash:", resultHash),
        paste0("elapsedTimeDigest:", format(elapsedTimeCacheDigest, units = "secs")),
        paste0("elapsedTimeFirstRun:", format(elapsedTimeFUN, units = "secs")),
        paste0(otherFns),
        attr(outputToSave, "tags"),
        # grep("cacheId", attr(outputToSave, "tags"), invert = TRUE, value = TRUE),
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
        if (exists("futureEnv", envir = .reproEnv)) {
          .reproEnv$futureEnv <- new.env(parent = emptyenv())
        }

        if (isTRUE(getOption("reproducible.futurePlan"))) {
          messageCache('options("reproducible.futurePlan") is TRUE. Setting it to "multisession".\n',
                       "Please specify a plan by name, e.g.,\n",
                       '  options("reproducible.futurePlan" = "multisession")',
                       verbose = verbose
          )
          future::plan("multisession", workers = 1)
        } else {
          if (!is(future::plan(), getOption("reproducible.futurePlan"))) {
            thePlan <- getOption("reproducible.futurePlan")
            future::plan(thePlan, workers = 1)
          }
        }
        .reproEnv$futureEnv[[paste0("future_", rndstr(1, 10))]] <-
          future::futureCall(
            FUN = writeFuture,
            args = list(written, outputToSave, cachePath, userTags, drv, conn,
                        cacheId = outputHash, linkToCacheId
            ),
            globals = list(
              written = written,
              outputToSave = outputToSave,
              cachePath = cachePath,
              userTags = userTags,
              drv = drv,
              conn = conn,
              cacheId = outputHash,
              linkToCacheId = linkToCacheId
            )
          )
        if (is.null(.reproEnv$alreadyMsgFuture)) {
          messageCache("Cache saved in a separate 'future' process. ",
                       "Set options('reproducible.futurePlan' = FALSE), if there is strange behaviour.",
                       "This message will not be shown again until next reload of reproducible",
                       verbose = verbose
          )
          .reproEnv$alreadyMsgFuture <- TRUE
        }
      } else {
        otsObjSize <- objectSizeGetFromUserTags(userTags)
        # otsObjSize <- gsub(grep("object\\.size:", userTags, value = TRUE),
        #                    pattern = "object.size:", replacement = ""
        # )
        # otsObjSize <- if (identical(unname(otsObjSize), "NA")) NA else as.numeric(otsObjSize)
        isBig <- isTRUE(otsObjSize > .objectSizeMinForBig)
        osMess <- .cacheMessageObjectSize(otsObjSize, isBig)
        # if (!anyNA(otsObjSize)) {
        #   class(otsObjSize) <- "object_size"
        #   osMess <- format(otsObjSize, units = "auto")[isBig]
        # } else {
        #   osMess <- ""
        # }

        m <- .message$SavingToCacheTxt(isBig, userTags, fnDetails$functionName, cacheId, otsObjSize, osMess)

        outputToSave <- progressBarCode(
          saveToCache(
            cachePath = cachePath, drv = drv, userTags = userTags,
            conn = conn, obj = outputToSave, cacheId = outputHash,
            linkToCacheId = linkToCacheId
          ),
          doProgress = isBig,
          message = m, # messageCache(m, verbose = verbose),
          # message = c(
          #   "Saving ", "large "[isBig], "object (fn: ", .messageFunctionFn(fnDetails$functionName),
          #   ", cacheId: ", outputHash, ") to Cache", ": "[isBig],
          #   osMess
          # ),
          verboseLevel = 2 - isBig, verbose = verbose,
          colour = getOption("reproducible.messageColourCache")
        )
        # .message$IndentRevert() # revert the indent of 2 spaces
        .message$Saved(cachePath, outputHash, functionName = fnDetails$functionName, verbose)
        # messageCache("Saved! Cache file: ",
        #              basename2(CacheStoredFile(cachePath = cachePath, cacheId = outputHash)),
        #              "; fn: ", .messageFunctionFn(fnDetails$functionName),
        #              verbose = verbose)
      }

      if (useCloud && .CacheIsNew) {
        # Here, upload local copy to cloud folder if it isn't already there
        cufc <- try(cloudUploadFromCache(isInCloud, outputHash, cachePath, cloudFolderID, ## TODO: saved not found
                                         outputToSave,
                                         verbose = verbose
        )) # , rasters))
        if (is(cufc, "try-error")) {
          .updateTagsRepo(outputHash, cachePath, "inCloud", "FALSE", drv = drv, conn = conn)
        }
      }

      verboseDF2(verbose, fnDetails$functionName, startSaveTime)

      verboseDF3(verbose, fnDetails$functionName, startCacheTime)

      if (isNullOutput) {
        return(NULL)
      }
    }
    return(output)
  }

#' @keywords internal
.formalsCache <- formals(Cache)[-(1:2)]

#' @keywords internal
.formalscache2 <- formals(cache2)[-(1:2)]

#' @keywords internal
.formalsCache[c("compareRasterFileLength", "digestPathContent")] <- NULL

#' @keywords internal
.namesCacheFormals <- names(.formalsCache)[]

#' @keywords internal
.namescache2Formals <- names(.formalscache2)[]

#' @keywords internal
.namesPostProcessFormals <- function() {
  c(
    "x", "filename1", "writeTo", "studyArea", "rasterToMatch",
    "overwrite", "useSAcrs", "useCache", "verbose"
  )
}


#' @keywords internal
.namesCacheFormalsSendToBoth <- intersect("verbose", names(.formalsCache)[])


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



#' Write to cache repository, using `future::future`
#'
#' This will be used internally if `options("reproducible.futurePlan" = TRUE)`.
#' This is still experimental.
#'
#' @param written Integer. If zero or positive then it needs to be written still.
#'                Should be 0 to start.
#' @param outputToSave The R object to save to repository
#' @param cachePath The file path of the repository
#' @param userTags Character string of tags to attach to this `outputToSave` in
#'                 the `CacheRepo`
#'
#' @export
#' @inheritParams Cache
#' @inheritParams saveToCache
#' @return
#' Run for its side effect.
#' This will add the `objectToSave` to the cache located at `cachePath`,
#' using `cacheId` as its id, while
#' updating the database entry. It will do this using the future package, so it is
#' written in a future.
writeFuture <- function(written, outputToSave, cachePath, userTags,
                        drv = getDrv(getOption("reproducible.drv", NULL)),
                        conn = getOption("reproducible.conn", NULL),
                        cacheId, linkToCacheId = NULL) {
  counter <- 0
  if (!CacheIsACache(cachePath = cachePath, drv = drv, conn = conn)) {
    stop("That cachePath does not exist")
  }

  if (missing(cacheId)) {
    cacheId <- .robustDigest(outputToSave)
  }
  output <- saveToCache(
    cachePath = cachePath, drv = drv, userTags = userTags,
    conn = conn, obj = outputToSave, cacheId = cacheId,
    linkToCacheId = linkToCacheId
  )
  saved <- cacheId

  return(saved)
}


findFun <- function(FUNcaptured, envir) {
  if (is.call(FUNcaptured[[1]])) {
    out <- findFun(FUNcaptured[[1]], envir = envir)
  } else {
    out <- eval(FUNcaptured[[1]], envir = envir)
  }
  out
}

isDollarSqBrPkgColon <- function(args) {
  ret <- FALSE
  if (length(args) == 3) { # i.e., only possible if it is just b$fun or stats::runif, not stats::runif(1) or b$fun(1)
    # ret <- isDollarOnlySqBr(args) | isPkgColon(args)
    ret <- isTRUE(any(try(grepl("^\\$|\\[|\\:\\:", args)[1], silent = TRUE)))
  }
  ret
}

isPkgColon <- function(args) {
  ret <- FALSE
  if (length(args) == 3) { # i.e., only possible if it is just b$fun or stats::runif, not stats::runif(1) or b$fun(1)
    ret <- isTRUE(any(try(grepl("\\:\\:", args)[1], silent = TRUE)))
  }
  ret
}

isDollarOnlySqBr <- function(args) {
  ret <- FALSE
  if (length(args) == 3) { # i.e., only possible if it is just b$fun or stats::runif, not stats::runif(1) or b$fun(1)
    ret <- isTRUE(any(try(grepl("^\\$|\\[", args)[1], silent = TRUE)))
  }
  ret
}

recursiveEvalNamesOnly <- function(args, envir = parent.frame(), outer = TRUE, recursive = TRUE) {

  needsEvaling <- (length(args) > 1) || (length(args) == 1 && is.call(args)) # second case is fun() i.e., no args
  if (isTRUE(needsEvaling)) {
    if (is.call(args[[1]])) { # e.g., a$fun, stats::runif
      args[[1]] <- eval(args[[1]], envir)
    }

    isStandAlone <- FALSE
    if (length(args) == 3) { # e.g., status::runif or fun(1, 2); second case requires subsequent efforts
      if (!is.function(args[[1]])) { # this removes fun(1, 2) case
        isStandAlone <- isDollarSqBrPkgColon(args[[1]])
      }
    } else if (length(args[[1]]) == 3) {
      isStandAlone <- isDollarSqBrPkgColon(args[[1]])
    }

    if (identical(as.name("<-"), args[[1]])) {
      args <- as.list(args[-(1:2)])[[1]]
    }

    if (identical(quote(parse), args[[1]])) {
      args <- eval(args)
    }

    if (!isTRUE(recursive)) {
      isStandAlone <- TRUE
    }

    if (!any(isStandAlone)) {
      out <- lapply(args, function(xxxx) {
        if (is.name(xxxx)) {
          # exists(xxxx, envir = envir, inherits = FALSE)
          if (exists(xxxx, envir)) { # looks like variables that are in ... in the `envir` are not found; would need whereInStack
            evd <- try(eval(xxxx, envir), silent = TRUE)
            isPrim <- is.primitive(evd)
            if (isPrim) {
              eval(xxxx)
            } else {
              isQuo <- is(evd, "quosure")
              if (isQuo) {
                evd <- rlang::eval_tidy(evd)
              }
              if (is(evd, "list")) {
                evd <- recursiveEvalNamesOnly(evd, envir, outer = FALSE)
              }
              evd
            }
          } else {
            ret <- xxxx
            ret
          }
        } else {
          if (is.call(xxxx)) {
            if (identical(quote(eval), xxxx[[1]])) { # basically "eval" should be evaluated
              message(
                "There is an `eval` call in a chain of calls for Cache; ",
                "\n  eval is evaluated before Cache which may be undesired. ",
                "\n  Perhaps use `do.call` if the evaluation should not occur prior to Cache"
              )
              ret <- eval(xxxx, envir = envir)
            } else {
              ret <- recursiveEvalNamesOnly(xxxx, envir, outer = FALSE)
            }
          } else {
            ret <- xxxx
          }
          ret
        }
      })



      args <- as.call(out)
      # args <- if (isTRUE(outer)) try(as.call(out)) else out
      if (is.function(args[[1]])) {
        args <- match_call_primitive(args[[1]], args, expand.dots = FALSE, envir = envir)
        args[[1]] <- getMethodAll(args, envir)
      }
    } else {
      args <- eval(args, envir)
    }
  } else {
    if (length(args) == 1 && is.name(args)) {
      args <- eval(args, envir)
    }
  }
  args
}


matchCall <- function(FUNcaptured, envir = parent.frame(), fnName) {
  if (length(FUNcaptured) > 1) {
    FUN <- FUNcaptured[[1]]
    args <- as.list(FUNcaptured[-1])
    if (is.call(FUN)) FUN <- eval(FUN, envir)
    if (is.function(FUN)) {
      forms <- if (is.primitive(FUN)) formals(args(FUN)) else formals(FUN)
      if (length(forms) == 0) {
        mc <- list(FUN)
      } else {
        if (is.primitive(FUN)) {
          # Must test for "non-normal non-positional matching", like round and round.POSIXt, ... see ?match.call
          #  can't tell a priori that a primitive doesn't have methods, so must test first.
          #  These will always be in base, so can just get0 directly, which is WAY faster than any other way
          nonPrimMeth <- NULL
          if (!is.null(fnName)) {
            cls <- is(args[[1]])
            # use for loop, so can break out if a method is found quickly
            for (classHere in cls) {
              nonPrimMeth <- get0(paste0(fnName, ".", classHere))
              if (!is.null(nonPrimMeth)) break
            }
          }
          if (length(nonPrimMeth)) {
            args2 <- formals(nonPrimMeth)
          } else {
            args2 <- forms
          }
          args2[seq(args)] <- args
          args2 <- args2[seq_along(args)] # chop off any trailing args
          mc <- append(list(FUN), args2)
        } else {
          mc <- match.call(FUN, FUNcaptured)
        }
      }
    } else {
      mc <- FUNcaptured
    }
  } else {
    mc <- list(FUNcaptured)
  }
  mc
}

#' @importFrom methods .S4methods
#' @importFrom utils getFromNamespace
getMethodAll <- function(FUNcaptured, callingEnv) {
  FUN <- FUNcaptured[[1]]
  if (!is.function(FUN))
    FUN <- tryCatch(eval(FUN, envir = callingEnv),
                    error = function(FU) eval(parse(text = FUN), envir = callingEnv))
  if (isS4(FUN)) {
    functionName <- FUN@generic
    # Not easy to selectMethod -- can't have trailing "ANY" -- see ?selectMethod last
    #  paragraph of "Using findMethod()" which says:
    # "Notice also that the length of the signature must be what the corresponding
    #  package used. If thisPkg had only methods for one argument, only length-1
    # signatures will match (no trailing "ANY"), even if another currently loaded
    # package had signatures with more arguments.
    numArgsInSig <- try(
      {
        suppressWarnings({
          info <- attr(methods::.S4methods(functionName), "info") # from hadley/sloop package s3_method_generic
          # info <- attr(utils::methods(functionName), "info")# from hadley/sloop package s3_method_generic
        })
        max(unlist(lapply(strsplit(rownames(info), split = ","), length)) - 1)
      },
      silent = TRUE
    )
    matchOn <- FUN@signature[seq(numArgsInSig)]

    argsClassesList <- lapply(FUNcaptured, function(x) class(x))
    # argsClasses <- unlist(argsClassesList)#[1]))
    argsClasses <- unlist(unname(argsClassesList[names(argsClassesList) %in% matchOn]))
    missingArgs <- matchOn[!(matchOn %in% names(argsClassesList))]

    missings <- rep("missing", length(missingArgs))
    names(missings) <- missingArgs
    argsClasses <- c(argsClasses, missings)

    argClassesAreCall <- argsClasses %in% "call" # maybe wasn't evaluated enough to know what it is; force eval
    if (any(argClassesAreCall)) {
      argsClasses <- "ANY"
      #whAreCall <- names(argsClasses[argClassesAreCall])
      #argsClasses <- Map(wac = whAreCall, function(wac) is(eval(FUNcaptured[[wac]], envir = callingEnv)))
    } else {
      FUN <- selectMethod(functionName, signature = argsClasses)
    }
    updatedFUN <- TRUE
  } else {
    isS3 <- isS3stdGeneric(FUN)
    if (!is.null(names(isS3))) {
      fnNameInitAlt <- names(isS3)
    }
    if (isS3) {
      updatedFUN <- TRUE
      classes <- is(FUNcaptured[[2]])
      for (cla in classes) {
        FUNposs <- utils::getS3method(fnNameInitAlt, cla, optional = TRUE) # S3 matches on 1st arg: FUNcaptured[[2]]
        if (!is.null(FUNposs)) {
          break
        }
      }

      # if generic fn was not exported, then getS3method won't find it above; try directly in NS
      if (is.null(FUNposs)) {
        envNam <- environmentName(environment(FUN))
        FUNpossGen <- get0(fnNameInitAlt, envir = asNamespace(envNam))
        for (cla in classes) {
          possMeth <- paste0(fnNameInitAlt, ".", cla)
          FUNposs <- try(getFromNamespace(possMeth, ns = envNam), silent = TRUE)
          if (!is(FUNposs, "try-error")) {
            break
          } else {
            FUNposs <- NULL
          }
        }
        if (is.null(FUNposs)) {
          FUNposs <- FUNpossGen
        }
      }

      if (is.null(FUNposs)) {
        FUNposs <- get0(fnNameInitAlt, envir = callingEnv)
        if (is.null(FUNposs) || isS4(FUNposs)) { # there are cases e.g., print that are both S4 & S3; this forces S3
          FUNposs <- get0(paste0(fnNameInitAlt, ".default"), envir = callingEnv)
        }
      }
      FUN <- FUNposs
    }
  }
  FUN
}

formals2 <- function(FUNcaptured) {
  modifiedDots <- as.list(FUNcaptured[-1])
  FUN <- FUNcaptured[[1]]
  modifiedDots <- formals3(FUN, modifiedDots, removeNulls = TRUE)
  modifiedDots
}


formals3 <- function(FUN, modifiedDots = list(), removeNulls = FALSE) {
  forms1 <- formals(FUN) # primitives don't have formals
  if (!is.null(forms1)) {
    forms1 <- forms1[setdiff(names(forms1), "...")]
    if (NROW(forms1)) {
      defaults <- setdiff(names(forms1), names(modifiedDots))
      if (removeNulls) {
        theNulls <- unlist(lapply(forms1[defaults], is.null))
        if (any(theNulls)) {
          defaults <- defaults[!theNulls]
        }
      }

      if (NROW(defaults)) { # there may be some arguments that are not specified

        # get the values of args that are eg. coming from options
        forms1[defaults] <- lapply(forms1[defaults], function(xxx) {
          yyy <- "default"
          if (length(xxx) > 0) {
            if (length(xxx) == 1) {
              if (isTRUE(nchar(xxx) == 0)) {
                yyy <- NULL
              }
            }
          }
          if (!is.null(yyy)) {
            # Some are used by other args, yet are undefined in the args ... because "missing"
            # ex is seq.default() # by is (from - to)/(length.out - 1), but length.out is NULL in args
            # so need try
            yyy <- try(eval(xxx, envir = modifiedDots), silent = TRUE)
            if (is(yyy, "try-error")) {
              yyy <- NULL
            }
          }
          yyy
        })
      }

      # Have to get rid of NULL because CacheDigest
      if (removeNulls) {
        forms1 <- forms1[!unlist(lapply(forms1, is.null))]
      }
      modifiedDots <- modifyList(forms1, modifiedDots)
      forms <- names(forms1)
    }
  }
  modifiedDots
}


getFunctionName2 <- function(mc) {
  if (length(mc) > 1) {
    if (identical(as.name("<-"), mc[[1]])) {
      mc <- mc[-(1:2)]
    }
    coloncolon <- .grepSysCalls(list(mc), "^\\$|\\[|\\:\\:")
    if (length(coloncolon)) { # stats::runif -- has to be first one, not some argument in middle
      if (length(coloncolon) && length(mc) != 3) { # stats::runif

    #if (any(grepl("^\\$|\\[|\\:\\:", mc)[1])) { # stats::runif -- has to be first one, not some argument in middle
    #  if (any(grepl("^\\$|\\[|\\:\\:", mc[[1]])) && length(mc) != 3) { # stats::runif
        fnNameInit <- deparse(mc[[1]])
      } else {
        fnNameInit <- deparse(mc)
      }
    } else {
      fnNameInit <- deparse(as.list(mc[[1]])[[1]]) # fun() and fun could both be here in first slot
    }
  } else {
    fnNameInit <- deparse(mc)
  }
  fnNameInit
}

#' @importFrom utils modifyList isS3stdGeneric methods
.fnCleanup <- function(FUN, ..., callingFun, FUNcaptured = NULL, CacheMatchedCall,
                       .functionName = NULL, callingEnv = parent.frame(2), .fnCleanup,
                       omitArgs = "") {
  if (is.null(FUNcaptured)) {
    FUNcaptured <- substitute(FUN)
  }

  FUNcapturedOrig <- FUNcaptured

  whCharName <- is.function(FUNcaptured) # this is bad; it means that it was not captured. Happens when user
  #  erroneously does do.call(Cache, args)
  if (all(whCharName %in% TRUE)) {
    stop(
      "It looks like Cache is called incorrectly, possibly something like do.call(Cache, args); \n",
      "Cache should be the outermost function. See examples for correct ways to use Cache"
    )
  }
  # Remove `quote`
  isQuoted <- any(grepl("^quote", FUNcaptured)[1]) # won't work for complicated quote
  if (isQuoted) {
    FUNcaptured <- FUNcaptured[[2]]
  }

  dotsCaptured <- substitute(list(...))
  dotsCaptured <- as.list(dotsCaptured[-1]) # need to remove the `list` on the inside of the substitute

  # Backward compatibility; has no effect now
  userTagsOtherFunctions <- NULL

  if (isDollarSqBrPkgColon(FUNcaptured)) {
    if (isPkgColonFn(FUNcaptured)) {
      FUNcaptured <- eval(FUNcaptured, envir = callingEnv)
    } else if (isPkgColon(FUNcaptured)) { # this is TRUE ONLY if it is *just* b$fun or stats::runif, i.e., not b$fun(1)
      FUNcaptured[[1]] <- eval(FUNcaptured[[1]], envir = callingEnv)
    } else if (isDollarOnlySqBr(FUNcaptured)) {
      FUNcaptured <- eval(FUNcaptured, envir = callingEnv)
    }
  }

  if (length(FUNcaptured) > 1) { # this will cover the cases where previous misses, e.g.,
    if (isDollarSqBrPkgColon(FUNcaptured[[1]])) { # this is TRUE ONLY if it is *just* b$fun(1), stats::runif(1)
      FUNcaptured[[1]] <- eval(FUNcaptured[[1]], envir = callingEnv)
    }
  }

  if (!is.call(FUNcaptured)) { # isDollarSqBrPkgColon(FUNcaptured)) { # turn the rnorm, 1, 2 into rnorm(1, 2)
    FUNcaptured <- as.call(append(list(FUNcaptured), dotsCaptured))
  }

  whCharName <- unlist(lapply(FUNcaptured, function(x) is.call(x) || is.name(x) || is.function(x) || is.character(x)))
  isDoCall <- if (any(whCharName)) {
    isTRUE(grepl("^do\\.call", FUNcaptured[whCharName])[[1]]) ||
      identical(do.call, FUNcaptured[[1]])
  } else {
    FALSE
  }
  needRmList <- FALSE
  fnNameInit <- NULL
  if (isDoCall) {
    mc <- match.call(do.call, FUNcaptured)
    fnNameInit <- deparse(mc$what)
    if (length(mc$args) > 1) {
      argsForWhat <- mc$args[-1]
    } else {
      needRmList <- TRUE
      argsForWhat <- mc$args # mc$args will be a list; needs to be evaluated to be unlisted; do below
    }
    FUNcaptured <- try(as.call(append(list(mc$what), as.list(argsForWhat))))
  }

  isSquiggly <- FALSE
  if (!is.function(FUNcaptured[[1]])) { # e.g., just the name, such as rnorm --> convert to the actual function code
    if (is(FUNcaptured[[1]], "(")) {
      fnNameInit <- "headless"
    }
    FUNcaptured[[1]] <- eval(FUNcaptured[[1]], envir = callingEnv)
  }

  if (length(FUNcaptured) > 1) isSquiggly <- identical(`{`, FUNcaptured[[1]])

  if (isSquiggly) {
    # Get rid of squiggly
    FUNcaptured <- as.list(FUNcaptured[-1]) # [[1]] ... if it has many calls... pipe will be just one; but others will be more
    if (length(FUNcaptured) > 1) {
      stop("Cache can only handle curly braces if all internal code uses base pipe |>; see examples")
    }
    FUNcaptured <- FUNcaptured[[1]]
    FUNcapturedNamesEvaled <- recursiveEvalNamesOnly(FUNcaptured, envir = callingEnv) # deals with e.g., stats::rnorm, b$fun, b[[fun]]
    mc1 <- matchCall(FUNcaptured, envir = callingEnv, fnName = fnNameInit)
    if (is.null(fnNameInit)) {
      fnNameInit <- getFunctionName2(mc1[[1]])
    }
    FUNcapturedNamesEvaled <- matchCall(FUNcapturedNamesEvaled, envir = callingEnv, fnName = fnNameInit)
  } else {
    if (is.null(fnNameInit)) {
      fnNameInit <- getFunctionName2(FUNcapturedOrig)
    }
    if (length(FUNcaptured) > 1) {
      # The next line works for any object that is NOT in a ..., because the
      #   object never shows up in the environment; it is passed through
      # mced <- names(CacheMatchedCall)

      # if (!is.null(unlist(argsToKeep))) {
      FUNcapturedList <- as.list(FUNcaptured[-1])
      nams <- names(FUNcapturedList)
      if (is.null(nams))
        nams <- sapply(seq_along(FUNcapturedList), function(x) paste0(sample(LETTERS, 14), collapse = ""))
      FUNcapturedArgs <- Map(
        ee = FUNcapturedList, nam = nams, function(ee, nam) {

            out <- try(eval(ee, envir = callingEnv), silent = TRUE)
            if (is(out, "try-error")) {
              if (identical(as.name("..."), ee)) {
                out <- "..."
              } else {
                env2 <- try(if (isDollarSqBrPkgColon(ee)) {
                  whereInStack(ee[[2]])
                } else {
                  whereInStack(ee)
                }, silent = TRUE)
                if (is(env2, "try-error")) {
                  out <- try(paste(format(ee$destinationPath), collapse = " "), silent = TRUE)
                  if (is(out, "try-error"))
                    stop(env2)
                } else {
                  out <- try(eval(ee, envir = env2), silent = TRUE)
                  if (is(out, "try-error")) {
                    out <- as.character(parse(text = ee))
                  }
                }
              }
            }
          # }

          out
        }) # may be slow as it is evaluating the args
      if (needRmList) { # it has one too many list elements # not sure about the length(out) == 1
        FUNcapturedArgs <- FUNcapturedArgs[[1]]
      }
      # }

      FUNcapturedNamesEvaled <- as.call(append(list(FUNcaptured[[1]]), FUNcapturedArgs))
      FUNcapturedNamesEvaled <- matchCall(FUNcapturedNamesEvaled, callingEnv, fnName = fnNameInit)
                             } else { # this is a function called with no arguments
                               FUNcapturedNamesEvaled <- FUNcaptured
    }
  }



  # Now FUNcaptured will always have at least 1 element, because it is a call


  FUN <- FUNcapturedNamesEvaled[[1]] # This will be wrong if a fn has no args
  if (is.call(FUN)) { # This will only happen if there are no args to FUN e.g., fun()... anything else is a name fun(1)
    FUN <- FUN[[1]]
    FUNcapturedNamesEvaled[[1]] <- FUN
  }

  fnDetails <- list(
    functionName = fnNameInit,
    .FUN = FUN,
    nestLevel = 1
  )

  modifiedDots <- as.list(FUNcapturedNamesEvaled[-1]) # this is prior to filling in with defaults
  if (is.function(FUN)) {
    FUN <- getMethodAll(FUNcapturedNamesEvaled, callingEnv)
    forms <- if (is.primitive(FUN)) formals(args(FUN)) else formals(FUN)
    FUNcapturedNamesEvaled[[1]] <- FUN # may be same if it was a primitive or just a function
    fnDetails$.FUN <- FUN

    if (!is.primitive(FUN) && (length(forms) > 0)) {
      modifiedDots <- formals2(FUNcapturedNamesEvaled) # this gets default values for methods;
    }
  } else {
    # This comes from `CacheDigest(something$something)`
    FUNcapturedNamesEvaled <- append(list(NULL), FUNcaptured) # the first arg is supposed to be a function below; put NULL as placeholder
    forms <- names(FUNcapturedNamesEvaled[-1])
  }

  FUNcapturedNamesEvaled <- checkOverlappingArgs(CacheMatchedCall, forms, dotsCaptured,
                                                 functionName = fnDetails$functionName, FUNcapturedNamesEvaled)

  # # Check for args that are passed to both Cache and the FUN -- if any overlap; pass to both
  # possibleOverlap <- names(formals(args(Cache)))
  # possibleOverlap <- intersect(names(CacheMatchedCall), possibleOverlap)
  # actualOverlap <- intersect(names(forms), possibleOverlap)
  # if (length(actualOverlap) && !identical(list(), dotsCaptured)) { # e.g., useCache, verbose; but if not in dots, then OK because were separate already
  #   message(
  #     "The following arguments are arguments for both Cache and ", fnDetails$functionName, ":\n",
  #     paste0(actualOverlap, collapse = ", "),
  #     "\n...passing to both. If more control is needed, pass as a call, e.g., ",
  #     "Cache(", fnDetails$functionName, "(...))"
  #   )
  #   overlappingArgsAsList <- as.list(CacheMatchedCall)[actualOverlap]
  #   FUNcapturedNamesEvaled <- as.call(append(as.list(FUNcapturedNamesEvaled), overlappingArgsAsList))
  # }

  if (!is.null(.functionName)) {
    fnDetails$functionName <- .functionName
  }

  return(append(fnDetails, list(
    FUN = FUN, matchedCall = FUNcapturedNamesEvaled,
    modifiedDots = modifiedDots, # isDoCall = isDoCall,
    formalArgs = forms,
    userTags = userTagsOtherFunctions
  )))
}


#' Set subattributes within a list by reference
#'
#' Sets only a single element within a list attribute.
#' @param object An arbitrary object
#' @param attr The attribute name (that is a list object) to change
#' @param subAttr The list element name to change
#' @param value The new value
#'
#' @return
#' This sets or updates the `subAttr` element of a list that is located at
#' `attr(object, attr)`, with the `value`. This, therefore, updates a sub-element
#'  of a list attribute and returns that same object with the updated attribute.
#'
#' @export
#' @rdname setSubAttrInList
.setSubAttrInList <- function(object, attr, subAttr, value) {
  .CacheAttr <- attr(object, attr)
  if (is.null(.CacheAttr)) .CacheAttr <- list()
  .CacheAttr[[subAttr]] <- value
  attr(object, attr) <- .CacheAttr
  object
}

#' The exact digest function that `Cache` uses
#'
#' This can be used by a user to pre-test their arguments before running
#' `Cache`, for example to determine whether there is a cached copy.
#'
#'
#' @param ... passed to `.robustDigest`.
#' @param objsToDigest A list of all the objects (e.g., arguments) to be digested
#' @param calledFrom a Character string, length 1, with the function to
#'    compare with. Default is "Cache". All other values may not produce
#'    robust CacheDigest results.
#'
#' @inheritParams Cache
#'
#' @return
#' A list of length 2 with the `outputHash`, which is the digest
#' that Cache uses for `cacheId` and also `preDigest`, which is
#' the digest of each sub-element in `objsToDigest`.
#'
#' @export
#'
#' @examples
#' data.table::setDTthreads(2)
#' a <- Cache(rnorm, 1)
#'
#' # like with Cache, user can pass function and args in a few ways
#' CacheDigest(rnorm(1)) # shows same cacheId as previous line
#' CacheDigest(rnorm, 1) # shows same cacheId as previous line
#'
CacheDigest <- function(objsToDigest, ..., algo = "xxhash64", calledFrom = "CacheDigest",
                        .functionName = NULL, quick = FALSE) {
  FUNcaptured <- substitute(objsToDigest)
  # origFUN <- quote(objsToDigest)
  fromCache <- identical(FUNcaptured, as.name("toDigest"))
  dots <- list(...)
  forms <- .formalsNotInCurrentDots(.robustDigest, dots = dots)
  if (is(FUNcaptured, "call") || # as in rnorm(1) ... but also list(outputToSave) needs to be avoided
    (NROW(dots) > 0 && # if not an function with call, then it has to have something there
      # ... so not "just" an object in objsToDigest
      (NROW(forms) > 1 || is.null(forms)))) { # can be CacheDigest(rnorm, 1)
    fnDetails <- .fnCleanup(
      FUN = objsToDigest, callingFun = "Cache", ..., FUNcaptured = FUNcaptured,
      .functionName = .functionName, CacheMatchedCall = match.call(CacheDigest)
    )
    modifiedDots <- fnDetails$modifiedDots
    modifiedDots$.FUN <- fnDetails$.FUN
    objsToDigest <- modifiedDots
  }

  if (!is(objsToDigest, "list")) {
    objsToDigest <- list(objsToDigest)
  }

  if (identical("Cache", calledFrom)) {
    namesOTD <- names(objsToDigest)
    lengthChars <- nchar(namesOTD)
    if (!any(namesOTD %in% "FUN")) {
      zeroLength <- which(lengthChars == 0)
      if (sum(zeroLength) > 0) {
        names(objsToDigest)[zeroLength[1]] <- ".FUN"
      }
    }
  }

  # need to omit arguments that are in Cache function call
  defaults <- names(objsToDigest) %in% .defaultCacheOmitArgs
  if (sum(defaults)) {
    objsToDigest[defaults] <- NULL
  }

  if (is.character(quick) || isTRUE(quick)) {
    quickObjs <- if (isTRUE(quick)) {
      rep(TRUE, length(objsToDigest))
    } else {
      if (is.null(names(objsToDigest))) {
         rep(FALSE, length(objsToDigest))
      } else {
        names(objsToDigest) %in% quick
      }

    }
    objsToDigestQuick <- objsToDigest[quickObjs]
    objsToDigest <- objsToDigest[!quickObjs]

    preDigestQuick <- lapply(objsToDigestQuick, function(x) {
      # remove the "newCache" attribute, which is irrelevant for digest
      if (!is.null(attr(x, ".Cache")$newCache)) {
        x <- .setSubAttrInList(x, ".Cache", "newCache", NULL)
        if (!identical(attr(x, ".Cache")$newCache, NULL)) stop("attributes are not correct 1")
      }
      .robustDigest(x, algo = algo, quick = TRUE, ...)
    })
  }

  if (!is(objsToDigest, "list"))
    browser()
  preDigest <- Map(x = objsToDigest, i = seq_along(objsToDigest), function(x, i) {
    # remove the "newCache" attribute, which is irrelevant for digest
    if (!is.null(attr(x, ".Cache")$newCache)) {
      x <- .setSubAttrInList(x, ".Cache", "newCache", NULL)
      if (!identical(attr(x, ".Cache")$newCache, NULL)) stop("attributes are not correct 1")
    }
    withCallingHandlers({
      .robustDigest(x, algo = algo, quick = FALSE, ...)
    }, error = function(e) {
      nam <- names(objsToDigest)
      if (!is.null(nam))
        messageCache("Error occurred during .robustDigest of ", nam[i], " in ", .functionName)
    })
  })
  preDigest2 <- .robustDigest(objsToDigest)

  if (!isTRUE(all.equal(.orderDotsUnderscoreFirst(preDigest), .orderDotsUnderscoreFirst(preDigest2[names(preDigest)]))))
  if (is.character(quick) || isTRUE(quick)) {
    preDigest <- append(preDigest, preDigestQuick)
  }

  res <- .robustDigest(unname(sort(unlist(preDigest))), algo = algo, quick = TRUE, ...)
  list(outputHash = res, preDigest = preDigest)
}


CacheDigestOnlyUniques <- function(dots) {
  ma <- match(dots, dots)
  dig <- Map(x = ...names(), function(x) x)
  dd <- dots[unique(ma)]
  dig2 <- .robustDigest(dd)
  nameOrigOrd <- reproducible:::.orderDotsUnderscoreFirst(names(dig2))
  dig[unique(ma)] <- dig2[...names()[unique(ma)]]
  wh <- which(duplicated(dots))
  dig[wh] <- dig2[...names()[ma[wh]]]
  #dig2[...names()] <- dig
  #dig2 <- dig2[nameOrigOrd]
  #dig2
  outputHash <- .robustDigest(unname(sort(unlist(dig))), quick = TRUE)
  dig2 <- list(outputHash = outputHash, preDigest = dig)
  dig2
}

#' @importFrom data.table setDT setkeyv melt
#' @keywords internal
.findSimilar <- function(localTags, showSimilar, scalls, preDigestUnlistTrunc, userTags,
                         userTagsOrig, functionName,
                         useCache = getOption("reproducible.useCache", TRUE),
                         verbose = getOption("reproducible.verbose", TRUE)) {
  setDT(localTags)
  localTags <- localTags[nzchar(tagValue)]
  isDevMode <- identical("devMode", useCache)
  if (isDevMode) {
    showSimilar <- 1
  }
  # browser(expr = exists("._findSimilar_1"))
  # deal with tag
  userTags2 <- .getOtherFnNamesAndTags(scalls = scalls)
  noValue <- endsWith(userTags2, ":")
  if (isTRUE(any(noValue)))
    userTags2 <- userTags2[!noValue]
  userTags2 <- c(userTags2, paste("preDigest", names(preDigestUnlistTrunc),
    preDigestUnlistTrunc,
    sep = ":"
  ))
  userTags3 <- c(userTags, userTags2)
  hashName <- .cacheTableHashColName()
  cn <- if (any(colnames(localTags) %in% "tag")) "tag" else "tagKey"

  if (!(cn %in% "tag")) {
    tag <- localTags[paste(tagKey, get(.cacheTableTagColName()), sep = ":"),
      on = .cacheTableHashColName()
    ][[hashName]]
    utOrig <- if (is.null(userTagsOrig)) NULL else paste0(userTagsOrig, ":", userTagsOrig)
  }
  aa <- localTags[tag %in% userTags3 | tag %in% utOrig]
  accessed <- localTags[tagKey == "accessed"]
  hasCommonFUN <- startsWith(aa$tagValue, ".FUN") |  # same function
    startsWith(aa$tagKey, "function")  # same function name
  if (any(hasCommonFUN)) {
    hasCommonUserTagsOrig <- userTagsOrig %in% aa[[.cacheTableTagColName()]]
    if (any(hasCommonUserTagsOrig %in% FALSE)) { # Doesn't share userTagsOrig
      hasCommonFUN <- rep(hasCommonUserTagsOrig, length(hasCommonFUN))
    }
    commonCacheId <- aa$cacheId[hasCommonFUN]
    aa <- aa[aa$cacheId %in% commonCacheId]
  } else {
    aa <- aa[0]
  }
  aa <- aa[, .N, keyby = hashName]
  setkeyv(aa, "N")
  similar <- if (NROW(aa) > 0) {
    aaWithMaxN <- aa[aa$N == max(aa$N)]
    localTags[localTags$cacheId %in% aaWithMaxN$cacheId]
  } else {
    localTags[0]
  }
  # tail(aa, as.numeric(showSimilar))
  accessed <- accessed[accessed$cacheId %in% similar$cacheId]
  data.table::setorderv(accessed, "tagValue", order = -1L) # will be top one
  similar <- similar[similar$cacheId %in% accessed$cacheId[as.numeric(showSimilar)]]

  userTagsMess <- if (!is.null(userTagsOrig)) {
    paste0(.message$BecauseOfA,
      "with user supplied tags: '",
      paste(userTagsOrig, collapse = ", "), "' "
    )
  }

  if (NROW(similar)) {
    if (cn %in% "tag") {
      similar2 <- similar[grepl("preDigest", tag)]
      cacheIdOfSimilar <- similar[grepl("cacheId", tag)][[.cacheTableTagColName("tag")]]
      cacheIdOfSimilar <- unlist(strsplit(cacheIdOfSimilar, split = ":"))[2]
      similar2[, `:=`(
        fun = unlist(lapply(strsplit(get(cn), split = ":"), function(xx) xx[[2]])),
        hash = unlist(lapply(strsplit(get(cn), split = ":"), function(xx) xx[[3]]))
      )]
    } else {
      Tag <- similar[paste(tagKey, get(.cacheTableTagColName()), sep = ":"),
        on = .cacheTableHashColName()
      ][[hashName]]
      similar2 <- similar[grepl("preDigest", Tag)]
      cacheIdOfSimilar <- unique(similar[[.cacheTableHashColName()]])
      similar2[, `:=`(
        fun = unlist(lapply(
          strsplit(get(.cacheTableTagColName()), split = ":"),
          function(xx) xx[[1]]
        )),
        hash = unlist(lapply(
          strsplit(get(.cacheTableTagColName()), split = ":"),
          function(xx) xx[[2]]
        ))
      )]
    }

    a <- setDT(as.list(preDigestUnlistTrunc))
    a <- melt(a, measure.vars = seq_along(names(a)), variable.name = "fun", value.name = "hash")

    similar2 <- similar2[a, on = "fun", nomatch = NA]
    similar2[, differs := (i.hash != hash)]

    similar2[!(fun %in% names(preDigestUnlistTrunc)), differs := NA]
    similar2[(hash %in% "other"), deeperThan3 := TRUE]
    similar2[(hash %in% "other"), differs := NA]
    differed <- FALSE
    fnTxt <- paste0(if (!is.null(functionName))
      paste0("of '", .messageFunctionFn(functionName), "' ") else "call ")
    if (isDevMode) {
      messageCache("------ devMode -------", verbose = verbose)
      messageCache("This call to cache will replace", verbose = verbose)
    } else {
      messageCache("Cache ",
        fnTxt,
        "differs from",
        verbose = verbose
      )
    }

    simFun <- similar[tagKey == "function", list(funName = tail(tagValue, 1)), by = cacheId]

    sameNames <- simFun$funName %in% functionName
    if (!all(sameNames)) {
      fnTxt <- paste0("(whose function name(s) was/were '", paste(simFun$funName, collapse = "', '"), "')")
    }
    messageCache(paste0(.message$BecauseOfA, "the next closest cacheId(s) ",
                        paste(cacheIdOfSimilar, collapse = ", "), " ",
                        fnTxt, userTagsMess,
                        collapse = "\n"
    ), appendLF = TRUE, verbose = verbose)

    if (sum(similar2[differs %in% TRUE]$differs, na.rm = TRUE)) {
      differed <- TRUE
      messageCache(.message$BecauseOfA, .message$BecauseOfA, " different ",
        paste(unique(similar2[differs %in% TRUE]$fun), collapse = ", "),
        verbose = verbose
      )
    }

    if (length(similar2[is.na(differs) & deeperThan3 == TRUE]$differs)) {
      differed <- TRUE
      messageCache("...possible, unknown, differences in a nested list ",
        "that is deeper than ", getOption("reproducible.showSimilarDepth", 3), " in ",
        paste(collapse = ", ", as.character(similar2[deeperThan3 == TRUE]$fun)),
        verbose = verbose
      )
    }
    missingArgs <- similar2[is.na(deeperThan3) & is.na(differs)]$fun
    if (length(missingArgs)) {
      differed <- TRUE
      messageCache(.message$BecauseOfA, .message$BecauseOfA, " new argument(s): ",
        paste(as.character(missingArgs), collapse = ", "),
        verbose = verbose
      )
    }
    if (isDevMode) {
      messageCache("------ end devMode -------", verbose = verbose)
    }
  } else {
    if (!identical("devMode", useCache)) {
      messageCache(.message$noSimilarCacheTxt(functionName), verbose = verbose)
      #messageCache("There is no similar item in the cachePath ",
      #  if (!is.null(functionName)) paste0("of '", functionName, "' ") else "",
      # verbose = verbose)
      if (!is.null(userTagsMess)) {
        messageCache("  ", userTagsMess, "\n", verbose = verbose)
      }
    }
  }
}


#' @keywords internal
.defaultCacheOmitArgs <- c(
  "useCloud", "checksumsFileID", "cloudFolderID",
  "notOlderThan", ".objects", "outputObjects", "algo", "cachePath",
  "length", "compareRasterFileLength", "userTags", "digestPathContent",
  "omitArgs", "classOptions", "debugCache", "sideEffect", "makeCopy",
  "quick", "verbose", "cacheId", "useCache", "showSimilar", "cl"
)

#' @keywords internal
verboseTime <- function(verbose, verboseLevel = 3) {
  if (verbose >= verboseLevel) {
    return(Sys.time())
  }
}

#' @keywords internal
verboseMessage1 <- function(verbose, userTags) {
  if (verbose > 2) {
    messageCache("Using devMode; overwriting previous Cache entry with tags: ",
      paste(userTags, collapse = ", "),
      verbose = verbose
    )
  }
  invisible(NULL)
}

#' @keywords internal
verboseMessage2 <- function(verbose) {
  if (verbose > 2) {
    messageCache("Using devMode; Found entry with identical userTags, ",
      "but since it is very different, adding new entry",
      verbose = verbose
    )
  }
  invisible(NULL)
}

#' @keywords internal
verboseMessage3 <- function(verbose, artifact) {
  if (length(unique(artifact)) > 1) {
    if (verbose > 2) {
      messageCache("Using devMode, but userTags are not unique; defaulting to normal useCache = TRUE",
        verbose = verbose
      )
    }
  }
}


verboseDF0 <- function(verbose, functionName, startHashTime, endTime) {
  if (verbose > 3) {
    if (missing(endTime))
      endTime <- Sys.time()
    verboseDF <- data.frame(
      functionName = functionName,
      component = "Hashing",
      elapsedTime = as.numeric(difftime(endTime, startHashTime, units = "secs")),
      units = "secs",
      stringsAsFactors = FALSE
    )
    verboseAppendOrCreateDF(verboseDF)
  }
  # if (exists("verboseTiming", envir = .reproEnv, inherits = FALSE)) {
  #   verboseDF$functionName <- paste0("  ", verboseDF$functionName)
  #   .reproEnv$verboseTiming <- rbind(.reproEnv$verboseTiming, verboseDF)
  # } else {
  #   .reproEnv$verboseTiming <- verboseDF
  # }
}

#' @keywords internal
verboseDF1 <- function(verbose, functionName, startRunTime, endTime) {
  if (verbose > 3) {
    if (missing(endTime))
      endTime <- Sys.time()
    verboseDF <- data.frame(
      functionName = functionName,
      component = paste("Running", functionName),
      elapsedTime = as.numeric(difftime(endTime, startRunTime, units = "secs")),
      units = "secs",
      stringsAsFactors = FALSE
    )

    if (exists("verboseTiming", envir = .reproEnv)) {
      .reproEnv$verboseTiming <- rbind(.reproEnv$verboseTiming, verboseDF)
    }
  }
}

#' @keywords internal
verboseDF2 <- function(verbose, functionName, startSaveTime, endTime) {
  if (verbose > 3) {
    if (missing(endTime))
      endTime <- Sys.time()
    verboseDF <-
      data.frame(
        functionName = functionName,
        component = "Saving to cachePath",
        elapsedTime = as.numeric(difftime(endTime, startSaveTime, units = "secs")),
        units = "secs",
        stringsAsFactors = FALSE
      )

    if (exists("verboseTiming", envir = .reproEnv)) {
      .reproEnv$verboseTiming <- rbind(.reproEnv$verboseTiming, verboseDF)
    }
  }
}


#' @keywords internal
verboseDF3 <- function(verbose, functionName, startCacheTime, endTime) {
  if (verbose > 3) {
    if (missing(endTime))
      endTime <- Sys.time()
    verboseDF <- data.frame(
      functionName = functionName,
      component = "Whole Cache call",
      elapsedTime = as.numeric(difftime(endTime, startCacheTime,
        units = "secs"
      )),
      units = "secs",
      stringsAsFactors = FALSE
    )

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
  # }

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

  # if (any(objOverride)) {
  #   # get from .reproEnv
  #   lsDotReproEnv <- ls(.reproEnv)
  #   prevVals <- .namesCacheFormals[objOverride] %in% lsDotReproEnv
  #   if (any(prevVals)) {
  #     list2env(mget(.namesCacheFormals[objOverride][prevVals], .reproEnv), envir = envir)
  #   }
  # }

  return(list(
    oldUserTags = oldUserTags, namesUserCacheArgs = namesUserCacheArgs,
    prevVals = prevValsInitial, prevUserTags = prevUserTags,
    objOverride = objOverride
  ))
}

getCacheRepos <- function(cachePath, modifiedDots, verbose = getOption("reproducible.verbose", 1)) {
  if (is.null(cachePath)) {
    cachePath <- .checkCacheRepo(modifiedDots, create = TRUE, verbose = verbose)
  } else {
    if (any(!dir.exists(unlist(cachePath))))
      cachePath <- lapply(cachePath, function(repo) {
        if (!dir.exists(repo))
          repo <- checkPath(repo, create = TRUE)
        repo
      })
  }
  return(cachePath)
}

devModeFn1 <- function(localTags, userTags, userTagsOrig, scalls, preDigestUnlistTrunc, useCache, verbose,
                       isInRepo, outputHash) {
  # browser(expr = exists("._devModeFn1_1"))
  userTags <- gsub(".*:(.*)", "\\1", userTags)
  isInRepoAlt <- localTags[localTags[[.cacheTableTagColName("tag")]] %in% userTags, , drop = FALSE]
  data.table::setDT(isInRepoAlt)
  if (NROW(isInRepoAlt) > 0) {
    isInRepoAlt <- isInRepoAlt[, iden := identical(sum(get(.cacheTableTagColName("tag"))
    %in% userTags), length(userTags)),
    by = eval(.cacheTableHashColName())
    ][iden == TRUE]
  }
  if (NROW(isInRepoAlt) > 0 && length(unique(isInRepoAlt[[.cacheTableHashColName()]])) == 1) {
    newLocalTags <- localTags[localTags[[.cacheTableHashColName()]] %in% isInRepoAlt[[.cacheTableHashColName()]], ]
    tags1 <- grepl(
      paste0(
        "(",
        # paste("accessed", "cacheId", "class", "date", "format", "function", "inCloud",
        #      "name", "object.size", otherFunctions, "preDigest", "file.size",
        #      sep = "|"),
        paste(.defaultUserTags, collapse = "|"),
        ")"
      ),
      newLocalTags[["tagKey"]]
    )
    localTagsAlt <- newLocalTags[!tags1, ]
    # browser(expr = exists("._devModeFn1_2"))

    if (all(localTagsAlt[[.cacheTableTagColName("tag")]] %in% userTags)) {
      mess <- capture.output(type = "output", {
        similars <- .findSimilar(newLocalTags,
          scalls = scalls,
          preDigestUnlistTrunc = preDigestUnlistTrunc,
          userTags = userTags, userTagsOrig = userTagsOrig,
          useCache = useCache,
          verbose = verbose
        )
      })
      similarsHaveNA <- sum(is.na(similars$differs))
      # similarsAreDifferent <- sum(similars$differs == TRUE, na.rm = TRUE)
      # likelyNotSame <- sum(similarsHaveNA, similarsAreDifferent)/NROW(similars)

      if (similarsHaveNA < 2) {
        verboseMessage1(verbose, userTags)
        uniqueCacheId <- unique(isInRepoAlt[[.cacheTableHashColName()]])
        outputHash <- uniqueCacheId[uniqueCacheId %in% newLocalTags[[.cacheTableHashColName()]]]
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

cloudFolderFromCacheRepo <- function(cachePath) {
  paste0(basename2(dirname(cachePath)), "_", basename2(cachePath))
}

.defaultUserTags <- c(
  "function", "class", "object.size", "accessed", "inCloud", "fromDisk",
  otherFunctions, "preDigest", "file.size", "cacheId",
  "elapsedTimeDigest", "elapsedTimeFirstRun", "resultHash", "elapsedTimeLoad"
)

.defaultOtherFunctionsOmit <- c(
  "(test_", "with_reporter", "force", "Restart", "with_mock",
  "eval", "::", "\\$", "\\.\\.", "standardGeneric",
  "Cache", "tryCatch", "doTryCatch", "withCallingHandlers",
  "FUN", "capture", "withVisible)"
)

isPkgColonFn <- function(x) {
  identical(x[[1]], quote(`::`))
}

evalTheFun <- function(FUNcaptured, isCapturedFUN, matchedCall, envir = parent.frame(),
                       verbose = getOption("reproducible.verbose"), ...) {
  .message$IndentUpdate()
  withCallingHandlers(
    {
      out <- eval(FUNcaptured, envir = envir)
      if (is.function(out)) { # if is wasn't "captured", then it is just a function, so now use it on the ...
        out <- out(...)
      }
    },
    warning = function(w) {
      asch <- format(w$call[[1]])
      warning("In ", format(matchedCall), ": ", w$message, call. = FALSE)
      invokeRestart("muffleWarning")
      #    }
    }
  )

  out
}

searchInRepos <- function(cachePaths, outputHash, drv, conn) {
  dbTabNam <- NULL
  tries <- 1
  while (tries <= length(cachePaths)) {
    repo <- cachePaths[[tries]]
    if (useDBI()) {
      dbTabNam <- CacheDBTableName(repo, drv = drv)

      isInRepo <- getHashFromDB(tries, conn, drv, repo, dbTabNam, outputHash)
      # if (tries > 1) {
      #   DBI::dbDisconnect(conn)
      #   conn <- dbConnectAll(drv, cachePath = repo)
      # }
      # qry <- glue::glue_sql("SELECT * FROM {DBI::SQL(glue::double_quote(dbTabName))} where \"cacheId\" = ({outputHash})",
      #   dbTabName = dbTabNam,
      #   outputHash = outputHash,
      #   .con = conn
      # )
      # res <- retry(
      #   retries = 15, exponentialDecayBase = 1.01,
      #   quote(DBI::dbSendQuery(conn, qry))
      # )
      # isInRepo <- setDT(DBI::dbFetch(res))
      # DBI::dbClearResult(res)
    } else {
      # The next line will find it whether it is qs, rds or other; this is necessary for "change cacheSaveFormat"
      csf <- CacheStoredFile(cachePath = repo, cacheId = outputHash, format = "check")

      if (all(file.exists(csf))) {
        dtFile <- CacheDBFileSingle(cachePath = repo, cacheId = outputHash)

        if (!file.exists(dtFile)) { # check first for wrong rds vs qs
          dtFile <- CacheDBFileSingle(cachePath = repo, cacheId = outputHash, format = "check")
          fe <- file.exists(dtFile)
          if (isTRUE(!(fe))) { # still doesn't == means it is broken state
            warning(
              "The Cache file exists for ", outputHash, ", but there is no database entry for it; removing ",
              "the file and rerunning the call"
            )
            unlink(csf)
            dtFile <- NULL
          } else if (length(fe) > 1) { # has both the qs and rds dbFile
            browser()

          }
        }

        isInRepo <- if (!is.null(dtFile)) {
          loadFile(dtFile)
        } else {
          NULL
        }
      } else {
        isInRepo <- data.table::copy(.emptyCacheTable)
      }
    }
    fullCacheTableForObj <- isInRepo
    if (NROW(isInRepo) > 1) isInRepo <- isInRepo[NROW(isInRepo), ]
    if (NROW(isInRepo) > 0) {
      # browser(expr = exists("._Cache_4"))
      cachePath <- repo
      break
    }
    tries <- tries + 1
  }
  list(
    isInRepo = isInRepo, dbTabName = dbTabNam, fullCacheTableForObj = fullCacheTableForObj,
    cachePath = repo
  )
}



returnObjFromRepo <- function(isInRepo, notOlderThan, fullCacheTableForObj, cachePath,
                              verbose = getOption("reproducible.verbose"),
                              FUN, fnDetails, modifiedDots,
                              debugCache, # sideEffect,
                              quick, algo, preDigest, startCacheTime, drv, conn,
                              outputHash, useCloud, gdriveLs, cloudFolderID, lastEntry, lastOne, ...) {
  .cacheMessageObjectToRetrieve(fnDetails$functionName, fullCacheTableForObj,
                           cachePath, cacheId = isInRepo[[.cacheTableHashColName()]], verbose)

  preLoadTime <- Sys.time()
  output <- try(.getFromRepo(FUN,
    isInRepo = isInRepo,
    # fileFormat = NULL,
    fullCacheTableForObj = fullCacheTableForObj,
    notOlderThan = notOlderThan,
    lastOne = lastOne,
    cachePath = cachePath,
    fnDetails = fnDetails,
    modifiedDots = modifiedDots,
    debugCache = debugCache,
    verbose = verbose,
    # sideEffect = sideEffect,
    quick = quick,
    algo = algo,
    preDigest = preDigest,
    startCacheTime = startCacheTime,
    drv = drv,
    conn = conn,
    ...
  ), silent = TRUE)
  postLoadTime <- Sys.time()
  elapsedTimeLoad <- postLoadTime - preLoadTime

  # browser(expr = exists("._Cache_7"))
  if (is(output, "try-error")) {
    cID <- # if (useDBI())
      isInRepo[[.cacheTableHashColName()]]
    # else
    #   gsub("cacheId:", "", isInRepo[[.cacheTableTagColName()]])
    clearCache(cachePath, userTags = cID, ask = FALSE)
    message(
      output, "\nError in trying to recover cacheID; it is likely corrupt.",
      "\n  removing it with... clearCache('", cachePath, "', userTags = '", cID, "')",
      "\n  and proceeding to recalculate"
    )
    return(output)
  }

  .updateTagsRepo(outputHash, cachePath, "elapsedTimeLoad",
    format(elapsedTimeLoad, units = "secs"),
    add = TRUE,
    drv = drv, conn = conn
  )
  if (useCloud) {
    # Here, upload local copy to cloud folder
    isInCloud <- any(grepl(outputHash, gdriveLs$name))
    if (isInCloud %in% FALSE) {
      outputToSave <- .wrap(output, cachePath, preDigest = preDigest, drv = drv, conn = conn,
                            cacheId = outputHash, verbose = verbose)
      cufc <- try(cloudUploadFromCache(isInCloud, outputHash, cachePath, cloudFolderID, ## TODO: saved not found
        outputToSave,
        verbose = verbose
      ))
      .updateTagsRepo(outputHash, cachePath, "inCloud", "TRUE", drv = drv, conn = conn)
    }
  }

  return(output)
}

whereInStack <- function(obj, startingEnv = parent.frame()) {
  foundStarting <- FALSE
  for (i in 1:sys.nframe()) {
    testEnv <- sys.frame(-i)
    if (!foundStarting) {
      if (identical(testEnv, startingEnv)) {
        foundStarting <- TRUE
      } else {
        next
      }
    }
    fn <- if (R.version$minor < "1.0" && R.version$major <= "4") { # faster than any other approach
      get0(as.character(parse(text = obj)), testEnv, inherits = FALSE)
    } else {
      get0(obj, testEnv, inherits = FALSE) # much faster; only works R >= 4.1
    }
    if (!is.null(fn)) {
      break
    }
  }
  return(testEnv)
}

browserCond <- function(expr) {
  any(startsWith(ls(.GlobalEnv), expr))
}

spatVectorNamesForCache <- c("x", "type", "atts", "crs")


addCacheAttr <- function(output, .CacheIsNew, outputHash, FUN) {
  output <- .setSubAttrInList(output, ".Cache", "newCache", .CacheIsNew)
  attr(output, "tags") <- paste0("cacheId:", outputHash)
  attr(output, "call") <- ""
  if (!identical(attr(output, ".Cache")$newCache, .CacheIsNew)) {
    stop("attributes are not correct 3")
  }
  if (!identical(attr(output, "call"), "")) {
    stop("attributes are not correct 4")
  }
  if (!identical(attr(output, "tags"), paste0("cacheId:", outputHash))) {
    stop("attributes are not correct 5")
  }

  if (isS4(FUN)) {
    attr(output, "function") <- FUN@generic
    if (!identical(attr(output, "function"), FUN@generic)) {
      stop("There is an unknown error 03")
    }
  }
  output
}


nullifyByArgName <- function(a, name) {
  if (is(a, "list")) {
    toNull <- names(a) %in% name
    a[toNull] <- NULL
    a <- lapply(a, nullifyByArgName, name = name)
  }
  a
}


objectSizeGetFromUserTags <- function(userTags) {
  otsObjSize <- gsub(grep("object\\.size:", userTags, value = TRUE),
                     pattern = "object.size:", replacement = ""
  )
  otsObjSize <- if (identical(unname(otsObjSize), "NA")) NA else as.numeric(otsObjSize)
  otsObjSize
}

.objectSizeMinForBig <- 5e6

getFromCacheWithCacheIdPrevious <- function(.functionName, verbose, tagKey, inRepos) {
  sc <- showCache(fun = .functionName, verbose = -2)
  if (NROW(sc)) {
    messageCache("cacheId is 'previous' meaning it will recover the most recent ",
                 "cache item (accessed) that matches on .functionName: ",
                 .messageFunctionFn(.functionName), "\nPlease ensure ",
                 "the function name is precise enough for this behaviour", verbose = verbose)
    outputHashNew <- data.table::setorderv(sc[tagKey == "accessed"], "tagValue", order = -1L)
    outputHash <- outputHashNew$cacheId[1]
    inRepos$isInRepo <- outputHashNew[1, ]
    inRepos$fullCacheTableForObj <- showCacheFast(cacheId = outputHash)
  }
}

cacheIdCheckInCache <- function(cacheId, calculatedCacheId, .functionName, verbose) {
  sc <- NULL
  if (!is.null(cacheId)) {
    if  (identical(cacheId, "previous")) {
      sc <- showCache(fun = .functionName, verbose = -2)
      if (NROW(sc)) {
        messageCache("cacheId is 'previous' meaning it will recover the most recent ",
                     "cache item (accessed) that matches on .functionName: ",
                     .messageFunctionFn(.functionName), "\nPlease ensure ",
                     "the function name is precise enough for this behaviour", verbose = verbose)
        outputHashNew <- data.table::setorderv(sc[tagKey == "accessed"], "tagValue", order = -1L)
        outputHash <- outputHashNew$cacheId[1]
        sc <- sc[cacheId %in% outputHash, ]
        attr(sc, "cacheId") <- outputHash
        # sc <- showCacheFast(cacheId = outputHash)
      } else {
        sc <- NULL
      }
    } else {
      outputHashManual <- cacheId
      if (identical(outputHashManual, calculatedCacheId)) {
        messageCache(.message$cacheIdSameTxt, verbose = verbose)
        sc <- showCache(userTags = cacheId, verbose = verbose -1)
      } else {
        sc <- showCache(userTags = sc, verbose = verbose -1)
        messageCache(.message$cacheIdNotSameTxt(cacheId), verbose = verbose)
        # if (NROW(sc))
          # isInRepo <- sc[1,]
      }
      attr(sc, "cacheId") <- cacheId
      # outputHash <- outputHashManual
    }

    # sc <- inRepos$fullCacheTableForObj
  }
  sc
}


checkOverlappingArgs <- function(CacheMatchedCall, forms, dotsCaptured, functionName,
                                 FUNcapturedNamesEvaled, whichCache = "Cache") {
  # Check for args that are passed to both Cache and the FUN -- if any overlap; pass to both
  possibleOverlap <- if (identical(whichCache, "Cache")) .namesCacheFormals else .namescache2Formals # names(formals(args(Cache)))
  if (!is.call(CacheMatchedCall[["FUN"]])) {
    possibleOverlap <- intersect(names(CacheMatchedCall), possibleOverlap)
    actualOverlap <- intersect(names(forms), possibleOverlap)
    if (length(actualOverlap) && !identical(list(), dotsCaptured)) { # e.g., useCache, verbose; but if not in dots, then OK because were separate already
      message(
        "The following arguments are arguments for both Cache and ", functionName, ":\n",
        paste0(actualOverlap, collapse = ", "),
        "\n...passing to both. If more control is needed, pass as a call, e.g., ",
        "Cache(", functionName, "(...))"
      )
      overlappingArgsAsList <- as.list(CacheMatchedCall)[actualOverlap]
      FUNcapturedNamesEvaled <- as.call(append(as.list(FUNcapturedNamesEvaled), overlappingArgsAsList))
    }
  }
  FUNcapturedNamesEvaled
}



verboseAppendOrCreateDF <- function(verboseDF) {
  if (exists("verboseTiming", envir = .reproEnv, inherits = FALSE)) {
    verboseDF$functionName <- paste0("  ", verboseDF$functionName)
    .reproEnv$verboseTiming <- rbind(.reproEnv$verboseTiming, verboseDF)
  } else {
    .reproEnv$verboseTiming <- verboseDF
  }
}



checkConns <- function(cachePaths, conn) {
  conns <- list()
  if (!is.null(conn)) { # if the conn was passed by user
    if (!is.list(conn)) {
      conn <- list(conn)
    }
    if (!identical(length(cachePaths), length(conn))) {
      stop("conn and cachePath are both provided, but are different lengths which is not allowed")
    }
    names(conn) <- cachePaths
    conns <- conn
  }
}


createConns <- function(cachePath, conns, drv) {
  if (useDBI()) {
    drv <- getDrv(drv)
    if (is.null(conns[[cachePath]])) {
      conns[[cachePath]] <- dbConnectAll(drv, cachePath = cachePath)
      RSQLite::dbClearResult(RSQLite::dbSendQuery(conns[[cachePath]], "PRAGMA busy_timeout=5000;"))
      RSQLite::dbClearResult(RSQLite::dbSendQuery(conns[[cachePath]], "PRAGMA journal_mode=WAL;"))
    }
  }

  isIntactRepo <- CacheIsACache(
    cachePath = cachePath, drv = drv, create = TRUE,
    conn = conns[[cachePath]]
  )
  if (any(!isIntactRepo)) {
    ret <- createCache(cachePath,
                       drv = drv, conn = conns[[cachePath]],
                       force = isIntactRepo
    )
  }
  conns
}

getHashFromDB <- function(tries, conn, drv, repo, dbTabNam, outputHash) {
  if (tries > 1) {
    DBI::dbDisconnect(conn)
    conn <- dbConnectAll(drv, cachePath = repo)
  }
  qry <- glue::glue_sql("SELECT * FROM {DBI::SQL(glue::double_quote(dbTabName))} where \"cacheId\" = ({outputHash})",
                        dbTabName = dbTabNam,
                        outputHash = outputHash,
                        .con = conn
  )
  res <- retry(
    retries = 15, exponentialDecayBase = 1.01,
    quote(DBI::dbSendQuery(conn, qry))
  )
  isInRepo <- setDT(DBI::dbFetch(res))
  DBI::dbClearResult(res)
  isInRepo
}
