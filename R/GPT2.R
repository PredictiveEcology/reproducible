utils::globalVariables("arg")

#' @param dryRun See [reproducibleOptions].
#'
#' @include messages.R
#' @export
#' @rdname Cache
Cache <- function(FUN, ..., dryRun = getOption("reproducible.dryRun", FALSE),
                  notOlderThan = NULL,
                  .objects = NULL, .cacheExtra = NULL, .functionName = NULL,
                  outputObjects = NULL, # nolint
                  algo = "xxhash64",
                  cachePath = NULL,
                  length = getOption("reproducible.length", Inf),
                  userTags = c(),
                  omitArgs = NULL,
                  classOptions = list(),
                  debugCache = character(),
                  quick = getOption("reproducible.quick", FALSE),
                  verbose = getOption("reproducible.verbose", 1),
                  cacheId = NULL,
                  cacheSaveFormat = getOption("reproducible.cacheSaveFormat"),
                  useCache = getOption("reproducible.useCache", TRUE),
                  useCloud = getOption("reproducible.useCloud", FALSE),
                  cloudFolderID = getOption("reproducible.cloudFolderID", NULL),
                  showSimilar = getOption("reproducible.showSimilar", FALSE),
                  drv = getOption("reproducible.drv", NULL),
                  conn = getOption("reproducible.conn", NULL)) {

  .callingEnv <- parent.frame()

  # Sets useDBI(TRUE) if a user has supplied a drv or conn
  optionsSetForCache(drv = drv, conn = conn)

  # Capture and match call so it can be manipulated
  callList <- matchCall2(sys.function(0), sys.call(0), envir = .callingEnv, FUN = FUN)

  if (isFALSE(getOption("reproducible.useCacheV3"))) {
    # This run CacheV2 which is the OLD CACHE FUNCTION
    callList$call[[1]] <- substitute(CacheV2)
    return(eval(callList$call, envir = .callingEnv))
  }

  # Check if this is a nested Cache call; this must be before skipCache because useCache may be numeric
  userTags <- setupCacheNesting(userTags, useCache) # get nested userTags

  # Skip Cache if user passes useCache = FALSE or 0 or nesting level is deeper than useCache
  useCache <- useCacheFromNested(useCache)
  if (isFALSE(useCache))
    return(skipCache(FUN, ..., usesDots = callList$usesDots, useCache = useCache,
                     functionName = format(callList$FUNorig), verbose = verbose, .callingEnv = .callingEnv))

  # Harmonize call so the different versions are all cannonical, now that useCache = FALSE is past
  callList <- harmonizeCall(callList, .callingEnv, .functionName)
  # Add .functionName to .pkgEnv userTags in case this becomes part of a nested Cache
  appendFunctionNameToNestedTags(userTags, callList$.functionName)
  # .pkgEnv$.reproEnv2$userTags
  # appendNestedTags(outerFunction = callList$.functionName)


  # do the Digest
  times <- list()
  times$CacheDigestStart <- Sys.time()

  # Construct the full file path for the cache directory and possible file
  cachePaths <- getCacheRepos(cachePath, callList$new_call[-1], verbose = verbose)

  # Override keyFull$key if user has specified with cacheId
  if (!is.null(cacheId) && !is.na(cacheId)) {
    keyFull <- list()
    keyFull$key <- cacheIdOverride(cacheId, keyFull$key, callList$.functionName, verbose)
    if (is.null(keyFull$key))
      cacheId <- NULL
  }

  if (is.null(cacheId) || is.na(cacheId)) {
    toDigest <- doDigestPrepare(callList$new_call, omitArgs, .cacheExtra)
    keyFull <- try(doDigest(toDigest, callList$.functionName, .objects,
                       length, algo, quick, classOptions, times$CacheDigestStart,
                       verbose = verbose))
    if (is(keyFull, "try-error")) {
      stopRcppError(toDigest, .objects, length, algo, quick, classOptions)
    }
  }

  # If debugCache is "quick", short circuit after doDigest
  if (isTRUE(!is.na(pmatch(debugCache, "quick"))))
    return(list(hash = keyFull$preDigest, content = callList$func_call))

  # cachePath <- cachePaths[[1]]
  CacheDBFileCheckAndCreate(cachePaths[[1]], drv, conn, verbose = verbose) # checks that we are using multiDBfile backend

  if (cloudWrite(useCloud)) {
    cloudFolderID <- checkAndMakeCloudFolderID(cloudFolderID, cachePaths[[1]], create = TRUE, verbose = verbose)
    gdriveLs <- retry(quote(driveLs(cloudFolderID, keyFull$key, cachePath = cachePaths[[1]], verbose = verbose)))
  }

  if (missing(dryRun)) dryRun <- getOption("reproducible.cacheDryRun", FALSE)

  # if (dryRun) {
  #   metadata <- metadata_define_preEval(keyFull, callList$.functionName, userTags,
  #                                       .objects, length, algo, quick, classOptions,
  #                                       times$EvaluateStart, times$CacheDigestStart)
  #
  #   if (isTRUE(showSimilar) || isDevMode(useCache, userTags))
  #     showSimilar(cachePaths[[1]], metadata, callList$.functionName, userTags, useCache,
  #                 drv = drv, conn = conn, verbose)
  #   return(NULL)
  # }

  # Memoise and return if it is there #
  if (!dryRun) {
    outputFromMemoise <- check_and_get_memoised_copy(keyFull$key, cachePaths, callList$.functionName,
                                                     callList$func, useCache, useCloud,
                                                     cloudFolderID, gdriveLs, full_call = callList$new_call,
                                                     outputObjects = outputObjects,
                                                     cacheSaveFormat = cacheSaveFormat,
                                                     drv = drv, conn = conn, verbose = verbose)
    if (!identical2(.returnNothing, outputFromMemoise))
      return(outputFromMemoise)

    # After memoising fail, try files; need to check Cache dir and set lockfile
    locked <- lockFile(cachePaths[[1]], keyFull$key, verbose = verbose)

    if (useDBI()) {
      connOrig <- conn
      conn <- checkConns(cachePaths, conn)
      drv <- getDrv(getOption("reproducible.drv", NULL))
      for (cachePath in cachePaths)
        conn <- createConns(cachePath, conn, drv, verbose = verbose) # this will convert backend if it is wrong

      if (is.null(connOrig)) # don't disconnect if conn was user passed
        # if this is >1st cachePath, then the db will already be disconnected; suppressWarnings
        on.exit(dbDisconnectAll(conn), add = TRUE)
    }

    # Check if keyFull$key is on disk and return if it is there
    outputFromDisk <- check_and_get_cached_copy(keyFull$key, cachePaths, cache_file, callList$.functionName, callList$func,
                                                useCache, useCloud, cloudFolderID, gdriveLs,
                                                full_call = callList$new_call,
                                                outputObjects = outputObjects,
                                                cacheSaveFormat = cacheSaveFormat,
                                                drv, conn, verbose = verbose)

    if (!identical2(.returnNothing, outputFromDisk))
      return(outputFromDisk)

  }
  if (useDBI()) conn <- attr(outputFromDisk, ".Cache")$conn

  cache_file <- CacheStoredFile(cachePaths[[1]], keyFull$key) # now we know it is not in Cache; use 1st cachePath
  if (cloudReadOnly(useCloud)) {# now that it is established it isn't in cache locally
    cloudFolderID <- checkAndMakeCloudFolderID(cloudFolderID, cachePaths[[1]], create = TRUE, verbose = verbose)
    gdriveLs <- retry(quote(driveLs(cloudFolderID, keyFull$key, cachePath = cachePaths[[1]], verbose = verbose)))
  }

  if (cloudWriteOrRead(useCloud) && isTRUE(any(keyInGdriveLs(keyFull$key, gdriveLs)))) {
    newFileName <- gdriveLs$name[which(keyInGdriveLs(keyFull$key, gdriveLs))] # paste0(outputHash,".rda")
    shownCache <- cloudDownload(keyFull$key, newFileName, gdriveLs, cachePaths[[1]], cloudFolderID,
                                drv = drv, conn = conn, verbose = verbose)
    outputFromDisk <- check_and_get_cached_copy(keyFull$key, cachePaths, cache_file, callList$.functionName, callList$func,
                                                useCache, useCloud = FALSE, cloudFolderID, gdriveLs,
                                                full_call = callList$new_call,
                                                outputObjects = outputObjects,
                                                cacheSaveFormat = cacheSaveFormat,
                                                drv, conn, verbose = verbose)
    return(outputFromDisk)
  } # Derive some metadata prior to evaluation so "showSimilar" can have something to compare with

  times$EvaluateStart <- Sys.time()
  metadata <- metadata_define_preEval(keyFull, callList$.functionName, userTags,
                                      .objects, length, algo, quick, classOptions,
                                      times$EvaluateStart, times$CacheDigestStart)

  if (isTRUE(showSimilar) || isDevMode(useCache, userTags) || isTRUE(dryRun)) {
    if (dryRun) messageColoured(.txtDryRunTRUE, colour = "green")
    showSimilar(cachePaths[[1]], metadata, callList$.functionName, userTags, useCache,
                # cacheSaveFormat = cacheSaveFormat,
                drv = drv, conn = conn, verbose)
  }
  if (isTRUE(dryRun))
    return(invisible(NULL))

  # ## evaluate the call ## #
  outputFromEvaluate <- evalTheFunAndAddChanged(callList = callList, keyFull = keyFull,
                                                outputObjects = outputObjects, length = length,
                                                algo = algo, quick = quick, classOptions = classOptions,
                                                .callingEnv = .callingEnv,
                                                verbose = verbose, ...)

  # ## Save to Cache; including to Memoise location; including metadata ## #
  times$SaveStart <- Sys.time()
  elapsedTimeFUN <- difftime(times$SaveStart, times$EvaluateStart, units = "secs")

  # update metadata with other elements including elapsedTime for evaluation
  metadata <- metadata_define_postEval(metadata, keyFull$key, outputFromEvaluate,
                                       userTags, .objects, length, algo, quick,
                                       classOptions, elapsedTimeFUN)

  outputFromEvaluate <- doSaveToCache(outputFromEvaluate, metadata, cachePaths, callList$func,
                                      .objects, length, algo, quick, classOptions,
                                      cache_file, userTags, callList$.functionName, debugCache,
                                      keyFull, outputObjects = outputObjects,
                                      useCloud, cloudFolderID, gdriveLs,
                                      func_call = callList$func_call,
                                      cacheSaveFormat = cacheSaveFormat, drv = drv, conn = conn,
                                      useMemoise = getOption("reproducible.useMemoise", FALSE),
                                      verbose = verbose,
                                      times$SaveStart, times$EvaluateStart)
  times$SaveEnd <- Sys.time()
  if (getOption("reproducible.savePreDigest", FALSE)) {
    keyFullPreDigest <- keyFull
    keyFullPreDigest$key <- paste0(.txtPreDigest, "_", keyFullPreDigest$key)
    times$SavePreDigestStart <- Sys.time()
    locked <- lockFile(cachePaths[[1]], keyFullPreDigest$key, verbose = verbose)

    toDigestOut <- doSaveToCache(toDigest, metadata, cachePaths, callList$func,
                                 .objects, length, algo, quick, classOptions,
                                 cache_file, userTags, callList$.functionName, debugCache,
                                 keyFullPreDigest, outputObjects = outputObjects,
                                 func_call = callList$func_call,
                                 cacheSaveFormat = cacheSaveFormat,
                                 drv = drv, conn = conn,
                                 useCloud = FALSE, # not this preDigest one
                                 cloudFolderID = NULL, gdriveLs = NULL,# not this preDigest one
                                 useMemoise = FALSE, # not this preDigest one
                                 verbose = verbose,
                                 times$SavePreDigestStart, times$SaveStart)
    times$SaveEnd <- Sys.time()
  }
  verboseCacheDFAll(verbose, callList$.functionName, times)

  return(outputFromEvaluate)
}

#' @rdname Cache
cache2 <- Cache

#' Convert all ways of calling a function into canonical form, including defaults
#'
#' e.g., stats::rnorm(1) --> rnorm(n = 1, mean = 0, sd = 1)
#' @param call The full captured call as it was passed by user.
#' @param usesDots Logical. Whether the original `Cache` call used `...`
#' @param isSquiggly Logical. Whether there are curly braces e.g., as in a pipe sequence.
#' @param .callingEnv Environment. The environment from which `Cache` was called.
convertCallToCommonFormat <- function(call, usesDots, isSquiggly, .callingEnv) {
  # Capture the unevaluated call

  .functionName <- NULL
  # Check if the first argument is a function call
  func_full <- NULL

  func_call <- NULL
  if (is.call(call[[2]])) {

    func_call <- call[[2]]  # This is the actual function call (e.g., stats::rnorm)
    # Extract the function without the package prefix
    if (is.call(func_call[[1]]) && func_call[[1]][[1]] == quote(`::`)) {
      func <- func_full <- func_call[[1]]
      if (length(func_call) == 2)
        args <- func_call[[-1]]
      else
        args <- as.list(func_call)[-1]
      func_call <- as.call(c(func_call[[1]][[3]], args))
    } else {
      if (func_call[[1]] == quote(`::`) || func_call[[1]] == quote(`:::`)) {
        func_full <- func_call
        func <- func_call  # Package prefix, using FUN as name only
        args <- as.list(call[-(1:2)])
        func_call <- as.call(c(func_call[[3]], args))
      } else {
        if (isDollarOnlySqBr(func_call)) {
          func <- eval(func_call, envir = .callingEnv)  # No package prefix
          if (usesDots)
            func_call <- as.call(append(list(func), as.list(call[-(1:2)])))
        } else {
          # It is a complete call e.g., FUN = rnorm(1)
          func <- func_call[[1]]  # No package prefix
          if (isDollarOnlySqBr(func)) {
            func <- eval(func, envir = .callingEnv)  # No package prefix
          }
        }
        if (identical(func, quote(do.call))) {
          func_call <- undoDoCall(func_call, .callingEnv = .callingEnv)
          func <- func_call[[1]]  # Extract the function for do.call (e.g., rnorm)
        }
        args <- as.list(func_call)[-1]
      }
    }
  } else if (identical(call[[2]], quote(do.call))) {
    # Special handling for do.call to return the function unevaluated
    func <- call[[3]]  # Extract the function for do.call (e.g., rnorm)
    args <- eval(call[[4]], envir = .callingEnv)  # Evaluate the argument list
  } else {
    func <- call[[2]]  # This is the function (e.g., rnorm)
    args <- as.list(call[-(1:2)])  # These are the arguments (e.g., 1)
    func_call <- as.call(append(list(func), args))
    # Check for package prefix
    if (is.call(func) && func[[1]] == quote(`::`)) {
      func <- func[[3]]  # Get the actual function name (e.g., rnorm)
    }
  }

  if (is.call(func) || is.name(func)) {
    if (is.name(func))
      .functionName <- format(func)
    fun <- if (is.null(func_full)) func else func_full
    if (is.name(fun)) {
      infixes <- c("+", "-", "*", "/", "==", "!=", "<", ">", "<=", ">=", "&&", "||")
      areInfixes <- any(fun == infixes)
      if (!any(areInfixes)) {
        fun <- parse(text = fun)
      }
    }
    func <- eval(fun, envir = .callingEnv)
  }

  # deal with defunct arguments
  if (usesDots) { # any defunct argument will show up in the usesDots; need to keep them for defunct fn
    argsSupplied <- unique(names(call))[-1]
  } else {
    argsSupplied <- names(args)
  }
  defunct(setdiff(argsSupplied, formalArgs(func))) # pull the plug if args are defunct, and not used in FUN
  argsRm <- names(args) %in% setdiff(names(.formalsCache), names(formals(func)))
  if (any(argsRm %in% TRUE))
    args <- args[!argsRm %in% TRUE]

  # build new call from func and args; both must be correct by here
  new_call <- as.call(append(list(func), args))
  # This matches call on the FUN, not a duplicate of matchCall2
  matched_call <- match_call_primitive(func, new_call, expand.dots = TRUE, envir = .callingEnv)

  if (isSquiggly) {
    FUNcaptured <- recursiveEvalNamesOnly(matched_call, envir = .callingEnv) # deals with e.g., stats::rnorm, b$fun, b[[fun]]
    args <- as.list(FUNcaptured[-1])
  } else {
    args <- as.list(matched_call)[-1]
    args <- evaluate_args(args, envir = .callingEnv)
  }

  combined_args <- combine_clean_args(func, args, .objects = NULL, .callingEnv)

  # Check for arguments that are in both Cache and the FUN
  matched_call <- checkOverlappingArgs(call, combined_args, dotsCaptured = args,
                                       functionName = "outer", matched_call, whichCache = "cache2")

  if (is.null(func_call)) func_call <- new_call
  func_call2 <- as.call(c(func_call[[1]], args))
  attr(matched_call, ".Cache")$func_call <- func_call2
  attr(matched_call, ".Cache")$args_w_defaults <- combined_args
  attr(matched_call, ".Cache")$method <- func
  attr(matched_call, ".Cache")$.functionName <- .functionName


  return(matched_call)
}

evaluate_args <- function(args, envir) {
  lapply(args, function(arg) {
    if (is.call(arg)) {
      arg <- tryCatch(eval(arg, envir = envir), error = function(err) {
        # If it's a call that cannot be evaluated, evaluate recursively
        fail <- "fail"
        newPossArgMinus1 <- tryCatch(evaluate_args(as.list(arg[-1]), envir), error = function(err) {
          fail
        })
        if (!identical(newPossArgMinus1, fail)) {
          arg <- as.call(c(arg, as.list(newPossArgMinus1[-1])))
        }
        arg
      })
    } else if (is.symbol(arg)) {
      # If it's a symbol, evaluate it in the specified environment
      arg <- eval(arg, envir)
    }
    return(arg)
  })
}

check_and_get_cached_copy <- function(cache_key, cachePaths, cache_file, functionName,
                                      func, useCache, useCloud, cloudFolderID, gdriveLs,
                                      full_call, outputObjects,
                                      cacheSaveFormat = getOption("reproducible.cacheSaveFormat"),
                                      drv, conn, envir = parent.frame(), verbose) {
  # Check if the result is already cached
  connOrig <- conn
  conns <- conn
  if (!is.null(conn) && !is.list(conns)) {
    conns <- list(conn)
    names(conns) <- cachePaths
  }

  for (cachePath in cachePaths) {
    cache_file <- CacheStoredFile(cachePath, cache_key, cacheSaveFormat = cacheSaveFormat)
    cacheFileExists <- file.exists(cache_file) # could be length >1
    if (useDBI()) {
      inReposPoss <- searchInRepos(cachePath,
                                   outputHash = cache_key,
                                   drv = drv, conn = conns[[cachePath]]
      )
      if (cachePath == cachePaths[[1]] || NROW(inReposPoss$isInRepo)) {
        # keep important parts if it is first one, or if it has the object in the cacheRepo
        # inRepos <- inReposPoss
        conn <- conns[[cachePath]] # keep it as a list so places where it needs the name work
        if (is.null(connOrig)) # don't disconnect if conn was user passed
          # if this is >1st cachePath, then the db will already be disconnected; suppressWarnings
          on.exit2(suppressWarnings(DBI::dbDisconnect(conn)), envir = envir)

        shownCache <- inReposPoss$fullCacheTableForObj
        if (NROW(inReposPoss$isInRepo)) {
          break
        }
        if (cachePath == tail(cachePaths, 1)) { # if it is the last or only cachePath, then end
          ret <- .returnNothing
          attr(ret, ".Cache")$conn <- conn
          return(invisible(ret))
        }

        # this disconnect won't happen if user passed just one conn because already returned/break from this loop
        DBI::dbDisconnect(conn) # try next cachePath -- disconnect previous;
      }
    } else {
      shownCache <- NULL
      if (isTRUE(cacheFileExists))
        break
    }
  }

  # Check if it was saved with other CacheSaveFormat
  changedSaveFormat <- FALSE
  if (sum(cacheFileExists) == 0) { # if it doesn't exist; could be changed backend or not in Cache
    sameCacheID <- checkSameCacheId(cache_file)
    if (length(sameCacheID) > 0) {
      changedSaveFormat <- TRUE
      cacheFileExists <- TRUE
      cache_file_orig <- cache_file
      cache_file <- file.path(dirname(cache_file), sameCacheID)
    }
  }

  if (sum(cacheFileExists)) {
    output <- loadFromDiskOrMemoise(fromMemoise = FALSE, useCache, useCloud,
                                    cloudFolderID = cloudFolderID, gdriveLs = gdriveLs,
                                    cachePath = cachePath,
                                    cache_key, functionName, cache_file = cache_file,
                                    changedSaveFormat = changedSaveFormat, sameCacheID,
                                    cache_file_orig, func, shownCache = shownCache,
                                    full_call = full_call,
                                    outputObjects = outputObjects,
                                    cacheSaveFormat = cacheSaveFormat,
                                    drv = drv, conn = conn, verbose = verbose)
    return(output)

  }
  invisible(.returnNothing)
}

.returnNothing <- ".nothing"

combine_clean_args <- function(FUN, args, .objects, .callingEnv) {
  # has to be after match.call --> relies on name matched arguments
  defaults <- get_function_defaults(eval(FUN, .callingEnv))
  combined_args <- reorder_arguments(defaults, args)
  empties <- vapply(combined_args, function(ca) if (is.symbol(ca)) capture.output(ca) else "Normal", character(1))
  empties <- !nzchar(empties)
  if (isTRUE(any(empties)))
    combined_args <- combined_args[!empties]

  # Process the .objects argument using the helper function
  if (!is.null(.objects)) {
    combined_args <- filter_objects(combined_args, .objects)
  }

  combined_args
}

metadata_update <- function(outputToSave, metadata, cache_key) {
  userTagsExtra <- attr(outputToSave, "tags") # .wrap may have added tags
  userTagsExtra <- grep("cacheId:", userTagsExtra, invert = TRUE, value = TRUE) # don't add cacheId to tagKey
  if (!is.null(userTagsExtra) && length(userTagsExtra) > 0) {
    ut <- strsplitOnlySingleColon(userTagsExtra)
    # ut <- strsplit(userTagsExtra, split = ":")
    ll <- lapply(ut, tail, 1)
    names(ll) <- lapply(ut, head, 1)
    userTagsList <- ll
    metadata <- rbindlist(list(metadata, userTagsListToDT(cache_key, userTagsList)))
  }
  metadata
}

check_and_get_memoised_copy <- function(cache_key, cachePaths, functionName, func,
                                        useCache, useCloud, cloudFolderID, gdriveLs,
                                        full_call, outputObjects,
                                        cacheSaveFormat = getOption("reproducible.cacheSaveFormat"),
                                        drv, conn, verbose) {
  if (getOption("reproducible.useMemoise", FALSE)) {
    for (cachePath in cachePaths) {
      cache_key_in_memoiseEnv <- exists(cache_key, envir = memoiseEnv(cachePath), inherits = FALSE)
      if (isTRUE(cache_key_in_memoiseEnv))
        break
    }

    if (cache_key_in_memoiseEnv) {
      output <- loadFromDiskOrMemoise(fromMemoise = TRUE, useCache = useCache, useCloud = useCloud,
                                      cloudFolderID = cloudFolderID, gdriveLs = gdriveLs,
                                      cachePath = cachePath, cache_key = cache_key,
                                      functionName = functionName, func = func,
                                      full_call = full_call,
                                      changedSaveFormat = FALSE,
                                      outputObjects = outputObjects,
                                      cacheSaveFormat = cacheSaveFormat,
                                      drv = drv, conn = conn, verbose = verbose,
                                      )
      return(output)
    }
  } else {
    # If useMemoise gets turned off, it needs to be emptied or there will be stale entries
    me <- memoiseEnv(cachePaths[[1]])
    le <- ls(me)
    if (length(le))
      rm(list = le, envir = me)
  }
  return(invisible(.returnNothing))
}

# Helper function to filter arguments based on .objects
filter_objects <- function(evaluated_args, .objects) {
  list_or_env_arg <- NULL
  for (name in names(evaluated_args)) {
    if (is.list(evaluated_args[[name]]) || is.environment(evaluated_args[[name]])) {
      list_or_env_arg <- name
      break
    }
  }

  if (!is.null(list_or_env_arg)) {
    actual_list <- evaluated_args[[list_or_env_arg]]
    filtered_elements <- actual_list[.objects]
    filtered_list <- actual_list
    filtered_list[names(filtered_list) %in% .objects] <- filtered_elements
    filtered_list <- filtered_list[.objects]
    evaluated_args[[list_or_env_arg]] <- filtered_list
  }

  return(evaluated_args)
}

# Function to normalize the call to handle `do.call`
undoDoCall <- function(call, .callingEnv) {
  if (is.call(call) && all(as.character(call[[1]]) == "do.call")) {
    func <- call[[2]]
    args <- call[[3]]

    if (isTRUE(is.call(args)) && isTRUE(as.character(args[[1]]) == "list")) {
      args <- as.list(args[-1])
    }
    if (is.name(args))
      args <- recursiveEvalNamesOnly(args, envir = .callingEnv)

  } else {
    func <- call[[1]]
    args <- as.list(call[-1])
  }
  return(as.call2(func, args))
}

# Helper function to get function defaults
get_function_defaults <- function(func) {
  formals_list <- formals(func)
  return(as.list(formals_list))
}

# Helper function to reorder arguments based on formal arguments, combining defaults and user args
reorder_arguments <- function(formals, args) {
  # Combine defaults and args: user args override defaults
  combined_args <- modifyList(formals, args, keep.null = TRUE)

  # Preserve the order of the formals
  ordered_args <- combined_args[union(names(formals), names(combined_args))]

  return(ordered_args)
}

match_call_primitive <- function(definition = sys.function(sys.parent()),
                                 call = sys.call(sys.parent()),
                                 expand.dots = TRUE,
                                 envir = parent.frame()) {
  # Check if the function is a primitive infix operator
  if (is.primitive(definition)) {
    # For infix operators like +, -, *, etc., they are not called in the standard way
    infixes <- c(`+`, `-`, `*`, `/`, `==`, `!=`, `<`, `>`, `<=`, `>=`, `&&`, `||`)
    areInfixes <- vapply(infixes, function(i) identical(i, definition), FUN.VALUE = logical(1))

    if (isTRUE(any(areInfixes))) {
      # Handle infix operators by keeping the call intact
      return(call)
    }

    # For other primitives, match as best as possible
    args <- as.list(call)[-1]  # remove the function name
    if (expand.dots) {
      args <- lapply(args, eval, envir = envir)
    }
    # Construct the matched call manually for primitive
    matched <- as.call(c(definition, args))
    return(matched)
  } else {
    # Non-primitive function: fall back to regular match.call
    return(base::match.call(definition = definition,
                            call = call,
                            expand.dots = expand.dots,
                            envir = envir))
  }
}


cache_Id_Identical <- function(metadata, cachePaths, cache_key,
                               cacheSaveFormat = getOption("reproducible.cacheSaveFormat")) {
  linkToCacheId <- NULL
  os <- metadata$tagValue[metadata$tagKey == "object.size"]

  skipPreDigest <- startsWith(cache_key, .txtPreDigest)

  if (!identical(os, "NA") && skipPreDigest %in% FALSE) {
    if (isTRUE(as.numeric(os) > .objectSizeMinForBig)) {
      for (cachePath in cachePaths) {
        allCache <- showCache(x = cachePath, verbose = -2)
        if (NROW(allCache)) {
          resultHash <- metadata$tagValue[metadata$tagKey == "resultHash"]
          alreadyExists <- allCache[allCache$tagKey == "resultHash" &
                                      allCache[[.cacheTableTagColName()]] %in% resultHash &
                                      allCache[[.cacheTableHashColName()]] != cache_key]
          if (NROW(alreadyExists)) {
            linkToCacheId <- alreadyExists[["cacheId"]][[1]]
          }
        }
      }
    }
  }
  if (!is.null(linkToCacheId)) linkToCacheId <- CacheStoredFile(cachePath, linkToCacheId, cacheSaveFormat = cacheSaveFormat)
  linkToCacheId
}

metadata_define_preEval <- function(detailed_key, func_name, userTags,
                                    .objects, length, algo, quick, classOptions,
                                    timeEvaluateStart, timeCacheDigestStart) {

  elapsedTimeCacheDigest <- difftime(timeEvaluateStart, timeCacheDigestStart, units = "secs")

  useCloud <- FALSE

  df <- unlist(
    .unlistToCharacter(unname(detailed_key[-1]), getOption("reproducible.showSimilarDepth", 3))
  )
  pat <- "[[:digit:]]{1,5}$"
  didWeGainNumerics <- grep(names(df), pattern = pat)
  wouldBe <- gsub("", pattern = pat, names(df))
  dups <- which(duplicated(wouldBe))
  wasFirstsOfDups <- setdiff(didWeGainNumerics, dups)
  isTheDupAGainedNumeric <- wasFirstsOfDups %in% didWeGainNumerics
  if (any(isTheDupAGainedNumeric)) {
    changeThese <- c(wasFirstsOfDups, dups)
    names(df)[changeThese] <- wouldBe[changeThese]
  }

  tagKey <- paste0(names(df), ":", as.character(df))
  if (length(userTags)) {
    ut <- strsplitOnlySingleColon(userTags)
    # ut <- strsplit(userTags, split = ":")
    ll <- lapply(ut, tail, 1)
    strt <- lapply(ut, function(x) x[-length(x)])
    utagLabel <- rep("userTags", length(ll))#lapply(ut, head, 1)
    hasLabel <- lengths(strt) > 0
    if (any(hasLabel)) {
      utagLabel[hasLabel] <- sapply(strt[hasLabel], function(x) paste0(x, collapse = ":"))
    }
    names(ll) <- utagLabel
    userTags <- ll
  }
  userTagsList <- c(
    list(func_name) |> setNames(nm = .cacheTagsFirstGroup[1]),
    userTags,
    list(sysTimeForCacheToChar()) |> setNames(nm = .cacheTagsFirstGroup[3]),
    list(isTRUE(useCloud)) |> setNames(nm = .cacheTagsFirstGroup[4]),
    list(format(elapsedTimeCacheDigest, units = "secs")) |> setNames(nm = .cacheTagsFirstGroup[5]),
    list(tagKey) |> setNames(nm = .cacheTagsFirstGroup[6])
  )
  names(userTagsList)[1] <- "function"
  cache_key <- detailed_key$key
  metadata <- userTagsListToDT(cache_key, userTagsList)
  return(metadata)
}

metadata_define_postEval <- function(metadata, cacheId, outputToSave, userTags,
                                     .objects, length, algo, quick, classOptions,
                                     elapsedTimeFUN) {
  objSize <- NA
  if (getOption("reproducible.objSize", TRUE)) {
    hasPointer <- usesPointer(outputToSave)
    if (any(unlist(hasPointer))) {
      os <- objSize(outputToSave, recursive = TRUE)
    } else {
      os <- objSize(outputToSave)
    }
    objSize <- sum(os)
  }

  resultHash <- ""
  if (isTRUE(objSize > .objectSizeMinForBig)) {
    resultHash <- CacheDigest(outputToSave,
                              .objects = .objects,
                              length = length, algo = algo, quick = quick,
                              classOptions = classOptions, calledFrom = "Cache"
    )$outputHash
  }
  fns <- Filenames(outputToSave)
  # tagsFromDefaults <- .cacheTagsDefault
  # .cacheTagsSecondGroup <- c("class", "object.size", "fromDisk", "resultHash", "elapsedTimeFirstRun")

  userTagsList <- c(
    list(class(outputToSave)[1]) |> setNames(nm = .cacheTagsSecondGroup[1]),
    list(format(as.numeric(objSize))) |> setNames(nm = .cacheTagsSecondGroup[2]),
    list(isTRUE(any(nchar(fns) > 0))) |> setNames(nm = .cacheTagsSecondGroup[3]),
    list(resultHash) |> setNames(nm = .cacheTagsSecondGroup[4]),
    list(format(elapsedTimeFUN, units = "secs")) |> setNames(nm = .cacheTagsSecondGroup[5])
  )
  cache_key <- cacheId
  metadataNew <- userTagsListToDT(cache_key, userTagsList)
  metadata <- rbindlist(list(metadata, metadataNew))
  # attr(metadata, "tags")$objectSize <- objSize
  metadata
}

userTagsListToDT <- function(cache_key, userTagsList) {
  theChars <- vapply(userTagsList, function(x) is.character(x) | is.logical(x), logical(1))
  if (any(!theChars)) {
    for (tc in which(!theChars))
      userTagsList[[tc]] <- tryCatch(format(userTagsList[[tc]]), error = function(u) as.character())
  }
  userTagsList <- utils::stack(userTagsList)
  metadataDT(cacheId = cache_key, tagKey = userTagsList$ind, tagValue = userTagsList$values)
}

as.call2 <- function(func, args) {
  as.call(c(as.name(deparse(func)), args))
}

skipCache <- function(FUN, ..., usesDots, functionName, useCache, verbose, .callingEnv) {
  .message$useCacheIsFALSE(.pkgEnv$.reproEnv2$nestLevel - 1, # original Cache counted differently; use -1 here
                           functionName = functionName, useCache = useCache, verbose = verbose)
  if (isTRUE(usesDots)) {
    FUN(...)
  } else {
    eval(FUN, envir = .callingEnv)
  }
}

clearCacheOverwrite <- function(cachePath, cache_key, functionName, drv, conn, verbose) {
  clearCache(x = cachePath, cacheId = cache_key, ask = FALSE, conn = conn, drv = drv, verbose = verbose - 1)
  .message$overwriting(functionName, type = "function", verbose)
}

sysTimeForCacheToChar <- function(digits = 5)
  format(Sys.time(), digits = digits)


setupCacheNesting <- function(userTags, useCache, envir = parent.frame(1)) {
  if (!exists(".reproEnv2", envir = .pkgEnv)) {
    .pkgEnv$.reproEnv2 <- new.env(parent = asNamespace("reproducible"))
    .pkgEnv$.reproEnv2$userTags <- userTags
    .pkgEnv$.reproEnv2$nestLevel <- 1
    .pkgEnv$.reproEnv2$useCache <- useCache
    on.exit2(rm(list = ".reproEnv2", envir = .pkgEnv), envir = envir)
  } else {
    userTagsOld <- .pkgEnv$.reproEnv2$userTags
    allUT1 <- c(userTagsOld, userTags)

    hasColon <- grepl(.txtGrepStrSplitSingleColon, allUT1, perl = TRUE)
    if (isTRUE(any(!hasColon)))
      allUT1[!hasColon] <- paste0("userTags:", allUT1[!hasColon])

    if (!is.null(allUT1)) {
      allUT2 <- allUT1[!duplicated(sapply(strsplitOnlySingleColon(allUT1), tail, 1))]

      splitted <- strsplitOnlySingleColon(allUT2)
      # firstPart <- sapply(strsplitOnlySingleColon(allUT2), function(x) x[[2]])
      # allUT2 <- allUT2[order(firstPart)]

      allUT2 <- sapply(
        reorder_by_first_element(splitted), function(x) paste0(x[[1]], ":", x[[2]])
      )

      userTags <- allUT2
      .pkgEnv$.reproEnv2$userTags <- userTags
      nestLevelOld <- .pkgEnv$.reproEnv2$nestLevel
      .pkgEnv$.reproEnv2$nestLevel <- nestLevelOld + 1
      on.exit2({
        .pkgEnv$.reproEnv2$nestLevel <- nestLevelOld
        .pkgEnv$.reproEnv2$userTags <- userTagsOld
      }, envir = envir)
    }
  }
  userTags
}

appendNestedTags <- function(...) {
  nams <- ...names()
  vals <- list(...)
  tags <- paste0(nams, ":", as.character(vals))
  .pkgEnv$.reproEnv2$userTags <- c(.pkgEnv$.reproEnv2$userTags, tags)
  return(invisible(NULL))
}

.addTagsRepoAccessedTime <- function(cache_key, cachePath = cachePath,
                                     cacheSaveFormat = getOption("reproducible.cacheSaveFormat")) {
  .addTagsRepo(cacheId = cache_key, tagKey = "accessed", tagValue = sysTimeForCacheToChar()
               , cacheSaveFormat = cacheSaveFormat, cachePath = cachePath)
}

callIsQuote <- function(call) {
  if (length(call$FUN) > 1) # just a function
    if (identical(call$FUN[[1]], quote(quote))) {
      call$FUN <- as.list(call$FUN)[[-1]] # unquote it
    }
  call
}

releaseLockFile <- function(locked) {
  lockFile <- locked[[2]]
  filelock::unlock(locked)
  if (file.exists(lockFile)) {
    unlink(lockFile)
  }
}

lockFile <- function(cachePath, cache_key, envir = parent.frame(),
                     verbose = getOption("reproducible.verbose")) {
  if (!useDBI()) {
    csd <- CacheStorageDir(cachePath)
    if (!any(dir.exists(csd)))
      lapply(csd, dir.create, showWarnings = FALSE, recursive = TRUE)
    lockFile <- file.path(csd, paste0(cache_key, suffixLockFile()))
    first <- TRUE
    tryCatch({
      while(!exists("locked", inherits = FALSE) ||
            tryCatch(isTRUE(is(locked, "try-error")), error = function(e) {TRUE})) {
        setTimeLimit(elapsed = 3)
        on.exit(setTimeLimit(elapsed = Inf))
        locked <- try(filelock::lock(lockFile), silent = TRUE)
        stillLocked <- tryCatch(isTRUE(any(is(locked, "try-error"))), error = function(err) {TRUE})
        if (stillLocked && isTRUE(first)) {
          first <- FALSE
          messageCache("The cache file (", lockFile,") is locked due to a concurrent process; waiting... ",
                       "\nIf there is no concurrent process (i.e., no parallelism), ",
                       "delete that lockfile", verbose = verbose + 2)
        }
      }}, silent = TRUE)
    # , error = function(e) {if (any(grepl("reached elapsed time limit", e$message)))
    #   invokeRestart("muffleError")
    # }
    # )
    if (first %in% FALSE) {
      messageCache("  ... ", lockFile, " released, continuing ... ", verbose = verbose + 2)
    }
    # locked <- evalWithTimeout(, timeout = 1, onTimeout = "error")

    on.exit2(releaseLockFile(locked), envir = envir)
    locked
  }
}

#' @importFrom data.table setorderv setcolorder
showSimilar <- function(cachePath, metadata, .functionName, userTags, useCache,
                        # cacheSaveFormat = getOption("reproducible.cacheSaveFormat"),
                        drv, conn, verbose) {
  devMode <- isDevMode(useCache, userTags)  # don't use devMode if no userTags
  shownCache <- showCache(cachePath, Function = .functionName, userTags = userTags, verbose = verbose - 2)
  # functionByDigest <- metadata[tagKey %in% "preDigest" & startsWith(tagValue, ".FUN")]$tagValue
  # shownCache <- shownCache[tagKey %in% "preDigest" & tagValue %in% functionByDigest]
  setorderv(shownCache, "createdDate", order = -1)
  # shownCache <- shownCache[tagKey != "outerFunction"] # doesn't matter what outerFunctions do, if all others are same
  # metadata <- metadata[tagKey != "outerFunction"]
  onKey <- c("tagKey", "tagValue")

  # if (NROW(shownCache)) {
  #   if (!is.null(userTags)) { # userTags are as "strong as" functionName
  #     userTags2a <- gsub("^.*:", "", userTags)
  #     userTags2b <- gsub(":.*$", "", userTags)
  #     noColons <- userTags2a == userTags2b
  #     if (any(noColons)) {
  #       userTags2b[noColons] <- "userTags"
  #     }
  #     userTags2b <- ifelse(!nzchar(userTags2b), "userTags", userTags2b)
  #     userTagsAsDT <- data.table(tagKey = userTags2b, tagValue = userTags2a)
  #     userTagsAsDT <- unique(userTagsAsDT)
  #     # identify only those items that match the userTags
  #     scMatch <- shownCache[userTagsAsDT, # .(length(unique(tagKey)) == length(userTags)),
  #                           on = onKey, # by = "cacheId",
  #                           nomatch = FALSE]# [V1 %in% TRUE]
  #     shownCache <- shownCache[cacheId %in% unique(scMatch[["cacheId"]]), on = "cacheId"]
  #   }
  # }

  if (NROW(shownCache)) {
    userTagsMess <- if (!is.null(userTags)) {
      paste0(.message$BecauseOfA,
             "with user supplied tags: '",
             paste(userTags, collapse = ", "), "' "
      )
    }

    rmTagKeys <- "otherFunction|elapsedTime|accessed"
    # shownCache <- shownCache[tagKey %in% c(metadata$tagKey)][grep(x = tagKey, rmTagKeys, invert = TRUE)]
    shownCache <- shownCache[grep(x = tagKey, rmTagKeys, invert = TRUE)]
    metadataSmall <- metadata[grep(x = tagKey, rmTagKeys, invert = TRUE)]
    # cacheIdOfSimilar
    # Can only compare on tagKeys that are *not yet* in the metadata; e.g., object.size may
    #   not be there, so don't know if it is different
    similarFull <- unique(shownCache[tagKey %in% unique(c(metadata$tagKey))], by = .dtFileMainCols)
    # similarFull <- unique(shownCache, by = .dtFileMainCols)
    # metadataSmall <- metadataSmall[tagKey %in% unique(c(similarFull$tagKey))]
    similarFullList <- split(similarFull, by = "cacheId")
    notInThisCall <- lapply(similarFullList, function(x) x[!metadataSmall, on = onKey])
    notInSC <- lapply(similarFullList, function(x) metadataSmall[!x, on = onKey])
    # notInThisCall <- similarFull[!metadataSmall, on = onKey]
    # notInSC <- metadataSmall[!similarFull, on = onKey]
    # notInSC[grep("userTags", tagKey ), tagValue := paste0(tagKey, ":", tagValue)]
    # notInThisCall[grep("userTags", tagKey ), tagValue := paste0(tagKey, ":", tagValue)]
    notInThisCall0 <- lapply(notInThisCall, function(x) x[grep("userTags", tagKey ), tagValue := paste0(tagKey, ":", tagValue)])
    notInSC0 <- lapply(notInSC, function(x) x[grep("userTags", tagKey ), tagValue := paste0(tagKey, ":", tagValue)])

    similar <- notInThisCall
    other <- logical()
    if (NROW(similar) == 0) {
      other <- vapply(strsplitOnlySingleColon(similarFull$tagValue, split = "\\:"),
                      function(x) ifelse(length(x) == 2, x[[2]], NA_character_), FUN.VALUE = character(1))
      otherLabels <- vapply(strsplitOnlySingleColon(similarFull$tagValue, split = "\\:"),
                      function(x) ifelse(length(x) == 2, x[[1]], NA_character_), FUN.VALUE = character(1))
      whOther <- other == "other"
      # similar <- similarFull[whOther %in% TRUE]
      cacheIdOfSimilar <- unique(similarFull$cacheId)
      simFun <- list(funName = unique(shownCache$tagValue[shownCache$tagKey == "function"]))
      messageCache("Cache of ", .messageFunctionFn(simFun), " differs from", verbose = verbose)
      sameNames <- simFun$funName %in% .functionName
      fnTxt <- paste0(if (!is.null(.functionName))
        paste0("of '", .messageFunctionFn(.functionName), "' ") else "call ")

      if (!all(sameNames)) {
        fnTxt <- paste0("(whose function name(s) was/were '", .messageFunctionFn(paste(simFun$funName, collapse = "', '")), "')")
      }
      messageCache(paste0(.message$BecauseOfA, "the next closest cacheId(s) ",
                          paste(cacheIdOfSimilar, collapse = ", "), " ",
                          fnTxt, userTagsMess,
                          collapse = "\n"
      ), appendLF = TRUE, verbose = verbose)
      messageCache("...possible, unknown, differences in a nested list ",
                   "that is deeper than ", getOption("reproducible.showSimilarDepth", 3), " in ",
                   paste(collapse = ", ", as.character(otherLabels[whOther %in% TRUE])),
                   verbose = verbose
      )

    }

    # This is for dryRun: i.e., there is a cacheId, but no difference in metadata
    # isIdentical0 <- vapply(similar, function(x) NROW(x) == 0, FUN.VALUE = logical(1))
    isIdentical1 <- vapply(notInSC0, function(x) NROW(x) == 0, FUN.VALUE = logical(1))
    isIdentical2 <- vapply(notInThisCall0, function(x) NROW(x) == 0, FUN.VALUE = logical(1))
    isIdentical <- isIdentical1 & isIdentical2
    if (any(isIdentical)) {
      messageCache("Call is identical to ", paste(names(similar)[isIdentical], collapse = ", "),
                   " and would return that object")
      return(NULL)
    }

    if (NROW(similar)) {

      notInSCLen <- vapply(notInSC0, NROW, FUN.VALUE = integer(1))
      notInThisCallLen <- vapply(notInThisCall0, NROW, FUN.VALUE = integer(1))
      numSimilars <- length(notInSCLen)

      # First pass -- this will shrink probably down a lot
      diffs <- mapply(n = notInSCLen, m = notInThisCallLen, function(n, m) n + m, SIMPLIFY = TRUE)
      minNumDiffs <- min(diffs)
      smallestDiffs <- which(diffs == minNumDiffs)
      notInSC2 <- notInSC0[smallestDiffs]
      notInThisCall2 <- notInThisCall0[smallestDiffs]

      notInSC4 <- lapply(notInSC2, function(x) {
        x <- createSimilar(x, verbose = verbose, devMode = devMode, .functionName = .functionName)
        data.table::setnames(x, old = c("valueInCache", "cacheIdInCache"),
                             new = c("valueThisCall", "cacheIdOfThisCall"),
                             skip_absent = TRUE)
        })
      notInThisCall3 <- lapply(notInThisCall2, function(x) {
        ss <- createSimilar(x, verbose = verbose, devMode = devMode, .functionName = .functionName)
        if (isTRUE(any("lsStr" %in% colnames(ss))))
          set(ss, NULL, "lsStr", NULL)
        ss
        })
      simi <- Map(n = names(notInThisCall3), function(n) {
        if (NROW(notInThisCall3[[n]]) || NROW(notInSC4[[n]])) {
          a <- notInSC4[[n]][notInThisCall3[[n]], on = "arg", allow.cartesian = TRUE]
          b <- notInThisCall3[[n]][notInSC4[[n]], on = "arg", allow.cartesian = TRUE]
          d <- unique(rbindlist(list(a, b), fill = TRUE))
        } else {
          d <- data.table(notInSC4[[n]], valueInCache = NA, cacheIdInCache = NA)
        }
        setcolorder(d, c("arg",
                         grep("InCache", value = TRUE, colnames(d)),
                         grep("ThisCall", value = TRUE, colnames(d))))
        d
      })

      # Second pass -- this will be different if there were no new arguments; just arg value changes
      diffs <- mapply(x = simi, function(x) NROW(x), SIMPLIFY = TRUE)
      minNumDiffs <- min(diffs)
      smallestDiffs <- which(diffs == minNumDiffs)
      numSmallest <- length(smallestDiffs)
      simi <- simi[smallestDiffs]

      messageCache("There are ", numSimilars,
                   " calls with same fn (", .messageFunctionFn(.functionName), ") in the Cache repository.",
                   verbose = verbose * !devMode)
      if (identical(numSimilars, 1L)) {
        messageCache("It has ", minNumDiffs, " differences", verbose = verbose * !devMode)
      } else {
        messageCache("With fewest differences (", minNumDiffs, "), there ", isAre(v = numSmallest),
                     " ", numSmallest,
                     " similar calls in the Cache repository.", verbose = verbose * !devMode)
      }
      # data.table::setnames(notInSC2, old = c("valueInCache", "cacheIdInCache"), c("valueThisCall", "cacheIdOfThisCall"),
      #                      skip_absent = TRUE)
      #
      # # a <- notInSC2[notInMetadata2, on = "arg"]
      # # b <- notInMetadata2[notInSC2, on = "arg"]
      # # simi <- rbindlist(list(a, b), fill = TRUE)
      #
      # simi <- lapply(split(notInMetadata2, by = "cacheIdInCache"), function(x) {
      #   a <- notInSC2[x, on = "arg"]
      #   b <- x[notInSC2, on = "arg"]
      #   d <- rbindlist(list(a, b), fill = TRUE)
      #   setcolorder(d, c("arg",
      #                    grep("InCache", value = TRUE, colnames(d)),
      #                    grep("ThisCall", value = TRUE, colnames(d))))
      #   d
      # })

      # notInMetadata2 <- createSimilar(notInThisCall, notInSC2, cacheIdInsimi = unique(simi[["cacheIdInCache"]]))
      # simi22 <- createMerged(notInSC2, notInMetadata2)

#
#       noMergeCols <- c("outerFunction", "userTags")
#       simi1NoMergeCols <- simi[arg %in% noMergeCols]
#       simi2NoMergeCols <- simi2[arg %in% noMergeCols]
#       simi1B <- simi1NoMergeCols[!simi2NoMergeCols, on = c("arg", "valueInCache" = "valueThisCall")]
#       simi2B <- simi2NoMergeCols[!simi1NoMergeCols, on = c("arg", "valueThisCall" = "valueInCache")]
#       simi1MergeCols <- simi[!arg %in% noMergeCols]
#       simi2MergeCols <- simi2[!arg %in% noMergeCols]
#
#
#       simiMergeCols <- data.table(simi1MergeCols, simi2MergeCols[match(simi1MergeCols$arg, arg), -"arg"])
#       simi <- rbindlist(list(simi1NoMergeCols, simi2NoMergeCols, simiMergeCols), fill = TRUE)
#       setorderv(simi, c("cacheIdInCache", "arg"))
      # simi <- simi[simi2, on = c("arg"), allow.cartesian = TRUE] # there can be duplicate args

      if (isDevMode(useCache, userTags)) {
        messageCache("------ devMode -------", verbose = verbose)
        messageCache("Previous call(s) exist in the cache with identical userTags (",
                     paste0(userTags, collapse = ", "), ")", verbose = verbose)
        messageCache("This call to cache will replace entry with cacheId(s): ",
                     paste0(simi[["cacheId"]], collapse = ", "), verbose = verbose)
        cacheIdsToClear <- unique(names(simi))
        # cacheIdsToClear <- paste0("^", unique(names(simi)), "$", collapse = "|")
        clearCache(cachePath, cacheId = cacheIdsToClear, ask = FALSE,  drv = drv, conn = conn, verbose = verbose - 2)
        # clearCache(cachePath, userTags = cacheIdsToClear, ask = FALSE, drv = drv, conn = conn, verbose = verbose - 2)
      }
      messageCache("with different elements (most recent at top):", verbose = verbose)
      # don't add a prefix if there is no `sim` in the stack
      prefix <- if (identical(.GlobalEnv, whereInStack("sim"))) "" else .message$NoPrefix
      # if (exists("aaaa", envir = .GlobalEnv)) browser()
      messageCache(.message$dashes, prefix)
      lala <- Map(si = simi, nam = names(simi), function(si, nam) {
        messageCache(paste0("Compared to cacheId: ", nam, prefix), verbose = verbose)
        if (verbose > 0) {
          oo <- capture.output(si)
          fn <- cliCol(getOption("reproducible.messageColourCache"))
          # cat(fn(oo), sep = "\n")
          # nc <- NCOL(si)
          # set(si, 1, NCOL(si), paste0(si[1, ..nc], .message$NoPrefix))
          # messageDF(si, colour = getOption("reproducible.messageColourCache"))
          # messageDF(paste0(as.character(fn(oo)), .message$NoPrefix))
          oo <- paddDFInitial(oo, rows = 1:2, .spaceTmpChar, colour = getOption("reproducible.messageColourCache"))
          messageColoured(paste0(paste(oo, collapse = "\n"), .message$NoPrefix),
                          colour = getOption("reproducible.messageColourCache"))
          # messageDF(fn(oo))
        }
        messageCache(.message$dashes, prefix)
      })

      messageCache("------ devMode -------", verbose = verbose * devMode)

    }
  } else {
    messageCache(.message$noSimilarCacheTxt(.functionName), verbose = verbose)
  }
}

CacheDBFileCheckAndCreate <- function(cachePath, drv = NULL, conn = NULL, verbose) {

  convertDBbackendIfIncorrect(cachePath, drv, conn, verbose = verbose)

  dbfile <- CacheDBFile(cachePath, drv = drv, conn = conn)
  if (isTRUE(!file.exists(dbfile[1])))
    file.create(dbfile[1])
  if (!useDBI()) {
    oldDBFile <- file.path(cachePath, "cache.db")
    if (isTRUE(file.exists(oldDBFile)))
      file.remove(oldDBFile)
  }
  dbfile
}

convertCallWithSquigglyBraces <- function(call, usesDots) {
  if (length(call) == 2) {
    if (length(call[[-1]]) > 2)
      stop("Cache does not yet support multi-step caching unless using the pipe (|>)")
    call <- as.call(c(call[[1]], call[[-1]][[-1]]))
  } else if ((length(call) > 2) && isFALSE(usesDots)) {
    call <- as.call(c(call[[1]], FUN = as.list(call[-1])[[1]][[-1]], as.list(call[-1])[-1]))
  }
  call
}

wrapSaveToCache <- function(outputFromEvaluate, metadata, cache_key, cachePath, # userTags,
                            preDigest, .functionName, outputObjects,
                            cacheSaveFormat = getOption("reproducible.cacheSaveFormat"),
                            drv, conn, verbose) {
  cacheIdIdentical <- cache_Id_Identical(metadata, cachePath, cache_key, cacheSaveFormat = cacheSaveFormat)
  linkToCacheId <- if (!is.null(cacheIdIdentical)) filePathSansExt(basename(cacheIdIdentical))  else NULL
  outputToSave <- .wrap(outputFromEvaluate, cachePath = cachePath, preDigest = preDigest,
                        outputObjects = outputObjects,
                        cacheId = cache_key, verbose = verbose)
  metadata <- metadata_update(outputToSave, metadata, cache_key) # .wrap may have added tags
  userTags <- paste0(metadata$tagKey, ":", metadata$tagValue)
  fs <- saveToCache(cachePath = cachePath, # drv = NULL, conn = NULL,
                    obj = outputToSave, verbose = verbose, # cache_file[1],
                    userTags = userTags, linkToCacheId = linkToCacheId,
                    cacheSaveFormat = cacheSaveFormat,
                    drv = drv, conn = conn,
                    cacheId = cache_key)
  .message$Saved(cachePath, cache_key, functionName = .functionName,
                 cacheSaveFormat = cacheSaveFormat, verbose = verbose)
  return(metadata)
}

doSaveToCache <- function(outputFromEvaluate, metadata, cachePaths, func,
                          .objects, length, algo, quick, classOptions,
                          cache_file, userTags, .functionName, debugCache,
                          detailed_key, func_call, outputObjects,
                          useCloud, cloudFolderID, gdriveLs,
                          cacheSaveFormat = getOption("reproducible.cacheSaveFormat"),
                          drv, conn, useMemoise = getOption("reproducible.useMemoise", FALSE),
                          verbose, timeSaveStart, timeEvaluateStart) {

  # elapsedTimeFUN <- difftime(timeSaveStart, timeEvaluateStart, units = "secs")
  #
  # # update metadata with other elements including elapsedTime for evaluation
  # metadata <- metadata_define_postEval(metadata, detailed_key$key, outputFromEvaluate,
  #                                      userTags, .objects, length, algo, quick,
  #                                      classOptions, elapsedTimeFUN)

  # Can't save NULL with attributes
  if (is.null(outputFromEvaluate)) outputFromEvaluate <- "NULL"

  outputFromEvaluate <- addCacheAttr(outputFromEvaluate, .CacheIsNew = TRUE, detailed_key$key, func)

  metadata <- wrapSaveToCache(outputFromEvaluate, metadata, detailed_key$key, cachePaths[[1]],
                              # userTags = paste0(metadata$tagKey, ":", metadata$tagValue),
                              outputObjects = outputObjects,
                              preDigest = detailed_key$preDigest, .functionName,
                              cacheSaveFormat = cacheSaveFormat, drv, conn, verbose)

  # Memoize the outputFromEvaluate by saving it in RAM
  if (isTRUE(useMemoise)) {
    assign(detailed_key$key, outputFromEvaluate, envir = memoiseEnv(cachePaths[[1]]))
  }


  if (identical(outputFromEvaluate, "NULL")) outputFromEvaluate <- NULL

  if (isTRUE(!is.na(pmatch(debugCache, "complete"))))
    outputFromEvaluate <- .debugCache(outputFromEvaluate, detailed_key$preDigest, fullCall = func_call)

  if (cloudWrite(useCloud)) {
    cloudUploadFromCache(detailed_key$key %in% filePathSansExt(gdriveLs[["name"]]), detailed_key$key,
                         cachePaths[[1]], cloudFolderID = cloudFolderID, outputFromEvaluate, verbose = verbose)
  }
  outputFromEvaluate

}


# doDigest <- function(new_call, omitArgs, .cacheExtra, .functionName, .objects,
#                      length, algo, quick, classOptions, timeCacheDigestStart,
#                      cachePath, verbose) {
#   # Compile a list of elements to digest
#   toDigest <- attr(new_call, ".Cache")$args_w_defaults # not evaluated arguments
#
#   # Deal with .objects -- wait these are dealt with by `.robustDigest`
#   # toDigest <- rmDotObjectsInList(toDigest, .objects)
#   # .objects <- dotObjectsToNULLInList(toDigest, .objects) # if .objects used in previous, set to NULL here
#
#   toDigest$.FUN <- attr(new_call, ".Cache")$method
#   # Deal with omitArgs by removing elements from the toDigest list of objects to digest
#   if (!is.null(omitArgs)) {
#     if (any("FUN" %in% omitArgs))
#       omitArgs <- c(".FUN", omitArgs)
#     toDigest[omitArgs] <- NULL
#   }
#   # Deal with .cacheExtra by adding it to the list of objects to digest
#   if (!is.null(.cacheExtra))
#     toDigest <- append(toDigest, list(.cacheExtra = .cacheExtra))
#   detailed_key <- CacheDigest(toDigest,
#                               .functionName = .functionName,
#                               .objects = .objects,
#                               length = length, algo = algo, quick = quick,
#                               classOptions = classOptions,
#                               calledFrom = "Cache"
#   )
#   diTi <- difftime(Sys.time(), timeCacheDigestStart, units = "sec")
#   if (diTi > 5) {
#     messageCache("Object digesting for ", .messageFunctionFn(.functionName)," took: ", format(diTi, digits = 2))
#   }
#   verboseCacheMessage(detailed_key$preDigest, .functionName, timeCacheDigestStart, quick = quick,
#                    modifiedDots = toDigest, verbose = verbose, verboseLevel = 3)
#
#   names(detailed_key)[[1]] <- "key"
#   detailed_key
# }

#' Remove `quote` and determine if call uses `...`
#'
#' Minor cleaning up of the `FUN` and `...` to be used subsequently. This does only very minor
#' things as it is run even if `useCache = FALSE`, i.e., even if the `Cache` is skipped.
#'
#' @inheritParams Cache
#' @inheritParams base::match.call
#' @param envir2 Environment. The environment where `matchCall2` was called.
#' @return A named list with `call` (the original call, without `quote`),
#' `FUNorig`, the original value passed by user to `FUN`, and `usesDots` which
#' is a logical indicating whether the `...` are used.
matchCall2 <- function(definition, call, envir, envir2 = parent.frame(), FUN) {
  if (missing(FUN)) {
    stop(.message$CacheRequiresFUNtxt())
  } else {
    FUNcaptured <- substitute(FUN, env = envir2)
    # This matches call for Cache
    call <- match.call(definition, call = call, expand.dots = TRUE, envir = envir)
    # call <- callIsQuote(call) # stip `quote`
    FUNorig <- call$FUN

    usesDots <- sum(!nzchar(names(call))) > 1 || sum(!names(call) %in% .namesCacheFormals) > 2
  }
  list(call = call, FUNorig = FUNorig, usesDots = usesDots, FUNcaptured = FUNcaptured)
}

#' Harmonize all forms of call
#'
#' This will convert all known (imagined) calls so that they have the same canonical
#' format i.e., `rnorm(n = 1, mean = 0, sd = 1)`
#'
#' @param callList A named list with elements `call`, `usesDots` and `FUNorig`
#' @param .callingEnv The calling environment where `Cache` was called from
#' @param .functionName A possible function name. If omitted, then it will be deduced
#'   from the `callList` and may be inaccurate.
#' @return A named list. We illustrate with the example `rnorm(1)`. The named
#' list will have the original `callList` (`call` (the original call, without `quote`),
#' `FUNorig`, the original value passed by user to `FUN`, and `usesDots` which
#' is a logical indicating whether the `...` are used), and appended with `new_call`
#' (the harmonized call, with the function and arguments evaluated, e.g.,
#' `(function (n, mean = 0, sd = 1) .Call(C_rnorm, n, mean, sd))(1)`), `func_call`, the same harmonized call
#' with neither function nor arguments not evaluated (e.g., `rnorm(1)`), `func` which
#' will be function or method definition
#' `function (n, mean = 0, sd = 1) .Call(C_rnorm, n, mean, sd)`,
#' and `.functionName`, which will be the function name as a character string (`rnorm`)
#' either directly passed from the user's `.functionName` or deduced from the `func_call`.
harmonizeCall <- function(callList, .callingEnv, .functionName = NULL) {
  callList$call <- callIsQuote(callList$call) # stip `quote`

  isSquiggly <- isSquigglyCall(callList$FUNorig)
  # isSquiggly <- is(callList$FUNorig, "{")
  if (isTRUE(isSquiggly))
    callList$call <- convertCallWithSquigglyBraces(callList$call, callList$usesDots)
  new_call <- convertCallToCommonFormat(callList$call, callList$usesDots, isSquiggly, .callingEnv) # evaluated arguments
  func_call <- attr(new_call, ".Cache")$func_call         # not evaluated arguments
  .functionNamePoss <- attr(new_call, ".Cache")$.functionName
  func <- as.list(new_call)[[1]]

  if (!is.null(.functionName)) {
    dotFnGrep <- "\\.functionName"
    hasDotFNLogical <- grepl(dotFnGrep, .functionName)
    hasDotFN <- isTRUE(any(hasDotFNLogical) )
    if (hasDotFN)
      .functionName <- gsub(dotFnGrep, .functionNamePoss, .functionName)
  }

  # Try to identify the .functionName; if can't just use the matched call callList$FUNorig
  if (is.null(.functionName)) {
    if (!is.null(.functionNamePoss))
      .functionName <- .functionNamePoss
    else
      .functionName <- getFunctionName2(func_call)# as.character(normalized_FUN[[1]])
  }
  if (!isTRUE(any(nzchar(.functionName)))) {
    .functionName <- format(callList$FUNorig)
  }
  append(callList, list(new_call = new_call, func_call = func_call,
                           func = func, .functionName = .functionName))
}

cacheIdOverride <- function(cacheId, key, .functionName, verbose) {
  if  (identical(cacheId, "previous")) {
    cacheId <- getPreviousEntryInCache(.functionName, cacheId, verbose)
  } else {
    shownCache <- cacheIdCheckInCache(cacheId, calculatedCacheId = key, .functionName, verbose)
    # if (NROW(shownCache) == 0)
    #   cacheId <- NULL
  }
  cacheId
}

useCacheFromNested <- function(useCache) {
  isNested <- isTRUE(.pkgEnv$.reproEnv2$nestLevel > 1)
  if (isNested && isTRUE(useCache))
    useCache <- .pkgEnv$.reproEnv2$useCache
  useCacheDueToNumeric <- (is.numeric(useCache) && isTRUE(useCache < .pkgEnv$.reproEnv2$nestLevel))
  if (is.character(useCache)) {
    if (any(!is.na(pmatch(table = useCache, c("over", "dev"))))) {
      return(useCache)
    }
  }
  !(isFALSE(useCache) || useCache == 0 || isTRUE(useCacheDueToNumeric))
}

loadFromDiskOrMemoise <- function(fromMemoise = FALSE, useCache,
                                  useCloud, cloudFolderID = NULL, gdriveLs,
                                  cachePath, cache_key,
                                  functionName,
                                  cache_file = NULL, changedSaveFormat, sameCacheID,
                                  cache_file_orig, func, shownCache = NULL,
                                  full_call, outputObjects,
                                  cacheSaveFormat = getOption("reproducible.cacheSaveFormat"),
                                  drv, conn, verbose) {

  if (identical(useCache, "overwrite")) {
    clearCacheOverwrite(cachePath, cache_key, functionName, drv, conn, verbose)
    return(invisible(.returnNothing))
  } else {
    format <- if (missing(cache_file) || is.null(cache_file)) cacheSaveFormat else
      fileExt(cache_file)

    for (iii in 1:2) {
      fe <- CacheDBFileSingle(cachePath = cachePath, cacheId = cache_key, cacheSaveFormat = cacheSaveFormat)
      if (useDBI()) {
        rerun <- FALSE
      } else {
        feReally <- file.exists(fe)
        if (any(feReally %in% FALSE)) {
          formatNew <- formatCheck(cachePath, cache_key, format)
          if (!identical(formatNew, cacheSaveFormat)) {
            cacheSaveFormat <- formatNew
            next
          }

        }
        # usually happens when user had memoise on before, then turned it off, then turned it back on
        danglingMemoise <- (isFALSE(any(feReally)) && fromMemoise)
        if (isTRUE(danglingMemoise)) rm(list = cache_key, envir = memoiseEnv(cachePath))
        rerun <- (!isTRUE(any(feReally)) && !fromMemoise) || danglingMemoise
        break
      }
    }

    cacheSaveFormatFail <- FALSE
    if (is.null(shownCache)) {
      shownCache <- try(showCacheFast(cache_key, cachePath, dtFile = fe,
                                      # cacheSaveFormat = cacheSaveFormat,
                                      drv = drv, conn = conn),
                        silent = TRUE)
      if (is(shownCache, "try-error")) {
        if (isTRUE(any(grepl("format not detected", shownCache)))) {
          cacheSaveFormatFail <- TRUE
        } else { # e.g., change from qs to qs2
          cacheSaveFormatFail <- TRUE
        }
      }
    }

    if (isFALSE(cacheSaveFormatFail))
      .cacheMessageObjectToRetrieve(functionName, shownCache, cachePath,
                                    cacheId = cache_key, cacheSaveFormat = cacheSaveFormat, verbose = verbose)
    memoiseFail <- FALSE
    if (fromMemoise && !rerun) {
      output <- get(cache_key, envir = memoiseEnv(cachePath))
      # need to update the individual files in file-backed objects from the cache; can't use memoise
      outputTestIntegrity <- try(output[1], silent = TRUE)
      fns <- try(Filenames(output), silent = TRUE) # previous will only get some of the failures

      if (isTRUE(is(outputTestIntegrity, "try-error")) || isTRUE(is(fns, "try-error"))) {
        # Some objects, especially Rcpp objects can get stale; rerun if this is the case
        failMsgs <- "external pointer.+not valid|NULL value passed as symbol address"
        if (isTRUE(any(grepl(failMsgs, outputTestIntegrity))) ||
            isTRUE(any(grepl(failMsgs, fns)))) {
          memoiseFail <- TRUE
          rm(list = cache_key, envir = memoiseEnv(cachePath))
          cache_file <- CacheStoredFile(cachePath, cache_key)
        }
      } else {
        fns <- fns[nzchar(fns)]
        if (!is.null(fns) && length(fns) > 0) {
          fnsInOutputObjects <- intersect(names(fns), outputObjects)
          fns <- fns[fnsInOutputObjects]
          fnsExistBefore <- try(file.exists(fns))
          fnsInCache <- file.path(CacheStorageDir(cachePath),
                                  basename(.prefix(fns, prefixCacheId(cacheId = cache_key))))
          hardLinkOrCopy(fnsInCache, fns, overwrite = TRUE, verbose = FALSE)
          fnsExistAfter <- file.exists(fns)
          if (any(fnsExistAfter %in% FALSE) && isTRUE(any(fnsExistBefore != fnsExistAfter))) # this means that hardLinkOrCopy failed
            browser()
        }
      }
    }

    if (!fromMemoise || rerun || memoiseFail || cacheSaveFormatFail) {
      obj <- if (!is.null(cache_file)) {
        try(loadFile(cache_file, cacheSaveFormat = cacheSaveFormat), silent = TRUE)
      } else {
        rerun <- TRUE
      }
      output <- try(.unwrap(obj, cachePath = cachePath, cacheId = cache_key))
      if (is(obj, "try-error") || rerun || is(output, "try-error")) {
        messageCache("It looks like the cache file is corrupt or was interrupted during write; deleting and recalculating")
        otherFiles2 <- dir(CacheStorageDir(cachePath), pattern = cache_key, full.names = TRUE)
        if (!is(shownCache, "try-error")) {
          otherFiles <- normPath(file.path(CacheStorageDir(cachePath),
                                           shownCache[tagKey == "filesToLoad"]$tagValue))
          otherFiles2 <- c(otherFiles, otherFiles2)
        }
        rmFiles <- unique(c(cache_file, otherFiles2))
        unlink(rmFiles)
        return(.returnNothing)
      }

      if (isTRUE(changedSaveFormat)) {
        swapCacheFileFormat(wrappedObj = obj, cachePath = cachePath, drv = drv, conn = conn,
                            cacheId = cache_key, sameCacheID = sameCacheID,
                            newFile = cache_file_orig, verbose = verbose)
        cacheSaveFormat <- fileExt(cache_file_orig) # setdiff(.cacheSaveFormats, cacheSaveFormat)
      }
    }

    if (cloudWrite(useCloud)) {
      cloudUploadFromCache(cache_key %in% filePathSansExt(gdriveLs[["name"]]), cache_key,
                           cachePath, cloudFolderID = cloudFolderID, output, verbose = verbose)
    }

    .cacheMessage(object = output, functionName = functionName, fromMemoise = fromMemoise, verbose = verbose)

    if (getOption("reproducible.useMemoise", FALSE)) {
      cache_key_in_memoiseEnv <- exists(cache_key, envir = memoiseEnv(cachePath), inherits = FALSE)
      if (cache_key_in_memoiseEnv %in% FALSE) {
        assign(cache_key, .unwrap(obj, cachePath = cachePath, cacheId = cache_key),
               envir = memoiseEnv(cachePath))
      }
    }

    if (!is.null(output))
      output <- addCacheAttr(output, .CacheIsNew = FALSE, outputHash = cache_key, func)

    .addTagsRepoAccessedTime(cache_key, cachePath = cachePath, cacheSaveFormat = cacheSaveFormat)
    attr(output, ".Cache")$newCache <- FALSE

    .dotsFromCache <- as.list(attr(full_call, ".Cache")$func_call)[-1]
    # # This allows for any class specific things
    if ("object" %in% names(.dotsFromCache))
      .dotsFromCache <- .dotsFromCache[setdiff(names(.dotsFromCache), "object")]

    output <- do.call(.prepareOutput, args = append(list(object = output, cachePath),
                                                    .dotsFromCache))


    return(output)
  }
}

defunct <- function(argNames) {
  # argNames <- call)
  deps <- .defunctCacheArgs
  for (d in deps)
    if (d %in% argNames) {
      stop(.message$defunct(d), call. = FALSE)
    }
}


.defunctCacheArgs <- c("sideEffect", "makeCopy", "compareRasterFileLength",
                       "cacheRepo", "digestPathContent")


isDevMode <- function(useCache, userTags) {
  isTRUE(any(pmatch(table = useCache, "dev") %in% 1)) && !is.null(userTags)
}

cloudWrite <- function(useCloud) {
  isTRUE(any(grepl("^w", useCloud) %in% 1)) || isTRUE(useCloud)
}

cloudWriteOrRead <- function(useCloud) {
  cloudWrite(useCloud) || cloudRead(useCloud)
}

cloudReadOnly <- function(useCloud) {
  isTRUE(any(grepl("^r", useCloud) %in% 1))
}

cloudRead <- function(useCloud) {
  cloudReadOnly(useCloud) || isTRUE(useCloud)
}

keyInGdriveLs <- function(cache_key, gdriveLs) {
  grepl(paste0("^(", cache_key, ").+$"), gdriveLs[["name"]])
  #filePathSansExt(filePathSansExt(gdriveLs[["name"]])) %in%  # double filePathSansExt because of the .dbFile.rds
  #  cache_key
}

verboseCacheDFAll <- function(verbose, functionName, times) {
  verboseDF1(verbose, functionName, times$CacheDigestStart, times$EvaluateStart)
  verboseDF2(verbose, functionName, times$EvaluateStart, times$SaveStart)
  verboseDF3(verbose, functionName, times$CacheDigestStart, times$SaveEnd)
  .message$CacheTimings(verbose)
}

optionsSetForCache <- function(drv = NULL, conn = NULL, envir = parent.frame(1),
                                verbose = getOption("reproducible.verbose")) {
  if (!is.null(drv) || !is.null(conn)) {
    useDBI(TRUE, verbose = verbose)
  }
  if (isFALSE(useDBI())) {
    opts <- options(
      reproducible.useDBI = FALSE
    )
  }
  if (!isFALSE(getOption("reproducible.useCacheV3")))
    opt2 <- options(
      reproducible.useCacheV3 = TRUE
    )
}

identical2 <- function(a, b) {
  id <- identical(a, b)
  if (isTRUE(id))
    return(TRUE)
  isTRUE(all.equal(a, b, check.attributes = FALSE))
}



evalTheFunAndAddChanged <- function(callList, keyFull, outputObjects, length, algo, quick,
                                    classOptions, .callingEnv, verbose, ...) {
  outputFromEvaluate <- evalTheFun(callList$FUNcaptured, !callList$usesDots,
                                   matchedCall = callList$call, envir = .callingEnv,
                                   verbose = verbose, ...)

  # Because this has be run, it means that it has changed; add an attribute to say that
  outputFromEvaluate <- .addChangedAttr(outputFromEvaluate, keyFull$preDigest,
                                        origArguments = attr(callList$new_call, ".Cache")$args_w_defaults,
                                        .objects = outputObjects, length = length,
                                        algo = algo, quick = quick, classOptions = classOptions, ...
  )
  outputFromEvaluate
}



.dtFileMainCols <- c("cacheId", "tagKey", "tagValue", "createdDate")

doDigestPrepare <- function(new_call, omitArgs, .cacheExtra) {
  toDigest <- attr(new_call, ".Cache")$args_w_defaults # not evaluated arguments

  # Deal with .objects -- wait these are dealt with by `.robustDigest`
  # toDigest <- rmDotObjectsInList(toDigest, .objects)
  # .objects <- dotObjectsToNULLInList(toDigest, .objects) # if .objects used in previous, set to NULL here

  toDigest$.FUN <- attr(new_call, ".Cache")$method
  # Deal with omitArgs by removing elements from the toDigest list of objects to digest
  if (!is.null(omitArgs)) {
    if (any("FUN" %in% omitArgs))
      omitArgs <- c(".FUN", omitArgs)
    toDigest[omitArgs] <- NULL
  }
  # Deal with .cacheExtra by adding it to the list of objects to digest
  if (!is.null(.cacheExtra))
    toDigest <- append(toDigest, list(.cacheExtra = .cacheExtra))
  toDigest
}



# Compile a list of elements to digest
# toDigest <- attr(new_call, ".Cache")$args_w_defaults # not evaluated arguments
#
# # Deal with .objects -- wait these are dealt with by `.robustDigest`
# # toDigest <- rmDotObjectsInList(toDigest, .objects)
# # .objects <- dotObjectsToNULLInList(toDigest, .objects) # if .objects used in previous, set to NULL here
#
# toDigest$.FUN <- attr(new_call, ".Cache")$method
# # Deal with omitArgs by removing elements from the toDigest list of objects to digest
# if (!is.null(omitArgs)) {
#   if (any("FUN" %in% omitArgs))
#     omitArgs <- c(".FUN", omitArgs)
#   toDigest[omitArgs] <- NULL
# }
# # Deal with .cacheExtra by adding it to the list of objects to digest
# if (!is.null(.cacheExtra))
#   toDigest <- append(toDigest, list(.cacheExtra = .cacheExtra))
doDigest <- function(toDigest, .functionName, .objects, length, algo, quick,
                      classOptions, timeCacheDigestStart, verbose) {
  detailed_key <- CacheDigest(toDigest,
                              .functionName = .functionName,
                              .objects = .objects,
                              length = length, algo = algo, quick = quick,
                              classOptions = classOptions,
                              calledFrom = "Cache"
  )
  diTi <- difftime(Sys.time(), timeCacheDigestStart, units = "sec")
  if (diTi > 5) {
    messageCache("Object digesting for ", .messageFunctionFn(.functionName)," took: ", format(diTi, digits = 2))
  }
  verboseCacheMessage(detailed_key$preDigest, .functionName, timeCacheDigestStart, quick = quick,
                      modifiedDots = toDigest, verbose = verbose, verboseLevel = 3)

  names(detailed_key)[[1]] <- "key"
  detailed_key
}



appendFunctionNameToNestedTags <- function(userTags, functionName) {
  # allUT <- c(paste0("outerFunction:", functionName), userTags)
  # dups <- duplicated(sapply(strsplitOnlySingleColon(allUT), tail, 1))
  # allUT <- allUT[!dups] # only take after :
  # allUT <- sort(allUT)
  .pkgEnv$.reproEnv2$userTags <- c(.pkgEnv$.reproEnv2$userTags,
                                   paste0("outerFunction:", functionName))
  .pkgEnv$.reproEnv2$userTags <- .pkgEnv$.reproEnv2$userTags[!duplicated(.pkgEnv$.reproEnv2$userTags)]
  # needFN <- identical(tail(dups, 1), FALSE)
  # if (isTRUE(needFN)) {
  #   appendNestedTags(outerFunction = functionName)
  # }
}

.txtGrepStrSplitSingleColon <- "(?<!:):(?!:)"

strsplitOnlySingleColon <- function(x, ...) {
  strsplit(x, split = .txtGrepStrSplitSingleColon, perl = TRUE)
}


reorder_by_first_element <- function(x) {
  keys <- sapply(x, `[[`, 1)  # extract the first elements
  seen <- character()
  res <- list()

  for (i in seq_along(x)) {
    key <- keys[i]
    if (!key %in% seen) {
      # First time we see this key: append
      res[[length(res) + 1]] <- x[[i]]
      seen <- c(seen, key)
    } else {
      # Find last index where this key is already in res
      last_idx <- max(which(sapply(res, `[[`, 1) == key))
      res <- append(res, list(x[[i]]), after = last_idx)
    }
  }

  res
}



createSimilar <- function(similar, .functionName, verbose, devMode) {

  simi <- similar[, .N, by = "cacheId"][similar, on = "cacheId"]
  data.table::setorderv(simi, c("N", "createdDate"))
  numSimilars <- NROW(unique(similar$cacheId))
  # messageCache("There are ", numSimilars,
  #              " similar calls (same fn: ", .messageFunctionFn(.functionName), ") in the Cache repository.",
  #              verbose = verbose * !devMode)
  simi <- split(simi, by = "N") # take first element in split list
  if (length(simi)) {
    simi <- simi[[1]]
    # if (identical(numSimilars, 1L)) {
    #   messageCache("It has ", simi$N[[1]], " differences", verbose = verbose * !devMode)
    # } else {
    #   messageCache("With fewest differences (", simi$N[[1]], "), there are ",
    #                NROW(unique(simi$cacheId)),
    #                " similar calls in the Cache repository.", verbose = verbose * !devMode)
    # }
    twoCols <- strsplitOnlySingleColon(simi[["tagValue"]])
    lens <- lengths(twoCols)
    hasNoColon <- lens == 1
    if (isTRUE(any(hasNoColon))) {
      whNoColon <- which(hasNoColon)
      twoCols[whNoColon] <- lapply(whNoColon, function(x) c(simi[["tagKey"]][[x]], twoCols[[x]]))
    }

    args <- vapply(twoCols, function(x) x[[1]], FUN.VALUE = character(1))
    lens <- lengths(twoCols)
    vals <- rep("", length(twoCols))
    vals[lens > 1] <- vapply(twoCols[lens > 1], function(x) x[[2]], FUN.VALUE = character(1))
    set(simi, NULL, "arg", args)
    set(simi, NULL, "value", vals)
    set(simi, NULL, c("N", "tagKey", "tagValue", "createdDate"), NULL)
    setcolorder(simi, c("cacheId", "arg", "value"))
    setnames(simi, old = c("cacheId", "value"), new = c("cacheIdInCache", "valueInCache"))
  } else {
    simi <- data.table(arg = character(), cacheIdInCache = character(), valueInCache = character())
  }
  simi
}


.txtNoPrefix <- "noPrefix"
.txtDryRunTRUE <- "dryRun = TRUE: "


stopRcppError <- function(toDigest, .objects, length, algo, quick, classOptions) {
  ooo <- Map(obj = names(toDigest), function(obj)
    try(.robustDigest(toDigest[[obj]], .objects = .objects,
                      length, algo, quick, classOptions)), silent = TRUE)
  ite <- Map(o = ooo, function(o) {
    is(o, "try-error")
  })
  ite <- ite[unlist(ite)]
  if (length(ite))
    stop(paste(names(ite), collapse = ", "), " ", isAre(ite), " corrupt. ",
         "This can usually be resolved by restarting the R session")
  else
    stop("One or more objects to be digested for Cache are corrupt. ",
         "This can usually be resolved by restarting the R session")
}



isSquigglyCall <- function(x) {
  is(x, "{")
}
