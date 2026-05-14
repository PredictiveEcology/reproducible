utils::globalVariables("arg")

#' @param dryRun See [reproducibleOptions].
#'
#' @include messages.R
#' @export
#' @rdname Cache
Cache <- function(FUN, ..., dryRun = getOption("reproducible.dryRun", FALSE),
                  notOlderThan = NULL,
                  .objects = NULL, .cacheExtra = NULL, .functionName = NULL,
                  .cacheChaining = getOption("reproducible.cacheChaining", NULL),
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

  validateUseCloud(useCloud)

  ## Lazy showCache async pre-populate: idempotent, ~10us when already spawned.
  ## See R/showCacheEtc.R::.maybeSpawnShowCacheAsync. Targets the cachePath
  ## actually being used (not the default at .onLoad time).
  .maybeSpawnShowCacheAsync(cachePath)

  # Capture and match call so it can be manipulated
  callList <- matchCall2(sys.function(0), sys.call(0), envir = .callingEnv, FUN = FUN)

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
    cacheChainDetails <- cacheChainingSetup(.cacheChaining, callList, omitArgs, verbose)
    toDigest <- doDigestPrepare(callList$new_call, cacheChainDetails$omitArgs, .cacheExtra)
    keyFull <- try2(doDigest(toDigest, callList$.functionName, .objects,
                            length, algo, quick, classOptions, times$CacheDigestStart,
                            verbose = verbose))
    if (is(keyFull, "try-error")) {
      # This is the bit that indicates that one or more objects in the toDigest
      #   are corrupted and can't be digested. So, it is the inputs to the
      #   function that are corrupted: this can't self heal. Needs better user
      #   error message to give help.
      stopRcppError(toDigest, .objects, length, algo, quick, classOptions)
    }
    # update with cacheChain info
    keyFull <- cacheChainingStep(keyFull, callList, .cacheChaining, cacheChainDetails, cachePaths)

  }

  # If debugCache is "quick", short circuit after doDigest
  if (isTRUE(!is.na(pmatch(debugCache, "quick"))))
    return(list(hash = keyFull$preDigest, content = callList$func_call))

  CacheDBFileCheckAndCreate(cachePaths[[1]], drv, conn, verbose = verbose) # checks that we are using multiDBfile backend

  if (cloudWrite(useCloud)) {
    cloudFolderID <- checkAndMakeCloudFolderID(cloudFolderID, cachePaths[[1]], create = TRUE, verbose = verbose)
    gdriveLs <- retry(quote(driveLs(cloudFolderID, keyFull$key, cachePath = cachePaths[[1]], verbose = verbose)))
  }

  if (missing(dryRun)) dryRun <- getOption("reproducible.cacheDryRun", FALSE)

  if (cacheSaveFormat %in% c(.qsFormat))
    cacheSaveFormat <- getOption("reproducible.qsFormat", .qs2Format)

  # Memoise and return if it is there #
  if (!dryRun) {
    outputFromMemoise <- check_and_get_memoised_copy(keyFull, cachePaths, callList$.functionName,
                                                     callList$func, useCache, useCloud,
                                                     cloudFolderID, gdriveLs, full_call = callList$new_call,
                                                     outputObjects = outputObjects,
                                                     cacheSaveFormat = cacheSaveFormat,
                                                     .cacheChaining = .cacheChaining,
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
    outputFromDisk <- check_and_get_cached_copy(keyFull, cachePaths, cache_file, callList$.functionName, callList$func,
                                                useCache, useCloud, cloudFolderID, gdriveLs,
                                                full_call = callList$new_call,
                                                outputObjects = outputObjects,
                                                cacheSaveFormat = cacheSaveFormat,
                                                .cacheChaining = .cacheChaining,
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
    outputFromDisk <- check_and_get_cached_copy(keyFull, cachePaths, cache_file, callList$.functionName, callList$func,
                                                useCache, useCloud = FALSE, cloudFolderID, gdriveLs,
                                                full_call = callList$new_call,
                                                outputObjects = outputObjects,
                                                cacheSaveFormat = cacheSaveFormat,
                                                .cacheChaining = .cacheChaining,
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

  outputFromEvaluate <- doSaveToCache(outputFromEvaluate, metadata, cachePaths, callList = callList, # callList$func,
                                      .objects, length, algo, quick, classOptions,
                                      cache_file, userTags, # callList$.functionName,
                                      debugCache,
                                      keyFull, outputObjects = outputObjects,
                                      useCloud, cloudFolderID, gdriveLs,
                                      # func_call = callList$func_call,
                                      cacheSaveFormat = cacheSaveFormat, drv = drv, conn = conn,
                                      useMemoise = getOption("reproducible.useMemoise", FALSE),
                                      .cacheChaining = .cacheChaining,
                                      verbose = verbose,
                                      times$SaveStart, times$EvaluateStart)
  times$SaveEnd <- Sys.time()
  if (getOption("reproducible.savePreDigest", FALSE)) {
    keyFullPreDigest <- keyFull
    keyFullPreDigest$key <- paste0(.txtPreDigest, "_", keyFullPreDigest$key)
    times$SavePreDigestStart <- Sys.time()
    locked <- lockFile(cachePaths[[1]], keyFullPreDigest$key, verbose = verbose)

    toDigestOut <- doSaveToCache(toDigest, metadata, cachePaths, callList = callList, # callList$func,
                                 .objects, length, algo, quick, classOptions,
                                 cache_file, userTags, # callList$.functionName,
                                 debugCache,
                                 keyFullPreDigest, outputObjects = outputObjects,
                                 # func_call = callList$func_call,
                                 cacheSaveFormat = cacheSaveFormat,
                                 drv = drv, conn = conn,
                                 useCloud = FALSE, # not this preDigest one
                                 cloudFolderID = NULL, gdriveLs = NULL,# not this preDigest one
                                 useMemoise = FALSE, # not this preDigest one
                                 .cacheChaining = .cacheChaining,
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

  if (requireNamespace("covr", quietly = TRUE) && covr::in_covr()) {
    strip_covr_wrappers <- function(expr) {
      while (is.call(expr) && identical(expr[[1]], as.name("{"))) expr <- expr[[length(expr)]]
      expr
    }
    call <- strip_covr_wrappers(call)
    if (length(call) >= 2L && is.language(call[[2]])) call[[2]] <- strip_covr_wrappers(call[[2]])
  }

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
        fun_chr <- as.character(fun)
        # Only parse if it's not a reserved word
        if (!(fun_chr %in% c("if", "function"))) {
          fun <- parse(text = fun_chr)
        }
        # else: leave fun as-is (symbol), so downstream logic can handle it
        # fun <- parse(text = fun)
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
      arg <- tryCatch(eval(arg, envir = envir), error = function(err) { # can't be tryCatch2 --> this must always be a tryCatch
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

check_and_get_cached_copy <- function(detailed_key, cachePaths, cache_file, functionName,
                                      func, useCache, useCloud, cloudFolderID, gdriveLs,
                                      full_call, outputObjects,
                                      cacheSaveFormat = getOption("reproducible.cacheSaveFormat"),
                                      .cacheChaining = getOption("reproducible.cacheChaining", FALSE),
                                      drv, conn, envir = parent.frame(), verbose) {
  cache_key <- detailed_key$key
  # Check if the result is already cached
  connOrig <- conn
  conns <- conn
  if (!is.null(conn) && !is.list(conns)) {
    conns <- list(conn)
    names(conns) <- cachePaths
  }

  for (cachePath in cachePaths) {
    cache_file <- CacheStoredFile(cachePath, cache_key, cacheSaveFormat = cacheSaveFormat, readOnly = TRUE)
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
                                    detailed_key, functionName, cache_file = cache_file,
                                    changedSaveFormat = changedSaveFormat, sameCacheID,
                                    cache_file_orig, func, shownCache = shownCache,
                                    full_call = full_call,
                                    outputObjects = outputObjects,
                                    cacheSaveFormat = cacheSaveFormat,
                                    .cacheChaining = .cacheChaining,
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

check_and_get_memoised_copy <- function(detailed_key, cachePaths, functionName, func,
                                        useCache, useCloud, cloudFolderID, gdriveLs,
                                        full_call, outputObjects,
                                        cacheSaveFormat = getOption("reproducible.cacheSaveFormat"),
                                        .cacheChaining = getOption("reproducible.cacheChaining", FALSE),
                                        drv, conn, verbose) {
  cache_key <- detailed_key$key
  if (getOption("reproducible.useMemoise", FALSE)) {
    for (cachePath in cachePaths) {
      cache_key_in_memoiseEnv <- exists(cache_key, envir = memoiseEnv(cachePath), inherits = FALSE)
      if (isTRUE(cache_key_in_memoiseEnv))
        break
    }

    if (cache_key_in_memoiseEnv) {
      output <- loadFromDiskOrMemoise(fromMemoise = TRUE, useCache = useCache, useCloud = useCloud,
                                      cloudFolderID = cloudFolderID, gdriveLs = gdriveLs,
                                      cachePath = cachePath, detailed_key = detailed_key,
                                      functionName = functionName, func = func,
                                      full_call = full_call,
                                      changedSaveFormat = FALSE,
                                      outputObjects = outputObjects,
                                      cacheSaveFormat = cacheSaveFormat,
                                      .cacheChaining = .cacheChaining,
                                      drv = drv, conn = conn, verbose = verbose,
                                      )
      return(output)
    }
  } else {
    # If useMemoise gets turned off, it needs to be emptied or there will be stale entries.
    # Preserve the "shownCache" binding -- it holds the showCache memoised
    # data.table and the async-spawn job table; both are independent of
    # useMemoise and clearing them would defeat the lazy async pre-populate
    # mechanism (jobs would be re-spawned on every Cache() call).
    me <- memoiseEnv(cachePaths[[1]])
    le <- setdiff(ls(me), "shownCache")
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
  if (is.primitive(func)) {
    formals_list <- formals(args(list))
  } else {
    formals_list <- formals(func)
  }
  return(as.list(formals_list))
}

# Helper function to reorder arguments based on formal arguments, combining defaults and user args
reorder_arguments <- function(formals, args) {
  # Combine defaults and args: user args override defaults

  areDots <- names(args) %in% "..."
  if (any(areDots)) {
    args2 <- args
    args2[[which(areDots)]] <- NULL
    args <- append(args2, args[[which(areDots)]])
  }

  if (FALSE) {
    # areDots <- names(args) %in% "..."
    namesOfArgs <- names(args) %in% "..."
    areDots <- any(names(formals) %in% "...") || any(namesOfArgs)
    if (any(areDots)) {
      if (length(namesOfArgs)) {
        args2 <- args
        for (wh in which(namesOfArgs)) {
          args2[[wh]] <- NULL
          args <- append(args2, args[[which(namesOfArgs)]])
        }

      } else {
        # these are unnamed args in the dots
      }
    }


  }

  if (length(formals) == 1 && all(names(formals) %in% "...")) {
    # This is case of things like `list`, `file.path`
    ordered_args <- args
  } else {
    # This will remove unnamed elements; which isn't right
    combined_args <- modifyList(formals, args, keep.null = TRUE)
    emptyNams <- names(args) %in% ""
    if (any(emptyNams)) {
      combined_args <- append(combined_args, args[emptyNams])
    }
    areDots <- names(combined_args) %in% "..."
    if (any(areDots)) {

      # argPlaceInsert <- which(!names(args) %in% names(formals))

      # needArgs <- !names(args) %in% names(combined_args)
      combined_args[areDots] <- NULL
      ordered_args <- combined_args
      # combined_args <- append(combined_args, args[needArgs])
      # areDots2 <- names(formals) %in% "..."
      # whNotDots <- which(!areDots2)
      # whDots <- which(areDots2)
      # first <- if (whDots > 1) seq(whDots - 1) else numeric()
      # anySeconds <- !whDots > whNotDots
      # second <- if (any(anySeconds)) whNotDots[anySeconds] else numeric()
      # ordered_args <- c(args[argPlaceInsert], formals[second])
    } else {
      ordered_args <- combined_args[union(names(formals), names(combined_args))]
    }
  }
  # Preserve the order of the formals

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
  if (!is.null(linkToCacheId))
    linkToCacheId <- CacheStoredFile(cachePath, linkToCacheId, cacheSaveFormat = cacheSaveFormat)
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
      userTagsList[[tc]] <- tryCatch2(format(userTagsList[[tc]]), error = function(u) as.character())
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
  filelock::unlock(locked)
  ## Do NOT delete the lock file: the fcntl lock is what protects the critical
  ## section, not the file's existence.  Deleting and recreating the file under
  ## concurrent load creates two bugs:
  ##   1. Workers that were blocked on fcntl(F_SETLKW) already have the old inode
  ##      open; a fresh caller that arrives after the delete creates a *new* inode
  ##      at the same path — both callers hold a "lock" on different inodes and
  ##      the critical section is no longer protected.
  ##   2. If a prior run was executed as root (or another user), a stale .lock file
  ##      with wrong ownership is left behind; the next caller gets EACCES at
  ##      open(O_RDWR|O_CREAT) — "Permission denied".
  ## Leaving the (empty) .lock file in place is safe and correct.
}

#' @importFrom stats runif
lockFile <- function(cachePath, cache_key,
                     envir   = parent.frame(),
                     verbose = getOption("reproducible.verbose")) {
  if (!useDBI()) {
    csd <- CacheStorageDir(cachePath)
    checkPath(csd, create = TRUE)

    lock_path <- file.path(csd, paste0(cache_key, suffixLockFile()))

    ## Three outcomes from filelock::lock:
    ##   NULL   — contention; sleep 2.5 s and retry
    ##   EMFILE — process near fd limit from other sources; gc + small sleep
    ##   EACCES — stale file owned by another user; remove and retry
    ##   other  — unexpected; re-throw immediately
    ##
    ## Note: PredictiveEcology/filelock >= 1.0.3.9001 fixes a bug in the
    ## upstream package where every failed non-blocking attempt leaked one fd
    ## (close()/CloseHandle() missing on the NULL return path in C).

    locked          <- NULL
    waiting         <- FALSE
    emfile_attempts <- 0L

    repeat {
      locked <- tryCatch(
        filelock::lock(lock_path, timeout = 0L),
        error = function(e) {
          msg <- conditionMessage(e)
          if (!grepl("Cannot open lock file", msg, fixed = TRUE)) stop(e)

          if (grepl("Too many open files", msg, fixed = TRUE)) {
            emfile_attempts <<- emfile_attempts + 1L
            if (emfile_attempts > 10L)
              stop("Persistent 'Too many open files' acquiring lock: ", lock_path,
                   "\nRaise ulimit -n or report a filelock fd-leak bug",
                   call. = FALSE)
            gc(FALSE)
            Sys.sleep(runif(1L, 0.1, 0.3) * emfile_attempts)
            return(NULL)
          }

          ## EACCES or similar — remove stale file and retry
          removed <- suppressWarnings(file.remove(lock_path))
          if (!isTRUE(removed))
            stop("Cannot open lock file and cannot remove it.\n",
                 "Manually delete (may need sudo): ", lock_path, "\n",
                 "Original error: ", msg, call. = FALSE)
          messageCache("Lock file not accessible; removed and retrying",
                       verbose = verbose + 1)
          dir.create(csd, showWarnings = FALSE, recursive = TRUE)
          return(NULL)
        }
      )

      if (!is.null(locked)) break

      if (!waiting) {
        waiting <- TRUE
        messageCache(
          "The cache file (", lock_path, ") is locked due to a concurrent process; waiting...",
          "\nTo diagnose the holding process (works on Linux/macOS):",
          "\n  system(\"fuser '", lock_path, "'\")",
          "\n  system(\"lsof '", lock_path, "'\")",
          "\nOn a network filesystem (NFS/CIFS), unlink() will NOT remove the file while",
          "\na process holds it open -- kill the holding process first, then the lock releases.",
          "\nIf no process is found (stale lock on a local filesystem), then delete the lockfile:",
          "\n  unlink('", lock_path, "', force = TRUE)",
          verbose = verbose + 2
        )
      }

      Sys.sleep(2.5)
    }

    if (waiting)
      messageCache("  ... ", lock_path, " released, continuing ... ", verbose = verbose + 2)

    # on.exit(filelock::unlock(locked), add = TRUE)
    #
    # # Try repeatedly, but with bounded waits and backoff
    # repeat {
    #   ## If you still want a time cap on the *attempt*, make it transient and reset:
    #   setTimeLimit(elapsed = 3, transient = TRUE)
    #   locked <- filelock::lock(lock_path, timeout = 250000)   # ~2.5 s wait, returns NULL on timeout
    #   setTimeLimit(elapsed = Inf, transient = TRUE)
    #
    #   if (!is.null(locked)) break  # acquired
    #
    #   if (isTRUE(first)) {
    #     first <- FALSE
    #     messageCache(
    #       "The cache file (", lock_path, ") is locked due to a concurrent process; waiting... ",
    #       "\nIf there is no concurrent process (i.e., no parallelism), delete that lockfile",
    #       verbose = verbose + 2
    #     )
    #   }
    #   Sys.sleep(0.25)  # backoff
    # }
    # Ensure release when the *outer* scope exits
    on.exit2(releaseLockFile(locked), envir = envir)
    locked
  }
}


#' @importFrom data.table setorderv setcolorder
showSimilar <- function(cachePath, metadata, .functionName, userTags, useCache,
                        # cacheSaveFormat = getOption("reproducible.cacheSaveFormat"),
                        drv, conn, verbose) {
  devMode <- isDevMode(useCache, userTags)  # don't use devMode if no userTags
  shownCacheUserTags <- showCache(cachePath, Function = .functionName, userTags = userTags,
                          verbose = verbose - 2)
  shownCache <- showCache(cachePath, Function = .functionName, # userTags = userTags,
                          verbose = verbose - 2)
  # functionByDigest <- metadata[tagKey %in% "preDigest" & startsWith(tagValue, dotFunTxt)]$tagValue
  # shownCache <- shownCache[tagKey %in% "preDigest" & tagValue %in% functionByDigest]
  setorderv(shownCache, "createdDate", order = -1)
  # shownCache <- shownCache[tagKey != "outerFunction"] # doesn't matter what outerFunctions do, if all others are same
  # metadata <- metadata[tagKey != "outerFunction"]
  onKey <- c("tagKey", "tagValue")

  if (NROW(shownCache)) {
    userTagsMess <- if (!is.null(userTags)) {
      paste0(.message$BecauseOfA,
             "with user supplied tags: '",
             paste(userTags, collapse = ", "), "' "
      )
    }

    rmTagKeys <- "otherFunction|elapsedTime|accessed|module:|eventType:|eventTime:|outerFunction:"
    shownCache <- shownCache[grep(x = tagKey, rmTagKeys, invert = TRUE)]
    metadataSmall <- metadata[grep(x = tagKey, rmTagKeys, invert = TRUE)]
    # Can only compare on tagKeys that are *not yet* in the metadata; e.g., object.size may
    #   not be there, so don't know if it is different
    similarFull <- unique(shownCache[tagKey %in% unique(c(metadata$tagKey))], by = .dtFileMainCols)
    similarFullList <- split(similarFull, by = "cacheId")
    notInThisCall <- lapply(similarFullList, function(x) x[!metadataSmall, on = onKey])
    notInSC <- lapply(similarFullList, function(x) metadataSmall[!x, on = onKey])
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
        data.table::setnames(x, old = c(valInCacheTxt, cacheIdInCacheTxt),
                             new = c(valThisCallTxt, cacheIdThisCallTxt),
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
          a <- notInSC4[[n]][notInThisCall3[[n]], on = argTxt, allow.cartesian = TRUE]
          b <- notInThisCall3[[n]][notInSC4[[n]], on = argTxt, allow.cartesian = TRUE]
          d <- unique(rbindlist(list(a, b), fill = TRUE))
        } else {
          d <- data.table(notInSC4[[n]], valueInCache = NA, cacheIdInCache = NA)
        }

        # Convert .FUN to the actual function name; need 2 mechanisms because SpaDES.core manually
        #   places an entry with the actual name
        hasDotFun <- d[[argTxt]] %in% dotFunTxt
        if (any(hasDotFun)) {
          dups <- duplicated(d[[valThisCallTxt]])
          if (any(dups)) {
            # Remove .FUN if there is another one with "more info"
            theDupCI <- d[[valThisCallTxt]][dups]
            theDotFun <- d[[valThisCallTxt]] %in% theDupCI & hasDotFun
            d <- d[!theDotFun]
          } else {
            # case where it shows only ".FUN", with no duplication
            scHere <- shownCache[shownCache$cacheId %in% d[[cacheIdInCacheTxt]], ]# $tagKey %in% "function"
            funName <- scHere[["tagValue"]][scHere[["tagKey"]] %in% "function"]
            if (length(funName))
              d[[argTxt]] <- funName
          }

        }
        setcolorder(d, c(argTxt, cacheIdInCacheTxt, valInCacheTxt,
                         cacheIdThisCallTxt, valThisCallTxt))
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

      if (isDevMode(useCache, userTags)) {
        # Only replace entries that actually matched on userTags (not just function name)
        cacheIdsToClear <- intersect(unique(names(simi)), unique(shownCacheUserTags$cacheId))
        if (length(cacheIdsToClear)) {
          messageCache("------ devMode -------", verbose = verbose)
          messageCache("Previous call(s) exist in the cache with identical userTags (",
                       paste0(userTags, collapse = ", "), ")", verbose = verbose)
          messageCache("This call to cache will replace entry with cacheId(s): ",
                       paste0(simi[["cacheId"]], collapse = ", "), verbose = verbose)
          clearCache(cachePath, cacheId = cacheIdsToClear, ask = FALSE,  drv = drv, conn = conn, verbose = verbose - 2)
        }
      }
      nShow <- min(numSmallest, 5)
      messageCache("with different elements (", nShow, " most recent at top):", verbose = verbose)
      # don't add a prefix if there is no `sim` in the stack
      wis <- .whereInStack("sim")
      prefix <- if (identical(.GlobalEnv, wis) || is.null(wis)) "" else .message$NoPrefix
      messageCache(.message$dashes, prefix)
      keepers <- seq_len(nShow)
      lala <- Map(si = simi[keepers], nam = names(simi[keepers]), function(si, nam) {
        messageCache(paste0("Compared to cacheId: ", nam, prefix), verbose = verbose)
        if (verbose > 0) {
          oo <- capture.output(si)
          fn <- cliCol(getOption("reproducible.messageColourCache"))
          oo <- paddDFInitial(oo, rows = 1:2, .spaceTmpChar, colour = getOption("reproducible.messageColourCache"))
          messageColoured(paste0(paste(oo, collapse = "\n"), .message$NoPrefix),
                          colour = getOption("reproducible.messageColourCache"))
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

  convertDBbackendIfIncorrect(cachePath, drv, conn, verbose = verbose - 1)

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

doSaveToCache <- function(outputFromEvaluate, metadata, cachePaths, callList, # func,
                          .objects, length, algo, quick, classOptions,
                          cache_file, userTags, # .functionName,
                          debugCache,
                          detailed_key, # func_call,
                          outputObjects,
                          useCloud, cloudFolderID, gdriveLs,
                          cacheSaveFormat = getOption("reproducible.cacheSaveFormat"),
                          drv, conn, useMemoise = getOption("reproducible.useMemoise", FALSE),
                          .cacheChaining = getOption("reproducible.cacheChaining", FALSE),
                          verbose, timeSaveStart, timeEvaluateStart) {
  # Can't save NULL with attributes
  if (is.null(outputFromEvaluate)) outputFromEvaluate <- "NULL"

  outputFromEvaluate <- addCacheAttr(outputFromEvaluate, .CacheIsNew = TRUE, detailed_key$key, callList$func)

  outputFromEvaluate <- cacheChainingPost(detailed_key, outputFromEvaluate,
                                            attr(callList$new_call, ".Cache")[cacheChainingOuterFunctionName],
                                            cachePaths[[1]], linkToCacheId = NULL, cacheSaveFormat,
                                          .cacheChaining = .cacheChaining, drv, conn, verbose = verbose)
  metadata <- wrapSaveToCache(outputFromEvaluate, metadata, detailed_key$key, cachePaths[[1]],
                              # userTags = paste0(metadata$tagKey, ":", metadata$tagValue),
                              outputObjects = outputObjects,
                              preDigest = detailed_key$preDigest, callList$.functionName,
                              cacheSaveFormat = cacheSaveFormat, drv, conn, verbose)

  # Memoize the outputFromEvaluate by saving it in RAM
  if (isTRUE(useMemoise)) {
    assign(detailed_key$key, outputFromEvaluate, envir = memoiseEnv(cachePaths[[1]]))
  }


  if (identical(outputFromEvaluate, "NULL")) outputFromEvaluate <- NULL

  if (isTRUE(!is.na(pmatch(debugCache, "complete"))))
    outputFromEvaluate <- .debugCache(outputFromEvaluate, detailed_key$preDigest, fullCall = callList$func_call)

  if (cloudWrite(useCloud)) {
    cloudUploadFromCache(detailed_key$key %in% filePathSansExt(gdriveLs[["name"]]), detailed_key$key,
                         cachePaths[[1]], cloudFolderID = cloudFolderID, outputFromEvaluate, verbose = verbose)
  }
  outputFromEvaluate

}




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
                                  cachePath, detailed_key,
                                  functionName,
                                  cache_file = NULL, changedSaveFormat, sameCacheID,
                                  cache_file_orig, func, shownCache = NULL,
                                  full_call, outputObjects,
                                  cacheSaveFormat = getOption("reproducible.cacheSaveFormat"),
                                  .cacheChaining = getOption("reproducible.cacheChaining", FALSE),
                                  drv, conn, verbose) {

  cache_key <- detailed_key$key
  if (identical(useCache, "overwrite")) {
    clearCacheOverwrite(cachePath, cache_key, functionName, drv, conn, verbose)
    return(invisible(.returnNothing))
  } else {
    format <- if (missing(cache_file) || is.null(cache_file)) cacheSaveFormat else
      fileExt(cache_file)

    for (iii in 1:2) {
      fe <- CacheDBFileSingle(cachePath = cachePath, cacheId = cache_key,
                              cacheSaveFormat = cacheSaveFormat)
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
      # shownCache <- showCacheFast(cache_key, cachePath, dtFile = fe,
      #                                 # cacheSaveFormat = cacheSaveFormat,
      #                                 drv = drv, conn = conn)
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
      # output <- get(cache_key, envir = memoiseEnv(cachePath))
      output <- .unwrap(get(cache_key, envir = memoiseEnv(cachePath)), cacheId = cache_key, cachePath = cachePath,
                        drv = drv, conn = conn)
      # need to update the individual files in file-backed objects from the cache; can't use memoise

      # Some objects, especially Rcpp objects can get stale; rerun if this is the case; the test with subsetting 1st element
      #   is not great, but I could not find a better one that will fail on those Rcpp fails. The problem
      #   is that the object exists, but it's inner structure is wrong
      outputTestIntegrity <- try(output[1], silent = TRUE) # This needs to be `try`, not `try2`
      fns <- try2(Filenames(output), silent = TRUE) # previous will only get some of the failures
      memoiseFail <- dealWithCacheRecoveryErrors(memoiseFail, outputTestIntegrity, fns, cache_key, cachePath, outputObjects)
      # if (isTRUE(is(outputTestIntegrity, "try-error")) || isTRUE(is(fns, "try-error"))) {
      #   failMsgs <- "external pointer.+not valid|NULL value passed as symbol address"
      #   if (isTRUE(any(grepl(failMsgs, outputTestIntegrity))) ||
      #       isTRUE(any(grepl(failMsgs, fns)))) {
      #     memoiseFail <- TRUE
      #     rm(list = cache_key, envir = memoiseEnv(cachePath))
      #     cache_file <- CacheStoredFile(cachePath, cache_key, readOnly = TRUE)
      #   }
      # } else {
      #   fns <- fns[nzchar(fns)]
      #   if (!is.null(fns) && length(fns) > 0) {
      #     fnsInOutputObjects <- intersect(names(fns), outputObjects)
      #     fns <- fns[fnsInOutputObjects]
      #     fnsExistBefore <- try2(file.exists(fns))
      #     fnsInCache <- file.path(CacheStorageDir(cachePath),
      #                             basename(.prefix(fns, prefixCacheId(cacheId = cache_key))))
      #     hardLinkOrCopy(fnsInCache, fns, overwrite = TRUE, verbose = FALSE)
      #     fnsExistAfter <- file.exists(fns)
      #     if (any(fnsExistAfter %in% FALSE) && isTRUE(any(fnsExistBefore != fnsExistAfter))) # this means that hardLinkOrCopy failed
      #       browser()
      #   }
      # }
    }

    if (!fromMemoise || rerun || memoiseFail || cacheSaveFormatFail) {
      obj <- if (!is.null(cache_file)) {
        # loadFile(cache_file, cacheSaveFormat = cacheSaveFormat,
        #              cacheId = cache_key, cachePath = cachePath, # in case it needs swapCacheFormat
        #              drv = drv, conn = conn, verbose = verbose)
        try(loadFile(cache_file, cacheSaveFormat = cacheSaveFormat,
                     cacheId = cache_key, cachePath = cachePath, # in case it needs swapCacheFormat
                     drv = drv, conn = conn, verbose = verbose), silent = TRUE)
      } else {
        rerun <- TRUE
      }

      if (isTRUE(changedSaveFormat)) {
        swapTry <- try(swapCacheFileFormat(
          wrappedObj = obj, cachePath = cachePath, drv = drv, conn = conn,
          cacheId = cache_key, sameCacheID = sameCacheID,
          userTags = paste0(shownCache$tagKey, ":", shownCache$tagValue),
          newFile = cache_file_orig, verbose = verbose), silent = TRUE)
        cacheSaveFormat <- fileExt(cache_file_orig) # setdiff(.cacheSaveFormats, cacheSaveFormat)
        # rerun <- TRUE
      }
      output <- try(.unwrap(obj, cachePath = cachePath, cacheId = cache_key))
      if (is(obj, "try-error") || rerun || is(output, "try-error")) {
        messageCache("It looks like the cache file is corrupt or was interrupted during write; deleting and recalculating")
        otherFiles2 <- dir(CacheStorageDir(cachePath), pattern = cache_key, full.names = TRUE)
        if (!is(shownCache, "try-error")) {
          if (!is.null(shownCache)) {
            otherFiles <- normPath(file.path(CacheStorageDir(cachePath),
                                             shownCache[tagKey == "filesToLoad"]$tagValue))
            otherFiles2 <- c(otherFiles, otherFiles2)
          }
        }
        rmFiles <- unique(c(cache_file, otherFiles2))
        unlink(rmFiles)
        return(.returnNothing)
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
        # assign(cache_key, .unwrap(obj, cachePath = cachePath, cacheId = cache_key),
        #        envir = memoiseEnv(cachePath))
        assign(cache_key, obj, envir = memoiseEnv(cachePath)) # try without .unwrap in memoiseEnv
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

    output <- cacheChainingPost(detailed_key, output,
                                attr(full_call, ".Cache")[cacheChainingOuterFunctionName],
                                cachePath, linkToCacheId = NULL, cacheSaveFormat,
                                .cacheChaining = .cacheChaining, drv, conn, verbose = verbose)

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

## `useCloud` accepts: TRUE/FALSE/NULL, or one of "push"/"pull".
##   - TRUE  / "push": developer role -- bidirectional. Download on cloud hit;
##                     upload on miss.
##   - "pull"        : user role -- read-only. Download on cloud hit; never
##                     upload. If the local cache already has the object, the
##                     cloud is not consulted at all (the gdriveLs fetch is
##                     deferred until after the local check fails).
##   - FALSE / NULL  : cloud disabled.
## Legacy "^w"/"^r" prefix matching is retained for back-compat (e.g. "write",
## "read", "readOnly") since the contract is otherwise narrow.
cloudWrite <- function(useCloud) {
  isTRUE(useCloud) ||
    identical(useCloud, "push") ||
    isTRUE(any(grepl("^w", useCloud) %in% 1))
}

cloudWriteOrRead <- function(useCloud) {
  cloudWrite(useCloud) || cloudRead(useCloud)
}

cloudReadOnly <- function(useCloud) {
  identical(useCloud, "pull") ||
    isTRUE(any(grepl("^r", useCloud) %in% 1))
}

cloudRead <- function(useCloud) {
  cloudReadOnly(useCloud) || isTRUE(useCloud) || identical(useCloud, "push")
}

## Validate the `useCloud` argument and return it unchanged. Errors on a
## character value that is not "pull" or "push" (or a legacy ^w/^r prefix).
validateUseCloud <- function(useCloud) {
  if (is.null(useCloud) || isTRUE(useCloud) || isFALSE(useCloud))
    return(invisible(useCloud))
  if (is.character(useCloud) && length(useCloud) == 1L &&
      (useCloud %in% c("pull", "push") ||
       grepl("^[wr]", useCloud)))
    return(invisible(useCloud))
  stop("`useCloud` must be TRUE, FALSE, NULL, \"pull\", or \"push\"; got: ",
       deparse(useCloud), call. = FALSE)
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

  toDigest$.FUN <- attr(new_call, ".Cache")$method
  # Deal with omitArgs:
  # - TRUE  => drop every captured arg; digest is based on .FUN (the actual
  #            function value, body included, so source edits still bust the
  #            cache) plus .cacheExtra
  # - char  => drop the named args
  # - NULL  => default, no change
  if (isTRUE(omitArgs)) {
    toDigest <- toDigest[names(toDigest) %in% ".FUN"]
  } else if (is.character(omitArgs)) {
    if (any("FUN" %in% omitArgs))
      omitArgs <- c(dotFunTxt, omitArgs)
    toDigest[omitArgs] <- NULL
  }
  # Deal with .cacheExtra by adding it to the list of objects to digest
  if (!is.null(.cacheExtra))
    toDigest <- append(toDigest, list(.cacheExtra = .cacheExtra))
  toDigest
}




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
    set(simi, NULL, argTxt, args)
    set(simi, NULL, "value", vals)
    set(simi, NULL, c("N", "tagKey", "tagValue", "createdDate"), NULL)
    setcolorder(simi, c("cacheId", argTxt, "value"))
    setnames(simi, old = c("cacheId", "value"), new = c(cacheIdInCacheTxt, valInCacheTxt))
  } else {
    simi <- data.table(arg = character(), cacheIdInCache = character(), valueInCache = character())
  }
  simi
}


.txtNoPrefix <- "noPrefix"
.txtDryRunTRUE <- "dryRun = TRUE: "


stopRcppError <- function(toDigest, .objects, length, algo, quick, classOptions) {
  ooo <- Map(obj = names(toDigest), function(obj)
    try2(.robustDigest(toDigest[[obj]], .objects = .objects,
                      length, algo, quick, classOptions), silent = TRUE))
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


cacheChainingSetup <- function(.cacheChaining, callList, omitArgs, verbose) {
  if (isTRUE(.cacheChaining %in% TRUE)) { #
    .cacheChaining <- sys.function(-2)
  }
  cfdigList <- details <- preDigests <- NULL
  messageCacheChainChanged <- FALSE

  if (useCacheChaining(.cacheChaining)) {
    bb <- attr(callList$new_call, ".Cache")
    hasCacheTags <- lapply(bb$args_w_defaults, function(y) attr(y, "tags")) |> unlist()
    cfdig <- .robustDigest(.cacheChaining)
    cfdigList <- list(cfdig) |> setNames(cacheChainingOuterFunctionName)
    if (length(hasCacheTags)) {
      if (is.null(.pkgEnv[["cacheChaining"]])) {
        .pkgEnv$cacheChaining <- new.env(parent = emptyenv())
      }
      details <- .pkgEnv$cacheChaining[[cfdigList[[1]]]]
      cids <- names(details)
      if (length(cids) == 0) { # not in the RAM stashing place "yet"; use normal Cache
        sc2 <- showCacheFast(cacheId = cfdigList[[1]])
        cids <- sc2[["tagValue"]][sc2$tagKey == "cacheChain"]
      }

      if (length(cids)) {
        # The function being assessed has to assess objects that were created within this same function;
        #   otherwise they could be from a Cache outside this function
        wasItInThisFn <- lapply(bb$args_w_defaults, function(y) attr(y, cacheChainingOuterFunctionName)) |> unlist()
        wasItInThisFn <- identical(wasItInThisFn[[1]], cfdigList[[1]])
        if (length(hasCacheTags) && wasItInThisFn) {
          onlyOneCid <- gsub("cacheId:", "", hasCacheTags)
          messageCache("Using cacheChaining ...", verbose = verbose)

          if (exists("sc2", inherits = FALSE)) {
            onlyOneCidReal <- sc2[["cacheId"]][sc2$tagValue %in% onlyOneCid]
            sc3 <- sc2[sc2[["cacheId"]] %in% onlyOneCidReal] # in case of duplicate entries
            sc4 <- sc3#[sc3$tagValue == onlyOneCid]
            preDigests <- Map(nam = names(hasCacheTags), function(nam)
              sc4[["tagValue"]][sc4[["tagKey"]] == nam])
            details <- as.list(sc2$tagValue[-1]) |> setNames(sc2$tagKey[-1]) |>
              list() |> setNames("preDigests") |>
              list() |> setNames(onlyOneCid) |>
              as.environment()
          } else {
            preDigests <- Map(nam = names(hasCacheTags), oocid = onlyOneCid, function(nam, oocid) {
              .pkgEnv$cacheChaining[[cfdigList[[1]]]][[oocid]]$preDigests[[nam]]
            })
          }

          omitArgs <- c(omitArgs, names(hasCacheTags))
          messageCache("Skipping digest of ", paste0(names(hasCacheTags), collapse = ", "), verbose = verbose)
          # .cacheExtra <- c(.cacheExtra, preDigests)
        } else {
          messageCacheChainChanged <- TRUE
          # messageCache("Using cacheChaining; but .cacheChaining has changed; adding to new chain")
        }
      } else {
        messageCacheChainChanged <- TRUE
        # messageCache("Using cacheChaining; but .cacheChaining has changed or ",
        #              "this is the first call in the .cacheChaining; starting a new chain")
      }
    } else {
      messageCacheChainChanged <- TRUE
    }
    attr(callList$new_call, ".Cache") <- append(attr(callList$new_call, ".Cache"), cfdigList)

  }
  if (isTRUE(messageCacheChainChanged))
    messageCache("Using cacheChaining; but enclosing function has changed or ",
                 "this is the first Cached call in the function where ", .messageFunctionFn(callList$.functionName),
                 " is being Cached; starting a new chain")
  list(.cacheChaining = .cacheChaining,
       preDigests = preDigests,
       omitArgs = omitArgs,
       callList = callList,
       details = details,
       cfdigList = cfdigList)
}

cacheChainingPost <- function(detailed_key, outputFromEvaluate, cacheChainingOuterFunction,
                              cachePath, linkToCacheId, cacheSaveFormat, .cacheChaining, drv, conn,
                              verbose = getOption("reproducible.verbose")) {
  if (!isTRUE(.cacheChaining %in% FALSE)) {

    dk <- detailed_key[["preDigest"]]
    if (!is.null(dk)) { # some have only `key` and no `preDigest`, e.g., Cache(.inputObjects(sim), .objects = objectsToEvaluateForCaching,
      if (!is.character(detailed_key$preDigest$.cacheExtra) &&
          !is.null(detailed_key$preDigest$.cacheExtra$cacheChainingOuterFunction)) {
        cacheChainingFnDigest <- detailed_key$preDigest$.cacheExtra$cacheChainingOuterFunction
      } else {
        cacheChainingFnDigest <- dk[[surroundingFunctionLabel]]
      }
      attr(outputFromEvaluate, cacheChainingOuterFunctionName) <- cacheChainingFnDigest

      if (is.null(.pkgEnv$cacheChaining[[cacheChainingFnDigest]])) {
        .pkgEnv$cacheChaining[[cacheChainingFnDigest]] <- new.env(parent = emptyenv())
      }

      dkSimple <- dk[-which(names(dk) == surroundingFunctionLabel)]
      if (any(names(dkSimple) %in% ".cacheExtra")) {
        if (is.null(names(dkSimple[[".cacheExtra"]]))) {
          names(dkSimple[[".cacheExtra"]]) <- as.character(seq_along(length(dkSimple[[".cacheExtra"]])))
        }
      }
      userTags <- paste0(names(unlist(dkSimple)), ":", paste0(detailed_key$key, ":", unlist(dkSimple)))
      fil <- CacheDBFileSingle(cachePath = cachePath, cacheId = cacheChainingFnDigest)
      needWrite <- TRUE
      if (file.exists(fil)) {
        # browser() # what should 'cacheId' be --> detailed_key?
        tmp <- loadFile(fil,
                        # cacheId = cacheId,
                        cachePath = cachePath, # in case it needs swapCacheFormat
                        drv = drv, conn = conn, verbose = verbose)
        userTags1 <- paste0(tmp$tagKey, ":", tmp$tagValue)
        userTags2 <- union(userTags, userTags1)
        if (identical(length(userTags2), length(userTags1))) {
          needWrite <- FALSE
        } else {
          userTags <- userTags2
        }

      }
      if (isTRUE(needWrite)) {
        # This adds or updates a new entry in the cache repository about the function itself
        fs <- saveToCache(cachePath = cachePath,
                          obj = NULL, verbose = verbose - 1, # cache_file[1],
                          userTags = userTags, linkToCacheId = linkToCacheId,
                          cacheSaveFormat = cacheSaveFormat,
                          drv = drv, conn = conn,
                          cacheId = cacheChainingFnDigest)

        assign(detailed_key$key,
               list(preDigests = detailed_key$preDigest) ,
               envir = .pkgEnv$cacheChaining[[cacheChainingFnDigest]])
      }
    }
  }
  return(outputFromEvaluate)
}

cacheChainingOuterFunctionName <- "cacheChainingOuterFunction"
cacheChainLabel <- "cacheChain_"
surroundingFunctionLabel <- "surroundingFunction"


useCacheChaining <- function(.cacheChaining) {
  first <- !is.null(.cacheChaining) && (is.function(.cacheChaining) || !.cacheChaining %in% FALSE)
  if (isTRUE(first)) {
    udbi <- useDBI(verbose = -2)
    if (udbi %in% TRUE) {
      if (is.null(.pkgEnv$cacheChainingMessage) ||
          isTRUE(difftime(Sys.time(), .pkgEnv$cacheChainingMessage) > 60*60)) {
        .pkgEnv$cacheChainingMessage <- Sys.time()
        message("cacheChaining will only work if not using DBI cache backend; ",
                "\nset `options(reproducible.cacheChaining = FALSE)` to remove this message",
                "\nor set `useDBI(FALSE)` ... this message will be shown at package startup and every hour")
      }
      first <- FALSE
    }
  }
  first
}


cacheChainingStep <- function(keyFull, callList, .cacheChaining, cacheChainDetails, cachePaths) {
  if (!isTRUE(.cacheChaining %in% FALSE)) {

    alreadyCachedArgs <- lapply(attr(callList$new_call, ".Cache")$args_w_defaults,
                                function(x) attr(x, "tags")) |> unlist()
    if (!is.null(alreadyCachedArgs)) {
      alreadyCachedTags <- paste0(cacheChainLabel, names(alreadyCachedArgs))#, ":",
      newBits <- Map(act = alreadyCachedTags, aca = alreadyCachedArgs, function(act, aca) {
        unname(gsub("cacheId:", "", aca))
      })
      keyFull[["preDigest"]] <- modifyList(keyFull[["preDigest"]], newBits)
    }
    .cacheChaining <- if (missing(cacheChainDetails)) .cacheChaining else cacheChainDetails$.cacheChaining
    if (!is.function(.cacheChaining))
      .cacheChaining <- sys.function(-2)
    keyFull[["preDigest"]][[surroundingFunctionLabel]] <- .robustDigest(.cacheChaining)
    if (useCacheChaining(.cacheChaining) && !is.null(cacheChainDetails$omitArgs)) {
      ccd <- lapply(cacheChainDetails$details, function(x) x$preDigests)
      cacheDigestDetails <- rbindlist(ccd, idcol = "obj", use.names = TRUE, fill = TRUE)

      sc <- showCacheFast(cacheId = cacheChainDetails$cfdigList[[1]])
      if (!is.null(sc)) {
        sc <- setDT(sc)
        sss <- strsplit(sc$tagValue, ":")
        set(sc, NULL, "cacheId2", vapply(sss, function(x) x[[1]], character(1)))
        set(sc, NULL, "tagValue", vapply(sss, function(x) x[[2]], character(1)))
        kf <- keyFull$preDigest[!names(keyFull$preDigest) %in% surroundingFunctionLabel]
        pre <- setDT(list(tagKey = names(unlist(kf)), tagValue = unname(unlist(kf))))

        # The next few steps are slower with data.table and are the bottlenecks when profiling
        # outs2 <- sc[pre, on = colnames(pre), nomatch = NA]
        outs <- setDT(merge(setDF(sc), setDF(pre), by = colnames(pre), all.y = T))
        if (anyNA(outs$cacheId)) {
          # if (length(outs$cacheId) == 0) {
          # not usable -- skip
        } else {
          neededNum <- length(unique(outs$tagKey))
          # lll <- outs[, .N, by = "cacheId2"]
          lll2 <- table(outs$cacheId2)
          # setorderv(lll, "cacheId2")
          # lll2 <- lll2[order(names(lll2))]
          # if (!(identical(lll$cacheId2, names(lll2)) && identical(lll$N, as.integer(unname(lll2))))) {
          # }
          hasAll <- lll2 == neededNum
          # hasAll <- lll$N == neededNum
          if (any(hasAll)) {
            cidToCheck <- names(lll2)[hasAll]
            # cidToCheck <- lll$cacheId2[hasAll]
            if (NROW(cidToCheck) > 1) {
              browser() # these are now wrong -- should be unnecessary -- there should not ever be >1 of these; so don't need to !keep
            }
            if (NROW(cidToCheck)) {
              if (!keyFull$key %in% cidToCheck) { # no override needed
                sc <- setDT(sc)
                rrr <- sc[sc$cacheId2 %in% cidToCheck & startsWith(sc$tagKey, cacheChainLabel)]
                tv <- unique(rrr$tagValue)
                tk <- unique(rrr$tagKey)
                scCheck <- showCacheFast(tv, cachePaths[[1]])
                argName <- gsub(cacheChainLabel, "", tk)
                allGood <- any(startsWith(scCheck$tagValue, paste0(argName, ":")))
                if (allGood) {
                  cacheIdOverrideFromChaining <- showCacheFast(cacheId = cidToCheck, cachePaths[[1]])
                  keyFull$key <- cacheIdOverrideFromChaining$cacheId[[1]]
                }
              }
            }
          }
        }
      }
    }
  }
  return(keyFull)
}


inCacheTxt <- "InCache"
thisCallTxt <- "ThisCall"
argTxt <- "arg"
valInCacheTxt <- paste0("value", inCacheTxt)
cacheIdInCacheTxt <- paste0("cacheId", inCacheTxt)
cacheIdThisCallTxt <- paste0("cacheIdOf", thisCallTxt)
valThisCallTxt <- paste0("value", thisCallTxt)


dealWithCacheRecoveryErrors <- function(memoiseFail, outputTestIntegrity, fns, cache_key, cachePath, outputObjects) {
  if (isTRUE(is(outputTestIntegrity, "try-error")) || isTRUE(is(fns, "try-error"))) {
    failMsgs <- "external pointer.+not valid|NULL value passed as symbol address"
    if (isTRUE(any(grepl(failMsgs, outputTestIntegrity))) ||
        isTRUE(any(grepl(failMsgs, fns)))) {
      memoiseFail <- TRUE
      rm(list = cache_key, envir = memoiseEnv(cachePath))
      cache_file <- CacheStoredFile(cachePath, cache_key, readOnly = TRUE)
    }
  } else {
    fns <- fns[nzchar(fns)]
    if (!is.null(fns) && length(fns) > 0) {
      fnsInOutputObjects <- intersect(names(fns), outputObjects)
      fns <- fns[fnsInOutputObjects]
      fnsExistBefore <- try2(file.exists(fns))
      fnsInCache <- file.path(CacheStorageDir(cachePath),
                              basename(.prefix(fns, prefixCacheId(cacheId = cache_key))))
      hardLinkOrCopy(fnsInCache, fns, overwrite = TRUE, verbose = FALSE)
      fnsExistAfter <- file.exists(fns)
      if (any(fnsExistAfter %in% FALSE) && isTRUE(any(fnsExistBefore != fnsExistAfter))) # this means that hardLinkOrCopy failed
        browser()
    }
  }
  memoiseFail
}
