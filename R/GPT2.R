#' @include messages.R
#' @export
#' @rdname Cache
Cache <- function(FUN, ..., notOlderThan = NULL,
                  .objects = NULL, .cacheExtra = NULL, .functionName = NULL,
                  outputObjects = NULL, # nolint
                  algo = "xxhash64",
                  cachePath = NULL,
                  length = getOption("reproducible.length", Inf),
                  userTags = c(),
                  omitArgs = NULL,
                  classOptions = list(), debugCache = character(),
                  quick = getOption("reproducible.quick", FALSE),
                  verbose = getOption("reproducible.verbose", 1),
                  cacheId = NULL,
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

  if (isFALSE(getOption("reproducible.cache2"))) {
    callList$call[[1]] <- substitute(Cache2)
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
  appendNestedTags(otherFunction = callList$.functionName)

  # do the Digest
  times <- list()
  times$CacheDigestStart <- Sys.time()
  keyFull <- doDigest(callList$new_call, omitArgs, .cacheExtra, callList$.functionName, .objects,
                      length, algo, quick, classOptions, times$CacheDigestStart, verbose)

  # If debugCache is "quick", short circuit after doDigest
  if (isTRUE(!is.na(pmatch(debugCache, "quick"))))
    return(list(hash = keyFull$preDigest, content = callList$func_call))
  # Override keyFull$key if user has specified with cacheId
  if (!is.null(cacheId))
    keyFull$key <- cacheIdOverride(cacheId, keyFull$key, callList$.functionName, verbose)


  # Construct the full file path for the cache directory and possible file
  cachePaths <- getCacheRepos(cachePath, callList$new_call[-1], verbose = verbose)
  # cachePath <- cachePaths[[1]]
  CacheDBFileCheckAndCreate(cachePaths[[1]], drv, conn, verbose = verbose) # checks that we are using multiDBfile backend

  if (cloudWrite(useCloud)) {
    cloudFolderID <- checkAndMakeCloudFolderID(cloudFolderID, cachePaths[[1]], create = TRUE, verbose = verbose)
    gdriveLs <- retry(quote(driveLs(cloudFolderID, keyFull$key, cachePath = cachePaths[[1]], verbose = verbose)))
  }

  # Memoise and return if it is there #
  outputFromMemoise <- check_and_get_memoised_copy(keyFull$key, cachePaths, callList$.functionName,
                                                   callList$func, useCache, useCloud,
                                                   cloudFolderID, gdriveLs, full_call = callList$new_call,
                                                   drv = drv, conn = conn, verbose)
  if (!identical2(.returnNothing, outputFromMemoise))
    return(outputFromMemoise)

  # After memoising fail, try files; need to check Cache dir and set lockfile
  locked <- lockFile(cachePaths[[1]], keyFull$key)

  # Check if keyFull$key is on disk and return if it is there
  outputFromDisk <- check_and_get_cached_copy(keyFull$key, cachePaths, cache_file, callList$.functionName, callList$func,
                                              useCache, useCloud, cloudFolderID, gdriveLs,
                                              full_call = callList$new_call,
                                              drv, conn, verbose = verbose)

  if (!identical2(.returnNothing, outputFromDisk))
    return(outputFromDisk)

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
                                                drv, conn, verbose = verbose)
    return(outputFromDisk)
  } # Derive some metadata prior to evaluation so "showSimilar" can have something to compare with

  times$EvaluateStart <- Sys.time()
  metadata <- metadata_define_preEval(keyFull, callList$.functionName, userTags,
                                      .objects, length, algo, quick, classOptions,
                                      times$EvaluateStart, times$CacheDigestStart)

  if (isTRUE(showSimilar) || isDevMode(useCache, userTags))
    showSimilar(cachePaths[[1]], metadata, callList$.functionName, userTags, useCache,
                drv = drv, conn = conn, verbose)

  # ## evaluate the call ## #
  outputFromEvaluate <- evalTheFunAndAddChanged(callList = callList, keyFull = keyFull,
                                                outputObjects = outputObjects, length = length,
                                                algo = algo, quick = quick, classOptions = classOptions,
                                                .callingEnv = .callingEnv,
                                                verbose = verbose, ...)

  # ## Save to Cache; including to Memoise location; including metadata ## #
  times$SaveStart <- Sys.time()
  outputFromEvaluate <- doSaveToCache(outputFromEvaluate, metadata, cachePaths, callList$func,
                                      .objects, length, algo, quick, classOptions,
                                      cache_file, userTags, callList$.functionName, debugCache,
                                      keyFull,
                                      useCloud, cloudFolderID, gdriveLs,
                                      func_call = callList$func_call, drv = drv, conn = conn,
                                      verbose = verbose,
                                      times$SaveStart, times$EvaluateStart)
  times$SaveEnd <- Sys.time()
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
          func_call <- undoDoCall(func_call)
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

    # Handle variable references in arguments
    # args <- lapply(args, function(arg) {
    #   if (is.symbol(arg) || is.call(arg)) {
    #     return(arg)  # Keep the argument as-is without evaluating
    #   } else {
    #     return(arg)  # Return non-symbol arguments unchanged
    #   }
    # })
  }

  if (is.call(func) || is.name(func)) {
    fun <- if (is.null(func_full)) func else func_full
    if (is.name(fun)) {
      infixes <- c("+", "-", "*", "/", "==", "!=", "<", ">", "<=", ">=", "&&", "||")
      areInfixes <- try(any(fun == infixes), silent = TRUE)
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
  new_call <- as.call(c(func, args))
  # This matches call on the FUN, not a duplicate of matchCall2
  matched_call <- match_call_primitive(func, new_call, expand.dots = TRUE, envir = .callingEnv)

  if (isSquiggly) {
    FUNcaptured <- recursiveEvalNamesOnly(matched_call, envir = .callingEnv) # deals with e.g., stats::rnorm, b$fun, b[[fun]]
    args <- as.list(FUNcaptured[-1])
  } else {
    args <- as.list(matched_call)[-1]
    args <- evaluate_args(args, envir = .callingEnv)
  }

  func <- getMethodAll(as.call(c(matched_call[[1]], args)), .callingEnv)

  combined_args <- combine_clean_args(func, args, .objects = NULL, .callingEnv)

  # Check for arguments that are in both Cache and the FUN
  matched_call <- checkOverlappingArgs(call, combined_args, dotsCaptured = args,
                                       functionName = "outer", matched_call, whichCache = "cache2")

  if (is.null(func_call)) func_call <- new_call
  func_call2 <- as.call(c(func_call[[1]], args))
  attr(matched_call, ".Cache")$func_call <- func_call2
  attr(matched_call, ".Cache")$args_w_defaults <- combined_args
  attr(matched_call, ".Cache")$method <- func

  return(matched_call)
}

evaluate_args <- function(args, envir) {
  lapply(args, function(arg) {
    if (is.call(arg)) {
      arg <- tryCatch(eval(arg, envir = envir), error = function(err) {
        # If it's a call that cannot be evaluated, evaluate recursively
        as.call(c(arg, evaluate_args(as.list(arg[-1]), envir)))
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
                                      full_call, drv, conn, envir = parent.frame(), verbose) {

  # Check if the result is already cached
  connOrig <- conn
  conns <- checkConns(cachePaths, conn)

  for (cachePath in cachePaths) {
    cache_file <- CacheStoredFile(cachePath, cache_key)
    cacheFileExists <- file.exists(cache_file) # could be length >1
    conns <- createConns(cachePath, conns, drv) # this will convert backend if it is wrong
    if (useDBI()) {
      inReposPoss <- searchInRepos(cachePath,
                                   outputHash = cache_key,
                                   drv = drv, conn = conns[[cachePath]]
      )
      if (cachePath == cachePaths[[1]] || NROW(inReposPoss$isInRepo)) {
        # keep important parts if it is first one, or if it has the object in the cacheRepo
        # inRepos <- inReposPoss
        conn <- conns[[cachePath]]
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
    ut <- strsplit(userTagsExtra, split = ":")
    ll <- lapply(ut, tail, 1)
    names(ll) <- lapply(ut, head, 1)
    userTagsList <- ll
    metadata <- rbindlist(list(metadata, userTagsListToDT(cache_key, userTagsList)))
  }
  metadata
}

check_and_get_memoised_copy <- function(cache_key, cachePaths, functionName, func,
                                        useCache, useCloud, cloudFolderID, gdriveLs,
                                        full_call, drv, conn, verbose) {
  if (getOption("reproducible.useMemoise")) {
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
                                      drv = drv, conn = conn, verbose = verbose,
                                      )
      return(output)
    }
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
undoDoCall <- function(call) {
  if (is.call(call) && all(as.character(call[[1]]) == "do.call")) {
    func <- call[[2]]
    args <- call[[3]]

    if (is.call(args) && as.character(args[[1]]) == "list") {
      args <- as.list(args[-1])
    }

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
  combined_args <- modifyList(formals, args)

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


cache_Id_Identical <- function(metadata, cachePaths, cache_key) {
  linkToCacheId <- NULL
  os <- metadata$tagValue[metadata$tagKey == "object.size"]
  if (!identical(os, "NA")) {
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
  if (!is.null(linkToCacheId)) linkToCacheId <- CacheStoredFile(cachePath, linkToCacheId)
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
  tagKey <- paste0(names(df), ":", as.character(df))
  if (length(userTags)) {
    ut <- strsplit(userTags, split = ":")
    ll <- lapply(ut, tail, 1)
    names(ll) <- rep("userTags", length(ll))#lapply(ut, head, 1)
    userTags <- ll
  }
  userTagsList <- c(
    list(FUN = func_name),
    userTags,
    list(accessed = sysTimeForCacheToChar()),
    list(inCloud = isTRUE(useCloud)),
    list(elapsedTimeDigest = format(elapsedTimeCacheDigest, units = "secs")),
    list(preDigest = tagKey)
  )
  names(userTagsList)[1] <- "function"

  cache_key <- detailed_key$key
  metadata <- userTagsListToDT(cache_key, userTagsList)
  return(metadata)
}

metadata_define_postEval <- function(metadata, cacheId, outputToSave,
                                     .objects, length, algo, quick, classOptions,
                                     elapsedTimeFUN) {

  objSize <- if (getOption("reproducible.objSize", TRUE)) sum(objSize(outputToSave)) else NA

  resultHash <- ""
  if (isTRUE(objSize > .objectSizeMinForBig)) {
    resultHash <- CacheDigest(outputToSave,
                              .objects = .objects,
                              length = length, algo = algo, quick = quick,
                              classOptions = classOptions, calledFrom = "Cache"
    )$outputHash
  }
  fns <- Filenames(outputToSave)
  userTagsList <- c(
    list(class = class(outputToSave)[1]),
    list(object.size = format(as.numeric(objSize))),
    list(fromDisk = isTRUE(any(nchar(fns) > 0))),
    list(resultHash = resultHash),
    list(elapsedTimeFirstRun = format(elapsedTimeFUN, units = "secs"))
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
    userTags <- .pkgEnv$.reproEnv2$userTags <- c(.pkgEnv$.reproEnv2$userTags, userTags)
    .pkgEnv$.reproEnv2$nestLevel <- .pkgEnv$.reproEnv2$nestLevel + 1
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

.addTagsRepoAccessedTime <- function(cache_key, cachePath = cachePath) {
  .addTagsRepo(cacheId = cache_key, tagKey = "accessed", tagValue = sysTimeForCacheToChar(),
               , cachePath = cachePath)
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

lockFile <- function(cachePath, cache_key, envir = parent.frame()) {
  if (!useDBI()) {
    csd <- CacheStorageDir(cachePath)
    if (!any(dir.exists(csd)))
      lapply(csd, dir.create, showWarnings = FALSE, recursive = TRUE)
    lockFile <- file.path(csd, paste0(cache_key, suffixLockFile()))
    locked <- filelock::lock(lockFile)
    on.exit2(releaseLockFile(locked), envir = envir)
    locked
  }
}

#' @importFrom data.table setorderv setcolorder
showSimilar <- function(cachePath, metadata, .functionName, userTags, useCache, drv, conn, verbose) {
  devMode <- isDevMode(useCache, userTags)  # don't use devMode if no userTags
  shownCache <- showCache(cachePath, Function = .functionName, verbose = verbose - 2)
  if (!is.null(userTags)) { # userTags are as "strong as" functionName
    if (!devMode) {  # This has to be exact match for devMode i.e., number of and exact tags;
      #  not exact match for showSimilar
      shownCache <- shownCache[tagValue %in% userTags]
    } else {
      scTmp <- shownCache[tagKey == "userTags"] # [tagValue %in% userTags, c("cacheId")]
      scTmp2 <- scTmp[, .N, by = "cacheId"][N == length(userTags)] # Has to be exact
      scTmp <- scTmp[scTmp2, on = "cacheId"]
      userTags2 <- gsub("^.*:", "", userTags)
      userTags3 <- data.table(tagKey = "userTags", tagValue = userTags2)
      rmCacheId <- scTmp[!userTags3, on = c("tagKey", "tagValue")]
      scTmp <- scTmp[which(!scTmp$cacheId %in% rmCacheId)]
      dups <- duplicated(scTmp)
      shownCache <- shownCache[scTmp[which(dups %in% FALSE)], on = "cacheId"]
    }
  }

  if (NROW(shownCache)) {
    userTagsMess <- if (!is.null(userTags)) {
      paste0(.message$BecauseOfA,
             "with user supplied tags: '",
             paste(userTags, collapse = ", "), "' "
      )
    }

    shownCache <- shownCache[tagKey %in% c(metadata$tagKey)][grep(x = tagKey, "elapsedTime|accessed", invert = TRUE)]
    # cacheIdOfSimilar
    similarFull <- shownCache[tagKey %in% c(metadata$tagKey)]
    similar <- similarFull[!metadata, on = c("tagKey", "tagValue")]
    other <- logical()
    if (NROW(similar) == 0) {
      other <- vapply(strsplit(similarFull$tagValue, split = "\\:"),
                      function(x) ifelse(length(x) == 2, x[[2]], NA_character_), FUN.VALUE = character(1))
      otherLabels <- vapply(strsplit(similarFull$tagValue, split = "\\:"),
                      function(x) ifelse(length(x) == 2, x[[1]], NA_character_), FUN.VALUE = character(1))
      whOther <- other == "other"
      # similar <- similarFull[whOther %in% TRUE]
      cacheIdOfSimilar <- unique(similarFull$cacheId)
      simFun <- list(funName = shownCache$tagValue[shownCache$tagKey == "function"])
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
      # lapply(strsplit(shownCache$tagValue, split = "\\:"), function(x) x[[3]])

    }
    if (NROW(similar)) {
      sim <- similar[, .N, by = "cacheId"][similar, on = "cacheId"] |> setorderv(c("N", "createdDate"))
      messageCache("There are ", NROW(unique(similar$cacheId)),
                   " similar calls (same fn: ", .messageFunctionFn(.functionName), ") in the Cache repository.",
                   verbose = verbose * !devMode)
      sim <- split(sim, by = "N") # take first element in split list
      sim <- sim[[1]]
      messageCache("With fewest differences (", sim$N[[1]], "), there are ",
                   NROW(unique(sim$cacheId)),
                   " similar calls in the Cache repository.", verbose = verbose * !devMode)
      twoCols <- strsplit(sim[["tagValue"]], ":")
      set(sim, NULL, c("N", "tagKey", "tagValue"), NULL)
      args <- vapply(twoCols, function(x) x[[1]], FUN.VALUE = character(1))
      vals <- vapply(twoCols, function(x) x[[2]], FUN.VALUE = character(1))
      set(sim, NULL, "arg", args)
      set(sim, NULL, "value", vals)
      setcolorder(sim, c("cacheId", "arg", "value"))
      if (isDevMode(useCache, userTags)) {
        messageCache("------ devMode -------", verbose = verbose)
        messageCache("Previous call(s) exist in the cache with identical userTags (",
                     paste0(userTags, collapse = ", "), ")", verbose = verbose)
        messageCache("This call to cache will replace entry with cacheId(s): ",
                     paste0(sim[["cacheId"]], collapse = ", "), verbose = verbose)
        cacheIdsToClear <- paste0("^", sim[["cacheId"]], "$", collapse = "|")
        clearCache(cachePath, userTags = cacheIdsToClear, ask = FALSE, drv = drv, conn = conn, verbose = verbose - 2)
      }
      messageCache("with different elements (most recent at top):", verbose = verbose)
      messageDF(sim, verbose = verbose)
      messageCache("------ devMode -------", verbose = verbose * devMode)
    }
  } else {
    messageCache(.message$noSimilarCacheTxt(.functionName), verbose = verbose)
  }
}

CacheDBFileCheckAndCreate <- function(cachePath, drv = NULL, conn = NULL, verbose) {

  convertDBbackendIfIncorrect(cachePath, drv, conn, verbose)

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

# saveMetadataFile <- function(metadata, cache_key, userTags, cachePath, objectSize, .functionName, drv, conn, verbose) {
#   metadata_file <- CacheDBFileSingle(cachePath = cachePath, cacheId = cache_key)
#   dbfile <- CacheDBFileCheckAndCreate(cachePath, verbose = verbose)
#   messageCache(
#     .message$SavingToCacheTxt(userTags = userTags, otsObjSize = objectSize,
#                               functionName = .functionName,
#                               cacheId = cache_key), verbose = verbose - 2)
#   saveFilesInCacheFolder(metadata, metadata_file, cachePath = cachePath, cacheId = cache_key)
#   .message$Saved(cachePath, cache_key, functionName = .functionName, verbose)
# }

convertCallWithSquigglyBraces <- function(call, usesDots) {
  if (length(call) == 2) {
    call <- as.call(c(call[[1]], call[[-1]][[-1]]))
  } else if ((length(call) > 2) && isFALSE(usesDots)) {
    call <- as.call(c(call[[1]], FUN = as.list(call[-1])[[1]][[-1]], as.list(call[-1])[-1]))
  }
  call
}

wrapSaveToCache <- function(outputFromEvaluate, metadata, cache_key, cachePath, # userTags,
                            preDigest, .functionName, drv, conn, verbose) {
  cacheIdIdentical <- cache_Id_Identical(metadata, cachePath, cache_key)
  linkToCacheId <- if (!is.null(cacheIdIdentical)) filePathSansExt(basename(cacheIdIdentical))  else NULL
  outputToSave <- .wrap(outputFromEvaluate, cachePath = cachePath, preDigest = preDigest, verbose = verbose)
  metadata <- metadata_update(outputToSave, metadata, cache_key) # .wrap may have added tags
  userTags <- paste0(metadata$tagKey, ":", metadata$tagValue)
  fs <- saveToCache(cachePath = cachePath, # drv = NULL, conn = NULL,
                    obj = outputToSave, verbose = verbose, # cache_file[1],
                    userTags = userTags, linkToCacheId = linkToCacheId,
                    drv = drv, conn = conn,
                    cacheId = cache_key)
  .message$Saved(cachePath, cache_key, functionName = .functionName, verbose = verbose)
  return(metadata)
}

doSaveToCache <- function(outputFromEvaluate, metadata, cachePaths, func,
                          .objects, length, algo, quick, classOptions,
                          cache_file, userTags, .functionName, debugCache,
                          detailed_key, func_call,
                          useCloud, cloudFolderID, gdriveLs,
                          drv, conn,
                          verbose, timeSaveStart, timeEvaluateStart) {

  elapsedTimeFUN <- difftime(timeSaveStart, timeEvaluateStart, units = "secs")

  # update metadata with other elements including elapsedTime for evaluation
  metadata <- metadata_define_postEval(metadata, detailed_key$key, outputFromEvaluate,
                                       .objects, length, algo, quick, classOptions,
                                       elapsedTimeFUN)
  # objectSize <- attr(metadata, "tags")$objectSize

  # Can't save NULL with attributes
  if (is.null(outputFromEvaluate)) outputFromEvaluate <- "NULL"

  # cacheIdIdentical <- cache_Id_Identical(metadata, cachePaths, detailed_key$key)
  # if (is.null(cacheIdIdentical)) {
  outputFromEvaluate <- addCacheAttr(outputFromEvaluate, .CacheIsNew = TRUE, detailed_key$key, func)
  metadata <- wrapSaveToCache(outputFromEvaluate, metadata, detailed_key$key, cachePaths[[1]],
                              # userTags = paste0(metadata$tagKey, ":", metadata$tagValue),
                              preDigest = detailed_key$preDigest, .functionName, drv, conn, verbose)
  # } else {
  #   browser()
  #   file.link(cacheIdIdentical, cache_file)
  #   # Save metadata file
  #   .message$FileLinkUsed(ftL = cacheIdIdentical, fts = cache_file, verbose)
  #   saveDBFileSingle()
  #   # saveMetadataFile(metadata, detailed_key$key, userTags, cachePaths[[1]], objectSize, .functionName,
  #   #                  drv, conn, verbose)
  # }

  # Memoize the outputFromEvaluate by saving it in RAM
  if (getOption("reproducible.useMemoise"))
    assign(detailed_key$key, outputFromEvaluate, envir = memoiseEnv(cachePaths[[1]]))


  if (identical(outputFromEvaluate, "NULL")) outputFromEvaluate <- NULL

  if (isTRUE(!is.na(pmatch(debugCache, "complete"))))
    outputFromEvaluate <- .debugCache(outputFromEvaluate, detailed_key$preDigest, fullCall = func_call)

  if (cloudWrite(useCloud)) {
    cloudUploadFromCache(detailed_key$key %in% filePathSansExt(gdriveLs[["name"]]), detailed_key$key,
                         cachePaths[[1]], cloudFolderID = cloudFolderID, outputFromEvaluate, verbose = verbose)
  }
  outputFromEvaluate

}


doDigest <- function(new_call, omitArgs, .cacheExtra, .functionName, .objects,
                     length, algo, quick, classOptions, timeCacheDigestStart, verbose) {
  # Compile a list of elements to digest
  toDigest <- attr(new_call, ".Cache")$args_w_defaults # not evaluated arguments
  toDigest$.FUN <- attr(new_call, ".Cache")$method
  # Deal with omitArgs by removing elements from the toDigest list of objects to digest
  if (!is.null(omitArgs))
    toDigest[omitArgs] <- NULL
  # Deal with .cacheExtra by adding it to the list of objects to digest
  if (!is.null(.cacheExtra))
    toDigest <- append(toDigest, list(.cacheExtra = .cacheExtra))
  detailed_key <- CacheDigest(toDigest,
                              .functionName = .functionName,
                              .objects = .objects,
                              length = length, algo = algo, quick = quick,
                              classOptions = classOptions,
                              calledFrom = "Cache"
  )
  verboseCacheMessage(detailed_key$preDigest, .functionName, timeCacheDigestStart, quick = quick,
                   modifiedDots = toDigest, verbose = verbose, verboseLevel = 3)

  names(detailed_key)[[1]] <- "key"
  detailed_key
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

  isSquiggly <- is(callList$FUNorig, "{")
  if (isTRUE(isSquiggly))
    callList$call <- convertCallWithSquigglyBraces(callList$call, callList$usesDots)
  new_call <- convertCallToCommonFormat(callList$call, callList$usesDots, isSquiggly, .callingEnv) # evaluated arguments
  func_call <- attr(new_call, ".Cache")$func_call         # not evaluated arguments
  func <- as.list(new_call)[[1]]

  # Try to identify the .functionName; if can't just use the matched call callList$FUNorig
  if (is.null(.functionName))
    .functionName <- getFunctionName2(func_call)# as.character(normalized_FUN[[1]])
  if (!isTRUE(any(nzchar(.functionName)))) {
    .functionName <- format(callList$FUNorig)
  }
  append(callList, list(new_call = new_call, func_call = func_call,
                           func = func, .functionName = .functionName))
}

cacheIdOverride <- function(cacheId, key, .functionName, verbose) {
  shownCache <- cacheIdCheckInCache(cacheId, calculatedCacheId = key, .functionName, verbose)
  if (NROW(shownCache)) cacheId <- shownCache$cacheId[1]
  cacheId
}

useCacheFromNested <- function(useCache) {
  isNested <- .pkgEnv$.reproEnv2$nestLevel > 1
  if (isNested)
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
                                  full_call, drv, conn, verbose) {
  if (identical(useCache, "overwrite")) {
    clearCacheOverwrite(cachePath, cache_key, functionName, drv, conn, verbose)
    return(invisible(.returnNothing))
  } else {
    format <- if (missing(cache_file) || is.null(cache_file)) getOption("reproducible.cacheSaveFormat") else
      fileExt(cache_file)

    fe <- CacheDBFileSingle(cachePath = cachePath, cacheId = cache_key, format = format)
    if (is.null(shownCache))
      shownCache <- showCacheFast(cache_key, cachePath, dtFile = fe, drv = drv, conn = conn)

    .cacheMessageObjectToRetrieve(functionName, shownCache, cachePath,
                                  cacheId = cache_key, verbose = verbose)
    if (fromMemoise) {
      output <- get(cache_key, envir = memoiseEnv(cachePath))
    } else {
      obj <- loadFile(cache_file)
      output <- .unwrap(obj, cachePath = cachePath)
      if (isTRUE(changedSaveFormat)) {
        swapCacheFileFormat(wrappedObj = obj, cachePath = cachePath, drv = drv, conn = conn,
                            cacheId = cache_key, sameCacheID = sameCacheID,
                            newFile = cache_file_orig, verbose = verbose)
      }
    }

    if (cloudWrite(useCloud)) {
      cloudUploadFromCache(cache_key %in% filePathSansExt(gdriveLs[["name"]]), cache_key,
                           cachePath, cloudFolderID = cloudFolderID, output, verbose = verbose)
    }

    .cacheMessage(object = output, functionName = functionName, fromMemoise = fromMemoise, verbose = verbose)


    if (getOption("reproducible.useMemoise")) {
      cache_key_in_memoiseEnv <- exists(cache_key, envir = memoiseEnv(cachePath), inherits = FALSE)
      if (cache_key_in_memoiseEnv %in% FALSE)
        assign(cache_key, output, envir = memoiseEnv(cachePath))
    }

    if (!is.null(output))
      output <- addCacheAttr(output, .CacheIsNew = FALSE, outputHash = cache_key, func)

    .addTagsRepoAccessedTime(cache_key, cachePath = cachePath)
    attr(output, ".Cache")$newCache <- FALSE

    .dotsFromCache <- as.list(attr(full_call, ".Cache")$func_call)[-1]
    # # This allows for any class specific things
    if ("object" %in% names(.dotsFromCache))
      .dotsFromCache <- .dotsFromCache[setdiff(names(.dotsFromCache), "object")]

    output <- do.call(.prepareOutput, args = append(list(object = output, cachePath), .dotsFromCache))


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
  isTRUE(any(grepl("w", useCloud) %in% 1)) || isTRUE(useCloud)
}

cloudWriteOrRead <- function(useCloud) {
  cloudWrite(useCloud) || cloudRead(useCloud)
}

cloudReadOnly <- function(useCloud) {
  isTRUE(any(grepl("r", useCloud) %in% 1))
}

cloudRead <- function(useCloud) {
  cloudReadOnly(useCloud) || isTRUE(useCloud)
}

keyInGdriveLs <- function(cache_key, gdriveLs) {
  filePathSansExt(filePathSansExt(gdriveLs[["name"]])) %in%  # double filePathSansExt because of the .dbFile.rds
    cache_key
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
  if (!isFALSE(getOption("reproducible.cache2")))
    opt2 <- options(
      reproducible.cache2 = TRUE
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
