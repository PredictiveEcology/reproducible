cache2 <- function(FUN, ..., .objects = NULL, .cacheExtra = NULL,
                   .functionName = NULL, algo = "xxhash64",
                   cachePath = NULL,
                   userTags = c(), omitArgs = NULL,
                   length = getOption("reproducible.length", Inf),
                   classOptions = list(),
                   quick = getOption("reproducible.quick", FALSE),
                   verbose = getOption("reproducible.verbose"),
                   useCache = getOption("reproducible.useCache", TRUE),
                   .callingEnv = parent.frame()) {

  # This makes this act like useDBI = FALSE --> creates correct dbFile
  opts <- options(reproducible.useDBI = FALSE)
  on.exit(options(opts), add = TRUE)

  call <- match.call(expand.dots = TRUE)

  # Check if FUN is a call and matches the expected structure
  if (length(call$FUN) > 1) # just a function
    if (identical(call$FUN[[1]], quote(quote))) {
      call$FUN <- as.list(call$FUN)[[-1]] # unquote it
    }

  usesDots <- sum(!nzchar(names(call))) > 1

  if (isFALSE(useCache) || useCache == 0)
    return(skipCache(FUN, ..., usesDots = usesDots, .callingEnv = .callingEnv))

  isSquiggly <- is(call$FUN, "{")
  if (isTRUE(isSquiggly)) {
    if (length(call) == 2) {
      call <- as.call(c(call[[1]], call[[-1]][[-1]]))
    } else if ((length(call) > 2) && isFALSE(usesDots)) {
      call <- as.call(c(call[[1]], FUN = as.list(call[-1])[[1]][[-1]], as.list(call[-1])[-1]))
    }
  }

  new_call <- harmonize_call(call, usesDots, isSquiggly, .callingEnv) # evaluated arguments

  func_call <- attr(new_call, ".Cache")$func_call         # not evaluated arguments
  func <- as.list(new_call)[[1]]
  args <- as.list(new_call)[-1]
  combined_args <- attr(new_call, ".Cache")$args_w_defaults         # not evaluated arguments

  combined_args$.FUN <- attr(new_call, ".Cache")$method # getMethodAll(func_call, .callingEnv)

  # full_call <- match_call_primitive(func, new_call, expand.dots = TRUE)

  if (is.null(.functionName))
    .functionName <- getFunctionName2(func_call)# as.character(normalized_FUN[[1]])

  # combined_args <- combine_clean_args(FUN, args, .objects, .callingEnv)

  preCacheDigestTime <- Sys.time()
  detailed_key <- CacheDigest(combined_args,
                              .functionName = .functionName,
                              .objects = .objects,
                              length = length, algo = algo, quick = quick,
                              classOptions = classOptions,
                              calledFrom = "Cache"
  )
  postCacheDigestTime <- Sys.time()
  elapsedTimeCacheDigest <- postCacheDigestTime - preCacheDigestTime

  .CacheVerboseFn1(detailed_key$preDigest, .functionName, preCacheDigestTime,
                   modifiedDots = combined_args, verbose = verbose, verboseLevel = 3)

  cache_key <- detailed_key$outputHash

  # Construct the full file path for the cache
  cachePaths <- getCacheRepos(cachePath, new_call[-1], verbose = verbose)
  cachePath <- cachePaths[[1]]
  cache_file <- CacheStoredFile(cachePath, cache_key)
  csd <- dirname(cache_file)

  if (!any(dir.exists(csd)))
    lapply(csd, dir.create, showWarnings = FALSE, recursive = TRUE)

  output <- check_and_get_memoised_copy(cache_key, cachePath, .functionName, func, useCache, verbose)
  if (!identical(output, returnNothing))
    return(output)

  lockFile <- file.path(csd, paste0(cache_key, suffixLockFile()))
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

  output <- check_and_get_cached_copy(cache_key, cachePath, cache_file, .functionName, func,
                                      useCache, verbose = verbose)
  if (!identical(output, returnNothing))
    return(output)

  preFUNTime <- Sys.time()

  if (!exists(".reproEnv2", envir = .pkgEnv)) {
    .pkgEnv$.reproEnv2 <- new.env(parent = asNamespace("reproducible"))
    .pkgEnv$.reproEnv2$userTags <- userTags
    .pkgEnv$.reproEnv2$nestLevel <- 1
    on.exit(rm(.reproEnv2, envir = .pkgEnv), add = TRUE)
  } else {
    userTags <- .pkgEnv$.reproEnv2$userTags <- c(.pkgEnv$.reproEnv2$userTags, userTags)
    .pkgEnv$.reproEnv2$nestLevel <- .pkgEnv$.reproEnv2$nestLevel + 1
  }

  output <- eval(new_call, envir = .callingEnv)
  if (is.function(output)) { # if is wasn't "captured", then it is just a function, so now use it on the ...
    output <- output(...)
  }

  postFUNTime <- Sys.time()
  elapsedTimeFUN <- postFUNTime - preFUNTime

  metadata <- metadata_define(detailed_key, output, .functionName, userTags,
                              .objects, length, algo, quick, classOptions,
                              elapsedTimeCacheDigest, elapsedTimeFUN)
  objectSize <- attr(metadata, "tags")$objectSize

  cacheIdIdentical <- cache_Id_Identical(metadata, cachePaths, cache_key)
  # Save the output to the cache

  # Can't save NULL with attributes
  if (is.null(output)) output <- "NULL"

  if (is.null(cacheIdIdentical)) {
    output <- addCacheAttr(output, .CacheIsNew = TRUE, outputHash = cache_key, func)
    outputToSave <- .wrap(output)
    metadata <- metadata_update(outputToSave, metadata, cache_key) # .wrap may have added tags

    # fs <- saveFilesInCacheFolder(outputToSave, cache_file[1], cachePath, cache_key)
    fs <- saveToCache(cachePath = cachePath, drv = NULL, conn = NULL,
                      obj = outputToSave, verbose = verbose, # cache_file[1],
                      cacheId = cache_key)
  } else {
    file.link(cacheIdIdentical, cache_file)
    .message$FileLinkUsed(ftL = cacheIdIdentical, fts = cache_file, verbose)
  }

  # Memoize the output by saving it in RAM
  if (getOption("reproducible.useMemoise"))
    assign(cache_key, output, envir = memoiseEnv(cachePath))

  metadata_file <- CacheDBFileSingle(cachePath = cachePath, cacheId = cache_key)

  dbfile <- CacheDBFile(cachePath, drv = NULL, conn = NULL)
  if (isTRUE(!file.exists(dbfile[1])))
    file.create(dbfile[1])

  messageCache(
    .message$SavingToCache(userTags = userTags, otsObjSize = objectSize,
                           functionName = .functionName,
                           cacheId = cache_key), verbose = verbose - 2)
  saveFilesInCacheFolder(metadata, metadata_file, cachePath = cachePath, cacheId = cache_key)
  .message$Saved(cachePath, cache_key, functionName = .functionName, verbose)

  # messageColoured("Computed result for: ", cache_key, verbose = verbose)
  if (identical(output, "NULL")) output <- NULL
  return(output)

}

#' Convert all ways of calling a function into canonical form, including defaults
#'
#' e.g., stats::rnorm(1) --> rnorm(n = 1, mean = 0, sd = 1)
harmonize_call <- function(call, usesDots, isSquiggly, .callingEnv) {
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
      if (func_call[[1]] == quote(`::`)) {
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
      fun <- parse(text = fun)
    }
    func <- eval(fun, envir = .callingEnv)
  }

  argsRm <- names(args) %in% names(.formalsCache)
  if (any(argsRm %in% TRUE))
    args <- args[!argsRm %in% TRUE]
  new_call <- as.call(c(func, args))
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

check_and_get_cached_copy <- function(cache_key, cachePath, cache_file, functionName, func, useCache, verbose) {

  # Check if the result is already cached
  cacheFileExists <- file.exists(cache_file) # could be length >1

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
    if (identical(useCache, "overwrite")) {
      clearCacheOverwrite(cachePath, cache_key, functionName, verbose)
    } else {
      format <- fileExt(cache_file)

      fe <- CacheDBFileSingle(cachePath = cachePath[which(cacheFileExists)], cacheId = cache_key, format = format)
      sc <- showCacheFast(cache_key, cachePath, dtFile = fe)

      .cacheMessageObjectToRetrieve(functionName, sc,
                                    cachePath, cacheId = cache_key, verbose = verbose)

      obj <- loadFile(cache_file[which(cacheFileExists)[1]])

      .cacheMessage(obj, functionName, fromMemoise = FALSE, verbose = verbose)
      output <- .unwrap(obj, cachePath = cachePath)
      if (isTRUE(changedSaveFormat)) {
        swapCacheFileFormat(wrappedObj = obj, cachePath = cachePath, drv = drv, conn = conn,
                            cacheId = cache_key, sameCacheID = sameCacheID,
                            newFile = cache_file_orig, verbose = verbose)
      }

      if (getOption("reproducible.useMemoise")) {
        cache_key_in_memoiseEnv <- exists(cache_key, envir = memoiseEnv(cachePath), inherits = FALSE)
        if (cache_key_in_memoiseEnv %in% FALSE)
          assign(cache_key, output, envir = memoiseEnv(cachePath))
      }

      if (!is.null(output))
        output <- addCacheAttr(output, .CacheIsNew = FALSE, outputHash = cache_key, func)

      attr(output, ".Cache")$newCache <- FALSE
      return(output)
    }

  }
  invisible(returnNothing)
}

returnNothing <- ".nothing"

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

check_and_get_memoised_copy <- function(cache_key, cachePath, functionName, func, useCache, verbose) {
  if (getOption("reproducible.useMemoise")) {
    cache_key_in_memoiseEnv <- exists(cache_key, envir = memoiseEnv(cachePath), inherits = FALSE)
    if (cache_key_in_memoiseEnv) {
      if (identical(useCache, "overwrite")) {
        clearCacheOverwrite(cachePath, cache_key, functionName, verbose)
      } else {

        fe <- CacheDBFileSingle(cachePath = cachePath, cacheId = cache_key)
        sc <- showCacheFast(cache_key, cachePath, dtFile = fe)
        .cacheMessageObjectToRetrieve(functionName, sc,
                                      cachePath, cacheId = cache_key, verbose = verbose)

        output <- get(cache_key, envir = memoiseEnv(cachePath))
        .cacheMessage(functionName = functionName, fromMemoise = TRUE, verbose = verbose)

        if (!is.null(output))
          output <- addCacheAttr(output, .CacheIsNew = FALSE, outputHash = cache_key, func)

        return(output)
      }
    }
  }
  return(invisible(returnNothing))
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
  if (isTRUE(as.numeric(metadata$tagValue[metadata$tagKey == "object.size"]) > .objectSizeMinForBig)) {
    orig <- getOption("reproducible.useDBI")
    if (isTRUE(orig)) {
      useDBI(FALSE, verbose = -2)
      on.exit(useDBI(orig, verbose = -2), add = TRUE)
    }
    for (cachePath in cachePaths) {
      allCache <- showCache(x = cachePath, verbose = -2)
      if (NROW(allCache)) {
        resultHash <- metadata$tagValue[metadata$tagKey == "resultHash"]
        alreadyExists <- allCache[allCache$tagKey == "resultHash" &
                                    allCache$tagValue %in% resultHash &
                                    allCache$cacheId != cache_key]
        if (NROW(alreadyExists)) {
          linkToCacheId <- alreadyExists[["cacheId"]][[1]]
        }
      }
    }
  }
  if (!is.null(linkToCacheId)) linkToCacheId <- CacheStoredFile(cachePath, linkToCacheId)
  linkToCacheId
}

metadata_define <- function(detailed_key, outputToSave, func_name, userTags,
                            .objects, length, algo, quick, classOptions,
                            elapsedTimeCacheDigest, elapsedTimeFUN) {

  objSize <- if (getOption("reproducible.objSize", TRUE)) sum(objSize(outputToSave)) else NA

  resultHash <- ""
  if (isTRUE(objSize > .objectSizeMinForBig)) {
    resultHash <- CacheDigest(outputToSave,
                              .objects = .objects,
                              length = length, algo = algo, quick = quick,
                              classOptions = classOptions, calledFrom = "Cache"
    )$outputHash
  }

  useCloud <- FALSE

  df <- unlist(
    .unlistToCharacter(unname(detailed_key[-1]), getOption("reproducible.showSimilarDepth", 3))
  )
  tagKey <- paste0(names(df), ":", as.character(df))
  fns <- Filenames(outputToSave)
  if (length(userTags)) {
    ut <- strsplit(userTags, split = ":")
    ll <- lapply(ut, tail, 1)
    names(ll) <- lapply(ut, head, 1)
    userTags <- ll
  }
  userTagsList <- c(
    list(FUN = func_name),
    userTags,
    list(class = class(outputToSave)[1]),
    list(object.size = format(as.numeric(objSize))),
    list(accessed = format(Sys.time())),
    list(inCloud = isTRUE(useCloud)),
    list(fromDisk = isTRUE(any(nchar(fns) > 0))),
    list(resultHash = resultHash),
    list(elapsedTimeDigest = format(elapsedTimeCacheDigest, units = "secs")),
    list(elapsedTimeFirstRun = format(elapsedTimeFUN, units = "secs")),
    list(preDigest = tagKey)
  )
  names(userTagsList)[1] <- "function"


  cache_key <- detailed_key$outputHash
  metadata <- userTagsListToDT(cache_key, userTagsList)
  attr(metadata, "tags")$objectSize <- objSize
  return(metadata)
}


userTagsListToDT <- function(cache_key, userTagsList) {
  userTagsList <- stack(userTagsList)
  metadataDT(cacheId = cache_key, tagKey = userTagsList$ind, tagValue = userTagsList$values)
}

as.call2 <- function(func, args) {
  as.call(c(as.name(deparse(func)), args))
}

skipCache <- function(FUN, ..., usesDots, .callingEnv) {
  if (isTRUE(usesDots)) {
    FUN(...)
  } else {
    eval(FUN, envir = .callingEnv)
  }
}

clearCacheOverwrite <- function(cachePath, cache_key, functionName, verbose) {
  clearCache(x = cachePath, cacheId = cache_key, ask = FALSE, conn = NULL, drv = NULL, verbose = verbose - 1)
  .message$overwriting(functionName, type = "function", verbose)
}
