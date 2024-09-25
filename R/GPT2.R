cache2 <- function(FUN, ..., .objects = NULL, .cacheExtra = NULL,
                   .functionName = NULL, algo = "xxhash64",
                   cachePath = NULL,
                   userTags = c(), omitArgs = NULL,
                   length = getOption("reproducible.length", Inf),
                   classOptions = list(),
                   quick = getOption("reproducible.quick", FALSE),
                   verbose = getOption("reproducible.verbose"),
                   .callingEnv = parent.frame()) {

  # This makes this act like useDBI = FALSE --> creates correct dbFile
  opts <- options(reproducible.useDBI = FALSE)
  on.exit(options(opts), add = TRUE)

  call <- match.call(expand.dots = TRUE)

  # Check if FUN is a call and matches the expected structure
  # if (length(call) == (2 + usesDots)) { # means it might have "quote" around it
  if (identical(call$FUN[[1]], quote(quote))) {
    # if (identical(call[[2]][[1]], quote(quote)))
      call$FUN <- as.list(call$FUN)[[-1]] # unquote it
  }

  usesDots <- sum(nzchar(names(call))) > 2
  new_call <- harmonize_call(call, usesDots, .callingEnv) # evaluated arguments
  func_call <- attr(new_call, ".Cache")$func_call         # not evaluated arguments
  func <- as.list(new_call)[[1]]
  args <- as.list(new_call)[-1]

  # full_call <- match_call_primitive(func, new_call, expand.dots = TRUE)
  FUN <- getMethodAll(func_call, .callingEnv)

  if (is.null(.functionName))
    .functionName <- getFunctionName2(func_call)# as.character(normalized_FUN[[1]])

  combined_args <- combine_clean_args(FUN, args, .objects, .callingEnv)

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

  cache_key <- detailed_key$outputHash

  # Construct the full file path for the cache
  cachePaths <- getCacheRepos(cachePath, new_call[-1], verbose = verbose)
  cachePath <- cachePaths[1]
  csd <- CacheStorageDir(cachePaths)
  cache_file <- CacheStoredFile(cachePath, cache_key)
  # cache_file <- file.path(csd, paste0(cache_key, ".rds"))

  if (!useDBI()) {
    dtFile <- CacheDBFileSingle(cachePath = cachePath, cacheId = cache_key)
    lockFile <- file.path(CacheStorageDir(cachePath = cachePath), paste0(cache_key, suffixLockFile()))
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

  output <- check_and_get_cached_copy(cache_key, cachePath, cache_file, verbose)
  if (!identical(output, returnNothing))
    return(output)

  if (!any(dir.exists(csd)))
    lapply(csd, dir.create, showWarnings = FALSE, recursive = TRUE)

  preFUNTime <- Sys.time()

  output <- eval(new_call, envir = .callingEnv)
  if (is.function(output)) { # if is wasn't "captured", then it is just a function, so now use it on the ...
    output <- output(...)
  }

  postFUNTime <- Sys.time()
  elapsedTimeFUN <- postFUNTime - preFUNTime

  metadata <- metadata_define(detailed_key, output, .functionName, userTags,
                              .objects, length, algo, quick, classOptions,
                              elapsedTimeCacheDigest, elapsedTimeFUN)

  cacheIdIdentical <- cache_Id_Identical(metadata, cachePaths, cache_key)
  # Save the output to the cache

  if (is.null(cacheIdIdentical)) {
    outputToSave <- .wrap(output)
    metadata <- metadata_update(outputToSave, metadata, cache_key) # .wrap may have added tags
    fs <- saveFilesInCacheFolder(outputToSave, cache_file[1], cachePath, cache_key)
    # saveRDS(outputToSave, cache_file[1])
    attr(output, ".Cache")$newCache <- TRUE
  } else {
    file.link(cacheIdIdentical, cache_file)
    messageColoured("Result Hash was same as: ", basename(cacheIdIdentical), "; creating link",
                    verbose = verbose)
  }

  # Memoize the output by saving it in RAM
  if (getOption("reproducible.useMemoise"))
    assign(cache_key, output, envir = memoiseEnv(cachePath))

  metadata_file <- CacheDBFileSingle(cachePath = cachePath, cacheId = cache_key)
  # metadata_file <- file.path(csd[1], paste0(cache_key, ".dbFile.rds"))

  dbfile <- CacheDBFile(cachePath, drv = NULL, conn = NULL)
  if (isTRUE(!file.exists(dbfile[1])))
    file.create(dbfile[1])

  saveFilesInCacheFolder(metadata, metadata_file, cachePath = cachePath, cacheId = cacheId)
  # saveDBFileSingle(metadata, cachePath, cache_key)
  # saveDBFileSingle(metadata, cachePath, cache_key)

  messageColoured("Computed result for: ", cache_key, verbose = verbose)
  return(output)

}

harmonize_call <- function(call, usesDots, .callingEnv) {
  # Capture the unevaluated call

  # Check if the first argument is a function call
  func_full <- NULL
  if (is.call(call[[2]])) {

    func_call <- call[[2]]  # This is the actual function call (e.g., stats::rnorm)
    # Extract the function without the package prefix
    if (is.call(func_call[[1]]) && func_call[[1]][[1]] == quote(`::`)) {
      func_full <- func_call[[1]]
      func <- func_call[[1]][[3]]  # Get the actual function name (e.g., rnorm)
      args <- as.list(func_call)[-1]  # Drop the function part
    } else {
      if (func_call[[1]] == quote(`::`)) {
        func_full <- func_call
        func <- func_call[[3]]  # Package prefix, using FUN as name only
        args <- as.list(call[-(1:2)])
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
          func_call <- normalize_call(func_call)
          func <- func_call[[1]]  # Extract the function for do.call (e.g., rnorm)
        }
        args <- as.list(func_call)[-1]
      }
    }
  } else if (identical(call[[2]], quote(do.call))) {
    browser() # need to use normalize_call
    # Special handling for do.call to return the function unevaluated
    func <- call[[3]]  # Extract the function for do.call (e.g., rnorm)
    args <- eval(call[[4]], envir = .callingEnv)  # Evaluate the argument list
  } else {
    func <- call[[2]]  # This is the function (e.g., rnorm)
    args <- as.list(call[-(1:2)])  # These are the arguments (e.g., 1)

    # Check for package prefix
    if (is.call(func) && func[[1]] == quote(`::`)) {
      func <- func[[3]]  # Get the actual function name (e.g., rnorm)
    }

    # Handle variable references in arguments
    args <- lapply(args, function(arg) {
      if (is.symbol(arg) || is.call(arg)) {
        return(arg)  # Keep the argument as-is without evaluating
      } else {
        return(arg)  # Return non-symbol arguments unchanged
      }
    })
  }

  args <- evaluate_args(args, envir = .callingEnv)
  if (is.call(func) || is.name(func)) {
    fun <- if (is.null(func_full)) func else func_full
    func <- eval(fun, envir = .callingEnv)
  }
  new_call <- as.call(c(func, args))
  full_call <- match_call_primitive(func, new_call, expand.dots = TRUE)
  func_call2 <- as.call(c(func_call[[1]], args))
  attr(full_call, ".Cache")$func_call <- func_call2
  # attr(new_call, ".Cache")$full_call <- full_call

  return(full_call)
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

check_and_get_cached_copy <- function(cache_key, cachePath, cache_file, verbose) {
  if (getOption("reproducible.useMemoise")) {
    cache_key_in_memoiseEnv <- exists(cache_key, envir = memoiseEnv(cachePath), inherits = FALSE)
    if (cache_key_in_memoiseEnv) {
      messageColoured("Returning memoized result for: ", cache_key, verbose = verbose)
      output <- get(cache_key, envir = memoiseEnv(cachePath))
      attr(output, ".Cache")$newCache <- FALSE
      return(output)
    }
  }

  # Check if the result is already cached
  cacheFileExists <- file.exists(cache_file) # could be length >1

  if (any(cacheFileExists)) {
    messageColoured("Returning cached result for: ", cache_key, verbose = verbose)
    output <- .unwrap(loadFile(cache_file[which(cacheFileExists)[1]]), cachePath = cachePath)
    if (getOption("reproducible.useMemoise"))
      if (cache_key_in_memoiseEnv %in% FALSE)
        assign(cache_key, output, envir = memoiseEnv(cachePath))
    attr(output, ".Cache")$newCache <- FALSE
    return(output)
  }
  invisible(returnNothing)
}

returnNothing <- ".nothing"

combine_clean_args <- function(FUN, args, .objects, .callingEnv) {
  defaults <- get_function_defaults(eval(FUN, .callingEnv))
  combined_args <- reorder_arguments(defaults, args)
  empties <- vapply(combined_args, function(ca) if (is.symbol(ca)) capture.output(ca) else "Normal", character(1))
  empties <- !nzchar(empties)
  # empties <- vapply(combined_args, is.name, FUN.VALUE = logical(1))
  if (isTRUE(any(empties)))
    combined_args <- combined_args[!empties]

  # Process the .objects argument using the helper function
  if (!is.null(.objects)) {
    combined_args <- filter_objects(combined_args, .objects)
  }

  combined_args$.FUN <- FUN
  combined_args
}

metadata_update <- function(outputToSave, metadata, cache_key) {
  userTagsExtra <- attr(outputToSave, "tags") # .wrap may have added tags
  if (!is.null(userTagsExtra)) {
    ut <- strsplit(userTagsExtra, split = ":")
    ll <- lapply(ut, tail, 1)
    names(ll) <- lapply(ut, head, 1)
    userTagsList <- ll
    metadata <- rbindlist(list(metadata, userTagsListToDT(cache_key, userTagsList)))
  }
  metadata
}

.minObjSize <- 1
