cache2 <- function(FUN, ..., .objects = NULL, .cacheExtra = NULL,
                   .functionName = NULL, algo = "xxhash64",
                   cachePath = NULL,
                   userTags = c(), omitArgs = NULL,
                   length = getOption("reproducible.length", Inf),
                   classOptions = list(),
                   quick = getOption("reproducible.quick", FALSE),
                   verbose = getOption("reproducible.verbose"),
                   .callingEnv = parent.frame()) {
  call <- match.call(expand.dots = TRUE)

  # Check if FUN is a call and matches the expected structure
  if (is.call(FUN) && length(call) == 2) { # means it has "quote" around it
    call[[2]] <- as.call(FUN)
  }
  usesDots <- length(nzchar(names(call))) > 2

  new_call <- harmonize_call(call, usesDots, .callingEnv)

  args <- as.list(new_call)[-1]

  FUN <- getMethodAll(new_call, .callingEnv)
  defaults <- get_function_defaults(eval(FUN, .callingEnv))
  combined_args <- reorder_arguments(defaults, args)

  # Process the .objects argument using the helper function
  if (!is.null(.objects)) {
    combined_args <- filter_objects(combined_args, .objects)
  }

  combined_args$.FUN <- FUN

  preCacheDigestTime <- Sys.time()
  detailed_key <- CacheDigest(combined_args,
                              # .functionName = func_name,
                              .objects = .objects,
                              # length = length, algo = algo, quick = quick,
                              # classOptions = classOptions,
                              calledFrom = "Cache"
  )
  postCacheDigestTime <- Sys.time()
  elapsedTimeCacheDigest <- postCacheDigestTime - preCacheDigestTime

  # Digest the detailed cache key to shorten it
  cache_key <- detailed_key$outputHash

  if (getOption("reproducible.useMemoise"))
    if (exists(cache_key, envir = memoiseEnv(cachePath), inherits = FALSE)) {
      messageColoured("Returning memoized result for: ", cache_key, verbose = verbose)
      output <- get(cache_key, envir = memoiseEnv(cachePath))
      attr(output, ".Cache")$newCache <- FALSE
      return(output)
    }

  # Construct the full file path for the cache
  cachePaths <- getCacheRepos(cachePath, new_call[-1], verbose = verbose)

  csd <- CacheStorageDir(cachePaths)
  lapply(csd, dir.create, showWarnings = FALSE, recursive = TRUE)
  cache_file <- file.path(csd, paste0(cache_key, ".rds"))

  # Check if the result is already cached
  cacheFileExists <- file.exists(cache_file) # could be length >1

  if (any(cacheFileExists)) {
    messageColoured("Returning cached result for: ", cache_key, verbose = verbose)
    output <- .unwrap(readRDS(cache_file[which(cacheFileExists)[1]]), cachePath = cachePath)
    if (getOption("reproducible.useMemoise"))
      assign(cache_key, output, envir = memoiseEnv(cachePath))
    attr(output, ".Cache")$newCache <- FALSE
    return(output)
  }

  preFUNTime <- Sys.time()

  outputToSave <- eval(new_call, envir = .callingEnv)
  if (is.function(outputToSave)) { # if is wasn't "captured", then it is just a function, so now use it on the ...
    outputToSave <- outputToSave(...)
  }

  postFUNTime <- Sys.time()
  elapsedTimeFUN <- postFUNTime - preFUNTime

  func_name <- getFunctionName2(new_call)# as.character(normalized_FUN[[1]])

  metadata <- metadata_define(detailed_key, outputToSave, func_name, userTags,
                              .objects, length, algo, quick, classOptions,
                              elapsedTimeCacheDigest, elapsedTimeFUN)

  cacheIdIdentical <- cacheIdIdentical(metadata, cachePaths, cache_key)
  # Save the outputToSave to the cache

  if (is.null(cacheIdIdentical)) {
    ots <- .wrap(outputToSave)
    userTags <- attr(ots, "tags")
    if (!is.null(userTags)) {
      ut <- strsplit(userTags, split = ":")
      ll <- lapply(ut, tail, 1)
      names(ll) <- lapply(ut, head, 1)
      userTagsList <- ll
      metadata <- rbindlist(list(metadata, userTagsListToDT(cache_key, userTagsList)))
    }
    saveRDS(ots, cache_file[1])
    attr(outputToSave, ".Cache")$newCache <- TRUE
  } else {
    file.link(cacheIdIdentical, cache_file)
    messageColoured("Result Hash was same as: ", cacheIdIdentical, "; creating link",
                    verbose = verbose)
  }

  # Memoize the output by saving it in RAM
  if (getOption("reproducible.useMemoise"))
    assign(cache_key, outputToSave, envir = memoiseEnv(cachePath))

  metadata_file <- file.path(csd[1], paste0(cache_key, ".dbFile.rds"))

  dbfile <- CacheDBFile(cachePaths, drv = NULL, conn = NULL)
  if (isTRUE(!file.exists(dbfile[1])))
    file.create(dbfile[1])

  saveRDS(metadata, metadata_file)

  messageColoured("Computed result for: ", cache_key, verbose = verbose)
  return(outputToSave)

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
          func <- func_call[[2]]  # Extract the function for do.call (e.g., rnorm)
          args <- func_call[[3]][[-1]]
        } else {
          args <- as.list(func_call)[-1]  # Drop the function part
        }
      }
    }
  } else if (identical(call[[2]], quote(do.call))) {
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
  new_call <- match.call(call = new_call, definition = func, envir = .callingEnv)
  return(new_call)
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
