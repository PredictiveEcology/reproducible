# Generic caching function with omitArgs, .objects, and cachePath
cache <- function(FUN, ...,
                  .objects = NULL, .cacheExtra = NULL,
                  .functionName = NULL, algo = "xxhash64",
                  cachePath = NULL,
                  userTags = c(), omitArgs = NULL,
                  length = getOption("reproducible.length", Inf),
                  classOptions = list(),
                  quick = getOption("reproducible.quick", FALSE),
                  verbose = getOption("reproducible.verbose")) {

  opts <- options(reproducible.useDBI = FALSE)
  on.exit(options(opts), add = TRUE)

  callingEnv <- parent.frame(1)
  # Convert FUN to a standard call format if it is a function
  FUNcaptured <- FUNcapturedOrig <- substitute(FUN)

  dotsCaptured <- as.list(substitute(list(...))[-1])

  if (length(FUNcaptured) == 1 || (isDollarSqBrPkgColon(FUNcaptured) && (length(dotsCaptured)))) {
    FUNcaptured <- as.call2(substitute(FUNcaptured), dotsCaptured)
    # FUNcaptured <- as.call(c(as.name(deparse(substitute(FUNcaptured))), dotsCaptured))
  } else {
    dotsCaptured <- append(as.list(FUNcaptured[-1]), dotsCaptured)
    FUNcaptured <- as.call2(FUNcaptured[[1]], dotsCaptured)
    # FUNcaptured <- as.call(c(as.name(deparse(FUNcaptured[[1]])), dotsCaptured))
  }
  # }

  # Normalize the FUNcaptured to handle `do.call` and `quote`, `bquote` etc.
  isSquiggly <- identical(quote(`{`), FUNcaptured[[1]])
  if (isTRUE(isSquiggly))
    FUNcaptured <- as.list(FUNcaptured[-1])[[1]]

  if (is.call(FUNcaptured) &&
      any(as.character(FUNcaptured[[1]]) %in% c("quote", "bquote", "alist", "enquote", "expr", "exprs"))) {
    FUNcaptured <- FUNcaptured[[2]]
  }

  normalized_FUN <- normalize_call(FUNcaptured) # remove do.call
  # Extract the function from the normalized call and normalize its name
  func <- extract_function(normalized_FUN, envir = callingEnv)

  # Create a call with the same function but with matched arguments
  full_call <- match_call_primitive(func, normalized_FUN, expand.dots = TRUE)

  func_name <- getFunctionName2(full_call)# as.character(normalized_FUN[[1]])

  # Extract arguments as a named list
  arg_list <- as.list(full_call)[-1]

  # Get function defaults
  FUN <- getMethodAll(full_call, callingEnv)

  defaults <- get_function_defaults(eval(FUN, callingEnv))

  # Reorder arguments according to function defaults, combining user-supplied args and defaults
  combined_args <- reorder_arguments(defaults, arg_list)

  empties <- vapply(combined_args, function(ca) if (is.symbol(ca)) capture.output(ca) else "Normal", character(1))
  empties <- !nzchar(empties)
  # empties <- vapply(combined_args, is.name, FUN.VALUE = logical(1))
  if (isTRUE(any(empties)))
    combined_args <- combined_args[!empties]

  normalized_FUN_w_defaults <- do.call(call, append(list(func_name), combined_args), quote = TRUE)

  # Evaluate all arguments once and store them in a named list
  if (isSquiggly) {
    FUNcaptured <- recursiveEvalNamesOnly(normalized_FUN_w_defaults, envir = callingEnv) # deals with e.g., stats::rnorm, b$fun, b[[fun]]
    evaluated_args <- as.list(FUNcaptured[-1])
  } else {
    evaluated_args <- evaluate_arguments(combined_args, envir = callingEnv)
    FUNcaptured <- as.call(append(list(func), evaluated_args))
  }

  FUN <- getMethodAll(FUNcaptured, callingEnv)

  # If omitArgs is provided, remove those arguments from arg_list and FUNcaptured
  if (!is.null(omitArgs)) {
    # combined_args <- combined_args[!names(combined_args) %in% omitArgs]
    FUNcaptured <- FUNcaptured[!names(FUNcaptured) %in% omitArgs]
  }

  # Process the .objects argument using the helper function
  if (!is.null(.objects)) {
    evaluated_args <- filter_objects(evaluated_args, .objects)
  }

  # Create a detailed cache key based on the filtered arguments
  evaluated_args$.FUN <- func

  if (!is.null(.functionName))
    func_name <- .functionName
  preCacheDigestTime <- Sys.time()
  detailed_key <- CacheDigest(evaluated_args,
                              .functionName = func_name,
                              .objects = .objects,
                              length = length, algo = algo, quick = quick,
                              classOptions = classOptions, calledFrom = "Cache"
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
  cachePaths <- getCacheRepos(cachePath, dotsCaptured, verbose = verbose)

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

  # Evaluate the original call
  preFUNTime <- Sys.time()
  outputToSave <- eval(FUNcaptured, envir = callingEnv)
  postFUNTime <- Sys.time()
  elapsedTimeFUN <- postFUNTime - preFUNTime

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
normalize_call <- function(call) {
  if (is.call(call) && all(as.character(call[[1]]) == "do.call")) {
    func <- call[[2]]
    args <- call[[3]]

    if (is.call(args) && as.character(args[[1]]) == "list") {
      args <- as.list(args[-1])
    }

    return(as.call2(func, args))
    # return(as.call(c(as.name(deparse(func)), args)))
  } else {
    func <- call[[1]]
    args <- as.list(call[-1])
  }
  # call <- as.call2(call[[1]], as.list(call[-1]))
  return(as.call2(func, args))
  # return(call)
}

# Helper function to extract function from different types of inputs
extract_function <- function(call, envir = parent.frame()) {

  # eval(parse(text = call[[1]]), envir = parent.frame(2))

  # call <- as.call(c(eval(call[[1]], envir = parent.frame(2)), as.list(call[-1])))

  if (is.call(call)) {
    func <- call[[1]]

    if (is.call(func)) {
      return(eval(func, envir = envir))
    }

    # Handle function with or without package prefix
    if (is.symbol(func) || is.name(func)) {
      func_name <- as.character(func)
      out <- tryCatch(eval(parse(text = func_name), envir),
                      error = function(fn) eval(func, envir = envir))
      return(out)
    } else if (is.call(func)) {
      func_name <- as.character(func[[1]])
      if (func_name == "do.call") {
        return(eval(func[[2]], envir))
      } else {
        return(eval(parse(text = func_name), envir))
      }
    }
  }

  return(call)
}

# Helper function to evaluate arguments
evaluate_arguments <- function(arg_list, envir) {
  evaluated_args <- lapply(arg_list, function(arg) {
    tryCatch({
      eval(arg, envir = envir)
    }, error = function(e) {
      arg  # Return original argument if it cannot be evaluated
    })
  })
  names(evaluated_args) <- names(arg_list)
  return(evaluated_args)
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
  ordered_args <- combined_args[names(formals)]

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

    func_name <- deparse(substitute(definition))

    # For other primitives, match as best as possible
    args <- as.list(call)[-1]  # remove the function name
    if (expand.dots) {
      args <- lapply(args, eval, envir = envir)
    }
    # Construct the matched call manually for primitive
    matched <- as.call(c(as.name(func_name), args))
    return(matched)
  } else {
    # Non-primitive function: fall back to regular match.call
    return(base::match.call(definition = definition,
                            call = call,
                            expand.dots = expand.dots,
                            envir = envir))
  }
}


cacheIdIdentical <- function(metadata, cachePaths, cache_key) {
  linkToCacheId <- NULL
  if (isTRUE(as.numeric(metadata$tagValue[metadata$tagKey == "object.size"]) > 1e6)) {
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
  if (isTRUE(objSize > 1e6)) {
    resultHash <- CacheDigest(outputToSave,
                              .objects = .objects,
                              length = length, algo = algo, quick = quick,
                              classOptions = classOptions, calledFrom = "Cache"
    )$outputHash
  }

  useCloud <- FALSE

  df <- stack(detailed_key[-1], stringsAsFactor = FALSE)
  tagKey <- paste0(rownames(df), ":", as.character(df$values))
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
    # paste0(otherFns),
    # grep("cacheId", attr(outputToSave, "tags"), invert = TRUE, value = TRUE),
    list(preDigest = tagKey)
  ) # |> stack()


  cache_key <- detailed_key$outputHash
  metadata <- userTagsListToDT(cache_key, userTagsList)
  return(metadata)
}


userTagsListToDT <- function(cache_key, userTagsList) {
  userTagsList <- stack(userTagsList)
  metadata <- data.table(
    cacheId = cache_key,
    tagKey = userTagsList$ind,
    tagValue = userTagsList$values,
    createdDate = Sys.time(),
    stringsAsFactors = FALSE
  )
}

as.call2 <- function(func, args) {
  as.call(c(as.name(deparse(func)), args))
}
