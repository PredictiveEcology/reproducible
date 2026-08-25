## Cache(): the package's main entry point, plus the orchestration around it --
## nesting, chaining, dev mode, and recovery from a corrupt cache entry.
##
## NOTE: .formalsCache/.formalscache2 and .namesCacheFormalsSendToBoth are
## evaluated at BUILD time from formals(Cache), so they must stay in this file,
## after Cache() and cache2(). Splitting them across files would make the build
## depend on Collate ordering.

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

  ## Lazy showCache async pre-populate, but ONLY when this call will actually
  ## consume it. showSimilar=TRUE is the sole Cache() path that calls showCache()
  ## (which harvests + reaps the fork at showCacheEtc.R). With the default
  ## showSimilar=FALSE nothing ever harvests it, so an unconditional spawn leaks
  ## one background process per cachePath for the life of the session -- measured
  ## at ~50 lingering forks / ~46 GB across the test suite, which OOM-kills small
  ## CI runners (exit 143). Direct showCache() users can still pre-warm explicitly
  ## via prepopulateCacheAsync(). The DBI backend is skipped inside the helper
  ## (indexed query, nothing to pre-warm). Targets the cachePath actually being
  ## used (not the default at .onLoad time). The helper is also a hard off-switch
  ## via options(reproducible.showCachePreWarm = FALSE) -- see reproducibleOptions()
  ## and the note in .maybeSpawnShowCacheAsync().
  if (isTRUE(showSimilar)) .maybeSpawnShowCacheAsync(cachePath)

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

  # Open a transient URL frame so inner prepInputs/preProcess calls can push
  # url accesses up to this Cache for attribution to this cacheId. on.exit
  # makes interrupted Cache calls clean up. See R/urlLog.R.
  .urlFrameId <- .openCacheUrlFrame()
  on.exit(.closeCacheUrlFrame(.urlFrameId), add = TRUE)
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
    if (!identical2(.returnNothing, outputFromMemoise)) {
      .maybeRecordUrlForCache(callList, keyFull, cachePaths, drv, conn,
                              isHit = TRUE, .callingEnv = .callingEnv,
                              urlFrameId = .urlFrameId)
      return(outputFromMemoise)
    }

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

    if (!identical2(.returnNothing, outputFromDisk)) {
      .maybeRecordUrlForCache(callList, keyFull, cachePaths, drv, conn,
                              isHit = TRUE, .callingEnv = .callingEnv,
                              urlFrameId = .urlFrameId)
      return(outputFromDisk)
    }

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
    .maybeRecordUrlForCache(callList, keyFull, cachePaths, drv, conn,
                            isHit = TRUE, .callingEnv = .callingEnv,
                            urlFrameId = .urlFrameId)
    return(outputFromDisk)
  } # Derive some metadata prior to evaluation so "showSimilar" can have something to compare with

  times$EvaluateStart <- Sys.time()
  metadata <- metadata_define_preEval(keyFull, callList$.functionName, userTags,
                                      .objects, length, algo, quick, classOptions,
                                      times$EvaluateStart, times$CacheDigestStart)

  if (isTRUE(showSimilar) || isDevMode(useCache, userTags) || isTRUE(dryRun)) {
    if (dryRun) messageColoured(.txtDryRunTRUE, colour = "green")
    showSimilar(cachePaths[[1]], metadata, callList$.functionName, userTags, useCache,
                useCloud = useCloud, cloudFolderID = cloudFolderID,
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
  .maybeRecordUrlForCache(callList, keyFull, cachePaths, drv, conn,
                          isHit = FALSE, .callingEnv = .callingEnv,
                          urlFrameId = .urlFrameId)
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


.returnNothing <- ".nothing"


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
              stop(
                "Internal error: cache chaining resolved more than one candidate ",
                "cacheId (", paste(cidToCheck, collapse = ", "), "). Please report ",
                "this at https://github.com/PredictiveEcology/reproducible/issues"
              )
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
        stop(
          "Failed to restore file-backed cache object(s) from the cache: ",
          paste(fns[fnsExistAfter %in% FALSE], collapse = ", ")
        )
    }
  }
  memoiseFail
}

utils::globalVariables(c(
  ".", "artifact", "createdDate", "deeperThan3", "differs", "fun", "hash",
  "i.hash", "iden", "N", "tag", "tagKey", "tagValue"
))


.reproEnv <- new.env(parent = asNamespace("reproducible"))


#' Saves a wide variety function call outputs to disk and optionally RAM, for recovery later
#'
#' @description
#' `r lifecycle::badge("stable")`
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
#' The algorithm used to compute the `cacheId` (the hash of the inputs) is
#' selectable via the `reproducible.digestVersion` option; see the
#' `digestVersion` entry in [reproducibleOptions()] for the available versions
#' and what each changes (notably version `4`, which makes `sf`/`SpatVector`
#' digests identical across operating systems).
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
#' Two character values are also accepted, intended for separating developer
#' and user roles when sharing a cloud-cache folder:
#'
#' - `"push"` is equivalent to `TRUE` (developer role) -- bidirectional;
#'   downloads on a cloud hit, uploads on a miss.
#' - `"pull"` is read-only (user role) -- downloads on a cloud hit, but never
#'   uploads. If the local cache already has the object, the cloud is not
#'   consulted at all (the Google Drive listing is deferred until after the
#'   local lookup fails). When neither local nor cloud has the object, the
#'   call falls back to a normal local-only Cache run.
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
#' @param .cacheChaining A logical or a the name of a function. If `TRUE`, then
#'   the current `Cache` call will evaluate the function "outside" the `Cache` call
#'   (via `sys.function(-1)`) and
#'   attach the `digest` of that outer function to the entry for this `Cache` call. This
#'   will then be used by any subsequent `Cache` call within the same function.
#'   If the outer function is unchanged, and there is one or more objects that had
#'   been returned by a previous `Cache` call,
#'   then those objects will not be digested; rather their `cacheId` tag will be used
#'   in place of a new `digest`. This *should* cause no change in Caching outcomes,
#'   and it should be faster in cases where there are several `Cache` calls within
#'   the same function. If `FALSE` (current default), then this feature is
#'   not used. If set to `NULL` (i.e., unset, the current default), then it will
#'   not use cache chaining, but it will attach more information to the Cache entries
#'   for each `cacheId`, as well as new entries for `"surroundingFunction"` digest,
#'   so that if a user switches to `.cacheChaining = TRUE`, then it will be able
#'   to begin using cache chaining without needing to rerun the calls again. Can be set by an `option`.
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
#' @param omitArgs Optional. A character vector of argument names in `FUN` to
#'   omit from the cache digest, or `TRUE` to omit *every* captured argument
#'   (the digest is then based on `FUN` itself -- including its body, so a
#'   meaningful edit to the function source still busts the cache -- and on
#'   `.cacheExtra`). Useful when the developer wants the cache to be
#'   insensitive to the function's inputs and pin freshness via `.cacheExtra`
#'   instead.
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
#' @param useCloud Logical (`TRUE` / `FALSE` / `NULL`) or one of `"pull"` /
#'   `"push"`. See Details.
#' @param cacheSaveFormat Character string: currently either `qs` or `rds`. Defaults to
#'    `getOption("reproducible.cacheSaveFormat")`. `qs` may be faster but appears to have
#'    narrower range of conditions that work; `rds` is safer, and may be slower.
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
#'   as used internally within `Cache`, [movedCache()], [.robustDigest()],
#'   [reproducibleOptions()] (e.g. the `digestVersion` option that selects the
#'   `cacheId` algorithm), and
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
CacheV2 <-
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

    .Defunct("Cache")
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




#' Write to cache repository, using `future::future` (defunct)
#'
#' @description
#' `r lifecycle::badge("defunct")`
#'
#' Defunct as of version 3.2.0. The future-based cache writing this supported was
#' never enabled by default -- `reproducible.futurePlan` defaults to `FALSE` --
#' and has been removed. [Cache()] writes directly.
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
#' Nothing; it signals a defunct error.
writeFuture <- function(written, outputToSave, cachePath, userTags,
                        drv = getDrv(getOption("reproducible.drv", NULL)),
                        conn = getOption("reproducible.conn", NULL),
                        cacheId, linkToCacheId = NULL,
                        verbose = getOption("reproducible.verbose")) {
  .Defunct(msg = paste0(
    "'writeFuture()' is defunct as of version 3.2.0. The future-based cache ",
    "writing it supported was never enabled by default and has been removed; ",
    "'Cache()' writes directly."))
}





#' @keywords internal
.defaultCacheOmitArgs <- c(
  "useCloud", "checksumsFileID", "cloudFolderID",
  "notOlderThan", ".objects", "outputObjects", "algo", "cachePath",
  "length", "compareRasterFileLength", "userTags", "digestPathContent",
  "omitArgs", "classOptions", "debugCache", "sideEffect", "makeCopy",
  "quick", "verbose", "cacheId", "useCache", "showSimilar", "cl"
)


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


browserCond <- function(expr) {
  any(startsWith(ls(.GlobalEnv), expr))
}


callInCache <- "callInCache"
