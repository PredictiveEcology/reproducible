.CacheVerboseFn1 <- function(preDigest, fnDetails,
                             startHashTime, modifiedDots, quick,
                             verbose = getOption("reproducible.verbose", 1)) {
  preDigestUnlist <- .unlistToCharacter(preDigest, 4)
  endHashTime <- Sys.time()
  verboseDF <- data.frame(
    functionName = fnDetails$functionName,
    component = "Hashing",
    elapsedTime = as.numeric(difftime(endHashTime, startHashTime, units = "secs")),
    units = "secs",
    stringsAsFactors = FALSE
  )

  hashObjectSize <- unlist(lapply(modifiedDots, function(x) {
    objSize <- objSize(x, quick = quick)
  }))

  lengths <- unlist(lapply(preDigestUnlist, function(x) length(unlist(x))))
  hashDetails <- data.frame(
    objectNames = rep(names(preDigestUnlist), lengths),
    #objSize = rep(hashObjectSize, lengths),
    hashElements = names(unlist(preDigestUnlist)),
    hash = unname(unlist(preDigestUnlist)),
    stringsAsFactors = FALSE
  )
  preDigestUnlistNames <- unlist(lapply(strsplit(names(unlist(preDigestUnlist)), split = "\\."), #nolint
                                        function(x) paste0(tail(x, 2), collapse = ".")))
  hashObjectSizeNames <- unlist(lapply(strsplit(names(hashObjectSize), split = "\\$"),
                                       function(x) paste0(tail(x, 2), collapse = ".")))
  # hashObjectSizeNames <- unlist(lapply(strsplit(hashObjectSizeNames, split = "\\.y"),
  #                                      function(x) paste0(tail(x, 2), collapse = ".")))
  hashObjectSizeNames <- gsub("\\.y", replacement = "", hashObjectSizeNames)
  hashObjectSizeNames <- unlist(lapply(strsplit(hashObjectSizeNames, split = "\\."),
                                       function(x) paste0(tail(x, 2), collapse = ".")))
  hashDetails$objSize <- NA
  hashDetails$objSize[preDigestUnlistNames %in% hashObjectSizeNames] <-
    hashObjectSize[hashObjectSizeNames %in% preDigestUnlistNames]

  if (exists("hashDetails", envir = .reproEnv)) {
    .reproEnv$hashDetails <- rbind(.reproEnv$hashDetails, hashDetails)
  } else {
    .reproEnv$hashDetails <- hashDetails
    on.exit({
      assign("hashDetailsAll", .reproEnv$hashDetails, envir = .reproEnv)
      messageDF(.reproEnv$hashDetails, colour = "blue")
      messageCache("The hashing details are available from .reproEnv$hashDetails", verbose = verbose)
      rm("hashDetails", envir = .reproEnv)
    }, add = TRUE)
  }

  if (exists("verboseTiming", envir = .reproEnv)) {
    verboseDF$functionName <- paste0("  ", verboseDF$functionName)
    .reproEnv$verboseTiming <- rbind(.reproEnv$verboseTiming, verboseDF)
  } else {
    .reproEnv$verboseTiming <- verboseDF
  }
}






.getFromRepo <- function(FUN, isInRepo, notOlderThan, lastOne, cacheRepo, fnDetails,
                         modifiedDots, debugCache, verbose, sideEffect, quick,
                         algo, preDigest, startCacheTime,
                         drv = getOption("reproducible.drv"),
                         conn = getOption("reproducible.conn", NULL), ...) {
  if (verbose > 3) {
    startLoadTime <- Sys.time()
  }

  cacheObj <- isInRepo[[.cacheTableHashColName()]][lastOne]

  fromMemoise <- NA
  if (getOption("reproducible.useMemoise")) {
    fromMemoise <- FALSE
    if (!is.null(.pkgEnv[[cacheRepo]]))
      if (exists(cacheObj, envir = .pkgEnv[[cacheRepo]]))
        fromMemoise <- TRUE
    loadFromMgs <- "Loading from memoised version of repo"
    output <- .loadFromLocalRepoMem(md5hash = cacheObj, cacheRepo = cacheRepo, value = TRUE)
    output <- unmakeMemoisable(output)
    #if (is(output, "simList_")) output <- as(output, "simList")
  } else {
    loadFromMgs <- "Loading from repo"
    output <- loadFromCache(cacheRepo, isInRepo[[.cacheTableHashColName()[lastOne]]],
                            sideEffect = sideEffect,
                            drv = drv, conn = conn)

  }

  if (verbose > 3) {
    endLoadTime <- Sys.time()
    verboseDF <- data.frame(
      functionName = fnDetails$functionName,
      component = loadFromMgs,
      elapsedTime = as.numeric(difftime(endLoadTime, startLoadTime, units = "secs")),
      units = "secs",
      stringsAsFactors = FALSE
    )

    if (exists("verboseTiming", envir = .reproEnv)) {
      .reproEnv$verboseTiming <- rbind(.reproEnv$verboseTiming, verboseDF)
    }
  }

  # Class-specific message
  # browser(expr = exists("dddd"))
  .cacheMessage(output, fnDetails$functionName, fromMemoise = fromMemoise, verbose = verbose)

  # This is protected from multiple-write to SQL collisions
  # .addTagsRepo(isInRepo, cacheRepo, lastOne, drv, conn = conn)
  .addTagsRepo(cacheId = isInRepo[[.cacheTableHashColName()]][lastOne],
               cachePath = cacheRepo, drv = drv, conn = conn)

  # This allows for any class specific things
  output <- if (fnDetails$isDoCall) {
    do.call(.prepareOutput, args = append(list(output, cacheRepo), modifiedDots$args))
  } else {
    do.call(.prepareOutput, args = append(list(output, cacheRepo), modifiedDots))
  }

  if (length(debugCache)) {
    if (!is.na(pmatch(debugCache, "complete")) | isTRUE(debugCache))
      #output <- do.call(.debugCache, args = append(list(output, preDigest), modifiedDots$args))
      output <- .debugCache(output, preDigest, ...)
  }

  .setSubAttrInList(output, ".Cache", "newCache", FALSE)
  #attr(output, ".Cache")$newCache <- FALSE
  if (!identical(attr(output, ".Cache")$newCache, FALSE)) stop("attributes are not correct 2")

  if (verbose > 3) {
    endCacheTime <- Sys.time()
    verboseDF <- data.frame(
      functionName = fnDetails$functionName,
      component = "Whole Cache call",
      elapsedTime = as.numeric(difftime(endCacheTime, startCacheTime, units = "secs")),
      units = "secs",
      stringsAsFactors = FALSE)

    if (exists("verboseTiming", envir = .reproEnv)) {
      .reproEnv$verboseTiming <- rbind(.reproEnv$verboseTiming, verboseDF)
    }
  }

  # If it was a NULL, the cacheRepo stored it as "NULL" ... return it as NULL
  if (is.character(output)) {
    if (identical(as.character(output), "NULL"))
      output <- NULL
  }

  return(output)
}
