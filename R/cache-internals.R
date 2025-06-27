verboseCacheMessage <- function(preDigest, functionName,
                             startHashTime, modifiedDots, quick,
                             verbose = getOption("reproducible.verbose", 1),
                             verboseLevel = 1) {
  if (verbose >= verboseLevel) {
    preDigestUnlist <- .unlistToCharacter(preDigest, 4)
    verboseDF0(verbose, functionName, startHashTime)
    # verboseDF <- data.frame(
    #   functionName = functionName,
    #   component = "Hashing",
    #   elapsedTime = as.numeric(difftime(endHashTime, startHashTime, units = "secs")),
    #   units = "secs",
    #   stringsAsFactors = FALSE
    # )

    # hashObjectSize <- unlist(lapply(modifiedDots, objSize, recursive = FALSE, quick = TRUE))
    hashObjectSize <- if (getOption("reproducible.objSize", TRUE)) {
      unlist(lapply(modifiedDots, function(x) {
        unname(attr(objSize(x, quick = FALSE), "objSize"))
      }))
    } else {
      Map(modifiedDots, function(x) NA)
    }

    lengths <- unlist(lapply(preDigestUnlist, function(x) length(unlist(x))))
    hashDetails <- data.frame(
      objectNames = rep(names(preDigestUnlist), lengths),
      hashElements = names(unlist(preDigestUnlist)),
      hash = unname(unlist(preDigestUnlist)),
      stringsAsFactors = FALSE
    )
    preDigestUnlistNames <- unlist(lapply(
      strsplit(names(unlist(preDigestUnlist)), split = "\\."), # nolint
      function(x) paste0(tail(x, 2), collapse = ".")
    ))
    hashObjectSizeNames <- unlist(lapply(
      strsplit(names(hashObjectSize), split = "\\$"),
      function(x) paste0(tail(x, 2), collapse = ".")
    ))
    hashObjectSizeNames <- gsub("\\.y", replacement = "", hashObjectSizeNames)
    hashObjectSizeNames <- unlist(lapply(
      strsplit(hashObjectSizeNames, split = "\\."),
      function(x) paste0(tail(x, 2), collapse = ".")
    ))
    hashDetails$objSize <- NA
    hashDetails$objSize[preDigestUnlistNames %in% hashObjectSizeNames] <-
      hashObjectSize[hashObjectSizeNames %in% preDigestUnlistNames]

    if (exists("hashDetails", envir = .reproEnv, inherits = FALSE)) {
      .reproEnv$hashDetails <- rbind(.reproEnv$hashDetails, hashDetails)
    } else {
      .reproEnv$hashDetails <- hashDetails
      on.exit(
        {
          assign("hashDetailsAll", .reproEnv$hashDetails, envir = .reproEnv)
          messageDF(.reproEnv$hashDetails, colour = "blue", verbose = verbose, verboseLevel = verboseLevel)
          messageCache("The hashing details are available from .reproEnv$hashDetailsAll",
                       verbose = verbose, verboseLevel = verboseLevel
          )
          rm("hashDetails", envir = .reproEnv)
        },
        add = TRUE
      )
    }

    # if (exists("verboseTiming", envir = .reproEnv, inherits = FALSE)) {
    #   verboseDF$functionName <- paste0("  ", verboseDF$functionName)
    #   .reproEnv$verboseTiming <- rbind(.reproEnv$verboseTiming, verboseDF)
    # } else {
    #   .reproEnv$verboseTiming <- verboseDF
    # }
  }
}


.CacheFn1 <- function(FUN, scalls) {
  if (!is(FUN, "function")) {
    # scalls <- sys.calls()
    if (any(startsWith(as.character(scalls), "function_list[[k"))) {
      srch <- search()
      whereRepro <- which(endsWith(srch, "reproducible")) - 1
      if (whereRepro > 1) {
        srchNum <- seq_len(whereRepro)
        for (sr in srchNum) {
          masker <- exists("%>%", srch[sr], inherits = FALSE)
          if (masker) break
        }
      }
      if (masker) {
        stop("It looks like the pipe (%>%) from package:reproducible is masked by ", srch[sr],
          ". Please make sure library(reproducible) is after library(",
          gsub(srch[sr], pattern = "package:", replacement = ""), ")",
          call. = FALSE
        )
      } else {
        stop("Is the %>% from reproducible masked?")
      }
    } else {
      stop(
        "Can't understand the function provided to Cache.\n",
        "Did you write it in the form: ",
        "Cache(function, functionArguments)?"
      )
    }
  } else {
    scalls <- NULL
  }

  scalls
}

.getFromRepo <- function(FUN, isInRepo, fullCacheTableForObj,
                         notOlderThan, lastOne, cachePath, fnDetails,
                         modifiedDots, debugCache, verbose, # sideEffect,
                         quick, # fileFormat = NULL,
                         algo, preDigest, startCacheTime,
                         drv = getDrv(getOption("reproducible.drv", NULL)),
                         conn = getOption("reproducible.conn", NULL), ...) {
  cacheObj <- isInRepo[[.cacheTableHashColName()]][lastOne]

  fromMemoise <- NA
  output <- loadFromCache(cachePath, isInRepo[[.cacheTableHashColName()[lastOne]]],
    fullCacheTableForObj = fullCacheTableForObj,
    .functionName = fnDetails$functionName, preDigest = preDigest, .dotsFromCache = modifiedDots,
    drv = drv, conn = conn,
    verbose = verbose
  )
  # This is protected from multiple-write to SQL collisions
  .addTagsRepo(
    cacheId = isInRepo[[.cacheTableHashColName()]][lastOne],
    cachePath = cachePath, drv = drv, conn = conn
  )
  if (length(debugCache)) {
    if (!is.na(pmatch(debugCache, "complete")) || isTRUE(debugCache)) {
      output <- .debugCache(output, preDigest, ...)
    }
  }

  output <- .setSubAttrInList(output, ".Cache", "newCache", FALSE)
  # attr(output, ".Cache")$newCache <- FALSE
  if (!identical(attr(output, ".Cache")$newCache, FALSE)) stop("attributes are not correct 2")

  # verboseDF3(verbose, fnDetails$functionName, startCacheTime)

  # If it was a NULL, the cachePath stored it as "NULL" ... return it as NULL
  if (is.character(output)) {
    if (identical(as.character(output), "NULL")) {
      output <- NULL
    }
  }

  return(output)
}
