.CacheVerboseFn1 <- function(preDigestUnlist, fnDetails,
                             startHashTime, modifiedDots, dotPipe, quick) {
  endHashTime <- Sys.time()
  verboseDF <- data.frame(
    functionName = fnDetails$functionName,
    component = "Hashing",
    elapsedTime = as.numeric(difftime(endHashTime, startHashTime, units = "secs")),
    units = "secs",
    stringsAsFactors = FALSE
  )

  hashObjectSize <- unlist(lapply(modifiedDots[!dotPipe], function(x) {
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
      print(.reproEnv$hashDetails)
      message("The hashing details are available from .reproEnv$hashDetails")
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


.CachePipeFn1 <- function(modifiedDots, fnDetails, FUN) {
  if (!is.call(modifiedDots$._lhs)) {
    # usually means it is the result of a pipe
    modifiedDots$._pipeFn <- "constant" # nolint
  }

  pipeFns <- paste(lapply(modifiedDots$._rhss, function(x) x[[1]]), collapse = ", ") %>%
    paste(modifiedDots$._pipeFn, ., sep = ", ") %>%
    gsub(., pattern = ", $", replacement = "") %>%
    paste0("'", ., "' pipe sequence")

  fnDetails$functionName <- pipeFns
  if (is.function(FUN)) {
    firstCall <- match.call(FUN, modifiedDots$._lhs)
    modifiedDots <- append(modifiedDots, lapply(as.list(firstCall[-1]), function(x) {
      eval(x, envir = modifiedDots$._envir)
    }))
  } else {
    modifiedDots <- append(modifiedDots, as.list(FUN))
  }

  for (fns in seq_along(modifiedDots$._rhss)) {
    functionName <- as.character(modifiedDots$._rhss[[fns]][[1]])
    FUN <- eval(parse(text = functionName)) # nolint
    if (is.primitive(FUN)) {
      otherCall <- modifiedDots$._rhss[[fns]]
    } else {
      otherCall <- match.call(definition = FUN, modifiedDots$._rhss[[fns]])
    }
    modifiedDots[[paste0("functionName", fns)]] <- as.character(modifiedDots$._rhss[[fns]][[1]])
    modifiedDots[[paste0(".FUN", fns)]] <-
      eval(parse(text = modifiedDots[[paste0("functionName", fns)]]))
    modifiedDots <- append(modifiedDots, as.list(otherCall[-1]))
  }
  return(list(modifiedDots = modifiedDots, fnDetails = fnDetails))
}

.CacheInternalFn1 <- function(FUN, scalls) {

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
             call. = FALSE)
      } else {
        stop("Is the %>% from reproducible masked?")
      }

    } else {
      stop("Can't understand the function provided to Cache.\n",
           "Did you write it in the form: ",
           "Cache(function, functionArguments)?")
    }
  } else {
    scalls <- NULL
  }

  scalls
}


.CacheSideEffectFn1 <- function(output, sideEffect, cacheRepo, quick, algo, FUN, ...) {
  message("sideEffect argument is poorly tested. It may not function as desired")
  needDwd <- logical(0)
  fromCopy <- character(0)
  cachedChcksum <- attributes(output)$chcksumFiles

  if (!is.null(cachedChcksum)) {
    for (x in cachedChcksum) {
      chcksumName <- sub(":.*", "", x)
      chcksumPath <- file.path(sideEffect, basename(chcksumName))

      if (file.exists(chcksumPath)) {
        checkDigest <- TRUE
      } else {
        checkCopy <- file.path(cacheRepo, "gallery", basename(chcksumName))
        if (file.exists(checkCopy)) {
          chcksumPath <- checkCopy
          checkDigest <- TRUE
          fromCopy <- c(fromCopy, basename(chcksumName))
        } else {
          checkDigest <- FALSE
          needDwd <- c(needDwd, TRUE)
        }
      }

      if (checkDigest) {
        if (quick) {
          sizeCurrent <- lapply(chcksumPath, function(z) {
            list(basename(z), file.size(z))
          })
          chcksumFls <- lapply(sizeCurrent, function(z) {
            digest::digest(z, algo = algo)
          })
        } else {
          chcksumFls <- lapply(chcksumPath, function(z) {
            digest::digest(file = z, algo = algo)
          })
        }
        # Format checksum from current file as cached checksum
        currentChcksum <- paste0(chcksumName, ":", chcksumFls)

        # List current files with divergent checksum (or checksum missing)
        if (!currentChcksum %in% cachedChcksum) {
          needDwd <- c(needDwd, TRUE)
        } else {
          needDwd <- c(needDwd, FALSE)
        }
      }
    }
    #}
  } else {
    message("  There was no record of files in sideEffects")
  }

  if (any(needDwd)) {
    do.call(FUN, list(...))
  }

  if (NROW(fromCopy)) {
    repoTo <- file.path(cacheRepo, "gallery")
    lapply(fromCopy, function(x) {
      file.copy(from = file.path(repoTo, basename(x)),
                to = file.path(cacheRepo), recursive = TRUE)
    })
  }
}

.CacheSideEffectFn2 <- function(sideEffect, cacheRepo, priorRepo, algo, output,
                                makeCopy, quick) {
  if (isTRUE(sideEffect)) {
    postRepo <- list.files(cacheRepo, full.names = TRUE)
  } else {
    postRepo <- list.files(sideEffect, full.names = TRUE)
  }
  dwdFlst <- setdiff(postRepo, priorRepo)
  if (length(dwdFlst > 0)) {
    if (quick) {
      sizecurFlst <- lapply(dwdFlst, function(x) {
        list(basename(x), file.size(file.path(x)))
      })
      cachecurFlst <- lapply(sizecurFlst, function(x) {
        digest::digest(x, algo = algo)
      })
    } else {
      cachecurFlst <- lapply(dwdFlst, function(x) {
        digest::digest(file = x, algo = algo)
      })
    }

    cacheName <- file.path(basename(sideEffect), basename(dwdFlst), fsep = "/")
    setattr(output, "chcksumFiles", paste0(cacheName, ":", cachecurFlst))
    #attr(output, "chcksumFiles") <- paste0(cacheName, ":", cachecurFlst)
    if (!identical(attr(output, "chcksumFiles"), paste0(cacheName, ":", cachecurFlst))) browser()

    if (makeCopy) {
      repoTo <- file.path(cacheRepo, "gallery")
      checkPath(repoTo, create = TRUE)
      lapply(dwdFlst, function(x) {
        file.copy(from = x, to = file.path(repoTo), recursive = TRUE)
      })
    }
  }
  return(output)
}

.addTagsRepo <- function(isInRepo, cacheRepo, lastOne) {
  written <- 0
  while (written >= 0) {
    saved <- suppressWarnings(try(silent = TRUE,
                                  addTagsRepo(isInRepo$artifact[lastOne],
                                              repoDir = cacheRepo,
                                              tags = paste0("accessed:", Sys.time()))))
    written <- if (is(saved, "try-error")) {
      Sys.sleep(sum(runif(written + 1,0.05, 0.1)))
      written + 1
    } else {
      -1
    }
  }

}

.getFromRepo <- function(isInRepo, notOlderThan, lastOne, cacheRepo, fnDetails,
                         modifiedDots, debugCache, verbose, sideEffect, quick,
                         algo, preDigest, startCacheTime, ...) {

  if (verbose > 1) {
    startLoadTime <- Sys.time()
  }

  fromMemoise <- NA
  if (getOption("reproducible.useMemoise")) {
    fromMemoise <-
      if (memoise::has_cache(.loadFromLocalRepoMem)(isInRepo$artifact[lastOne],
                                                    repoDir = cacheRepo, value = TRUE)) {
        TRUE
      } else {
        FALSE
      }
    loadFromMgs <- "Loading from memoise version of repo"
    output <- .loadFromLocalRepoMem(isInRepo$artifact[lastOne],
                                    repoDir = cacheRepo, value = TRUE)
    output <- unmakeMemoiseable(output)
    #if (is(output, "simList_")) output <- as(output, "simList")
  } else {
    loadFromMgs <- "Loading from repo"
    output <- loadFromLocalRepo(isInRepo$artifact[lastOne],
                                repoDir = cacheRepo, value = TRUE)
  }

  if (verbose > 1) {
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
  .cacheMessage(output, fnDetails$functionName,
                fromMemoise = fromMemoise)

  # This is protected from multiple-write to SQL collisions
  .addTagsRepo(isInRepo, cacheRepo, lastOne)

  if (sideEffect != FALSE) {
    #if(isTRUE(sideEffect)) {
    .CacheSideEffectFn1(output, sideEffect, cacheRepo, quick, algo, FUN, ...)
  }

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

  if (verbose > 1) {
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
  if (is.character(output))
    if (identical(as.character(output), "NULL"))
      output <- NULL

  return(output)

}
