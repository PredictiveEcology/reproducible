.CacheVerboseFn1 <- function(preDigest, fnDetails,
                             startHashTime, modifiedDots, quick,
                             verbose = getOption("reproducible.verbose", 1),
                             verboseLevel = 1) {
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
    objSize <- unname(attr(objSize(x), "objSize"))
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
      messageDF(.reproEnv$hashDetails, colour = "blue", verbose = verbose, verboseLevel = verboseLevel)
      messageCache("The hashing details are available from .reproEnv$hashDetails",
                   verbose = verbose, verboseLevel = verboseLevel)
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


# .CacheSideEffectFn1 <- function(output, sideEffect, cachePath, quick, algo, FUN,
#                                 verbose = getOption("reproducible.verbose", 1), ...) {
#   messageCache("sideEffect argument is poorly tested. It may not function as desired")
#   # browser(expr = exists("sideE"))
#   needDwd <- logical(0)
#   fromCopy <- character(0)
#   cachedChcksum <- attributes(output)$chcksumFiles
#
#   if (!is.null(cachedChcksum)) {
#     for (x in cachedChcksum) {
#       chcksumName <- sub(":.*", "", x)
#       chcksumPath <- file.path(sideEffect, basename(chcksumName))
#
#       if (file.exists(chcksumPath)) {
#         checkDigest <- TRUE
#       } else {
#         checkCopy <- file.path(CacheStorageDir(cachePath), basename(chcksumName))
#         if (file.exists(checkCopy)) {
#           chcksumPath <- checkCopy
#           checkDigest <- TRUE
#           fromCopy <- c(fromCopy, basename(chcksumName))
#         } else {
#           checkDigest <- FALSE
#           needDwd <- c(needDwd, TRUE)
#         }
#       }
#
#       if (checkDigest) {
#         if (quick) {
#           sizeCurrent <- lapply(chcksumPath, function(z) {
#             list(basename(z), file.size(z))
#           })
#           chcksumFls <- lapply(sizeCurrent, function(z) {
#             digest::digest(z, algo = algo)
#           })
#         } else {
#           chcksumFls <- lapply(chcksumPath, function(z) {
#             digest::digest(file = z, algo = algo)
#           })
#         }
#         # Format checksum from current file as cached checksum
#         currentChcksum <- paste0(chcksumName, ":", chcksumFls)
#
#         # List current files with divergent checksum (or checksum missing)
#         if (!currentChcksum %in% cachedChcksum) {
#           needDwd <- c(needDwd, TRUE)
#         } else {
#           needDwd <- c(needDwd, FALSE)
#         }
#       }
#     }
#     #}
#   } else {
#     messageCache("  There was no record of files in sideEffects", verbose = verbose)
#   }
#
#   if (any(needDwd)) {
#     do.call(FUN, list(...))
#   }
#
#   if (NROW(fromCopy)) {
#     repoTo <- CacheStorageDir(cachePath)
#     lapply(fromCopy, function(x) {
#       file.copy(from = file.path(repoTo, basename(x)),
#                 to = file.path(cachePath), recursive = TRUE)
#     })
#   }
# }

# .CacheSideEffectFn2 <- function(sideEffect, cachePath, priorRepo, algo, output,
#                                 makeCopy, quick) {
#   # browser(expr = exists("sideE"))
#   if (isTRUE(sideEffect)) {
#     postRepo <- list.files(cachePath, full.names = TRUE)
#   } else {
#     postRepo <- list.files(sideEffect, full.names = TRUE)
#   }
#   dwdFlst <- setdiff(postRepo, priorRepo)
#   if (length(dwdFlst > 0)) {
#     if (quick) {
#       sizecurFlst <- lapply(dwdFlst, function(x) {
#         list(basename(x), file.size(file.path(x)))
#       })
#       cachecurFlst <- lapply(sizecurFlst, function(x) {
#         digest::digest(x, algo = algo)
#       })
#     } else {
#       cachecurFlst <- lapply(dwdFlst, function(x) {
#         digest::digest(file = x, algo = algo)
#       })
#     }
#
#     cacheName <- file.path(basename(sideEffect), basename(dwdFlst), fsep = "/")
#     setattr(output, "chcksumFiles", paste0(cacheName, ":", cachecurFlst))
#     #attr(output, "chcksumFiles") <- paste0(cacheName, ":", cachecurFlst)
#     if (!identical(attr(output, "chcksumFiles"), paste0(cacheName, ":", cachecurFlst)))
#       stop("There is an unknown error 01")
#
#     # browser(expr = exists("sideE"))
#     if (makeCopy) {
#       repoTo <- CacheStorageDir(cachePath)
#       checkPath(repoTo, create = TRUE)
#       lapply(dwdFlst, function(x) {
#         file.copy(from = x, to = file.path(repoTo), recursive = TRUE)
#       })
#     }
#   }
#   return(output)
# }

.getFromRepo <- function(FUN, isInRepo, fullCacheTableForObj,
                         notOlderThan, lastOne, cachePath, fnDetails,
                         modifiedDots, debugCache, verbose, # sideEffect,
                         quick, fileFormat = NULL,
                         algo, preDigest, startCacheTime,
                         drv = getDrv(getOption("reproducible.drv", NULL)),
                         conn = getOption("reproducible.conn", NULL), ...) {
  # if (verbose > 3) {
  #   startLoadTime <- Sys.time()
  # }

  cacheObj <- isInRepo[[.cacheTableHashColName()]][lastOne]

  fromMemoise <- NA
  output <- loadFromCache(cachePath, isInRepo[[.cacheTableHashColName()[lastOne]]],
                          fullCacheTableForObj = fullCacheTableForObj,
                          # format = fileFormat, loadFun = loadFun,
                          .functionName = fnDetails$functionName, .dotsFromCache = modifiedDots,
                          drv = drv, conn = conn)
  # This is protected from multiple-write to SQL collisions
  .addTagsRepo(cacheId = isInRepo[[.cacheTableHashColName()]][lastOne],
               cachePath = cachePath, drv = drv, conn = conn)
  if (length(debugCache)) {
    if (!is.na(pmatch(debugCache, "complete")) | isTRUE(debugCache))
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

  # If it was a NULL, the cachePath stored it as "NULL" ... return it as NULL
  if (is.character(output)) {
    if (identical(as.character(output), "NULL"))
      output <- NULL
  }

  return(output)
}
