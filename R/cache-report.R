## User-facing reporting for Cache(): showSimilar's explanation of a cache miss,
## the verbose timing frames, and the debug dumps.


sysTimeForCacheToChar <- function(digits = 5)
  format(Sys.time(), digits = digits)



#' @importFrom data.table setorderv setcolorder
showSimilar <- function(cachePath, metadata, .functionName, userTags, useCache,
                        useCloud = FALSE, cloudFolderID = NULL,
                        # cacheSaveFormat = getOption("reproducible.cacheSaveFormat"),
                        drv, conn, verbose) {
  devMode <- isDevMode(useCache, userTags)  # don't use devMode if no userTags
  shownCacheUserTags <- showCache(cachePath, Function = .functionName, userTags = userTags,
                          verbose = verbose - 2)
  shownCache <- showCache(cachePath, Function = .functionName, # userTags = userTags,
                          verbose = verbose - 2)

  # With `useCloud`, the remote cache may hold artifacts produced on other
  # machines that never landed in this local cache. showCache() only reads the
  # local backend, so pull the cloud metadata (the small `.dbFile.*` files),
  # restrict it to the same function, and fold it into `shownCache` before the
  # normal comparison path below.
  if (cloudWriteOrRead(useCloud) && !is.null(cloudFolderID)) {
    cloudShown <- showCacheCloud(cloudFolderID, cachePath,
                                 existingCacheIds = unique(shownCache$cacheId),
                                 drv = drv, conn = conn, verbose = verbose - 1)
    shownCache <- mergeShownCacheCloud(shownCache, cloudShown, .functionName)
  }
  # functionByDigest <- metadata[tagKey %in% "preDigest" & startsWith(tagValue, dotFunTxt)]$tagValue
  # shownCache <- shownCache[tagKey %in% "preDigest" & tagValue %in% functionByDigest]
  setorderv(shownCache, "createdDate", order = -1)
  # shownCache <- shownCache[tagKey != "outerFunction"] # doesn't matter what outerFunctions do, if all others are same
  # metadata <- metadata[tagKey != "outerFunction"]
  onKey <- c("tagKey", "tagValue")

  if (NROW(shownCache)) {
    userTagsMess <- if (!is.null(userTags)) {
      paste0(.message$BecauseOfA,
             "with user supplied tags: '",
             paste(userTags, collapse = ", "), "' "
      )
    }

    rmTagKeys <- "otherFunction|elapsedTime|accessed|module:|eventType:|eventTime:|outerFunction:"
    shownCache <- shownCache[grep(x = tagKey, rmTagKeys, invert = TRUE)]
    metadataSmall <- metadata[grep(x = tagKey, rmTagKeys, invert = TRUE)]
    # Can only compare on tagKeys that are *not yet* in the metadata; e.g., object.size may
    #   not be there, so don't know if it is different
    similarFull <- unique(shownCache[tagKey %in% unique(c(metadata$tagKey))], by = .dtFileMainCols)
    similarFullList <- split(similarFull, by = "cacheId")
    notInThisCall <- lapply(similarFullList, function(x) x[!metadataSmall, on = onKey])
    notInSC <- lapply(similarFullList, function(x) metadataSmall[!x, on = onKey])
    notInThisCall0 <- lapply(notInThisCall, function(x) x[grep("userTags", tagKey ), tagValue := paste0(tagKey, ":", tagValue)])
    notInSC0 <- lapply(notInSC, function(x) x[grep("userTags", tagKey ), tagValue := paste0(tagKey, ":", tagValue)])

    similar <- notInThisCall
    other <- logical()
    if (NROW(similar) == 0) {
      other <- vapply(strsplitOnlySingleColon(similarFull$tagValue, split = "\\:"),
                      function(x) ifelse(length(x) == 2, x[[2]], NA_character_), FUN.VALUE = character(1))
      otherLabels <- vapply(strsplitOnlySingleColon(similarFull$tagValue, split = "\\:"),
                      function(x) ifelse(length(x) == 2, x[[1]], NA_character_), FUN.VALUE = character(1))
      whOther <- other == "other"
      cacheIdOfSimilar <- unique(similarFull$cacheId)
      simFun <- list(funName = unique(shownCache$tagValue[shownCache$tagKey == "function"]))
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

    }

    # This is for dryRun: i.e., there is a cacheId, but no difference in metadata
    # isIdentical0 <- vapply(similar, function(x) NROW(x) == 0, FUN.VALUE = logical(1))
    isIdentical1 <- vapply(notInSC0, function(x) NROW(x) == 0, FUN.VALUE = logical(1))
    isIdentical2 <- vapply(notInThisCall0, function(x) NROW(x) == 0, FUN.VALUE = logical(1))
    isIdentical <- isIdentical1 & isIdentical2
    if (any(isIdentical)) {
      messageCache("Call is identical to ", paste(names(similar)[isIdentical], collapse = ", "),
                   " and would return that object")
      return(NULL)
    }

    if (NROW(similar)) {

      notInSCLen <- vapply(notInSC0, NROW, FUN.VALUE = integer(1))
      notInThisCallLen <- vapply(notInThisCall0, NROW, FUN.VALUE = integer(1))
      numSimilars <- length(notInSCLen)

      # First pass -- this will shrink probably down a lot
      diffs <- mapply(n = notInSCLen, m = notInThisCallLen, function(n, m) n + m, SIMPLIFY = TRUE)
      minNumDiffs <- min(diffs)
      smallestDiffs <- which(diffs == minNumDiffs)
      notInSC2 <- notInSC0[smallestDiffs]
      notInThisCall2 <- notInThisCall0[smallestDiffs]

      notInSC4 <- lapply(notInSC2, function(x) {
        x <- createSimilar(x, verbose = verbose, devMode = devMode, .functionName = .functionName)
        data.table::setnames(x, old = c(valInCacheTxt, cacheIdInCacheTxt),
                             new = c(valThisCallTxt, cacheIdThisCallTxt),
                             skip_absent = TRUE)
        })
      notInThisCall3 <- lapply(notInThisCall2, function(x) {
        ss <- createSimilar(x, verbose = verbose, devMode = devMode, .functionName = .functionName)
        if (isTRUE(any("lsStr" %in% colnames(ss))))
          set(ss, NULL, "lsStr", NULL)
        ss
        })

      simi <- Map(n = names(notInThisCall3), function(n) {
        if (NROW(notInThisCall3[[n]]) || NROW(notInSC4[[n]])) {
          a <- notInSC4[[n]][notInThisCall3[[n]], on = argTxt, allow.cartesian = TRUE]
          b <- notInThisCall3[[n]][notInSC4[[n]], on = argTxt, allow.cartesian = TRUE]
          d <- unique(rbindlist(list(a, b), fill = TRUE))
        } else {
          d <- data.table(notInSC4[[n]], valueInCache = NA, cacheIdInCache = NA)
        }

        # Convert .FUN to the actual function name; need 2 mechanisms because SpaDES.core manually
        #   places an entry with the actual name
        hasDotFun <- d[[argTxt]] %in% dotFunTxt
        if (any(hasDotFun)) {
          dups <- duplicated(d[[valThisCallTxt]])
          if (any(dups)) {
            # Remove .FUN if there is another one with "more info"
            theDupCI <- d[[valThisCallTxt]][dups]
            theDotFun <- d[[valThisCallTxt]] %in% theDupCI & hasDotFun
            d <- d[!theDotFun]
          } else {
            # case where it shows only ".FUN", with no duplication
            scHere <- shownCache[shownCache$cacheId %in% d[[cacheIdInCacheTxt]], ]# $tagKey %in% "function"
            funName <- scHere[["tagValue"]][scHere[["tagKey"]] %in% "function"]
            if (length(funName))
              d[[argTxt]] <- funName
          }

        }
        setcolorder(d, c(argTxt, cacheIdInCacheTxt, valInCacheTxt,
                         cacheIdThisCallTxt, valThisCallTxt))
        d
      })

      # Second pass -- this will be different if there were no new arguments; just arg value changes
      diffs <- mapply(x = simi, function(x) NROW(x), SIMPLIFY = TRUE)
      minNumDiffs <- min(diffs)
      smallestDiffs <- which(diffs == minNumDiffs)
      numSmallest <- length(smallestDiffs)
      simi <- simi[smallestDiffs]

      messageCache("There are ", numSimilars,
                   " calls with same fn (", .messageFunctionFn(.functionName), ") in the Cache repository.",
                   verbose = verbose * !devMode)
      if (identical(numSimilars, 1L)) {
        messageCache("It has ", minNumDiffs, " differences", verbose = verbose * !devMode)
      } else {
        messageCache("With fewest differences (", minNumDiffs, "), there ", isAre(v = numSmallest),
                     " ", numSmallest,
                     " similar calls in the Cache repository.", verbose = verbose * !devMode)
      }

      if (isDevMode(useCache, userTags)) {
        # Only replace entries that actually matched on userTags (not just function name)
        cacheIdsToClear <- intersect(unique(names(simi)), unique(shownCacheUserTags$cacheId))
        if (length(cacheIdsToClear)) {
          messageCache("------ devMode -------", verbose = verbose)
          messageCache("Previous call(s) exist in the cache with identical userTags (",
                       paste0(userTags, collapse = ", "), ")", verbose = verbose)
          messageCache("This call to cache will replace entry with cacheId(s): ",
                       paste0(simi[["cacheId"]], collapse = ", "), verbose = verbose)
          clearCache(cachePath, cacheId = cacheIdsToClear, ask = FALSE,  drv = drv, conn = conn, verbose = verbose - 2)
        }
      }
      nShow <- min(numSmallest, 5)
      messageCache("with different elements (", nShow, " most recent at top):", verbose = verbose)
      # don't add a prefix if there is no `sim` in the stack
      wis <- .whereInStack("sim")
      prefix <- if (identical(.GlobalEnv, wis) || is.null(wis)) "" else .message$NoPrefix
      messageCache(.message$dashes, prefix)
      keepers <- seq_len(nShow)
      lala <- Map(si = simi[keepers], nam = names(simi[keepers]), function(si, nam) {
        messageCache(paste0("Compared to cacheId: ", nam, prefix), verbose = verbose)
        if (verbose > 0) {
          oo <- capture.output(si)
          fn <- cliCol(getOption("reproducible.messageColourCache"))
          oo <- paddDFInitial(oo, rows = 1:2, .spaceTmpChar, colour = getOption("reproducible.messageColourCache"))
          messageColoured(paste0(paste(oo, collapse = "\n"), .message$NoPrefix),
                          colour = getOption("reproducible.messageColourCache"))
        }
        messageCache(.message$dashes, prefix)
      })

      messageCache("------ devMode -------", verbose = verbose * devMode)

    }
  } else {
    messageCache(.message$noSimilarCacheTxt(.functionName), verbose = verbose)
  }
}


verboseCacheDFAll <- function(verbose, functionName, times) {
  verboseDF1(verbose, functionName, times$CacheDigestStart, times$EvaluateStart)
  verboseDF2(verbose, functionName, times$EvaluateStart, times$SaveStart)
  verboseDF3(verbose, functionName, times$CacheDigestStart, times$SaveEnd)
  .message$CacheTimings(verbose)
}


strsplitOnlySingleColon <- function(x, ...) {
  strsplit(x, split = .txtGrepStrSplitSingleColon, perl = TRUE)
}



reorder_by_first_element <- function(x) {
  keys <- sapply(x, `[[`, 1)  # extract the first elements
  seen <- character()
  res <- list()

  for (i in seq_along(x)) {
    key <- keys[i]
    if (!key %in% seen) {
      # First time we see this key: append
      res[[length(res) + 1]] <- x[[i]]
      seen <- c(seen, key)
    } else {
      # Find last index where this key is already in res
      last_idx <- max(which(sapply(res, `[[`, 1) == key))
      res <- append(res, list(x[[i]]), after = last_idx)
    }
  }

  res
}




createSimilar <- function(similar, .functionName, verbose, devMode) {

  simi <- similar[, .N, by = "cacheId"][similar, on = "cacheId"]
  data.table::setorderv(simi, c("N", "createdDate"))
  numSimilars <- NROW(unique(similar$cacheId))
  # messageCache("There are ", numSimilars,
  #              " similar calls (same fn: ", .messageFunctionFn(.functionName), ") in the Cache repository.",
  #              verbose = verbose * !devMode)
  simi <- split(simi, by = "N") # take first element in split list
  if (length(simi)) {
    simi <- simi[[1]]
    # if (identical(numSimilars, 1L)) {
    #   messageCache("It has ", simi$N[[1]], " differences", verbose = verbose * !devMode)
    # } else {
    #   messageCache("With fewest differences (", simi$N[[1]], "), there are ",
    #                NROW(unique(simi$cacheId)),
    #                " similar calls in the Cache repository.", verbose = verbose * !devMode)
    # }
    twoCols <- strsplitOnlySingleColon(simi[["tagValue"]])
    lens <- lengths(twoCols)
    hasNoColon <- lens == 1
    if (isTRUE(any(hasNoColon))) {
      whNoColon <- which(hasNoColon)
      twoCols[whNoColon] <- lapply(whNoColon, function(x) c(simi[["tagKey"]][[x]], twoCols[[x]]))
    }

    args <- vapply(twoCols, function(x) x[[1]], FUN.VALUE = character(1))
    lens <- lengths(twoCols)
    vals <- rep("", length(twoCols))
    vals[lens > 1] <- vapply(twoCols[lens > 1], function(x) x[[2]], FUN.VALUE = character(1))
    set(simi, NULL, argTxt, args)
    set(simi, NULL, "value", vals)
    set(simi, NULL, c("N", "tagKey", "tagValue", "createdDate"), NULL)
    setcolorder(simi, c("cacheId", argTxt, "value"))
    setnames(simi, old = c("cacheId", "value"), new = c(cacheIdInCacheTxt, valInCacheTxt))
  } else {
    simi <- data.table(arg = character(), cacheIdInCache = character(), valueInCache = character())
  }
  simi
}


cacheChainingOuterFunctionName <- "cacheChainingOuterFunction"

cacheChainLabel <- "cacheChain_"

surroundingFunctionLabel <- "surroundingFunction"



inCacheTxt <- "InCache"

thisCallTxt <- "ThisCall"

argTxt <- "arg"

valInCacheTxt <- paste0("value", inCacheTxt)

cacheIdInCacheTxt <- paste0("cacheId", inCacheTxt)

cacheIdThisCallTxt <- paste0("cacheIdOf", thisCallTxt)

valThisCallTxt <- paste0("value", thisCallTxt)




verboseDF0 <- function(verbose, functionName, startHashTime, endTime) {
  if (verbose > 3) {
    if (missing(endTime))
      endTime <- Sys.time()
    verboseDF <- data.frame(
      functionName = functionName,
      component = "Hashing",
      elapsedTime = as.numeric(difftime(endTime, startHashTime, units = "secs")),
      units = "secs",
      stringsAsFactors = FALSE
    )
    verboseAppendOrCreateDF(verboseDF)
  }
  # if (exists("verboseTiming", envir = .reproEnv, inherits = FALSE)) {
  #   verboseDF$functionName <- paste0("  ", verboseDF$functionName)
  #   .reproEnv$verboseTiming <- rbind(.reproEnv$verboseTiming, verboseDF)
  # } else {
  #   .reproEnv$verboseTiming <- verboseDF
  # }
}


#' @keywords internal
verboseDF1 <- function(verbose, functionName, startRunTime, endTime) {
  if (verbose > 3) {
    if (missing(endTime))
      endTime <- Sys.time()
    verboseDF <- data.frame(
      functionName = functionName,
      component = paste("Running", functionName),
      elapsedTime = as.numeric(difftime(endTime, startRunTime, units = "secs")),
      units = "secs",
      stringsAsFactors = FALSE
    )

    if (exists("verboseTiming", envir = .reproEnv)) {
      .reproEnv$verboseTiming <- rbind(.reproEnv$verboseTiming, verboseDF)
    }
  }
}


#' @keywords internal
verboseDF2 <- function(verbose, functionName, startSaveTime, endTime) {
  if (verbose > 3) {
    if (missing(endTime))
      endTime <- Sys.time()
    verboseDF <-
      data.frame(
        functionName = functionName,
        component = "Saving to cachePath",
        elapsedTime = as.numeric(difftime(endTime, startSaveTime, units = "secs")),
        units = "secs",
        stringsAsFactors = FALSE
      )

    if (exists("verboseTiming", envir = .reproEnv)) {
      .reproEnv$verboseTiming <- rbind(.reproEnv$verboseTiming, verboseDF)
    }
  }
}



#' @keywords internal
verboseDF3 <- function(verbose, functionName, startCacheTime, endTime) {
  if (verbose > 3) {
    if (missing(endTime))
      endTime <- Sys.time()
    verboseDF <- data.frame(
      functionName = functionName,
      component = "Whole Cache call",
      elapsedTime = as.numeric(difftime(endTime, startCacheTime,
        units = "secs"
      )),
      units = "secs",
      stringsAsFactors = FALSE
    )

    if (exists("verboseTiming", envir = .reproEnv)) {
      .reproEnv$verboseTiming <- rbind(.reproEnv$verboseTiming, verboseDF)
    }
  }
}





#' Search for objects in the call stack
#'
#' Normally, this is only used in special, advanced uses. The standard approach
#' to getting an object from an environment in the call stack is to explicitly
#' pass it into the function.
#'
#' @param obj Character string. The object name to search.
#' @param startingEnv An environment to start searching in.
#'
#' @return The environment in which the object exists. It will return the
#' first environment it finds, searching outwards from where the function is used.
#' @export
.whereInStack <- function(obj, startingEnv = parent.frame()) {
  foundStarting <- FALSE
  snf <- sys.nframe()
  for (i in 1:snf) {
    testEnv <- sys.frame(-i)
    if (!foundStarting) {
      if (identical(testEnv, startingEnv)) {
        foundStarting <- TRUE
      } else {
        next
      }
    }
    fn <- if (R.version$minor < "1.0" && R.version$major <= "4") { # faster than any other approach
      get0(as.character(parse(text = obj)), testEnv, inherits = FALSE)
    } else {
      get0(obj, testEnv, inherits = FALSE) # much faster; only works R >= 4.1
    }
    if (!is.null(fn)) {
      break
    }
  }
  if (identical(testEnv, .GlobalEnv) && identical(i, snf))
    testEnv <- NULL
  return(testEnv)
}




verboseAppendOrCreateDF <- function(verboseDF) {
  if (exists("verboseTiming", envir = .reproEnv, inherits = FALSE)) {
    verboseDF$functionName <- paste0("  ", verboseDF$functionName)
    .reproEnv$verboseTiming <- rbind(.reproEnv$verboseTiming, verboseDF)
  } else {
    .reproEnv$verboseTiming <- verboseDF
  }
}
