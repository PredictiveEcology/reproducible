## Turning a call and its arguments into a cacheId, and assembling the metadata
## (tags) recorded alongside a cached object.


metadata_update <- function(outputToSave, metadata, cache_key) {
  userTagsExtra <- attr(outputToSave, "tags") # .wrap may have added tags
  userTagsExtra <- grep("cacheId:", userTagsExtra, invert = TRUE, value = TRUE) # don't add cacheId to tagKey
  if (!is.null(userTagsExtra) && length(userTagsExtra) > 0) {
    ut <- strsplitOnlySingleColon(userTagsExtra)
    # ut <- strsplit(userTagsExtra, split = ":")
    ll <- lapply(ut, tail, 1)
    names(ll) <- lapply(ut, head, 1)
    userTagsList <- ll
    metadata <- rbindlist(list(metadata, userTagsListToDT(cache_key, userTagsList)))
  }
  metadata
}



cache_Id_Identical <- function(metadata, cachePaths, cache_key,
                               cacheSaveFormat = getOption("reproducible.cacheSaveFormat")) {
  linkToCacheId <- NULL
  os <- metadata$tagValue[metadata$tagKey == "object.size"]

  skipPreDigest <- startsWith(cache_key, .txtPreDigest)

  if (!identical(os, "NA") && skipPreDigest %in% FALSE) {
    if (isTRUE(as.numeric(os) > .objectSizeMinForBig)) {
      for (cachePath in cachePaths) {
        allCache <- showCache(x = cachePath, verbose = -2)
        if (NROW(allCache)) {
          resultHash <- metadata$tagValue[metadata$tagKey == "resultHash"]
          alreadyExists <- allCache[allCache$tagKey == "resultHash" &
                                      allCache[[.cacheTableTagColName()]] %in% resultHash &
                                      allCache[[.cacheTableHashColName()]] != cache_key]
          if (NROW(alreadyExists)) {
            linkToCacheId <- alreadyExists[["cacheId"]][[1]]
          }
        }
      }
    }
  }
  if (!is.null(linkToCacheId))
    linkToCacheId <- CacheStoredFile(cachePath, linkToCacheId, cacheSaveFormat = cacheSaveFormat)
  linkToCacheId
}


metadata_define_preEval <- function(detailed_key, func_name, userTags,
                                    .objects, length, algo, quick, classOptions,
                                    timeEvaluateStart, timeCacheDigestStart) {

  elapsedTimeCacheDigest <- difftime(timeEvaluateStart, timeCacheDigestStart, units = "secs")

  useCloud <- FALSE

  df <- unlist(
    .unlistToCharacter(unname(detailed_key[-1]), getOption("reproducible.showSimilarDepth", 3))
  )
  pat <- "[[:digit:]]{1,5}$"
  didWeGainNumerics <- grep(names(df), pattern = pat)
  wouldBe <- gsub("", pattern = pat, names(df))
  dups <- which(duplicated(wouldBe))
  wasFirstsOfDups <- setdiff(didWeGainNumerics, dups)
  isTheDupAGainedNumeric <- wasFirstsOfDups %in% didWeGainNumerics
  if (any(isTheDupAGainedNumeric)) {
    changeThese <- c(wasFirstsOfDups, dups)
    names(df)[changeThese] <- wouldBe[changeThese]
  }

  tagKey <- paste0(names(df), ":", as.character(df))
  if (length(userTags)) {
    ut <- strsplitOnlySingleColon(userTags)
    # ut <- strsplit(userTags, split = ":")
    ll <- lapply(ut, tail, 1)
    strt <- lapply(ut, function(x) x[-length(x)])
    utagLabel <- rep("userTags", length(ll))#lapply(ut, head, 1)
    hasLabel <- lengths(strt) > 0
    if (any(hasLabel)) {
      utagLabel[hasLabel] <- sapply(strt[hasLabel], function(x) paste0(x, collapse = ":"))
    }
    names(ll) <- utagLabel
    userTags <- ll
  }
  userTagsList <- c(
    list(func_name) |> setNames(nm = .cacheTagsFirstGroup[1]),
    userTags,
    list(sysTimeForCacheToChar()) |> setNames(nm = .cacheTagsFirstGroup[3]),
    list(isTRUE(useCloud)) |> setNames(nm = .cacheTagsFirstGroup[4]),
    list(format(elapsedTimeCacheDigest, units = "secs")) |> setNames(nm = .cacheTagsFirstGroup[5]),
    list(tagKey) |> setNames(nm = .cacheTagsFirstGroup[6])
  )
  names(userTagsList)[1] <- "function"
  cache_key <- detailed_key$key
  metadata <- userTagsListToDT(cache_key, userTagsList)
  return(metadata)
}


metadata_define_postEval <- function(metadata, cacheId, outputToSave, userTags,
                                     .objects, length, algo, quick, classOptions,
                                     elapsedTimeFUN) {
  objSize <- NA
  if (getOption("reproducible.objSize", TRUE)) {
    hasPointer <- usesPointer(outputToSave)
    if (any(unlist(hasPointer))) {
      os <- objSize(outputToSave, recursive = TRUE)
    } else {
      os <- objSize(outputToSave)
    }
    objSize <- sum(os)
  }

  resultHash <- ""
  if (isTRUE(objSize > .objectSizeMinForBig)) {
    resultHash <- CacheDigest(outputToSave,
                              .objects = .objects,
                              length = length, algo = algo, quick = quick,
                              classOptions = classOptions, calledFrom = "Cache"
    )$outputHash
  }
  fns <- Filenames(outputToSave)
  # tagsFromDefaults <- .cacheTagsDefault
  # .cacheTagsSecondGroup <- c("class", "object.size", "fromDisk", "resultHash", "elapsedTimeFirstRun")

  userTagsList <- c(
    list(class(outputToSave)[1]) |> setNames(nm = .cacheTagsSecondGroup[1]),
    list(format(as.numeric(objSize))) |> setNames(nm = .cacheTagsSecondGroup[2]),
    list(isTRUE(any(nchar(fns) > 0))) |> setNames(nm = .cacheTagsSecondGroup[3]),
    list(resultHash) |> setNames(nm = .cacheTagsSecondGroup[4]),
    list(format(elapsedTimeFUN, units = "secs")) |> setNames(nm = .cacheTagsSecondGroup[5])
  )
  cache_key <- cacheId
  metadataNew <- userTagsListToDT(cache_key, userTagsList)
  metadata <- rbindlist(list(metadata, metadataNew))
  # attr(metadata, "tags")$objectSize <- objSize
  metadata
}


userTagsListToDT <- function(cache_key, userTagsList) {
  theChars <- vapply(userTagsList, function(x) is.character(x) | is.logical(x), logical(1))
  if (any(!theChars)) {
    for (tc in which(!theChars))
      userTagsList[[tc]] <- tryCatch2(format(userTagsList[[tc]]), error = function(u) as.character())
  }
  userTagsList <- utils::stack(userTagsList)
  metadataDT(cacheId = cache_key, tagKey = userTagsList$ind, tagValue = userTagsList$values)
}


.addTagsRepoAccessedTime <- function(cache_key, cachePath = cachePath,
                                     cacheSaveFormat = getOption("reproducible.cacheSaveFormat")) {
  .addTagsRepo(cacheId = cache_key, tagKey = "accessed", tagValue = sysTimeForCacheToChar()
               , cacheSaveFormat = cacheSaveFormat, cachePath = cachePath)
}


cacheIdOverride <- function(cacheId, key, .functionName, verbose) {
  if  (identical(cacheId, "previous")) {
    cacheId <- getPreviousEntryInCache(.functionName, cacheId, verbose)
  } else {
    shownCache <- cacheIdCheckInCache(cacheId, calculatedCacheId = key, .functionName, verbose)
    # if (NROW(shownCache) == 0)
    #   cacheId <- NULL
  }
  cacheId
}


doDigestPrepare <- function(new_call, omitArgs, .cacheExtra) {
  toDigest <- attr(new_call, ".Cache")$args_w_defaults # not evaluated arguments

  toDigest$.FUN <- attr(new_call, ".Cache")$method
  # Deal with omitArgs:
  # - TRUE  => drop every captured arg; digest is based on .FUN (the actual
  #            function value, body included, so source edits still bust the
  #            cache) plus .cacheExtra
  # - char  => drop the named args
  # - NULL  => default, no change
  if (isTRUE(omitArgs)) {
    toDigest <- toDigest[names(toDigest) %in% ".FUN"]
  } else if (is.character(omitArgs)) {
    if (any("FUN" %in% omitArgs))
      omitArgs <- c(dotFunTxt, omitArgs)
    toDigest[omitArgs] <- NULL
  }
  # Deal with .cacheExtra by adding it to the list of objects to digest
  if (!is.null(.cacheExtra))
    toDigest <- append(toDigest, list(.cacheExtra = .cacheExtra))
  toDigest
}





doDigest <- function(toDigest, .functionName, .objects, length, algo, quick,
                      classOptions, timeCacheDigestStart, verbose) {
  detailed_key <- CacheDigest(toDigest,
                              .functionName = .functionName,
                              .objects = .objects,
                              length = length, algo = algo, quick = quick,
                              classOptions = classOptions,
                              calledFrom = "Cache"
  )
  diTi <- difftime(Sys.time(), timeCacheDigestStart, units = "sec")
  if (diTi > 5) {
    messageCache("Object digesting for ", .messageFunctionFn(.functionName)," took: ", format(diTi, digits = 2))
  }
  verboseCacheMessage(detailed_key$preDigest, .functionName, timeCacheDigestStart, quick = quick,
                      modifiedDots = toDigest, verbose = verbose, verboseLevel = 3)

  names(detailed_key)[[1]] <- "key"
  # Optional diagnostic dump of the full element-by-element preDigest (off unless
  # options(reproducible.preDigestDump=) is set); for diffing cacheIds across
  # machines/OSs. See .dumpPreDigest().
  .dumpPreDigest(detailed_key, .functionName)
  detailed_key
}




appendFunctionNameToNestedTags <- function(userTags, functionName) {
  # allUT <- c(paste0("outerFunction:", functionName), userTags)
  # dups <- duplicated(sapply(strsplitOnlySingleColon(allUT), tail, 1))
  # allUT <- allUT[!dups] # only take after :
  # allUT <- sort(allUT)
  .pkgEnv$.reproEnv2$userTags <- c(.pkgEnv$.reproEnv2$userTags,
                                   paste0("outerFunction:", functionName))
  .pkgEnv$.reproEnv2$userTags <- .pkgEnv$.reproEnv2$userTags[!duplicated(.pkgEnv$.reproEnv2$userTags)]
}


.txtGrepStrSplitSingleColon <- "(?<!:):(?!:)"



.txtNoPrefix <- "noPrefix"

.txtDryRunTRUE <- "dryRun = TRUE: "



#' Set subattributes within a list by reference
#'
#' Sets only a single element within a list attribute.
#' @param object An arbitrary object
#' @param attr The attribute name (that is a list object) to change
#' @param subAttr The list element name to change
#' @param value The new value
#'
#' @return
#' This sets or updates the `subAttr` element of a list that is located at
#' `attr(object, attr)`, with the `value`. This, therefore, updates a sub-element
#'  of a list attribute and returns that same object with the updated attribute.
#'
#' @export
#' @rdname setSubAttrInList
.setSubAttrInList <- function(object, attr, subAttr, value) {
  .CacheAttr <- attr(object, attr)
  if (is.null(.CacheAttr)) .CacheAttr <- list()
  .CacheAttr[[subAttr]] <- value
  attr(object, attr) <- .CacheAttr
  object
}


#' The exact digest function that `Cache` uses
#'
#' This can be used by a user to pre-test their arguments before running
#' `Cache`, for example to determine whether there is a cached copy.
#'
#'
#' @param ... passed to `.robustDigest`.
#' @param objsToDigest A list of all the objects (e.g., arguments) to be digested
#' @param calledFrom a Character string, length 1, with the function to
#'    compare with. Default is "Cache". All other values may not produce
#'    robust CacheDigest results.
#'
#' @inheritParams Cache
#'
#' @return
#' A list of length 2 with the `outputHash`, which is the digest
#' that Cache uses for `cacheId` and also `preDigest`, which is
#' the digest of each sub-element in `objsToDigest`.
#'
#' @export
#'
#' @examples
#' data.table::setDTthreads(2)
#' a <- Cache(rnorm, 1)
#'
#' # like with Cache, user can pass function and args in a few ways
#' CacheDigest(rnorm(1)) # shows same cacheId as previous line
#' CacheDigest(rnorm, 1) # shows same cacheId as previous line
#'
CacheDigest <- function(objsToDigest, ..., algo = "xxhash64", calledFrom = "CacheDigest",
                        .functionName = NULL, quick = FALSE) {
  FUNcaptured <- substitute(objsToDigest)
  # origFUN <- quote(objsToDigest)
  fromCache <- identical(FUNcaptured, as.name("toDigest"))
  dots <- list(...)
  forms <- .formalsNotInCurrentDots(.robustDigest, dots = dots)
  if (is(FUNcaptured, "call") || # as in rnorm(1) ... but also list(outputToSave) needs to be avoided
    (NROW(dots) > 0 && # if not an function with call, then it has to have something there
      # ... so not "just" an object in objsToDigest
      (NROW(forms) > 1 || is.null(forms)))) { # can be CacheDigest(rnorm, 1)
    fnDetails <- .fnCleanup(
      FUN = objsToDigest, callingFun = "Cache", ..., FUNcaptured = FUNcaptured,
      .functionName = .functionName, CacheMatchedCall = match.call(CacheDigest)
    )
    modifiedDots <- fnDetails$modifiedDots
    modifiedDots$.FUN <- fnDetails$.FUN
    objsToDigest <- modifiedDots
  }

  if (!is(objsToDigest, "list")) {
    objsToDigest <- list(objsToDigest)
  }

  if (identical("Cache", calledFrom)) {
    namesOTD <- names(objsToDigest)
    lengthChars <- nchar(namesOTD)
    if (!any(namesOTD %in% "FUN")) {
      zeroLength <- which(lengthChars == 0)
      alreadyHasDotFun <- dotFunTxt %in% namesOTD
      if (sum(zeroLength) > 0 && !alreadyHasDotFun) {
        names(objsToDigest)[zeroLength[1]] <- dotFunTxt
      }
    }
  }

  # need to omit arguments that are in Cache function call
  defaults <- names(objsToDigest) %in% .defaultCacheOmitArgs
  if (sum(defaults)) {
    objsToDigest[defaults] <- NULL
  }

  if (is.character(quick) || isTRUE(quick)) {
    quickObjs <- if (isTRUE(quick)) {
      rep(TRUE, length(objsToDigest))
    } else {
      if (is.null(names(objsToDigest))) {
         rep(FALSE, length(objsToDigest))
      } else {
        names(objsToDigest) %in% quick
      }

    }
    objsToDigestQuick <- objsToDigest[quickObjs]
    objsToDigest <- objsToDigest[!quickObjs]
    preDigestQuick <- .robustDigest(objsToDigestQuick, algo = algo, quick = TRUE, ...)
    # preDigestQuick <- lapply(objsToDigestQuick, function(x) {
    #   # remove the "newCache" attribute, which is irrelevant for digest
    #   if (!is.null(attr(x, ".Cache")$newCache)) {
    #     x <- .setSubAttrInList(x, ".Cache", "newCache", NULL)
    #     if (!identical(attr(x, ".Cache")$newCache, NULL)) stop("attributes are not correct 1")
    #   }
    #   .robustDigest(x, algo = algo, quick = TRUE, ...)
    # })
  }

  # if (!is(objsToDigest, "list"))
  preDigest <- .robustDigest(objsToDigest, algo = algo, quick = FALSE, ...)
  # preDigest <- Map(x = objsToDigest, i = seq_along(objsToDigest), function(x, i) {
  #   # remove the "newCache" attribute, which is irrelevant for digest
  #   if (!is.null(attr(x, ".Cache")$newCache)) {
  #     x <- .setSubAttrInList(x, ".Cache", "newCache", NULL)
  #     if (!identical(attr(x, ".Cache")$newCache, NULL)) stop("attributes are not correct 1")
  #   }
  #   withCallingHandlers({
  #     .robustDigest(x, algo = algo, quick = FALSE, ...)
  #   }, error = function(e) {
  #     nam <- names(objsToDigest)
  #     if (!is.null(nam))
  #       messageCache("Error occurred during .robustDigest of ", nam[i], " in ", .functionName)
  #   })
  # })


  # if (!isTRUE(all.equal(.orderDotsUnderscoreFirst(preDigest), .orderDotsUnderscoreFirst(preDigest2[names(preDigest)]))))
  if (is.character(quick) || isTRUE(quick)) {
    preDigest <- append(preDigest, preDigestQuick)
  }

  # preDigest <- .robustDigest(preDigest) # add the ._list
  # preDigest[["._list"]] <- NULL # don't need this for CacheDigest

  # don't unname -- Eliot Jan 13, 2025 -- this keeps the outputHash
  if (.digestVersion() >= 3L) {
    res <- .doDigest(preDigest, algo = algo, ...)
  } else {
    res <- .robustDigest(unname(sort(unlist(preDigest))), algo = algo, quick = TRUE, ...)
    # res <- .robustDigest(.sortDotsUnderscoreFirst(unlist(preDigest)), algo = algo, quick = TRUE, ...)
  }
  list(outputHash = res, preDigest = preDigest)
}


spatVectorNamesForCache <- c("x", "type", "atts", "crs")



.objectSizeMinForBig <- 5e6


cacheIdCheckInCache <- function(cacheId, calculatedCacheId, .functionName,
                                verbose) {
  sc <- NULL
  if (!is.null(cacheId)) {
    if  (identical(cacheId, "previous")) {
      sc <- getPreviousEntryInCache(.functionName, cacheId, verbose)
    } else {
      outputHashManual <- cacheId
      sc <- list(1)
      if (identical(outputHashManual, calculatedCacheId)) {
        messageCache(.message$cacheIdSameTxt, verbose = verbose)
      } else {
        if (!is.null(calculatedCacheId)) {
          messageCache(.message$cacheIdNotSameTxt(cacheId), verbose = verbose)
        } else {
          messageCache(.message$cacheIdNotAssessed(cacheId), verbose = verbose)
        }
      }
      attr(sc, "cacheId") <- cacheId
      if (NROW(sc) == 0)
        sc <- NULL

    }
  }

  sc

}



checkOverlappingArgs <- function(CacheMatchedCall, forms, dotsCaptured, functionName,
                                 FUNcapturedNamesEvaled, whichCache = "Cache") {
  # Check for args that are passed to both Cache and the FUN -- if any overlap; pass to both
  possibleOverlap <- if (identical(whichCache, "Cache")) .namesCacheFormals else .namescache2Formals # names(formals(args(Cache)))
  if (!is.call(CacheMatchedCall[["FUN"]])) {
    possibleOverlap <- intersect(names(CacheMatchedCall), possibleOverlap)
    actualOverlap <- intersect(names(forms), possibleOverlap)
    if (length(actualOverlap) && !identical(list(), dotsCaptured)) { # e.g., useCache, verbose; but if not in dots, then OK because were separate already
      message(
        "The following arguments are arguments for both Cache and ", functionName, ":\n",
        paste0(actualOverlap, collapse = ", "),
        "\n...passing to both. If more control is needed, pass as a call, e.g., ",
        "Cache(", functionName, "(...))"
      )
      overlappingArgsAsList <- as.list(CacheMatchedCall)[actualOverlap]
      FUNcapturedNamesEvaled <- as.call(append(as.list(FUNcapturedNamesEvaled), overlappingArgsAsList))
    }
  }
  FUNcapturedNamesEvaled
}

dotFunTxt <- ".FUN"
