## Reading from and writing to a cache repository: lookup, load, save, locking,
## DB connections, and the glue to the cloud backend.


check_and_get_cached_copy <- function(detailed_key, cachePaths, cache_file, functionName,
                                      func, useCache, useCloud, cloudFolderID, gdriveLs,
                                      full_call, outputObjects,
                                      cacheSaveFormat = getOption("reproducible.cacheSaveFormat"),
                                      .cacheChaining = getOption("reproducible.cacheChaining", FALSE),
                                      drv, conn, envir = parent.frame(), verbose) {
  cache_key <- detailed_key$key
  # Check if the result is already cached
  connOrig <- conn
  conns <- conn
  if (!is.null(conn) && !is.list(conns)) {
    conns <- list(conn)
    names(conns) <- cachePaths
  }

  for (cachePath in cachePaths) {
    cache_file <- CacheStoredFile(cachePath, cache_key, cacheSaveFormat = cacheSaveFormat, readOnly = TRUE)
    cacheFileExists <- file.exists(cache_file) # could be length >1
    if (useDBI()) {
      inReposPoss <- searchInRepos(cachePath,
                                   outputHash = cache_key,
                                   drv = drv, conn = conns[[cachePath]]
      )
      if (cachePath == cachePaths[[1]] || NROW(inReposPoss$isInRepo)) {
        # keep important parts if it is first one, or if it has the object in the cacheRepo
        # inRepos <- inReposPoss
        conn <- conns[[cachePath]] # keep it as a list so places where it needs the name work
        if (is.null(connOrig)) # don't disconnect if conn was user passed
          # if this is >1st cachePath, then the db will already be disconnected; suppressWarnings
          on.exit2(suppressWarnings(DBI::dbDisconnect(conn)), envir = envir)

        shownCache <- inReposPoss$fullCacheTableForObj
        if (NROW(inReposPoss$isInRepo)) {
          break
        }
        if (cachePath == tail(cachePaths, 1)) { # if it is the last or only cachePath, then end
          ret <- .returnNothing
          attr(ret, ".Cache")$conn <- conn
          return(invisible(ret))
        }

        # this disconnect won't happen if user passed just one conn because already returned/break from this loop
        DBI::dbDisconnect(conn) # try next cachePath -- disconnect previous;
      }
    } else {
      shownCache <- NULL
      if (isTRUE(cacheFileExists))
        break
    }
  }

  # Check if it was saved with other CacheSaveFormat
  changedSaveFormat <- FALSE
  if (sum(cacheFileExists) == 0) { # if it doesn't exist; could be changed backend or not in Cache
    sameCacheID <- checkSameCacheId(cache_file)
    if (length(sameCacheID) > 0) {
      changedSaveFormat <- TRUE
      cacheFileExists <- TRUE
      cache_file_orig <- cache_file
      cache_file <- file.path(dirname(cache_file), sameCacheID)
    }
  }

  if (sum(cacheFileExists)) {
    output <- loadFromDiskOrMemoise(fromMemoise = FALSE, useCache, useCloud,
                                    cloudFolderID = cloudFolderID, gdriveLs = gdriveLs,
                                    cachePath = cachePath,
                                    detailed_key, functionName, cache_file = cache_file,
                                    changedSaveFormat = changedSaveFormat, sameCacheID,
                                    cache_file_orig, func, shownCache = shownCache,
                                    full_call = full_call,
                                    outputObjects = outputObjects,
                                    cacheSaveFormat = cacheSaveFormat,
                                    .cacheChaining = .cacheChaining,
                                    drv = drv, conn = conn, verbose = verbose)
    return(output)

  }
  invisible(.returnNothing)
}


check_and_get_memoised_copy <- function(detailed_key, cachePaths, functionName, func,
                                        useCache, useCloud, cloudFolderID, gdriveLs,
                                        full_call, outputObjects,
                                        cacheSaveFormat = getOption("reproducible.cacheSaveFormat"),
                                        .cacheChaining = getOption("reproducible.cacheChaining", FALSE),
                                        drv, conn, verbose) {
  cache_key <- detailed_key$key
  if (getOption("reproducible.useMemoise", FALSE)) {
    for (cachePath in cachePaths) {
      cache_key_in_memoiseEnv <- exists(cache_key, envir = memoiseEnv(cachePath), inherits = FALSE)
      if (isTRUE(cache_key_in_memoiseEnv))
        break
    }

    if (cache_key_in_memoiseEnv) {
      output <- loadFromDiskOrMemoise(fromMemoise = TRUE, useCache = useCache, useCloud = useCloud,
                                      cloudFolderID = cloudFolderID, gdriveLs = gdriveLs,
                                      cachePath = cachePath, detailed_key = detailed_key,
                                      functionName = functionName, func = func,
                                      full_call = full_call,
                                      changedSaveFormat = FALSE,
                                      outputObjects = outputObjects,
                                      cacheSaveFormat = cacheSaveFormat,
                                      .cacheChaining = .cacheChaining,
                                      drv = drv, conn = conn, verbose = verbose,
                                      )
      return(output)
    }
  } else {
    # If useMemoise gets turned off, it needs to be emptied or there will be stale entries.
    # Preserve the "shownCache" binding -- it holds the showCache memoised
    # data.table and the async-spawn job table; both are independent of
    # useMemoise and clearing them would defeat the lazy async pre-populate
    # mechanism (jobs would be re-spawned on every Cache() call).
    me <- memoiseEnv(cachePaths[[1]])
    le <- setdiff(ls(me), "shownCache")
    if (length(le))
      rm(list = le, envir = me)
  }
  return(invisible(.returnNothing))
}


releaseLockFile <- function(locked) {
  filelock::unlock(locked)
  ## Do NOT delete the lock file: the fcntl lock is what protects the critical
  ## section, not the file's existence.  Deleting and recreating the file under
  ## concurrent load creates two bugs:
  ##   1. Workers that were blocked on fcntl(F_SETLKW) already have the old inode
  ##      open; a fresh caller that arrives after the delete creates a *new* inode
  ##      at the same path — both callers hold a "lock" on different inodes and
  ##      the critical section is no longer protected.
  ##   2. If a prior run was executed as root (or another user), a stale .lock file
  ##      with wrong ownership is left behind; the next caller gets EACCES at
  ##      open(O_RDWR|O_CREAT) — "Permission denied".
  ## Leaving the (empty) .lock file in place is safe and correct.
}


#' @importFrom stats runif
lockFile <- function(cachePath, cache_key,
                     envir   = parent.frame(),
                     verbose = getOption("reproducible.verbose")) {
  if (!useDBI()) {
    csd <- CacheStorageDir(cachePath)
    checkPath(csd, create = TRUE)

    lock_path <- file.path(csd, paste0(cache_key, suffixLockFile()))

    ## Three outcomes from filelock::lock:
    ##   NULL   — contention; sleep 2.5 s and retry
    ##   EMFILE — process near fd limit from other sources; gc + small sleep
    ##   EACCES — stale file owned by another user; remove and retry
    ##   other  — unexpected; re-throw immediately
    ##
    ## Note: PredictiveEcology/filelock >= 1.0.3.9001 fixes a bug in the
    ## upstream package where every failed non-blocking attempt leaked one fd
    ## (close()/CloseHandle() missing on the NULL return path in C).

    locked          <- NULL
    waiting         <- FALSE
    emfile_attempts <- 0L

    repeat {
      locked <- tryCatch(
        filelock::lock(lock_path, timeout = 0L),
        error = function(e) {
          msg <- conditionMessage(e)
          if (!grepl("Cannot open lock file", msg, fixed = TRUE)) stop(e)

          if (grepl("Too many open files", msg, fixed = TRUE)) {
            emfile_attempts <<- emfile_attempts + 1L
            if (emfile_attempts > 10L)
              stop("Persistent 'Too many open files' acquiring lock: ", lock_path,
                   "\nRaise ulimit -n or report a filelock fd-leak bug",
                   call. = FALSE)
            gc(FALSE)
            Sys.sleep(runif(1L, 0.1, 0.3) * emfile_attempts)
            return(NULL)
          }

          ## EACCES or similar — remove stale file and retry
          removed <- suppressWarnings(file.remove(lock_path))
          if (!isTRUE(removed))
            stop("Cannot open lock file and cannot remove it.\n",
                 "Manually delete (may need sudo): ", lock_path, "\n",
                 "Original error: ", msg, call. = FALSE)
          messageCache("Lock file not accessible; removed and retrying",
                       verbose = verbose + 1)
          dir.create(csd, showWarnings = FALSE, recursive = TRUE)
          return(NULL)
        }
      )

      if (!is.null(locked)) break

      if (!waiting) {
        waiting <- TRUE
        messageCache(
          "The cache file (", lock_path, ") is locked due to a concurrent process; waiting...",
          "\nTo diagnose the holding process (works on Linux/macOS):",
          "\n  system(\"fuser '", lock_path, "'\")",
          "\n  system(\"lsof '", lock_path, "'\")",
          "\nOn a network filesystem (NFS/CIFS), unlink() will NOT remove the file while",
          "\na process holds it open -- kill the holding process first, then the lock releases.",
          "\nIf no process is found (stale lock on a local filesystem), then delete the lockfile:",
          "\n  unlink('", lock_path, "', force = TRUE)",
          verbose = verbose + 2
        )
      }

      Sys.sleep(2.5)
    }

    if (waiting)
      messageCache("  ... ", lock_path, " released, continuing ... ", verbose = verbose + 2)

    # on.exit(filelock::unlock(locked), add = TRUE)
    #
    # # Try repeatedly, but with bounded waits and backoff
    # repeat {
    #   ## If you still want a time cap on the *attempt*, make it transient and reset:
    #   setTimeLimit(elapsed = 3, transient = TRUE)
    #   locked <- filelock::lock(lock_path, timeout = 250000)   # ~2.5 s wait, returns NULL on timeout
    #   setTimeLimit(elapsed = Inf, transient = TRUE)
    #
    #   if (!is.null(locked)) break  # acquired
    #
    #   if (isTRUE(first)) {
    #     first <- FALSE
    #     messageCache(
    #       "The cache file (", lock_path, ") is locked due to a concurrent process; waiting... ",
    #       "\nIf there is no concurrent process (i.e., no parallelism), delete that lockfile",
    #       verbose = verbose + 2
    #     )
    #   }
    #   Sys.sleep(0.25)  # backoff
    # }
    # Ensure release when the *outer* scope exits
    on.exit2(releaseLockFile(locked), envir = envir)
    locked
  }
}


#' Fold cloud cache metadata into a local `showCache` result for `showSimilar`
#'
#' Restricts the cloud-sourced metadata (from [showCacheCloud()]) to the same
#' function as the current call, then `rbind`s it onto the local `shownCache`,
#' de-duplicating on `cacheId`/`tagKey`/`tagValue`. Pure (no I/O) so the merge
#' behaviour can be tested without Google Drive.
#'
#' @param shownCache   The local [showCache()] `data.table`.
#' @param cloudShown   The cloud metadata `data.table` from [showCacheCloud()].
#' @param .functionName The function name to keep from `cloudShown` (or `NULL` for all).
#' @return A combined cache `data.table`.
#' @keywords internal
mergeShownCacheCloud <- function(shownCache, cloudShown, .functionName = NULL) {
  if (is.null(cloudShown) || NROW(cloudShown) == 0)
    return(shownCache)
  if (!is.null(.functionName))
    cloudShown <- cloudShown[cloudShown[tagKey %in% "function" & tagValue %in% .functionName,
                                        "cacheId", with = FALSE], on = "cacheId", nomatch = NULL]
  if (NROW(cloudShown) == 0)
    return(shownCache)
  shownCache <- rbindlist(list(shownCache, cloudShown), fill = TRUE, use.names = TRUE)
  unique(shownCache, by = c("cacheId", "tagKey", "tagValue"))
}


CacheDBFileCheckAndCreate <- function(cachePath, drv = NULL, conn = NULL, verbose) {

  convertDBbackendIfIncorrect(cachePath, drv, conn, verbose = verbose - 1)

  dbfile <- CacheDBFile(cachePath, drv = drv, conn = conn)
  if (isTRUE(!file.exists(dbfile[1])))
    file.create(dbfile[1])
  if (!useDBI()) {
    oldDBFile <- file.path(cachePath, "cache.db")
    if (isTRUE(file.exists(oldDBFile)))
      file.remove(oldDBFile)
  }
  dbfile
}


wrapSaveToCache <- function(outputFromEvaluate, metadata, cache_key, cachePath, # userTags,
                            preDigest, .functionName, outputObjects,
                            cacheSaveFormat = getOption("reproducible.cacheSaveFormat"),
                            drv, conn, verbose) {
  cacheIdIdentical <- cache_Id_Identical(metadata, cachePath, cache_key, cacheSaveFormat = cacheSaveFormat)
  linkToCacheId <- if (!is.null(cacheIdIdentical)) filePathSansExt(basename(cacheIdIdentical))  else NULL
  outputToSave <- .wrap(outputFromEvaluate, cachePath = cachePath, preDigest = preDigest,
                        outputObjects = outputObjects,
                        cacheId = cache_key, verbose = verbose)
  metadata <- metadata_update(outputToSave, metadata, cache_key) # .wrap may have added tags
  userTags <- paste0(metadata$tagKey, ":", metadata$tagValue)
  fs <- saveToCache(cachePath = cachePath, # drv = NULL, conn = NULL,
                    obj = outputToSave, verbose = verbose, # cache_file[1],
                    userTags = userTags, linkToCacheId = linkToCacheId,
                    cacheSaveFormat = cacheSaveFormat,
                    drv = drv, conn = conn,
                    cacheId = cache_key)
  .message$Saved(cachePath, cache_key, functionName = .functionName,
                 cacheSaveFormat = cacheSaveFormat, verbose = verbose)
  return(metadata)
}


doSaveToCache <- function(outputFromEvaluate, metadata, cachePaths, callList, # func,
                          .objects, length, algo, quick, classOptions,
                          cache_file, userTags, # .functionName,
                          debugCache,
                          detailed_key, # func_call,
                          outputObjects,
                          useCloud, cloudFolderID, gdriveLs,
                          cacheSaveFormat = getOption("reproducible.cacheSaveFormat"),
                          drv, conn, useMemoise = getOption("reproducible.useMemoise", FALSE),
                          .cacheChaining = getOption("reproducible.cacheChaining", FALSE),
                          verbose, timeSaveStart, timeEvaluateStart) {
  # Can't save NULL with attributes
  if (is.null(outputFromEvaluate)) outputFromEvaluate <- "NULL"

  outputFromEvaluate <- addCacheAttr(outputFromEvaluate, .CacheIsNew = TRUE, detailed_key$key, callList$func)

  outputFromEvaluate <- cacheChainingPost(detailed_key, outputFromEvaluate,
                                            attr(callList$new_call, ".Cache")[cacheChainingOuterFunctionName],
                                            cachePaths[[1]], linkToCacheId = NULL, cacheSaveFormat,
                                          .cacheChaining = .cacheChaining, drv, conn, verbose = verbose)
  metadata <- wrapSaveToCache(outputFromEvaluate, metadata, detailed_key$key, cachePaths[[1]],
                              # userTags = paste0(metadata$tagKey, ":", metadata$tagValue),
                              outputObjects = outputObjects,
                              preDigest = detailed_key$preDigest, callList$.functionName,
                              cacheSaveFormat = cacheSaveFormat, drv, conn, verbose)

  # Memoize the outputFromEvaluate by saving it in RAM
  if (isTRUE(useMemoise)) {
    assign(detailed_key$key, outputFromEvaluate, envir = memoiseEnv(cachePaths[[1]]))
  }


  if (identical(outputFromEvaluate, "NULL")) outputFromEvaluate <- NULL

  if (isTRUE(!is.na(pmatch(debugCache, "complete"))))
    outputFromEvaluate <- .debugCache(outputFromEvaluate, detailed_key$preDigest, fullCall = callList$func_call)

  if (cloudWrite(useCloud)) {
    cloudUploadFromCache(detailed_key$key %in% filePathSansExt(gdriveLs[["name"]]), detailed_key$key,
                         cachePaths[[1]], cloudFolderID = cloudFolderID, outputFromEvaluate, verbose = verbose)
  }
  outputFromEvaluate

}


loadFromDiskOrMemoise <- function(fromMemoise = FALSE, useCache,
                                  useCloud, cloudFolderID = NULL, gdriveLs,
                                  cachePath, detailed_key,
                                  functionName,
                                  cache_file = NULL, changedSaveFormat, sameCacheID,
                                  cache_file_orig, func, shownCache = NULL,
                                  full_call, outputObjects,
                                  cacheSaveFormat = getOption("reproducible.cacheSaveFormat"),
                                  .cacheChaining = getOption("reproducible.cacheChaining", FALSE),
                                  drv, conn, verbose) {

  cache_key <- detailed_key$key
  if (identical(useCache, "overwrite")) {
    clearCacheOverwrite(cachePath, cache_key, functionName, drv, conn, verbose)
    return(invisible(.returnNothing))
  } else {
    format <- if (missing(cache_file) || is.null(cache_file)) cacheSaveFormat else
      fileExt(cache_file)

    for (iii in 1:2) {
      fe <- CacheDBFileSingle(cachePath = cachePath, cacheId = cache_key,
                              cacheSaveFormat = cacheSaveFormat)
      if (useDBI()) {
        rerun <- FALSE
      } else {
        feReally <- file.exists(fe)
        if (any(feReally %in% FALSE)) {
          formatNew <- formatCheck(cachePath, cache_key, format)
          if (!identical(formatNew, cacheSaveFormat)) {
            cacheSaveFormat <- formatNew
            next
          }

        }
        # usually happens when user had memoise on before, then turned it off, then turned it back on
        danglingMemoise <- (isFALSE(any(feReally)) && fromMemoise)
        if (isTRUE(danglingMemoise)) rm(list = cache_key, envir = memoiseEnv(cachePath))
        rerun <- (!isTRUE(any(feReally)) && !fromMemoise) || danglingMemoise
        break
      }
    }

    cacheSaveFormatFail <- FALSE
    if (is.null(shownCache)) {
      # shownCache <- showCacheFast(cache_key, cachePath, dtFile = fe,
      #                                 # cacheSaveFormat = cacheSaveFormat,
      #                                 drv = drv, conn = conn)
      shownCache <- try(showCacheFast(cache_key, cachePath, dtFile = fe,
                                      # cacheSaveFormat = cacheSaveFormat,
                                      drv = drv, conn = conn),
                        silent = TRUE)
      if (is(shownCache, "try-error")) {
        if (isTRUE(any(grepl("format not detected", shownCache)))) {
          cacheSaveFormatFail <- TRUE
        } else { # e.g., change from qs to qs2
          cacheSaveFormatFail <- TRUE
        }
      }
    }

    if (isFALSE(cacheSaveFormatFail))
      .cacheMessageObjectToRetrieve(functionName, shownCache, cachePath,
                                    cacheId = cache_key, cacheSaveFormat = cacheSaveFormat, verbose = verbose)
    memoiseFail <- FALSE
    if (fromMemoise && !rerun) {
      # output <- get(cache_key, envir = memoiseEnv(cachePath))
      output <- .unwrap(get(cache_key, envir = memoiseEnv(cachePath)), cacheId = cache_key, cachePath = cachePath,
                        drv = drv, conn = conn)
      # need to update the individual files in file-backed objects from the cache; can't use memoise

      # Some objects, especially Rcpp objects can get stale; rerun if this is the case; the test with subsetting 1st element
      #   is not great, but I could not find a better one that will fail on those Rcpp fails. The problem
      #   is that the object exists, but it's inner structure is wrong
      outputTestIntegrity <- try(output[1], silent = TRUE) # This needs to be `try`, not `try2`
      fns <- try2(Filenames(output), silent = TRUE) # previous will only get some of the failures
      memoiseFail <- dealWithCacheRecoveryErrors(memoiseFail, outputTestIntegrity, fns, cache_key, cachePath, outputObjects)
      # if (isTRUE(is(outputTestIntegrity, "try-error")) || isTRUE(is(fns, "try-error"))) {
      #   failMsgs <- "external pointer.+not valid|NULL value passed as symbol address"
      #   if (isTRUE(any(grepl(failMsgs, outputTestIntegrity))) ||
      #       isTRUE(any(grepl(failMsgs, fns)))) {
      #     memoiseFail <- TRUE
      #     rm(list = cache_key, envir = memoiseEnv(cachePath))
      #     cache_file <- CacheStoredFile(cachePath, cache_key, readOnly = TRUE)
      #   }
      # } else {
      #   fns <- fns[nzchar(fns)]
      #   if (!is.null(fns) && length(fns) > 0) {
      #     fnsInOutputObjects <- intersect(names(fns), outputObjects)
      #     fns <- fns[fnsInOutputObjects]
      #     fnsExistBefore <- try2(file.exists(fns))
      #     fnsInCache <- file.path(CacheStorageDir(cachePath),
      #                             basename(.prefix(fns, prefixCacheId(cacheId = cache_key))))
      #     hardLinkOrCopy(fnsInCache, fns, overwrite = TRUE, verbose = FALSE)
      #     fnsExistAfter <- file.exists(fns)
      #     if (any(fnsExistAfter %in% FALSE) && isTRUE(any(fnsExistBefore != fnsExistAfter))) # this means that hardLinkOrCopy failed
      #       browser()
      #   }
      # }
    }

    if (!fromMemoise || rerun || memoiseFail || cacheSaveFormatFail) {
      obj <- if (!is.null(cache_file)) {
        # loadFile(cache_file, cacheSaveFormat = cacheSaveFormat,
        #              cacheId = cache_key, cachePath = cachePath, # in case it needs swapCacheFormat
        #              drv = drv, conn = conn, verbose = verbose)
        try(loadFile(cache_file, cacheSaveFormat = cacheSaveFormat,
                     cacheId = cache_key, cachePath = cachePath, # in case it needs swapCacheFormat
                     drv = drv, conn = conn, verbose = verbose), silent = TRUE)
      } else {
        rerun <- TRUE
      }

      if (isTRUE(changedSaveFormat)) {
        swapTry <- try(swapCacheFileFormat(
          wrappedObj = obj, cachePath = cachePath, drv = drv, conn = conn,
          cacheId = cache_key, sameCacheID = sameCacheID,
          userTags = paste0(shownCache$tagKey, ":", shownCache$tagValue),
          newFile = cache_file_orig, verbose = verbose), silent = TRUE)
        cacheSaveFormat <- fileExt(cache_file_orig) # setdiff(.cacheSaveFormats, cacheSaveFormat)
        # rerun <- TRUE
      }
      output <- try(.unwrap(obj, cachePath = cachePath, cacheId = cache_key))
      if (is(obj, "try-error") || rerun || is(output, "try-error")) {
        messageCache("It looks like the cache file is corrupt or was interrupted during write; deleting and recalculating")
        otherFiles2 <- dir(CacheStorageDir(cachePath), pattern = cache_key, full.names = TRUE)
        if (!is(shownCache, "try-error")) {
          if (!is.null(shownCache)) {
            otherFiles <- normPath(file.path(CacheStorageDir(cachePath),
                                             shownCache[tagKey == "filesToLoad"]$tagValue))
            otherFiles2 <- c(otherFiles, otherFiles2)
          }
        }
        rmFiles <- unique(c(cache_file, otherFiles2))
        unlink(rmFiles)
        return(.returnNothing)
      }

    }

    if (cloudWrite(useCloud)) {
      cloudUploadFromCache(cache_key %in% filePathSansExt(gdriveLs[["name"]]), cache_key,
                           cachePath, cloudFolderID = cloudFolderID, output, verbose = verbose)
    }

    .cacheMessage(object = output, functionName = functionName, fromMemoise = fromMemoise, verbose = verbose)

    if (getOption("reproducible.useMemoise", FALSE)) {
      cache_key_in_memoiseEnv <- exists(cache_key, envir = memoiseEnv(cachePath), inherits = FALSE)
      if (cache_key_in_memoiseEnv %in% FALSE) {
        # assign(cache_key, .unwrap(obj, cachePath = cachePath, cacheId = cache_key),
        #        envir = memoiseEnv(cachePath))
        assign(cache_key, obj, envir = memoiseEnv(cachePath)) # try without .unwrap in memoiseEnv
      }
    }

    if (!is.null(output))
      output <- addCacheAttr(output, .CacheIsNew = FALSE, outputHash = cache_key, func)

    .addTagsRepoAccessedTime(cache_key, cachePath = cachePath, cacheSaveFormat = cacheSaveFormat)
    attr(output, ".Cache")$newCache <- FALSE

    .dotsFromCache <- as.list(attr(full_call, ".Cache")$func_call)[-1]
    # # This allows for any class specific things
    if ("object" %in% names(.dotsFromCache))
      .dotsFromCache <- .dotsFromCache[setdiff(names(.dotsFromCache), "object")]

    output <- do.call(.prepareOutput, args = append(list(object = output, cachePath),
                                                    .dotsFromCache))

    output <- cacheChainingPost(detailed_key, output,
                                attr(full_call, ".Cache")[cacheChainingOuterFunctionName],
                                cachePath, linkToCacheId = NULL, cacheSaveFormat,
                                .cacheChaining = .cacheChaining, drv, conn, verbose = verbose)

    return(output)
  }
}


## `useCloud` accepts: TRUE/FALSE/NULL, or one of "push"/"pull".
##   - TRUE  / "push": developer role -- bidirectional. Download on cloud hit;
##                     upload on miss.
##   - "pull"        : user role -- read-only. Download on cloud hit; never
##                     upload. If the local cache already has the object, the
##                     cloud is not consulted at all (the gdriveLs fetch is
##                     deferred until after the local check fails).
##   - FALSE / NULL  : cloud disabled.
## Legacy "^w"/"^r" prefix matching is retained for back-compat (e.g. "write",
## "read", "readOnly") since the contract is otherwise narrow.
cloudWrite <- function(useCloud) {
  isTRUE(useCloud) ||
    identical(useCloud, "push") ||
    isTRUE(any(grepl("^w", useCloud) %in% 1))
}


cloudWriteOrRead <- function(useCloud) {
  cloudWrite(useCloud) || cloudRead(useCloud)
}


cloudReadOnly <- function(useCloud) {
  identical(useCloud, "pull") ||
    isTRUE(any(grepl("^r", useCloud) %in% 1))
}


cloudRead <- function(useCloud) {
  cloudReadOnly(useCloud) || isTRUE(useCloud) || identical(useCloud, "push")
}


## Validate the `useCloud` argument and return it unchanged. Errors on a
## character value that is not "pull" or "push" (or a legacy ^w/^r prefix).
validateUseCloud <- function(useCloud) {
  if (is.null(useCloud) || isTRUE(useCloud) || isFALSE(useCloud))
    return(invisible(useCloud))
  if (is.character(useCloud) && length(useCloud) == 1L &&
      (useCloud %in% c("pull", "push") ||
       grepl("^[wr]", useCloud)))
    return(invisible(useCloud))
  stop("`useCloud` must be TRUE, FALSE, NULL, \"pull\", or \"push\"; got: ",
       deparse(useCloud), call. = FALSE)
}


keyInGdriveLs <- function(cache_key, gdriveLs) {
  grepl(paste0("^(", cache_key, ").+$"), gdriveLs[["name"]])
  #filePathSansExt(filePathSansExt(gdriveLs[["name"]])) %in%  # double filePathSansExt because of the .dbFile.rds
  #  cache_key
}




.dtFileMainCols <- c("cacheId", "tagKey", "tagValue", "createdDate")


#' @keywords internal

getCacheRepos <- function(cachePath, modifiedDots, verbose = getOption("reproducible.verbose", 1)) {
  if (is.null(cachePath)) {
    cachePath <- .checkCacheRepo(modifiedDots, create = TRUE, verbose = verbose)
  } else {
    if (any(!dir.exists(unlist(cachePath))))
      cachePath <- lapply(cachePath, function(repo) {
        if (!dir.exists(repo))
          repo <- checkPath(repo, create = TRUE)
        repo
      })
  }
  return(cachePath)
}



cloudFolderFromCacheRepo <- function(cachePath) {
  paste0(basename2(dirname(cachePath)), "_", basename2(cachePath))
}


searchInRepos <- function(cachePaths, outputHash, drv, conn, verbose = getOption("reproducible.verbose")) {
  dbTabNam <- NULL
  tries <- 1
  while (tries <= length(cachePaths)) {
    repo <- cachePaths[[tries]]
    if (useDBI()) {
      if (is.list(conn))
        conn <- conn[[cachePaths[1]]]
      dbTabNam <- CacheDBTableName(repo, drv = drv)

      isInRepo <- getHashFromDB(tries, conn, drv, repo, dbTabNam, outputHash)
      # if (tries > 1) {
      #   DBI::dbDisconnect(conn)
      #   conn <- dbConnectAll(drv, cachePath = repo)
      # }
      # qry <- glue::glue_sql("SELECT * FROM {DBI::SQL(glue::double_quote(dbTabName))} where \"cacheId\" = ({outputHash})",
      #   dbTabName = dbTabNam,
      #   outputHash = outputHash,
      #   .con = conn
      # )
      # res <- retry(
      #   retries = 15, exponentialDecayBase = 1.01,
      #   quote(DBI::dbSendQuery(conn, qry))
      # )
      # isInRepo <- setDT(DBI::dbFetch(res))
      # DBI::dbClearResult(res)
    } else {
      # The next line will find it whether it is qs2, rds or other; this is necessary for "change cacheSaveFormat"
      csf <- CacheStoredFile(cachePath = repo, cacheId = outputHash, cacheSaveFormat = "check")

      if (all(file.exists(csf))) {
        dtFile <- CacheDBFileSingle(cachePath = repo, cacheId = outputHash)

        if (!file.exists(dtFile)) { # check first for wrong rds vs qs2
          dtFile <- CacheDBFileSingle(cachePath = repo, cacheId = outputHash, cacheSaveFormat = "check")
          fe <- file.exists(dtFile)
          if (isTRUE(!(fe))) { # still doesn't == means it is broken state
            warning(
              "The Cache file exists for ", outputHash, ", but there is no database entry for it; removing ",
              "the file and rerunning the call"
            )
            unlink(csf)
            dtFile <- NULL
          } else if (length(fe) > 1) { # has both the qs2 and rds dbFile
            stop(
              "Internal error: found both a qs2 and rds cache database file for ",
              outputHash, " in ", repo, ". Please report this at ",
              "https://github.com/PredictiveEcology/reproducible/issues"
            )
          }
        }

        isInRepo <- if (!is.null(dtFile)) {
          loadFile(dtFile,
                   cacheId = outputHash, cachePath = repo, # in case it needs swapCacheFormat
                   drv = drv, conn = conn, verbose = verbose)
        } else {
          NULL
        }
      } else {
        isInRepo <- data.table::copy(.emptyCacheTable)
      }
    }
    fullCacheTableForObj <- isInRepo
    if (NROW(isInRepo) > 1) isInRepo <- isInRepo[NROW(isInRepo), ]
    if (NROW(isInRepo) > 0) {
      # browser(expr = exists("._Cache_4"))
      cachePath <- repo
      break
    }
    tries <- tries + 1
  }
  list(
    isInRepo = isInRepo, dbTabName = dbTabNam, fullCacheTableForObj = fullCacheTableForObj,
    cachePath = repo
  )
}



addCacheAttr <- function(output, .CacheIsNew, outputHash, FUN) {
  output <- .setSubAttrInList(output, ".Cache", "newCache", .CacheIsNew)
  attr(output, "tags") <- paste0("cacheId:", outputHash)
  attr(output, callInCache) <- ""
  if (!identical(attr(output, ".Cache")$newCache, .CacheIsNew)) {
    stop("attributes are not correct 3")
  }
  if (!identical(attr(output, callInCache), "")) {
    stop("attributes are not correct 4")
  }
  if (!identical(attr(output, "tags"), paste0("cacheId:", outputHash))) {
    stop("attributes are not correct 5")
  }

  if (isS4(FUN)) {
    attr(output, "function") <- FUN@generic
    if (!identical(attr(output, "function"), FUN@generic)) {
      stop("There is an unknown error 03")
    }
  }
  output
}




checkConns <- function(cachePaths, conn) {
  conns <- list()
  if (!is.null(conn)) { # if the conn was passed by user
    if (!is.list(conn)) {
      conn <- list(conn)
    }
    if (!identical(length(cachePaths), length(conn))) {
      stop("conn and cachePath are both provided, but are different lengths which is not allowed")
    }
    names(conn) <- cachePaths
    conns <- conn
  }
}



createConns <- function(cachePath, conns, drv,
                        verbose = getOption("reproducible.verbose")) {
  if (useDBI()) {
    drv <- getDrv(drv)
    if (is.null(conns[[cachePath]])) {
      conns[[cachePath]] <- dbConnectAll(drv, cachePath = cachePath)
      # PRAGMA is SQLite-specific syntax. `reproducible.drv`/`reproducible.conn`
      # accept any DBI backend (RPostgres is the documented other one), and those
      # error on PRAGMA, so gate on the connection actually being SQLite. Also
      # skips safely when dbConnectAll() failed and returned NULL.
      if (is(conns[[cachePath]], "SQLiteConnection")) {
        RSQLite::dbClearResult(RSQLite::dbSendQuery(conns[[cachePath]], "PRAGMA busy_timeout=5000;"))
        RSQLite::dbClearResult(RSQLite::dbSendQuery(conns[[cachePath]], "PRAGMA journal_mode=WAL;"))
      }
    }
  }

  isIntactRepo <- CacheIsACache(
    cachePath = cachePath, drv = drv, create = TRUE,
    conn = conns[[cachePath]], verbose = verbose
  )
  if (any(!isIntactRepo)) {
    ret <- createCache(cachePath,
                       drv = drv, conn = conns[[cachePath]],
                       force = isIntactRepo
    )
  }
  conns
}


getHashFromDB <- function(tries, conn, drv, repo, dbTabNam, outputHash) {
  if (tries > 1) {
    DBI::dbDisconnect(conn)
    conn <- dbConnectAll(drv, cachePath = repo)
  }
  qry <- glue::glue_sql("SELECT * FROM {DBI::SQL(glue::double_quote(dbTabName))} where \"cacheId\" = ({outputHash})",
                        dbTabName = dbTabNam,
                        outputHash = outputHash,
                        .con = conn
  )
  res <- retry(
    retries = 15, exponentialDecayBase = 1.01,
    quote(DBI::dbSendQuery(conn, qry))
  )
  isInRepo <- setDT(DBI::dbFetch(res))
  DBI::dbClearResult(res)
  isInRepo
}


getPreviousEntryInCache <- function(.functionName, verbose, data.table, setorderv, tagKey, cacheId) {
  sc <- showCache(fun = .functionName, verbose = -2)
  if (NROW(sc)) {
    messageCache("cacheId is 'previous' meaning it will recover the most recent ",
                 "cache item (accessed) that matches on .functionName: ",
                 .messageFunctionFn(.functionName), "\nPlease ensure ",
                 "the function name is precise enough for this behaviour", verbose = verbose)
    outputHashNew <- data.table::setorderv(sc[tagKey == "accessed"], "tagValue", order = -1L)
    outputHash <- outputHashNew$cacheId[1]
    sc <- sc[cacheId %in% outputHash, ]
    attr(sc, "cacheId") <- outputHash
    # sc <- showCacheFast(cacheId = outputHash)
  } else {
    sc <- NULL
  }
}
