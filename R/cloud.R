utils::globalVariables(c(
  "cacheId", "checksumsFilename", "checksumsID", "id"
))

# One-time (per session) notice that cloud caching is being skipped because no
# cloudFolderID is set. reproducible deliberately does NOT auto-create a cloud
# folder from the local cache path: that folder differs across machines and
# would silently break shared/cloud caching.
# NOTE: keep this defined ABOVE the roxygen block below. If it sits between that
# block and checkAndMakeCloudFolderID, roxygen2 misattaches the `@export` to this
# internal helper and drops the export of checkAndMakeCloudFolderID.
.cloudFolderUnsetMessageOnce <- function(verbose = getOption("reproducible.verbose", 1)) {
  if (isTRUE(.pkgEnv$.cloudFolderUnsetEmitted)) return(invisible())
  messageCache(
    "useCloud is on but no cloudFolderID is set (neither the argument nor ",
    "options(reproducible.cloudFolderID)); skipping cloud caching (local cache ",
    "only). reproducible does not create a cloud folder automatically. To enable ",
    "cloud caching, set the same folder on every machine, e.g. ",
    "options(reproducible.cloudFolderID = googledrive::as_id(\"<id>\")).",
    verbose = verbose
  )
  .pkgEnv$.cloudFolderUnsetEmitted <- TRUE
  invisible()
}

#' Check for presence of `checkFolderID` (for `Cache(useCloud)`)
#'
#' Will check for presence of a `cloudFolderID` and make a new one
#' if one not present on Google Drive, with a warning.
#'
#' @inheritParams Cache
#' @param cloudFolderID The google folder ID where cloud caching will occur.
#' @param create Logical. If `TRUE`, then the `cloudFolderID` will be created.
#'     This should be used with caution as there are no checks for overwriting.
#'     See `googledrive::drive_mkdir`. Default `FALSE`.
#' @param overwrite Logical. Passed to `googledrive::drive_mkdir`.
#' @param team_drive Logical indicating whether to check team drives.
#'
#' @return
#' Returns the character string of the cloud folder ID created or reported
#' @export
checkAndMakeCloudFolderID <- function(cloudFolderID = getOption("reproducible.cloudFolderID", NULL),
                                      cachePath = NULL,
                                      create = FALSE,
                                      overwrite = FALSE,
                                      verbose = getOption("reproducible.verbose", 1),
                                      team_drive = NULL) {
  .requireNamespace("googledrive", stopOnFALSE = TRUE, messageStart = "to use google drive files")

  if (!is(cloudFolderID, "dribble")) {
    isNullCFI <- is.null(cloudFolderID)
    cloudFolderIDorig <- cloudFolderID # for messaging if it cannot be resolved
    if (isNullCFI) {
      if (is.null(cachePath)) {
        cachePath <- .checkCacheRepo(cachePath, verbose = verbose)
      }
      cloudFolderID <- cloudFolderFromCacheRepo(cachePath)
    }

    # This is an imperfect test for a google drive ID ... because of this, we try 2x,
    #   first with best guess, then if wrong, try the other branch of "if (isID)"
    stripHTTP <- gsub("http.+drive.google.com.+folders/(.{32,33})\\?*.+$", "\\1", cloudFolderID)
    if (isID(stripHTTP))
      cloudFolderID <- stripHTTP
    isID <- isTRUE(32 <= nchar(cloudFolderID) && nchar(cloudFolderID) <= 33)
    if (packageVersion("googledrive") < "2.0.0") {
      args <- list(temp_drive = team_drive)
    } else {
      args <- list(shared_drive = team_drive)
    }
    for (attempt in 1:2) {
      cfidTmp <- if (isID) googledrive::as_id(cloudFolderID) else {
        if (fs::is_absolute_path(cloudFolderID)) {
          cloudFolderID <- cloudFolderFromCacheRepo(cloudFolderID)
        }
        cloudFolderID
      }

      driveLs <- tryCatch(# suppressMessages( # will show: "The googledrive package is requesting access to your Google account."
        do.call(googledrive::drive_get, append(list(cfidTmp), args)# )
        ),
        error = function(e) {
          if (!is.null(e$parent)) {
            if (grepl("File not found", as.character(e$parent)) && attempt == 2) {
              stop(e)
            }
          }
        },
        silent = TRUE
      )
      if (is(driveLs, "dribble")) {
        break
      } else {
        isID <- !isID
      }
    }

    if (NROW(driveLs) == 0) {
      # An explicit cloudFolderID was supplied but Google Drive could not resolve
      # it (not found, or not accessible to the authenticated account). The code
      # below silently substitutes a folder *derived from the local cache path*,
      # which differs from machine to machine -- so cloud caches will NOT be
      # shared across machines (the symptom: identical cacheIds, but each machine
      # recomputes and re-uploads into its own folder). Warn so this is visible.
      if (!isNullCFI) {
        warning(
          "The supplied cloudFolderID (", as.character(cloudFolderIDorig)[1], ") could not be ",
          "resolved on Google Drive -- it was not found, or is not accessible to the ",
          "authenticated Google account. ",
          if (isTRUE(create)) "A folder derived from the local cache path is being used/created instead. " else "",
          "That derived folder differs between machines, so cloud caches will NOT be shared ",
          "across machines/operating systems. To share a cloud cache, ensure every machine ",
          "passes the SAME cloudFolderID (a Drive folder id the authenticated account can ",
          "access), e.g. options(reproducible.cloudFolderID = googledrive::as_id(\"<id>\")).",
          call. = FALSE
        )
      }
      newcfid <- cloudFolderID
      if (isTRUE(create)) {
        if (isID) {
          if (is.null(cachePath)) {
            cachePath <- .checkCacheRepo(cachePath, verbose = verbose)
          }
          cloudFolderID <- cloudFolderFromCacheRepo(cachePath)
        }

        if (fs::is_absolute_path(cloudFolderID)) {
          cloudFolderID <- cloudFolderFromCacheRepo(cloudFolderID)
        }

        newcfid <- try(googledrive::drive_mkdir(cloudFolderID, path = NULL, overwrite = overwrite), silent = TRUE) # can handle a single string
      }
      if (is(newcfid, "try-error")) { # this happens if the folder exists
        a <- newcfid
        if (fs::is_absolute_path(cloudFolderID)) {
          cloudFolderID <- cloudFolderFromCacheRepo(cloudFolderID)
        }
        newcfid <- try(googledrive::drive_get(cloudFolderID))
        if (is(newcfid, "try-error"))
          stop(newcfid)
      }
      cloudFolderID <- newcfid

    } else {
      cloudFolderID <- driveLs
    }
    if (isNullCFI) {
      messageCache("Setting 'reproducible.cloudFolderID' option to be cloudFolder: ",
        ifelse(!is.null(names(cloudFolderID)), cloudFolderID$name, cloudFolderID),
        verbose = verbose
      )
    }
    optionValOrig <- options("reproducible.cloudFolderID" = cloudFolderID)
    on.exit2(options(optionValOrig), envir = parent.frame())
  }
  return(cloudFolderID)
}

driveLs <- function(cloudFolderID = NULL, pattern = NULL, cachePath = getOption("reproducible.cachePath"),
                    verbose = getOption("reproducible.verbose", 1),
                    team_drive = NULL) {
  .requireNamespace("googledrive",
    stopOnFALSE = TRUE,
    messageStart = "to use google drive files"
  )

  if (!is(cloudFolderID, "tbl")) {
    cloudFolderID <- checkAndMakeCloudFolderID(
      cloudFolderID = cloudFolderID, cachePath = cachePath, create = FALSE,
      team_drive = team_drive
    ) # only deals with NULL case
  }

  messageCache("Retrieving file list in cloud folder", verbose = verbose)
  gdriveLs <- retry(quote({
    googledrive::drive_ls(
      path = cloudFolderID, ## TODO: team drives needs a dribble
      pattern = paste0(collapse = "|", c(pattern))
    )
  }))
  if (is(gdriveLs, "try-error")) {
    fnf <- grepl("File not found", gdriveLs)
    if (!fnf) {
      gdriveLs <- retry(quote({
        googledrive::drive_ls(
          path = googledrive::as_id(cloudFolderID), ## TODO: team drives needs a dribble
          pattern = paste0(cloudFolderID, "|", pattern)
        )
      }))
    } else {
      stop("cloudFolderID not found on Gdrive\n", gdriveLs)
    }
  }
  gdriveLs
}


#' Download from cloud, if necessary
#'
#' Meant for internal use, as there are internal objects as arguments.
#'
#' @param newFileName The character string of the local filename that the downloaded object will have
#' @param outputHash The `cacheId` of the object to upload
#' @param gdriveLs The result of `googledrive::drive_ls(googledrive::as_id(cloudFolderID), pattern = "outputHash")`
#' @inheritParams Cache
cloudDownload <- function(outputHash, newFileName, gdriveLs, cachePath, cloudFolderID,
                          drv = getDrv(getOption("reproducible.drv", NULL)),
                          conn = getOption("reproducible.conn", NULL),
                          verbose = getOption("reproducible.verbose")) {
  .requireNamespace("googledrive",
    stopOnFALSE = TRUE,
    messageStart = "to use google drive files"
  )

  messageCache("Downloading cloud copy of ", newFileName, ", with cacheId: ", outputHash,
    verbose = verbose
  )
  isInCloud <- grepl(outputHash, gdriveLs$name)

  outs <- list()
  for (i in 1:1) { # was 1:2 when files were downloaded separately, when file-backed files didn't have cacheId
    localNewFilename <- file.path(tempdir2(), basename2(newFileName))
    outs <- append(outs, lapply(seq_along(isInCloud), function(ind) {
      retry(quote(googledrive::drive_download(
        file = googledrive::as_id(gdriveLs$id[ind]),
        path = localNewFilename[ind], # take first if there are duplicates
        overwrite = TRUE
      )))
    }))
    if (i %in% 1) {
      dtFile <- outs[[1]]$local_path # grep(CacheDBFileSingleExt(), outs$local_path, value = TRUE)
      dt <- loadFile(dtFile, cacheSaveFormat = fileExt(dtFile),
                     cacheId = outputHash, cachePath = cachePath, # in case it needs swapCacheFormat
                     drv = drv, conn = conn, verbose = verbose)
      fromDisk <- extractFromCache(dt, elem = "fromDisk") %in% "TRUE"
      if (all(!fromDisk)) break
      localFilenames <- Filenames(dt)
      # remapFilenames()
      # newFilename2 <- extractFromCache(dt, elem = "origFilename")
      # isInCloud <- seq(newFilename2)
      # gdriveLs <- googledrive::drive_ls(
      #   path = googledrive::as_id(cloudFolderID),
      #   pattern = paste(collapse = "|", newFilename2)
      # )
      # newFilename2 <- newFilename2[match(newFilename2, gdriveLs$name)]
    }
  }
  outs <- rbindlist(outs)

  if (!useDBI()) {
    dtFileInCache <- CacheDBFileSingle(cachePath, cacheId = outputHash)
    suppressMessages(hardLinkOrCopy(dtFile, dtFileInCache))
  }
  objFiles <- grep(CacheDBFileSingleExt(), outs$local_path, value = TRUE, invert = TRUE)
  # objFiles <- grep(paste0(".", formatCheck(cachePath, outputHash)), objFiles, value = TRUE)
  filenamesInCache <- file.path(CacheStorageDir(cachePath = cachePath), basename2(objFiles))
  hardLinkOrCopy(objFiles, to = filenamesInCache)

  if (useDBI()) { # with useDBI = FALSE, the dbFile is already there.
    Map(tv = dt$tagValue, tk = dt$tagKey, function(tv, tk) {
      .addTagsRepo(outputHash, cachePath, tagKey = tk, tagValue = tv, drv = drv, conn = conn)
    })
  }
  inReposPoss <- searchInRepos(
    cachePaths = cachePath, outputHash = outputHash,
    drv = drv, conn = conn
  )
  inReposPoss
}

#' List and read all cloud cache metadata (dbFile) files
#'
#' Downloads every per-`cacheId` metadata file (the small `.dbFile.*` files) from
#' the cloud cache folder, loads them, and `rbindlist`s them into a single cache
#' `data.table` with the same shape as [showCache()]. This is what allows
#' `showSimilar` to compare the current call against artifacts cached by *other*
#' machines sharing the same `cloudFolderID`, not just those cached locally.
#'
#' The full object files are never fetched -- only the small metadata files --
#' and any `cacheId` already present in `existingCacheIds` is skipped, so
#' locally-known artifacts do not pay for a re-download.
#'
#' @param existingCacheIds Character vector of `cacheId`s already known locally;
#'   their cloud metadata is skipped.
#' @inheritParams cloudDownload
#' @return A cache `data.table` (possibly empty) of cloud-only metadata.
#' @keywords internal
showCacheCloud <- function(cloudFolderID, cachePath, existingCacheIds = character(),
                           drv = getDrv(getOption("reproducible.drv", NULL)),
                           conn = getOption("reproducible.conn", NULL),
                           verbose = getOption("reproducible.verbose")) {
  .requireNamespace("googledrive",
    stopOnFALSE = TRUE,
    messageStart = "to use google drive files"
  )

  gdriveLs <- retry(quote(driveLs(cloudFolderID, pattern = suffixMultipleDBFiles(),
                                  cachePath = cachePath, verbose = verbose - 2)))
  if (NROW(gdriveLs) == 0)
    return(.emptyCacheTable)

  # Keep only the per-cacheId metadata files (defensive: the pattern should
  # already exclude object files), then drop any cacheId we already have locally.
  isDbFile <- grepl(suffixMultipleDBFiles(), gdriveLs[["name"]], fixed = TRUE)
  gdriveLs <- gdriveLs[isDbFile, ]
  cacheIds <- gsub(paste0(suffixMultipleDBFiles(), ".*$"), "", gdriveLs[["name"]])
  keep <- !cacheIds %in% existingCacheIds
  gdriveLs <- gdriveLs[keep, ]
  cacheIds <- cacheIds[keep]
  if (NROW(gdriveLs) == 0)
    return(.emptyCacheTable)

  messageCache("Retrieving ", NROW(gdriveLs),
               " cloud cache metadata file(s) for showSimilar", verbose = verbose)

  dts <- lapply(seq_len(NROW(gdriveLs)), function(ind) {
    localFile <- file.path(tempdir2(), basename2(gdriveLs[["name"]][ind]))
    dl <- try(retry(quote(googledrive::drive_download(
      file = googledrive::as_id(gdriveLs[["id"]][ind]),
      path = localFile, overwrite = TRUE
    ))), silent = TRUE)
    if (is(dl, "try-error"))
      return(NULL)
    out <- try(loadFile(localFile, cacheSaveFormat = fileExt(localFile),
                        cacheId = cacheIds[ind], cachePath = cachePath,
                        drv = drv, conn = conn, verbose = verbose - 2),
               silent = TRUE)
    if (is(out, "try-error")) NULL else out
  })
  dts <- Filter(Negate(is.null), dts)
  if (length(dts) == 0)
    return(.emptyCacheTable)
  rbindlist(dts, fill = TRUE)
}

#' Upload a file to cloud directly from local `cachePath`
#'
#' Meant for internal use, as there are internal objects as arguments.
#'
#' @param isInCloud     A logical indicating whether an `outputHash` is in the cloud already.
#' @param outputToSave  Only required if `any(rasters) == TRUE`.
#'                      This is the `Raster*` object.
#' @param rasters       A logical vector of length >= 1 indicating which elements in
#'                      `outputToSave` are `Raster*` objects.
#' @inheritParams cloudDownload
#'
#' @keywords internal
cloudUploadFromCache <- function(isInCloud, outputHash, cachePath, cloudFolderID,
                                 outputToSave, rasters,
                                 verbose = getOption("reproducible.verbose")) {
  .requireNamespace("googledrive",
    stopOnFALSE = TRUE,
    messageStart = "to use google drive files"
  )
  # browser(expr = exists("._cloudUploadFromCache_1"))

  if (!any(isInCloud)) {
    cacheIdFileName <- CacheStoredFile(cachePath, outputHash, "check", obj = outputToSave)
    if (useDBI()) {
      dt <- showCache(userTags = outputHash)
      td <- tempdir()
      useDBI(FALSE, verbose = -1)
      on.exit(useDBI(TRUE, verbose = -1))
      cacheDB <- CacheDBFileSingle(cachePath = td, outputHash) # put it in a temp location b/c don't want persistent
      on.exit(unlink(cacheDB), add = TRUE)
      if (!dir.exists(dirname(cacheDB))) {
        checkPath(dirname(cacheDB), create = TRUE)
        on.exit(unlink(dirname(cacheDB)), add = TRUE)
      }
      suppress <- saveFilesInCacheFolder(obj = dt, fts = cacheDB, cacheId = outputHash, cachePath = cachePath)
      useDBI(TRUE, verbose = -1)
    } else {
      cacheDB <- CacheDBFileSingle(cachePath, outputHash)
    }
    # Only upload the cache files that are actually on disk. CacheStoredFile(...,
    # "check", obj = ) *predicts* extra file-backed sidecars from Filenames(obj)
    # (e.g. SpatRaster backends of a cached simList), but those are not always
    # written under the cacheId in the cache folder -- a cached simList commonly
    # *references* its rasters at their existing locations rather than copying
    # them. Previously a single predicted-but-absent file made this stop() and
    # abort the whole (already locally-saved) run. Cloud upload is best-effort:
    # upload what exists, warn about the rest, and never crash the caller.
    filesExist <- file.exists(cacheIdFileName)
    if (any(filesExist)) {
      if (!all(filesExist)) {
        messageCache(
          "Some predicted cache file(s) are not on disk; uploading the ",
          sum(filesExist), " present file(s) and skipping:\n",
          paste(basename2(cacheIdFileName[!filesExist]), collapse = "\n"),
          verbose = verbose
        )
      }
      cacheIdFileName <- cacheIdFileName[filesExist]
      newFileName <- basename2(cacheIdFileName)

      cloudFolderID <- checkAndMakeCloudFolderID(cloudFolderID = cloudFolderID, cachePath = cachePath, create = TRUE)

      messageCache("Uploading new cached object -- file(s):\n", paste(newFileName, collapse = "\n"),
                   "\n ... with cacheId: ",
                   outputHash, " to cloud folder id: ", cloudFolderID$name, " or ", cloudFolderID$id,
                   verbose = verbose
      )
      du <- Map(med = cacheIdFileName, nam = newFileName, function(med, nam) {
        try(retry(quote(
          googledrive::drive_upload(
            media = med, path = googledrive::as_id(cloudFolderID),
            name = nam, overwrite = FALSE
          )
        )))
      })

      du2 <- try(retry(quote(googledrive::drive_upload(
        media = cacheDB,
        path = googledrive::as_id(cloudFolderID), name = basename2(cacheDB),
        overwrite = FALSE
      ))))
      if (is(du, "try-error")) {
        return(du)
      }
    } else {
      # Nothing on disk to push: do not abort the run -- the local cache is
      # intact; cloud upload is best-effort.
      messageCache("No cache file(s) available on disk to upload for cacheId ",
                   outputHash, "; skipping cloud upload (local cache is intact).",
                   verbose = verbose)
    }
  }
  # cloudUploadRasterBackends(obj = outputToSave, cloudFolderID)
}

cloudUploadRasterBackends <- function(obj, cloudFolderID) {
  .requireNamespace("googledrive",
    stopOnFALSE = TRUE,
    messageStart = "to use google drive files"
  )

  # browser(expr = exists("._cloudUploadRasterBackends_1"))
  rasterFilename <- Filenames(obj)
  out <- NULL
  if (!is.null(unlist(rasterFilename)) && length(rasterFilename) > 0) {
    allRelevantFiles <- unique(rasterFilename)
    out <- lapply(allRelevantFiles, function(file) {
      try(retry(quote(googledrive::drive_upload(
        media = file, path = cloudFolderID,
        name = basename(file), overwrite = FALSE
      ))))
    })
  }
  return(invisible(out))
}

cloudDownloadRasterBackend <- function(output, cachePath, cloudFolderID,
                                       drv = getDrv(getOption("reproducible.drv", NULL)),
                                       conn = getOption("reproducible.conn", NULL)) {
  .requireNamespace("googledrive", stopOnFALSE = TRUE, messageStart = "to use google drive files")

  if (is(output, "Raster")) {
    rasterFilename <- Filenames(output)
    if (!is.null(unlist(rasterFilename)) && length(rasterFilename) > 0) {
      gdriveLs2 <- NULL
      cacheRepoRasterDir <- file.path(cachePath, "rasters")
      checkPath(cacheRepoRasterDir, create = TRUE)
      simpleFilenames <- unique(filePathSansExt(basename2(unlist(rasterFilename))))
      retry(quote({
        gdriveLs2 <- googledrive::drive_ls(
          path = as_id(cloudFolderID),
          pattern = paste(collapse = "|", simpleFilenames)
        )
      }))

      if (all(simpleFilenames %in% filePathSansExt(gdriveLs2$name))) {
        filenameMismatches <- unlist(lapply(seq_len(NROW(gdriveLs2)), function(idRowNum) {
          localNewFilename <- file.path(cacheRepoRasterDir, basename2(gdriveLs2$name[idRowNum]))
          filenameMismatch <- identical(localNewFilename, rasterFilename)
          retry(quote(googledrive::drive_download(
            file = gdriveLs2[idRowNum, ],
            path = localNewFilename, # take first if there are duplicates
            overwrite = TRUE
          )))
          return(filenameMismatch)
        }))
        if (any(filenameMismatches)) {
          fnM <- seq_along(filenameMismatches)
          if (is(output, "RasterStack")) {
            for (i in fnM[filenameMismatches]) {
              output@layers[[i]]@file@name <- file.path(cacheRepoRasterDir, basename2(rasterFilename)[i])
            }
          } else {
            output@filename <- file.path(cacheRepoRasterDir, basename2(rasterFilename))
          }
        }
      } else {
        warning(
          "Raster backed files are not available in googledrive; \n",
          "will proceed with rerunning code because cloud copy is incomplete"
        )
        output <- NULL
      }
    }
  }
  output
}

isOrHasRaster <- function(obj) {
  rasters <- if (is(obj, "environment")) {
    .requireNamespace("rlang", stopOnFALSE = TRUE)
    if (rlang::inherits_only(obj, "environment")) {
      lapply(mget(ls(obj), envir = obj), function(x) isOrHasRaster(x))
    } else {
      tryCatch(lapply(
        mget(ls(obj), envir = obj@.xData),
        function(x) isOrHasRaster(x)
      ), error = function(x) FALSE)
    }
  } else if (is.list(obj)) {
    lapply(obj, function(x) isOrHasRaster(x))
  } else {
    is(obj, "Raster") || .isSpatRaster(obj)
  }
  return(rasters)
}

isID <- function(x) {
  isTRUE(32 <= nchar(x) && nchar(x) <= 33)
}
