if (getRversion() >= "3.1.0") {
  utils::globalVariables(c("cacheId", "checksumsFilename", "checksumsID", "id"))
}

#' Check for presence of \code{checkFolderID} (for \code{Cache(useCloud)})
#'
#' Will check for presence of a \code{cloudFolderID} and optionally create a new one
#' if one not present on Google Drive. The function will determine the cloudFolderID using the
#' first non-NULL, non-empty result in the following sequence, in this order: 1. Supplied as
#' argument, 2. \code{getOption("reproducible.cloudFolderID")}, 3.
#' \code{Sys.getenv("reproducible.cloudFolderID")}, 4. determined from the \code{cacheRepo},
#' using \code{cloudFolderFromCacheRepo(cacheRepo)}. Once a cloudFolder has been created,
#' its googledrive ID must be saved or else it may be difficult to find it again. For
#' this reason, whatever result is found with this function, it will be assigned
#' to \code{options("reproducible.cloudFolderID")} so that it can be used again.
#'
#' @inheritParams Cache
#' @param cloudFolderID The google folder ID where cloud caching will occur.
#' @param create Logical. If \code{TRUE}, then the \code{cloudFolderID} will be created.
#'     This should be used with caution as there are no checks for overwriting.
#'     See \code{googledrive::drive_mkdir}. Default \code{FALSE}.
#' @param overwrite Logical. Passed to \code{googledrive::drive_mkdir}.
#' @param team_drive Logical indicating whether to check team drives.
#'
#' @return
#' The \code{cloudFolderID} as a \code{dribble}, which will also be assigned to
#' \code{options("reproducible.cloudFolderID")} so that it can be used again.
#'
#' @export
cloudFolderID <- function(cloudFolderID = NULL,
                                      cacheRepo = getOption("reproducible.cachePath", NULL),
                                      create = FALSE,
                                      overwrite = FALSE,
                                      verbose = getOption("reproducible.verbose", 1),
                                      team_drive = NULL) {
  if (!requireNamespace("googledrive", quietly = TRUE))
    stop(requireNamespaceMsg("googledrive", "to use google drive files"))

  if (is.null(cloudFolderID)) {
    cloudFolderID <- getOption('reproducible.cloudFolderID', NULL)
    if (is.null(cloudFolderID)) {
      cloudFolderID <- Sys.getenv("reproducible.cloudFolderID")
      if (nchar(cloudFolderID) == 0) cloudFolderID <- NULL
    }
  }

  if (!is(cloudFolderID, "dribble")) {
    isNullCFI <- is.null(cloudFolderID)
    if (isNullCFI) {
      if (is.null(cacheRepo)) {
        cacheRepo <- .checkCacheRepo(cacheRepo, verbose = verbose)
      }
      cloudFolderID <- cloudFolderFromCacheRepo(cacheRepo)
    }
    isID <- is(cloudFolderID, "drive_id")

    if (!isID) {
      isStringForID <- isTRUE(32 <= nchar(cloudFolderID) && nchar(cloudFolderID) <= 33)
      if (isStringForID) cloudFolderID <- googledrive::as_id(cloudFolderID)
    }

    driveLs <-
      if (packageVersion("googledrive") < "2.0.0") {
        tryCatch(googledrive::drive_get(cloudFolderID, team_drive = team_drive),
                 error = function(x) {character()})
      } else {
        tryCatch(googledrive::drive_get(cloudFolderID, shared_drive = team_drive),
                 error = function(x) {character()})
      }

    if (NROW(driveLs) == 0) {
      if (isTRUE(create)) {
        if (isID) {
          if (is.null(cacheRepo)) {
            cacheRepo <- .checkCacheRepo(cacheRepo, verbose = verbose)
          }
          cloudFolderID <- cloudFolderFromCacheRepo(cacheRepo)
        }
        cloudFolderID <- googledrive::drive_mkdir(as.character(cloudFolderID),
                                                  path = NULL, overwrite = overwrite)
      } else {
        messageCache("There is no cloudFolderID and create = FALSE; returning NULL", verbose = verbose)
      }
    } else {
      cloudFolderID <- driveLs
    }
    options('reproducible.cloudFolderID' = cloudFolderID)
  }
  return(cloudFolderID)
}

cloudDriveLs <- function(cloudFolderID = NULL, pattern = NULL,
                    verbose = getOption("reproducible.verbose", 1),
                    team_drive = NULL) {
  if (!requireNamespace("googledrive", quietly = TRUE))
    stop(requireNamespaceMsg("googledrive", "to use google drive files"))

  if (!is(cloudFolderID, "tbl")) {
    cloudFolderID <- cloudFolderID(cloudFolderID = cloudFolderID, create = FALSE,
                                               team_drive = team_drive) # only deals with NULL case
  }

  messageCache("Retrieving file list in cloud folder", verbose = verbose)
  suppressMessages(gdriveLs <- retry(quote({
    googledrive::drive_ls(path = cloudFolderID, ## TODO: team drives needs a dribble
                          pattern = paste0(collapse = "|", c(cloudFolderID$id ,pattern)))
  })))
  if (is(gdriveLs, "try-error")) {
    fnf <- grepl("File not found", gdriveLs)
    if (!fnf) {
      gdriveLs <- retry(quote({
        googledrive::drive_ls(path = googledrive::as_id(cloudFolderID), ## TODO: team drives needs a dribble
                              pattern = paste0(cloudFolderID, "|",pattern))
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
#' @inheritParams cloudUpload
#' @inheritParams Cache
#' @rdname cloudCache
#' @export
cloudDownload <- function(outputHash, gdriveLs, cacheRepo, cloudFolderID,
                          drv = getOption("reproducible.drv"),
                          conn = getOption("reproducible.conn", NULL)) {

  newFileName <- CacheStoredFile(cacheRepo, outputHash) # paste0(outputHash,".rda")
  isInCloud <- gsub(gdriveLs$name,
                    pattern = paste0("\\.", fileExt(newFileName)),
                    replacement = "") %in% outputHash
  output <- NULL
  if (any(isInCloud)) {

    if (!requireNamespace("googledrive", quietly = TRUE))
      stop(requireNamespaceMsg("googledrive", "to use google drive files"))

    messageCache("Downloading cloud copy of ", newFileName,", with cacheId: ", outputHash)
    localNewFilename <- file.path(tempdir2(), basename2(newFileName))
    du <- retry(quote(
      googledrive::drive_download(file = googledrive::as_id(gdriveLs$id[isInCloud][1]),
                                  path = localNewFilename, # take first if there are duplicates
                                  overwrite = TRUE)))

    cloudAddTagsRepo(du, outputHash, cacheRepo, drv, conn)
    output <- loadFile(localNewFilename)
    output <- cloudDownloadRasterBackend(output, cacheRepo, cloudFolderID, outputHash = outputHash, drv = drv)
    output <- dealWithClassOnRecovery(output, cacheRepo = cacheRepo,
                                      cacheId = outputHash,
                                      drv = drv, conn = conn)

  }
  output
}


#' Upload a file to cloud directly from local \code{cacheRepo}
#'
#' Meant for internal use, as there are internal objects as arguments.
#'
#' @param isInCloud     A logical indicating whether an \code{outputHash} is in the cloud already.
#' @param outputToSave  Only required if \code{any(rasters) == TRUE}.
#'                      This is the \code{Raster*} object.
#' @inheritParams cloudUpload
#'
#' @keywords internal
#'
cloudUpload <- function(isInCloud, outputHash, cacheRepo, cloudFolderID,
                                 outputToSave, gdriveLs, drv = getOption("reproducible.drv"),
                                 conn = getOption("reproducible.conn", NULL)) {
  if (!requireNamespace("googledrive", quietly = TRUE))
    stop(requireNamespaceMsg("googledrive", "to use google drive files"))

  cacheIdFileName <- CacheStoredFile(cacheRepo, outputHash)
  isInCloud <- gsub(gdriveLs$name,
                    pattern = paste0("\\.", fileExt(cacheIdFileName)),
                    replacement = "") %in% outputHash
  #browser(expr = exists("._cloudUploadFromCache_1"))
  du <- NULL
  if (!any(isInCloud)) {
    newFileName <- basename2(cacheIdFileName)

    cloudFolderID <- cloudFolderID(cloudFolderID = cloudFolderID, create = TRUE)
    messageCache("Uploading object ", newFileName,", with cacheId: ",
            outputHash," to cloud folder id: ", cloudFolderID$name, " or ", cloudFolderID$id)
    du <- try(retry(quote(googledrive::drive_upload(media = cacheIdFileName,
                                       path = googledrive::as_id(cloudFolderID), name = newFileName,
                                       overwrite = FALSE))))
    if (is(du, "try-error")) {
      return(du)
    }
    cu <- cloudUploadRasterBackends(obj = outputToSave, cloudFolderID)
    if (NROW(cu)) {
      du <- rbind(du, cu)
    }

    cloudAddTagsRepo(drib = du, outputHash, cacheRepo, drv = drv, conn = conn)

    # Now update the db file in cloud
    suppressMessages(sc <- showCache(userTags = outputHash, x = cacheRepo, drv = drv,
                                     conn = conn))
    cloudAddToDBFile(sc, cloudFolderID, gdriveLs = NULL, drv, conn)


    if (FALSE) {
      gdriveLs <- googledrive::drive_ls(cloudFolderID)
      suppressMessages(csc <- cloudShowCache(cloudFolderID, drv = drv, gdriveLs = gdriveLs))
      toKeep <- sc[!cacheId %in% unique(toRm$cacheId)]

      dbFile <- cloudDBFile(cloudFolderID, gdriveLs = NULL, drv, conn)

      localDB <- cacheDFToTmpDB(drv = drv, df = toKeep, cloudFolderID = cloudFolderID,
                                gdriveLs = NULL)
      cloudUploadOrUpdateDBFile(cloudFolderID, dbFile, localDB)


      cloud
      cloudAddToDBFile(sc, cloudFolderID, gdriveLs = NULL, drv, conn)
    }
  }

  return(du)
}

cloudUploadRasterBackends <- function(obj, cloudFolderID) {
  if (!requireNamespace("googledrive", quietly = TRUE))
    stop(requireNamespaceMsg("googledrive", "to use google drive files"))

  #browser(expr = exists("._cloudUploadRasterBackends_1"))
  rasterFilename <- Filenames(obj)
  out <- NULL
  if (!is.null(unlist(rasterFilename)) && length(rasterFilename) > 0 && all(nchar(rasterFilename) > 0)) {
    allRelevantFiles <- unique(rasterFilename)
    out <- lapply(allRelevantFiles, function(file) {
      try(retry(quote(googledrive::drive_upload(media = file,  path = cloudFolderID,
                                                name = basename(file), overwrite = FALSE))))
    })
    out <- do.call(rbind, out)
  }
  return(invisible(out))
}

cloudDownloadRasterBackend <- function(output, cacheRepo, cloudFolderID, outputHash,
                                       drv = getOption("reproducible.drv"),
                                       conn = getOption("reproducible.conn", NULL)) {
  if (!requireNamespace("googledrive", quietly = TRUE))
    stop(requireNamespaceMsg("googledrive", "to use google drive files"))

  rasterFilename <- Filenames(output)
  if (!is.null(unlist(rasterFilename)) && length(rasterFilename) > 0 && all(nchar(rasterFilename) > 0)) {
    gdriveLs2 <- NULL
    cacheRepoRasterDir <- file.path(cacheRepo, "rasters")
    checkPath(cacheRepoRasterDir, create = TRUE)
    simpleFilenames <- unique(filePathSansExt(basename2(unlist(rasterFilename))))
    retry(quote({
      gdriveLs2 <- googledrive::drive_ls(
        path = googledrive::as_id(cloudFolderID),
        pattern = paste(collapse = "|", simpleFilenames))
    }))

    if (all(simpleFilenames %in% filePathSansExt(gdriveLs2$name))) {
      filenameMismatches <- unlist(lapply(seq_len(NROW(gdriveLs2)), function(idRowNum) {
        localNewFilename <- file.path(cacheRepoRasterDir, basename2(gdriveLs2$name[idRowNum]))
        filenameMismatch <- identical(localNewFilename, rasterFilename)
        du <- retry(quote(googledrive::drive_download(file = gdriveLs2[idRowNum,],
                                   path = localNewFilename, # take first if there are duplicates
                                   overwrite = TRUE)))
        cloudAddTagsRepo(drib = du, outputHash, cacheRepo, drv = drv, conn = conn)
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
      warning("Raster backed files are not available in googledrive; \n",
              "will proceed with rerunning code because cloud copy is incomplete")
      output <- NULL
    }
  }
  output
}

isOrHasRaster <- function(obj) {
  rasters <- if (is(obj, "environment")) {
    if (identical(class(obj), "environment")) {
      lapply(mget(ls(obj), envir = obj), function(x) isOrHasRaster(x))
    } else {
      tryCatch(lapply(mget(ls(obj), envir = obj@.xData),
                             function(x) isOrHasRaster(x)), error = function(x) FALSE)
    }
  } else if (is.list(obj)) {
    lapply(obj, function(x) isOrHasRaster(x))
  } else {
    is(obj, "Raster")
  }
  return(rasters)
}


cloudAddTagsRepo <- function(drib, outputHash, cacheRepo, drv, conn) {
  .updateTagsRepo(outputHash, cacheRepo, "inCloud", "TRUE", drv = drv, conn = conn)
  lapply(drib$name, function(nam) .addTagsRepo(outputHash, cacheRepo, "inCloudFile",
                                             nam, drv = drv, conn = conn))
  ids <- if (is(drib$id, "drive_id")) as.character(drib$id) else drib$id
  lapply(ids, function(id) .addTagsRepo(outputHash, cacheRepo, "inCloudID",
                                          id, drv = drv, conn = conn))

}


cloudRemove <- function(cloudFolderID, x, cacheIds, cacheDT) {
  if (is.null(cloudFolderID)) {
    cloudFolderID <- cloudFolderID(cloudFolderID, cacheRepo = x)
  }
  cloudIDs <- cacheDT[cacheId %in% cacheIds & tagKey %in% c("inCloudFile", "inCloudID")]
  du <- googledrive::as_dribble(googledrive::as_id(cloudIDs$tagValue[cloudIDs$tagKey == "inCloudID"]))
  try(googledrive::drive_trash(du))
}


#' Clear the cloudFolderID
#'
#' This is a convenience function that removes all the files in \code{cloudFolderID}.
#' To fully remove the entire \code{cloudFolderID} directory, use
#' \code{googledrive::drive_trash(cloudFolderID)}.
#'
#' @inheritParams Cache
#' @export
#' @rdname cloudCache
cloudClearCache <- function(cloudFolderID = NULL,
                            userTags = character(), after = NULL, before = NULL,
                            drv = getOption("reproducible.drv"),
                            conn = getOption("reproducible.conn", NULL)) {
  cloudFolderID <- cloudFolderID(cloudFolderID)
  toKeep <- NULL
  if (!is.null(cloudFolderID)) {
    gdriveLs <- googledrive::drive_ls(cloudFolderID)
    sc <- cloudShowCache(cloudFolderID = cloudFolderID, drv = drv, gdriveLs = gdriveLs)
    toKeep <- sc
    if (NROW(sc)) {
      after <- toNA(after)
      afterNA <- is.na(after)
      before <- toNA(before)
      beforeNA <- is.na(before)
      sc <- afterBefore(sc, afterNA, after, beforeNA, before)

      cacheIdToRm <- "all"
      if (length(userTags))
        cacheIdToRm <- sc[cacheId %in% userTags | tagKey %in% userTags | tagValue %in% userTags]

      if (NROW(cacheIdToRm)) {
        toRm <- if (identical(cacheIdToRm, "all"))
          sc[tagKey == "inCloudID"]
        else
          sc[tagKey == "inCloudID" & cacheId %in% cacheIdToRm]
        gdriveLs2 <- googledrive::as_dribble(googledrive::as_id(toRm$tagValue))
        googledrive::drive_trash(gdriveLs2)
        toKeep <- sc[!cacheId %in% unique(toRm$cacheId)]

        dbFile <- cloudDBFile(cloudFolderID, gdriveLs, drv, conn)

        localDB <- cacheDFToTmpDB(drv = drv, df = toKeep, cloudFolderID = cloudFolderID,
                                  gdriveLs = NULL)
        cloudUploadOrUpdateDBFile(cloudFolderID, dbFile, localDB)

      }
    }


  } else {
    message("No cloudFolderID supplied; nothing to do")
  }
  toKeep
}

#' @rdname cloudCache
#' @inheritParams Cache
#' @export
#'
#' @details
#' \code{cloudCacheBrowse} will open up a browser at the current cloudFolderID.
#'
cloudCacheBrowse <- function(cloudFolderID = NULL) {
  cloudFolderID <- cloudFolderID(cloudFolderID)
  if (!is.null(cloudFolderID)) {
    cfid <- if (is(cloudFolderID, "dribble")) {
      cloudFolderID$id
    } else if (is(cloudFolderID, "drive_id")) {
      cloudFolderID
    } else if (is(cloudFolderID, "character")) {
      googledrive::as_id(cloudFolderID)
    } else {
      stop("cloudFolderID must be a dribble, drive_id or character string")
    }

    browseURL(file.path("https://drive.google.com/drive/folders/", cfid))
  }
}

#' @rdname cloudCache
#' @export
#' @inheritParams Cache
#' @include cache.R
#'
#' \code{cloudFolderFromCacheRepo} will return a path using the innermost 2 directories
#' of the \code{cacheRepo} supplied, separated with a \code{"_"}
cloudFolderFromCacheRepo <- function(cacheRepo) {
  paste0("cloudCache_", basename2(dirname(cacheRepo)), "_", basename2(cacheRepo))
}


cloudAddToDBFile <- function(sc, cloudFolderID, gdriveLs = NULL, drv, conn) {

  connTmp <- suppressMessages(cloudConnectDB(cloudFolderID, gdriveLs, drv))

  on.exit({
    suppressWarnings(dbDisconnectAll(connTmp, shutdown = TRUE))
  }, add = TRUE)
  dbTabNam <- if (useSQL(connTmp)) DBI::dbListTables(connTmp) else NULL
  dbFile <- cloudDBFile(cloudFolderID, gdriveLs, drv, connTmp)

  dfToDB(sc, drv, connTmp, dbTabNam)
  cloudDisconnectDB(connTmp, drv, dbFile, cloudFolderID)

}

cloudRmFromDBFile <- function(objsDT, cloudFolderID, cacheIds, gdriveLs, drv, conn) {
  dbFile <- cloudDBFile(cloudFolderID, gdriveLs, drv, conn)
  connTmp <- suppressMessages(cloudConnectDB(cloudFolderID, gdriveLs, drv))
  dbTabNam <- if (useSQL(connTmp)) DBI::dbListTables(connTmp) else NULL
  cp <- cacheRepoFromConn(connTmp)
  rmFromCache(cachePath = cp, conn = connTmp, drv = drv, cacheId = cacheIds,
              dbTabNam = dbTabNam)
  cloudDisconnectDB(connTmp, #cp,
                                     drv, dbFile, cloudFolderID)

}


cloudConnectDB <- function(cloudFolderID, gdriveLs, drv, dbFile = NULL) {

  if (is.null(dbFile))
    dbFile <- cloudDBFile(cloudFolderID, gdriveLs, drv, conn)

  cp <- .reproducibleTempPath(sub = rndstr(1, 6))
  if (NROW(dbFile)) {
    name <- cloudFolderID$name
    dld <- googledrive::drive_download(
      dbFile, file.path(cp, basename2(CacheDBFile(name, drv = drv))))
  }
  connTmp <- dbConnectAll(drv, cachePath = cp)
  if (NROW(dbFile) == 0) {
    createEmptyTable(connTmp, cp, drv)
  }

  if (is(drv, "SQLiteDriver")) {
    if (!requireNamespace("RSQLite")) stop("To use RSQLite::SQLite driver, please install.packages('RSQLite')")
    RSQLite::dbClearResult(RSQLite::dbSendQuery(connTmp, "PRAGMA busy_timeout=5000;"))
    RSQLite::dbClearResult(RSQLite::dbSendQuery(connTmp, "PRAGMA journal_mode=WAL;"))
  }

  return(connTmp)
}

cloudDisconnectDB <- function(conn, drv, dbFile, cloudFolderID) {
  localDB <- dbFileFromConn(conn)
  # localDB <- CacheDBFile(cacheRepo, drv, conn)
  dbDisconnectAll(conn, shutdown = TRUE)
  cloudUploadOrUpdateDBFile(cloudFolderID, dbFile, localDB)
  # if (NROW(dbFile) == 0) {
  #   googledrive::drive_upload(localDB, path = cloudFolderID)
  # } else {
  #   googledrive::drive_update(dbFile, localDB)
  # }
}

dbFileFromConn <- function(conn) {
  cp <- if (useSQL(conn)) {
    conn@dbname
  } else {
    conn
  }
  cp
}

cacheRepoFromConn <- function(conn) {
  cp <- dbFileFromConn(conn)
  cp <- dirname(cp)
}

cloudDBFile <- function(cloudFolderID, gdriveLs = NULL, drv, conn) {
  if (is.null(gdriveLs))
    suppressMessages(gdriveLs <- googledrive::drive_ls(cloudFolderID))
  name <- cloudFolderID$name
  dbFile <- basename2(CacheDBFile(name, drv = drv))
  gdriveLs[gdriveLs$name == dbFile, ]
}

#' @export
#' @inheritParams Cache
#' @rdname cloudCache
#' @details
#' \code{cloudShowCache} is the equivalent to \code{\link{showCache}}, but only
#' for a \code{cloudFolderID}. This works by identifying the database file in the
#' \code{cloudFolderID}, downloading that file to a local, temporary file location,
#' then reading from it. It will remove the temporary file on completion. This
#' \code{showCloudCache} will have no relationship to \code{showCache}; if the
#' user would like to see what objects in the current local \code{cacheRepo} are
#' in the cloud, use \code{showCache(cacheRepo)},
#' which will have a \code{tagKey} of `inCloud` and a \code{tagValue} `TRUE`.
cloudShowCache <- function(cloudFolderID = getOption("reproducible.cloudFolderID"),
                           gdriveLs = NULL,
                           userTags = character(), after = NULL, before = NULL,
                           drv = getOption("reproducible.drv"),
                           conn = getOption("reproducible.conn", NULL)) {
  cloudFolderID <- cloudFolderID(cloudFolderID)
  if (is.null(gdriveLs)) {
    # Note :: adding pattern = basename(drv = drv) was actually slower
    suppressMessages(gdriveLs <- googledrive::drive_ls(cloudFolderID))
  }
  connTmp <- suppressMessages(cloudConnectDB(cloudFolderID, gdriveLs, drv))
  dbFilename <- dbFileFromConn(connTmp)
  cp <- dirname(dbFilename)
  on.exit({
    dbDisconnectAll(connTmp, shutdown = TRUE)
    file.remove(dbFilename)
  }
  )
  dbTabNam <- if (useSQL(connTmp)) DBI::dbListTables(connTmp) else NULL
  sc <- showCache(x = cp, dbTabNam = dbTabNam, userTags = userTags,
                  drv = drv, conn = connTmp, allowNoStorageFolder = TRUE,
                  allowTBMismatch = TRUE,
                  verbose = 0)
  sc[]

}


cloudUploadOrUpdateDBFile <- function(cloudFolderID, dbFile, localDB) {
  if (NROW(dbFile) == 0) {
    googledrive::drive_upload(localDB, path = cloudFolderID)
  } else {
    googledrive::drive_update(dbFile, localDB)
  }
}


cacheDFToTmpDB <- function(drv, df, cloudFolderID, gdriveLs) {
  connTmp <- cloudConnectDB(cloudFolderID, gdriveLs, drv, dbFile = numeric())
  on.exit({
    dbDisconnectAll(connTmp, shutdown = TRUE)
  }
  )
  dbTabNam <- if (useSQL(connTmp)) DBI::dbListTables(connTmp) else NULL
  # dbFilename <- connTmp@dbname
  dfToDB(df, drv, connTmp, dbTabNam)
  localDB <- normPath(dbFileFromConn(connTmp))
  return(localDB)
}

dfToDB <- function(df, drv, conn, tbNam) {
  # cp <- if (useSQL(conn)) {
  #   conn@dbname
  # } else {
  #   conn
  # }
  # cp <- dirname(cp)

  cp <- cacheRepoFromConn(conn)
  lapply(seq(NROW(df)), function(x) {
    scInner <- df[x, ]
    .addTagsRepo(cacheId = scInner$cacheId, tagKey = scInner$tagKey,
                 tagValue = scInner$tagValue, drv = drv, conn = conn, cachePath = cp,
                 tbNam = tbNam)
  }
  )
}
