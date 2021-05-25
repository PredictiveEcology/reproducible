if (getRversion() >= "3.1.0") {
  utils::globalVariables(c("cacheId", "checksumsFilename", "checksumsID", "id"))
}

#' Check for presence of \code{checkFolderID} (for \code{Cache(useCloud)})
#'
#' Will check for presence of a \code{cloudFolderID} and make a new one
#' if one not present on Google Drive, with a warning.
#'
#' @inheritParams Cache
#' @param cloudFolderID The google folder ID where cloud caching will occur.
#' @param create Logical. If \code{TRUE}, then the \code{cloudFolderID} will be created.
#'     This should be used with caution as there are no checks for overwriting.
#'     See \code{googledrive::drive_mkdir}. Default \code{FALSE}.
#' @param overwrite Logical. Passed to \code{googledrive::drive_mkdir}.
#' @param team_drive Logical indicating whether to check team drives.
#'
#' @export
checkAndMakeCloudFolderID <- function(cloudFolderID = getOption('reproducible.cloudFolderID', NULL),
                                      cacheRepo = NULL,
                                      create = FALSE,
                                      overwrite = FALSE,
                                      verbose = getOption("reproducible.verbose", 1),
                                      team_drive = NULL) {
  if (!requireNamespace("googledrive", quietly = TRUE))
    stop(requireNamespaceMsg("googledrive", "to use google drive files"))

  browser(expr = exists("._checkAndMakeCloudFolderID_1"))
  if (!is(cloudFolderID, "dribble")) {
    isNullCFI <- is.null(cloudFolderID)
    if (isNullCFI) {
      if (is.null(cacheRepo)) {
        cacheRepo <- .checkCacheRepo(cacheRepo, verbose = verbose)
      }
      cloudFolderID <- cloudFolderFromCacheRepo(cacheRepo)
    }
    isID <- isTRUE(32 <= nchar(cloudFolderID) && nchar(cloudFolderID) <= 33)
    driveLs <- if (isID) {
      tryCatch(googledrive::drive_get(googledrive::as_id(cloudFolderID), team_drive = team_drive),
               error = function(x) {character()})
    } else {
      tryCatch(googledrive::drive_get(cloudFolderID, team_drive = team_drive),
               error = function(x) { character() })
    }

    if (NROW(driveLs) == 0) {
      if (isTRUE(create)) {
        if (isID) {
          if (is.null(cacheRepo)) {
            cacheRepo <- .checkCacheRepo(cacheRepo, verbose = verbose)
          }
          cloudFolderID <- cloudFolderFromCacheRepo(cacheRepo)
        }
        newDir <- googledrive::drive_mkdir(cloudFolderID, path = NULL, overwrite = overwrite)
        cloudFolderID <- newDir
      }
    } else {
      cloudFolderID <- driveLs
    }
    if (isNullCFI) {
      messageCache("Setting 'reproducible.cloudFolderID' option to be cloudFolder: ",
                   ifelse(!is.null(names(cloudFolderID)), cloudFolderID$name, cloudFolderID),
                   verbose = verbose)
    }
    options('reproducible.cloudFolderID' = cloudFolderID)
  }
  return(cloudFolderID)
}

driveLs <- function(cloudFolderID = NULL, pattern = NULL,
                    verbose = getOption("reproducible.verbose", 1),
                    team_drive = NULL) {
  if (!requireNamespace("googledrive", quietly = TRUE))
    stop(requireNamespaceMsg("googledrive", "to use google drive files"))
  #browser(expr = exists("kkkk"))

  if (!is(cloudFolderID, "tbl"))
    cloudFolderID <- checkAndMakeCloudFolderID(cloudFolderID = cloudFolderID, create = FALSE,
                                               team_drive = team_drive) # only deals with NULL case

  messageCache("Retrieving file list in cloud folder", verbose = verbose)
  gdriveLs <- retry(quote({
    googledrive::drive_ls(path = cloudFolderID, ## TODO: team drives needs a dribble
                          pattern = paste0(collapse = "|", c(cloudFolderID$id ,pattern)))
  }))
  if (is(gdriveLs, "try-error")) {
    fnf <- grepl("File not found", gdriveLs)
    if (!fnf) {
      gdriveLs <- retry(quote({
        googledrive::drive_ls(path = googledrive::as_id(cloudFolderID), ## TODO: team drives needs a dribble
                              pattern = paste0(cloudFolderID, "|",pattern))
      }))
      #cloudFolderID <- checkAndMakeCloudFolderID(cloudFolderID, create = TRUE)
      #gdriveLs <- try(googledrive::drive_ls(path = googledrive::as_id(cloudFolderID), pattern = paste0(cloudFolderID, "|",pattern)))
    } else {
      stop("cloudFolderID not found on Gdrive\n", gdriveLs)
    }
  }
  gdriveLs
}

#' Upload to cloud, if necessary
#'
#' Meant for internal use, as there are internal objects as arguments.
#'
#' @param isInRepo A data.table with the information about an object that is in the local cacheRepo
#' @param outputHash The \code{cacheId} of the object to upload
#' @param gdriveLs The result of \code{googledrive::drive_ls(googledrive::as_id(cloudFolderID), pattern = "outputHash")}
#' @param output The output object of FUN that was run in \code{Cache}
#' @inheritParams Cache
cloudUpload <- function(isInRepo, outputHash, gdriveLs, cacheRepo, cloudFolderID, output) {
  if (!requireNamespace("googledrive", quietly = TRUE))
      stop(requireNamespaceMsg("googledrive", "to use google drive files"))

  artifact <- isInRepo[[.cacheTableHashColName()]][1]
  # browser(expr = exists("._cloudUpload_1"))
  artifactFileName <- CacheStoredFile(cacheRepo, hash = artifact)
  #artifactFileName <- paste0(artifact, ".rda")
  if (useDBI()) {
    newFileName <- basename2(artifactFileName)
  } else {
    newFileName <- paste0(outputHash,".rda")
  }
  isInCloud <- gsub(gdriveLs$name,
                    pattern = paste0("\\.", fileExt(CacheStoredFile(cacheRepo, outputHash))),
                    replacement = "") %in% outputHash

  if (!any(isInCloud)) {
    messageCache("Uploading local copy of ", artifactFileName,", with cacheId: ",
                 outputHash," to cloud folder")
    numRetries <- 1
    while (numRetries < 6) {
      du <- try(retry(retries = numRetries,
                      quote(googledrive::drive_upload(media = artifactFileName, path = cloudFolderID,
                                         name = newFileName, overwrite = FALSE))))
      if (is(du, "try-error")) {
        if (!isTRUE(any(grepl("overwrite", du)))) {
          numRetries <- numRetries + 4
        } else {
          return(du)
        }
      } else {
        numRetries <- 6
      }
    }
    cloudUploadRasterBackends(obj = output, cloudFolderID)
  }
}

#' Download from cloud, if necessary
#'
#' Meant for internal use, as there are internal objects as arguments.
#'
#' @param newFileName The character string of the local filename that the downloaded object will have
#' @inheritParams cloudUpload
#' @inheritParams Cache
cloudDownload <- function(outputHash, newFileName, gdriveLs, cacheRepo, cloudFolderID,
                          drv = getOption("reproducible.drv", RSQLite::SQLite()),
                          conn = getOption("reproducible.conn", NULL)) {
  if (!requireNamespace("googledrive", quietly = TRUE))
    stop(requireNamespaceMsg("googledrive", "to use google drive files"))

  #browser(expr = exists("._cloudDownload_1"))
  messageCache("Downloading cloud copy of ", newFileName,", with cacheId: ", outputHash)
  localNewFilename <- file.path(tempdir2(), basename2(newFileName))
  isInCloud <- gsub(gdriveLs$name,
                    pattern = paste0("\\.", fileExt(CacheStoredFile(cacheRepo, outputHash))),
                    replacement = "") %in% outputHash

  retry(quote(googledrive::drive_download(file = googledrive::as_id(gdriveLs$id[isInCloud][1]),
                             path = localNewFilename, # take first if there are duplicates
                             overwrite = TRUE)))
  if (useDBI()) {
    output <- loadFile(localNewFilename)
  } else {
    ee <- new.env(parent = emptyenv())
    loadedObjName <- load(localNewFilename)
    output <- get(loadedObjName, inherits = FALSE)
  }
  output <- cloudDownloadRasterBackend(output, cacheRepo, cloudFolderID, drv = drv)
  output
}

#' Upload a file to cloud directly from local \code{cacheRepo}
#'
#' Meant for internal use, as there are internal objects as arguments.
#'
#' @param isInCloud     A logical indicating whether an \code{outputHash} is in the cloud already.
#' @param outputToSave  Only required if \code{any(rasters) == TRUE}.
#'                      This is the \code{Raster*} object.
#' @param rasters       A logical vector of length >= 1 indicating which elements in
#'                      \code{outputToSave} are \code{Raster*} objects.
#' @inheritParams cloudUpload
#'
#' @keywords internal
cloudUploadFromCache <- function(isInCloud, outputHash, cacheRepo, cloudFolderID,
                                 outputToSave, rasters) {
  if (!requireNamespace("googledrive", quietly = TRUE))
    stop(requireNamespaceMsg("googledrive", "to use google drive files"))

  #browser(expr = exists("._cloudUploadFromCache_1"))
  if (!any(isInCloud)) {
    cacheIdFileName <- CacheStoredFile(cacheRepo, outputHash)
    newFileName <- if (useDBI()) {
      basename2(cacheIdFileName)
    }
    cloudFolderID <- checkAndMakeCloudFolderID(cloudFolderID = cloudFolderID, create = TRUE)
    messageCache("Uploading new cached object ", newFileName,", with cacheId: ",
            outputHash," to cloud folder id: ", cloudFolderID$name, " or ", cloudFolderID$id)
    du <- try(retry(quote(googledrive::drive_upload(media = CacheStoredFile(cacheRepo, outputHash),
                                       path = googledrive::as_id(cloudFolderID), name = newFileName,
                                       overwrite = FALSE))))
    if (is(du, "try-error")) {
      return(du)
    }
  }
  cloudUploadRasterBackends(obj = outputToSave, cloudFolderID)
}

cloudUploadRasterBackends <- function(obj, cloudFolderID) {
  if (!requireNamespace("googledrive", quietly = TRUE))
    stop(requireNamespaceMsg("googledrive", "to use google drive files"))

  #browser(expr = exists("._cloudUploadRasterBackends_1"))
  rasterFilename <- Filenames(obj)
  out <- NULL
  if (!is.null(unlist(rasterFilename)) && length(rasterFilename) > 0) {
    allRelevantFiles <- unique(rasterFilename)
    out <- lapply(allRelevantFiles, function(file) {
      try(retry(quote(googledrive::drive_upload(media = file,  path = cloudFolderID,
                                                name = basename(file), overwrite = FALSE))))
    })
  }
  return(invisible(out))
}

cloudDownloadRasterBackend <- function(output, cacheRepo, cloudFolderID,
                                       drv = getOption("reproducible.drv", RSQLite::SQLite()),
                                       conn = getOption("reproducible.conn", NULL)) {
  if (!requireNamespace("googledrive", quietly = TRUE))
    stop(requireNamespaceMsg("googledrive", "to use google drive files"))

  #browser(expr = exists("._cloudDownloadRasterBackend_1"))
  rasterFilename <- Filenames(output)
  if (!is.null(unlist(rasterFilename)) && length(rasterFilename) > 0) {
    gdriveLs2 <- NULL
    cacheRepoRasterDir <- file.path(cacheRepo, "rasters")
    checkPath(cacheRepoRasterDir, create = TRUE)
    simpleFilenames <- unique(filePathSansExt(basename2(unlist(rasterFilename))))
    retry(quote({
      gdriveLs2 <- googledrive::drive_ls(path = as_id(cloudFolderID),
                            pattern = paste(collapse = "|", simpleFilenames))
    }))

    if (all(simpleFilenames %in% filePathSansExt(gdriveLs2$name))) {
      filenameMismatches <- unlist(lapply(seq_len(NROW(gdriveLs2)), function(idRowNum) {
        localNewFilename <- file.path(cacheRepoRasterDir, basename2(gdriveLs2$name[idRowNum]))
        filenameMismatch <- identical(localNewFilename, rasterFilename)
        retry(quote(googledrive::drive_download(file = gdriveLs2[idRowNum,],
                                   path = localNewFilename, # take first if there are duplicates
                                   overwrite = TRUE)))
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

#' @importFrom rlang inherits_only
isOrHasRaster <- function(obj) {
  rasters <- if (is(obj, "environment")) {
    if (inherits_only(obj, "environment")) {
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
