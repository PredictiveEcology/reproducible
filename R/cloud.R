if (getRversion() >= "3.1.0") {
  utils::globalVariables(c("cacheId", "checksumsFilename", "checksumsID", "id"))
}

#' Check for presence of \code{checkFolderID} (for \code{Cache(useCloud)})
#'
#' Will check for presence of a \code{cloudFolderID} and make a new one
#' if one not present on googledrive, with a warning.
#'
#' @param cloudFolderID The google folder ID where cloud caching will occur.
#' @export
#' @importFrom googledrive drive_mkdir
checkAndMakeCloudFolderID <- function(cloudFolderID = NULL) {
  if (is.null(cloudFolderID)) {
    newDir <- retry({drive_mkdir("testFolder")})
    cloudFolderID = newDir$id
    warning("No cloudFolderID supplied; if this is the first time using 'useCloud',",
            " this cloudFolderID, ", cloudFolderID,
            " should likely be kept and used in all subsequent calls to Cache using 'useCloud = TRUE'.", #nolint
            " Making a new cloud folder and setting options('reproducible.cloudFolderID' = ",
            cloudFolderID, ")")
    options('reproducible.cloudFolderID' = cloudFolderID)
  }
  return(cloudFolderID)
}

#' Upload to cloud, if necessary
#'
#' Meant for internal use, as there are internal objects as arguments.
#'
#' @param isInRepo A data.table with the information about an object that is in the local cacheRepo
#' @param outputHash The \code{cacheId} of the object to upload
#' @param gdriveLs The result of \code{googledrive::drive_ls(as_id(cloudFolderID), pattern = "outputHash")}
#' @param output The output object of FUN that was run in \code{Cache}
#' @importFrom googledrive drive_upload
#' @inheritParams Cache
cloudUpload <- function(isInRepo, outputHash, gdriveLs, cacheRepo, cloudFolderID, output) {
  artifact <- isInRepo[[.cacheTableHashColName()]][1]
  artifactFileName <- paste0(artifact, ".rda")
  newFileName <- paste0(outputHash,".rda")
  isInCloud <- gsub(gdriveLs$name, pattern = "\\.rda", replacement = "") %in% outputHash

  if (!any(isInCloud)) {
    message("Uploading local copy of ", artifactFileName,", with cacheId: ",
            outputHash," to cloud folder")
    retry(drive_upload(media = file.path(CacheStorageDir(cacheRepo), artifactFileName),
                 path = as_id(cloudFolderID), name = newFileName))

    cloudUploadRasterBackends(obj = output, cloudFolderID)
  }
}

#' Download from cloud, if necessary
#'
#' Meant for internal use, as there are internal objects as arguments.
#'
#' @param newFileName The character string of the local filename that the downloaded object will have
#' @inheritParams cloudUpload
#' @importFrom googledrive drive_download
#' @inheritParams Cache
cloudDownload <- function(outputHash, newFileName, gdriveLs, cacheRepo, cloudFolderID,
                          drv = RSQLite::SQLite(), conn = NULL) {
  message("Downloading cloud copy of ", newFileName,", with cacheId: ",
          outputHash)
  localNewFilename <- file.path(tempdir(), newFileName)
  isInCloud <- gsub(gdriveLs$name, pattern = "\\.rda", replacement = "") %in% outputHash

  retry(drive_download(file = as_id(gdriveLs$id[isInCloud][1]),
                 path = localNewFilename, # take first if there are duplicates
                 overwrite = TRUE))
  ee <- new.env(parent = emptyenv())
  loadedObjName <- load(localNewFilename)
  output <- get(loadedObjName, inherits = FALSE)
  output <- cloudDownloadRasterBackend(output, cacheRepo, cloudFolderID, drv = drv)
  output
}

#' Upload a file to cloud directly from local \code{cacheRepo}
#'
#' Meant for internal use, as there are internal objects as arguments.
#'
#' @param isInCloud A logical indicating whether an \code{outputHash} is in the cloud already.
#' @param saved The character string of the saved file's archivist digest value.
#' @param outputToSave Only required if \code{any(rasters) == TRUE}. This is the \code{Raster*} object.
#' @param rasters A logical vector of length >= 1 indicating which elements in \code{outputToSave}
#'                are \code{Raster*} objects.
#' @inheritParams cloudUpload
#'
#' @importFrom googledrive drive_download
#' @keywords internal
cloudUploadFromCache <- function(isInCloud, outputHash, saved, cacheRepo, cloudFolderID,
                                 outputToSave, rasters) {
  if (!any(isInCloud)) {
    cacheIdFileName <- paste0(outputHash,".rda")
    newFileName <- paste0(saved, ".rda")
    message("Uploading new cached object ", newFileName,", with cacheId: ",
            outputHash," to cloud folder")
    retry(drive_upload(media = file.path(CacheStorageDir(cacheRepo), newFileName),
                       path = as_id(cloudFolderID), name = cacheIdFileName))
  }
  cloudUploadRasterBackends(obj = outputToSave, cloudFolderID)
}

cloudUploadRasterBackends <- function(obj, cloudFolderID) {
  rasterFilename <- Filenames(obj)
  if (!is.null(rasterFilename) && length(rasterFilename) > 0) {
    allRelevantFiles <- sapply(rasterFilename, function(file) {
      unique(dir(dirname(file), pattern = paste(collapse = "|",
                                                file_path_sans_ext(basename(file))),
                 full.names = TRUE))
    })
    out <- lapply(allRelevantFiles, function(file) {
      retry(drive_upload(media = file,  path = as_id(cloudFolderID), name = basename(file)))
    })
  }
  return(invisible())
}

cloudDownloadRasterBackend <- function(output, cacheRepo, cloudFolderID,
                                       drv = RSQLite::SQLite()) {
  rasterFilename <- Filenames(output)
  if (!is.null(rasterFilename) && length(rasterFilename) > 0) {
    cacheRepoRasterDir <- file.path(cacheRepo, "rasters")
    checkPath(cacheRepoRasterDir, create = TRUE)
    simpleFilenames <- file_path_sans_ext(basename(unlist(rasterFilename)))
    retry({
      gdriveLs2 <- drive_ls(path = as_id(cloudFolderID),
                            pattern = paste(collapse = "|", simpleFilenames))
    })

    if (all(simpleFilenames %in% file_path_sans_ext(gdriveLs2$name))) {
      lapply(seq_len(NROW(gdriveLs2)), function(idRowNum) {
        localNewFilename <- file.path(cacheRepoRasterDir, basename(gdriveLs2$name[idRowNum]))
        retry(drive_download(file = as_id(gdriveLs2$id[idRowNum]),
                             path = localNewFilename, # take first if there are duplicates
                             overwrite = TRUE))

      })
      if (!all(file.exists(unlist(rasterFilename)))) {
        lapply(names(rasterFilename), function(rasName) {
          output[[rasName]] <- .prepareFileBackedRaster(output[[rasName]],
                                                        repoDir = cacheRepo, overwrite = FALSE,
                                                        drv = drv)
        })
        output <- .prepareFileBackedRaster(output, repoDir = cacheRepo, overwrite = FALSE,
                                           drv = drv)
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
    unlist(lapply(mget(ls(obj), envir = obj), function(x) isOrHasRaster(x)))
  } else if (is.list(obj)) {
    unlist(lapply(obj, function(x) isOrHasRaster(x)))
  } else {
    is(obj, "Raster")
  }
  return(rasters)
}
