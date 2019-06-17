if (getRversion() >= "3.1.0") {
  utils::globalVariables(c("cacheId", "checksumsFilename", "checksumsID", "id"))
}

#' Check for presence of checkFolderID (for \code{Cache(useCloud)})
#'
#' Will check for presence of a \code{cloudFolderID} and make a new one
#' if one not present on googledrive, with a warning.
#'
#' @param cloudFolderID The google folder ID where cloud caching will occur.
#' @export
#' @importFrom googledrive drive_mkdir
#' @seealso \code{\link{cloudSyncCache}}, \code{\link{Cache}}, \code{\link{cloudWrite}}
checkAndMakeCloudFolderID <- function(cloudFolderID) {
  if (is.null(cloudFolderID)) {
    newDir <- drive_mkdir("testFolder")
    cloudFolderID = newDir$id
    warning("No cloudFolderID supplied; if this is the first time using 'useCloud', this cloudFolderID, ",
            cloudFolderID," should likely be kept and used in all subsequent calls to Cache using 'useCloud = TRUE'.",
            " Making a new cloud folder and setting options('reproducible.cloudFolderID' = ", cloudFolderID, ")")
    options('reproducible.cloudFolderID' = cloudFolderID)
  }
  return(cloudFolderID)
}


#' Upload to cloud, if necessary
#'
#' Meant for internal use, as there are internal objects as arguments.
#'
#' @importFrom googledrive drive_upload
cloudUpload <- function(isInRepo, outputHash, gdriveLs, cacheRepo, cloudFolderID, output) {
  artifact <- isInRepo$artifact[1]
  artifactFileName <- paste0(artifact, ".rda")
  newFileName <- paste0(outputHash,".rda")
  isInCloud <- gsub(gdriveLs$name, pattern = "\\.rda", replacement = "") %in% outputHash

  if (!any(isInCloud)) {
    message("Uploading local copy of ", artifactFileName,", with cacheId: ",
            outputHash," to cloud folder")
    drive_upload(media = file.path(cacheRepo, "gallery", artifactFileName),
                 path = as_id(cloudFolderID), name = newFileName)

    outputIsList <- is.list(output)
    if (outputIsList) {
      rasters <- unlist(lapply(output, is, "Raster"))
    } else {
      rasters <- is(output, "Raster")
    }

    if (any(rasters)) {
      rasterFilename <- Filename(output)
      rasterDirname <- dirname(rasterFilename)
      browser(expr = exists("aaa"))
      allRelevantFiles <- unique(dir(rasterDirname, pattern = paste(collapse = "|", file_path_sans_ext(basename(rasterFilename))),
                                     full.names = TRUE))
      out <- lapply(allRelevantFiles, function(file) {
        drive_upload(media = file,  path = as_id(cloudFolderID), name = basename(file))
      })

    }

  }
}


#' Download from cloud, if necessary
#'
#' Meant for internal use, as there are internal objects as arguments.
#'
#' @importFrom googledrive drive_download
cloudDownload <- function(outputHash, newFileName, gdriveLs, cacheRepo, cloudFolderID, isInCloud) {
  message("Downloading cloud copy of ", newFileName,", with cacheId: ",
          outputHash)
  localNewFilename <- file.path(tempdir(), newFileName)
  drive_download(file = as_id(gdriveLs$id[isInCloud][1]),
                 path = localNewFilename, # take first if there are duplicates
                 overwrite = TRUE)
  ee <- new.env(parent = emptyenv())
  loadedObjName <- load(localNewFilename)
  output <- get(loadedObjName, inherits = FALSE)
  if (is(output, "Raster")) {
    cacheRepoRasterDir <- file.path(cacheRepo, "rasters")
    checkPath(cacheRepoRasterDir, create = TRUE)
    gdriveLs2 <- drive_ls(path = as_id(cloudFolderID),
                          pattern = paste(collapse = "|", file_path_sans_ext(basename(Filename(output)))))

    lapply(seq_len(NROW(gdriveLs2)), function(idRowNum) {
      localNewFilename <- file.path(cacheRepoRasterDir, basename(gdriveLs2$name[idRowNum]))
      drive_download(file = as_id(gdriveLs2$id[idRowNum]),
                     path = localNewFilename, # take first if there are duplicates
                     overwrite = TRUE)

    })
    if (!all(file.exists(Filename(output))))
      output <- .prepareFileBackedRaster(output, repoDir = cacheRepo, overwrite = FALSE)

  }
  rm(loadedObjName)
  output
}

cloudUploadFromCache <- function(isInCloud, outputHash, saved, cacheRepo, cloudFolderID, outputToSave, rasters) {
if (!any(isInCloud)) {
  cacheIdFileName <- paste0(outputHash,".rda")
  newFileName <- paste0(saved, ".rda")
  message("Uploading new cached object ", newFileName,", with cacheId: ",
          outputHash," to cloud folder")
  drive_upload(media = file.path(cacheRepo, "gallery", newFileName),
               path = as_id(cloudFolderID), name = cacheIdFileName)
  if (any(rasters)) {
    rasterFilename <- Filename(outputToSave)
    rasterDirname <- dirname(rasterFilename)
    browser(expr = exists("aaa"))
    allRelevantFiles <- unique(dir(rasterDirname,
                                   pattern = paste(collapse = "|", file_path_sans_ext(basename(rasterFilename))),
                                   full.names = TRUE))
    out <- lapply(allRelevantFiles, function(file) {
      drive_upload(media = file,  path = as_id(cloudFolderID), name = basename(file))
    })

  }
}
}
