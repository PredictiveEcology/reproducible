if (getRversion() >= "3.1.0") {
  utils::globalVariables(c("cacheId", "checksumsFilename", "checksumsID", "id"))
}

#' Check for presence of \code{checkFolderID} (for \code{Cache(useCloud)})
#'
#' Will check for presence of a \code{cloudFolderID} and make a new one
#' if one not present on googledrive, with a warning.
#'
#' @param cloudFolderID The google folder ID where cloud caching will occur.
#' @param create Logical. If \code{TRUE}, then the cloudFolderID will be created.
#'     This should be used with caution as there are no checks for overwriting.
#'     See \code{googledrive::drive_mkdir}. Default \code{FALSE}.
#' @export
#' @importFrom googledrive drive_mkdir
checkAndMakeCloudFolderID <- function(cloudFolderID = NULL, create = FALSE) {
  browser(expr = exists("kkkk"))
  isNullCFI <- is.null(cloudFolderID)
  if (isNullCFI && isTRUE(create)) {
    if (isNullCFI) {
      cloudFolderID <- rndstr(1, 6)
    }
    newDir <- retry(quote(drive_mkdir(cloudFolderID, path = "~/")))
    cloudFolderID = newDir$id
    if (isNullCFI)
      warning("No cloudFolderID supplied; if this is the first time using 'useCloud',",
              " this cloudFolderID, ", cloudFolderID,
              " should likely be kept and used in all subsequent calls to Cache using 'useCloud = TRUE'.", #nolint
              " Making a new cloud folder, '",newDir$name,
              "' and setting options('reproducible.cloudFolderID' = '",
              cloudFolderID, "')")
    options('reproducible.cloudFolderID' = cloudFolderID)
  }
  return(cloudFolderID)
}

driveLs <- function(cloudFolderID = NULL, pattern = NULL) {
  browser(expr = exists("kkkk"))
  cloudFolderID <- checkAndMakeCloudFolderID(cloudFolderID = cloudFolderID) # only deals with NULL case
  message("Retrieving file list in cloud folder")
  gdriveLs <- retry(quote(drive_ls(path = as_id(cloudFolderID),
                           pattern = paste0(collapse = "|", c(cloudFolderID ,pattern)))))
  if (is(gdriveLs, "try-error")) {
    fnf <- grepl("File not found", gdriveLs)
    if (!fnf) {
      gdriveLs <- retry(quote(drive_ls(path = as_id(cloudFolderID),
                                       pattern = paste0(cloudFolderID, "|",pattern))))
      #cloudFolderID <- checkAndMakeCloudFolderID(cloudFolderID, create = TRUE)
      #gdriveLs <- try(drive_ls(path = as_id(cloudFolderID), pattern = paste0(cloudFolderID, "|",pattern)))
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
#' @param gdriveLs The result of \code{googledrive::drive_ls(as_id(cloudFolderID), pattern = "outputHash")}
#' @param output The output object of FUN that was run in \code{Cache}
#' @importFrom googledrive drive_upload
#' @importFrom tools file_ext
#' @inheritParams Cache
cloudUpload <- function(isInRepo, outputHash, gdriveLs, cacheRepo, cloudFolderID, output) {
  artifact <- isInRepo[[.cacheTableHashColName()]][1]
  browser(expr = exists("kkkk"))
  artifactFileName <- CacheStoredFile(cacheRepo, hash = artifact)
  #artifactFileName <- paste0(artifact, ".rda")
  if (useDBI()) {
    newFileName <- basename2(artifactFileName)
  } else {
    newFileName <- paste0(outputHash,".rda")
  }
  isInCloud <- gsub(gdriveLs$name,
                    pattern = paste0("\\.", file_ext(CacheStoredFile(cacheRepo, outputHash))),
                    replacement = "") %in% outputHash

  if (!any(isInCloud)) {
    message("Uploading local copy of ", artifactFileName,", with cacheId: ",
            outputHash," to cloud folder")
    retry(quote(drive_upload(media = artifactFileName, path = as_id(cloudFolderID),
                             name = newFileName)))

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
                          drv = getOption("reproducible.drv", RSQLite::SQLite()),
                          conn = getOption("reproducible.conn", NULL)) {
  browser(expr = exists("kkkk"))
  message("Downloading cloud copy of ", newFileName,", with cacheId: ", outputHash)
  localNewFilename <- file.path(tempdir(), basename2(newFileName))
  isInCloud <- gsub(gdriveLs$name,
                    pattern = paste0("\\.", file_ext(CacheStoredFile(cacheRepo, outputHash))),
                    replacement = "") %in% outputHash

  retry(quote(drive_download(file = as_id(gdriveLs$id[isInCloud][1]),
                             path = localNewFilename, # take first if there are duplicates
                             overwrite = TRUE)))
  if (useDBI()) {
    if (getOption("reproducible.cacheSaveFormat", "qs") == "qs")
      output <- qread(localNewFilename, nthreads = getOption("reproducible.nThreads", 1))
    else
      output <- readRDS(localNewFilename)

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
#' @param saved         The character string of the saved file's digest value.
#' @param outputToSave  Only required if \code{any(rasters) == TRUE}.
#'                      This is the \code{Raster*} object.
#' @param rasters       A logical vector of length >= 1 indicating which elements in
#'                      \code{outputToSave} are \code{Raster*} objects.
#' @inheritParams cloudUpload
#'
#' @importFrom googledrive drive_download
#' @keywords internal
cloudUploadFromCache <- function(isInCloud, outputHash, saved, cacheRepo, cloudFolderID,
                                 outputToSave, rasters) {
  browser(expr = exists("kkkk"))
  if (!any(isInCloud)) {
    cacheIdFileName <- CacheStoredFile(cacheRepo, outputHash)
    newFileName <- if (useDBI()) {
      basename2(cacheIdFileName)
    } else {
      paste0(saved, ".rda")
    }
    message("Uploading new cached object ", newFileName,", with cacheId: ",
            outputHash," to cloud folder")
    cloudFolderID <- checkAndMakeCloudFolderID(cloudFolderID = cloudFolderID, create = TRUE)
    retry(quote(drive_upload(media = CacheStoredFile(cacheRepo, outputHash),
                             path = as_id(cloudFolderID), name = newFileName)))
  }
  cloudUploadRasterBackends(obj = outputToSave, cloudFolderID)
}

cloudUploadRasterBackends <- function(obj, cloudFolderID) {
  browser(expr = exists("kkkk"))
  rasterFilename <- Filenames(obj)
  if (!is.null(unlist(rasterFilename)) && length(rasterFilename) > 0) {
    allRelevantFiles <- sapply(rasterFilename, function(file) {
      unique(dir(dirname(file), pattern = paste(collapse = "|", file_path_sans_ext(basename(file))),
                 full.names = TRUE))
    })
    out <- lapply(allRelevantFiles, function(file) {
      retry(quote(drive_upload(media = file,  path = as_id(cloudFolderID), name = basename(file))))
    })
  }
  return(invisible())
}

cloudDownloadRasterBackend <- function(output, cacheRepo, cloudFolderID,
                                       drv = getOption("reproducible.drv", RSQLite::SQLite()),
                                       conn = getOption("reproducible.conn", NULL)) {
  browser(expr = exists("kkkk"))
  rasterFilename <- Filenames(output)
  if (!is.null(unlist(rasterFilename)) && length(rasterFilename) > 0) {
    gdriveLs2 <- NULL
    cacheRepoRasterDir <- file.path(cacheRepo, "rasters")
    checkPath(cacheRepoRasterDir, create = TRUE)
    simpleFilenames <- file_path_sans_ext(basename(unlist(rasterFilename)))
    retry(quote({
      gdriveLs2 <- drive_ls(path = as_id(cloudFolderID),
                            pattern = paste(collapse = "|", simpleFilenames))
    }))

    if (all(simpleFilenames %in% file_path_sans_ext(gdriveLs2$name))) {
      lapply(seq_len(NROW(gdriveLs2)), function(idRowNum) {
        localNewFilename <- file.path(cacheRepoRasterDir, basename(gdriveLs2$name[idRowNum]))
        retry(quote(drive_download(file = as_id(gdriveLs2$id[idRowNum]),
                                   path = localNewFilename, # take first if there are duplicates
                                   overwrite = TRUE)))

      })
      if (!all(file.exists(unlist(rasterFilename)))) {
        lapply(names(rasterFilename), function(rasName) {
          output[[rasName]] <- .prepareFileBackedRaster(output[[rasName]],
                                                        repoDir = cacheRepo, overwrite = FALSE,
                                                        drv = drv, conn = conn)
        })
        output <- .prepareFileBackedRaster(output, repoDir = cacheRepo, overwrite = FALSE,
                                           drv = drv, conn = conn)
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
      unlist(lapply(mget(ls(obj), envir = obj), function(x) isOrHasRaster(x)))
    } else {
      tryCatch(unlist(lapply(mget(ls(obj), envir = obj@.xData),
                        function(x) isOrHasRaster(x))), error = function(x) FALSE)
    }
  } else if (is.list(obj)) {
    unlist(lapply(obj, function(x) isOrHasRaster(x)))
  } else {
    is(obj, "Raster")
  }
  return(rasters)
}
