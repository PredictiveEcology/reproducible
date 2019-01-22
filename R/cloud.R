if (getRversion() >= "3.1.0") {
  utils::globalVariables(c("checksumsFilename", "checksumsID", "id"))
}

#' Basic tool for using cloud-based caching
#'
#' Very experimental
#' @param toDigest The R object to consider, e.g., all the arguments to a function.
#'
#' @param checksumsFileID A google file ID where the checksums data.table is located,
#'   provided as a character string.
#'
#' @param cloudFolderID The google folder ID where a new checksums file should
#'    be written. This will only be used if \code{checksumsFileID} is not provided
#'   provided as a character string.
#' @export
#' @importFrom googledrive as_id drive_download drive_upload
#' @seealso \code{\link{cloudSyncCache}}, \code{\link{Cache}}, \code{\link{cloudWrite}}
cloudCheck <- function(toDigest, checksumsFileID = NULL, cloudFolderID = NULL) {
  dig <- CacheDigest(toDigest)$outputHash
  if (is.null(checksumsFileID)) {
    if (!is.null(cloudFolderID)) {
      checksumsFileID <- getChecksumsFileID(cloudFolderID)
    } else {
      stop("checksumsFileID must be supplied, or else cloudFolderID to create a new one")
    }
  }

  suppressMessages(checksums <- cloudDownloadChecksums(checksumsFileID))
  hashExists <- checksums$cacheId == dig
  out <- if (isTRUE(any(hashExists))) {
    objectFilename2 <- tempfile(fileext = ".rda");
    a <- checksums[hashExists]$filesize
    class(a) <- "object_size"
    message("  downloading object from google drive; this could take a while; it is: ",
            format(a, "auto"))
    drive_download(as_id(checksums[hashExists, id]), path = objectFilename2, verbose = FALSE)
    env <- new.env()
    load(objectFilename2, envir = env)
    as.list(env)[[1]]
  } else {
    NULL
  }
  return(list(object = out, digest = dig, checksums = checksums,
              checksumsFileID = checksumsFileID))
}

#' Basic tool for using cloud-based caching
#'
#' Very experimental
#'
#' @inheritParams cloudCheck
#' @param object The R object to write to cloud
#' @param digest The cacheId of the input arguments, outputted from \code{cloudCheck}
#' @param checksums A \code{data.table} that is outputted from \code{cloudCheck} that
#'   is the the checksums file
#' @param cloudFolderID The google folder ID where a new object should be written
#'
#' @export
#' @importFrom data.table data.table rbindlist
#' @importFrom googledrive as_id drive_update drive_upload
#' @seealso \code{\link{cloudSyncCache}}, \code{\link{cloudCheck}}, \code{\link{cloudExtras}}
cloudWrite <- function(object, digest, cloudFolderID = NULL, checksums, checksumsFileID) {
  if (!is.null(cloudFolderID)) {
    objectFile <- tempfile(fileext = ".rda")
    save(object, file = objectFile)
    # checksums <- cloudDownloadChecksums(checksumsFileID)
    os <- tolower(Sys.info()[["sysname"]])
    .onLinux <- .Platform$OS.type == "unix" && unname(os) == "linux" &&
      requireNamespace("future", quietly = TRUE) &&
      !isFALSE(getOption("reproducible.futurePlan"))
    if (.onLinux) {
      curPlan <- future::plan()
      if (is(curPlan, "sequential"))
        future::plan(getOption("reproducible.futurePlan", "multiprocess"))
      message("  uploading object to google drive in a 'future'")
      future::futureAssign("cloudCheckSums", assign.env = .reproEnv,
                           cloudUploadFileAndChecksums(objectFile, cloudFolderID, digest,
                                                       checksums, checksumsFileID))
    } else {
      message("  uploading object to google drive; this could take a while")
      cloudUploadFileAndChecksums(objectFile, cloudFolderID, digest,
                                  checksums, checksumsFileID)

    }

  } else {
    stop("cloudFolder must be provided as a google id string to a folder (not a file)")
  }
}

#' Cloud extras
#'
#' Mostly for internal use, but may be useful to a user.
#'
#' @details
#' \code{cloudDownloadChecksums} gets the checksums data.table directly.
#'
#' @inheritParams cloudCheck
#'
#' @aliases cloudExtras
#' @export
#' @importFrom googledrive as_id drive_download
#' @rdname cloudExtras
#' @seealso \code{\link{cloudSyncCache}}, \code{\link{cloudCache}}, \code{\link{cloudExtras}}
cloudDownloadChecksums <- function(checksumsFileID) {
  checksumsFilename <- tempfile(fileext = ".rds");
  os <- tolower(Sys.info()[["sysname"]])
  .onLinux <- .Platform$OS.type == "unix" && unname(os) == "linux" &&
    requireNamespace("future", quietly = TRUE) &&
    !isFALSE(getOption("reproducible.futurePlan"))
  if (.onLinux)
    if (exists("cloudChecksums", envir = .reproEnv)) # it should have been deleted in `checkFutures`
      bb <- future::value(.reproEnv)       # but if not, then force its value here
  drive_download(as_id(checksumsFileID), path = checksumsFilename, verbose = FALSE)
  checksums <- readRDS(checksumsFilename)
}

#' @details
#' \code{cloudDownloadChecksums} gets the checksums data.table directly.
#'
#' @inheritParams cloudWrite
#'
#' @export
#' @importFrom googledrive as_id drive_update
#' @rdname cloudExtras
cloudUpdateChecksums <- function(checksums, checksumsFileID) {
  saveRDS(checksums, file = getOption("reproducible.cloudChecksumsFilename"))
  drive_update(as_id(checksumsFileID), media = getOption("reproducible.cloudChecksumsFilename"),
               verbose = FALSE)
}

#' Experimental use of googledrive for Caching
#'
#' This is still very experimental. See examples.
#'
#' @details
#' This has \code{Cache} internally. The main goal of this function is to look at
#' local Cache first, if the object is there locally, then use it & upload it to
#' googledrive. If the object is not there locally, check on googledrive for the
#' object. If it is there, download it, then add it to the local Cache. If it is
#' not there, then run the function de novo, wrapped in \code{Cache} and upload
#' the object to googledrive (i.e., it will be in local Cache and cloud location).
#'
#' @note This is essentially a wrapper around \code{Cache}, so it will still use
#' the local Cache.
#'
#' @inheritParams cloudCheck
#' @inheritParams cloudWrite
#' @param ... Passed to \code{\link{Cache}}
#' @param useCloud Logical. This allows this to be turned off; will send all arguments to
#'   \code{Cache} (including possibly \code{useCache}, where all caching can be turned off)
#'
#' @export
#' @importFrom archivist createLocalRepo saveToLocalRepo
#' @importFrom data.table setattr
#' @rdname cloudCache
#' @seealso \code{\link{cloudSyncCache}}, \code{\link{Cache}}, \code{\link{cloudWrite}},
#'   \code{\link{cloudCheck}}, \code{\link{cloudExtras}}
#'
#' @examples
#' \dontrun{
#' # Make a folder on googledrive -- share it with yourself and anybody else -- either use
#' #   googledrive package or do this manually on drive.google.com
#' # Grab the share link -- pass it here to cloudFolderID
#' options("reproducible.useNewDigestAlgorithm" = TRUE) # need new approach for this to work correctly
#'
#' # first time -- looks in cloudFolderID for checksums -- none there, so it makes it
#' #   then it runs the function, caching locally, and uploading to cloud -- copy exists in
#' #   2 places
#' library(googledrive)
#' newDir <- drive_mkdir("testFolder")
#' a1 <- cloudCache(rnorm, 1, cloudFolderID = newDir$id)
#' # second time -- sees that it is in both places, takes local
#' a2 <- cloudCache(rnorm, 1, cloudFolderID = newDir$id)
#'
#' # clear local -- get from cloud copy, make a local copy in cacheRepo
#' clearCache(ask = FALSE)
#' a3 <- cloudCache(rnorm, 1, cloudFolderID = newDir$id)
#'
#' # now both local and cloud exist
#' a4 <- cloudCache(rnorm, 1, cloudFolderID = newDir$id)
#'
#' # Clean up -- see cloudSyncCache
#' }
cloudCache <- function(..., useCloud = getOption("reproducible.useCloud", TRUE),
                       checksumsFileID = NULL, cloudFolderID = NULL) {

  if (is.null(checksumsFileID))
    if (is.null(cloudFolderID))
      stop("You must supply a checksumsFileID or, if not supplied, a cloudFolderID where the",
           "\n checksums file will be made")
  hasCopy <- FALSE
  hasCloudCopy <- FALSE

  fnDetails <- .fnCleanup(FUN = list(...)[[1]], callingFun = "Cache", ...)
  cacheRepo <- suppressMessages(.checkCacheRepo(fnDetails$modifiedDots, create = TRUE))
  suppressMessages(archivist::createLocalRepo(cacheRepo))

  dots <- list(...)
  if (isTRUE(useCloud)) {
    if (!is.null(dots$omitArgs)) {
      dots <- .CacheDigestWithOmitArgs(sys.call(), envir = sys.frame(-1))
    }
    dig <- CacheDigest(dots)
    checkLocalFirst <- try(suppressMessages(showCache(userTags = dig$outputHash, x = cacheRepo)))
    hasLocalCopy <- NROW(checkLocalFirst) > 0 && !is(checkLocalFirst, "try-error")
    if (hasLocalCopy) {
      if (is.null(checksumsFileID))
        checksumsFileID <- getChecksumsFileID(cloudFolderID)
      suppressMessages(checksums <- cloudDownloadChecksums(checksumsFileID))
      hasCloudCopy <- any(checksums$cacheId %in% dig$outputHash)
      if (hasCloudCopy)
        message("  local and cloud copy exist; using local")
      else
        message("  local copy exists; using it; will upload copy to googledrive")

      cachedCopy <- list(digest = dig$outputHash, checksums = checksums,
                         checksumsFileID = checksumsFileID)
    } else {
      cachedCopy <- cloudCheck(dots, checksumsFileID, cloudFolderID)
      checksumsFileID <- cachedCopy$checksumsFileID
      hasCloudCopy <- any(cachedCopy$checksums$cacheId %in% cachedCopy$digest)
      # it may not have a cloud copy either, meaning it will have a NULL
      if (!is.null(cachedCopy$object)) {
        message("  writing to local Cache")
        objSize <- sum(unlist(objSize(cachedCopy)))
        preDigestUnlistTrunc <- unlist(.unlistToCharacter(dig$preDigest, 3))
        userTags <- c(if (!is.na(fnDetails$functionName))
          paste0("function:", fnDetails$functionName),
          paste0("object.size:", objSize),
          paste0("accessed:", Sys.time()),
          paste("preDigest", names(preDigestUnlistTrunc),
                preDigestUnlistTrunc, sep = ":")
        )
        written <- 0
        suppressMessages(cacheRepo <- .checkCacheRepo(fnDetails$modifiedDots, create = TRUE))
        setattr(cachedCopy$object, "tags", paste0("cacheId:", dig$outputHash))
        while (written >= 0) {
          saved <- suppressWarnings(try(silent = TRUE,
                                        archivist::saveToLocalRepo(
                                          cachedCopy$object,
                                          repoDir = cacheRepo,
                                          artifactName = NULL,
                                          archiveData = FALSE,
                                          archiveSessionInfo = FALSE,
                                          archiveMiniature = FALSE,
                                          rememberName = FALSE,
                                          silent = TRUE,
                                          userTags = userTags
                                        )
          ))

          # This is for simultaneous write conflicts. SQLite on Windows can't handle them.
          written <- if (is(saved, "try-error")) {
            Sys.sleep(sum(runif(written + 1, 0.05, 0.1)))
            written + 1
          } else {
            -1
          }
        }
      }
    }
  }
  out <- if (!hasCloudCopy || hasLocalCopy) {
    Cache(..., cacheRepo = cacheRepo)
  } else {
    cachedCopy$object
  }
  if (isTRUE(useCloud)) {
    if (!(hasCloudCopy)) {
      cloudWrite(out, digest = cachedCopy$digest, checksums = cachedCopy$checksums,
                 cloudFolderID = cloudFolderID,
                 checksumsFileID = cachedCopy$checksumsFileID)
    }
  }
  setattr(out, "reproducible.checksumsFileID", checksumsFileID)
  return(out)
}

#' @importFrom crayon blue
#' @importFrom data.table data.table
#' @importFrom googledrive as_id drive_ls drive_upload
#' @keywords internal
getChecksumsFileID <- function(cloudFolderID) {
  lsFiles <- googledrive::drive_ls(as_id(cloudFolderID))
  whChecksums <- which(lsFiles$name %in% "checksums")
  checksumsFileID <- if (length(whChecksums) > 0) {
    lsFiles[whChecksums[1],]$id
  } else {
    message(crayon::blue("There is no checkums file yet; creating it and uploading"))
    checksums <- data.table(cacheId = character(), id = character(), time = character(),
                            filesize = integer())
    saveRDS(checksums, file = getOption("reproducible.cloudChecksumsFilename"))
    res <- drive_upload(getOption("reproducible.cloudChecksumsFilename"),
                        path = as_id(cloudFolderID),
                        name = "checksums",
                        verbose = FALSE)
    res$id
  }
  return(checksumsFileID)
}

#' Sync cloud with local Cache
#'
#' This is still experimental, see examples.
#'
#' @details
#' \code{cloudSyncCache} will remove any entries in a cloudCache that are not in a
#'
#' @inheritParams Cache
#' @inheritParams cloudCache
#' @param cacheRepo See \code{x} in \code{\link{showCache}}
#' @param delete Logical. If \code{TRUE}, the default, it will delete any objects
#'   that are in \code{cloudFolderID} that are absent from local \code{cacheRepo}.
#'   If \code{FALSE}, it will not delete objects.
#'
#' @export
#' @importFrom crayon blue
#' @importFrom data.table data.table rbindlist
#' @importFrom googledrive as_id drive_rm drive_upload
#' @seealso \code{\link{cloudCache}}, \code{\link{Cache}}, \code{\link{cloudWrite}},
#'   \code{\link{cloudCheck}}, \code{\link{cloudExtras}}
#' @examples
#' \dontrun{
#'   #make a google drive folder
#'   library(googledrive)
#'   newDir <- drive_mkdir("testFolder")
#'   a <- Cache(rnorm, 1)
#'   b <- Cache(rnorm, 2)
#'
#'   # Will copy the 2 to the cloud
#'   cloudSyncCache(cloudFolderID = newDir$id)
#'
#'   # remove a local one
#'   clearCache(userTags = CacheDigest(list(rnorm, 2))$outputHash, ask = FALSE)
#'
#'   # Now will delete the object in the cloud that was just deleted locally
#'   cloudSyncCache(cloudFolderID = newDir$id)
#'
#'   # clean up
#'   clearCache(ask = FALSE)
#'   cloudSyncCache(cloudFolderID = newDir$id)
#'
#'   # To remove whole folder:
#'   drive_rm(as_id(newDir$id))
#' }
cloudSyncCache <- function(cacheRepo = getOption("reproducible.cachePath")[1],
                           checksumsFileID = NULL, cloudFolderID = NULL,
                           delete = TRUE, ...) {
  if (is.null(checksumsFileID)) {
    if (!is.null(cloudFolderID)) {
      checksumsFileID <- getChecksumsFileID(cloudFolderID)
    } else {
      stop("checksumsFileID must be supplied, or else cloudFolderID to create a new one")
    }
  }

  suppressMessages(checksums <- cloudDownloadChecksums(checksumsFileID))
  suppressMessages(localCache <- showCache(x = cacheRepo, ...))
  out <- NULL

  if (NROW(localCache)) {
    uniqueCacheArtifacts <- unique(localCache$artifact)
    cacheIDs <- localCache[artifact %in% uniqueCacheArtifacts & tagKey == "cacheId"]$tagValue
    needToUpload <- !(cacheIDs %in% checksums$cacheId)

    if (sum(needToUpload) > 0) {
      message(crayon::blue("Uploading", length(needToUpload), "files, ",
                           "\n ", paste(uniqueCacheArtifacts[needToUpload], ".rda", sep = "",
                                        collapse = "\n  ")))
      out <- lapply(uniqueCacheArtifacts[needToUpload], function(art) {
        cacheID <- localCache[artifact == art & tagKey == "cacheId"]$tagValue
        drive_upload(file.path(cacheRepo, "gallery", paste0(art, ".rda")),
                     path = as_id(cloudFolderID),
                     name = paste0(cacheID, ".rda"),
                     verbose = FALSE)
      })
      checksums <- rbindlist(
        list(checksums,
             data.table(cacheId = cacheIDs, id = rbindlist(out)$id, time = as.character(Sys.time()),
                        filesize = file.size(file.path(cacheRepo, "gallery", paste0(uniqueCacheArtifacts, ".rda"))))),
        use.names = TRUE, fill = TRUE)
      out <- rbindlist(out)
    }

    needToDelete <- !(checksums$cacheId %in% cacheIDs)
  } else {
    needToDelete <- rep(TRUE, NROW(checksums))
  }

  if (isTRUE(delete) && any(needToDelete)) {
    rmRes <- drive_rm(as_id(checksums$id[needToDelete]), verbose = FALSE)
    if (isTRUE(all(rmRes))) {
      message(crayon::blue(sum(needToDelete), "file(s) deleted:",
                           "\n ", paste(checksums$cacheId[needToDelete], ".rda",
                                        sep = "", collapse = "\n  ")))
    }
    checksums <- checksums[!needToDelete]
  }
  suppressMessages(cloudUpdateChecksums(checksums, checksumsFileID))
  return(invisible(checksums))
}

cloudUploadFileAndChecksums <- function(objectFile, cloudFolderID, digest,
                                        checksums, checksumsFileID) {
  uploadRes <- drive_upload(objectFile, path = as_id(cloudFolderID),
                            name = paste0(digest, ".rda"), verbose = FALSE)
  checksums <- rbindlist(
    list(checksums,
         data.table(cacheId = digest, id = uploadRes$id, time = as.character(Sys.time()),
                    filesize = file.size(objectFile))), use.names = TRUE, fill = TRUE)
  saveRDS(checksums, file = getOption("reproducible.cloudChecksumsFilename"))
  res <- drive_update(as_id(checksumsFileID),
               media = getOption("reproducible.cloudChecksumsFilename"),
               verbose = FALSE)
  return(invisible(res))
}

.CacheDigestWithOmitArgs <- function(sysCallWCloudCache, envir) {
  a <- match.call(Cache, sysCallWCloudCache, expand.dots = TRUE)[-1]
  forms <- formalArgs(cloudCache)
  b <- match.call(eval(a$FUN), a, expand.dots = TRUE)
  b[names(b) %in% eval(b$omitArgs) | names(b) %in% forms | names(b) %in% names(.formalsCache)] <- NULL
  dots <- lapply(b, eval, envir = envir)

}
