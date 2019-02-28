if (getRversion() >= "3.1.0") {
  utils::globalVariables(c("cacheID", "checksumsFilename", "checksumsID", "id"))
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
    cloudDownload(checksums[hashExists])
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
#' @param futurePlan Which \code{future::plan} to use. Default:
#'    \code{getOption("reproducible.futurePlan")}
#'
#' @export
#' @importFrom data.table data.table rbindlist
#' @importFrom googledrive as_id drive_update drive_upload
#' @seealso \code{\link{cloudSyncCache}}, \code{\link{cloudCheck}}, \code{\link{cloudExtras}}
cloudWrite <- function(object, digest, cloudFolderID = NULL, checksums, checksumsFileID,
                       futurePlan = getOption("reproducible.futurePlan")) {
  if (!is.null(cloudFolderID)) {
    objectFile <- tempfile(fileext = ".rda")
    save(object, file = objectFile)
    # checksums <- cloudDownloadChecksums(checksumsFileID)
    os <- tolower(Sys.info()[["sysname"]])
    .onLinux <- .Platform$OS.type == "unix" && unname(os) == "linux" &&
      requireNamespace("future", quietly = TRUE) &&
      !isFALSE(futurePlan)
    a <- file.size(objectFile)
    class(a) <- "object_size"
    msgUpload <-   paste0("  uploading file (", format(a, "auto"),") to google drive")
    if (.onLinux) {
      curPlan <- future::plan()
      if (is(curPlan, "sequential"))
        future::plan(futurePlan)
      message(msgUpload, " in a 'future'\n    Filename: ", paste0(digest, ".rda"))
      rstr <- rndstr(len = 10)
      if (!exists("futureEnv", envir = .reproEnv))
        .reproEnv$futureEnv <- new.env()
      future::futureAssign(paste0("cloudCheckSums", rstr), assign.env = .reproEnv$futureEnv,
                           cloudUploadFileAndChecksums(objectFile, cloudFolderID, digest,
                                                       checksums, checksumsFileID))
    } else {
      message(msgUpload)
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
    if (exists("futureEnv", envir = .reproEnv))
      if (exists("cloudChecksums", envir = .reproEnv$futureEnv)) # it should have been deleted in `checkFutures`
        bb <- future::value(.reproEnv$futureEnv)       # but if not, then force its value here
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

  hasCopy <- FALSE
  hasCloudCopy <- FALSE

  if (is.null(list(...)$cacheRepo)) {
    cacheRepos <- suppressMessages(.checkCacheRepo(list(...), create = TRUE))
  } else {
    cacheRepos <- lapply(list(...)$cacheRepo, function(repo) {
      repo <- checkPath(repo, create = TRUE)
    })
  }
  cacheRepo <- cacheRepos[[1]]

  #cacheRepo <- suppressMessages(.checkCacheRepo(list(...), create = TRUE))#fnDetails$modifiedDots, create = TRUE))
  suppressMessages(archivist::createLocalRepo(cacheRepo))
  if (isTRUE(useCloud)) {

    if (is.null(checksumsFileID))
      if (is.null(cloudFolderID))
        stop("You must supply a checksumsFileID or, if not supplied, a cloudFolderID where the",
             "\n checksums file will be made")

    dots <- list(...)
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
        message("  local copy exists; using it; need to upload copy to googledrive")

      cachedCopy <- list(digest = dig$outputHash, checksums = checksums,
                         checksumsFileID = checksumsFileID)
    } else {
      cachedCopy <- cloudCheck(dots, checksumsFileID, cloudFolderID)
      checksumsFileID <- cachedCopy$checksumsFileID
      hasCloudCopy <- any(cachedCopy$checksums$cacheId %in% cachedCopy$digest)
      # it may not have a cloud copy either, meaning it will have a NULL
      if (!is.null(cachedCopy$object)) {

        fnDetails <- .fnCleanup(FUN = eval(match.call(Cache, expand.dots = TRUE)$FUN),
                                callingFun = "Cache", ...)

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
    if ("cacheRepo" %in% names(list(...))) {
      Cache(...)
    } else {
      Cache(..., cacheRepo = cacheRepo)
    }
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
#' @inheritParams clearCache
#' @param cacheRepo See \code{x} in \code{\link{showCache}}
#' @param cacheIds If supplied, then only this/these cacheId objects
#'   will be uploaded or deleted. Default is \code{NULL}, meaning do
#'   full sync (i.e., match cloudFolder with local cacheRepo, constrained by
#'   \code{delete} or \code{upload})
#' @param delete Logical. If \code{TRUE}, the default, it will delete any objects
#'   that are in \code{cloudFolderID} that are absent from local \code{cacheRepo}.
#'   If \code{FALSE}, it will not delete objects.
#' @param upload Logical. If \code{TRUE}, the default, it will upload any objects
#'   identified by the internal \code{showCache(...)} call. See examples. If \code{FALSE},
#'   then no files will be uploaded. Can be used in conjunction with \code{delete}
#'   to create behaviours similar to \code{clearCache} and \code{keepCache}.
#' @param download Logical. If \code{FALSE}, the default, then the function will
#'   either delete the remote copy if \code{delete = TRUE} and there is no local
#'   copy, or upload the local copy if \code{upload = TRUE} and there is a local
#'   copy. If \code{TRUE}, then this will override \code{delete}, and download
#'   to local machine if it exists remotely.
#' @param ... Passed to \code{showCache} to get the artifacts to delete
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
#'   opts <- options("reproducible.cachePath" = tempdir(), "reproducible.ask" = FALSE)
#'   library(googledrive)
#'   newDir <- drive_mkdir("testFolder")
#'   a <- Cache(rnorm, 1)
#'   b <- Cache(rnorm, 2)
#'
#'   # Will copy the 2 to the cloud
#'   cloudSyncCache(cloudFolderID = newDir$id)
#'
#'   # remove a local one
#'   clearCache(userTags = CacheDigest(list(rnorm, 2))$outputHash)
#'
#'   # Now will delete the object in the cloud that was just deleted locally
#'   cloudSyncCache(cloudFolderID = newDir$id)
#'
#'   # clean up
#'   clearCache(ask = FALSE)
#'   cloudSyncCache(cloudFolderID = newDir$id)
#'
#'   #######################################################################
#'   # use showCache args to have control ... on upload & delete NOTE difference!
#'   #######################################################################
#'   a <- Cache(rnorm, 1)
#'   b <- Cache(rnorm, 2)
#'   # only sync the one with rnorm, 2 as arguments
#'   #   This CacheDigest is the same algorithm used by Cache
#'   tag <- CacheDigest(list(rnorm, 2))$outputHash
#'   cloudSyncCache(cloudFolderID = newDir$id, userTags = tag) # only syncs the one
#'                                                             # that is identified
#'                                                             # with userTags
#'
#'   cloudSyncCache(cloudFolderID = newDir$id) # sync any other ones
#'
#'   # Now clear an object locally -- next how to propagate this deletion to cloud
#'   clearCache(userTags = tag)
#'
#'   # Add one more to local, so now local has 2 (a and d), cloud has 2 (a and b)
#'   d <- Cache(rnorm, 4)
#'
#'   # DELETING IS DIFFERENT
#'   # Doesn't quite work same way for deleting -- this tag is not in local Cache,
#'   # so can't find it this way.
#'   # This next line DOES THE WRONG THING -- IT DELETES EVERYTHING because there is
#'   #         no entry in the local cache -- use cacheId arg instead -- see below
#'   #    showCache(userTags = tags) shows empty
#'   #    cloudSyncCache(cloudFolderID = newDir$id, userTags = tag)
#'
#'   # Only delete the one that was removed from local cache, set upload = FALSE,
#'   #    leaving only 1 in cloud: a  -- this is still a sync, so, it will only
#'   #    delete 1 file because local has 1 few files -- see next for just deleting 1 artifact
#'   cloudSyncCache(cloudFolderID = newDir$id, upload = FALSE)
#'   # Upload the d, because it is the only one in the localCache not in the cloudCache
#'   cloudSyncCache(cloudFolderID = newDir$id)
#'
#'   f <- Cache(rnorm, 5)
#'   g <- Cache(rnorm, 6)
#'   # upload both
#'   cloudSyncCache(cloudFolderID = newDir$id) # only syncs the one
#'   tag5 <- CacheDigest(list(rnorm, 5))$outputHash # this is the same algorithm used by Cache
#'   tag6 <- CacheDigest(list(rnorm, 6))$outputHash
#'   clearCache(userTags = tag5)
#'   clearCache(userTags = tag6)
#'   # delete only one by tag
#'   cloudSyncCache(cloudFolderID = newDir$id, cacheIds = tag5) # will delete only this obj in cloud
#'   # delete another one by tag
#'   cloudSyncCache(cloudFolderID = newDir$id, cacheIds = tag6)
#'
#'   # clean up
#'   clearCache(ask = FALSE)
#'   cloudSyncCache(cloudFolderID = newDir$id)
#'
#'   # To remove whole folder:
#'   drive_rm(as_id(newDir$id))
#'   options(opts)
#' }
cloudSyncCache <- function(cacheRepo = getOption("reproducible.cachePath")[1],
                           checksumsFileID = NULL, cloudFolderID = NULL,
                           delete = TRUE, upload = TRUE, download = !delete,
                           ask = getOption("reproducible.ask"),
                           cacheIds = NULL,
                           ...) {
  if (isTRUE(download)) delete <- FALSE
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

  needToDelete <- NULL
  uniqueCacheArtifacts <- unique(localCache$artifact)
  if (!is.null(uniqueCacheArtifacts))
    cacheIDs <- localCache[artifact %in% uniqueCacheArtifacts & tagKey == "cacheId"]$tagValue

  if (!is.null(cacheIds)) {
    cacheIdsExisting <- which(cacheIDs %in% cacheIds)
    if (length(cacheIdsExisting) == 0) {
      message("None of the supplied cacheIds exist locally: \n  ", paste(cacheIds, sep = "\n  "))
    }
    cacheIDs <- cacheIDs[cacheIdsExisting]
  }

  if (NROW(localCache) && isTRUE(upload)) {
    needToUpload <- !(cacheIDs %in% checksums$cacheId)

    if (sum(needToUpload) > 0) {
      message(crayon::blue("Uploading", sum(needToUpload), "files, ",
                           "\n ", paste(cacheIDs[needToUpload], ".rda ( == ",
                                        uniqueCacheArtifacts[needToUpload],
                                        ".rda in local cache)", sep = "",
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
             data.table(
               cacheId = cacheIDs[needToUpload],
               id = rbindlist(out)$id,
               time = as.character(Sys.time()),
               filesize = file.size(file.path(cacheRepo, "gallery",
                                              paste0(uniqueCacheArtifacts[needToUpload], ".rda")))
             )
        ),
        use.names = TRUE, fill = TRUE)
      out <- rbindlist(out)
    }

  } else {
    if (NROW(localCache) == 0) {
      needToDelete <- rep(TRUE, NROW(checksums))
    }
  }
  if (is.null(needToDelete)) {
    if (!is.null(uniqueCacheArtifacts)) {
      needToDelete <- if (!is.null(cacheIds)) {
        out <- (checksums$cacheId %in% cacheIds)
        if (all(!out)) {
          message("That/those cacheId(s) are not in the cloudFolderID. Nothing to delete.")
        }
        out
      } else {
        !(checksums$cacheId %in% cacheIDs)
      }

    }
  }

  if (isTRUE(delete) && any(needToDelete)) {
    out <- if (isTRUE(ask)) {
      readline(paste0("Are you sure you want to delete:\n  ",
                      paste(checksums$cacheId[needToDelete], ".rda",
                            sep = "", collapse = "\n  "), "\n Y or N: "))
    } else {
      "Y"
    }
    rmRes <- if (identical(tolower(out), "y")) {
      drive_rm(as_id(checksums$id[needToDelete]), verbose = FALSE)
    } else {
      FALSE
    }
    if (isTRUE(all(rmRes))) {
      message(crayon::blue(sum(needToDelete), "file(s) deleted:",
                           "\n ", paste(checksums$cacheId[needToDelete], ".rda",
                                        sep = "", collapse = "\n  ")))
      checksums <- checksums[!needToDelete]
    }
  } else {
    if (any(needToDelete)) {
      needToDownload <- needToDelete
      if (isTRUE(download)) {
        if (!cacheIds %in% localCache[artifact %in% uniqueCacheArtifacts & tagKey == "cacheId"]$tagValue) {
          out <- cloudDownload(checksums[needToDownload,])
          objSize <- sum(unlist(objSize(out)))
          #preDigestUnlistTrunc <- unlist(.unlistToCharacter(CacheDigest(list(out))$preDigest, 3))
          userTags <- c(#if (!is.na(fnDetails$functionName))
            #paste0("function:", fnDetails$functionName),
            paste0("object.size:", objSize),
            paste0("accessed:", Sys.time())#,
            #paste("preDigest", names(preDigestUnlistTrunc),
            #      preDigestUnlistTrunc, sep = ":")
          )

          written <- 0
          suppressMessages(cacheRepo <- .checkCacheRepo(cacheRepo, create = TRUE))
          setattr(out, "tags", paste0("cacheId:", checksums[needToDownload, cacheId]))
          message("  Writing to local cacheRepo")
          while (written >= 0) {
            saved <- suppressWarnings(try(silent = TRUE,
                                          archivist::saveToLocalRepo(
                                            out,
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
        } else {
          message("  cacheIds already present locally")
        }
      } else {
        message("There were files identified to delete to create full sync, but delete = FALSE")
      }
    }

  }
  suppressMessages(cloudUpdateChecksums(checksums, checksumsFileID))
  return(invisible(checksums))
}


cloudUploadFileAndChecksums <- function(objectFile, cloudFolderID, digest,
                                        checksums, checksumsFileID) {
  uploadRes <- try(drive_upload(objectFile, path = as_id(cloudFolderID),
                            name = paste0(digest, ".rda"), verbose = FALSE), silent = TRUE)
  if (!is(uploadRes, "try-error")) {

    checksums <- rbindlist(
      list(checksums,
           data.table(cacheId = digest, id = uploadRes$id, time = as.character(Sys.time()),
                      filesize = file.size(objectFile))), use.names = TRUE, fill = TRUE)
    saveRDS(checksums, file = getOption("reproducible.cloudChecksumsFilename"))
    res <- drive_update(as_id(checksumsFileID),
                        media = getOption("reproducible.cloudChecksumsFilename"),
                        verbose = FALSE)
  } else {
    if (any(grepl("403", uploadRes))) {
      message("No write access to cloudFolderID; not uploading cached copy")
    }
  }
  return(invisible(res))
}

.CacheDigestWithOmitArgs <- function(sysCallWCloudCache, envir) {
  a <- match.call(Cache, sysCallWCloudCache, expand.dots = TRUE)[-1]
  forms <- formalArgs(cloudCache)
  b <- match.call(eval(a$FUN), a, expand.dots = TRUE)
  b[names(b) %in% eval(b$omitArgs) | names(b) %in% forms | names(b) %in% names(.formalsCache)] <- NULL
  dots <- lapply(b, eval, envir = envir)

}

cloudDownload <- function(checksums) {
  if (NROW(checksums) > 1) {
    stop("Can only download 1 file at a time, currently")
  }
  objectFilename2 <- tempfile(fileext = ".rda");
  a <- checksums$filesize
  class(a) <- "object_size"
  message("  downloading object from google drive; this could take a while; it is: ",
          format(a, "auto"))
  drive_download(as_id(checksums[, id]), path = objectFilename2, verbose = FALSE)
  env <- new.env()
  load(objectFilename2, envir = env)
  as.list(env)[[1]]

}
