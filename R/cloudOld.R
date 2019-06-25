if (getRversion() >= "3.1.0") {
  utils::globalVariables(c("cacheId", "checksumsFilename", "checksumsID", "id"))
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
#' @seealso \code{\link{cloudSyncCacheOld}}, \code{\link{Cache}}, \code{\link{cloudWriteOld}}
cloudCheckOld <- function(toDigest, checksumsFileID = NULL, cloudFolderID = NULL) {
  dig <- CacheDigest(toDigest)$outputHash
  if (is.null(checksumsFileID)) {
    if (!is.null(cloudFolderID)) {
      checksumsFileID <- getChecksumsFileIDOld(cloudFolderID)
    } else {
      stop("checksumsFileID must be supplied, or else cloudFolderID to create a new one")
    }
  }

  suppressMessages(checksums <- cloudDownloadChecksumsOld(checksumsFileID))
  hashExists <- checksums$cacheId == dig
  out <- if (isTRUE(any(hashExists))) {
    if (sum(hashExists) > 1) {
      stop("running cloudCheckOld with >1 object toDigest is not implemented yet;",
           "contact eliot.mcintire@canada.ca")
    }
    cloudDownloadOld(checksums[hashExists])[[1]]
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
#' @inheritParams cloudCheckOld
#' @param object The R object to write to cloud
#' @param digest The cacheId of the input arguments, outputted from \code{cloudCheckOld}
#' @param checksums A \code{data.table} that is outputted from \code{cloudCheckOld} that
#'   is the the checksums file
#' @param cloudFolderID The google folder ID where a new object should be written
#' @param futurePlan Which \code{future::plan} to use. Default:
#'    \code{getOption("reproducible.futurePlan")}
#'
#' @export
#' @importFrom data.table data.table rbindlist
#' @importFrom googledrive as_id drive_update drive_upload
#' @seealso \code{\link{cloudSyncCacheOld}}, \code{\link{cloudCheckOld}}, \code{\link{cloudExtras}}
cloudWriteOld <- function(object, digest, cloudFolderID = NULL, checksums, checksumsFileID,
                       futurePlan = getOption("reproducible.futurePlan")) {
  if (!is.null(cloudFolderID)) {
    objectFile <- tempfile(fileext = ".rda")
    save(object, file = objectFile)
    # checksums <- cloudDownloadChecksumsOld(checksumsFileID)
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
                           cloudUploadFileAndChecksumsOld(objectFile, cloudFolderID, digest,
                                                       checksums, checksumsFileID))
    } else {
      message(msgUpload)
      cloudUploadFileAndChecksumsOld(objectFile, cloudFolderID, digest,
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
#' \code{cloudDownloadChecksumsOld} gets the checksums data.table directly.
#'
#' @inheritParams cloudCheckOld
#'
#' @aliases cloudExtras
#' @export
#' @importFrom googledrive as_id drive_download
#' @rdname cloudExtras
#' @seealso \code{\link{cloudSyncCacheOld}}, \code{\link{cloudCache}}, \code{\link{cloudExtras}}
cloudDownloadChecksumsOld <- function(checksumsFileID = NULL, cloudFolderID = NULL) {

  if (is.null(checksumsFileID))
    checksumsFileID <- getChecksumsFileIDOld(cloudFolderID)

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
  return(checksums)
}

#' @details
#' \code{cloudDownloadChecksumsOld} gets the checksums data.table directly.
#'
#' @inheritParams cloudWriteOld
#'
#' @export
#' @importFrom googledrive as_id drive_update
#' @rdname cloudExtras
cloudUpdateChecksumsOld <- function(checksums, checksumsFileID) {
  saveRDS(checksums, file = getOption("reproducible.cloudChecksumsFilename"))
  drive_update(as_id(checksumsFileID), media = getOption("reproducible.cloudChecksumsFilename"),
               verbose = FALSE)
}

#' Deprecated
#'
#' Please use \code{Cache}, with args \code{useCloud} and \code{cloudFolderID}.
#'
#' @inheritParams cloudCheckOld
#' @inheritParams cloudWriteOld
#' @param ... Passed to \code{\link{Cache}}
#'
#' @export
#' @importFrom archivist createLocalRepo saveToLocalRepo
#' @importFrom data.table setattr
#' @rdname Deprcated
#' @seealso \code{\link{cloudSyncCacheOld}}, \code{\link{Cache}}, \code{\link{cloudWriteOld}},
#'   \code{\link{cloudCheckOld}}, \code{\link{cloudExtras}}
cloudCache <- function(...) Cache(...)

#' @importFrom crayon blue
#' @importFrom data.table data.table
#' @importFrom googledrive as_id drive_ls drive_upload
#' @keywords internal
getChecksumsFileIDOld <- function(cloudFolderID) {
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
#' \code{cloudSyncCacheOld} will remove any entries in a cloudCache that are not in a
#'
#' @inheritParams Cache
#' @inheritParams cloudCache
#' @inheritParams clearCache
#' @param cacheRepo See \code{x} in \code{\link{showCache}}
#' @param checksumsFileID A google file ID where the checksums data.table is located,
#'   provided as a character string.
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
#' @param ... Passed to \code{showCache} to get the artifacts to delete.
#'
#' @export
#' @importFrom crayon blue
#' @importFrom data.table data.table rbindlist
#' @importFrom googledrive as_id drive_rm drive_upload
#' @seealso \code{\link{cloudCache}}, \code{\link{Cache}}, \code{\link{cloudWriteOld}},
#'   \code{\link{cloudCheckOld}}, \code{\link{cloudExtras}}
#' @examples
#' \dontrun{
#'   #make a google drive folder
#'   #   Can use >1 cacheRepo
#'   opts <- options("reproducible.cachePath" = c(tempdir()),
#'                   "reproducible.ask" = FALSE)
#'   cachePaths <- getOption("reproducible.cachePath")
#'   library(googledrive)
#'   newDir <- drive_mkdir("testFolder")
#'   #a <- Cache(rnorm, 1, cacheRepo = getOption("reproducible.cachePath")[3])
#'   a <- Cache(rnorm, 1)
#'   b <- Cache(rnorm, 2)
#'
#'   # Will copy the 2 to the cloud
#'   cloudSyncCacheOld(cloudFolderID = newDir$id)
#'
#'   # remove a local one
#'   clearCache(userTags = CacheDigest(list(rnorm, 2))$outputHash)
#'
#'   # Now will delete the object in the cloud that was just deleted locally
#'   cloudSyncCacheOld(cloudFolderID = newDir$id)
#'
#'   # clean up
#'   lapply(cachePaths, clearCache, ask = FALSE)
#'   # clearCache(ask = FALSE) # if there were only 1 cacheRepo
#'   cloudSyncCacheOld(cloudFolderID = newDir$id)
#'
#'   #######################################################################
#'   # use showCache args to have control ... on upload & delete NOTE difference!
#'   #######################################################################
#'   # a <- Cache(rnorm, 1, cacheRepo = getOption("reproducible.cachePath")[3]) # multiple cacheRepos!
#'   a <- Cache(rnorm, 1)
#'   b <- Cache(rnorm, 2)
#'   # only sync the one with rnorm, 2 as arguments
#'   #   This CacheDigest is the same algorithm used by Cache
#'   tag <- CacheDigest(list(rnorm, 2))$outputHash
#'   cloudSyncCacheOld(cloudFolderID = newDir$id, userTags = tag) # only syncs the one
#'                                                             # that is identified
#'                                                             # with userTags
#'
#'   cloudSyncCacheOld(cloudFolderID = newDir$id) # sync any other ones
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
#'   #    cloudSyncCacheOld(cloudFolderID = newDir$id, userTags = tag)
#'
#'   # Only delete the one that was removed from local cache, set upload = FALSE,
#'   #    leaving only 1 in cloud: a  -- this is still a sync, so, it will only
#'   #    delete 1 file because local has 1 few files -- see next for just deleting 1 artifact
#'   cloudSyncCacheOld(cloudFolderID = newDir$id, upload = FALSE)
#'   # Upload the d, because it is the only one in the localCache not in the cloudCache
#'   cloudSyncCacheOld(cloudFolderID = newDir$id)
#'
#'   f <- Cache(rnorm, 5)
#'   g <- Cache(rnorm, 6)
#'   # upload both
#'   cloudSyncCacheOld(cloudFolderID = newDir$id) # only syncs the one
#'   tag5 <- CacheDigest(list(rnorm, 5))$outputHash # this is the same algorithm used by Cache
#'   tag6 <- CacheDigest(list(rnorm, 6))$outputHash
#'   clearCache(userTags = tag5)
#'   clearCache(userTags = tag6)
#'   # delete only one by tag
#'   cloudSyncCacheOld(cloudFolderID = newDir$id, cacheIds = tag5) # will delete only this obj in cloud
#'   # delete another one by tag
#'   cloudSyncCacheOld(cloudFolderID = newDir$id, cacheIds = tag6)
#'
#'   # clean up
#'   # clearCache(ask = FALSE) # if only one cacheRepo
#'   lapply(cachePaths, clearCache, ask = FALSE)
#'   cloudSyncCacheOld(cloudFolderID = newDir$id)
#'
#'   # To remove whole folder:
#'   drive_rm(as_id(newDir$id))
#'   options(opts)
#' }
cloudSyncCacheOld <- function(cacheRepo = getOption("reproducible.cachePath"),
                              checksumsFileID = NULL, cloudFolderID = NULL,
                              delete = TRUE, upload = TRUE, download = !delete,
                              ask = getOption("reproducible.ask"),
                              cacheIds = NULL, ...) {

  .Deprecated("Cache", msg = "Please use the 'useCloud' and 'cloudFolderID' args in 'Cache' instead")

  if (isTRUE(download)) {
    if (isTRUE(delete)) {
      delete <- FALSE
      message("download is TRUE; setting delete to FALSE")
    }
  }
  if (is.null(checksumsFileID)) {
    if (!is.null(cloudFolderID)) {
      checksumsFileID <- getChecksumsFileIDOld(cloudFolderID)
    } else {
      stop("checksumsFileID must be supplied, or else cloudFolderID to create a new one")
    }
  }

  suppressMessages(checksums <- cloudDownloadChecksumsOld(checksumsFileID))

  # get cacheRepo if not supplied
  names(cacheRepo) <- normPath(cacheRepo)
  cacheRepos <- lapply(cacheRepo, function(repo) {
      repo <- checkPath(repo, create = TRUE)
    })

  # cacheRepo <- cacheRepos[[1]]
  suppressMessages(localCaches <- lapply(cacheRepos, ..., function(cacheRepo, ...) {
    tryCatch(showCache(x = cacheRepo, ...), error = function(x) NULL)
  }))
  #suppressMessages(localCache <- showCache(x = cacheRepo, ...))
  out <- NULL

  needToDelete <- NULL
  uniqueCacheArtifacts <- lapply(localCaches, function(x) unique(x$artifact))
  cacheIDs <- lapply(cacheRepos, function(uca) {
    if (!is.null(uniqueCacheArtifacts[[uca]]))
      localCaches[[uca]][artifact %in% uniqueCacheArtifacts[[uca]] & tagKey == "cacheId"]$tagValue

  })

  if (!is.null(cacheIds)) {
    cacheIDs <- lapply(cacheIDs, function(cacheIDsInner) {
      cacheIdsExisting <- which(cacheIDsInner %in% cacheIds)
      cacheIDsInner[cacheIdsExisting]
    })
    if (all(unlist(lapply(cacheIDs, function(x) length(x) == 0)))) {
      message("None of the supplied cacheIds exist locally: \n  ", paste(cacheIds, sep = "\n  "))
    }

  }

  ########################################################
  # Upload to the Cloud
  ########################################################
  if ( any(unlist(lapply(localCaches, NROW))>0) ) {
    needToUpload <- !(unlist(cacheIDs) %in% checksums$cacheId)
    if (isTRUE(upload)) {
      if (sum(needToUpload) > 0) {
        df <- data.frame(repo = rep(names(uniqueCacheArtifacts), sapply(uniqueCacheArtifacts, length)),
                   artifact = unname(unlist(uniqueCacheArtifacts)), stringsAsFactors = FALSE)
        message(crayon::blue("Uploading", sum(needToUpload), "files, ",
                             "\n ", paste(unlist(cacheIDs)[needToUpload], ".rda ( == ",
                                          unlist(uniqueCacheArtifacts)[needToUpload],
                                          ".rda in local cache)", sep = "",
                                          collapse = "\n  ")))
        whCache <- match(df$repo, unlist(cacheRepos ))
        uniqueOnes <- df[needToUpload,]
        out <- lapply(seq(NROW(uniqueOnes)), function(artSeq) {
          repo <- uniqueOnes[artSeq, "repo"]
          art <- uniqueOnes[artSeq, "artifact"]
          cacheID <- localCaches[[repo]][artifact == art & tagKey == "cacheId"]$tagValue
          drive_upload(file.path(repo, "gallery", paste0(art, ".rda")),
                       path = as_id(cloudFolderID),
                       name = paste0(cacheID, ".rda"),
                       verbose = FALSE)
        })
        checksums <- rbindlist(
          list(checksums,
               data.table(
                 cacheId = unlist(cacheIDs)[needToUpload],
                 id = rbindlist(out)$id,
                 time = as.character(Sys.time()),
                 filesize = file.size(file.path(df[needToUpload, "repo"], "gallery",
                                                paste0(df[needToUpload, "artifact"], ".rda")))
               )
          ),
          use.names = TRUE, fill = TRUE)
        out <- rbindlist(out)
      }
    } else {
      if (length(needToUpload) > 0)
        message("Need to upload, but upload = FALSE: \n  ",
                paste(unlist(cacheIDs)[needToUpload]))
    }

  } else {
    if (NROW(rbindlist(localCaches, use.names = TRUE, fill = TRUE)) == 0) {
      needToDelete <- rep(TRUE, NROW(checksums))
    }
  }

  ########################################################
  # Delete in the Cloud
  ########################################################
  if (is.null(needToDelete)) {
    if (!is.null(unlist(uniqueCacheArtifacts))) {
      needToDelete <- if (!is.null(cacheIds)) {
        out <- (checksums$cacheId %in% cacheIds)
        if (all(!out)) {
          message("That/those cacheId(s) are not in the cloudFolderID. Nothing to delete.")
        }
        out
      } else {
        !(checksums$cacheId %in% unlist(cacheIDs))
      }

    }
  }

  if (isTRUE(delete) && any(needToDelete)) { # there are 1 or more to delete, and delete is TRUE
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
      needToDownload <- needToDelete # sync when local is missing -- either delete the cloud or download from cloud
      if (isTRUE(download)) {
        message("Object in cloud, not in local; delete = FALSE, download = TRUE; downloading...: \n  ",
                paste(checksums$cacheId[needToDownload], ".rda",
                      sep = "", collapse = "\n  "))
        cacheIdsToDownload <- checksums$cacheId[needToDownload]
        outs <- cloudDownloadOld(checksums[needToDownload,])
        suppressMessages(cacheRepo <- .checkCacheRepo(cacheRepos[1], create = TRUE)) # write to 1st one
        message("  Writing to local cacheRepo")
        saved <- lapply(outs, function(out) {
          objSize <- sum(unlist(objSize(out)))
          userTags <- c(#if (!is.na(fnDetails$functionName))
            #paste0("function:", fnDetails$functionName),
            paste0("object.size:", objSize),
            paste0("accessed:", Sys.time())#,
            #paste("preDigest", names(preDigestUnlistTrunc),
            #      preDigestUnlistTrunc, sep = ":")
          )

          written <- 0
          setattr(out, "tags", paste0("cacheId:", checksums[needToDownload, cacheId]))
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

        })
      } else {
        message("There were files identified to delete to create full sync, but delete = FALSE")
        #message("  cacheIds already present locally")
      }
    }
  }
  message("uploading updated checksums file on googledrive")
  suppressMessages(cloudUpdateChecksumsOld(checksums, checksumsFileID))
  return(invisible(checksums))
}

cloudUploadFileAndChecksumsOld <- function(objectFile, cloudFolderID, digest,
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

cloudDownloadOld <- function(checksums) {
  out <- lapply(seq(NROW(checksums)), function(checksumsIndex) {
    objectFilename2 <- tempfile(fileext = ".rda");
    a <- checksums[checksumsIndex,]$filesize
    class(a) <- "object_size"
    message("  downloading object from google drive; it is: ",
            format(a, "auto"))
    drive_download(as_id(checksums[checksumsIndex, id]), path = objectFilename2, verbose = FALSE)
    env <- new.env()
    load(objectFilename2, envir = env)
    as.list(env)[[1]]
  })

}
