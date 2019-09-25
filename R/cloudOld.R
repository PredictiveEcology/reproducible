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
  .Deprecated("Cache", msg = "Please use the 'useCloud' and 'cloudFolderID' args in 'Cache' instead")
  return(invisible())
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
  .Deprecated("Cache", msg = "Please use the 'useCloud' and 'cloudFolderID' args in 'Cache' instead")
  return(invisible())
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
cloudCache <- function(...) {
  .Deprecated("Cache", msg = "Please use the 'useCloud' and 'cloudFolderID' args in 'Cache' instead")
  return(invisible())
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
cloudSyncCacheOld <- function(cacheRepo = getOption("reproducible.cachePath"),
                              checksumsFileID = NULL, cloudFolderID = NULL,
                              delete = TRUE, upload = TRUE, download = !delete,
                              ask = getOption("reproducible.ask"),
                              cacheIds = NULL, ...) {

  .Deprecated("Cache", msg = "Please use the 'useCloud' and 'cloudFolderID' args in 'Cache' instead")
  return(invisible())
}

