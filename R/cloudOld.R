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
#'    be written. This will only be used if `checksumsFileID` is not provided
#'   provided as a character string.
#' @export
#' @seealso [cloudSyncCacheOld()], [Cache()], [cloudWriteOld()]
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
#' @param digest The cacheId of the input arguments, outputted from `cloudCheckOld`
#' @param checksums A `data.table` that is outputted from `cloudCheckOld` that
#'   is the the checksums file
#' @param cloudFolderID The google folder ID where a new object should be written
#' @param futurePlan Which `future::plan` to use. Default:
#'    `getOption("reproducible.futurePlan")`
#'
#' @export
#' @importFrom data.table data.table rbindlist
#' @seealso [cloudSyncCacheOld()], [cloudCheckOld()]
cloudWriteOld <- function(object, digest, cloudFolderID = NULL, checksums, checksumsFileID,
                       futurePlan = getOption("reproducible.futurePlan")) {
  .Deprecated("Cache", msg = "Please use the 'useCloud' and 'cloudFolderID' args in 'Cache' instead")
  return(invisible())
}

#' Deprecated
#'
#' \if{html}{\figure{lifecycle-defunct.svg}{options: alt="defunct"}}
#'
#' Please use `Cache`, with args `useCloud` and `cloudFolderID`.
#'
#' @param ... Passed to [Cache()]
#'
#' @export
#' @importFrom data.table setattr
#' @rdname Deprcated
#' @seealso [cloudSyncCacheOld()], [Cache()], [cloudWriteOld()],
#'   [cloudCheckOld()]
cloudCache <- function(...) {
  .Deprecated("Cache", msg = "Please use the 'useCloud' and 'cloudFolderID' args in 'Cache' instead")
  return(invisible())
}

#' Sync cloud with local Cache
#'
#' This is still experimental, see examples.
#'
#' @details
#' `cloudSyncCacheOld` will remove any entries in a cloudCache that are not in a
#'
#' @inheritParams Cache
#' @inheritParams cloudCache
#' @inheritParams clearCache
#' @param cacheRepo See `x` in [showCache()]
#' @param checksumsFileID A google file ID where the checksums data.table is located,
#'   provided as a character string.
#' @param cacheIds If supplied, then only this/these cacheId objects
#'   will be uploaded or deleted. Default is `NULL`, meaning do
#'   full sync (i.e., match cloudFolder with local cacheRepo, constrained by
#'   `delete` or `upload`)
#' @param delete Logical. If `TRUE`, the default, it will delete any objects
#'   that are in `cloudFolderID` that are absent from local `cacheRepo`.
#'   If `FALSE`, it will not delete objects.
#' @param upload Logical. If `TRUE`, the default, it will upload any objects
#'   identified by the internal `showCache(...)` call. See examples. If `FALSE`,
#'   then no files will be uploaded. Can be used in conjunction with `delete`
#'   to create behaviours similar to `clearCache` and `keepCache`.
#' @param download Logical. If `FALSE`, the default, then the function will
#'   either delete the remote copy if `delete = TRUE` and there is no local
#'   copy, or upload the local copy if `upload = TRUE` and there is a local
#'   copy. If `TRUE`, then this will override `delete`, and download
#'   to local machine if it exists remotely.
#' @param ... Passed to `showCache` to get the artifacts to delete.
#'
#' @export
#' @importFrom data.table data.table rbindlist
#' @seealso [cloudCache()], [Cache()], [cloudWriteOld()],
#'   [cloudCheckOld()]
cloudSyncCacheOld <- function(cacheRepo = getOption("reproducible.cachePath"),
                              checksumsFileID = NULL, cloudFolderID = NULL,
                              delete = TRUE, upload = TRUE, download = !delete,
                              ask = getOption("reproducible.ask"),
                              cacheIds = NULL, ...) {

  .Deprecated("Cache", msg = "Please use the 'useCloud' and 'cloudFolderID' args in 'Cache' instead")
  return(invisible())
}

