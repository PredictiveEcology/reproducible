#' Clear erroneous archivist artifacts
#'
#' \lifecycle{defunct}
#'
#' Stub artifacts can result from several causes. The most common being
#' erroneous removal of a file in the SQLite database. This can be caused
#' sometimes if an archive object is being saved multiple times by multiple
#' threads. This function will clear entries in the SQLite database which
#' have no actual file with data.
#'
#' @return Invoked for its side effect on the \code{repoDir}.
#'
#' @param repoDir A character denoting an existing directory of the repository for
#' which metadata will be returned. If \code{NULL} (default), it will use the
#' \code{repoDir} specified in \code{archivist::setLocalRepo}.
#'
#' @author Eliot McIntire
#' @export
#' @rdname clearStubArtifacts
#'
#' @examples
#' tmpDir <- file.path(tempdir(), "reproducible_examples", "clearStubArtifacts")
#'
#' lapply(c(runif, rnorm), function(f) {
#'   reproducible::Cache(f, 10, cacheRepo = tmpDir)
#' })
#'
#' # clear out any stub artifacts
#' showCache(tmpDir)
#'
#' file2Remove <- dir(CacheStorageDir(tmpDir), full.name = TRUE)[1]
#' file.remove(file2Remove)
#' showCache(tmpDir) # repository directory still thinks files are there
#'
#' # run clearStubArtifacts
#' suppressWarnings(clearStubArtifacts(tmpDir))
#' showCache(tmpDir) # stubs are removed
#'
#' # cleanup
#' clearCache(tmpDir, ask = FALSE)
#' unlink(tmpDir, recursive = TRUE)
#'
setGeneric("clearStubArtifacts", function(repoDir = NULL) {
  standardGeneric("clearStubArtifacts")
})

#' @export
#' @rdname clearStubArtifacts
#' @importFrom magrittr %>%
setMethod(
  "clearStubArtifacts",
  definition = function(repoDir) {
    .Deprecated()
  })
