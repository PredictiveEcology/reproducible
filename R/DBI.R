# importFrom(archivist,addTagsRepo)
# importFrom(archivist,cache)
# importFrom(archivist,createLocalRepo)
# importFrom(archivist,loadFromLocalRepo)
# importFrom(archivist,rmFromLocalRepo)
# importFrom(archivist,saveToLocalRepo)
# importFrom(archivist,searchInLocalRepo)
# importFrom(archivist,showLocalRepo)
# importFrom(archivist,splitTagsLocal)

#' Create a new cache
#'
#' @param cacheDir A path describing the directory in which to create
#'   the database file(s)
#' @param drv A driver, passed to \code{dbConnect}
#' @param force If
#' # replaces archivist::createLocalRepo
#' @importFrom DBI dbConnect dbDisconnect dbWriteTable
#' @importFrom data.table data.table
createCache <- function(cacheDir, drv = RSQLite::SQLite(), force = FALSE) {
  dbPath <- file.path(cacheDir, "cache.db")
  alreadyExists <- dir.exists(cacheDir) && file.exists(dbPath) && dir.exists(file.path(cacheDir, "cacheObjects"))
  if (alreadyExists && force == FALSE) {
    message("Cache already exists at ", cacheDir, " and force = FALSE. Not creating new cache.")
    return(invisible(cacheDir))
  }

  checkPath(cacheDir, create = TRUE)
  checkPath(file.path(cacheDir, "cacheObjects"), create = TRUE)
  con <- dbConnect(drv, dbname = file.path(cacheDir, "cache.db"))
  on.exit(dbDisconnect(con))
  dt <- .emptyCacheTable
  dbWriteTable(con, "dt", dt, overwrite = TRUE, field.types = c(createdDate = "timestamp"))
}

saveToCache <- function(cacheDir, drv = RSQLite::SQLite(),
                        outputToSave, userTags, cacheId) {
  con <- dbConnect(drv, dbname = file.path(cacheDir, "cache.db"))
  on.exit(dbDisconnect(con))

  if (missing(userTags)) userTags = "otherFunctions"
  tagKey <- sub(userTags, pattern = ":.*$", replacement = "")
  tagValue <- sub(userTags, pattern = "^[^:]*:", replacement = "")

  #saveRDS(file = file.path(cacheDir, "cacheObjects", paste0(cacheId, ".rds")),
  #        outputToSave)

  browser(expr = exists("aaaa"))
  outputToSaveIsList <- is(outputToSave, "list") # is.list is TRUE for anything, e.g., data.frame. We only want "list"
  if (outputToSaveIsList) {
    rasters <- unlist(lapply(outputToSave, is, "Raster"))
  } else {
    rasters <- is(outputToSave, "Raster")
  }
  browser(expr = exists("aaaa"))
  if (any(rasters)) {
    atts <- attributes(outputToSave)
    if (outputToSaveIsList) {
      outputToSave[rasters] <- lapply(outputToSave[rasters], function(x)
        .prepareFileBackedRaster(x, repoDir = cacheDir, overwrite = FALSE))
    } else {
      outputToSave <- .prepareFileBackedRaster(outputToSave, repoDir = cacheDir,
                                               overwrite = FALSE)
    }

    # have to reset all these attributes on the rasters as they were undone in prev steps
    setattr(outputToSave, "tags", atts$tags)
    .setSubAttrInList(outputToSave, ".Cache", "newCache", atts$.Cache$newCache)
    setattr(outputToSave, "call", atts$call)

    if (!identical(attr(outputToSave, ".Cache")$newCache, atts$.Cache$newCache))
      stop("attributes are not correct 6")
    if (!identical(attr(outputToSave, "call"), atts$call))
      stop("attributes are not correct 7")
    if (!identical(attr(outputToSave, "tags"), atts$tags))
      stop("attributes are not correct 8")

    if (!is.null(atts[["function"]])) {
      setattr(outputToSave, "function", atts[["function"]])
      if (!identical(attr(outputToSave, "function"), atts[["function"]]))
        stop("There is an unknown error 04")
    }
    # attr(outputToSave, "function") <- attr(output, "function")

   #output <- outputToSave
  }
  dt <- data.table("cacheId" = cacheId, "tagKey" = tagKey,
                   "tagValue" = tagValue, "createdDate" = as.character(Sys.time()))

  retry(dbWriteTable(con, "dt", dt, append=TRUE, row.names = FALSE), retries = 15)
  qs::qsave(file = file.path(cacheDir, "cacheObjects", paste0(cacheId, ".qs")),
            outputToSave)

  return(outputToSave)

}


loadFromCache <- function(cachePath, cacheId) {
  qs::qread(file = file.path(cachePath, "cacheObjects", paste0(cacheId, ".qs")))
  #readRDS(file.path(cachePath, "cacheObjects", paste0(cacheId, ".rds")))
}

rmFromCache <- function(cachePath, cacheId, con, drv) {
  if (missing(con)) {
    con <- dbConnect(drv, dbname = file.path(cacheDir, "cache.db"))
    on.exit(dbDisconnect(con))
  }
  res <- DBI::dbSendQuery(con, paste0("DELETE FROM dt WHERE cacheId = '", cacheId, "'"))
  dbClearResult(res)
  unlink(file.path(cachePath, "cacheObjects", paste0(cacheId, ".qs")))


}
# saveToLocalRepo(
#   outputToSave,
#   repoDir = cacheRepo,
#   artifactName = NULL,
#   archiveData = FALSE,
#   archiveSessionInfo = FALSE,
#   archiveMiniature = FALSE,
#   rememberName = FALSE,
#   silent = TRUE,
#   userTags = userTags
# )
#
# unction (repoDir, force = FALSE, default = FALSE)
# {
#   stopifnot(is.character(repoDir), length(repoDir) == 1)
#   stopifnot(is.logical(default), length(default) == 1)
#   if (file.exists(repoDir) & file.exists(paste0(repoDir, "/backpack.db")) &
#       !force) {
#     message(paste0("Directory ", repoDir, " does exist and contain the backpack.db file. Use force=TRUE to reinitialize."))
#     return(invisible(repoDir))
#   }
#   if (file.exists(repoDir) & file.exists(paste0(repoDir, "/backpack.db")) &
#       force) {
#     message(paste0("Directory ", repoDir, " does exist and contain the backpack.db file. Reinitialized due to force=TRUE."))
#   }
#   if (!file.exists(repoDir)) {
#     dir.create(repoDir)
#   }
#   backpack <- getConnectionToDB(repoDir)
#   artifact <- data.frame(md5hash = "", name = "",
#                          createdDate = as.character(now()), stringsAsFactors = FALSE)
#   tag <- data.frame(artifact = "", tag = "", createdDate = as.character(now()),
#                     stringsAsFactors = FALSE)
#   dbWriteTable(backpack, "artifact", artifact, overwrite = TRUE,
#                row.names = FALSE)
#   dbWriteTable(backpack, "tag", tag, overwrite = TRUE,
#                row.names = FALSE)
#   dbExecute(backpack, "delete from artifact")
#   dbExecute(backpack, "delete from tag")
#   dbDisconnect(backpack)
#   if (!file.exists(file.path(repoDir, "gallery"))) {
#     dir.create(file.path(repoDir, "gallery"), showWarnings = FALSE)
#   }
#   if (default) {
#     setLocalRepo(repoDir)
#   }
#   return(invisible(repoDir))
# }
# <bytecode: 0x000001a4f35f1750>
#   <environment: namespace:archivist>
#   >
#
#   
#
# 

.emptyCacheTable <- data.table(cacheId = character(), tagKey = character(),
                               tagValue = character(), createdDate = character())

