#' Create a new cache
#'
#' @param cacheDir A path describing the directory in which to create
#'   the database file(s)
#' @param drv A driver, passed to \code{dbConnect}
#' @param force If
#' # replaces archivist::createLocalRepo
createCache <- function(cacheDir, drv = RSQLite::SQLite(), force = FALSE) {
  dbPath <- file.path(cacheDir, "cache.db")
  alreadyExists <- dir.exists(cacheDir) && file.exists(dbPath)
  if (alreadyExists && force == FALSE) {
    message("Cache already exists at ", cacheDir, " and force = FALSE. Not creating new cache.")
    return(invisible(cacheDir))
  }

  checkPath(cacheDir, create = TRUE)
  checkPath(file.path(cacheDir, "cacheObjects"), create = TRUE)
  con <- dbConnect(drv, dbname = file.path(cacheDir, "cache.db"))
  on.exit(dbDisconnect(con))
  dt <- data.table("cacheId", "tagKey", "tagValue", "createdDate")
  dbWriteTable(con, "dt", dt, overwrite = TRUE)
}

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
