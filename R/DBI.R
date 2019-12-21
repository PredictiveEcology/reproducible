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
#' @param cachePath A path describing the directory in which to create
#'   the database file(s)
#' @param drv A driver, passed to \code{dbConnect}
#' @param force Logical. Should it create a cache in the \code{cachePath},
#'   even if it already exists, overwriting.
#' # replaces archivist::createLocalRepo
#' @importFrom DBI dbConnect dbDisconnect dbWriteTable
#' @importFrom data.table data.table
#' @inheritParams DBI::dbConnect
#' @inheritParams DBI::dbWriteTable
#' @rdname cacheTools
createCache <- function(cachePath, drv = RSQLite::SQLite(),
                        conn = NULL, force = FALSE) {
  dbPath <- .sqliteFile(cachePath) # file.path(cachePath, "cache.db")
  alreadyExists <- .cacheIsACache(cachePath)
  if (alreadyExists && force == FALSE) {
    message("Cache already exists at ", cachePath, " and force = FALSE. Not creating new cache.")
    return(invisible(cachePath))
  }

  checkPath(cachePath, create = TRUE)
  checkPath(.cacheStorageDir(cachePath), create = TRUE)
  if (is.null(conn)) {
    conn <- dbConnectAll(drv, dir = cachePath)
    on.exit(dbDisconnect(conn))
  }
  dt <- .emptyCacheTable
  dbWriteTable(conn, "dt", dt, overwrite = TRUE,
               field.types = c(cacheId = "text", tagKey = "text",
                               tagValue = "text", createdDate = "text"))
}

#' @rdname cacheTools
#' @inheritParams Cache
#' @param cacheId The hash string representing the result of \code{.robustDigest}
#' @param obj The R object to save to the cache
#' @importFrom qs qsave
saveToCache <- function(cachePath, drv = RSQLite::SQLite(),
                        conn = NULL,
                        obj, userTags, cacheId) {
  if (is.null(conn)) {
    conn <- dbConnectAll(drv, dir = cachePath)
    on.exit(dbDisconnect(conn))
  }

  if (missing(userTags)) userTags = "otherFunctions"
  if (length(userTags) == 0) userTags = "otherFunctions"
  if (NCOL(userTags) > 1) {
    tagKey <- userTags$tagKey
    tagValue <- userTags$tagValue
  } else {
    tagKey <- sub(userTags, pattern = ":.*$", replacement = "")
    tagValue <- sub(userTags, pattern = "^[^:]*:", replacement = "")
  }

  outputToSaveIsList <- is(obj, "list") # is.list is TRUE for anything, e.g., data.frame. We only want "list"
  if (outputToSaveIsList) {
    rasters <- unlist(lapply(obj, is, "Raster"))
  } else {
    rasters <- is(obj, "Raster")
  }
  browser(expr = exists("aaaa"))
  if (any(rasters)) {
    atts <- attributes(obj)
    if (outputToSaveIsList) {
      obj[rasters] <- lapply(obj[rasters], function(x)
        .prepareFileBackedRaster(x, repoDir = cachePath, overwrite = FALSE, drv = drv))
    } else {
      obj <- .prepareFileBackedRaster(obj, repoDir = cachePath,
                                               overwrite = FALSE, drv = drv)
    }

    # have to reset all these attributes on the rasters as they were undone in prev steps
    setattr(obj, "tags", atts$tags)
    .setSubAttrInList(obj, ".Cache", "newCache", atts$.Cache$newCache)
    setattr(obj, "call", atts$call)

    if (!identical(attr(obj, ".Cache")$newCache, atts$.Cache$newCache))
      stop("attributes are not correct 6")
    if (!identical(attr(obj, "call"), atts$call))
      stop("attributes are not correct 7")
    if (!identical(attr(obj, "tags"), atts$tags))
      stop("attributes are not correct 8")

    if (!is.null(atts[["function"]])) {
      setattr(obj, "function", atts[["function"]])
      if (!identical(attr(obj, "function"), atts[["function"]]))
        stop("There is an unknown error 04")
    }
    # attr(obj, "function") <- attr(output, "function")

   #output <- obj
  }
  dt <- data.table("cacheId" = cacheId, "tagKey" = tagKey,
                   "tagValue" = tagValue, "createdDate" = as.character(Sys.time()))

  retry(dbWriteTable(conn, "dt", dt, append=TRUE, row.names = FALSE),
        retries = 15)
  qs::qsave(file = .cacheStoredFile(cachePath, cacheId), obj)

  return(obj)

}


#' @export
#' @rdname cacheTools
#' @importFrom qs qread
loadFromCache <- function(cachePath, cacheId) {
  qs::qread(file = .cacheStoredFile(cachePath, cacheId))
}

#' Low level tools to work with Cache
#'
#' @importFrom DBI dbClearResult dbSendStatement dbBind
#' @export
#' @rdname cacheTools
rmFromCache <- function(cachePath, cacheId, drv = RSQLite::SQLite(),
                        conn = NULL) {
  if (is.null(conn)) {
    conn <- dbConnectAll(drv, dir = cachePath, create = FALSE)
    on.exit(dbDisconnect(conn))
  }
  # from https://cran.r-project.org/web/packages/DBI/vignettes/spec.html
  query <- paste0("DELETE FROM dt WHERE \"cacheId\" = $1")
  res <- dbSendStatement(conn, query)
  dbBind(res, list(cacheId))

  dbClearResult(res)
  unlink(file.path(cachePath, "cacheObjects", paste0(cacheId, ".qs")))

}

dbConnectAll <- function(drv, dir, create = TRUE) {
  args <- list(drv = drv)
  if (is(drv, "SQLiteDriver")) {
    if (!.cacheIsACache(drv = drv, dir = dir))
      if (isFALSE(create)) {
        return(invisible())
      }
    args <- append(args, list(dbname = .sqliteFile(dir)))
  } # other types of drv, e.g., Postgres can be done via env vars
  do.call(dbConnect, args)
}

.emptyCacheTable <- data.table(cacheId = character(), tagKey = character(),
                               tagValue = character(), createdDate = character())

