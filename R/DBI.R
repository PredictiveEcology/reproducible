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
  browser(expr = exists("aaaa"))
  alreadyExists <- CacheIsACache(cachePath, drv = drv)
  if (alreadyExists && force == FALSE) {
    message("Cache already exists at ", cachePath, " and force = FALSE. Not creating new cache.")
    return(invisible(cachePath))
  }

  checkPath(cachePath, create = TRUE)
  checkPath(CacheStorageDir(cachePath), create = TRUE)
  if (is.null(conn)) {
    conn <- dbConnectAll(drv, cachePath = cachePath)
    on.exit(dbDisconnect(conn))
  }
  dt <- .emptyCacheTable

  retry(
    dbWriteTable(conn, CacheDBTableName(cachePath, drv), dt, overwrite = TRUE,
                 field.types = c(cacheId = "text", tagKey = "text",
                                 tagValue = "text", createdDate = "text"))
  )
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
    conn <- dbConnectAll(drv, cachePath = cachePath)
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

  retry(dbWriteTable(conn, CacheDBTableName(cachePath, drv), dt, append=TRUE, row.names = FALSE),
        retries = 15)
  qs::qsave(file = CacheStoredFile(cachePath, cacheId), obj)

  return(obj)

}


#' @export
#' @rdname cacheTools
#' @importFrom qs qread
loadFromCache <- function(cachePath, cacheId) {
  qs::qread(file = CacheStoredFile(cachePath, cacheId))
}

#' Low level tools to work with Cache
#'
#' @importFrom DBI dbClearResult dbSendStatement dbBind
#' @export
#' @rdname cacheTools
rmFromCache <- function(cachePath, cacheId, drv = RSQLite::SQLite(),
                        conn = NULL) {
  if (is.null(conn)) {
    conn <- dbConnectAll(drv, cachePath = cachePath, create = FALSE)
    on.exit(dbDisconnect(conn))
  }
  # from https://cran.r-project.org/web/packages/DBI/vignettes/spec.html
  query <- paste0("DELETE FROM ",CacheDBTableName(cachePath, drv),
                  " WHERE \"cacheId\" = $1")

  res <- retry({dbSendStatement(conn, query)})
  browser(expr = is(res, "try-error"))
  retry(dbBind(res, list(cacheId)))

  dbClearResult(res)
  unlink(file.path(cachePath, "cacheObjects", paste0(cacheId, ".qs")))

}

dbConnectAll <- function(drv = RSQLite::SQLite(), cachePath,
                         create = TRUE) {
  args <- list(drv = drv)
  browser(expr = exists("yyyy"))
  if (is(drv, "SQLiteDriver")) {
    # if (!CacheIsACache(cachePath = cachePath, drv = drv, conn = conn))
    #   if (isFALSE(create)) {
    #     return(invisible())
    #   }
    args <- append(args, list(dbname = CacheDBFile(cachePath, drv)))
  } # other types of drv, e.g., Postgres can be done via env vars
  do.call(dbConnect, args)
}

.emptyCacheTable <- data.table(cacheId = character(), tagKey = character(),
                               tagValue = character(), createdDate = character())


.addTagsRepo <- function(isInRepo, cachePath, lastOne,
                         drv = RSQLite::SQLite(), conn = NULL) {
  browser(expr = exists("xxxx"))
  if (getOption("reproducible.newAlgo", TRUE)) {
    if (is.null(conn)) {
      conn <- dbConnectAll(drv, cachePath = cachePath, create = FALSE)
      on.exit(dbDisconnect(conn))
    }

    rs <- retry(DBI::dbSendStatement(conn, paste0("insert into ",CacheDBTableName(cachePath, drv),
                                      " (cacheId, tagKey, tagValue, createdDate) values ",
                                      "('", isInRepo$cacheId[lastOne],
                                      "', 'accessed', '", as.character(Sys.time()), "', '", as.character(Sys.time()), "')")),
                retries = 15)

    # executeSingleSilentQuery(dir, paste0("insert into tag (artifact, tag, createdDate) values ",
    #                                      "('", md5hash, "', '", gsub(tag, pattern = "'", replacement = ""),
    #                                      "', '", as.character(now()), "')"))
    #
    # rs <- dbSendStatement(con,
    #                       "INSERT INTO cars (speed, dist) VALUES (1, 1), (2, 2), (3, 3);")
    # dbHasCompleted(rs)
    # dbGetRowsAffected(rs)
    dbClearResult(rs)

    #dt <- data.table("cacheId" = isInRepo$cacheId[lastOne], "tagKey" = "accessed",
    #                 "tagValue" = as.character(Sys.time()), "createdDate" = as.character(Sys.time()))

    # retry(dbAppendTable(conn, CacheDBTableName(cachePath, drv), dt), retries = 15)

  } else {

    written <- 0
    while (written >= 0) {
      saved <- suppressWarnings(try(silent = TRUE,
                                    addTagsRepo(isInRepo[[.cacheTableHashColName()]][lastOne],
                                                repoDir = cachePath,
                                                tags = paste0("accessed:", Sys.time()))))
      written <- if (is(saved, "try-error")) {
        Sys.sleep(sum(runif(written + 1,0.05, 0.1)))
        written + 1
      } else {
        -1
      }
    }
  }

}

.cacheNumDefaultTags <- function() {
  if (getOption("reproducible.newAlgo", TRUE))
    5
  else
    10
}

.cacheTableHashColName <- function() {
  if (getOption("reproducible.newAlgo", TRUE)) {
    "cacheId"
  } else {
    "artifact"
  }
}

.cacheTableTagColName <- function(option = NULL) {
  out <- "tagValue"
  if (getOption("reproducible.newAlgo", TRUE)) {
  } else {
    if (isTRUE(option == "tag")) {
      out <- "tag"
    }
  }
  out
}

#' A collection of low level tools for Cache
#'
#' These are not intended for normal use.
#'
#' @inheritParams Cache
#' @inheritParams createCache
#' @rdname CacheHelpers
#' @export
#' @details
#' \code{CacheStoredFile} returns the file path to the file with the specified hash value.
CacheDBFile <- function(cachePath, drv = RSQLite::SQLite()) {
  if (is(drv, "SQLiteDriver")) {

    if (getOption("reproducible.newAlgo", TRUE)) {
      file.path(cachePath, "cache.db")
    } else {
      file.path(cachePath, "backpack.db")
    }
  } else {
    file.path(cachePath, "cache.txt")
  }

}

#' @rdname CacheHelpers
#' @export
CacheStorageDir <- function(cachePath) {
  if (getOption("reproducible.newAlgo", TRUE)) {
    file.path(cachePath, "cacheOutputs")
  } else {
    file.path(cachePath, "gallery")
  }
}

#' @details
#' \code{CacheStoredFile} returns the file path to the file with the specified hash value.
#'
#' @rdname CacheHelpers
#' @export
#' @param hash The cacheId or otherwise digested hash value, as character string.
CacheStoredFile <- function(cachePath, hash) {
  csf <- if (isTRUE(getOption("reproducible.newAlgo", TRUE)) == FALSE) {
    "rda"
  } else {
    getOption("reproducible.cacheSaveFormat", "qs")
  }
  csExtension <- if (csf == "qs") {
    "qs"
  } else if (csf == "rds") {
    "rds"
  } else {
    "rda"
  }
  filename <- paste(hash, csExtension, sep = ".")
  file.path(CacheStorageDir(cachePath), filename)
}

#' @rdname CacheHelpers
#' @export
CacheDBTableName <- function(cachePath, drv = RSQLite::SQLite()) {
  if (!is(cachePath, "Path")) {
    cachePath <- asPath(cachePath, nParentDirs = 2)
  }
  if (getOption("reproducible.newAlgo", TRUE)) {
    toGo <- attr(cachePath, "nParentDirs")
    cachePathTmp <- normPath(cachePath)
    newPath <- basename2(cachePathTmp)
    while(toGo > 1) {
      toGo <- toGo - 1
      cachePathTmp <- dirname(cachePathTmp)
      newPath <- paste(basename2(cachePathTmp), newPath, sep = "_")
    }
  } else {
    newPath <- "dt"
  }
  # SQLite can't handle numbers as initial character of a table name
  if (grepl("^[[:digit:]]", newPath)) {newPath <- paste0("_", newPath)}
  return(newPath)
}

#' @rdname CacheHelpers
#' @param create Logical. Currently only affects non RQSLite default drivers. If this
#'   is \code{TRUE} and there is no Cache database, the function will create one.
#' @importFrom DBI dbListTables
#' @export
#' @details
#' \code{CacheIsACache} returns a logical of whether the specified cachePath
#'   is actually a functioning cache.
CacheIsACache <- function(cachePath, create = FALSE, drv = RSQLite::SQLite(), conn = NULL) {
  if (is.null(conn)) {
    conn <- dbConnectAll(drv, cachePath = cachePath)
    on.exit(dbDisconnect(conn))
  }
  # Can't use conn here as it will become infinite recursion

  ret <- FALSE
  if (is(drv, "SQLiteDriver")) {
    browser(expr = exists("aaaa"))
    ret <- all(basename2(c(CacheDBFile(cachePath, drv), CacheStorageDir(cachePath))) %in%
                 list.files(cachePath))
  } # other types of drv, e.g., Postgres can be done via env vars
  if (is(drv, "PqDriver")) {
    browser(expr = exists("jjjj"))
    ret <- all(basename2(c(CacheDBFile(cachePath, drv), CacheStorageDir(cachePath))) %in%
                 list.files(cachePath))
  }
  ret <- ret && (length(dbListTables(conn)) > 0)

  if (isFALSE(ret) && isTRUE(create)) {
    if (is(drv, "PqDriver")) {
      file.create(CacheDBFile(cachePath, drv = drv))
    }
  }
  return(ret)
}

