#' Create a new cache
#'
#' @param cachePath A path describing the directory in which to create
#'   the database file(s)
#' @param drv A driver, passed to \code{dbConnect}
#' @param force Logical. Should it create a cache in the \code{cachePath},
#'   even if it already exists, overwriting.
#' @importFrom data.table data.table
#' @inheritParams DBI::dbConnect
#' @inheritParams DBI::dbWriteTable
#' @rdname cache-tools
#' @export
createCache <- function(cachePath = getOption("reproducible.cachePath"),
                        drv = getOption("reproducible.drv"),
                        conn = getOption("reproducible.conn", NULL), force = FALSE) {
  # browser(expr = exists("aaaa"))
  alreadyExists <- CacheIsACache(cachePath, drv = drv, conn = conn, create = TRUE)
  if (alreadyExists && force == FALSE) {
    messageCache("Cache already exists at ", cachePath, " and force = FALSE. Not creating new cache.")
    return(invisible(cachePath))
  }

  checkPath(cachePath, create = TRUE)
  checkPath(CacheStorageDir(cachePath), create = TRUE)
  if (is.null(conn)) {
    conn <- dbConnectAll(drv, cachePath = cachePath)
    on.exit(dbDisconnectAll(conn, shutdown = TRUE))
  }

  createEmptyTable(conn, cachePath, drv)
}

#' @rdname cache-tools
#' @inheritParams Cache
#' @param cacheId The hash string representing the result of \code{.robustDigest}
#' @param obj The R object to save to the cache
#' @param linkToCacheId Optional. If a \code{cacheId} is provided here, then a \code{file.link}
#'   will be made to the file with that \code{cacheId} name in the cache repo.
#'   This is used when identical outputs exist in the cache. This will save disk space.
saveToCache <- function(cachePath = getOption("reproducible.cachePath"),
                        drv = getOption("reproducible.drv"),
                        conn = getOption("reproducible.conn", NULL), obj, userTags, cacheId,
                        linkToCacheId = NULL) {
  # browser(expr = exists("._saveToCache_1"))
  if (is.null(conn)) {
    conn <- dbConnectAll(drv, cachePath = cachePath)
    on.exit(dbDisconnectAll(conn, shutdown = TRUE))
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

  fts <- CacheStoredFile(cachePath, cacheId)

  # browser(expr = exists("._saveToCache_2"))

  # TRY link first, if there is a linkToCacheId, but some cases will fail; not sure what these cases are
  if (!is.null(linkToCacheId)) {
    ftL <- CacheStoredFile(cachePath, linkToCacheId)
    if (exists("._saveToCache_1")) browser()
    suppressWarnings({
      out <- try(file.link(from = ftL, to = fts), silent = TRUE)
    })
    if (is(out, "try-error") | !out)
      linkToCacheId <- NULL
    else {
      messageCache("  (A file with identical properties already exists in the Cache: ",basename(ftL),"; ",
                   "The newly added (",basename(fts),") is a file.link to that file)")
    }
    fs <- file.size(fts)
  }

  if (is.null(linkToCacheId)) {
    fs <- saveFileInCacheFolder(obj, fts)
    # if (getOption("reproducible.cacheSaveFormat", "rds") == "qs") {
    #   .requireNamespace("qs", stopOnFALSE = TRUE)
    #   for (attempt in 1:2) {
    #     fs <- qs::qsave(obj, file = fts, nthreads = getOption("reproducible.nThreads", 1),
    #                     preset = getOption("reproducible.qsavePreset", "high"))
    #     fs1 <- file.size(fts)
    #     if (!identical(fs, fs1)) {
    #       if (attempt == 1) {
    #         warning("Attempted to save to Cache, but save seemed to fail; trying again")
    #       } else {
    #         stop("Saving to Cache did not work correctly; file appears corrupted. Please retry")
    #       }
    #     } else {
    #       break
    #     }
    #   }
    # } else {
    #   saveRDS(obj, file = fts)
    #   fs <- file.size(fts)
    # }
  }
  if (isTRUE(getOption("reproducible.useMemoise"))) {
    if (is.null(.pkgEnv[[cachePath]]))
      .pkgEnv[[cachePath]] <- new.env(parent = emptyenv())
    assign(cacheId, obj, envir = .pkgEnv[[cachePath]])
  }

  fsChar <- as.character(fs)

  tagKeyHasFS <- tagKey %in% "file.size"
  if (isFALSE(any(tagKeyHasFS))) {
    tagKey <- c(tagKey, "file.size")
    tagValue <- c(tagValue, fsChar)
  } else {
    tagValue[tagKeyHasFS] <- fsChar
  }

  # Compare the file size with the object size -- to test for "captured environments"
  #  There is a buffer of 4x, plus file sizes are smaller than binary size with qs defaults
  #  So effectively, it is like 6x buffer to try to avoid false positives.
  whichOS <- which(tagKey == "object.size")
  if (length(whichOS)) {
    fsBig <- (as.numeric(tagValue[whichOS]) * 4 ) < fs
    if (isTRUE(fsBig)) {
      # browser(expr = exists("._saveToCache_3"))
      messageCache("Object with cacheId ", cacheId, " appears to have a much larger size ",
                   "on disk than in memory. ",
                   "This usually means that the object has captured an environment with ",
                   "many objects due to how a function or a formula is defined. ",
                   "Usually, a solution involves using quote and eval around the formulas ",
                   "and defining functions in a package or otherwise clean space, ",
                   "i.e., not inside another function.\n",
                   "See http://adv-r.had.co.nz/memory.html#gc and 'capturing environments'.")
    }
  }
  dt <- data.table("cacheId" = cacheId, "tagKey" = tagKey,
                   "tagValue" = tagValue, "createdDate" = as.character(Sys.time()))
  appendAll(conn, cachePath, drv, dt)

  return(obj)
}

#' @export
#' @rdname cache-tools
#' @inheritParams CacheStoredFile
loadFromCache <- function(cachePath = getOption("reproducible.cachePath"),
                          cacheId,
                          format = getOption("reproducible.cacheSaveFormat", "rds"),
                          drv = getOption("reproducible.drv"),
                          conn = getOption("reproducible.conn", NULL) ) {
  isMemoised <- FALSE
  if (isTRUE(getOption("reproducible.useMemoise"))) {
    if (is.null(.pkgEnv[[cachePath]]))
      .pkgEnv[[cachePath]] <- new.env(parent = emptyenv())
    isMemoised <- exists(cacheId, envir = .pkgEnv[[cachePath]])
    if (isTRUE(isMemoised)) {
      obj <- get(cacheId, envir = .pkgEnv[[cachePath]])
    }
  }
  if (!isTRUE(isMemoised)) {
    f <- CacheStoredFile(cachePath, cacheId, format)

    # First test if it is correct format
    if (!file.exists(f)) {
      sameCacheID <- dir(dirname(f), pattern = filePathSansExt(basename(f)))
      if (length(sameCacheID)) {
        messageCache("     (Changing format of Cache entry from ", fileExt(sameCacheID), " to ",
                     fileExt(f), ")")
        obj <- loadFromCache(cachePath = cachePath, cacheId = cacheId,
                             format = fileExt(sameCacheID))
        fs <- saveToCache(obj = obj, cachePath = cachePath, drv = drv, conn = conn,
                          cacheId = cacheId)
        rmFromCache(cachePath = cachePath, cacheId = cacheId, drv = drv, conn = conn,
                    format = fileExt(sameCacheID))
        return(fs)
      }
    }
    obj <- loadFile(f, format = format)
  }
  obj <- dealWithClassOnRecovery(obj, cacheRepo = cachePath,
                                 cacheId = cacheId,
                                 drv = drv, conn = conn)

  if (isTRUE(getOption("reproducible.useMemoise")) && !isTRUE(isMemoised)) {
    assign(cacheId, obj, envir = .pkgEnv[[cachePath]])
  }
  obj

}

#' Low level tools to work with Cache
#'
#' @export
#' @rdname cache-tools
rmFromCache <- function(cachePath = getOption("reproducible.cachePath"),
                        cacheId, drv = getOption("reproducible.drv"),
                        conn = getOption("reproducible.conn", NULL),
                        format = getOption("reproducible.cacheSaveFormat", "rds")) {
  if (is.null(conn)) {
    conn <- dbConnectAll(drv, cachePath = cachePath, create = FALSE)
    on.exit(dbDisconnectAll(conn, shutdown = TRUE))
  }
  # from https://cran.r-project.org/web/packages/DBI/vignettes/spec.html
  # query <- glue::glue_sql(
  #   "DELETE FROM {DBI::SQL(double_quote(dbTabName))} WHERE \"cacheId\" IN ({cacheId*})",
  #   dbTabName = CacheDBTableName(cachePath, drv),
  #   cacheId = cacheId,
  #   .con = conn)
  # res <- dbSendQuery(conn, query)
  #
  # if (FALSE)   { # this is the "unsafe" version
  #   query <- paste0("DELETE FROM \"", CacheDBTableName(cachePath, drv), "\" WHERE \"cacheId\" = $1")
  #   res <- dbSendStatement(conn, query)
  #   dbBind(res, list(cacheId))
  # }
  #
  # dbClearResult(res)

  rmFromCacheAll(cachePath, drv, cacheId, conn)

  unlink(CacheStoredFile(cachePath, hash = cacheId, format = format))
}

dbConnectAll <- function(drv = getOption("reproducible.drv"),
                         cachePath = getOption("reproducible.cachePath"),
                         conn = getOption("reproducible.conn", NULL), create = TRUE,
                         read_only = FALSE) {

  if (identical(drv, "fst")) {
    conn <- CacheDBFile(cachePath, drv = drv, conn = conn)
    return(conn)
  }
  args <- list(drv = drv)
  stayConnected <- keepDBConnected(drv)
  if (stayConnected) {
    conn <- try(get(connObject(cachePath), envir = .pkgEnv), silent = TRUE)
    if (!is(conn, "try-error")) {
      isValid <- DBI::dbIsValid(conn)
      if (isValid)
        return(conn)
    }
  }
  if (is(drv, "SQLiteDriver") || is(drv, "duckdb_driver")) {
    args <- append(args, list(dbname = CacheDBFile(cachePath, drv = drv, conn = conn),
                              synchronous = NULL))
    if (is(drv, "duckdb_driver")) {
      if (!requireNamespace("duckdb", quietly = TRUE)) stop("To use duckdb, please install.packages('duckdb')")
      args <- list(duckdb::duckdb(args$dbname, read_only = read_only))
    }
  }
  # other types of drv, e.g., Postgres can be done via env vars
  conn <- do.call(DBI::dbConnect, args)
  if (stayConnected) {
    assign(connObject(cachePath), conn, envir = .pkgEnv)
  }
  return(conn)
}


connObject <- function(cachePath) {
  cp <- gsub("/", "_", cachePath)
  cp <- gsub(":", "", cp)
  paste0(cp, "_conn")
}

.emptyCacheTable <- data.table::data.table(cacheId = character(), tagKey = character(),
                                           tagValue = character(), createdDate = character())

.length1CacheTable <- data.table::data.table(cacheId = "a", tagKey = "a",
                                             tagValue = "a", createdDate = "a")

.addTagsRepo <- function(cacheId, cachePath = getOption("reproducible.cachePath"),
                         tagKey = character(), tagValue = character(),
                         drv = getOption("reproducible.drv"),
                         conn = getOption("reproducible.conn", NULL)) {
  # browser(expr = exists("._addTagsRepo_1"))
  if (length(cacheId) > 0) {
    if (length(cacheId) > 1) stop(".addTagsRepo can only handle appending 1 tag at a time")
    if (is.null(conn)) {
      conn <- dbConnectAll(drv, cachePath = cachePath, create = FALSE)
      on.exit(dbDisconnectAll(conn, shutdown = TRUE))
    }
    curTime <- as.character(Sys.time())
    if (length(tagKey) < length(cacheId))
      tagKey <- "accessed"
    if (length(tagValue) < length(cacheId))
      tagValue <- curTime

    # This is what the next code pair of lines does
    rs <- addTagsAll(conn, cachePath, drv, cacheId, tagKey, tagValue, curTime)


  }
}

.updateTagsRepo <- function(cacheId, cachePath = getOption("reproducible.cachePath"),
                            tagKey = character(), tagValue = character(),
                            add = TRUE,
                            drv = getOption("reproducible.drv"),
                            conn = getOption("reproducible.conn", NULL)) {
  # browser(expr = exists("._updateTagsRepo_1"))
  if (length(cacheId) > 0) {
    if (length(cacheId) > 1) stop(".updateTagsRepo can only handle updating 1 tag at a time")
    if (is.null(conn)) {
      conn <- dbConnectAll(drv, cachePath = cachePath, create = FALSE)
      on.exit(dbDisconnectAll(conn, shutdown = TRUE))
    }
    curTime <- as.character(Sys.time())
    if (length(tagKey) < length(cacheId)) {
      warning("tagKey and/or tagValue must both be supplied for .updateTagsRepo.")
      return(invisible())
    }

    affectedAnyRows <- updateTagsAll(conn, cachePath, drv, tagValue, cacheId, tagKey)
    if (!affectedAnyRows) {
      if (isTRUE(add)) {
        .addTagsRepo(cacheId, cachePath, tagKey, tagValue, drv = drv, conn = conn)
      }
    }

  }

}
.cacheNumDefaultTags <- function() {
  8
}

.ignoreTagKeys <- function() {
  c("preDigest", "otherFunctions", "accessed", "elapsedTimeLoad", "fromDisk", "origRaster", "cacheRaster")
}

.cacheTableHashColName <- function() {
  "cacheId"
}

.cacheTableTagColName <- function(option = NULL) {
  out <- "tagValue"
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
CacheDBFile <- function(cachePath = getOption("reproducible.cachePath"),
                        drv = getOption("reproducible.drv"),
                        conn = getOption("reproducible.conn", NULL)) {

  type <- gsub("Driver", "", class(drv))

  if (!is.null(conn)) {
    type <- gsub("Connection", "", class(conn))
  }

  if (grepl(type, "SQLite")) {
    file.path(cachePath, "cache.db")
  } else {
    file.path(cachePath, "cache_dbOther")
  }
}

#' @rdname CacheHelpers
#' @export
CacheStorageDir <- function(cachePath = getOption("reproducible.cachePath")) {
  file.path(cachePath, "cacheOutputs")
}

#' @details
#' \code{CacheStoredFile} returns the file path to the file with the specified hash value.
#'
#' @rdname CacheHelpers
#' @export
#' @param hash The cacheId or otherwise digested hash value, as character string.
#' @param format The text string representing the file extension used normally by
#'   different save formats; currently only \code{"rds"} or \code{"qs"}. Defaults
#'   to \code{getOption("reproducible.cacheSaveFormat", "rds")}
CacheStoredFile <- function(cachePath = getOption("reproducible.cachePath"), hash,
                            format = getOption("reproducible.cacheSaveFormat", "rds")) {
  csf <- format
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
CacheDBTableName <- function(cachePath = getOption("reproducible.cachePath"),
                             drv = getOption("reproducible.drv")) {
  if (!is(cachePath, "Path")) {
    cachePath <- asPath(cachePath, nParentDirs = 2)
  }
  toGo <- attr(cachePath, "nParentDirs")
  cachePathTmp <- normPath(cachePath)
  newPath <- basename2(cachePathTmp)
  while (toGo > 1) {
    toGo <- toGo - 1
    cachePathTmp <- dirname(cachePathTmp)
    newPath <- paste(basename2(cachePathTmp), newPath, sep = "_")
  }
  # SQLite can't handle numbers as initial character of a table name
  if (grepl("^[[:digit:]]", newPath)) {newPath <- paste0("_", newPath)}
  return(newPath)
}

#' @rdname CacheHelpers
#' @param create Logical. Currently only affects non RQSLite default drivers. If this
#'   is \code{TRUE} and there is no Cache database, the function will create one.
#' @export
#' @details
#' \code{CacheIsACache} returns a logical of whether the specified cachePath
#'   is actually a functioning cache.
CacheIsACache <- function(cachePath = getOption("reproducible.cachePath"), create = FALSE,
                          drv = getOption("reproducible.drv"),
                          conn = getOption("reproducible.conn", NULL)) {
  checkPath(cachePath, create = TRUE)
  # browser(expr = exists("._CacheIsACache_1"))
  ret <- FALSE
  connIsNull <- is.null(conn)
  if (connIsNull) {
    conn <- dbConnectAll(drv, cachePath = cachePath)
  }

  type <- gsub("Connection", "", class(conn))

  ret <- all(basename2(c(CacheDBFile(cachePath, drv, conn), CacheStorageDir(cachePath))) %in%
               list.files(cachePath))
  if (!keepDBConnected(drv) && isTRUE(ret) && connIsNull)
    on.exit(dbDisconnectAll(conn, shutdown = TRUE))

  if (ret && useSQL(conn)) {
    tablesInDB <- retry(retries = 250, exponentialDecayBase = 1.01,
                        quote(DBI::dbListTables(conn)))
    tableShouldBe <- CacheDBTableName(cachePath)
    if (length(tablesInDB) == 1) {
      if (!any(tablesInDB %in% tableShouldBe) && grepl(type, "SQLite")) {
        warning(paste0("The table in the Cache repo does not match the cacheRepo. ",
                       "If this is because of a moved repository (i.e., files ",
                       "copied), then it is being updated automatically. ",
                       "If not, cache is in an error state. ",
                       "You may need to delete the Cache"))
        movedCache(cachePath, #old = tablesInDB,
                   drv = drv, conn = conn)
      }

    }
    ret <- ret && any(grepl(tableShouldBe, tablesInDB))
  }

  if (isFALSE(ret) && isTRUE(create)) {
    if (grepl(type, "Pq")) {
      file.create(CacheDBFile(cachePath, drv = drv, conn = conn))
    }
  }
  return(ret)
}

#' Deal with moved cache issues
#'
#' If a user manually copies a complete Cache folder (including the db file and rasters folder),
#' there are issues that must be addressed. Primarily, the db table must be renamed. Run
#' this function after a manual copy of a cache folder. See examples for one way to do that.
#'
#' @param  new Either the path of the new \code{cachePath} where the cache was moved or copied to, or
#'   the new DB Table Name
#' @param  old Optional, if there is only one table in the \code{new} cache path.
#'   Either the path of the previous \code{cachePath} where the cache was moved or copied from, or
#'   the old DB Table Name
#' @inheritParams Cache
#' @export
#' @examples
#' tmpCache <- file.path(tempdir(), "tmpCache")
#' tmpdir <- file.path(tempdir(), "tmpdir")
#' bb <- Cache(rnorm, 1, cacheRepo = tmpCache)
#'
#' # Copy all files from tmpCache to tmpdir
#' froms <- normPath(dir(tmpCache, recursive = TRUE, full.names = TRUE))
#' checkPath(file.path(tmpdir, "rasters"), create = TRUE)
#' checkPath(file.path(tmpdir, "cacheOutputs"), create = TRUE)
#' file.copy(from = froms, overwrite = TRUE,
#'           to = gsub(normPath(tmpCache), normPath(tmpdir), froms))
#'
#' # Must use 'movedCache' to update the database table
#' movedCache(new = tmpdir, old = tmpCache)
#' bb <- Cache(rnorm, 1, cacheRepo = tmpdir) # should recover the previous call
#'
movedCache <- function(new, old, drv = getOption("reproducible.drv"),
                       conn = getOption("reproducible.conn", NULL)) {
  if (is.null(conn)) {
    conn <- dbConnectAll(drv, cachePath = new)
    if (!keepDBConnected(drv))
      on.exit(dbDisconnectAll(conn, shutdown = TRUE))
  }
  renameCacheAll(new, old, drv = drv, conn = conn)
}

loadFile <- function(file, format) {
  # browser(expr = exists("._loadFile_1"))
  if (missing(format))
    format <- fileExt(file)
  if (format == "qs") {
    .requireNamespace("qs", stopOnFALSE = TRUE)
    obj <- qs::qread(file = file, nthreads = getOption("reproducible.nThreads", 1))
  } else {
    obj <- readRDS(file = file)
  }
}

saveFileInCacheFolder <- function(obj, fts, cachePath, cacheId) {
  if (missing(fts))
    fts <- CacheStoredFile(cachePath, cacheId)

  if (getOption("reproducible.cacheSaveFormat", "rds") == "qs") {
    .requireNamespace("qs", stopOnFALSE = TRUE)
    for (attempt in 1:2) {
      fs <- qs::qsave(obj, file = fts, nthreads = getOption("reproducible.nThreads", 1),
                      preset = getOption("reproducible.qsavePreset", "high"))
      fs1 <- file.size(fts)
      if (!identical(fs, fs1)) {
        if (attempt == 1) {
          warning("Attempted to save to Cache, but save seemed to fail; trying again")
        } else {
          stop("Saving to Cache did not work correctly; file appears corrupted. Please retry")
        }
      } else {
        break
      }
    }
  } else {
    saveRDS(obj, file = fts)
    fs <- file.size(fts)
  }
  fs
}


addTagsAll <- function(conn, cachePath, drv, cacheId, tagKey, tagValue, curTime) {
  if (useSQL(conn)) {
    rs <- retry(retries = 250, exponentialDecayBase = 1.01, quote(
      DBI::dbSendStatement(
        conn,
        paste0("insert into \"", CacheDBTableName(cachePath, drv), "\"",
               " (\"cacheId\", \"tagKey\", \"tagValue\", \"createdDate\") values ",
               "('", cacheId,
               "', '", tagKey, "', '", tagValue, "', '", curTime, "')"))
    ))

    DBI::dbClearResult(rs)
  } else {
    dt <- setDT(list(cacheId = cacheId, tagKey = tagKey, tagValue = tagValue, createdDate = curTime))
    appendAll(conn, cachePath, drv, dt)
  }
}

appendAll <- function(conn, cachePath, drv, dt) {
  a <- if (useSQL(conn))
    retry(retries = 250, exponentialDecayBase = 1.01, quote(
      DBI::dbAppendTable(conn, CacheDBTableName(cachePath, drv), dt)))
  else {
    objName <- objNameFromConn(conn)
    fsTab <- readFilebasedConn(objName, conn)
    # fsTab <- rbindlist(list(fsTab, dt))
    writeFilebasedConnToMemory(conn = conn, dt = fsTab, objName = objName, dtRowsAdded = dt)
  }
}

updateTagsAll <- function(conn, cachePath, drv, tagValue, cacheId, tagKey) {
  if (useSQL(conn)) {
    rs <- #retry(retries = 250, exponentialDecayBase = 1.01, quote(
      DBI::dbSendStatement(
        conn,
        paste0("update \"", CacheDBTableName(cachePath, drv), "\"",
               " set \"tagValue\" = '",tagValue,"' where ",
               " \"cacheId\" = '",cacheId, "'", " AND \"tagKey\" = '",tagKey, "'"))
    #))
    affectedAnyRows <- DBI::dbGetRowsAffected(rs) > 0
    DBI::dbClearResult(rs)
  } else {
    # dt <- as.data.table(list(cacheId = cacheId, tagKey = cacheId, tagValue = tagValue, createdDate = curTime))

    objName <- objNameFromConn(conn)
    fsTab <- readFilebasedConn(objName, conn)
    affectedAnyRows <- which(fsTab$cacheId == cacheId & fsTab$tagKey == tagKey)
    if (length(affectedAnyRows)) {
      # fsTab[affectedAnyRows, tagValue := tagValue]
      writeFilebasedConnToMemory(conn = conn, objName = objName, dt = fsTab, update = fsTab[affectedAnyRows,])
    }
    affectedAnyRows <- length(affectedAnyRows) > 0
  }
  return(affectedAnyRows)
}

objNameFromConn <- function(conn) {
  gsub(":", "", gsub("/|\\\\", "_", conn))
}

#' @importFrom fst read_fst
readFilebasedConn <- function(objName, conn, columns = NULL, from = 1, to = NULL) {
  if (missing(objName)) {
    objName <- objNameFromConn(conn)
  }
  fsTab <- NULL
  if (exists(objName, envir = .pkgEnv, inherits = FALSE)) {
    fsTab <- get(objName, envir = .pkgEnv, inherits = FALSE)
  } else {
    # Read from disk, then assign right away to memory for all future reads
    if (file.exists(conn)) {
      tf <- tempfile()
      on.exit({if (file.exists(tf)) file.remove(tf)})
      fsTab <- try(retry(retries = 11, exponentialDecayBase = 1.01, silent = TRUE, quote({
        file.copy(conn, tf, overwrite = TRUE)
        fsTab <- read_fst(tf)
      }
      )))
      if (is(fsTab, "try-error"))
        stop("Something went wrong with accessing the Cache 2")

      if (!is(fsTab, "try-error")) {
        dig <- digest::digest(file = tf, algo = "xxhash64")
        writeFilebasedConnToMemory(objName = objName, dt = fsTab, conn = conn, dig = dig)
      }
      if (file.exists(tf)) file.remove(tf)
    } else {
      fsTab <- NULL
    }

  }
  if (!is.null(columns))
    fsTab <- fsTab[, columns, drop = FALSE]
  if (!is.null(to))
    fsTab <- fsTab[from:to, , drop = FALSE]

  fsTab
}

#' @importFrom fst write_fst
writeFilebasedConnToMemory <- function(cachePath, drv, conn, dt, objName, dig = NULL,
                                       dtRowsAdded = NULL, rm = NULL, update = NULL) {
  if (missing(conn))
    conn <- CacheDBFile(cachePath, drv = drv, conn = conn)
  if (missing(objName))
    objName <- objNameFromConn(conn)
  assign(objName, dt, envir = .pkgEnv)
  if (!is.null(dig)) {
    objNameDig <- objNameWithDig(objName)
    assign(objNameDig, dig, envir = .pkgEnv)
  }
  if (!is.null(rm)) {
    objNameRm <- objNameWithRm(objName)
    assign(objNameRm, rm, envir = .pkgEnv)
  }
  if (!is.null(dtRowsAdded)) {
    objNameToAdd <- objNameWithToAdd(objName)
    alreadyThere <- get0(objNameToAdd, envir = .pkgEnv)
    dtRowsAdded <- list(dtRowsAdded)
    if (!is.null(alreadyThere)) {
      dtRowsAdded <- append(dtRowsAdded, alreadyThere)
    }
    assign(objNameToAdd, dtRowsAdded, envir = .pkgEnv)
  }
  if (!is.null(update)) {
    objNameToUpdate <- objNameWithToUpdate(objName)
    alreadyThere <- get0(objNameToUpdate, envir = .pkgEnv)
    update <- list(update)
    if (!is.null(alreadyThere)) {
      update <- append(update, alreadyThere)
    }
    assign(objNameToUpdate, update, envir = .pkgEnv)
  }

  return(invisible())
}

finalizeDTtoWrite <- function(conn, dt, objName) {


  objNameDig <- objNameWithDig(objName)
  digPre <- get0(objNameDig, envir = .pkgEnv)
  if (!is.null(digPre)) {
    tf <- tempfile()
    on.exit({if (file.exists(tf)) file.remove(tf)})

    dt <- try(
      retry(retries = 10, exponentialDecayBase = 1.01, silent = TRUE, quote({
        file.copy(conn, tf, overwrite = TRUE)

        digPost <- digest::digest(file = tf, algo = "xxhash64")
        if (!identical(digPost, digPre)) {
          dt <- try(readFilebasedConn(conn = tf), silent = FALSE)
        }
        if (file.exists(tf)) file.remove(tf)
        return(dt)
      }
      )))
    if (is(dt, "try-error"))
      stop("Something went wrong with accessing the Cache 1")


    objNameRm <- objNameWithRm(objName)
    toRm <- get0(objNameRm, envir = .pkgEnv)
    if (!is.null(toRm)) {
      wh <- which(!dt$cacheId %in% toRm)
      if (length(wh) == 0)
        dt <- setDF(.emptyCacheTable)
      else
        dt <- dt[wh, ]
    }

    objNameToAdd <- objNameWithToAdd(objName)
    ToAdd <- get0(objNameToAdd, envir = .pkgEnv)
    if (!is.null(ToAdd)) {
      dt <- rbindlist(append(list(dt), ToAdd))
    }
    objNameToUpdate <- objNameWithToUpdate(objName)
    ToUpdate <- get0(objNameToUpdate, envir = .pkgEnv)
    if (!is.null(ToUpdate)) {
      if (length(ToUpdate) > 1) {
        ToUpdate <- rbindlist(ToUpdate)
      } else {
        ToUpdate <- ToUpdate[[1]]
      }
      dt$tagValue[ToUpdate$cacheId == dt$cacheId & ToUpdate$tagKey == dt$tagKey] <- ToUpdate$tagValue

    }
  }

  # In parallel calculations, the original table may be old and incorrect
  return(dt)
}

dbDisconnectAll <- function(conn = getOption("reproducible.conn", NULL), ...) {
  if (useSQL(conn))
    DBI::dbDisconnect(conn, ...)
  else {
    objName <- objNameFromConn(conn)
    tab <- readFilebasedConn(objName, conn)
    tab <- finalizeDTtoWrite(conn = conn, dt = tab, objName = objName)
    rm(list = ls(.pkgEnv, pattern = objName), envir = .pkgEnv)
    writeFilebasedConn(conn = conn, dt = tab, objName = objName)
  }

}

writeFilebasedConn <- function(cachePath, drv, conn, dt, objName) {
  if (!is.null(dt)) { # basically, if there was an error in the Cache function, there won't be anything here
    if (missing(conn))
      conn <- CacheDBFile(cachePath, drv = drv, conn = conn)
    tf <- tempfile()
    on.exit({if (file.exists(tf)) file.remove(tf)})

    # objNameDig <- objNameWithDig(objName)
    # digPre <- get(objNameDig, envir = .pkgEnv)
    #
    # # In parallel calculations, the original table may be old and incorrect
    # tf <- tempfile()
    # retry(retries = 2, exponentialDecayBase = 1.01, quote(file.copy(conn, tf)))
    # digPost <- digest::digest(file = tf, algo = "xxhash64")
    # if (!identical(digPost, digPre)) {
    #   fsTab <- read_fst(tf)
    #
    # }
    fsTab <- write_fst(dt, tf)
    retry(retries = 2, exponentialDecayBase = 1.01, silent = TRUE,
          quote(file.rename(tf, conn)))
    if (file.exists(tf)) file.remove(tf)
  }
  return(invisible())
}

createEmptyTable <- function(conn, cachePath, drv) {
  dt <- .emptyCacheTable
  if (useSQL(conn)) {
    # Some tough to find cases where stalls on dbWriteTable -- this *may* prevent some
    a <- retry(retries = 250, exponentialDecayBase = 1.01,
               quote(DBI::dbListTables(conn)))

    if (isTRUE(!CacheDBTableName(cachePath, drv) %in% a))
      try(DBI::dbWriteTable(conn, CacheDBTableName(cachePath, drv), dt, overwrite = FALSE,
                            field.types = c(cacheId = "text", tagKey = "text",
                                            tagValue = "text", createdDate = "text")), silent = TRUE)
  } else {
    setDF(dt)
    writeFilebasedConn(cachePath, drv, conn, dt = dt)
  }
  return(invisible())
}

rmFromCacheAll <- function(cachePath, drv, cacheId, conn) {
  if (useSQL(conn)) {
    query <- glue::glue_sql(
      "DELETE FROM {DBI::SQL(glue::double_quote(dbTabName))} WHERE \"cacheId\" IN ({cacheId*})",
      dbTabName = CacheDBTableName(cachePath, drv),
      cacheId = cacheId,
      .con = conn)
    res <- DBI::dbSendQuery(conn, query)

    if (FALSE)   { # this is the "unsafe" version
      query <- paste0("DELETE FROM \"", CacheDBTableName(cachePath, drv), "\" WHERE \"cacheId\" = $1")
      res <- DBI::dbSendStatement(conn, query)
      DBI::dbBind(res, list(cacheId))
    }

    DBI::dbClearResult(res)
  } else {
    objDT <- readFilebasedConn(conn = conn)
    # wh <- which(!objDT$cacheId %in% cacheId)
    # if (length(wh) == 0)
    #   objDT <- setDF(.emptyCacheTable)
    # else
    #   objDT <- objDT[wh, ]
    writeFilebasedConnToMemory(cachePath, drv, conn, dt = objDT, rm = cacheId)
  }
}

renameCacheAll <- function(new, old, drv, conn) {
  if (useSQL(conn)) {
    tables <- DBI::dbListTables(conn)
    # browser(expr = exists("._movedCache_2"))
    if (missing(old)) {
      if (length(tables) == 1) {
        messageCache("Assuming old database table is ", tables)
      } else {
        dbname <- try(conn@dbname, silent = TRUE)
        if (is(dbname, "try-error"))
          dbname <- "conn"
        stop("old not provided and there are more than one database table in ", )
      }
      old <- tables
      oldTable <- old
    } else {
      oldTable <- CacheDBTableName(old, drv = drv)
    }

    if (!any(tables == oldTable)) {
      stop("The 'old' table name does not appear inside the path to the 'new'")
    }
    newTable <- CacheDBTableName(new, drv = drv)

    qry <- glue::glue_sql("ALTER TABLE {`old`} RENAME TO {`new`}",
                          old = oldTable,
                          new = newTable,
                          .con = conn)
    res <- retry(retries = 15, exponentialDecayBase = 1.01, quote(dbSendQuery(conn, qry)))
    # dbFetch(res)
    DBI::dbClearResult(res)
  } # fst does not need to rename a table, as there are no tables
}

useSQL <- function(conn) {
  if (is(conn, "DBIConnection")) {
    if (!requireNamespace("RSQLite")) stop("to use an SQL database, you must also install.packages(c('RSQLite', 'DBI'))")
    TRUE
  } else {
    FALSE
  }
}

objNameWithDig <- function(objName) paste0(objName, "_dig")

objNameWithToAdd <- function(objName) paste0(objName, "_dtRowsAdded")

objNameWithRm <- function(objName) paste0(objName, "_rm")

objNameWithToUpdate <- function(objName) paste0(objName, "_update")
