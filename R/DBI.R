#' Create a new cache
#'
#' @param cachePath A path describing the directory in which to create
#'   the database file(s)
#' @param drv A driver, passed to \code{dbConnect}
#' @param force Logical. Should it create a cache in the \code{cachePath},
#'   even if it already exists, overwriting.
#'   (Replaces \code{archivist::createLocalRepo}.)
#' @importFrom data.table data.table
#' @importFrom DBI dbConnect dbDisconnect dbWriteTable
#' @inheritParams DBI::dbConnect
#' @inheritParams DBI::dbWriteTable
#' @rdname cacheTools
#' @export
createCache <- function(cachePath, drv = getOption("reproducible.drv", RSQLite::SQLite()),
                        conn = getOption("reproducible.conn", NULL), force = FALSE) {
  browser(expr = exists("aaaa"))
  alreadyExists <- CacheIsACache(cachePath, drv = drv, conn = conn, create = TRUE)
  if (alreadyExists && force == FALSE) {
    message("Cache already exists at ", cachePath, " and force = FALSE. Not creating new cache.")
    return(invisible(cachePath))
  }

  checkPath(cachePath, create = TRUE)
  checkPath(CacheStorageDir(cachePath), create = TRUE)
  if (useDBI()) {
    if (is.null(conn)) {
      conn <- dbConnectAll(drv, cachePath = cachePath)
      on.exit(dbDisconnect(conn))
    }
  }
  dt <- .emptyCacheTable

  # Some tough to find cases where stalls on dbWriteTable -- this *may* prevent some
  a <- retry(retries = 250, exponentialDecayBase = 1.01,
        quote(dbListTables(conn)))

  if (isTRUE(!CacheDBTableName(cachePath, drv) %in% a))
    #retry(retries = 5, exponentialDecayBase = 1.5, quote(
      try(dbWriteTable(conn, CacheDBTableName(cachePath, drv), dt, overwrite = FALSE,
                   field.types = c(cacheId = "text", tagKey = "text",
                                   tagValue = "text", createdDate = "text")), silent = TRUE)
    #)
}

#' @rdname cacheTools
#' @inheritParams Cache
#' @param cacheId The hash string representing the result of \code{.robustDigest}
#' @param obj The R object to save to the cache
#' @param linkToCacheId Optional. If a \code{cacheId} is provided here, then a \code{file.link}
#'   will be made to the file with that \code{cacheId} name in the cache repo.
#'   This is used when identical outputs exist in the cache. This will save disk space.
#' @importFrom qs qsave
saveToCache <- function(cachePath, drv = getOption("reproducible.drv", RSQLite::SQLite()),
                        conn = getOption("reproducible.conn", NULL), obj, userTags, cacheId,
                        linkToCacheId = NULL) {
  browser(expr = exists("._saveToCache_1"))
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

  # outputToSaveIsList <- is(obj, "list") # is.list is TRUE for anything, e.g., data.frame. We only want "list"
  # if (outputToSaveIsList) {
  #   rasters <- unlist(lapply(obj, is, "Raster"))
  # } else {
  #   rasters <- is(obj, "Raster")
  # }
  # browser(expr = exists("aaaa"))
  # if (any(rasters)) {
  #   atts <- attributes(obj)
  #   if (outputToSaveIsList) {
  #     obj[rasters] <- lapply(obj[rasters], function(x)
  #       .prepareFileBackedRaster(x, repoDir = cachePath, overwrite = FALSE, drv = drv))
  #   } else {
  #     obj <- .prepareFileBackedRaster(obj, repoDir = cachePath,
  #                                              overwrite = FALSE, drv = drv)
  #   }
  #
  #   # have to reset all these attributes on the rasters as they were undone in prev steps
  #   setattr(obj, "tags", atts$tags)
  #   .setSubAttrInList(obj, ".Cache", "newCache", atts$.Cache$newCache)
  #   setattr(obj, "call", atts$call)
  #
  #   if (!identical(attr(obj, ".Cache")$newCache, atts$.Cache$newCache))
  #     stop("attributes are not correct 6")
  #   if (!identical(attr(obj, "call"), atts$call))
  #     stop("attributes are not correct 7")
  #   if (!identical(attr(obj, "tags"), atts$tags))
  #     stop("attributes are not correct 8")
  #
  #   if (!is.null(atts[["function"]])) {
  #     setattr(obj, "function", atts[["function"]])
  #     if (!identical(attr(obj, "function"), atts[["function"]]))
  #       stop("There is an unknown error 04")
  #   }
  #   # attr(obj, "function") <- attr(output, "function")
  #
  #  #output <- obj
  # }

  # This section is more direct, and under some conditions is faster, but sooo wordy
  # ci <- rep(cacheId, length(tagKey))
  # tagKey
  # tagValue
  # times <- rep(as.character(Sys.time()), length(tagKey))
  # vals <- paste0("(\"", paste(collapse = "\"), (\"",
  #       apply(data.frame(ci, tagKey, tagValue, times), 1, function(y) {
  #         paste(y, collapse = "\", \"")
  #       })
  #       ), "\")")
  # res <- retry(quote(dbSendStatement(
  #   conn, paste0("insert into ",CacheDBTableName(cachePath, drv),
  #                " ('cacheId', 'tagKey', 'tagValue', 'createdDate') values ", vals)),
  #   retries = 15))
  # dbClearResult(res)

  # The above can replace this
  fts <- CacheStoredFile(cachePath, cacheId)

  browser(expr = exists("._saveToCache_2"))
  if (is.null(linkToCacheId)) {
    if (getOption("reproducible.cacheSaveFormat", "rds") == "qs")
      fs <- qs::qsave(obj, file = fts, nthreads = getOption("reproducible.nThreads", 1))
    else {
      saveRDS(obj, file = fts)
      fs <- file.size(fts)
    }
  } else {
    ftL <- CacheStoredFile(cachePath, linkToCacheId)
    message("  (A file with identical properties already exists in the Cache; ",
            "creating a file.link instead of a new file)")
    if (identical(tolower(.Platform$OS.type), "windows")) {
      file.link(from = ftL, to = fts)
    } else {
      file.symlink(from = ftL, to = fts)
    }
    fs <- file.size(fts)
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
    if (fsBig) {
      browser(expr = exists("._saveToCache_3"))
      message("Object with cacheId ", cacheId, " appears to have a much larger size ",
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
  a <- retry(retries = 250, exponentialDecayBase = 1.01, quote(
    dbAppendTable(conn, CacheDBTableName(cachePath, drv), dt)))

  return(obj)
}

#' @export
#' @rdname cacheTools
#' @importFrom qs qread
loadFromCache <- function(cachePath, cacheId) {
  if (getOption("reproducible.cacheSaveFormat", "rds") == "qs")
    qs::qread(file = CacheStoredFile(cachePath, cacheId),
              nthreads = getOption("reproducible.nThreads", 1))
  else
    readRDS(file = CacheStoredFile(cachePath, cacheId))
}

#' Low level tools to work with Cache
#'
#' @importFrom DBI dbClearResult dbSendStatement dbBind dbAppendTable
#' @export
#' @rdname cacheTools
rmFromCache <- function(cachePath, cacheId, drv = getOption("reproducible.drv", RSQLite::SQLite()),
                        conn = getOption("reproducible.conn", NULL)) {
  if (is.null(conn)) {
    conn <- dbConnectAll(drv, cachePath = cachePath, create = FALSE)
    on.exit(dbDisconnect(conn))
  }
  # from https://cran.r-project.org/web/packages/DBI/vignettes/spec.html
  query <- glue::glue_sql(
    "DELETE FROM {DBI::SQL(double_quote(dbTabName))} WHERE \"cacheId\" IN ({cacheId*})",
    dbTabName = CacheDBTableName(cachePath, drv),
    cacheId = cacheId,
    .con = conn)
  res <- dbSendQuery(conn, query)

  if (FALSE)   { # this is the "unsafe" version
    query <- paste0("DELETE FROM \"", CacheDBTableName(cachePath, drv), "\" WHERE \"cacheId\" = $1")
    res <- dbSendStatement(conn, query)
    dbBind(res, list(cacheId))
  }

  dbClearResult(res)

  unlink(file.path(cachePath, "cacheObjects",
                   paste0(cacheId, ".", getOption("reproducible.cacheSaveFormat", "rds"))))
}

dbConnectAll <- function(drv = getOption("reproducible.drv", RSQLite::SQLite()), cachePath,
                         conn = getOption("reproducible.conn", NULL), create = TRUE) {
  args <- list(drv = drv)
  browser(expr = exists("yyyy"))
  if (is(drv, "SQLiteDriver")) {
    # if (!CacheIsACache(cachePath = cachePath, drv = drv, conn = conn))
    #   if (isFALSE(create)) {
    #     return(invisible())
    #   }
    args <- append(args, list(dbname = CacheDBFile(cachePath, drv = drv, conn = conn),
                              synchronous = NULL))
  } # other types of drv, e.g., Postgres can be done via env vars
  do.call(dbConnect, args)
}

.emptyCacheTable <- data.table(cacheId = character(), tagKey = character(),
                               tagValue = character(), createdDate = character())

#' @importFrom DBI dbSendStatement dbClearResult
.addTagsRepo <- function(cacheId, cachePath, tagKey = character(), tagValue = character(),
                         drv = getOption("reproducible.drv", RSQLite::SQLite()),
                         conn = getOption("reproducible.conn", NULL)) {
  browser(expr = exists("._addTagsRepo_1"))
  if (length(cacheId) > 0) {
    if (length(cacheId) > 1) stop(".addTagsRepo can only handle appending 1 tag at a time")
    if (useDBI()) {
      if (is.null(conn)) {
        conn <- dbConnectAll(drv, cachePath = cachePath, create = FALSE)
        on.exit(dbDisconnect(conn))
      }
      curTime <- as.character(Sys.time())
      if (length(tagKey) < length(cacheId))
        tagKey <- "accessed"
      if (length(tagValue) < length(cacheId))
        tagValue <- curTime

      # This is what the next code pair of lines does
      # dt <- data.table("cacheId" = cacheId, "tagKey" = "accessed",
      #                 "tagValue" = as.character(Sys.time()),
      #                 "createdDate" = as.character(Sys.time()))
      #
      # retry(quote(dbAppendTable(conn, CacheDBTableName(cachePath, drv), dt), retries = 15))
      rs <- retry(retries = 250, exponentialDecayBase = 1.01, quote(
        dbSendStatement(
          conn,
          paste0("insert into \"", CacheDBTableName(cachePath, drv), "\"",
                 " (\"cacheId\", \"tagKey\", \"tagValue\", \"createdDate\") values ",
                 "('", cacheId,
                 "', '",tagKey,"', '", tagValue, "', '", curTime, "')"))
      ))

      dbClearResult(rs)
    } else {
      written <- 0
      while (written >= 0) {
        saved <- suppressWarnings(try(silent = TRUE,
                                      archivist::addTagsRepo(cacheId,
                                                             repoDir = cachePath,
                                                             tags = paste0("accessed:", Sys.time()))))
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

.updateTagsRepo <- function(cacheId, cachePath, tagKey = character(), tagValue = character(),
                            add = TRUE,
                            drv = getOption("reproducible.drv", RSQLite::SQLite()),
                            conn = getOption("reproducible.conn", NULL)) {
  browser(expr = exists("._updateTagsRepo_1"))
  if (length(cacheId) > 0) {
    if (length(cacheId) > 1) stop(".updateTagsRepo can only handle updating 1 tag at a time")
    if (useDBI()) {
      if (is.null(conn)) {
        conn <- dbConnectAll(drv, cachePath = cachePath, create = FALSE)
        on.exit(dbDisconnect(conn))
      }
      curTime <- as.character(Sys.time())
      if (length(tagKey) < length(cacheId)) {
        warning("tagKey and/or tagValue must both be supplied for .updateTagsRepo.")
        return(invisible())
      }

      # This is what the next code pair of lines does
      # dt <- data.table("cacheId" = cacheId, "tagKey" = "accessed",
      #                 "tagValue" = as.character(Sys.time()),
      #                 "createdDate" = as.character(Sys.time()))
      #
      # retry(quote(dbAppendTable(conn, CacheDBTableName(cachePath, drv), dt), retries = 15))
      rs <- #retry(retries = 250, exponentialDecayBase = 1.01, quote(
        dbSendStatement(
          conn,
          paste0("update \"", CacheDBTableName(cachePath, drv), "\"",
                 " set \"tagValue\" = \"",tagValue,"\" where ",
                 " \"cacheId\" = '",cacheId, "'", " AND \"tagKey\" = '",tagKey, "'"))
      #))
      affectedAnyRows <- DBI::dbGetRowsAffected(rs) > 0
      dbClearResult(rs)
      if (!affectedAnyRows) {
        if (isTRUE(add)) {
          .addTagsRepo(cacheId, cachePath, tagKey, tagValue, drv = drv, conn = conn)
        }
      }
    } else {
      warning("updateTagsRepo not implemented when useDBI = FALSE")
    }

  }

}
.cacheNumDefaultTags <- function() {
  if (useDBI()) 8 else 12
}

.ignoreTagKeys <- function() {
  c("preDigest", "otherFunctions", "accessed", "elapsedTimeLoad")
}

.cacheTableHashColName <- function() {
  if (useDBI()) {
    "cacheId"
  } else {
    "artifact"
  }
}

.cacheTableTagColName <- function(option = NULL) {
  out <- "tagValue"
  if (useDBI()) {
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
CacheDBFile <- function(cachePath, drv = getOption("reproducible.drv", RSQLite::SQLite()),
                        conn = getOption("reproducible.conn", NULL)) {

  type <- gsub("Driver", "", class(drv))

  if (useDBI()) {
    if (!is.null(conn)) {
      type <- gsub("Connection", "", class(conn))
    }
  }

  if (grepl(type, "SQLite")) {
    if (useDBI()) {
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
  if (useDBI()) {
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
  csf <- if (isTRUE(useDBI()) == FALSE) {
    "rda"
  } else {
    getOption("reproducible.cacheSaveFormat", "rds")
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
CacheDBTableName <- function(cachePath,
                             drv = getOption("reproducible.drv", RSQLite::SQLite())) {
  if (!is(cachePath, "Path")) {
    cachePath <- asPath(cachePath, nParentDirs = 2)
  }
  if (useDBI()) {
    toGo <- attr(cachePath, "nParentDirs")
    cachePathTmp <- normPath(cachePath)
    newPath <- basename2(cachePathTmp)
    while (toGo > 1) {
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
CacheIsACache <- function(cachePath, create = FALSE,
                          drv = getOption("reproducible.drv", RSQLite::SQLite()),
                          conn = getOption("reproducible.conn", NULL)) {
  browser(expr = exists("._CacheIsACache_1"))
  if (useDBI()) {
    if (is.null(conn)) {
      conn <- dbConnectAll(drv, cachePath = cachePath)
      on.exit(dbDisconnect(conn))
    }
    type <- gsub("Connection", "", class(conn))
  }

  ret <- FALSE
  browser(expr = exists("jjjj"))
  ret <- all(basename2(c(CacheDBFile(cachePath, drv, conn), CacheStorageDir(cachePath))) %in%
               list.files(cachePath))
  if (useDBI()) {
    browser(expr = exists("._CacheIsACache_2"))
    if (ret) {
      tablesInDB <- retry(retries = 250, exponentialDecayBase = 1.01,
                          quote(dbListTables(conn)))
      tableShouldBe <- CacheDBTableName(cachePath)
      if (length(tablesInDB) == 1) {
        if (!any(tablesInDB %in% tableShouldBe) && grepl(type, "SQLite")) {
          warning(paste0("The table in the Cache repo does not match the cacheRepo. ",
                     "If this is because of a moved repository (i.e., files ",
                     "copied), press 'Enter' and it will be updated. ",
                     "If not, cache is in an error state. ",
                     "You may need to delete the Cache"))
          movedCache(cachePath, #old = tablesInDB,
                     drv = drv, conn = conn)
        }

      }
      ret <- ret && any(grepl(tableShouldBe, tablesInDB))
    }
  }

  if (useDBI()) {
    if (isFALSE(ret) && isTRUE(create)) {
      if (grepl(type, "Pq")) {
        file.create(CacheDBFile(cachePath, drv = drv, conn = conn))
      }
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
#' @importFrom DBI dbListTables
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
movedCache <- function(new, old, drv = getOption("reproducible.drv", RSQLite::SQLite()),
                       conn = getOption("reproducible.conn", NULL)) {
  if (useDBI()) {
    if (is.null(conn)) {
      conn <- dbConnectAll(drv, cachePath = new)
      on.exit(dbDisconnect(conn))
    }
  }
  tables <- dbListTables(conn)
  browser(expr = exists("._movedCache_2"))
  if (missing(old)) {
    if (length(tables) == 1) {
      message("Assuming old database table is ", tables)
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

  if (!identical(normPath(tables), normPath(oldTable))) {
    stop("The 'old' table name does not appear inside the path to the 'new'")
  }
  newTable <- CacheDBTableName(new, drv = drv)

  qry <- glue::glue_sql("ALTER TABLE {old} RENAME TO {new}",
                        old = oldTable,
                        new = newTable,
                        .con = conn)
  res <- retry(retries = 15, exponentialDecayBase = 1.01, quote(dbSendQuery(conn, qry)))
  dbFetch(res)
  dbClearResult(res)
}
