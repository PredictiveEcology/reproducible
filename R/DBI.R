#' Create a new cache
#'
#' @param cachePath A path describing the directory in which to create
#'   the database file(s)
#' @param drv A driver, passed to \code{dbConnect}
#' @param force Logical. Should it create a cache in the \code{cachePath},
#'   even if it already exists, overwriting.
#' @importFrom data.table data.table
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
#' @param cacheId The hash string representing the result of \code{CacheDigest(...)[[1]]}
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

  return(list(dt, conn, drv, cachePath))
  #return(obj)
}

#' @export
#' @rdname cache-tools
#' @inheritParams CacheStoredFile
loadFromCache <- function(cachePath = getOption("reproducible.cachePath"),
                          cacheId,
                          format = getOption("reproducible.cacheSaveFormat", "rds"),
                          sideEffect = FALSE,
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
        rmFromCache(cacheRepo = cachePath, cacheIds = cacheId, drv = drv, conn = conn,
                    format = fileExt(sameCacheID))
        return(fs)
      }
    }
    obj <- loadFile(f, format = format)
  }
  obj <- dealWithClassOnRecovery(obj, cacheRepo = cachePath,
                                 cacheId = cacheId, sideEffect = sideEffect,
                                 drv = drv, conn = conn)

  if (isTRUE(getOption("reproducible.useMemoise")) && !isTRUE(isMemoised)) {
    assign(cacheId, obj, envir = .pkgEnv[[cachePath]])
  }
  obj

}

#' Low level tools to work with Cache
#'
#' @export
#' @inheritParams Cache
#' @inheritParams clearCache
#' @param cacheIds Either a character string with the cacheId of item to remove from
#'   Cache repository, or a data.table coming from `showCache` with the items to remove.
#' @param dbTabNam For internal use only.
#' @rdname cache-tools
rmFromCache <- function(cacheRepo = getOption("reproducible.cachePath"),
                        cacheIds, ask = getOption("reproducible.ask"),
                        verbose = getOption("reproducible.verbose"),
                        drv = getOption("reproducible.drv"),
                        conn = getOption("reproducible.conn", NULL),
                        format = getOption("reproducible.cacheSaveFormat", "rds"),
                        dbTabNam = NULL, ...) {
  if (is.null(conn)) {
    conn <- dbConnectAll(drv, cachePath = cacheRepo, create = FALSE)
    on.exit(dbDisconnectAll(conn, shutdown = TRUE))
  }

  if (is(cacheIds, "character") || !is.null(list(...)$cacheId)) {
    cacheId <- cacheIds
    cacheIds <- showCache(cacheRepo, userTags = cacheIds, drv = drv, conn = conn, dbTabNam = dbTabNam)
  } else {
    cacheId <- unique(cacheIds$cacheId)
  }

  filesToRemove <- CacheDTFilesAll(cacheRepo = cacheRepo, cacheIds, format = format)
  if (isInteractive() ) {
    fileSizes <- file.size(filesToRemove)
    nas <- is.na(fileSizes)
    if (any(!nas)) {
      cacheSize <- sum(fileSizes[!nas])
      class(cacheSize) <- "object_size"
      formattedCacheSize <- format(cacheSize, "auto")
      if (isTRUE(ask)) {
        messageQuestion("Your size of your selected objects is ", formattedCacheSize, ".\n",
                        " Are you sure you would like to delete it all? Y or N")
        rl <- readline()
        if (!identical(toupper(rl), "Y")) {
          messageCache("Aborting clearCache", verbose = verbose)
          return(invisible())
        }
      }
    }
  }

  rmFromCacheDBFileAll(cacheRepo, drv, cacheId, conn, dbTabNam = dbTabNam)
  rmFromCacheStorage(filesToRemove, format = format)
  rmFromCacheMemoise(cacheRepo, cacheId)

}

dbConnectAll <- function(drv = getOption("reproducible.drv"),
                         cachePath = getOption("reproducible.cachePath"),
                         conn = getOption("reproducible.conn", NULL), create = TRUE,
                         read_only = FALSE) {

  if (identical(drv, "csv") ) {
    conn <- CacheDBFile(cachePath, drv = drv, conn = conn)
    return(conn)
  }
  args <- list(drv = drv)
  if (is(drv, "SQLiteDriver")) {
    args <- append(args, list(dbname = CacheDBFile(cachePath, drv = drv, conn = conn),
                              synchronous = NULL))
  }
  # other types of drv, e.g., Postgres can be done via env vars
  conn <- do.call(DBI::dbConnect, args)
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
                         conn = getOption("reproducible.conn", NULL),
                         tbNam = NULL) {
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
    rs <- addTagsAll(conn, cachePath, drv, cacheId, tagKey, tagValue, curTime, tbNam)


  }
}

.updateTagsRepo <- function(cacheId, cachePath = getOption("reproducible.cachePath"),
                            tagKey = character(), tagValue = character(),
                            add = TRUE,
                            drv = getOption("reproducible.drv"),
                            conn = getOption("reproducible.conn", NULL)) {
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
  9
}

.ignoreTagKeys <- function() {
  repNS <- asNamespace("reproducible");
  knownTags <- unname(unlist(mget(ls(pattern = "userTag_", envir = repNS),
                                  envir = repNS, inherits = FALSE)))
  c("prerun", "preDigest", "otherFunctions", "accessed", "elapsedTimeLoad",
    "fromDisk", knownTags)
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

  if (exists("aaa")) browser()
  outFile <- if (grepl(type, "SQLite")) {
    file.path(cachePath, dbFileSQLite)
  } else if (grepl(type, "character")) {
    if (identical(drv, "csv")) {
      file.path(cachePath, dbFilecsv)
    } else {
      stop(errMessWrongDrv)
    }
  }
  outFile
}

errMessWrongDrv <- "Currently can only use 'csv', RSQLite::SQLite(), or RPostgres::Postgres()"

#' @rdname CacheHelpers
#' @param sub A character string indicating the sub-folder to use within the Cache Repository
#'   to store the binary files.
#' @export
CacheStorageDir <- function(cachePath = getOption("reproducible.cachePath"),
                            sub = getOption("reproducible.cacheStorageFolder", "cacheOutputs")) {
  file.path(cachePath, sub)
}

#' @rdname CacheHelpers
#' @export
CacheStorageRasterDir <- function(cachePath = getOption("reproducible.cachePath"),
                                  sub = getOption("reproducible.cacheRasterFolder", "cacheOutputs")) {
  file.path(cachePath, sub)
}

#' @rdname CacheHelpers
#' @export
CacheStorageSideEffectsDir <- function(cachePath = getOption("reproducible.cachePath"),
                                       sub = getOption("reproducible.cacheSideEffectsFolder", "cacheOutputs")) {
  file.path(cachePath, sub)
}


#' @rdname CacheHelpers
#' @export
CacheStorageDirs <- function(cachePath = getOption("reproducible.cachePath")) {
  c(CacheStorageDir(cachePath), CacheStorageRasterDir(cachePath),
    CacheStorageSideEffectsDir(cachePath))
}

#' @details
#' \code{CacheStoredFile} returns the file path to the file with the specified hash value.
#'
#' @rdname CacheHelpers
#' @export
#' @param cacheId The cacheId or otherwise digested hash value, as character string.
#' @param format The text string representing the file extension used normally by
#'   different save formats; currently only \code{"rds"} or \code{"qs"}. Defaults
#'   to \code{getOption("reproducible.cacheSaveFormat", "rds")}
CacheStoredFile <- function(cachePath = getOption("reproducible.cachePath"), cacheId,
                            format = getOption("reproducible.cacheSaveFormat", "rds")) {
  csf <- format
  csExtension <- if (csf == "qs") {
    "qs"
  } else if (csf == "rds") {
    "rds"
  } else {
    "rda"
  }
  filename <- paste(cacheId, csExtension, sep = ".")
  file.path(CacheStorageDir(cachePath), filename)
}

#' @rdname CacheHelpers
#' @export
#' @inheritParams CacheStoredFile
#' @param filename The text string representing the file filename (absolute or basename)
CacheStoredRasterFile <- function(cachePath = getOption("reproducible.cachePath"), cacheId = NULL,
                                  filename) {
  cacheId <- if (is.null(cacheId)) "" else paste0(cacheId, "_")
  file.path(CacheStorageRasterDir(cachePath), paste0(cacheId, basename(filename)))
}

#' @rdname CacheHelpers
#' @export
#' @inheritParams CacheStoredFile
#' @param filename The text string representing the file filename (absolute or basename)
CacheStoredSideEffectFile <- function(cachePath = getOption("reproducible.cachePath"), cacheId = NULL,
                                  filename) {
  cacheId <- if (is.null(cacheId)) "" else paste0(cacheId, "_")
  file.path(CacheStorageSideEffectsDir(cachePath), paste0(cacheId, basename(filename)))
}

#' @rdname CacheHelpers
#' @export
#' @inheritParams CacheStoredFile
#' @param filename The text string representing the file filename (absolute or basename)
CacheStoredFilesList <- function(cachePath = getOption("reproducible.cachePath"), cacheId = NULL) {
  stop("Not a function yet")
  dd <- dir(unique(CacheStorageDirs(cachePath)))
  browser()
  c(CacheStoredFile(cachePath, cacheId),
    CacheStoredFile(cachePath, cacheId),
    CacheStoredFile(cachePath, cacheId))
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
#' @param allowNoStorageFolder Logical. Not intended for user.
#'   If \code{FALSE} then this function may still return
#'   \code{TRUE} even if there is no outputs folder.
#' @param allowTBMismatch Logical. Not intended for user.
#'   There are 2 known reasons why a db table will not be named
#'   from the cacheRepo: the repo was moved or it is from a cloudCache situation,
#'   which is temporary.
#' @export
#' @details
#' \code{CacheIsACache} returns a logical of whether the specified cachePath
#'   is actually a functioning cache.
CacheIsACache <- function(cachePath = getOption("reproducible.cachePath"), create = FALSE,
                          allowNoStorageFolder = FALSE, allowTBMismatch = FALSE,
                          drv = getOption("reproducible.drv"),
                          conn = getOption("reproducible.conn", NULL)) {
  if (exists("ccc")) browser()
  checkPath(cachePath, create = TRUE)
  ret <- FALSE
  connIsNull <- is.null(conn)
  if (connIsNull) {
    conn <- dbConnectAll(drv, cachePath = cachePath)
    on.exit({
      if (exists("ddd")) browser()
      dbDisconnectAll(conn, shutdown = TRUE)
      }, add = TRUE)
  }

  type <- gsub("Connection", "", class(conn))

  filesPresent <- list.files(cachePath)
  dbFile <- CacheDBFile(cachePath, drv, conn)
  filesNeeded <- dbFile
  if (!allowNoStorageFolder) {
    filesNeeded <- c(filesNeeded, CacheStorageDir(cachePath))
  }
  filesNeededArePresent <- basename2(filesNeeded) %in% filesPresent
  switchedDBTable <- if (length(filesNeeded) >= 2) {
    filesNeededArePresent[2] && !filesNeededArePresent[1]
  } else {
    FALSE
  }

  pat <- paste0(basename2(CacheDBFile(cachePath = cachePath, drv = drv)),"|",
                basename2(unique(CacheStorageDirs(cachePath))))
  connFilePresentNotNeeded <- grep(dir(cachePath), pattern = pat, invert = TRUE, value = TRUE)

  if (length(connFilePresentNotNeeded) > 0) {
    relevantFiles <- connFilePresentNotNeeded %in% dbKnownFiles
    connFilePresentNotNeeded <- connFilePresentNotNeeded[relevantFiles]
  }

  if ( (all(!filesNeededArePresent) && length(connFilePresentNotNeeded) == 0) ||
       (!filesNeededArePresent[1] && length(connFilePresentNotNeeded) == 0) )
    return(ret)

  needWriteToConn <- FALSE #  If a db gets change, need to get it to disk, not just RAM
  if (switchedDBTable && identical(connFilePresentNotNeeded, dbFileSQLite)) {
    # means a switched backend -- folder of storage files exist, but no db frontend
    if (identical(drv, "csv") ) {
      if (!requireNamespace("RSQLite")) {
        stop("It looks like this Cache database used to be an RSQLite database, but RSQLite is not installed; please install.packages('RSQLite')")
      }
      drvOther <- RSQLite::SQLite()
      tmpConn_db <- dbConnectAll(drv = drvOther, cachePath = cachePath)
      on.exit({
        dbDisconnectAll(tmpConn_db, shutdown = TRUE)
      }
        , add = TRUE)
      sc <- suppressMessages(showCache(x = cachePath, conn = tmpConn_db))
      fileOther_db <- CacheDBFile(cachePath, conn = tmpConn_db)
      on.exit({
        if (file.exists(fileOther_db)) {
          suppressWarnings(try(file.remove(fileOther_db), silent = TRUE))
        }
      }
      , add = TRUE)
      needWriteToConn <- TRUE
    }
    filesNeededArePresent[1] <- file.exists(dbFile)
  }

  ret <- all(filesNeededArePresent)
  wrongDBFile <- !filesNeededArePresent[1]

  useSQLConn <- useSQL(conn)
  if (ret && useSQLConn && !allowTBMismatch || (wrongDBFile && !useSQLConn)) {
    retOrig <- ret
    for (i in 1:2) {
      # This length-2 loop is for cases where the backend db changes;
      # if it is intact, will break and only do 1
      if (useSQLConn) {
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
        ret <- retOrig && any(grepl(tableShouldBe, tablesInDB))
      }
      if (isFALSE(ret)) {
        # the files all existed, but there is no db Table; likely from a conversion from a file-based to SQLite
        createEmptyTable(conn, cachePath, drv)
        drvOther <- fileExt(connFilePresentNotNeeded)
        if (exists("ddd")) browser()
        if (identical(drvOther, "db")) drvOther <- RSQLite::SQLite()
        tmpConn <- dbConnectAll(drv = drvOther, cachePath = cachePath)
        on.exit({
          dbDisconnectAll(tmpConn, shutdown = TRUE)
        }
        , add = TRUE)
        sc <- suppressMessages(showCache(x = cachePath, drv = drvOther, conn = tmpConn))
        appendAll(conn, cachePath, drv, sc)
        needWriteToConn <- !useSQLConn
        fileOther <- CacheDBFile(cachePath, drv = drvOther, conn = tmpConn)
        on.exit({
          if (file.exists(fileOther))
            file.remove(fileOther)
        }
        , add = TRUE)
        ret <- TRUE

      } else {
        break
      }
    }

  }

  if (isTRUE(needWriteToConn))
    writeFilebasedConn(cachePath = cachePath, conn = conn, dt = sc, drv = drv)
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


addTagsAll <- function(conn, cachePath, drv, cacheId, tagKey, tagValue, curTime, tbNam = NULL) {
  if (useSQL(conn)) {
    if (is.null(tbNam)) {
      tbNam <- CacheDBTableName(cachePath, drv)
    }
    rs <- retry(retries = 250, exponentialDecayBase = 1.01, quote(
      DBI::dbSendStatement(
        conn,
        paste0("insert into \"", tbNam, "\"",
               " (\"cacheId\", \"tagKey\", \"tagValue\", \"createdDate\") values ",
               "('", cacheId,
               "', '", tagKey, "', '", tagValue, "', '", curTime, "')"))
    ))

    DBI::dbClearResult(rs)
  } else {
    dt <- setDT(list(cacheId = cacheId, tagKey = tagKey, tagValue = tagValue,
                     createdDate = curTime))
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
    fsTab <- finalizeDTtoWrite(conn = conn, dt = fsTab, objName = objName)
    affectedAnyRows <- which(fsTab$cacheId == cacheId & fsTab$tagKey == tagKey)
    if (length(affectedAnyRows)) {
      # fsTab[affectedAnyRows, tagValue := tagValue]
      writeFilebasedConnToMemory(conn = conn, objName = objName,
                                 dt = fsTab, update = fsTab[affectedAnyRows,])
    }
    affectedAnyRows <- length(affectedAnyRows) > 0
  }
  return(affectedAnyRows)
}

objNameFromConn <- function(conn) {
  gsub(":", "", gsub("/|\\\\", "_", conn))
}

readFilebasedConn <- function(objName, conn, columns = NULL, from = 1, to = NULL) {
  messageCache(Sys.getpid(), " readFilebasedConn -- start of", verboseLevel = 3, verbose = getOption("reproducible.verbose"))
  if (missing(objName)) {
    objName <- objNameFromConn(conn)
  }
  fsTab <- NULL
  exist <- try(exists(objName, envir = .pkgEnv, inherits = FALSE))
  if (is(exist, "try-error")) browser()
  if (exist) {
    fsTab <- get(objName, envir = .pkgEnv, inherits = FALSE)
  } else {
    # Read from disk, then assign right away to memory for all future reads
    if (file.exists(conn)) {
      tf <- tempfile()
      on.exit({if (file.exists(tf)) file.remove(tf)})
      messageCache(Sys.getpid(), " readFilebasedConn -- pre file.copy", verboseLevel = 3, verbose = getOption("reproducible.verbose"))
      file.copy(conn, tf, overwrite = FALSE)
      messageCache(Sys.getpid(), " readFilebasedConn -- pre read_fst", verboseLevel = 3, verbose = getOption("reproducible.verbose"))
      fsTab <- readFilebasedConnFile(tf)
      # fsTab <- setDF(data.table::fread(tf, colClasses = rep("character", 4)))
      messageCache(Sys.getpid(), " readFilebasedConn -- post read_fst", verboseLevel = 3, verbose = getOption("reproducible.verbose"))

      if (is(fsTab, "try-error"))
        stop("Something went wrong with accessing the Cache 2")

      if (!is(fsTab, "try-error")) {
        dig <- file.mtime(conn)
        messageCache(Sys.getpid(), " readFilebasedConn -- pre writeFilebasedConnToMemory", verboseLevel = 3, verbose = getOption("reproducible.verbose"))
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
  messageCache(Sys.getpid(), " readFilebasedConn -- end of", verboseLevel = 3, verbose = getOption("reproducible.verbose"))
  fsTab
}

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

finalizeDTtoWrite <- function(conn, dt, objName, deleteObjs = FALSE) {

  objNameDig <- objNameWithDig(objName)
  digPre <- get0(objNameDig, envir = .pkgEnv)
  if (!is.null(digPre)) {
    tf <- tempfile()
    on.exit({if (file.exists(tf)) file.remove(tf)})

    dt <- try(
      retry(retries = 10, exponentialDecayBase = 1.01, silent = TRUE, quote({

        digPost <- file.mtime(conn)
        if (!identical(digPost, digPre)) {
          file.copy(conn, tf, overwrite = TRUE)
          dt <- try(readFilebasedConn(conn = tf), silent = TRUE)
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
      if (isTRUE(deleteObjs))
        rm(list = objNameToAdd, envir = .pkgEnv)
    }
    objNameToUpdate <- objNameWithToUpdate(objName)
    ToUpdate <- get0(objNameToUpdate, envir = .pkgEnv)
    if (!is.null(ToUpdate)) {
      if (length(ToUpdate) > 1) {
        ToUpdate <- rbindlist(ToUpdate)
      } else {
        ToUpdate <- ToUpdate[[1]]
      }
      data.table::setorderv(ToUpdate, colnames(ToUpdate), order = -1L)
      data.table::setorderv(dt, colnames(ToUpdate), order = -1L)
      colsForUniq <- head(colnames(ToUpdate),3)
      dt <- unique(dt, by = colsForUniq)

      ToUpdate <- unique(ToUpdate, by = colsForUniq)

      lapply(seq(NROW(ToUpdate)), function(tu) {
        wh <- which(ToUpdate$cacheId[tu] == dt$cacheId & ToUpdate$tagKey[tu] == dt$tagKey)
        set(dt, wh, "tagValue", ToUpdate$tagValue[tu])
      })

      if (isTRUE(deleteObjs))
        rm(list = objNameToUpdate, envir = .pkgEnv)

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
    tab <- finalizeDTtoWrite(conn = conn, dt = tab, objName = objName,
                             deleteObjs = TRUE)
    lss <- ls(.pkgEnv);
    rm(list = lss[startsWith(lss, objName)], envir = .pkgEnv)
    writeFilebasedConn(conn = conn, dt = tab, objName = objName)
  }

}

writeFilebasedConn <- function(cachePath, drv, conn, dt, objName) {
  messageCache(Sys.getpid(), " writeFilebasedConn -- start of", verboseLevel = 3, verbose = getOption("reproducible.verbose"))

  if (!is.null(dt)) { # basically, if there was an error in the Cache function, there won't be anything here
    if (missing(conn))
      conn <- CacheDBFile(cachePath, drv = drv, conn = conn)
    tf <- tempfile()
    on.exit({if (file.exists(tf)) file.remove(tf)})

    rend <- FALSE
    if (exists("fff")) browser()
    retry(retries = 30, exponentialDecayBase = 1.01, silent = TRUE,
          quote({

            if (!rend || !file.exists(tf)) {
              messageCache(Sys.getpid(), " writeFilebasedConn -- pre write_fst", verboseLevel = 3, verbose = getOption("reproducible.verbose"))
              fsTab <- data.table::fwrite(dt, file = tf)
              # fsTab <- write_fst(dt, tf)
              messageCache(Sys.getpid(), " writeFilebasedConn -- post write_fst", verboseLevel = 3, verbose = getOption("reproducible.verbose"))
            }
            rend <- file.copy(tf, conn, overwrite = TRUE)
            messageCache(Sys.getpid(), " writeFilebasedConn -- post file.copy", verboseLevel = 3, verbose = getOption("reproducible.verbose"))
            if (!rend) {
              tf <- tempfile()
            }

            ## CHECK IT WORKED
            if (TRUE) {
              objName <- objNameFromConn(conn)
              rm(list = ls(.pkgEnv, pattern = objName), envir = .pkgEnv)
              check1 <- readFilebasedConn(conn = conn)
              un1 <- unique(dt$cacheId);
              un2 <- unique(check1$cacheId)
              aa <- un1[!un1 %in% un2]
              if (length(aa) != 0) {
                stop("Cache 3")
              }
            }

          }))


    if (file.exists(tf)) file.remove(tf)
  }
  messageCache(Sys.getpid(), " writeFilebasedConn -- end of", verboseLevel = 3, verbose = getOption("reproducible.verbose"))

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

rmFromCacheDBFileAll <- function(cachePath, drv, cacheId, conn, dbTabNam = NULL) {
  if (useSQL(conn)) {
    if (is.null(dbTabNam))
      dbTabNam <- CacheDBTableName(cachePath, drv)
    query <- glue::glue_sql(
      "DELETE FROM {DBI::SQL(glue::double_quote(dbTabNam))} WHERE \"cacheId\" IN ({cacheId*})",
      dbTabNam = dbTabNam,
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
    res <- retry(retries = 15, exponentialDecayBase = 1.01, quote(DBI::dbSendQuery(conn, qry)))
    # dbFetch(res)
    DBI::dbClearResult(res)
  } # csv does not need to rename a table, as there are no tables
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

rmFromCacheStorage <- function(filesToRemove, cacheRepo, cacheIds, format = getOption("reproducible.cacheSaveFormat", "rds")) {
  if (missing(filesToRemove))
    filesToRemove <- CacheDTFilesAll(CacheDT = cacheIds, cacheRepo = cacheRepo, format = format)
  unlink(filesToRemove)
}

rmFromCacheMemoise <- function(cacheRepo, cacheId) {
  if (isTRUE(getOption("reproducible.useMemoise")))
    if (exists(cacheRepo, envir = .pkgEnv, inherits = FALSE))
      suppressWarnings(rm(list = cacheId, envir = .pkgEnv[[cacheRepo]]))

}

readFilebasedConnFile <- function(file) {
  fe <- fileExt(file)
  if (identical(fe, ""))
    fe <- getOption("reproducible.drv")
  ret <- if (identical(fe, "csv"))
    data.table::fread(file, colClasses = rep("character", 4))
  setDF(ret)
}

writeFilebasedConnFile <- function(file, dt) {
  fe <- fileExt(file)
  if (identical(fe, ""))
    fe <- getOption("reproducible.drv")
  ret <- if (identical(fe, "csv"))
    data.table::fwrite(dt, file = file)

}


dbFileSQLite <- "cache.db"
dbFilecsv <- "cache.csv"
dbKnownFiles <- c(dbFilecsv, dbFileSQLite)
