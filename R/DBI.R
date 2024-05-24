#' Low-level functions to create and work with a cache
#'
#' **These are intended for advanced use only.**
#'
#' @param cachePath A path describing the directory in which to create
#'   the database file(s)
#'
#' @inheritParams Cache
#'
#' @param drv A driver, passed to `dbConnect`
#'
#' @param force Logical. Should it create a cache in the `cachePath`,
#'   even if it already exists, overwriting.
#'
#' @details
#' - `createCache()` will create a Cache folder structure and necessary files, based on
#' the particular `drv` or `conn` provided;
#'
#' @return
#' - `createCache()` returns `NULL` (invisibly) and intended to be called for side effects;
#'
#' @export
#' @importFrom data.table data.table
#' @rdname CacheHelpers
#'
#' @examples
#' data.table::setDTthreads(2)
#' newCache <- tempdir2()
#' createCache(newCache)
#'
#' out <- Cache(rnorm(1), cachePath = newCache)
#' cacheId <- gsub("cacheId:", "", attr(out, "tags"))
#' loadFromCache(newCache, cacheId = cacheId)
#'
#' rmFromCache(newCache, cacheId = cacheId)
#'
#' # clean up
#' unlink(newCache, recursive = TRUE)
#'
createCache <- function(cachePath = getOption("reproducible.cachePath"),
                        drv = getDrv(getOption("reproducible.drv", NULL)),
                        conn = getOption("reproducible.conn", NULL), force = FALSE,
                        verbose = getOption("reproducible.verbose")) {
  alreadyExists <- CacheIsACache(cachePath, drv = drv, conn = conn, create = TRUE)
  if (alreadyExists && force == FALSE) {
    messageCache("Cache already exists at ", cachePath, " and force = FALSE. Not creating new cache.",
      verbose = verbose
    )
    return(invisible(cachePath))
  }

  checkPath(cachePath, create = TRUE)
  checkPath(CacheStorageDir(cachePath), create = TRUE)
  if (useDBI()) {
    .createCache(cachePath = cachePath, drv = drv, conn = conn)
  }

  invisible(NULL)
}

#' @keywords internal
.createCache <- function(cachePath, drv, conn) {
  if (is.null(conn)) {
    conn <- dbConnectAll(drv, cachePath = cachePath)
    on.exit(DBI::dbDisconnect(conn))
  }
  dt <- .emptyCacheTable

  # Some tough to find cases where stalls on dbWriteTable -- this *may* prevent some
  a <- retry(
    retries = 250, exponentialDecayBase = 1.01,
    quote(DBI::dbListTables(conn))
  )

  if (isTRUE(!CacheDBTableName(cachePath, drv) %in% a)) {
    # retry(retries = 5, exponentialDecayBase = 1.5, quote(
    try(DBI::dbWriteTable(conn, CacheDBTableName(cachePath, drv), dt,
      overwrite = FALSE,
      field.types = c(
        cacheId = "text", tagKey = "text",
        tagValue = "text", createdDate = "text"
      )
    ), silent = TRUE)
  }
  # )
}

#' Save an object to Cache
#'
#' This is not expected to be used by a user as it requires that the `cacheId` be
#' calculated in exactly the same as it calculated inside `Cache`
#' (which requires `match.call` to match arguments with their names, among other things).
#'
#' @inheritParams Cache
#'
#' @param cacheId The hash string representing the result of `.robustDigest`
#'
#' @param obj The R object to save to the cache
#'
#' @param linkToCacheId Optional. If a `cacheId` is provided here, then a `file.link`
#'   will be made to the file with that `cacheId` name in the cache repo.
#'   This is used when identical outputs exist in the cache. This will save disk space.
#'
#' @return
#' This is used for its side effects, namely, it will add the object to the cache and
#' cache database.
saveToCache <- function(cachePath = getOption("reproducible.cachePath"),
                        drv = getDrv(getOption("reproducible.drv", NULL)),
                        conn = getOption("reproducible.conn", NULL), obj, userTags, cacheId,
                        linkToCacheId = NULL,
                        verbose = getOption("reproducible.verbose")) {
  if (useDBI()) {
    if (is.null(conn)) {
      conn <- dbConnectAll(drv, cachePath = cachePath)
      on.exit(DBI::dbDisconnect(conn))
    }
  }

  if (missing(userTags)) userTags <- otherFunctions
  if (length(userTags) == 0) userTags <- otherFunctions
  if (NCOL(userTags) > 1) {
    tagKey <- userTags$tagKey
    tagValue <- userTags$tagValue
  } else {
    tagKey <- sub(userTags, pattern = ":.*$", replacement = "")
    tagValue <- sub(userTags, pattern = "^[^:]*:", replacement = "")
  }

  fts <- CacheStoredFile(cachePath, cacheId, obj = obj)

  # TRY link first, if there is a linkToCacheId, but some cases will fail; not sure what these cases are
  if (!is.null(linkToCacheId)) {
    ftL <- CacheStoredFile(cachePath, linkToCacheId)
    suppressWarnings({
      out <- try(file.link(from = ftL, to = fts), silent = TRUE)
    })
    if (is(out, "try-error") || !all((out %in% TRUE))) {
      linkToCacheId <- NULL
    } else {
      messageCache("  (A file with identical properties already exists in the Cache: ", basename(ftL), "; ")
      messageCache("    The newly added (", basename(fts), ") is a file.link to that file)",
        verbose = verbose
      )
    }
    fs <- file.size(fts)
  }

  # Save to db file first, then storage file
  dt <- data.table(
    "cacheId" = cacheId, "tagKey" = tagKey,
    "tagValue" = tagValue, "createdDate" = as.character(Sys.time())
  )
  if (!useDBI()) {
    dtFile <- saveDBFileSingle(dt = dt, cachePath, cacheId)
  } else {
    a <- retry(retries = 250, exponentialDecayBase = 1.01, quote(
      DBI::dbAppendTable(conn, CacheDBTableName(cachePath, drv), dt)
    ))
  }

  if (is.null(linkToCacheId)) {
    fs <- saveFilesInCacheFolder(cachePath = cachePath, obj, fts, cacheId = cacheId)
  }
  if (isTRUE(getOption("reproducible.useMemoise"))) {
    obj <- .unwrap(obj, cachePath, cacheId, drv, conn) # This takes time, but whether it happens now or later, same
    obj2 <- makeMemoisable(obj)
    assign(cacheId, obj2, envir = memoiseEnv(cachePath))
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
    objSize <- if (identical(unname(tagValue[whichOS]), "NA")) NA else as.numeric(tagValue[whichOS])
    fsBig <- (objSize * 4) < fs
    if (isTRUE(fsBig)) {
      messageCache("Object with cacheId ", cacheId, " appears to have a much larger size ",
        "on disk than in memory. ",
        "This usually means that the object has captured an environment with ",
        "many objects due to how a function or a formula is defined. ",
        "Usually, a solution involves using quote and eval around the formulas ",
        "and defining functions in a package or otherwise clean space, ",
        "i.e., not inside another function.\n",
        "See http://adv-r.had.co.nz/memory.html#gc and 'capturing environments'.",
        verbose = verbose
      )
    }
  }

  return(obj)
}

#' @inheritParams CacheStoredFile
#'
#' @param fullCacheTableForObj The result of `showCache`, but subsetted for only
#'   the `cacheId` being loaded or selected
#'
#' @param .dotsFromCache Optional. Used internally.
#'
#' @param .functionName Optional. Used for messaging when this function is called from `Cache`
#'
#' @param preDigest The list of `preDigest` that comes from `CacheDigest` of an object
#'
#' @details
#' - `loadFromCache()` retrieves a single object from the cache, given its `cacheId`;
#'
#' @return
#' - `loadFromCache()` returns the object from the cache that has the particular `cacheId`;
#'
#' @export
#' @rdname CacheHelpers
loadFromCache <- function(cachePath = getOption("reproducible.cachePath"),
                          cacheId, preDigest,
                          fullCacheTableForObj = NULL,
                          format = getOption("reproducible.cacheSaveFormat", "rds"),
                          .functionName = NULL, .dotsFromCache = NULL,
                          drv = getDrv(getOption("reproducible.drv", NULL)),
                          conn = getOption("reproducible.conn", NULL),
                          verbose = getOption("reproducible.verbose")) {
  if (verbose > 3) {
    startLoadTime <- Sys.time()
  }

  if (length(cacheId) > 1) {
    cacheId <- unique(cacheId)
  }

  isMemoised <- .isMemoised(cacheId, cachePath = cachePath)
  # isMemoised <- NA
  # if (isTRUE(getOption("reproducible.useMemoise"))) {
  #   isMemoised <- exists(cacheId, envir = memoiseEnv(cachePath))
  if (isTRUE(isMemoised)) {
    obj <- get(cacheId, envir = memoiseEnv(cachePath))
    obj <- unmakeMemoisable(obj)
  }
  # }

  if (!isTRUE(isMemoised)) {
    # Put this in a loop -- try the format that the user requested, but switch back if can't do it
    for (i in 1:2) {
      f <- CacheStoredFile(cachePath, cacheId, format)
      f <- unique(f) # It is OK if there is a vector of unique cacheIds e.g., loadFromCache(showCache(userTags = "hi")$cacheId)

      # First test if it is correct format
      if (!all(file.exists(f))) {
        sameCacheID <- dir(dirname(f), pattern = filePathSansExt(basename(f)))
        if (!useDBI() || length(sameCacheID) > 1) {
          sameCacheID <- onlyStorageFiles(sameCacheID)
        }

        if (length(sameCacheID)) {
          # if (!identical(whereInStack("sim"), .GlobalEnv)) {
          #   browser()
          #   format <- setdiff(c("rds", "qs"), format)
          #   message("User tried to change options('reproducible.cacheSaveFormat') for an ",
          #           "existing cache, while using a simList. ",
          #           "This currently does not work. Keeping the ",
          #           "option at: ", format)
          #   next
          # }

          messageCache("     (Changing format of Cache entry from ", fileExt(sameCacheID), " to ",
                       fileExt(f), ")",
                       verbose = verbose
          )
          obj <- loadFromCache(
            cachePath = cachePath, fullCacheTableForObj = fullCacheTableForObj,
            cacheId = cacheId,
            format = fileExt(sameCacheID),
            preDigest = preDigest,
            verbose = verbose
          )

          obj2 <- .wrap(obj, cachePath = cachePath, drv = drv, conn = conn)
          fs <- saveToCache(
            obj = obj2, cachePath = cachePath, drv = drv, conn = conn,
            cacheId = cacheId
          )
          rmFromCache(
            cachePath = cachePath, cacheId = cacheId, drv = drv, conn = conn,
            format = fileExt(sameCacheID)
          )
          return(obj)
        }
      }
      # Need exclusive lock
      obj <- loadFile(f)
      obj <- .unwrap(obj,
                     cachePath = cachePath,
                     cacheId = cacheId,
                     drv = drv, conn = conn
      )
      break # if you got this far, then break out of the for i loop
    }
  }

  # Class-specific message
  useMemoise <- if (getOption("reproducible.useMemoise") %in% TRUE) TRUE else NA
  fromMemoise <- isMemoised && useMemoise
  loadFromMgs <- .cacheMessage(obj, .functionName, fromMemoise = fromMemoise, verbose = verbose)

  # # This allows for any class specific things
  obj <- do.call(.prepareOutput, args = append(list(obj, cachePath), .dotsFromCache))

  if (isTRUE(useMemoise) && !isTRUE(isMemoised)) {
  # if (isTRUE(getOption("reproducible.useMemoise")) && !isTRUE(isMemoised)) {
    obj2 <- makeMemoisable(obj)
    assign(cacheId, obj2, envir = memoiseEnv(cachePath))
  }

  if (verbose > 3) {
    endLoadTime <- Sys.time()
    verboseDF <- data.frame(
      functionName = .functionName,
      component = gsub("(.+)(ed)(.+) result from.+$", "\\1ing\\3", loadFromMgs),
      elapsedTime = as.numeric(difftime(endLoadTime, startLoadTime, units = "secs")),
      units = "secs",
      stringsAsFactors = FALSE
    )

    if (exists("verboseTiming", envir = .reproEnv)) {
      .reproEnv$verboseTiming <- rbind(.reproEnv$verboseTiming, verboseDF)
    }
  }

  obj
}

#' @param sc a cache tags `data.table` object
#' @param elem character string specifying a `tagKey` value to match
#' @param ifNot character (or NULL) specifying the return value to use if `elem` not matched
#'
#' @details
#' - `extractFromCache()` retrieves a single `tagValue` from the cache based on
#' the `tagKey` of `elem`;
#'
#' @return
#' - `extractFromCache()` returns the `tagValue` from the cache corresponding to `elem` if found,
#' otherwise the value of `ifNot`;
#'
#' @export
#' @rdname CacheHelpers
extractFromCache <- function(sc, elem, ifNot = NULL) {
  rowNum <- sc[["tagKey"]] %in% elem
  elemExtracted <- if (any(rowNum)) {
    sc[["tagValue"]][rowNum]
  } else {
    ifNot
  }
  elemExtracted
}

#' @details
#' - `rmFromCache()` removes one or more items from the cache, and updates the cache
#' database files.
#'
#' @return
#' - `rmFromCache()` returns `NULL` (invisibly) and is intended to be called for side effects;
#'
#' @export
#' @rdname CacheHelpers
rmFromCache <- function(cachePath = getOption("reproducible.cachePath"),
                        cacheId, drv = getDrv(getOption("reproducible.drv", NULL)),
                        conn = getOption("reproducible.conn", NULL),
                        format = getOption("reproducible.cacheSaveFormat", "rds")) {
  if (useDBI()) {
    if (is.null(conn)) {
      conn <- dbConnectAll(drv, cachePath = cachePath, create = FALSE)
      on.exit(DBI::dbDisconnect(conn))
    }
    # from https://cran.r-project.org/web/packages/DBI/vignettes/spec.html
    query <- glue::glue_sql(
      "DELETE FROM {DBI::SQL(glue::double_quote(dbTabName))} WHERE \"cacheId\" IN ({cacheId*})",
      dbTabName = CacheDBTableName(cachePath, drv),
      cacheId = cacheId,
      .con = conn
    )
    res <- DBI::dbSendQuery(conn, query)

    if (FALSE) { # this is the "unsafe" version
      query <- paste0("DELETE FROM \"", CacheDBTableName(cachePath, drv), "\" WHERE \"cacheId\" = $1")
      res <- DBI::dbSendStatement(conn, query)
      DBI::dbBind(res, list(cacheId))
    }

    DBI::dbClearResult(res)
  } else {
    dtFile <- CacheDBFileSingle(cachePath = cachePath, cacheId = cacheId, format = format)
    unlink(dtFile)
  }
  unlink(CacheStoredFile(cachePath, cacheId = cacheId, format = format))
}

dbConnectAll <- function(drv = getDrv(getOption("reproducible.drv", NULL)),
                         cachePath = getOption("reproducible.cachePath"),
                         conn = getOption("reproducible.conn", NULL), create = TRUE,
                         verbose = getOption("reproducible.verbose")) {
  args <- list(drv = drv)
  if (is(drv, "SQLiteDriver")) {
    args <- append(args, list(
      dbname = CacheDBFile(cachePath, drv = drv, conn = conn),
      synchronous = NULL
    ))
  }
  conn <- try(do.call(DBI::dbConnect, args), silent = TRUE)
  if (is(conn, "try-error")) {
    messageCache("There is no Cache at this location", verbose = verbose)
    return(invisible(NULL))
  }
  conn
}

.emptyCacheTable <- data.table::data.table(
  cacheId = character(), tagKey = character(),
  tagValue = character(), createdDate = character()
)

.addTagsRepo <- function(cacheId, cachePath = getOption("reproducible.cachePath"),
                         tagKey = character(), tagValue = character(),
                         drv = getDrv(getOption("reproducible.drv", NULL)),
                         conn = getOption("reproducible.conn", NULL)) {
  if (length(cacheId) > 0) {
    if (length(cacheId) > 1) stop(".addTagsRepo can only handle appending 1 tag at a time")
    curTime <- as.character(Sys.time())
    if (length(tagKey) < length(cacheId)) {
      tagKey <- "accessed"
    }
    if (length(tagValue) < length(cacheId)) {
      tagValue <- curTime
    }

    if (useDBI()) {
      if (is.null(conn)) {
        conn <- dbConnectAll(drv, cachePath = cachePath, create = FALSE)
        on.exit(DBI::dbDisconnect(conn))
      }

      # This is what the next code pair of lines does
      # dt <- data.table("cacheId" = cacheId, "tagKey" = "accessed",
      #                 "tagValue" = as.character(Sys.time()),
      #                 "createdDate" = as.character(Sys.time()))
      #
      # retry(quote(dbAppendTable(conn, CacheDBTableName(cachePath, drv), dt), retries = 15))
      rs <- retry(retries = 250, exponentialDecayBase = 1.01, quote(
        DBI::dbSendStatement(
          conn,
          paste0(
            "insert into \"", CacheDBTableName(cachePath, drv), "\"",
            " (\"cacheId\", \"tagKey\", \"tagValue\", \"createdDate\") values ",
            "('", cacheId,
            "', '", tagKey, "', '", tagValue, "', '", curTime, "')"
          )
        )
      ))

      DBI::dbClearResult(rs)
    } else {
      dt <- data.table(
        "cacheId" = cacheId, "tagKey" = tagKey,
        "tagValue" = tagValue,
        "createdDate" = as.character(Sys.time())
      )
      dtFile <- CacheDBFileSingle(cachePath = cachePath, cacheId = cacheId)
      dt2 <- loadFile(dtFile)
      dt <- rbindlist(list(dt2, dt))
      saveFilesInCacheFolder(dt, dtFile, cachePath = cachePath, cacheId = cacheId)
    }
  }
}

.updateTagsRepo <- function(cacheId, cachePath = getOption("reproducible.cachePath"),
                            tagKey = character(), tagValue = character(),
                            add = TRUE,
                            drv = getDrv(getOption("reproducible.drv", NULL)),
                            conn = getOption("reproducible.conn", NULL)) {
  if (length(cacheId) > 0) {
    curTime <- as.character(Sys.time())
    if (length(tagKey) < length(cacheId)) {
      warning("tagKey and/or tagValue must both be supplied for .updateTagsRepo.")
      return(invisible())
    }
    if (length(cacheId) > 1) stop(".updateTagsRepo can only handle updating 1 tag at a time")
    if (useDBI()) {
      if (is.null(conn)) {
        conn <- dbConnectAll(drv, cachePath = cachePath, create = FALSE)
        on.exit(DBI::dbDisconnect(conn))
      }

      # This is what the next code pair of lines does
      # dt <- data.table("cacheId" = cacheId, "tagKey" = "accessed",
      #                 "tagValue" = as.character(Sys.time()),
      #                 "createdDate" = as.character(Sys.time()))
      #
      # retry(quote(dbAppendTable(conn, CacheDBTableName(cachePath, drv), dt), retries = 15))
      rs <- # retry(retries = 250, exponentialDecayBase = 1.01, quote(
        DBI::dbSendStatement(
          conn,
          paste0(
            "update \"", CacheDBTableName(cachePath, drv), "\"",
            " set \"tagValue\" = '", tagValue, "' where ",
            " \"cacheId\" = '", cacheId, "'", " AND \"tagKey\" = '", tagKey, "'"
          )
        )
      # ))
      affectedAnyRows <- DBI::dbGetRowsAffected(rs) > 0
      DBI::dbClearResult(rs)
      if (!affectedAnyRows) {
        if (isTRUE(add)) {
          .addTagsRepo(cacheId, cachePath, tagKey, tagValue, drv = drv, conn = conn)
        }
      }
    } else {
      dt <- data.table(
        "cacheId" = cacheId, "tagKey" = tagKey,
        "tagValue" = tagValue,
        "createdDate" = as.character(Sys.time())
      )
      dtFile <- CacheDBFileSingle(cachePath = cachePath, cacheId = cacheId)
      dt2 <- loadFile(dtFile)
      tk <- tagKey
      alreadyThere <- sum(dt2$tagKey == tk & dt2$cacheId == cacheId)
      if (add && alreadyThere == 0) {
        dt2 <- rbindlist(list(dt2, dt))
      } else {
        set(dt2, which(dt2$tagKey == tk & dt2$cacheId == cacheId), "tagValue", dt$tagValue)
        # dt2[tagKey == tk & cacheId == cacheId, tagValue := dt$tagValue]
      }
      saveFilesInCacheFolder(dt2, dtFile, cachePath = cachePath, cacheId = cacheId)
    }
  }
}
.cacheNumDefaultTags <- function() {
  7 # else 12
}

.ignoreTagKeys <- function() {
  c("preDigest", otherFunctions, "accessed", "elapsedTimeLoad", "fromDisk", "origRaster", "cacheRaster")
}

.cacheTableHashColName <- function() {
  "cacheId"
}

.cacheTableTagColName <- function(option = NULL) {
  "tagValue"
}

#' @inheritParams Cache
#'
#' @inheritParams createCache
#'
#' @return
#' - `CacheDBFile()` returns the name of the database file for a given Cache,
#' when `useDBI() == FALSE`, or `NULL` if `TRUE`;
#' - `CacheDBFiles()` (i.e,. plural) returns the name of all the database files for
#' a given Cache when `useDBI() == TRUE`, or `NULL` if `FALSE`;
#' - `CacheStoredFile()` returns the file path to the file with the specified hash value,
#' This can be loaded to memory with e.g., [loadFile()].;
#'
#' @export
#' @rdname CacheHelpers
#'
#' @examples
#' data.table::setDTthreads(2)
#' newCache <- tempdir2()
#'
#' # Given the drv and conn, creates the minimum infrastructure for a cache
#' createCache(newCache)
#'
#' CacheDBFile(newCache) # identifies the database file
#' CacheStorageDir(newCache) # identifies the directory where cached objects are stored
#'
#' out <- Cache(rnorm(1), cachePath = newCache)
#' cacheId <- gsub("cacheId:", "", attr(out, "tags"))
#' CacheStoredFile(newCache, cacheId = cacheId)
#'
#' # The name of the table inside the SQL database
#' CacheDBTableName(newCache)
#'
#' CacheIsACache(newCache) # returns TRUE
#'
#' # clean up
#' unlink(newCache, recursive = TRUE)
CacheDBFile <- function(cachePath = getOption("reproducible.cachePath"),
                        drv = getDrv(getOption("reproducible.drv", NULL)),
                        conn = getOption("reproducible.conn", NULL)) {
  type <- gsub("Driver", "", class(drv))

  if (useDBI()) {
    if (!is.null(conn)) {
      type <- gsub("Connection", "", class(conn))
    }
    #   }

    if (grepl(type, "SQLite")) {
      file.path(cachePath, "cache.db")
    } else {
      file.path(cachePath, "cache.txt")
    }
  } else {
    file.path(cachePath, "multifileDB.txt")
  }
}

#' @return
#' - `CacheStorageDir()` returns the name of the directory where cached objects are stored;
#'
#' @export
#' @rdname CacheHelpers
CacheStorageDir <- function(cachePath = getOption("reproducible.cachePath")) {
  file.path(cachePath, "cacheOutputs")
}

#' @param obj The optional object that is of interest; it may have an attribute "saveRawFile"
#'   that would be important.
#'
#' @param cacheId The cacheId or otherwise digested hash value, as character string.
#'
#' @param format The text string representing the file extension used normally by
#'   different save formats; currently only `"rds"` or `"qs"`. Defaults
#'   to `getOption("reproducible.cacheSaveFormat", "rds")`
#'
#' @return
#' - `CacheStoredFile` returns the file path to the file with the specified hash value;
#'
#' @export
#' @rdname CacheHelpers
CacheStoredFile <- function(cachePath = getOption("reproducible.cachePath"), cacheId,
                            format = NULL, obj = NULL) {
  if (is.null(format)) format <- getOption("reproducible.cacheSaveFormat", "rds")
  if (missing(cacheId)) cacheId <- NULL
  if (any(format %in% "check")) {
    format <- formatCheck(cachePath, cacheId, format)
  }
  csf <- format
  csExtension <- if (isTRUE(any("qs" %in% csf))) {
    "qs"
  } else if (isTRUE(any("rds" %in% csf))) {
    "rds"
  } else {
    if (is.character(format)) {
      format
    } else {
      "rda"
    }
  }
  filename <- if (is.null(cacheId)) NULL else paste(cacheId, csExtension, sep = ".")
  if (length(cacheId) > 1) {
    filename <- vapply(filename, nextNumericName, FUN.VALUE = character(1))
    for (i in seq(filename[-1]) + 1) {
      filename[i] <- basename2(nextNumericName(filename[i - 1]))
    }
  }

  fns <- basename2(Filenames(obj, allowMultiple = TRUE))
  fns <- fns[nzchar(fns)]
  file.path(CacheStorageDir(cachePath), c(filename, fns))
}

#' @return
#' - `CacheDBTableName()` returns the name of the table inside the SQL database, if that
#' is being used;
#'
#' @export
#' @rdname CacheHelpers
CacheDBTableName <- function(cachePath = getOption("reproducible.cachePath"),
                             drv = getDrv(getOption("reproducible.drv", NULL))) {
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
  if (grepl("^[[:digit:]]", newPath)) {
    newPath <- paste0("_", newPath)
  }
  return(newPath)
}

#' @param create Logical. Currently only affects non \pkg{RSQLite} default drivers.
#'        If `TRUE` and there is no Cache database, the function will create one.
#'
#' @return
#' - `CacheIsACache()` returns a logical indicating whether the `cachePath` is currently
#' a `reproducible` cache database;
#'
#' @export
#' @rdname CacheHelpers
CacheIsACache <- function(cachePath = getOption("reproducible.cachePath"), create = FALSE,
                          drv = getDrv(getOption("reproducible.drv", NULL)),
                          conn = getOption("reproducible.conn", NULL)) {
  checkPath(cachePath, create = TRUE)
  if (useDBI()) {
    if (is.null(conn)) {
      conn <- dbConnectAll(drv, cachePath = cachePath)
      on.exit(DBI::dbDisconnect(conn))
    }
    type <- gsub("Connection", "", class(conn))
  }

  ret <- all(basename2(c(CacheDBFile(cachePath, drv, conn), CacheStorageDir(cachePath))) %in%
    list.files(cachePath))

  ## Need to check even if ret is TRUE because we may be in the process of changing
  convertDBbackendIfIncorrect(cachePath, drv, conn)

  needCreate <- FALSE
  if (useDBI()) {
    if (ret) {
      tablesInDB <- retry(
        retries = 250, exponentialDecayBase = 1.01,
        quote(DBI::dbListTables(conn))
      )
      tableShouldBe <- CacheDBTableName(cachePath)
      if (length(tablesInDB) == 1) {
        if (!any(tablesInDB %in% tableShouldBe) && grepl(type, "SQLite")) {
          warning(paste0(
            "The table in the Cache repo does not match the cachePath. ",
            "If this is because of a moved repository (i.e., files ",
            "copied), then it is being updated automatically. ",
            "If not, cache is in an error state. ",
            "You may need to delete the Cache"
          ))
          movedCache(cachePath, # old = tablesInDB,
            drv = drv, conn = conn
          )
        }
      }
      ret <- ret && any(grepl(tableShouldBe, tablesInDB))
    }

    if (isFALSE(ret) && isTRUE(create)) {
      if (grepl(type, "Pq")) {
        needCreate <- TRUE
      }
    }
  } else { # This is for DBI = FALSE
    if (isTRUE(create)) {
      needCreate <- TRUE
    }
  }
  if (isTRUE(needCreate)) {
    file.create(CacheDBFile(cachePath, drv = drv, conn = conn))
  }

  return(ret)
}

#' Deal with moved cache issues
#'
#' If a user manually copies a complete Cache folder (including the db file and rasters folder),
#' there are issues that must be addressed, depending on the Cache backend used.
#' If using DBI (e.g., RSQLite or Postgres), the db table must be renamed. Run
#' this function after a manual copy of a cache folder. See examples for one way to do that.
#'
#' @param  new Either the path of the new `cachePath` where the cache was moved or copied to, or
#'   the new DB Table Name
#' @param  old Optional, if there is only one table in the `new` cache path.
#'   Either the path of the previous `cachePath` where the cache was moved or copied from, or
#'   the old DB Table Name
#' @inheritParams Cache
#' @export
#' @details
#' When the backend database for a `reproducinle` cache is an SQL database, the files
#' on disk cannot be copied manually to a new location because they contain internal
#' tables. Because `reproducible` gives the main table a name based on the `cachePath`
#' path, calls to `Cache` will attempt to call this internally if it detects a
#' name mismatch.
#' @return
#' `movedCache` does not return anything; it is called for its side effects.
#'
#' @examples
#' data.table::setDTthreads(2)
#' tmpdir <- "tmpdir"
#' tmpCache <- "tmpCache"
#' tmpCacheDir <- normalizePath(file.path(tempdir(), tmpCache), mustWork = FALSE)
#' tmpdirPath <- normalizePath(file.path(tempdir(), tmpdir), mustWork = FALSE)
#' bb <- Cache(rnorm, 1, cachePath = tmpCacheDir)
#'
#' # Copy all files from tmpCache to tmpdir
#' froms <- normalizePath(dir(tmpCacheDir, recursive = TRUE, full.names = TRUE),
#'   mustWork = FALSE
#' )
#' dir.create(file.path(tmpdirPath, "rasters"), recursive = TRUE, showWarnings = FALSE)
#' dir.create(file.path(tmpdirPath, "cacheOutputs"), recursive = TRUE, showWarnings = FALSE)
#' file.copy(
#'   from = froms, overwrite = TRUE,
#'   to = gsub(tmpCache, tmpdir, froms)
#' )
#'
#' # Can use 'movedCache' to update the database table, though will generally
#' #   happen automatically, with message indicating so
#' movedCache(new = tmpdirPath, old = tmpCacheDir)
#' bb <- Cache(rnorm, 1, cachePath = tmpdirPath) # should recover the previous call
#'
movedCache <- function(new, old, drv = getDrv(getOption("reproducible.drv", NULL)),
                       conn = getOption("reproducible.conn", NULL),
                       verbose = getOption("reproducible.verbose")) {
  if (useDBI()) {
    if (is.null(conn)) {
      conn <- dbConnectAll(drv, cachePath = new)
      on.exit(DBI::dbDisconnect(conn))
    }

    tables <- DBI::dbListTables(conn)
    if (missing(old)) {
      if (length(tables) == 1) {
        messageCache("Assuming old database table is ", tables,
          verbose = verbose
        )
      } else {
        dbname <- try(conn@dbname, silent = TRUE)
        if (is(dbname, "try-error")) {
          dbname <- "conn"
        }
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
      .con = conn
    )
    res <- retry(retries = 15, exponentialDecayBase = 1.01, quote(DBI::dbSendQuery(conn, qry)))
    # dbFetch(res)
    out <- DBI::dbClearResult(res)
  }
  return(invisible())
}


#' Load a file from the cache
#'
#' @param file character specifying the path to the file
#'
#' @param format (optional) character string specifying file extension "qs" or "rds" of `file`;
#'        if not specified (i.e., NULL), will be deduced from the file extension of `file`.
#'
#' @return the object loaded from `file`
#'
#' @export
loadFile <- function(file, format = NULL) {
  if (is.null(format)) {
    format <- fileExt(file)
  }
  isQs <- format %in% "qs"

  if (any(isQs)) {
    .requireNamespace("qs", stopOnFALSE = TRUE)
    obj <- qs::qread(file = file[isQs], nthreads = getOption("reproducible.nThreads", 1))
  } else {
    obj <- readRDS(file = file[!isQs])
  }

  obj
}

saveFilesInCacheFolder <- function(obj, fts, cachePath, cacheId) {
  if (missing(fts)) {
    fts <- CacheStoredFile(cachePath, cacheId = cacheId, obj = obj)
  }

  fsOther <- numeric()
  if (length(fts) > 1) {
    ftsOther <- fts[-1]
    fns <- Filenames(obj, allowMultiple = TRUE)
    hardLinkOrCopy(fns, ftsOther, verbose = -2)
    fsOther <- sum(file.size(ftsOther))
    fts <- fts[1]
  }
  if (getOption("reproducible.cacheSaveFormat", "rds") == "qs") {
    .requireNamespace("qs", stopOnFALSE = TRUE)
    for (attempt in 1:2) {
      fs <- qs::qsave(obj,
        file = fts,
        nthreads = getOption("reproducible.nThreads", 1),
        preset = getOption("reproducible.qsavePreset", "high")
      )
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
    fs <- sum(file.size(fts))
  }
  fs <- sum(fs, fsOther)


  fs
}

CacheDBFileSingle <- function(cachePath, cacheId,
                              format = getOption("reproducible.cacheSaveFormat")) {
  fullSuff <- CacheDBFileSingleExt(format = format)
  if (any(format %in% "check")) {
    format <- formatCheck(cachePath, cacheId, format)
    if (!is.null(format)) {
      fullSuff <- CacheDBFileSingleExt(format)
    }
  }
  out <- file.path(CacheStorageDir(cachePath), paste0(cacheId, fullSuff))
  out
}

CacheDBFileSingleExt <- function(format = getOption("reproducible.cacheSaveFormat")) {
  paste0(suffixMultipleDBFiles(), format)
}

suffixMultipleDBFiles <- function() {
  ".dbFile."
}

suffixLockFile <- function() ".lock"

onlyStorageFiles <- function(files) {
  grep(gsub("\\.", "\\\\.", paste0(suffixMultipleDBFiles(), "|", suffixLockFile())),
    files,
    invert = TRUE, value = TRUE
  )
}

formatCheck <- function(cachePath, cacheId, format) {
  altFile <- dir(dirname(CacheStoredFile(cachePath, cacheId)), pattern = cacheId)
  altFile <- onlyStorageFiles(altFile)
  if (length(altFile)) {
    format <- tools::file_ext(altFile)
  } else if (format == "check") {
    format <- getOption("reproducible.cacheSaveFormat")
  }
  format
}

getDrv <- function(drv = NULL) {
  if (useDBI()) {
    if (is.null(drv)) {
      if (!requireNamespace("RSQLite", quietly = TRUE)) {
        stop("Need RSQLite package when using DBI; install.packages('RSQLite')")
      }
      drv <- RSQLite::SQLite()
    }
  } else {
    drv <- NULL
  }
  drv
}

saveDBFileSingle <- function(dt, cachePath, cacheId) {
  dtFile <- CacheDBFileSingle(cachePath = cachePath, cacheId = cacheId)
  saveFilesInCacheFolder(dt, dtFile, cachePath = cachePath, cacheId = cacheId)
  dtFile
}

convertDBbackendIfIncorrect <- function(cachePath, drv, conn,
                                        verbose = getOption("reproducible.verbose")) {
  origDBI <- useDBI()
  newDBI <- suppressMessages(useDBI(!origDBI)) # switch to the other
  if (!identical(newDBI, origDBI)) { # if they are same, then DBI is not installed; not point proceeding
    on.exit(suppressMessages(useDBI(origDBI)))
    drv <- getDrv(drv) # This will return the DBI driver, if it is installed, regardless of drv
    DBFileWrong <- CacheDBFile(cachePath, drv, conn)
    if (file.exists(DBFileWrong)) {
      sc <- showCache(cachePath, drv = drv, conn = conn, verbose = -2)
      if (NROW(sc)) {
        messageCache("This cache repository previously was using a ",
          messConvert()[[as.character(useDBI())]], ".\n",
          "User has requested to change this using ",
          "e.g., `useDBI(", useDBI(), ")`. Converting now ...",
          verbose = verbose, verboseLevel = 1
        )
        if (isTRUE(origDBI)) { # using DBI --> convert all data to a DBI database
          suppressMessages(useDBI(origDBI))
          .createCache(cachePath, drv = drv, conn = conn)
          Map(tv = sc$tagValue, tk = sc$tagKey, oh = sc$cacheId, function(tv, tk, oh) {
            .addTagsRepo(
              cacheId = oh, cachePath = cachePath,
              tagKey = tk, tagValue = tv, drv = drv, conn = conn
            )
          })
          unlink(CacheDBFiles(cachePath))
        } else { # using multifile DB --> convert all data to multi-file backend
          singles <- split(sc, by = "cacheId")
          Map(dt = singles, ci = names(singles), function(dt, ci) {
            saveDBFileSingle(dt, cachePath = cachePath, cacheId = ci)
          })
        }
        messageCache("... Done!", verbose = verbose, verboseLevel = 1)
      }
      unlink(DBFileWrong)
    }
  }
}

messConvert <- function() {
  list(
    `TRUE` = c("multi-file backend"),
    `FALSE` = c("DBI backend")
  )
}

CacheDBFiles <- function(cachePath = getOption("reproducible.cachePath")) {
  ext <- CacheDBFileSingleExt()
  dtFiles <- dir(CacheStorageDir(cachePath), pattern = ext, full.names = TRUE)
  dtFiles
}

memoiseEnv <- function(cachePath, envir = .GlobalEnv) {
  memPersist <- isTRUE(getOption("reproducible.memoisePersist", NULL))
  if (memPersist) {
    obj <- paste0(".reproducibleMemoise_", cachePath)
    if (!exists(obj, envir = envir))
      assign(obj, new.env(parent = emptyenv()), envir = envir)
    memEnv <- get(obj, envir = envir, inherits = FALSE)
  } else {
    if (is.null(.pkgEnv[[cachePath]])) {
      .pkgEnv[[cachePath]] <- new.env(parent = emptyenv())
    }
    memEnv <- .pkgEnv[[cachePath]]
  }
  memEnv
}


otherFunctions <- "otherFunctions"

#' Evaluate whether a cacheId is memoised
#'
#' Intended for internal use. Exported so other packages can use this function.
#'
#' @inheritParams Cache
#' @return A logical, length 1 indicating whether the `cacheId` is memoised.
#'
#' @export
.isMemoised <- function(cacheId, cachePath = getOption("reproducible.cachePath")) {
  isMemoised <- NA
  if (isTRUE(getOption("reproducible.useMemoise"))) {
    isMemoised <- exists(cacheId, envir = memoiseEnv(cachePath))
  }
  isMemoised
}
