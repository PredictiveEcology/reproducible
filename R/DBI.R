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
  if (getOption("reproducible.useDBI", TRUE)) {
    if (is.null(conn)) {
    conn <- dbConnectAll(drv, cachePath = cachePath)
    on.exit(dbDisconnect(conn))
    }
  }
  dt <- .emptyCacheTable

  retry(quote(dbWriteTable(conn, CacheDBTableName(cachePath, drv), dt, overwrite = TRUE,
                           field.types = c(cacheId = "text", tagKey = "text",
                                           tagValue = "text", createdDate = "text")))
  )
}

#' @rdname cacheTools
#' @inheritParams Cache
#' @param cacheId The hash string representing the result of \code{.robustDigest}
#' @param obj The R object to save to the cache
#' @importFrom qs qsave
saveToCache <- function(cachePath, drv = getOption("reproducible.drv", RSQLite::SQLite()),
                        conn = getOption("reproducible.conn", NULL), obj, userTags, cacheId) {
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
  if (getOption("reproducible.cacheSaveFormat", "qs") == "qs")
    fs <- qs::qsave(obj, file = fts, nthreads = getOption("reproducible.nThreads", 1))
  else {
    saveRDS(obj, file = fts)
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
  a <- retry(quote(dbAppendTable(conn, CacheDBTableName(cachePath, drv), dt)), retries = 15)

  return(obj)
}

#' @export
#' @rdname cacheTools
#' @importFrom qs qread
loadFromCache <- function(cachePath, cacheId) {
  if (getOption("reproducible.cacheSaveFormat", "qs") == "qs")
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
                   paste0(cacheId, ".", getOption("reproducible.cacheSaveFormat", "qs"))))
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
    args <- append(args, list(dbname = CacheDBFile(cachePath, drv = drv, conn = conn)))
  } # other types of drv, e.g., Postgres can be done via env vars
  do.call(dbConnect, args)
}

.emptyCacheTable <- data.table(cacheId = character(), tagKey = character(),
                               tagValue = character(), createdDate = character())

#' @importFrom DBI dbSendStatement dbClearResult
.addTagsRepo <- function(isInRepo, cachePath, lastOne,
                         drv = getOption("reproducible.drv", RSQLite::SQLite()),
                         conn = getOption("reproducible.conn", NULL)) {
  browser(expr = exists("xxxx"))
  if (getOption("reproducible.useDBI", TRUE)) {
    if (is.null(conn)) {
      conn <- dbConnectAll(drv, cachePath = cachePath, create = FALSE)
      on.exit(dbDisconnect(conn))
    }

    # This is what the next code pair of lines does
    # dt <- data.table("cacheId" = isInRepo$cacheId[lastOne], "tagKey" = "accessed",
    #                 "tagValue" = as.character(Sys.time()),
    #                 "createdDate" = as.character(Sys.time()))
    #
    # retry(quote(dbAppendTable(conn, CacheDBTableName(cachePath, drv), dt), retries = 15))
    rs <- retry(quote(dbSendStatement(
      conn,
      paste0("insert into \"", CacheDBTableName(cachePath, drv), "\"",
             " (\"cacheId\", \"tagKey\", \"tagValue\", \"createdDate\") values ",
             "('", isInRepo$cacheId[lastOne],
             "', 'accessed', '", as.character(Sys.time()), "', '", as.character(Sys.time()), "')")
      )), retries = 15)

    dbClearResult(rs)
  } else {
    written <- 0
    while (written >= 0) {
      saved <- suppressWarnings(try(silent = TRUE,
                                    addTagsRepo(isInRepo[[.cacheTableHashColName()]][lastOne],
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

.cacheNumDefaultTags <- function() {
  if (getOption("reproducible.useDBI", TRUE)) 7 else 11
}

.cacheTableHashColName <- function() {
  if (getOption("reproducible.useDBI", TRUE)) {
    "cacheId"
  } else {
    "artifact"
  }
}

.cacheTableTagColName <- function(option = NULL) {
  out <- "tagValue"
  if (getOption("reproducible.useDBI", TRUE)) {
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

  if (getOption('reproducible.useDBI', TRUE)) {
    if (!is.null(conn)) {
      type <- gsub("Connection", "", class(conn))
    }
  }

  if (grepl(type, "SQLite")) {
    if (getOption("reproducible.useDBI", TRUE)) {
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
  if (getOption("reproducible.useDBI", TRUE)) {
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
  csf <- if (isTRUE(getOption("reproducible.useDBI", TRUE)) == FALSE) {
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
CacheDBTableName <- function(cachePath,
                             drv = getOption("reproducible.drv", RSQLite::SQLite())) {
  if (!is(cachePath, "Path")) {
    cachePath <- asPath(cachePath, nParentDirs = 2)
  }
  if (getOption("reproducible.useDBI", TRUE)) {
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
  browser(expr = exists("._CacheIsACache"))
  if (getOption('reproducible.useDBI', TRUE)) {
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
  if (getOption("reproducible.useDBI", TRUE)) {
    if (ret) {
      ret <- ret && any(grepl(CacheDBTableName(cachePath), dbListTables(conn)))
    }
  }

  if (getOption('reproducible.useDBI', TRUE)) {
    if (isFALSE(ret) && isTRUE(create)) {
      if (grepl(type, "Pq")) {
        file.create(CacheDBFile(cachePath, drv = drv, conn = conn))
      }
    }
  }
  return(ret)
}
