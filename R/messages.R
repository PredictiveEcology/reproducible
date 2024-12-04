## This is an incomplete file; it will be slowly transitioned to have all messaging here
## Any new message should be written as a .messageGreps entry, then used in the functions
## with the mess*.
## All messages and message-generating functions are stored in the .message environment,
## to allow updating (i.e., `.message$PreProcessIndent`).

#' @keywords internal
#' @rdname pkgEnv
.message <- new.env()

.message$SkipDownload <- "Skipping download of url; local copy already exists and passes checksums"

.message$Greps <- list(
  studyArea_Spatial = "The \\'studyArea\\' provided is not a Spatial\\* object.",
  rasterToMatch_Raster = "The \\'rasterToMatch\\' provided is not a Raster\\* object.",
  anySpatialClass = "Raster\\*, Spat\\*, sf or Spatial object"
)

.message$PreProcessIndentOrig <- .message$PreProcessIndent <- ""

.message$CacheIndent <- "    "

.message$Spatial <- lapply(.message$Greps, gsub, pattern = "\\\\", replacement = "")

.message$LoadedCacheResult <- function(src = 1) {
  srcPoss <- c("Cached", "Memoised")
  if (is.numeric(src)) {
    src <- srcPoss[src]
  } else if (is.character(src)) {
    src <- srcPoss[grepl(src, srcPoss)]
  }
  paste0("Loaded! ", src, " result from previous")
}

.message$AddingToMemoised <- "(and added a memoised copy)"

.message$LoadedCache <- function(root, functionName) {
  paste0(root, " ", functionName, " call")
}

.message$BecauseOfA <- "...because of (a)"

.message$BecauseOfA <- "  "

.message$NoCachePathSupplied <- "No cachePath supplied"

.message$NoCacheRepoSuppliedGrep <- paste0(.message$NoCachePathSupplied, " and.+getOption\\('reproducible.cachePath'\\).+is.+inside")

.message$RequireNamespaceFn <- function(pkg, messageExtra = character(), minVersion = NULL) {
  mess <- paste0(
    pkg, if (!is.null(minVersion)) {
      paste0("(>=", minVersion, ")")
    }, " is required but not yet installed. Try: ",
    "install.packages('", pkg, "')"
  )
  if (length(messageExtra) > 0) {
    mess <- paste(mess, messageExtra)
  }
  mess
}

#' Use `message` with a consistent use of `verbose`
#'
#' This family has a consistent use of `verbose` allowing messages to be
#' turned on or off or verbosity increased or decreased throughout the family of
#' messaging in `reproducible`.
#'
#' - `messageDF` uses `message` to print a clean square data structure.
#' - `messageColoured` allows specific colours to be used.
#' - `messageQuestion` sets a high level for `verbose` so that the message always gets asked.
#'
#' @param df A data.frame, data.table, matrix
#' @param round An optional numeric to pass to `round`
#' @param colour Passed to `getFromNamespace(colour, ns = "crayon")`,
#'   so any colour that `crayon` can use
#' @param colnames Logical or `NULL`. If `TRUE`, then it will print
#'   column names even if there aren't any in the `df` (i.e., they will)
#'   be `V1` etc., `NULL` will print them if they exist, and `FALSE`
#'   which will omit them.
#' @param verboseLevel The numeric value for this `message*` call, equal or above
#'   which `verbose` must be. The higher this is set, the more unlikely the call
#'   will show a message.
#' @param indent An integer, indicating whether to indent each line
#' @inheritParams base::message
#'
#' @return
#' Used for side effects. This will produce a message of a structured `data.frame`.
#'
#' @inheritParams Cache
#'
#' @export
#' @importFrom data.table is.data.table as.data.table
#' @importFrom utils capture.output
#' @rdname messageColoured
messageDF <- function(df, round, colour = NULL, colnames = NULL, indent = NULL,
                      verbose = getOption("reproducible.verbose"), verboseLevel = 1,
                      appendLF = TRUE) {
  if (isTRUE(verboseLevel <= verbose)) {
    origColNames <- if (is.null(colnames) || isTRUE(colnames)) colnames(df) else NULL

    if (is.matrix(df)) {
      df <- as.data.frame(df)
    }
    if (!is.data.table(df)) {
      df <- as.data.table(df)
    }
    df <- Copy(df)
    skipColNames <- if (is.null(origColNames) && !isTRUE(colnames)) TRUE else FALSE
    if (!missing(round)) {
      isNum <- sapply(df, is.numeric)
      isNum <- colnames(df)[isNum]
      for (Col in isNum) {
        set(df, NULL, Col, round(df[[Col]], round))
      }
    }
    outMess <- capture.output(df)
    if (skipColNames) outMess <- outMess[-1]
    outMess <- .addSlashNToAllButFinalElement(outMess)
    messageColoured(outMess, indent = indent, hangingIndent = FALSE,
                    colour = colour, verbose = verbose,
                    verboseLevel = verboseLevel, appendLF = appendLF)
    # out <- lapply(outMess, function(x) {
    #   messageColoured(x,
    #     colour = colour, indent = indent, appendLF = appendLF, verbose = verbose,
    #     verboseLevel = verboseLevel
    #   )
    # })
  }
}

#' @export
#' @rdname messageColoured
messagePrepInputs <- function(..., appendLF = TRUE,
                              verbose = getOption("reproducible.verbose"),
                              verboseLevel = 1) {
  messageColoured(...,
                  colour = getOption("reproducible.messageColourPrepInputs"),
                  verboseLevel = verboseLevel, verbose = verbose, appendLF = appendLF
  )
}

#' @export
#' @rdname messageColoured
messagePreProcess <- function(..., appendLF = TRUE,
                              verbose = getOption("reproducible.verbose"),
                              verboseLevel = 1) {
  messageColoured(..., indent = .message$PreProcessIndent,
                  colour = getOption("reproducible.messageColourPrepInputs"),
                  verboseLevel = verboseLevel, verbose = verbose, appendLF = appendLF
  )
}

#' @export
#' @rdname messageColoured
messageCache <- function(..., colour = getOption("reproducible.messageColourCache"),
                         verbose = getOption("reproducible.verbose"), verboseLevel = 1,
                         appendLF = TRUE) {
  needIndent <- try(any(grepl("\b", unlist(list(...)))))
  if (is(needIndent, "try-error")) browser()
  indent <- if (isTRUE(!needIndent)) .message$PreProcessIndent else ""
  messageColoured(..., indent = indent, # .message$CacheIndent,
                  colour = colour, appendLF = appendLF,
                  verboseLevel = verboseLevel, verbose = verbose
  )
}

#' @export
#' @rdname messageColoured
messageQuestion <- function(..., verboseLevel = 0, appendLF = TRUE) {
  # force this message to print
  messageColoured(...,
                  colour = getOption("reproducible.messageColourQuestion"),
                  verbose = 10, verboseLevel = verboseLevel, appendLF = appendLF
  )
}

#' @rdname messageColoured
.messageFunctionFn <- function(..., appendLF = TRUE, verbose = getOption("reproducible.verbose"),
                            verboseLevel = 1) {
  fn <- getFromNamespace(getOption("reproducible.messageColourFunction"), asNamespace("crayon"))
  fn(...)
}

#' @param colour Any colour that can be understood by `crayon`
#' @param hangingIndent Logical. If there are `\n`, should there be a handing indent of 2 spaces.
#'   Default is `TRUE`
#' @param ... Any character vector, passed to `paste0(...)`
#'
#' @export
#' @importFrom utils getFromNamespace
#' @rdname messageColoured
messageColoured <- function(..., colour = NULL, indent = NULL, hangingIndent = TRUE,
                            verbose = getOption("reproducible.verbose", 1),
                            verboseLevel = 1, appendLF = TRUE) {
  if (isTRUE(verboseLevel <= verbose)) {
    needCrayon <- FALSE
    if (!is.null(colour)) {
      if (is.character(colour)) {
        needCrayon <- TRUE
      }
    }
    mess <- paste0(..., collapse = "")
    if (!is.null(indent)) {
      mess <- paste0(indent, mess)
    }

    # do line wrap with hanging indent
    maxLineLngth <- getOption("width") - 10 # 10 is a "buffer" for Rstudio miscalculations
    chars <- nchar(mess)
    if (chars > maxLineLngth) {
      splitOnSlashN <- strsplit(mess, "\n")
      newMess <- lapply(splitOnSlashN, function(m) {
        anyOneLine <- any(nchar(m) > maxLineLngth)
        if (anyOneLine) {
          messSplit <- strsplit(mess, split = " ")
          remainingChars <- chars
          messBuild <- character()
          while (remainingChars > maxLineLngth) {
            whNewLine <- which(cumsum(nchar(messSplit[[1]]) + 1) >= maxLineLngth)[1] - 1
            # if (isTRUE(any(grepl("...because of", mess)))) browser()
            if (anyNA(whNewLine)) browser()

            keepInd <- 1:whNewLine
            newMess <- paste(messSplit[[1]][keepInd], collapse = " ")
            messBuild <- c(messBuild, newMess)
            if (is.null(indent)) {
              # if it starts with a space -- that is the indent that is needed
              if (startsWith(newMess, " ")) {
                indent <<- sub("^( +).+", "\\1", newMess)
                if (grepl("^ +\\.\\.\\.", newMess)) {
                  indent <<- paste0(indent, " ")
                }
              } else {
                indent <<- ""
              }

            }
            messSplit[[1]] <- messSplit[[1]][-keepInd]
            remainingChars <- remainingChars - nchar(newMess) - 1
            hangingIndent <<- TRUE
          }
          newMess <- paste(messSplit[[1]], collapse = " ")
          m <- c(messBuild, newMess)
        }
        m
      })
      mess <- unlist(newMess)
      mess <- paste0(.addSlashNToAllButFinalElement(mess), collapse = "")
    }
    hi <- if (isTRUE(hangingIndent)) paste0(indent, .message$BecauseOfA) else indent
    if (any(grepl("\n", mess))) {
      mess <- gsub("\n *", paste0("\n", hi), mess)
    }

    if (needCrayon && requireNamespace("crayon", quietly = TRUE)) {
      mess <- lapply(strsplit(mess, "\n"), function(m) paste0(getFromNamespace(colour, "crayon")(m)))[[1]]
      mess <- .addSlashNToAllButFinalElement(mess)
      message(mess, appendLF = appendLF)
      # message(getFromNamespace(colour, "crayon")(mess), appendLF = appendLF)
    } else {
      if (needCrayon && !isTRUE(.pkgEnv$.checkedCrayon) && !.requireNamespace("crayon")) {
        message("To add colours to messages, install.packages('crayon')", appendLF = appendLF)
        .pkgEnv$.checkedCrayon <- TRUE
      }
      message(mess, appendLF = appendLF)
    }
  }
}

#' @keywords internal
.message$CacheSize <- function(x, artifacts = NULL, cacheTable,
                              verbose = getOption("reproducible.verbose")) {
  tagCol <- "tagValue"
  if (missing(cacheTable)) {
    a <- showCache(x, verbose = verbose - 1, sorted = FALSE)
  } else {
    a <- cacheTable
  }
  cn <- if (any(colnames(a) %in% "tag")) "tag" else "tagKey"

  nas <- a[[.cacheTableTagColName()]] %in% "NA" & a[[cn]] == "object.size"
  if (any(nas))
    a <- a[!nas]

  b <- a[a[[cn]] == "object.size", ]
  if (any(colnames(a) %in% "tag")) {
    fsTotal <- sum(as.numeric(unlist(lapply(strsplit(b[[cn]], split = ":"), function(x) x[[2]])))) / 4
  } else {
    fsTotal <- sum(as.numeric(b[[.cacheTableTagColName()]])) / 4
  }
  fsTotalRasters <- sum(file.size(dir(file.path(x, "rasters"), full.names = TRUE, recursive = TRUE)))
  fsTotal <- fsTotal + fsTotalRasters
  class(fsTotal) <- "object_size"
  preMessage1 <- "  Total (including Rasters): "

  b <- a[a[[.cacheTableHashColName()]] %in% artifacts &
           (a[[cn]] %in% "object.size"), ]
  if (cn == "tag") {
    fs <- sum(as.numeric(unlist(lapply(strsplit(b[[cn]], split = ":"), function(x) x[[2]])))) / 4
  } else {
    fs <- sum(as.numeric(b[[.cacheTableTagColName()]])) / 4
  }

  class(fs) <- "object_size"
  preMessage <- "  Selected objects (not including Rasters): "

  messageCache("Cache size: ", verbose = verbose)
  messageCache(preMessage1, format(fsTotal, "auto"), verbose = verbose)
  messageCache(preMessage, format(fs, "auto"), verbose = verbose)
}

.message$ObjToRetrieveFn <- function(funName) {
  paste0("Object to retrieve (fn: ", .messageFunctionFn(funName))
}

.message$IndentDefault <- 1

.message$IndentUpdate <- function(nchar = .message$IndentDefault, envir = parent.frame(), ns = "reproducible") {
  val <- paste0(rep(" ", nchar), collapse = "")
  .message$PreProcessIndent <- paste0(.message$PreProcessIndent, val)
  withr::defer(
    envir = envir,
    expr =
      {
        .message$PreProcessIndent <- gsub(paste0(val, "$"), "", .message$PreProcessIndent)
      }
  )
}

.message$IndentRevert <- function(nchar = .message$IndentDefault, envir = parent.frame(), ns = "reproducible") {
  val <- paste0(rep(" ", nchar), collapse = "")
  .message$PreProcessIndent <- gsub(paste0(val, "$"), "", .message$PreProcessIndent)
  withr::deferred_clear(envir = envir)
}

.txtUnableToAccessIndex <- "unable to access index"
