#' @keywords internal
.pkgSnapshot <- function(instPkgs, instVers, packageVersionFile = "._packageVersionsAuto.txt") {
  # browser(expr = exists("aaaa"))
  inst <- data.frame(instPkgs, instVers = unlist(instVers), stringsAsFactors = FALSE)
  write.table(inst, file = packageVersionFile, row.names = FALSE)
  inst
}

################################################################################
#' Convert numeric to character with padding
#'
#' This will pad floating point numbers, right or left. For integers, either class
#' integer or functionally integer (e.g., 1.0), it will not pad right of the decimal.
#' For more specific control or to get exact padding right and left of decimal,
#' try the \code{stringi} package. It will also not do any rounding. See examples.
#'
#' @param x numeric. Number to be converted to character with padding
#'
#' @param padL numeric. Desired number of digits on left side of decimal.
#'              If not enough, \code{pad} will be used to pad.
#'
#' @param padR numeric. Desired number of digits on right side of decimal.
#'              If not enough, \code{pad} will be used to pad.
#'
#' @param pad character to use as padding (\code{nchar(pad) == 1} must be \code{TRUE}).
#'
#' @return Character string representing the filename.
#'
#' @author Eliot McIntire and Alex Chubaty
#' @export
#' @rdname paddedFloatToChar
#'
#' @examples
#' paddedFloatToChar(1.25)
#' paddedFloatToChar(1.25, padL = 3, padR = 5)
#' paddedFloatToChar(1.25, padL = 3, padR = 1) # no rounding, so keeps 2 right of decimal
paddedFloatToChar <- function(x, padL = ceiling(log10(x + 1)), padR = 3, pad = "0") {
  xf <- x %% 1
  numDecimals <- nchar(gsub("(.*)(\\.)|([0]*$)","",xf))

  # this == used to be fpCompare -- but this function is more or less deprecated
  newPadR <- ifelse(abs(xf - 0) < sqrt(.Machine$double.eps), 0, pmax(numDecimals, padR))
  xFCEnd <- sprintf(paste0("%0", padL+newPadR+1*(newPadR > 0),".", newPadR, "f"), x)
  return(xFCEnd)
}


#' Add a prefix or suffix to the basename part of a file path
#'
#' Prepend (or postpend) a filename with a prefix (or suffix).
#' If the directory name of the file cannot be ascertained from its path,
#' it is assumed to be in the current working directory.
#'
#' @param f       A character string giving the name/path of a file.
#' @param prefix  A character string to prepend to the filename.
#' @param suffix  A character string to postpend to the filename.
#'
#' @author Jean Marchal and Alex Chubaty
#' @export
#' @rdname prefix
#'
#' @examples
#' # file's full path is specified (i.e., dirname is known)
#' myFile <- file.path("~/data", "file.tif")
#' .prefix(myFile, "small_")    ## "/home/username/data/small_file.tif"
#' .suffix(myFile, "_cropped") ## "/home/username/data/myFile_cropped.shp"
#'
#' # file's full path is not specified
#' .prefix("myFile.shp", "small")    ## "./small_myFile.shp"
#' .suffix("myFile.shp", "_cropped") ## "./myFile_cropped.shp"
#'
.prefix <- function(f, prefix = "") {
  file.path(dirname(f), paste0(prefix, basename(f)))
}

#' @export
#' @name suffix
#' @rdname prefix
.suffix <- function(f, suffix = "") {
  file.path(dirname(f), paste0(filePathSansExt(basename(f)), suffix,
                               ".", fileExt(f)))
}

#' Get a unique name for a given study area
#'
#' Digest a spatial object to get a unique character string (hash) of the study area.
#' Use \code{.suffix()} to append the hash to a filename,
#' e.g., when using \code{filename2} in \code{prepInputs}.
#'
#' @param studyArea Spatial object.
#' @param ... Other arguments (not currently used)
#'
#' @export
#' @importFrom digest digest
setGeneric("studyAreaName", function(studyArea, ...) {
  standardGeneric("studyAreaName")
})

#' @export
#' @rdname studyAreaName
setMethod(
  "studyAreaName",
  signature = "SpatialPolygonsDataFrame",
  definition = function(studyArea, ...) {
    studyArea <- studyArea[, -c(1:ncol(studyArea))]
    studyArea <- as(studyArea, "SpatialPolygons")
    studyAreaName(studyArea, ...)
})

#' @export
#' @rdname studyAreaName
setMethod(
  "studyAreaName",
  signature = "character",
  definition = function(studyArea, ...) {
    sort(studyArea) ## ensure consistent hash for same subset of study area names
    digest(studyArea, algo = "xxhash64") ## TODO: use `...` to pass `algo`
})

#' @export
#' @rdname studyAreaName
setMethod(
  "studyAreaName",
  signature = "ANY",
  definition = function(studyArea, ...) {
    if (is(studyArea, "sf")) {
      if (requireNamespace("sf")) {
        studyArea <- sf::st_geometry(studyArea)
      }
    }
    if (!(isSpatialAny(studyArea)) || is.character(studyArea)) {
      stop("studyAreaName expects an object that returns TRUE with ",
           "from isSpatialAny(x) or character vector")
    }
    digest(studyArea, algo = "xxhash64") ## TODO: use `...` to pass `algo`
})

#' Identify which formals to a function are not in the current \code{...}
#'
#' Advanced use.
#'
#' @keywords internal
#' @export
#' @param fun A function
#' @param ... The ... from inside a function. Will be ignored if \code{dots} is
#'        provided explicitly.
#' @param dots Optional. If this is provided via say \code{dots = list(...)},
#'             then this will cause the \code{...} to be ignored.
#' @param formalNames Optional character vector. If provided then it will override the \code{fun}
.formalsNotInCurrentDots <- function(fun, ..., dots, formalNames, signature = character()) {
  if (is.character(fun)) {
    fun <- get(fun, mode = "function", envir = parent.frame())
  }

  if (missing(formalNames))
    if (isS4(fun)) {
      forms <- methodFormals(fun, signature = signature, envir = parent.frame())
      formalNames <- names(forms)
    } else {
      formalNames <- names(formals(fun))
    }

  if (!missing(dots)) {
    out <- names(dots)[!(names(dots) %in% formalNames)]
  } else {
    out <- names(list(...))[!(names(list(...)) %in% formalNames)]
  }
  out
}

#' @keywords internal
rndstr <- function(n = 1, len = 8) {
  unlist(lapply(character(n), function(x) {
    x <- paste0(sample(c(0:9, letters, LETTERS), size = len,
                       replace = TRUE), collapse = "")
  }))
}

#' Alternative to \code{interactive()} for unit testing
#'
#' This is a suggestion from
#' \url{https://github.com/MangoTheCat/blog-with-mock/blob/master/Blogpost1.Rmd}
#' as a way to test interactive code in unit tests. Basically, in the unit tests,
#' we use \code{testthat::with_mock}, and inside that we redefine \code{isInteractive}
#' just for the test. In all other times, this returns the same things as
#' \code{interactive()}.
#' @keywords internal
#' @examples
#' \dontrun{
#' testthat::with_mock(
#' `isInteractive` = function() {browser(); TRUE},
#' {
#'   tmpdir <- tempdir()
#'   aa <- Cache(rnorm, 1, cacheRepo = tmpdir, userTags = "something2")
#'   # Test clearCache -- has an internal isInteractive() call
#'   clearCache(tmpdir, ask = FALSE)
#'   })
#' }
isInteractive <- function() interactive()

#' A version of \code{base::basename} that is \code{NULL} resistant
#'
#' Returns \code{NULL} if x is \code{NULL}, otherwise, as \code{basename}.
#'
#' @param x A character vector of paths
#' @export
#' @return Same as \code{\link[base]{basename}}
#'
basename2 <- function(x) {
  if (is.null(x)) {
    NULL
  } else {
    basename(x)
  }
}

#' A wrapper around \code{try} that retries on failure
#'
#' This is useful for functions that are "flaky", such as \code{curl}, which may fail for unknown
#' reasons that do not persist.
#'
#' @details
#' Based on \url{https://github.com/jennybc/googlesheets/issues/219#issuecomment-195218525}.
#'
#' @param expr     Quoted expression to run, i.e., \code{quote(...)}
#' @param retries  Numeric. The maximum number of retries.
#' @param envir    The environment in which to evaluate the quoted expression, default
#'   to \code{parent.frame(1)}
#' @param exponentialDecayBase Numeric > 1.0. The delay between
#'   successive retries will be \code{runif(1, min = 0, max = exponentialDecayBase ^ i - 1)}
#'   where \code{i} is the retry number (i.e., follows \code{seq_len(retries)})
#' @param silent   Logical indicating whether to \code{try} silently.
#' @param exprBetween Another expression that should be run after a failed attempt
#'   of the `expr`. This should return a named list, where the names indicate the object names
#'   to update in the main expr, and the return value is the new value. (previous versions allowed
#'   a non-list return, but where the final line had to be an assignment operator,
#'   specifying what object (that is used in `expr`) will be updated prior to running
#'   the `expr` again. For backwards compatibility, this still works).
#' @param messageFn A function for messaging to console. Defaults to \code{message}
#'
#' @export
retry <- function(expr, envir = parent.frame(), retries = 5,
                  exponentialDecayBase = 1.3, silent = TRUE,
                  exprBetween = NULL, messageFn = message) {
  if (exponentialDecayBase < 1)
    stop("exponentialDecayBase must be equal to or greater than 1")
  for (i in seq_len(retries)) {
    if (!(is.call(expr) || is.name(expr))) warning("expr is not a quoted expression")
    result <- try(expr = eval(expr, envir = envir), silent = silent)
    if (inherits(result, "try-error")) {
      if (!is.null(exprBetween)) {
        finalPart <- length(format(exprBetween))

        # The expression is different if it is 1 line vs >1 line
        exprBetweenTail <- if (finalPart > 1) {
          finalPart <- length(exprBetween)
          exprBetween[[finalPart]]
        } else {
          exprBetween
        }
        # if (!identical(as.character(exprBetweenTail)[[1]], "<-"))
        #   stop("exprBetween must have an assignment operator <- with a object on",
        #        "the LHS that is used on the RHS of expr ")
        objName <- as.character(exprBetweenTail[[2]])
        result1 <-
          try(expr = eval(exprBetween, envir = envir), silent = silent)
        if (is.list(result1)) {
          if (!is.null(names(result1))) {
            objName <- names(result1)
          } else {
            stop("The return object from exprBetween must be a named list, with names being objects to overwrite")
          }
        } else {
          result1 <- list(result1)
          names(result1) <- objName
        }

        lapply(objName, function(objNam) assign(objNam, result1[[objNam]], envir = envir))

      }
      backoff <- sample(1:1000/1000, size = 1) * (exponentialDecayBase^i - 1)
      if (backoff > 3) {
        messageFn("Waiting for ", round(backoff, 1), " seconds to retry; the attempt is failing")
      }
      Sys.sleep(backoff)
    } else {
      if (exists("result1", inherits = FALSE))
        messageFn("    ...fixed!")
      break
    }
  }

  if (inherits(result, "try-error")) {
    stop(result, "\nFailed after ", retries, " attempts.")
  } else {
    return(result)
  }
}

#' Test whether system is Windows
#'
#' This is used so that unit tests can override this using \code{testthat::with_mock}.
#' @keywords internal
isWindows <- function() identical(.Platform$OS.type, "windows")

#' @keywords internal
isMac <- function() identical(tolower(Sys.info()["sysname"]), "darwin")

#' Provide standard messaging for missing package dependencies
#'
#' This provides a standard message format for missing packages, e.g.,
#' detected via \code{requireNamespace}.
#'
#' @export
#' @param pkg Character string indicating name of package required
#' @param minVersion Character string indicating minimum version of package
#'   that is needed
#' @param messageStart A character string with a prefix of message to provide
#' @param stopOnFALSE Logical. If \code{TRUE}, this function will create an
#'   error (i.e., \code{stop}) if the function returns \code{FALSE}; otherwise
#'   it simply returns \code{FALSE}
.requireNamespace <- function(pkg = "methods", minVersion = NULL,
                              stopOnFALSE = FALSE,
                              messageStart = paste0(pkg, if (!is.null(minVersion))
                                paste0("(>=", minVersion, ")"), " is required. Try: ")) {
  need <- FALSE
  if (!requireNamespace(pkg, quietly = TRUE)) {
    need <- TRUE
  } else {
    if (!is.null(minVersion)) {
      if (isTRUE(packageVersion(pkg) < minVersion))
        need <- TRUE
    }
  }
  if (isTRUE(stopOnFALSE) && isTRUE(need))
    stop(requireNamespaceMsg(pkg))
  !need
}

#' Use message to print a clean square data structure
#'
#' Sends to \code{message}, but in a structured way so that a data.frame-like can
#' be cleanly sent to messaging.
#'
#' @param df A data.frame, data.table, matrix
#' @param round An optional numeric to pass to \code{round}
#' @param colour Passed to \code{getFromNamespace(colour, ns = "crayon")},
#'   so any colour that \code{crayon} can use
#' @param colnames Logical or \code{NULL}. If \code{TRUE}, then it will print
#'   column names even if there aren't any in the \code{df} (i.e., they will)
#'   be \code{V1} etc., \code{NULL} will print them if they exist, and \code{FALSE}
#'   which will omit them.
#' @inheritParams base::message
#'
#' @export
#' @importFrom data.table is.data.table as.data.table
#' @importFrom utils capture.output
messageDF <- function(df, round, colour = NULL, colnames = NULL, appendLF = TRUE) {
  origColNames <- if (is.null(colnames) | isTRUE(colnames)) colnames(df) else NULL

  if (is.matrix(df))
    df <- as.data.frame(df)
  if (!is.data.table(df)) {
    df <- as.data.table(df)
  }
  df <- Copy(df)
  skipColNames <- if (is.null(origColNames) & !isTRUE(colnames)) TRUE else FALSE
  if (!missing(round)) {
    isNum <- sapply(df, is.numeric)
    isNum <- colnames(df)[isNum]
    for (Col in isNum) {
      set(df, NULL, Col, round(df[[Col]], round))
    }
  }
  outMess <- capture.output(df)
  if (skipColNames) outMess <- outMess[-1]
  out <- lapply(outMess, function(x) {
    if (!is.null(colour)) {
      messageColoured(x, colour = colour, appendLF = appendLF)
    } else {
      message(x, appendLF = appendLF)
    }
  })
}

filePathSansExt <- function(x) {
  sub("([^.]+)\\.[[:alnum:]]+$", "\\1", x)
}

fileExt <- function(x) {
  pos <- regexpr("\\.([[:alnum:]]+)$", x)
  ifelse(pos > -1L, substring(x, pos + 1L), "")
}

isDirectory <- function(pathnames) {
  keep <- is.character(pathnames)
  if (length(pathnames) == 0) return(logical())
  if (isFALSE(keep)) stop("pathnames must be character")
  origPn <- pathnames
  pathnames <- normPath(pathnames[keep])
  id <- dir.exists(pathnames)
  id[id] <- file.info(pathnames[id])$isdir
  names(id) <- origPn
  id
}

isFile <- function(pathnames) {
  keep <- is.character(pathnames)
  if (isFALSE(keep)) stop("pathnames must be character")
  origPn <- pathnames
  pathnames <- normPath(pathnames[keep])
  iF <- file.exists(pathnames)
  iF[iF] <- !file.info(pathnames[iF])$isdir
  names(iF) <- origPn
  iF
}

isAbsolutePath <- function(pathnames) {
  # modified slightly from R.utils::isAbsolutePath
  keep <- is.character(pathnames)
  if (isFALSE(keep)) stop("pathnames must be character")
  origPn <- pathnames
  nPathnames <- length(pathnames)
  if (nPathnames == 0L)
    return(logical(0L))
  if (nPathnames > 1L) {
    res <- sapply(pathnames, FUN = isAbsolutePath)
    return(res)
  }
  if (is.na(pathnames))
    return(FALSE)
  if (regexpr("^~", pathnames) != -1L)
    return(TRUE)
  if (regexpr("^.:(/|\\\\)", pathnames) != -1L)
    return(TRUE)
  components <- strsplit(pathnames, split = "[/\\]")[[1L]]
  if (length(components) == 0L)
    return(FALSE)
  (components[1L] == "")
}

# This is so that we don't need to import from backports
isFALSE <- function(x) is.logical(x) && length(x) == 1L && !is.na(x) && !x


messagePrepInputs <- function(..., appendLF = TRUE) {
  messageColoured(..., colour = getOption("reproducible.messageColourPrepInputs"),
                  appendLF = appendLF)
}

messageCache <- function(..., colour = getOption("reproducible.messageColourCache"),
                         appendLF = TRUE) {
  messageColoured(..., colour = colour, appendLF = appendLF)
}

messageQuestion <- function(..., verboseLevel = 0, appendLF = TRUE) {
  # force this message to print
  messageColoured(..., colour = getOption("reproducible.messageColourQuestion"),
                  verboseLevel = verboseLevel, verbose = 0, appendLF = appendLF)
}

messageColoured <- function(..., colour = NULL, verboseLevel = 1,
                            verbose = getOption("reproducible.verbose", 1),
                            appendLF = TRUE) {
  if (isTRUE(verboseLevel <= verbose)) {
    needCrayon <- FALSE
    if (!is.null(colour)) {
      if (is.character(colour))
        needCrayon <- TRUE
    }
    if (needCrayon && requireNamespace("crayon", quietly = TRUE)) {
      message(getFromNamespace(colour, "crayon")(paste0(...)), appendLF = appendLF)
    } else {
      if (!isTRUE(.pkgEnv$.checkedCrayon) && !.requireNamespace("crayon")) {
        message("To add colours to messages, install.packages('crayon')", appendLF = appendLF)
        .pkgEnv$.checkedCrayon <- TRUE
      }
      message(paste0(...), appendLF = appendLF)
    }
  }

}



methodFormals <- function(fun, signature = character(), envir = parent.frame()) {
  if (is.character(fun))
    fun <- get(fun, mode = "function", envir = envir)

  fdef <- getGeneric(fun)
  method <- selectMethod(fdef, signature)
  genFormals <- base::formals(fdef)
  b <- body(method)
  if(is(b, "{") && is(b[[2]], "<-") && identical(b[[2]][[2]], as.name(".local"))) {
    local <- eval(b[[2]][[3]])
    if(is.function(local))
      return(formals(local))
    warning("Expected a .local assignment to be a function. Corrupted method?")
  }
  genFormals
}


.fileExtsKnown <- function() {
  shpFile <- getOption("reproducible.shapefileRead")
  if (is.null(shpFile)) shpFile <- "sf::st_read"

  df <- data.frame(
    rbind(
      c("rds", "base::readRDS", "binary"),
      c("qs", "qs::qread", "qs"),
      cbind(c("asc", "grd", "tif"), c("raster::raster"), "Raster"),
      cbind(c("shp", "gdb"), shpFile, "shapefile")
    )
  )
  colnames(df) <- c("extension", "fun", "type")
  df
}



#' `all.equal` that allows comparison of Cached and non-Cached objects
#'
#' Cache places attributes on objects. This function strips those attributes,
#' allowing comparison of 2 objects one or both of which came from a Cache.
#'
#' @export
#' @inheritParams base::all.equal
allEqual <- function(a, b, ...) {
  attr(a, ".Cache") <- NULL
  attr(b, ".Cache") <- NULL
  all.equal(a, b, ...)
}
