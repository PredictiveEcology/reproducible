
################################################################################
#' Convert numeric to character with padding
#'
#' This will pad floating point numbers, right or left. For integers, either class
#' integer or functionally integer (e.g., 1.0), it will not pad right of the decimal.
#' For more specific control or to get exact padding right and left of decimal,
#' try the `stringi` package. It will also not do any rounding. See examples.
#'
#' @param x numeric. Number to be converted to character with padding
#'
#' @param padL numeric. Desired number of digits on left side of decimal.
#'              If not enough, `pad` will be used to pad.
#'
#' @param padR numeric. Desired number of digits on right side of decimal.
#'              If not enough, `pad` will be used to pad.
#'
#' @param pad character to use as padding (`nchar(pad) == 1` must be `TRUE`).
#'
#' @return Character string representing the filename.
#'
#' @author Eliot McIntire and Alex Chubaty
#' @export
#' @rdname paddedFloatToChar
#' @importFrom fpCompare %==%
#'
#' @examples
#' paddedFloatToChar(1.25)
#' paddedFloatToChar(1.25, padL = 3, padR = 5)
#' paddedFloatToChar(1.25, padL = 3, padR = 1) # no rounding, so keeps 2 right of decimal
paddedFloatToChar <- function(x, padL = ceiling(log10(x + 1)), padR = 3, pad = "0") {
  xf <- x %% 1
  numDecimals <- nchar(gsub("(.*)(\\.)|([0]*$)","",xf))
  newPadR <- ifelse(xf %==% 0, 0, pmax(numDecimals, padR))
  xFCEnd <- sprintf(paste0("%0", padL + newPadR + 1*(newPadR > 0),".", newPadR, "f"), x)
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
#' @return
#' A character string or vector with the prefix pre-pended or suffix post-pended
#' on the `basename` of the `f`, before the file extension.
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
#' Use `.suffix()` to append the hash to a filename,
#' e.g., when using `filename2` in `prepInputs`.
#'
#' @param studyArea Spatial object.
#' @param ... Other arguments (not currently used)
#'
#' @export
#' @return
#' A character string using the `.robustDigest` of the `studyArea`. This is only intended
#' for use with spatial objects.
#' @examples
#' studyAreaName("Ontario")
setGeneric("studyAreaName", function(studyArea, ...) {
  standardGeneric("studyAreaName")
})

# @export
# @rdname studyAreaName
# setMethod(
#   "studyAreaName",
#   signature = "SpatialPolygonsDataFrame",
#   definition = function(studyArea, ...) {
#     studyArea <- studyArea[, -c(1:ncol(studyArea))]
#     studyArea <- as(studyArea, "SpatialPolygons")
#     studyAreaName(studyArea, ...)
# })

#' @export
#' @rdname studyAreaName
setMethod(
  "studyAreaName",
  signature = "character",
  definition = function(studyArea, ...) {
    sort(studyArea) ## ensure consistent hash for same subset of study area names
    .robustDigest(studyArea, algo = "xxhash64") ## TODO: use `...` to pass `algo`
})

#' @export
#' @rdname studyAreaName
setMethod(
  "studyAreaName",
  signature = "ANY",
  definition = function(studyArea, ...) {
    if (inherits(studyArea, "sf")) {
      .requireNamespace("sf", stopOnFALSE = TRUE)
      studyArea <- sf::st_geometry(studyArea)
    } else if (inherits(studyArea, "SpatialPolygonsDataFrame")) {
      studyArea <- studyArea[, -c(1:ncol(studyArea))]
      studyArea <- as(studyArea, "SpatialPolygons")
      studyAreaName(studyArea, ...)
    } else if (!(inherits(studyArea, "Spatial") || inherits(studyArea, "sfc") ||
          inherits(studyArea, "SpatVector") || is.character(studyArea))) {
      stop("studyAreaName expects a spatialClasses object (or character vector)")
    }
    .robustDigest(studyArea, algo = "xxhash64") ## TODO: use `...` to pass `algo`
})

#' Identify which formals to a function are not in the current `...`
#'
#' Advanced use.
#'
#' @keywords internal
#' @export
#' @return
#' A list of the formals of the `fun` that are missing from the `...` or `dots`.
#'
#' @param fun A function
#' @param ... The ... from inside a function. Will be ignored if `dots` is
#'        provided explicitly.
#' @param dots Optional. If this is provided via say `dots = list(...)`,
#'             then this will cause the `...` to be ignored.
#' @param formalNames Optional character vector. If provided then it will override the `fun`
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
    out <- ...names()[!(...names() %in% formalNames)]
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

#' Alternative to `interactive()` for unit testing
#'
#' This is a suggestion from
#' <https://github.com/MangoTheCat/blog-with-mock/blob/master/Blogpost1.Rmd>
#' as a way to test interactive code in unit tests. Basically, in the unit tests,
#' we use `testthat::with_mock`, and inside that we redefine `isInteractive`
#' just for the test. In all other times, this returns the same things as
#' `interactive()`.
#' @keywords internal
isInteractive <- function() interactive()

#' A version of `base::basename` that is `NULL` resistant
#'
#' @return
#' `NULL` if x is `NULL`, otherwise, as `basename`.
#'
#' @param x A character vector of paths
#' @export
#' @return Same as [base::basename()]
#'
basename2 <- function(x) {
  if (is.null(x)) {
    NULL
  } else {
    basename(x)
  }
}

#' A wrapper around `try` that retries on failure
#'
#' This is useful for functions that are "flaky", such as `curl`, which may fail for unknown
#' reasons that do not persist.
#'
#' @details
#' Based on <https://github.com/jennybc/googlesheets/issues/219#issuecomment-195218525>.
#'
#' @param expr     Quoted expression to run, i.e., `quote(...)`
#' @param retries  Numeric. The maximum number of retries.
#' @param envir    The environment in which to evaluate the quoted expression, default
#'   to `parent.frame(1)`
#' @param exponentialDecayBase Numeric > 1.0. The delay between
#'   successive retries will be `runif(1, min = 0, max = exponentialDecayBase ^ i - 1)`
#'   where `i` is the retry number (i.e., follows `seq_len(retries)`)
#' @param silent   Logical indicating whether to `try` silently.
#' @param exprBetween Another expression that should be run after a failed attempt
#'   of the `expr`. This should return a named list, where the names indicate the object names
#'   to update in the main expr, and the return value is the new value. (previous versions allowed
#'   a non-list return, but where the final line had to be an assignment operator,
#'   specifying what object (that is used in `expr`) will be updated prior to running
#'   the `expr` again. For backwards compatibility, this still works).
#' @param messageFn A function for messaging to console. Defaults to `message`
#'
#' @return
#' As with `try`, so the successfully returned `return()` from the `expr` or a `try-error`.
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

#' @keywords internal
isCI <- function() {
  as.logical(Sys.getenv("CI"))
}

#' Test whether system is Windows
#'
#' This is used so that unit tests can override this using `testthat::with_mock`.
#' @keywords internal
isWindows <- function() identical(.Platform$OS.type, "windows")

#' @keywords internal
isMac <- function() {
  Sys.info()[["sysname"]] |>
    tolower() |>
    identical("darwin")
}

#' Provide standard messaging for missing package dependencies
#'
#' This provides a standard message format for missing packages, e.g.,
#' detected via `requireNamespace`.
#'
#' @export
#'
#' @return
#' A logical or stop if the namespace is not available to be loaded.
#'
#' @param pkg Character string indicating name of package required
#' @param minVersion Character string indicating minimum version of package
#'   that is needed
#' @param messageStart A character string with a prefix of message to provide
#' @param stopOnFALSE Logical. If `TRUE`, this function will create an
#'   error (i.e., `stop`) if the function returns `FALSE`; otherwise
#'   it simply returns `FALSE`
.requireNamespace <- function(pkg = "methods", minVersion = NULL,
                              stopOnFALSE = FALSE,
                              messageStart = NULL) {
  need <- FALSE
  if (!requireNamespace(pkg, quietly = TRUE)) {
    need <- TRUE
  } else {
    if (!is.null(minVersion))
      if (isTRUE(packageVersion(pkg) < minVersion))
        need <- TRUE
  }

  if (need) # separate these so it is faster
    if (isTRUE(stopOnFALSE))
      stop(requireNamespaceMsg(pkg, extraMsg = messageStart, minVersion = minVersion))
  !need
}


# This is directly from tools::file_ext_sans_ext
filePathSansExt <- function(x) {
  sub("([^.]+)\\.[[:alnum:]]+$", "\\1", x)
}

# This is directly from tools::file_ext
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

findAbsolutePartOfPaths <- function(paths) {
  pathsSplt <- lapply(paths, strsplit, split = "\\\\|/")
  pathsSplt <- mapply(pths = pathsSplt, MoreArgs = list(firstPth = pathsSplt$modulePath),
                      function(pths, firstPth) {
                        intersect(pths[[1]], firstPth[[1]])
                      })
  pathsSplt <- unique(pathsSplt)
  absPathOrig <- paste(pathsSplt[which.min(lengths(pathsSplt))][[1]], collapse = "/")
}

isAbsolutePath <- function(pathnames) {
  # modified slightly from R.utils::isAbsolutePath
  keep <- is.character(pathnames)
  if (isFALSE(keep)) stop("pathnames must be character")
  nPathnames <- length(pathnames)
  if (nPathnames == 0L)
    return(logical(0L))
  done <- logical(nPathnames)
  nas <- is.na(pathnames)
  if (all(nas)) return(!nas)

  tildas <- startsWith(pathnames[!nas], "~")
  if (any(tildas))
    done[!nas][tildas] <- TRUE

  if (all(tildas))
    return(done)

  colon <- regexpr("^.:(/|\\\\)", pathnames[!nas][!tildas])
  hasColon <- colon != -1L
  if (any(hasColon))
    done[!nas][!tildas][hasColon] <- TRUE
  if (all(hasColon))
    return(done)
  curPaths <- pathnames[!nas][!tildas][!hasColon]
  done[!nas][!tildas][!hasColon] <- startsWith(curPaths, "/") | startsWith(curPaths, "\\")
  names(done) <- pathnames
  return(done)
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


#' Use `message` with a consistent use of `verbose`
#'
#' This family has a consistent use of `verbose` allowing messages to be
#' turned on or off or verbosity increased or decreased throughout the family of
#' messaging in `reproducible`. `messageDF` uses `message` to print a clean
#' square data structure. `messageColoured`
#' allows specific colours to be used. `messageQuestion` sets a high level for
#' `verbose` so that the message always gets asked.
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
#' @inheritParams base::message
#'
#' @export
#' @return
#' Used for side effects. This will produce a message of a structured `data.frame`.
#'
#' @importFrom data.table is.data.table as.data.table
#' @importFrom utils capture.output
#' @rdname messageColoured
#' @inheritParams Cache
messageDF <- function(df, round, colour = NULL, colnames = NULL,
                      verbose = getOption("reproducible.verbose"), verboseLevel = 1,
                      appendLF = TRUE) {

  if (isTRUE(verboseLevel <= verbose)) {
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
      messageColoured(x, colour = colour, appendLF = appendLF, verbose = verbose,
                      verboseLevel = verboseLevel)
    })
  }
}

messagePrepInputs <- function(..., appendLF = TRUE,
                              verbose = getOption("reproducible.verbose"),
                              verboseLevel = 1) {
  messageColoured(..., colour = getOption("reproducible.messageColourPrepInputs"),
                  verboseLevel = verboseLevel, verbose = verbose, appendLF = appendLF)
}

messageCache <- function(..., colour = getOption("reproducible.messageColourCache"),
                         verbose = getOption("reproducible.verbose"), verboseLevel = 1,
                         appendLF = TRUE) {
  messageColoured(..., colour = colour, appendLF = appendLF,
                  verboseLevel = verboseLevel, verbose = verbose)
}

#' @rdname messageColoured
messageQuestion <- function(..., verboseLevel = 0, appendLF = TRUE) {
  # force this message to print
  messageColoured(..., colour = getOption("reproducible.messageColourQuestion"),
                  verbose = 10, verboseLevel = verboseLevel, appendLF = appendLF)
}

#' @importFrom utils getFromNamespace
#' @param colour Any colour that can be understood by `crayon`
#' @rdname messageColoured
messageColoured <- function(..., colour = NULL,
                            verbose = getOption("reproducible.verbose", 1),
                            verboseLevel = 1, appendLF = TRUE) {
  if (isTRUE(verboseLevel <= verbose)) {
    needCrayon <- FALSE
    if (!is.null(colour)) {
      if (is.character(colour))
        needCrayon <- TRUE
    }
    if (needCrayon && requireNamespace("crayon", quietly = TRUE)) {
      message(getFromNamespace(colour, "crayon")(paste0(...)), appendLF = appendLF)
    } else {
      if (needCrayon && !isTRUE(.pkgEnv$.checkedCrayon) && !.requireNamespace("crayon")) {
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
  if (requireNamespace("sf", quietly = TRUE) && requireNamespace("DBI", quietly = TRUE))
    shpFile <- getOption("reproducible.shapefileRead", "sf::st_read")
  else {
    shpFile <- "terra::vect"
    if (identical(getOption("reproducible.shapefileRead"), "sf::st_read"))
      options("reproducible.shapefileRead" = shpFile) # can't use sf::st_read
  }
  griddedFile <- getOption("reproducible.rasterRead", "terra::rast")
  griddedFileSave <- ""
  shpFileSave <- ""
  if (griddedFile %in% "terra::rast")
    griddedFileSave <- "terra::writeRaster"
  if (griddedFile %in% "raster::raster")
    griddedFileSave <- "terra::writeRaster"
  if (shpFile %in% "sf::st_read")
    shpFileSave <- "sf::st_write"

  if (shpFile %in% "terra::vect")
    shpFileSave <- "terra::writeVector"

  df <- data.frame(
    rbind(
      c("rds", "base::readRDS", "base::saveRDS", "binary"),
      c("qs", "qs::qread", "qs::qsave", "qs"),
      cbind(c("asc", "grd", "tif"), griddedFile, griddedFileSave,
            rasterType(rasterRead = griddedFile)),
      cbind(c("shp", "gdb"), shpFile, shpFileSave,
            vectorType(vectorRead = shpFile))
    )
  )
  colnames(df) <- c("extension", "fun", "saveFun", "type")
  df
}


#' @importFrom utils packageDescription
.isDevelVersion <- function() {
  length(strsplit(packageDescription("reproducible")$Version, "\\.")[[1]]) > 3
}



#' A helper to `getOption("reproducible.rasterRead")`
#'
#' A helper to `getOption("reproducible.rasterRead")`
#' @export
#' @param ... Passed to the function parsed and evaluated from
#'   `getOption("reproducible.rasterRead")`
#'
#' @return
#' A function, that will be the evaluated, parsed character
#' string, e.g., `eval(parse(text = "terra::rast"))`
rasterRead <- function(...)
  eval(parse(text = getOption("reproducible.rasterRead")))(...)


rasterType <- function(nlayers = 1,
                       rasterRead = getOption("reproducible.rasterRead", "terra::rast")) {
  if (is.character(rasterRead)) {
    rasterRead <- if (.requireNamespace("terra") || .requireNamespace("raster"))
      eval(parse(text = rasterRead))
    else
      ""
  }
  if (!is.character(rasterRead))
    rasterRead <- if (identical(rasterRead, terra::rast))
      "SpatRaster"
    else
      if (nlayers == 1) "RasterLayer" else "RasterStack"
  rasterRead
}


vectorType <- function(vectorRead = getOption("reproducible.shapefileRead", "sf::st_read")) {
  needRasterPkg <- FALSE
  vectorReadSubs <- substitute(vectorRead)
  if (any(grepl("^shapefile$", vectorReadSubs))) {
    .requireNamespace("raster", stopOnFALSE = TRUE)
    needRasterPkg <- TRUE
  }
  if (is.character(vectorRead)) {
    if (endsWith(suffix = "shapefile", vectorRead)) {
      if (.requireNamespace("raster", stopOnFALSE = TRUE))
        needRasterPkg <- TRUE
    }
    vectorRead <- if (.requireNamespace("terra") || .requireNamespace("sf") || .requireNamespace("sp"))
      eval(parse(text = vectorRead))
    else
      ""
  }
  if (!is.character(vectorRead))
    vectorRead <- if (identical(vectorRead, terra::vect)) {
      "SpatVector"
    } else if (needRasterPkg) {
      .requireNamespace("raster", stopOnFALSE = TRUE)
      "SpatialPolygons"
    }  else {
      "sf"
    }
  vectorRead
}


#' Set seed with a random value using Sys.time()
#'
#' This will set a random seed.
#' @export
#' @param set.seed Logical. If `TRUE`, the default, then the function will call
#' `set.seed` internally with the new random seed.
#' @details
#' This function uses 6 decimal places of `Sys.time()`, i.e., microseconds. Due to
#' integer limits, it also truncates at 1000 seconds, so there is a possibility that
#' this will be non-unique after 1000 seconds (at the microsecond level). In
#' tests, this showed no duplicates after 1e7 draws in a loop, as expected.
#'
#' @note
#' This function does not appear to be as reliable on R <= 4.1.3
#'
#' @return
#' This will return the new seed invisibly. However, this is also called for
#' its side effects, which is a new seed set using `set.seed`
set.randomseed <- function(set.seed = TRUE) {
  digits <- 9
  newSeed <- as.numeric(Sys.time()) * 10^(digits - 3) # microseconds
  newSeed <- as.integer(round(newSeed, -digits) - newSeed)
  if (isTRUE(set.seed))
    set.seed(newSeed)
  return(invisible(newSeed))
}

# This used to be in helper-allEqual.R --> one of the tests couldn't find it
getDataFn <- function(...) {
  suppressWarningsSpecific({     geodata::gadm(...)   },
                           falseWarnings = "getData will be removed in a future version of raster")
}

milliseconds <- function(time = Sys.time()) {
  tt <- as.numeric(time)
  rnd <- round(tt, -5)
  (tt-rnd) * 1000
}

cat2file <- function(..., file) {
  if (missing(file))
    file = "~/log.txt"
  cat(..., file = file)
}

layerNamesDelimiter <- "_%%_"
