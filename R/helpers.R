#' @keywords internal
.pkgSnapshot <- function(instPkgs, instVers, packageVersionFile = "._packageVersionsAuto.txt") {
  browser(expr = exists("aaaa"))
  inst <- data.frame(instPkgs, instVers = unlist(instVers), stringsAsFactors = FALSE)
  write.table(inst, file = packageVersionFile, row.names = FALSE)
  inst
}

#' @importFrom utils chooseCRANmirror
#' @keywords internal
getCRANrepos <- function(repos = NULL) {
  if (is.null(repos)) {
    repos <- getOption("repos")["CRAN"]
  }

  # still might be imprecise repository, specifically ""
  if (isTRUE("" == repos)) {
    repos <- "@CRAN@"
  }

  # if @CRAN@, and non interactive session
  if (isTRUE("@CRAN@" %in% repos)) {
    cranRepo <- Sys.getenv("CRAN_REPO")
    repos <- if (nzchar(cranRepo)) {
      cranRepo
    } else {
      chooseCRANmirror2() ## sets repo option
      getOption("repos")["CRAN"]
    }
  }

  return(repos)
}

#' @importFrom utils chooseCRANmirror
#' @keywords internal
chooseCRANmirror2 <- function() {
  if (isInteractive()) {
    chooseCRANmirror()
  } else {
    chooseCRANmirror(ind = 1) ## https://cloud.r-project.org
  }
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
#' @importFrom tools file_ext file_path_sans_ext
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
  file.path(dirname(f), paste0(tools::file_path_sans_ext(basename(f)), suffix,
                               ".", tools::file_ext(f)))
}

#' Get a unique name for a given study area
#'
#' Digest a spatial object to get a unique character string (hash) of the study area.
#' Use \code{.suffix()} to append the hash to a filename, e.g., when using \code{filename2} in \code{prepInputs}.
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
    studyAreaName(studyArea, ...)
})

#' @export
#' @rdname studyAreaName
setMethod(
  "studyAreaName",
  signature = "SpatialPolygons",
  definition = function(studyArea, ...) {
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
.formalsNotInCurrentDots <- function(fun, ..., dots) {
  if (!missing(dots)) {
    out <- names(dots)[!(names(dots) %in% names(formals(fun)))]
  } else {
    out <- names(list(...))[!(names(list(...)) %in% names(formals(fun)))]
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
#'
#' @export
retry <- function(expr, envir = parent.frame(), retries = 5,
                  exponentialDecayBase = 1.3, silent = TRUE) {
  if (exponentialDecayBase <= 1)
    stop("exponentialDecayBase must be greater than 1.0")
  for (i in seq_len(retries)) {
    if (!(is.call(expr) || is.name(expr))) warning("expr is not a quoted expression")
    result <- try(expr = eval(expr, envir = envir), silent = silent)
    if (inherits(result, "try-error")) {
      backoff <- runif(n = 1, min = 0, max = exponentialDecayBase^i - 1)
      if (backoff > 3) {
        message("Waiting for ", round(backoff, 1), " seconds to retry; the attempt is failing")
      }
      Sys.sleep(backoff)
    } else {
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
.requireNamespace <- function(pkg = "methods", minVersion = NULL,
                              messageStart = paste0(pkg, if (!is.null(minVersion))
                                paste0("(>=", minVersion, ")"), " is required. Try: ")) {
  need <- FALSE
  if (suppressWarnings(!requireNamespace(pkg, quietly = TRUE, warn.conflicts = FALSE))) {
    need <- TRUE
  } else {
    if (isTRUE(packageVersion(pkg) < minVersion))
      need <- TRUE
  }
  if (need) {
    message(messageStart,
         "install.packages('",pkg,"')")
  }
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
#'
#' @export
#' @importFrom data.table is.data.table as.data.table
#' @importFrom utils capture.output
messageDF <- function(df, round, colour = NULL, colnames = NULL) {
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
      message(getFromNamespace(colour, ns = "crayon")(x))
    } else {
    message(x)
    }
  })
}
