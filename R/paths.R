#' Normalize filepath
#'
#' Checks the specified filepath for formatting consistencies:
#'  1) use slash instead of backslash;
#'  2) do tilde etc. expansion;
#'  3) remove trailing slash.
#'
#' @param path A character vector of filepaths.
#'
#' @return Character vector of cleaned up filepaths.
#'
#' @export
#' @rdname normPath
#'
#' @example inst/examples/example_checkPath.R
#'
setGeneric("normPath", function(path) {
  standardGeneric("normPath")
})

#' @export
#' @rdname normPath
setMethod(
  "normPath",
  signature(path = "character"),
  definition = function(path) {
    if (length(path) > 0) {
      nonEmpty <- nzchar(path)
      if (any(nonEmpty)) {
        pathOrig <- path
        path <- pathOrig[nonEmpty]

        nas <- is.na(path)
        if (!all(nas)) {
          if (any(!nas)) {
            path[!nas] <-
              normalizePath(path[!nas], winslash = "/", mustWork = FALSE)
          }
          if (any(nas)) {
            path[nas] <- NA_character_
          }

          # Eliot changed this Sept 24, 2019 because weird failures with getwd()
          # in non-interactive testing
          path <- unlist(path)
          if (!is.null(path)) {
            nonNApath <- path[!nas]
            nonNApath <- gsub("\\\\", "//", nonNApath)
            nonNApath <- gsub("//", "/", nonNApath)
            hasDotStart <- startsWith(nonNApath, "./")
            if (isTRUE(any(hasDotStart))) {
              nonNApath[hasDotStart] <-
                gsub("^[.]/", paste0(getwd(), "/"), nonNApath[hasDotStart]) # is absolute b/c getwd()
            }
            nonNApath <- gsub("/$", "", nonNApath) # nolint


            # if the files or dirs don't exist, then there is a possibility on *nix-alikes that they will still
            #    not be absolute -- this is true with R 4.2.3, maybe was not previously
            if (!isWindows() && !all(nas) && any(hasDotStart %in% FALSE)) {
              areAbs <- isAbsolutePath(nonNApath[!hasDotStart]) # hasDotStart is already absolute; still this is slow
              if (any(areAbs %in% FALSE)) {
                nonNApath[!hasDotStart][areAbs %in% FALSE] <-
                  normalizePath(file.path(getwd(), nonNApath[!hasDotStart][areAbs %in% FALSE]),
                    winslash = "/", mustWork = FALSE
                  )
              }
            }
            path[!nas] <- nonNApath

            if (!all(nonEmpty)) {
              pathOrig[nonEmpty] <- path
              path <- pathOrig
            }
          }
        }
      }
    }
    return(path)
  }
)

#' @export
#' @rdname normPath
setMethod(
  "normPath",
  signature(path = "list"),
  definition = function(path) {
    return(normPath(unlist(path)))
  }
)

#' @export
#' @rdname normPath
setMethod(
  "normPath",
  signature(path = "NULL"),
  definition = function(path) {
    return(character(0))
  }
)

#' @export
#' @rdname normPath
setMethod(
  "normPath",
  signature(path = "missing"),
  definition = function() {
    return(character(0))
  }
)

#' @export
#' @rdname normPath
setMethod(
  "normPath",
  signature(path = "logical"),
  definition = function(path) {
    return(NA_character_)
  }
)

#' Check directory path
#'
#' Checks the specified path to a directory for formatting consistencies,
#' such as trailing slashes, etc.
#'
#' @note This will not work for paths to files.
#' To check for existence of files, use `file.exists()`.
#' To normalize a path to a file, use `normPath()` or `normalizePath()`.
#'
#' @param path A character string corresponding to a directory path.
#'
#' @param create A logical indicating whether the path should
#' be created if it does not exist. Default is `FALSE`.
#'
#' @return Character string denoting the cleaned up filepath.
#'
#' @seealso [file.exists()], [dir.create()], [normPath()]
#'
#' @export
#' @rdname checkPath
#'
#' @example inst/examples/example_checkPath.R
#'
setGeneric("checkPath", function(path, create) {
  standardGeneric("checkPath")
})

#' @export
#' @rdname checkPath
setMethod(
  "checkPath",
  signature(path = "character", create = "logical"),
  definition = function(path, create) {
    if (isTRUE(all(is.na(path)))) {
      stop("Invalid path: cannot be NA.")
    } else {
      path <- normPath(path) # this is necessary to cover Windows
      # double slash used on non-Windows
      dirsThatExist <- dir.exists(path)
      if (any(!dirsThatExist)) {
        isExistingFile <- file.exists(path)
        if (all(isExistingFile)) {
          messageCache(
            "That path is an existing file(s)",
            verboseLevel = 0,
            verbose = getOption("reproducible.verbose")
          )
        } else {
          if (create == TRUE) {
            lapply(path[!dirsThatExist[!isExistingFile]], function(pth) {
              dir.create(file.path(pth),
                recursive = TRUE,
                showWarnings = FALSE
              )
            })
          } else {
            stop(
              "Specified path, ", normPath(path), ", does not exist. Maybe set `create = TRUE?`"
            )
          }
        }
      }
      if (SysInfo[["sysname"]] == "Darwin") path <- normPath(path) # ensure path re-normalized after creation

      return(path)
    }
  }
)

#' @export
#' @rdname checkPath
setMethod(
  "checkPath",
  signature(path = "character", create = "missing"),
  definition = function(path) {
    return(checkPath(path, create = FALSE))
  }
)

#' @export
#' @rdname checkPath
setMethod(
  "checkPath",
  signature(path = "NULL", create = "ANY"),
  definition = function(path) {
    stop("Invalid path: cannot be NULL.")
  }
)

#' @export
#' @rdname checkPath
setMethod(
  "checkPath",
  signature(path = "missing", create = "ANY"),
  definition = function() {
    stop("Invalid path: no path specified.")
  }
)

#' Make a temporary (sub-)directory
#'
#' Create a temporary subdirectory in `getOption("reproducible.tempPath")`.
#'
#' @param sub Character string, length 1. Can be a result of
#'   `file.path("smth", "smth2")` for nested temporary subdirectories.
#'   If the zero length character, then a random sub-directory will be created.
#' @param tempdir Optional character string where the temporary
#'   directory should be placed. Defaults to `getOption("reproducible.tempPath")`.
#' @param create Logical. Should the directory be created. Default `TRUE`.
#' @return
#' A character string of a path (that will be created if `create = TRUE`) in a
#' sub-directory of the `tempdir()`.
#'
#' @seealso [tempfile2]
#' @export
tempdir2 <- function(sub = "",
                     tempdir = getOption("reproducible.tempPath", .reproducibleTempPath()),
                     create = TRUE) {
  if (!nzchar(sub)) {
    sub <- rndstr(1)
  }
  np <- normPath(file.path(tempdir, sub))
  if (isTRUE(create)) {
    checkPath(np, create = TRUE)
  }
  np
}

#' Make a temporary file in a temporary (sub-)directory
#'
#' @param ... passed to `tempfile`, e.g., `fileext`
#'
#' @seealso [tempdir2]
#' @inheritParams tempdir2
#' @param ... passed to `tempfile`, e.g., `fileext`
#' @return
#' A character string of a path to a file in a
#' sub-directory of the `tempdir()`. This file will likely not exist yet.
#' @export
tempfile2 <- function(sub = "",
                      tempdir = getOption("reproducible.tempPath", .reproducibleTempPath()),
                      ...) {
  normPath(file.path(tempdir2(sub = sub, tempdir = tempdir), basename(tempfile(...))))
}

SysInfo <- Sys.info() # do this on load; nothing can change, so repeated calls are a waste
