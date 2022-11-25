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
      nas <- is.na(path)
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
        path <- gsub("\\\\", "//", path)
        path <- gsub("//", "/", path)
        hasDotStart <- startsWith(path, "./")
        if (isTRUE(any(hasDotStart))) {
          path[hasDotStart] <-
            gsub("^[.]/", paste0(getwd(), "/"), path[hasDotStart])
        }
        path <- gsub("/$", "", path) # nolint
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
#' @seealso [file.exists()], [dir.create()].
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
      path <-
        normPath(path) # this is necessary to cover Windows
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
              paste(
                "Specified path",
                normPath(path),
                "does not exist.",
                "Create it and try again."
              )
            )
          }
        }
      }
      if (SysInfo[["sysname"]] == "Darwin") {
        path <-
          normPath(path)
      } # ensure path re-normalized after creation

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
#' @param tempdir Optional character string where the
#'   temporary dir should be placed. Defaults to `getOption("reproducible.tempPath")`.
#' @param create Logical. Should the directory be created. Default `TRUE`.
#'
#' @export
tempdir2 <- function(sub = "",
                     tempdir = getOption("reproducible.tempPath", .reproducibleTempPath()),
                     create = TRUE) {
  np <- normPath(file.path(tempdir, sub))
  if (isTRUE(create)) {
    checkPath(np, create = TRUE)
  }
  np
}

SysInfo <- Sys.info() # do this on load; nothing can change, so repeated calls are a waste
