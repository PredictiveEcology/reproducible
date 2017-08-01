################################################################################
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
#' @importFrom magrittr %>%
#' @rdname normPath
#'
setGeneric("normPath", function(path) {
  standardGeneric("normPath")
})

#' @export
#' @rdname normPath
setMethod("normPath",
          signature(path = "character"),
          definition = function(path) {
            lapply(path, function(x) {
              if (is.na(x)) {
                NA_character_
              } else {
                normalizePath(x, winslash = "/", mustWork = FALSE)
              }
            }) %>%
              unlist() %>%
              gsub("^[.]", paste0(getwd()), .) %>%
              gsub("\\\\", "//", .) %>%
              gsub("//", "/", .) %>%
              gsub("/$", "", .) # nolint
})

#' @export
#' @rdname normPath
setMethod("normPath",
          signature(path = "list"),
          definition = function(path) {
            return(normPath(unlist(path)))
})

#' @export
#' @rdname normPath
setMethod("normPath",
          signature(path = "NULL"),
          definition = function(path) {
            return(character(0))
})

#' @export
#' @rdname normPath
setMethod("normPath",
          signature(path = "missing"),
          definition = function() {
            return(character(0))
})

################################################################################
#' Check filepath
#'
#' Checks the specified filepath for formatting consistencies,
#' such as trailing slashes, etc.
#'
#' @param path A character string corresponding to a filepath.
#'
#' @param create A logical indicating whether the path should
#' be created if it doesn't exist. Default is \code{FALSE}.
#'
#' @return Character string denoting the cleaned up filepath.
#'
#' @seealso \code{\link{file.exists}}, \code{\link{dir.create}}.
#'
#' @export
#' @importFrom magrittr %>%
#' @rdname checkPath
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
    if (length(path) != 1) {
      stop("path must be a character vector of length 1.")
    } else {
      if (is.na(path)) {
        stop("Invalid path: cannot be NA.")
      } else {
        path <- normPath(path)

        if (!file.exists(path)) {
          if (create == TRUE) {
            dir.create(file.path(path), recursive = TRUE, showWarnings = FALSE)
          } else {
            stop(paste("Specified path", path, "doesn't exist.",
                       "Create it and try again."))
          }
        }
        return(normPath(path)) # ensure path re-normalized after creation (see #267)
      }
    }
})

#' @export
#' @rdname checkPath
setMethod("checkPath",
          signature(path = "character", create = "missing"),
          definition = function(path) {
            return(checkPath(path, create = FALSE))
})

#' @export
#' @rdname checkPath
setMethod("checkPath",
          signature(path = "NULL", create = "ANY"),
          definition = function(path) {
            stop("Invalid path: cannot be NULL.")
})

#' @export
#' @rdname checkPath
setMethod("checkPath",
          signature(path = "missing", create = "ANY"),
          definition = function() {
            stop("Invalid path: no path specified.")
})
