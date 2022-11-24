setGeneric("normPath", function(path) {
  standardGeneric("normPath")
})

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

setMethod(
  "normPath",
  signature(path = "list"),
  definition = function(path) {
    return(normPath(unlist(path)))
  }
)

setMethod(
  "normPath",
  signature(path = "NULL"),
  definition = function(path) {
    return(character(0))
  }
)

setMethod(
  "normPath",
  signature(path = "missing"),
  definition = function() {
    return(character(0))
  }
)

setMethod(
  "normPath",
  signature(path = "logical"),
  definition = function(path) {
    return(NA_character_)
  }
)

setGeneric("checkPath", function(path, create) {
  standardGeneric("checkPath")
})

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
            verboseLevel = 1,
            verbose = getOption("Require.verbose")
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
      if (Sys.info()[["sysname"]] == "Darwin") {
        path <-
          normPath(path)
      } # ensure path re-normalized after creation

      return(path)
    }
  }
)

setMethod(
  "checkPath",
  signature(path = "character", create = "missing"),
  definition = function(path) {
    return(checkPath(path, create = FALSE))
  }
)

setMethod(
  "checkPath",
  signature(path = "NULL", create = "ANY"),
  definition = function(path) {
    stop("Invalid path: cannot be NULL.")
  }
)

setMethod(
  "checkPath",
  signature(path = "missing", create = "ANY"),
  definition = function() {
    stop("Invalid path: no path specified.")
  }
)

tempdir2 <- function(sub = "",
                     tempdir = getOption("reproducible.tempPath", .reproducibleTempPath()),
                     create = TRUE) {
  np <- normPath(file.path(tempdir, sub))
  if (isTRUE(create)) {
    checkPath(np, create = TRUE)
  }
  np
}

