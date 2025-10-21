#' Normalize file paths
#'
#' Checks the specified path for formatting consistencies:
#' 1. use slash instead of backslash;
#' 2. do tilde etc. expansion;
#' 3. remove trailing slash.
#'
#' Additionally, `normPath()` attempts to create a absolute paths,
#' whereas `normPathRel()` maintains relative paths.
#'
#' ```
#' d> getwd()
#' [1] "/home/achubaty/Documents/GitHub/PredictiveEcology/reproducible"
#' d> normPathRel("potato/chips")
#' [1] "potato/chips"
#' d> normPath("potato/chips")
#' [1] "/home/achubaty/Documents/GitHub/PredictiveEcology/reproducible/potato/chips"
#' ```
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
            # fs has different behaviour than `normalizePath`. Need to replace all the differences:
            # Below are "differences"... i.e., I am not writing what normalizePath does, as it is not this:
            # 1. fs::path_norm doesn't expand
            # 2. fs::path_expand: doesn't expand R_USER env var --> must use fs::path_expand_r
            # 3. fs::??? Long windows paths that are shortened to 8 characters (e.g., on GitHub Actions),
            #    normalizePaths does this, can't find equivalent in fs
            path[!nas] <-
              normalizePath(path[!nas], winslash = "/", mustWork = FALSE)
            # fs::path_expand_r(fs::path_abs(path[!nas])) # faster than normalizePath on some machines
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

#' @importFrom fs path_expand path_norm
#' @export
#' @rdname normPath
normPathRel <- function(path) {
  if (missing(path) || is.null(path)) path <- character(0)

  path <- unlist(path)

  ## empty paths should not be normalized b/c returns the current directory
  path[nzchar(path)] <- path[nzchar(path)] |>
    fs::path_norm() |>
    fs::path_expand() # |>
  # normalizePath(winslash = "/", mustWork = FALSE)

  path
}

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
      if (.pkgEnv$SysInfo[["sysname"]] == "Darwin") path <- normPath(path) # ensure path re-normalized after creation

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

#' @importFrom fs is_absolute_path
isAbsolutePath <- function(pathnames) {
  fs::is_absolute_path(pathnames)
}

#' @importFrom fs path_abs
makeAbsolute <- function(files, absoluteBase) {
  nas <- is.na(files)
  if (!all(nas)) {
    needNormPath <- rep(TRUE, length(files))
    if (length(files[!nas])) {
      areAbs <- isAbsolutePath(files[!nas])
      if (any(!areAbs)) {
        files[!nas][!areAbs] <- fs::path_abs(files[!nas][!areAbs], absoluteBase)
        needNormPath[!nas][!areAbs] <- FALSE
      }
    }
    if (any(needNormPath))
      files[needNormPath] <- normPath(files[needNormPath])
  } else {
    files
  }
  files
}

#' Relative paths
#'
#' Extracting relative file paths.
#'
#' - `getRelative()` searches `path` "from the right" (instead of "from the left")
#' and tries to reconstruct it relative to directory specified by `relativeToPath`.
#' This is useful when dealing with symlinked paths.
#'
#' - `makeRelative()` checks to see if `files` and `normPath(absoluteBase)` share a common path
#' (i.e., "from the left"), otherwise it returns `files`.
#'
#' @param path character vector or list specifying file paths
#'
#' @param relativeToPath directory against which `path` will be relativized.
#'
#' @examplesIf !identical(.Platform$OS.type, "windows")
#'
#' ## create a project directory (e.g., on a hard drive)
#' (tmp1 <- tempdir2("myProject", create = TRUE))
#'
#' ## create a cache directory elsewhere (e.g., on an SSD)
#' (tmp2 <- tempdir2("my_cache", create = TRUE))
#'
#' ## symlink the project cache directory to tmp2
#' ## files created here are actually stored in tmp2
#' prjCache <- file.path(tmp1, "cache")
#' file.symlink(tmp2, prjCache)
#'
#' ## create a dummy cache object file in the project cache dir
#' (tmpf <- tempfile("cache_", prjCache))
#' cat(rnorm(100), file = tmpf)
#' file.exists(tmpf)
#' normPath(tmpf) ## note the 'real' location (i.e., symlink resolved)
#'
#' getRelative(tmpf, prjCache) ## relative path
#' getRelative(tmpf, tmp2) ## relative path
#'
#' makeRelative(tmpf, tmp2) ## abs path; tmpf and normPath(tmp2) don't share common path
#' makeRelative(tmpf, prjCache) ## abs path; tmpf and normPath(tmp2) don't share common path
#' makeRelative(normPath(tmpf), prjCache) ## rel path; share common path when both normPath-ed
#'
#' unlink(tmp1, recursive = TRUE)
#' unlink(tmp2, recursive = TRUE)
#'
#' @importFrom fs is_absolute_path
#' @export
#' @rdname relativePaths
getRelative <- function(path, relativeToPath) {
  path <- normPathRel(path)
  relativeToPath <- normPathRel(relativeToPath)

  relPath <- vapply(path, function(p) {
    if (is_absolute_path(p)) {
      a <- unlist(strsplit(p, "/"))
      a <- a[nzchar(a)]

      b <- unlist(strsplit(relativeToPath, "/"))
      b <- b[nzchar(b)]

      id <- which(a %in% b)
      if (length(id) > 0) {
        ## assume most internal subdirectory is the matching one
        relPath <- do.call(file.path, as.list(a[(max(id) + 1):length(a)]))
      } else {
        relPath <- p
      }
    } else {
      relPath <- p
    }
  }, character(1))

  return(relPath)
}
getRelative <- Vectorize(getRelative, USE.NAMES = FALSE)

#' @param files character vector or list specifying file paths
#' @param absoluteBase base directory (as absolute path) to prepend to `files`
#'
#' @export
#' @rdname relativePaths
makeRelative <- function(files, absoluteBase) {
  # NOT VECTORIZED on absoluteBase
  isList <- is(files, "list")
  filesOrig <- files
  if (isList) {
    nams <- names(files)
    files <- unlist(files)
  }
  if (length(files)) {
    areAbs <- isAbsolutePath(files)
    if (any(areAbs)) {
      absoluteBase <- normPath(absoluteBase) # can be "." which means 'any character' in a grep
      if (length(absoluteBase) < length(files))
        absoluteBase <- rep(absoluteBase, length.out = length(files))
      files[areAbs] <- unlist(Map(ab = absoluteBase[areAbs], file = files[areAbs], function(ab, file)
        gsub(paste0("^", ab, "/{0,1}"), "", file)
      ))
      # files[areAbs] <- gsub(paste0("^", absoluteBase, "/{0,1}"), "", files[areAbs])

      # this does dumb things when it is not relative ... i.e., with prepend ../../../../../..
      # files[areAbs] <- fs::path_rel(start = absoluteBase, files[areAbs])
    }
  }
  if (length(files)) {
    hasStartingDot <- startsWith(files, ".")
    if (isTRUE(any(hasStartingDot)))
      files[hasStartingDot] <- fs::path_norm(files[hasStartingDot])
  }
  if (isList) {
    if (length(nams) == length(files)) {
      names(files) <- nams
    }
  }
  files
}

#' An alternative to `basename` and `dirname` when there are sub-folders
#'
#' This confirms that the `files` which may be absolute actually
#'   exist when compared `makeRelative(knownRelativeFiles, absolutePrefix)`.
#'   This is different than just using `basename` because it will include any
#'   sub-folder structure within the `knownRelativePaths`
#'
#' @param files A character vector of files to check to see if they are the same
#'   as `knownRelativeFiles`, once the `absolutePrefix` is removed
#'
#' @param absolutePrefix A directory to "remove" from `files` to compare
#'   to `knownRelativeFiles`
#'
#' @param knownRelativeFiles A character vector of relative filenames, that could
#'   have sub-folder structure.
#'
#' @inheritParams prepInputs
#'
checkRelative <- function(files, absolutePrefix, knownRelativeFiles,
                          verbose = getOption("reproducible.verbose")) {
  if (!is.null(knownRelativeFiles)) {
    neededFilesRel <- makeRelative(files, absolutePrefix)
    areAbs <- isAbsolutePath(knownRelativeFiles)
    if (any(areAbs)) {
      knownRelativeFiles[areAbs] <- makeRelative(knownRelativeFiles, absolutePrefix)
    }
    relativeNamesCorrect <- neededFilesRel %in% knownRelativeFiles

    if (!all(relativeNamesCorrect)) { # means user has asked for incorrect relative path

      #  must include directory names
      knownRelativeFiles <- unique(c(knownRelativeFiles, dirname(knownRelativeFiles)))
      knownRelativeFilesBase <- basename2(knownRelativeFiles)
      basenamesCorrect <- basename2(neededFilesRel) %in% knownRelativeFilesBase
      needUpdateRelNames <- basenamesCorrect & !relativeNamesCorrect # filename is correct, but not nesting structure
      if (any(needUpdateRelNames)) { # means basenames
        # needUpdateFromArchive <- !knownRelativeFilesBase %in% basename2(neededFilesRel)
        needUpdateFromArchive <- match(basename2(neededFilesRel)[needUpdateRelNames], knownRelativeFilesBase)
        files[needUpdateRelNames] <- makeAbsolute(knownRelativeFiles[needUpdateFromArchive], absolutePrefix)
        files <- unique(files)
        messagePrepInputs("User supplied files don't correctly specify the ",
                          "files in the archive (likely because of sub-folders); \n",
                          "using items in archive with same basenames. Renaming to these: \n",
                          paste(makeRelative(files[needUpdateRelNames], absoluteBase = absolutePrefix), collapse = "\n"),
                          verbose = verbose
        )
      }
    }
  }
  files
}

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
