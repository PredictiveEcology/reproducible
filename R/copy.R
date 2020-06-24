#' Move a file to a new location
#'
#' @param from,to character vectors, containing file names or paths.
#' @param overwrite logical indicating whether to overwrite destination file if it exists.
#'
#' @return Logical indicating whether operation succeeded.
#'
#' @export
file.move <- function(from, to, overwrite = FALSE) {
  stopifnot(file.exists(from))
  res <- suppressWarnings(file.rename(from = from, to = to))
  if (isFALSE(res)) {
    res2 <- file.copy(from = from, to = to, overwrite = overwrite)
    if (isTRUE(res2)) {
      file.remove(from)
    }
    return(res2)
  } else {
    return(res)
  }
}

#' Make a temporary sub-directory or file in that subdirectory
#'
#' Create a temporary subdirectory in \code{.reproducibleTempPath()}, or a
#' temporary file in that temporary subdirectory.
#'
#' @param sub Character string, length 1. Can be a result of
#'   \code{file.path("smth", "smth2")} for nested temporary sub
#'   directories.
#' @param tempdir Optional character string where the temporary dir should be placed.
#'   Defaults to \code{.reproducibleTempPath()}.
#'
#' @importFrom Require normPath
#' @export
tempdir2 <- function(sub = "", tempdir = getOption("reproducible.tempPath", .reproducibleTempPath())) {
  Require::checkPath(Require::normPath(file.path(tempdir, sub)), create = TRUE)
}

#' @param ... passed to \code{tempfile}, e.g., \code{fileext}
#'
#' @importFrom Require normPath
#' @export
tempfile2 <- function(sub = "", ...) {
  normPath(file.path(tempdir2(sub = sub), basename(tempfile(...))))
}

#' Recursive copying of nested environments, and other "hard to copy" objects
#'
#' When copying environments and all the objects contained within them, there are
#' no copies made: it is a pass-by-reference operation. Sometimes, a deep copy is
#' needed, and sometimes, this must be recursive (i.e., environments inside
#' environments).
#'
#' @details
#' To create a new Copy method for a class that needs its own method, try something like
#' shown in example and put it in your package (or other R structure).
#'
#'
#' @param object  An R object (likely containing environments) or an environment.
#'
#' @param filebackedDir A directory to copy any files that are backing R objects,
#'                      currently only valid for \code{Raster} classes. Defaults
#'                      to \code{.reproducibleTempPath()}, which is unlikely to be very useful.
#'                      Can be \code{NULL}, which means that the file will not be
#'                      copied and could therefore cause a collision as the
#'                      pre-copied object and post-copied object would have the same
#'                      file backing them.
#'
#' @param ... Only used for custom Methods
#'
#' @author Eliot McIntire
#' @export
#' @importFrom data.table copy
#' @inheritParams Cache
#' @rdname Copy
#' @seealso \code{\link{.robustDigest}}
#'
#' @examples
#' e <- new.env()
#' e$abc <- letters
#' e$one <- 1L
#' e$lst <- list(W = 1:10, X = runif(10), Y = rnorm(10), Z = LETTERS[1:10])
#' ls(e)
#'
#' # 'normal' copy
#' f <- e
#' ls(f)
#' f$one
#' f$one <- 2L
#' f$one
#' e$one ## uh oh, e has changed!
#'
#' # deep copy
#' e$one <- 1L
#' g <- Copy(e)
#' ls(g)
#' g$one
#' g$one <- 3L
#' g$one
#' f$one
#' e$one
#'
#' \dontrun{
#' setMethod("Copy", signature = "the class", # where = specify here if not in a package,
#'   definition = function(object, filebackendDir, ...) {
#'   # write deep copy code here
#' })
#' }
setGeneric("Copy", function(object, filebackedDir, ...) {
  standardGeneric("Copy")
})

#' @rdname Copy
setMethod(
  "Copy",
  signature(object = "ANY"),
  definition = function(object, # filebackedDir,
                        ...) {
    if (any(grepl("DBIConnection", is(object)))) {
      message("Copy will not do a deep copy of a DBI connection object; no copy being made. ",
              "This may have unexpected consequences...")
    }

    if (is(object, "proto")) { # don't want to import class for reproducible package; an edge case
      return(get(class(object)[1])(object))
    }

    # keep this environment method here, as it will intercept "proto"
    #   and other environments that it shouldn't
    if (!identical(is(object)[1], "environment") && is.environment(object)) {
      message("Trying to do a deep copy (Copy) of object of class ", class(object),
              "which does not appear to be a normal environment. If it can be copied ",
              "like a normal environment, ignore this message; otherwise, may need to create ",
              "a Copy method for this class. See ?Copy")

    }
    if (is.environment(object)) {
      # if (missing(filebackedDir)) {
      #   filebackedDir <- tempdir2(rndstr(1, 9))
      # }
      listVersion <- Copy(as.list(object, all.names = TRUE),
                          #filebackedDir = filebackedDir,
                          ...)

      parentEnv <- parent.env(object)
      newEnv <- new.env(parent = parentEnv)
      list2env(listVersion, envir = newEnv)
      attr(newEnv, "name") <- attr(object, "name")
      return(newEnv)

    }
    return(object)
})


#' @rdname Copy
setMethod("Copy",
          signature(object = "SQLiteConnection"),
          definition = function(object, ...) {
            con <- dbConnect(RSQLite::SQLite(), ":memory:")
            message("Making a copy of the entire SQLite database: ",object@dbname,
                    "; this may not be desireable ...")
            RSQLite::sqliteCopyDatabase(object, con)
})

#' @rdname Copy
setMethod("Copy",
          signature(object = "data.table"),
          definition = function(object, ...) {
            data.table::copy(object)
})

#' @rdname Copy
setMethod("Copy",
          signature(object = "list"),
          definition = function(object,  ...) {
            #if (missing(filebackedDir)) {
            #  browser()
            #  filebackedDir <- tempdir2(rndstr(1, 10))
            #}
            lapply(object, function(x) {
              Copy(x, ...)
            })
})

#' @rdname Copy
setMethod("Copy",
          signature(object = "refClass"),
          definition = function(object,  filebackedDir, ...) {
            if (exists("copy", envir = object)) {
              object$copy()
            } else {
              stop("There is no method to copy this refClass object; ",
                   "see developers of reproducible package")
            }
})

#' @rdname Copy
setMethod("Copy",
          signature(object = "data.frame"),
          definition = function(object,  filebackedDir, ...) {
            object
})

#' @rdname Copy
#' @inheritParams DBI::dbConnect
setMethod("Copy",
          signature(object = "Raster"),
          definition = function(object, filebackedDir,
                                drv = getOption("reproducible.drv", RSQLite::SQLite()),
                                conn = getOption("reproducible.conn", NULL), ...) {
            if (fromDisk(object)) {
              if (missing(filebackedDir)) {
                filebackedDir <- tempdir2(rndstr(1, 11))
              }
              if (!is.null(filebackedDir))
                object <- .prepareFileBackedRaster(object, repoDir = filebackedDir, drv = drv, conn = conn)
            }
            object
})
