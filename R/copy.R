#' Move a file to a new location
#'
#' This will first try to `file.rename`, and if that fails, then it will
#' `file.copy` then `file.remove`.
#' @param from,to character vectors, containing file names or paths.
#' @param overwrite logical indicating whether to overwrite destination file if it exists.
#' @export
#' @return Logical indicating whether operation succeeded.
#'
.file.move <- function(from, to, overwrite = FALSE) {
  stopifnot(file.exists(from))
  res <- suppressWarnings(file.rename(from = from, to = to))

  if (!isTRUE(all(res))) {
    res2 <- file.copy(from = from, to = to, overwrite = overwrite)
    if (isTRUE(all(res2))) {
      file.remove(from)
    }
    return(res2)
  } else {
    return(res)
  }
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
#'                      currently only valid for `Raster` classes. Defaults
#'                      to `.reproducibleTempPath()`, which is unlikely to be very useful.
#'                      Can be `NULL`, which means that the file will not be
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
#' @return
#' The same object as `object`, but with pass-by-reference class elements "deep" copied.
#' `reproducible` has methods for several classes.
#'
#' @seealso [.robustDigest()], [Filenames()]
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
#' ## To create a new deep copy method, use the following template
#' ## setMethod("Copy", signature = "the class", # where = specify here if not in a package,
#' ##           definition = function(object, filebackendDir, ...) {
#' ##           # write deep copy code here
#' ##           })
#'
setGeneric("Copy", function(object, ...) {
  standardGeneric("Copy")
})

#' @rdname Copy
setMethod(
  "Copy",
  signature(object = "ANY"),
  definition = function(object, # filebackedDir,
                        ...) {
    if (any(grepl("DBIConnection", is(object)))) {
      messageCache("Copy will not do a deep copy of a DBI connection object; no copy being made. ",
              "This may have unexpected consequences...")
    }

    if (is(object, "proto")) { # don't want to import class for reproducible package; an edge case
      return(get(class(object)[1])(object))
    }

    # keep this environment method here, as it will intercept "proto"
    #   and other environments that it shouldn't
    if (!identical(is(object)[1], "environment") && is.environment(object)) {
      messageCache("Trying to do a deep copy (Copy) of object of class ", class(object),
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
            messageCache("Making a copy of the entire SQLite database: ",object@dbname,
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
            lapply(object, function(x) {
              Copy(x, ...)
            })
})

#' @rdname Copy
setMethod("Copy",
          signature(object = "refClass"),
          definition = function(object,  ...) {
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
          definition = function(object,  ...) {
            object
})

#' @rdname Copy
#' @inheritParams DBI::dbConnect
setMethod("Copy",
          signature(object = "Raster"),
          definition = function(object, filebackedDir,
                                drv = getOption("reproducible.drv", RSQLite::SQLite()),
                                conn = getOption("reproducible.conn", NULL), ...) {
            # raster::fromDisk fails when only some of the RasterLayers in a RasterStack are fromDisk
            #  --> changing to Filenames
            # if (fromDisk(object)) {
            if (any(nchar(Filenames(object)) > 0)) {
              if (missing(filebackedDir)) {
                filebackedDir <- tempdir2(rndstr(1, 11))
              }
              if (!is.null(filebackedDir))
                object <- .prepareFileBackedRaster(object, repoDir = filebackedDir, drv = drv, conn = conn)
            }
            object
})
