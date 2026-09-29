#' Move a file to a new location -- Defunct -- use `hardLinkOrCopy`
#'
#' This will first try to `file.rename`, and if that fails, then it will
#' `file.copy` then `file.remove`.
#' @param from,to character vectors, containing file names or paths.
#' @param overwrite logical indicating whether to overwrite destination file if it exists.
#' @export
#' @return Logical indicating whether operation succeeded.
#'
.file.move <- function(from, to, overwrite = FALSE) {
  .Deprecated("hardLinkOrCopy")
  hardLinkOrCopy(from, to, overwrite)
  file.remove(from)
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
#' @param filebackedDir A directory to copy any files that are backing R objects
#'                      (`Raster` and `SpatRaster` classes). If missing, a fresh temporary
#'                      directory is used. Can be `NULL`, which means that the file will not be
#'                      copied and could therefore cause a collision as the
#'                      pre-copied object and post-copied object would have the same
#'                      file backing them. A copy is never written beside the original file.
#'
#' @param ... Only used for custom Methods
#'
#' @author Eliot McIntire
#' @export
#' @importFrom data.table copy
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
#' @inheritParams Cache
setMethod(
  "Copy",
  signature(object = "ANY"),
  definition = function(object, filebackedDir,
                        drv = getDrv(getOption("reproducible.drv", NULL)),
                        conn = getOption("reproducible.conn", NULL),
                        verbose = getOption("reproducible.verbose"),
                        ...) {
    out <- object # many methods just do a pass through
    if (any(grepl("DBIConnection", is(object)))) {
      messageCache("Copy will not do a deep copy of a DBI connection object; no copy being made. ",
                   "This may have unexpected consequences...",
                   verbose = verbose
      )
    } else if (is(object, "proto")) { # don't want to import class for reproducible package; an edge case
      out <- get(class(object)[1])(object)
    } else if (!identical(is(object)[1], "environment") && is.environment(object)) {
      # keep this environment method here, as it will intercept "proto"
      #   and other environments that it shouldn't
      messageCache("Trying to do a deep copy (Copy) of object of class ", class(object),
                   ", which does not appear to be a normal environment. If it can be copied ",
                   "like a normal environment, ignore this message; otherwise, may need to create ",
                   "a Copy method for this class. See ?Copy",
                   verbose = verbose
      )
    } else if (is.environment(object)) {
      listVersion <- Copy(as.list(object, all.names = TRUE),
                          filebackedDir = filebackedDir,
                          drv = drv, conn = conn, verbose = verbose, ...
      )

      parentEnv <- parent.env(object)
      out <- new.env(parent = parentEnv)
      list2env(listVersion, envir = out)
      attr(out, "name") <- attr(object, "name")
    } else if (inherits(object, "Raster")) {
      if (any(nchar(Filenames(object)) > 0)) {
        if (missing(filebackedDir)) {
          filebackedDir <- tempdir2(rndstr(1, 11))
        }
        if (!is.null(filebackedDir)) {
          out <- .prepareFileBackedRaster(object, repoDir = filebackedDir, drv = drv, conn = conn,
                                          verbose = verbose)
        }
      }
    } else if (.isSpatRaster(object)) {
      fns <- Filenames(object, allowMultiple = FALSE)
      nz <- nzchar(fns)
      ## `filebackedDir` means the same as for `Raster`: missing, a fresh temporary directory; `NULL`,
      ## the files are not copied; a directory, the copies go there. A copy is never written beside
      ## the original: two processes that share a folder would both write the same "<name>_1" file
      ## (each Cache() memoise of a simList did this, 2026-09-28).
      if (missing(filebackedDir)) {
        filebackedDir <- tempdir2(rndstr(1, 11))
      }
      if (any(nz) && !is.null(filebackedDir)) {
        fns <- fns[nz]
        fnsAll <- Filenames(object, allowMultiple = TRUE)
        if (!isAbsolutePath(filebackedDir)) {
          filebackedDir <- file.path(getwd(), filebackedDir)
        }
        checkPath(filebackedDir, create = TRUE)
        newFns <- file.path(filebackedDir, basename2(fnsAll))
        ## a second copy into the same directory takes the next free number
        taken <- file.exists(newFns)
        if (any(taken)) {
          newFns[taken] <- vapply(newFns[taken], nextNumericName, character(1))
        }
        copyFile(fnsAll, newFns)
        newFnsSingles <- newFns[match(fns, fnsAll)]
        out <- terra::rast(newFnsSingles)
        if (length(nz) == 1) { # one file for all layers
          names(out) <- names(object)
        } else {
          names(out) <- names(object[[nz]])
        }

        # If there are layers that were in RAM; need to add them back, in correct order
        if (any(!nz)) {
          memoryLayers <- names(object)[!nz]
          out[[memoryLayers]] <- object[[memoryLayers]]
          out <- out[[match(names(object), names(out))]]
        }
      }
    }
    return(out)
  }
)

#' @rdname Copy
setMethod("Copy",
  signature(object = "data.table"),
  definition = function(object, ...) {
    data.table::copy(object)
  }
)

#' @rdname Copy
setMethod("Copy",
  signature(object = "list"),
  definition = function(object, ...) {
    lapply(object, function(x) {
      Copy(x, ...)
    })
  }
)

#' @rdname Copy
setMethod("Copy",
  signature(object = "refClass"),
  definition = function(object, ...) {
    if (exists("copy", envir = object)) {
      object$copy()
    } else {
      stop(
        "There is no method to copy this refClass object; ",
        "see developers of reproducible package"
      )
    }
  }
)

#' @rdname Copy
setMethod("Copy",
  signature(object = "data.frame"),
  definition = function(object, ...) {
    object
  }
)
