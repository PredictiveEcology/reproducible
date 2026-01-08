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
  .Deprecated("hardLinkeOrCopy")
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
    } else if (inherits(object, "SQLiteConnection")) {
      con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
      messageCache("Making a copy of the entire SQLite database: ", object@dbname,
                   "; this may not be desireable ...",
                   verbose = verbose
      )
      out <- RSQLite::sqliteCopyDatabase(object, con)
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
      if (any(nz)) {
        fns <- fns[nz]
        fnsAll <- Filenames(object, allowMultiple = TRUE)
        hadNumeric <- grepl("_[:0-9:]+$", tools::file_path_sans_ext(fnsAll))
        needNewFn <- FALSE
        if (!missing(filebackedDir)) {
          if (!is.null(filebackedDir)) {
            needNewFn <- TRUE
          }
        }
        if (needNewFn) {
          if (!isAbsolutePath(filebackedDir)) {
            # relative
            filebackedDir <- file.path(getwd(), filebackedDir)
          }
          areAbs <- isAbsolutePath(fnsAll)
          fnsAllBase <- fnsAll
          if (any(areAbs)) {
            fnsAllBase[areAbs] <- reproducible::basename2(fnsAll[areAbs])
          }
          newFns <- file.path(filebackedDir, fnsAllBase)
        } else {
          newFns <- sapply(fnsAll, nextNumericName)
        }
        hasNumeric <- grepl("_[:0-9:]+$", tools::file_path_sans_ext(newFns))
        copyFile(fnsAll, newFns)
        # Copy may have given "nextNumericName"
        fnsBase <- tools::file_path_sans_ext(basename(fns))
        if (any(hadNumeric)) fnsBase <- gsub("_[:0-9:]+$", "", fnsBase)

        newFnsSingles <- newFns[Map(
          sn = fnsBase,
          fns1 = fns, function(sn, fns1) {
            which(startsWith(basename(newFns), sn) &
              endsWith(basename(newFns), tools::file_ext(fns1)))
          }
        ) |> unlist()]
        # newFnsSingles <- newFns[match(tools::file_path_sans_ext(basename(fns)), basename(newFns))]
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
