################################################################################
#' Create reproducible digests of objects in R
#'
#' Not all aspects of R objects are captured by current hashing tools in R (e.g.
#' \code{digest::digest}, \code{knitr} caching,
#' \code{archivist::cache}). This is mostly because many objects have "transient"
#' (e.g., functions have environments), or "disk-backed" features. Since
#' the goal of using reproducibility is to have tools that are not session specific,
#' this function
#' attempts to strip all session specific information so that the digest
#' works between sessions and operating systems. It is tested under many
#' conditions and object types, there are bound to be others that don't
#' work correctly.
#'
#' @section Classes:
#'
#' \code{Raster*} objects have the potential for disk-backed storage.
#' If the object in the R session is cached using \code{archivist::cache}, only
#' the header component will be assessed for caching. Thus, objects like this
#' require more work. Also, because \code{Raster*} can have a built-in representation
#' for having their data content located on disk, this format will be maintained if the
#' raster already is file-backed, i.e., to create \code{.tif} or \code{.grd} backed rasters,
#' use \code{writeRaster} first, then Cache. The .tif or .grd will be copied to the "raster"
#' subdirectory of the \code{cacheRepo}.
#' Their RAM representation (as an R object) will still be in the usual \file{gallery/} directory.
#' For \code{inMemory} raster objects, they will remain as binary \code{.RData} files.
#'
#' Functions (which are contained within environments) are
#' converted to a text representation via a call to \code{format(FUN)}.
#'
#' Objects contained within a list or environment are recursively hashed
#' using \code{\link[digest]{digest}}, while removing all references to
#' environments.
#'
#' Character strings are first assessed with \code{dir.exists} and \code{file.exists}
#' to check for paths. If they are found to be paths, then the path is hashed with
#' only its filename via \code{basename(filename)}. If it is actually a path, we suggest
#' using \code{asPath(thePath)}
#'
#' @param object an object to digest.
#'
#' @param objects Optional character vector indicating which objects are to
#'                be considered while making digestible. This argument is not used
#'                in the default cases; the only known method that uses this
#'                in the default cases; the only known method that uses this
#'                argument is the \code{simList} class from \code{SpaDES.core}.
#'
#' @inheritParams Cache
#'
#' @return A hash i.e., digest of the object passed in.
#'
#' @seealso \code{\link[archivist]{cache}}.
#'
#' @author Eliot McIntire
#' @export
#' @importFrom digest digest
#' @importFrom fastdigest fastdigest
#' @keywords internal
#' @rdname robustDigest
#' @examples
#'
#' a <- 2
#' tmpfile1 <- tempfile()
#' tmpfile2 <- tempfile()
#' save(a, file = tmpfile1)
#' save(a, file = tmpfile2)
#'
#' # treats as character string, so 2 filenames are different
#' digest::digest(tmpfile1)
#' digest::digest(tmpfile2)
#'
#' # tests to see whether character string is representing a file
#' .robustDigest(tmpfile1)
#' .robustDigest(tmpfile2) # same
#'
#' # if you tell it that it is a path, then you can decide if you want it to be
#' #  treated as a character string or as a file path
#' .robustDigest(asPath(tmpfile1), quick = TRUE)
#' .robustDigest(asPath(tmpfile2), quick = TRUE) # different because using file info
#'
#' .robustDigest(asPath(tmpfile1), quick = FALSE)
#' .robustDigest(asPath(tmpfile2), quick = FALSE) # same because using file content
#'
#' # Rasters are interesting because it is not know a priori if it
#' #   it has a file name associated with it.
#' library(raster)
#' r <- raster(extent(0,10,0,10), vals = 1:100)
#'
#' # write to disk
#' r1 <- writeRaster(r, file = tmpfile1)
#' r2 <- writeRaster(r, file = tmpfile2)
#'
#' digest::digest(r1)
#' digest::digest(r2) # different
#' digest::digest(r1)
#' digest::digest(r2) # different
#' .robustDigest(r1)
#' .robustDigest(r2) # same... data are the same in the file
#'
#' # note, this is not true for comparing memory and file-backed rasters
#' .robustDigest(r)
#' .robustDigest(r1) # different
#'
setGeneric(".robustDigest", function(object, .objects,
                                     length = getOption("reproducible.length", Inf),
                                     algo = "xxhash64",
                                     quick = getOption("reproducible.quick", FALSE),
                                     classOptions = list(), ...) {
  standardGeneric(".robustDigest")
})

#' @rdname robustDigest
#' @export
setMethod(
  ".robustDigest",
  signature = "ANY",
  definition = function(object, .objects, length, algo, quick,
                        classOptions) {
    object <- .removeCacheAtts(object)
    if (isTRUE(getOption("reproducible.useNewDigestAlgorithm")))
      digest(object, algo = algo)
    else
      fastdigest(object)
})

#' @import parallel
setOldClass("cluster")

#' @rdname robustDigest
#' @export
setMethod(
  ".robustDigest",
  signature = "cluster",
  definition = function(object, .objects, length, algo, quick, classOptions) {
    #object <- .removeCacheAtts(object)
    if (isTRUE(getOption("reproducible.useNewDigestAlgorithm")))
      digest(NULL, algo = algo)
    else
      fastdigest(NULL)
})

#' @rdname robustDigest
#' @export
setMethod(
  ".robustDigest",
  signature = "function",
  definition = function(object, .objects, length, algo, quick, classOptions) {
    .robustDigestFormatOnly(object, algo = algo)
})

#' @rdname robustDigest
#' @export
setMethod(
  ".robustDigest",
  signature = "expression",
  definition = function(object, .objects, length, algo, quick, classOptions) {
    .robustDigestFormatOnly(object, algo = algo)
})

#' @rdname robustDigest
#' @export
setMethod(
  ".robustDigest",
  signature = "character",
  definition = function(object, .objects, length, algo, quick, classOptions) {
    object <- .removeCacheAtts(object)

    if (!quick) {
        if (any(unlist(lapply(object, file.exists)))) {
          unlist(lapply(object, function(x) {
            if (dir.exists(x)) {
              if (isTRUE(getOption("reproducible.useNewDigestAlgorithm")))
                digest(basename(x), algo = algo)
              else
                fastdigest(basename(x))
            } else if (file.exists(x)) {
                digest(file = x, length = length, algo = algo)
            } else {
              if (isTRUE(getOption("reproducible.useNewDigestAlgorithm")))
                digest(x, algo = algo)
              else
                fastdigest(x)
            }
          }))
        } else {
          if (isTRUE(getOption("reproducible.useNewDigestAlgorithm")))
            digest(object, algo = algo)
          else
            fastdigest(object)
        }
      } else {
        if (isTRUE(getOption("reproducible.useNewDigestAlgorithm")))
          digest(object, algo = algo)
        else
          fastdigest(object)
      }
})

#' @rdname robustDigest
#' @export
setMethod(
  ".robustDigest",
  signature = "Path",
  definition = function(object, .objects, length, algo, quick, classOptions) {
    nParentDirs <- attr(object, "nParentDirs")
    object <- .removeCacheAtts(object)

    if (!quick) {
      lapply(object, function(x) {
        isExistentFile <- FALSE
        if (file.exists(x)) {
          if (!dir.exists(x)) {
            isExistentFile <- TRUE
          }
        }
        if (isExistentFile) {
          digest::digest(file = x, length = length, algo = algo)
        } else {
          # just do file basename as a character string, if file does not exist
          if (isTRUE(getOption("reproducible.useNewDigestAlgorithm")))
            digest(.basenames(x, nParentDirs), algo = algo)
          else
            fastdigest(.basenames(x, nParentDirs))
        }
      })
    } else {
      if (isTRUE(getOption("reproducible.useNewDigestAlgorithm")))
        digest(.basenames(object, nParentDirs), algo = algo)
      else
        fastdigest(.basenames(object, nParentDirs))
    }
})

#' @rdname robustDigest
#' @export
setMethod(
  ".robustDigest",
  signature = "environment",
  definition = function(object, .objects, length, algo, quick, classOptions) {
    object <- .removeCacheAtts(object)
    .robustDigest(as.list(object, all.names = TRUE), .objects = .objects,
                 length = length,
                 algo = algo, quick = quick)
})

#' @rdname robustDigest
#' @export
setMethod(
  ".robustDigest",
  signature = "list",
  definition = function(object, .objects, length, algo, quick, classOptions) {
    object <- .removeCacheAtts(object)
    lapply(.sortDotsUnderscoreFirst(object), function(x) {
      .robustDigest(object = x, .objects = .objects,
                   length = length,
                   algo = algo, quick = quick)
    })
})

#' @rdname robustDigest
#' @export
setMethod(
  ".robustDigest",
  signature = "data.frame",
  definition = function(object, .objects, length, algo, quick, classOptions) {
    #  Need a specific method for data.frame or else it get "list" method, which is wrong
    object <- .removeCacheAtts(object)
    if (isTRUE(getOption("reproducible.useNewDigestAlgorithm")))
      digest(object, algo = algo)
    else
      fastdigest(object)
})

#' @rdname robustDigest
#' @export
setMethod(
  ".robustDigest",
  signature = "Raster",
  definition = function(object, .objects, length, algo, quick, classOptions) {
    object <- .removeCacheAtts(object)

    if (is(object, "RasterStack")) {
      # have to do one file at a time with Stack
      dig <- suppressWarnings(
             lapply(object@layers, function(yy) {
               .digestRasterLayer(yy, length = length, algo = algo, quick = quick)
             })
      )
    } else {
      # Brick and Layers have only one file
      dig <- suppressWarnings(
        .digestRasterLayer(object, length = length, algo = algo, quick = quick))
    }
    return(dig)
})

#' @rdname robustDigest
#' @export
setMethod(
  ".robustDigest",
  signature = "Spatial",
  definition = function(object, .objects, length, algo, quick, classOptions) {
    object <- .removeCacheAtts(object)

  if (is(object, "SpatialPoints")) {
      aaa <- as.data.frame(object)
    } else {
      aaa <- object
    }

    # The following Rounding is necessary to make digest equal on linux and windows
    if (inherits(aaa, "SpatialPolygonsDataFrame")) {
      bbb <- unlist(lapply(as.data.frame(aaa), is.numeric))
      if (sum(bbb)) {
        bbbWh <- which(bbb)
        for (i in bbbWh) { # changed because may not have correct names, can be NAs, Eliot March 2019
                           #  Error was: Error in round(aaa[[i]], 4) :
                           # non-numeric argument to mathematical function
          aaa[[i]] <- round(aaa[[i]], 4)
        }
      }
    }

    #
    if (isTRUE(getOption("reproducible.useNewDigestAlgorithm")))
      digest(aaa, algo = algo)
    else
      fastdigest(aaa)
})

.basenames <- function(object, nParentDirs) {
  if (missing(nParentDirs)) {
    nParentDirs <- 0
  }
  object <- if (nParentDirs > 0) {
    obj <- object
    objOut <- basename(obj)
    for (i in seq_len(nParentDirs)) {
      obj <- dirname(obj)
      objOut <- file.path(basename(obj), objOut)
    }
    objOut
  } else {
    basename(object)
  }
  object
}

#' @importFrom data.table setattr
.removeCacheAtts <- function(x) {
  setattr(x, "tags", NULL)
  setattr(x, ".Cache", NULL)
  setattr(x, "call", NULL)
  x
}

.robustDigestFormatOnly <- function(object, .objects, length, algo, quick,
                               classOptions) {
  object <- .removeCacheAtts(object)
  if (isTRUE(getOption("reproducible.useNewDigestAlgorithm")))
    digest(format(object), algo = algo)
  else
    fastdigest(format(object))
}
