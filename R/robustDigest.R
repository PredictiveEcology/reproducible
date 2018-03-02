################################################################################
#' Create reproducible digests of objects in R
#'
#' Not all aspects of R objects are captured by current hashing tools in R (e.g.
#' \code{digest::digest}, \code{fastdigest::fastdigest}, \code{knitr} caching,
#' \code{archivist::cache}). This is mostly because many objects have "transient"
#' (e.g., functions have environments), or "disk-backed" features. This function
#' allows for these accommodations to be made and uses \code{\link[fastdigest]{fastdigest}}
#' internally.  Since
#' the goal of using reproducibility is to have tools that are not session specific,
#' this function
#' attempts to strip all session specific information so that the fastdigest
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
#' using \code{\link[fastdigest]{fastdigest}}, while removing all references to
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
#'                be considered while making digestible. This is only relevant
#'                if the object being passed is an environment or list or the like.
#' @inheritParams Cache
#'
#' @return A hash i.e., digest of the object passed in.
#'
#' @seealso \code{\link[archivist]{cache}}.
#' @seealso \code{\link[fastdigest]{fastdigest}}.
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
#' fastdigest::fastdigest(tmpfile1)
#' fastdigest::fastdigest(tmpfile2)
#'
#' # tests to see whether character string is representing a file
#' .robustDigest(tmpfile1)
#' .robustDigest(tmpfile2) # same
#'
#' # if you tell it that it is a path, then you can decide if you want it to be
#' #  treated as a character string or as a file path
#' .robustDigest(asPath(tmpfile1))
#' .robustDigest(asPath(tmpfile2)) # different because you have a choice
#'
#' .robustDigest(asPath(tmpfile1), digestPathContent = TRUE)
#' .robustDigest(asPath(tmpfile2), digestPathContent = TRUE) # same
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
#' fastdigest::fastdigest(r1)
#' fastdigest::fastdigest(r2) # different
#' .robustDigest(r1)
#' .robustDigest(r2) # same... data are the same in the file
#'
#' # note, this is not true for comparing memory and file-backed rasters
#' .robustDigest(r)
#' .robustDigest(r1) # different
#'
setGeneric(".robustDigest", function(object, objects,
                                    compareRasterFileLength = 1e6,
                                    algo = "xxhash64",
                                    digestPathContent = !getOption("reproducible.quick"),
                                    classOptions = list()) {
  standardGeneric(".robustDigest")
})

#' @rdname robustDigest
#' @exportMethod .robustDigest
setMethod(
  ".robustDigest",
  signature = "ANY",
  definition = function(object, compareRasterFileLength, algo, digestPathContent,
                        classOptions) {
    fastdigest(object)
})

#' @import parallel
setOldClass("cluster")

#' @rdname robustDigest
#' @exportMethod .robustDigest
setMethod(
  ".robustDigest",
  signature = "cluster",
  definition = function(object, compareRasterFileLength, algo, digestPathContent,
                        classOptions) {
    fastdigest(NULL)
})

#' @rdname robustDigest
#' @exportMethod .robustDigest
setMethod(
  ".robustDigest",
  signature = "function",
  definition = function(object, compareRasterFileLength, algo, digestPathContent,
                        classOptions) {
    fastdigest(format(object))
})

#' @rdname robustDigest
#' @exportMethod .robustDigest
setMethod(
  ".robustDigest",
  signature = "expression",
  definition = function(object, compareRasterFileLength, algo, digestPathContent,
                        classOptions) {
    fastdigest(format(object))
})

#' @rdname robustDigest
#' @exportMethod .robustDigest
setMethod(
  ".robustDigest",
  signature = "character",
  definition = function(object, compareRasterFileLength, algo, digestPathContent,
                        classOptions) {
    if (digestPathContent) {
      if (any(unlist(lapply(object, dir.exists)))) {
        unlist(lapply(object, function(x) {
          if (dir.exists(x)) {
            fastdigest(basename(x))
          } else {
            fastdigest(x)
          }
        }))
      } else if (any(unlist(lapply(object, file.exists)))) {
        unlist(lapply(object, function(x) {
          if (file.exists(x)) {
            digest::digest(file = x, length = compareRasterFileLength, algo = algo)
          } else {
            fastdigest(x)
          }
        }))
      } else {
        fastdigest(object)
      }
    } else {
      fastdigest(object)
    }
})

#' @rdname robustDigest
#' @exportMethod .robustDigest
setMethod(
  ".robustDigest",
  signature = "Path",
  definition = function(object, compareRasterFileLength, algo, digestPathContent,
                        classOptions) {
    if (digestPathContent) {
      lapply(object, function(x) {
        isExistentFile <- FALSE
        if (file.exists(x)) {
          if (!dir.exists(x)) {
            isExistentFile <- TRUE
          }
        }
        if (isExistentFile) {
          digest::digest(file = x, length = compareRasterFileLength, algo = algo)
        } else {
          # just do file basename as a character string, if file does not exist
          fastdigest(basename(x))
        }
      })
    } else {
      fastdigest(basename(object))
    }
})

#' @rdname robustDigest
#' @exportMethod .robustDigest
setMethod(
  ".robustDigest",
  signature = "environment",
  definition = function(object, compareRasterFileLength, algo, digestPathContent,
                        classOptions) {
    .robustDigest(as.list(object, all.names = TRUE),
                 compareRasterFileLength = compareRasterFileLength,
                 algo = algo, digestPathContent = digestPathContent)
})

#' @rdname robustDigest
#' @exportMethod .robustDigest
setMethod(
  ".robustDigest",
  signature = "list",
  definition = function(object, compareRasterFileLength, algo, digestPathContent,
                        classOptions) {
    lapply(.sortDotsUnderscoreFirst(object), function(x) {
      .robustDigest(object = x,
                   compareRasterFileLength = compareRasterFileLength,
                   algo = algo, digestPathContent = digestPathContent)
    })
})

#' @rdname robustDigest
#' @exportMethod .robustDigest
setMethod(
  ".robustDigest",
  signature = "Raster",
  definition = function(object, compareRasterFileLength, algo, digestPathContent,
                        classOptions) {
    if (digestPathContent) {
      if (is(object, "RasterStack")) {
        # have to do one file at a time with Stack
        dig <- suppressWarnings(
               lapply(object@layers, function(yy) {
                 .digestRaster(yy, compareRasterFileLength, algo)
               })
        )
      } else {
        # Brick and Layers have only one file
        dig <- suppressWarnings(.digestRaster(object, compareRasterFileLength, algo))
      }
    } else {
      dig <- object
    }
    return(fastdigest(dig))
})

#' @rdname robustDigest
#' @exportMethod .robustDigest
setMethod(
  ".robustDigest",
  signature = "Spatial",
  definition = function(object, compareRasterFileLength, algo, digestPathContent,
                        classOptions) {
    if (is(object, "SpatialPoints")) {
      aaa <- as.data.frame(object)
    } else {
      aaa <- object
    }

    # The following Rounding is necessary to make digest equal on linux and windows
    if (inherits(aaa, "SpatialPolygonsDataFrame")) {
      bbb <- unlist(lapply(as.data.frame(aaa), is.numeric))
      if (sum(bbb)) {
        for (i in names(aaa)[bbb]) {
          aaa[[i]] <- round(aaa[[i]], 4)
        }
      }
    }

    return(fastdigest(aaa))
  })
