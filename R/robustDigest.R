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
#' \code{Raster*} objects have the potential for disk-backed storage. If
#' the object in the R session is cached using \code{archivist::cache}, only
#' the header component will be assessed for caching. Thus, objects like this
#' require more work. Also, because \code{Raster*} can have a built-in representation
#' for having their data content located on disk, this format will be maintained
#' if the raster already is file-backed, i.e., to create .tif or .grd backed rasters,
#' use writeRaster first, then Cache. The .tif or .grd will be copied to the "raster"
#' subdirectory of the \code{cacheRepo}.
#' Their RAM representation (as an R object) will still be in the usual "gallery" directory.
#' For \code{inMemory} raster objects, they will remain as binary .rdata files.
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
#' only its filename via \code{basename(filename)}.
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
#' @importFrom digest digest
#' @importFrom fastdigest fastdigest
#' @docType methods
#' @keywords internal
#' @rdname robustDigest
#' @author Eliot McIntire
#' @export
setGeneric("robustDigest", function(object, objects,
                                    compareRasterFileLength = 1e6,
                                    algo = "xxhash64",
                                    digestPathContent = FALSE,
                                    classOptions = list()) {
  standardGeneric("robustDigest")
})


#' @rdname robustDigest
#' @exportMethod robustDigest
setMethod(
  "robustDigest",
  signature = "ANY",
  definition = function(object, compareRasterFileLength, algo, digestPathContent,
                        classOptions) {
    fastdigest(object)
  })

#' @import parallel
setOldClass("cluster")

#' @rdname robustDigest
#' @exportMethod robustDigest
setMethod(
  "robustDigest",
  signature = "cluster",
  definition = function(object, compareRasterFileLength, algo, digestPathContent,
                        classOptions) {
    fastdigest(NULL)
  })

#' @rdname robustDigest
#' @exportMethod robustDigest
setMethod(
  "robustDigest",
  signature = "function",
  definition = function(object, compareRasterFileLength, algo, digestPathContent,
                        classOptions) {
    fastdigest(format(object))
  })

#' @rdname robustDigest
#' @exportMethod robustDigest
setMethod(
  "robustDigest",
  signature = "expression",
  definition = function(object, compareRasterFileLength, algo, digestPathContent,
                        classOptions) {
    fastdigest(format(object))
  })

#' @rdname robustDigest
#' @exportMethod robustDigest
setMethod(
  "robustDigest",
  signature = "character",
  definition = function(object, compareRasterFileLength, algo, digestPathContent,
                        classOptions) {
    if (any(unlist(lapply(object, dir.exists)))) {
      unlist(lapply(object, function(x) {
        if (dir.exists(x)) {
          fastdigest::fastdigest(basename(x))
        } else {
          fastdigest::fastdigest(x)
        }
      }))
    } else if (any(unlist(lapply(object, file.exists)))) {
      unlist(lapply(object, function(x) {
        if (file.exists(x)) {
          digest::digest(file = x, length = compareRasterFileLength, algo = algo)
        } else {
          fastdigest::fastdigest(x)
        }
      }))
    } else {
      fastdigest::fastdigest(object)
    }
  })

#' @rdname robustDigest
#' @exportMethod robustDigest
setMethod(
  "robustDigest",
  signature = "Path",
  definition = function(object, compareRasterFileLength, algo, digestPathContent,
                        classOptions) {
    if (digestPathContent) {
      lapply(object, function(x) {
        if (file.exists(x)) {
          digest::digest(file = x, length = compareRasterFileLength, algo = algo)
        } else {
          fastdigest::fastdigest(basename(x))
        }
      })
    } else {
      fastdigest::fastdigest(basename(object))
    }
  })

#' @rdname robustDigest
#' @exportMethod robustDigest
setMethod(
  "robustDigest",
  signature = "environment",
  definition = function(object, compareRasterFileLength, algo, digestPathContent,
                        classOptions) {
    robustDigest(as.list(object, all.names = TRUE),
                 compareRasterFileLength = compareRasterFileLength,
                 algo = algo, digestPathContent = digestPathContent)
  })

#' @rdname robustDigest
#' @exportMethod robustDigest
setMethod(
  "robustDigest",
  signature = "list",
  definition = function(object, compareRasterFileLength, algo, digestPathContent,
                        classOptions) {
    lapply(sortDotsUnderscoreFirst(object), function(x) {
      robustDigest(object = x,
                   compareRasterFileLength = compareRasterFileLength,
                   algo = algo, digestPathContent = digestPathContent)
    })
  })

#' @rdname robustDigest
#' @exportMethod robustDigest
setMethod(
  "robustDigest",
  signature = "Raster",
  definition = function(object, compareRasterFileLength, algo, digestPathContent,
                        classOptions) {
    if (is(object, "RasterStack") | is(object, "RasterBrick")) {
      dig <- suppressWarnings(
        list(dim(object), res(object), crs(object), extent(object),
             lapply(object@layers, function(yy) {
               digestRaster(yy, compareRasterFileLength, algo)
             })
        )
      )
      if (nzchar(object@filename, keepNA = TRUE)) {
        # if the Raster is on disk, has the first compareRasterFileLength characters;
        # uses digest::digest on the file
        dig <- append(dig, digest(file = object@filename, length = compareRasterFileLength))
      }
    } else {
      dig <- suppressWarnings(digestRaster(object, compareRasterFileLength, algo))
    }
    return(fastdigest::fastdigest(dig))
  })

#' @rdname robustDigest
#' @exportMethod robustDigest
setMethod(
  "robustDigest",
  signature = "Spatial",
  definition = function(object, compareRasterFileLength, algo, digestPathContent,
                        classOptions) {
    if (is(object, "SpatialPoints")) {
      aaa <- as.data.frame(object)
    } else {
      aaa <- object
    }
    # The following Rounding is necessary to make digest equal on linux and windows
    for (i in names(aaa)) {
      if (!is.integer(aaa[, i])) {
        if (is.numeric(aaa[, i]))
          aaa[, i] <- round(aaa[, i], 4)
      }
    }

    return(fastdigest::fastdigest(aaa))
  })
