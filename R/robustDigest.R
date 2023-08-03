################################################################################
#' Create reproducible digests of objects in R
#'
#' Not all aspects of R objects are captured by current hashing tools in R
#' (e.g. `digest::digest`, `knitr` caching, `archivist::cache`).
#' This is mostly because many objects have "transient"
#' (e.g., functions have environments), or "disk-backed" features.
#' Since the goal of using reproducibility is to have tools that are not session specific,
#' this function attempts to strip all session specific information so that the digest
#' works between sessions and operating systems.
#' It is tested under many conditions and object types, there are bound to be others that don't
#' work correctly.
#'
#' @section Classes:
#'
#' `Raster*` objects have the potential for disk-backed storage, thus, require more work.
#' Also, because `Raster*` can have a built-in representation for having their data content
#' located on disk, this format will be maintained if the raster already is file-backed,
#' i.e., to create `.tif` or `.grd` backed rasters, use `writeRaster` first,
#' then `Cache`.
#' The \file{.tif} or \file{.grd} will be copied to the \file{raster/} subdirectory of the
#' `cachePath`.
#' Their RAM representation (as an R object) will still be in the usual  \file{cacheOutputs/}
#' (or formerly \file{gallery/}) directory.
#' For `inMemory` raster objects, they will remain as binary `.RData` files.
#'
#' Functions (which are contained within environments) are
#' converted to a text representation via a call to `format(FUN)`.
#'
#' Objects contained within a list or environment are recursively hashed
#' using [digest::digest()], while removing all references to
#' environments.
#'
#' Character strings are first assessed with `dir.exists` and `file.exists`
#' to check for paths. If they are found to be paths, then the path is hashed with
#' only its filename via `basename(filename)`. If it is actually a path, we suggest
#' using `asPath(thePath)`
#'
#' @param object an object to digest.
#'
#' @param objects Optional character vector indicating which objects are to
#'                be considered while making digestible. This argument is not used
#'                in the default cases; the only known method that uses this
#'                in the default cases; the only known method that uses this
#'                argument is the `simList` class from `SpaDES.core`.
#'
#' @inheritParams Cache
#'
#' @return A hash i.e., digest of the object passed in.
#'
#' @author Eliot McIntire
#' @export
#' @importFrom digest digest
#' @keywords internal
#' @rdname robustDigest
#' @examples
#'
#' a <- 2
#' tmpfile1 <- tempfile()
#' tmpfile2 <- tempfile()
#' tmpfile3 <- tempfile(fileext = ".grd")
#' tmpfile4 <- tempfile(fileext = ".grd")
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
#' # SpatRasters are have pointers
#' if (requireNamespace("terra", quietly = TRUE)) {
#'   r <- terra::rast(system.file("ex/elev.tif", package = "terra"))
#'   r3 <- terra::deepcopy(r)
#'   r1 <- terra::writeRaster(r, filename = tmpfile3)
#'
#'   digest::digest(r)
#'   digest::digest(r3) # different but should be same
#'   .robustDigest(r1)
#'   .robustDigest(r3) # same... data & metadata are the same
#'
#'   # note, this is not true for comparing memory and file-backed rasters
#'   .robustDigest(r)
#'   .robustDigest(r1) # different
#' }
#'
setGeneric(".robustDigest", function(object, .objects = NULL,
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
    # browser(expr = exists("._robustDigest_1"))
    if (is(object, "quosure")) { # can't get this class from rlang via importClass rlang quosure
      if (!requireNamespace("rlang")) stop("Please `install.packages('rlang')`")
      object <- rlang::eval_tidy(object)
    }

    if (inherits(object, "Spatial")) {
      object <- .removeCacheAtts(object)
      if (is(object, "SpatialPoints")) {
        forDig <- as.data.frame(object)
      } else {
        forDig <- object
      }
      # The following Rounding is necessary to make digest equal on linux and windows
      if (inherits(forDig, "SpatialPolygonsDataFrame")) {
        bbb <- unlist(lapply(as.data.frame(forDig), is.numeric))
        if (sum(bbb)) {
          bbbWh <- which(bbb)
          for (i in bbbWh) { # changed because may not have correct names, can be NAs, Eliot March 2019
            #  Error was: Error in round(in[[i]], 4) :
            # non-numeric argument to mathematical function
            forDig[[i]] <- round(forDig[[i]], 4)
          }
        }
      }
    } else if (is(object, "Raster")) {
      object <- .removeCacheAtts(object)

      dig <- suppressWarnings(
        .digestRasterLayer(object, length = length, algo = algo, quick = quick)
      )
      forDig <- unlist(dig)
    } else if (is(object, "cluster")) { # can't get this class from parallel via importClass parallel cluster
      forDig <- NULL
    } else if (inherits(object, "SpatRaster")) {
      if (!requireNamespace("terra", quietly = TRUE)) {
        stop("Please install terra package")
      }
      terraSrcs <- Filenames(object)
      if (any(nchar(terraSrcs) > 0)) {
        out <- lapply(terraSrcs, function(x) {
          if (grepl("^NETCDF:", x)) {
            x <- sub("^NETCDF:\"", "", x)
            x <- sub("\":.*$", "", x)
          }
          .robustDigest(object = x, length = length, algo = algo, quick = quick)
        })

        dig <- .robustDigest(
          list(
            terra::nrow(object), terra::ncol(object), terra::nlyr(object),
            terra::res(object), terra::crs(object),
            as.vector(terra::ext(object)), # There is something weird with this pointer that doesn't cache consistently
            names(object)
          ),
          length = length, quick = quick,
          algo = algo, classOptions = classOptions
        ) # don't include object@data -- these are volatile
        forDig <- list(out, dig)
      } else {
        forDig <- terra::wrap(object)
      }
    } else if (inherits(object, "SpatVector")) {
      if (!requireNamespace("terra", quietly = TRUE)) {
        stop("Please install terra package")
      }
      forDig <- wrapSpatVector(object)
    } else if (inherits(object, "SpatExtent")) {
      forDig <- .wrap(object)
    } else {
      forDig <- .removeCacheAtts(object)
    }
    out <- .doDigest(forDig, algo)
    return(out)
  }
)

#' @rdname robustDigest
#' @export
setMethod(
  ".robustDigest",
  signature = "function",
  definition = function(object, .objects, length, algo, quick, classOptions) {
    .robustDigestFormatOnly(object, algo = algo)
  }
)

#' @rdname robustDigest
#' @export
setMethod(
  ".robustDigest",
  signature = "expression",
  definition = function(object, .objects, length, algo, quick, classOptions) {
    .robustDigestFormatOnly(object, algo = algo)
  }
)

#' @rdname robustDigest
#' @export
setMethod(
  ".robustDigest",
  signature = "language",
  definition = function(object, .objects, length, algo, quick, classOptions) {
    .robustDigestFormatOnly(object, algo = algo)
  }
)

#' @rdname robustDigest
#' @export
setMethod(
  ".robustDigest",
  signature = "character",
  definition = function(object, .objects, length, algo, quick, classOptions) {
    object <- .removeCacheAtts(object)

    simpleDigest <- TRUE
    if (!quick) {
      # If a character string has nonASCII characters e.g., from french
      #  "Cordill\xe8re arctique", file.exists fails with "file name conversion problem" error
      howMany <- min(10, NROW(object))
      whCheck <- object[1:howMany]
      asc <- iconv(whCheck, "latin1", "ASCII")
      if (anyNA(asc)) {
        whCheck <- whCheck[!is.na(asc)]
      }

      if (any(unlist(lapply(whCheck, file.exists)))) { # only try first 10 elements
        simpleDigest <- FALSE
      }
    }
    if (!simpleDigest) {
      # browser(expr = exists("hhhh"))
      unlist(lapply(object, function(x) {
        # browser(expr = exists("hhhh"))
        if (dir.exists(x)) {
          .doDigest(basename(x), algo)
        } else if (file.exists(x)) {
          digest(file = x, length = length, algo = algo)
        } else {
          .doDigest(x, algo)
        }
      }))
      # } else {
      # .doDigest(object, algo = algo)
      # }
    } else {
      .doDigest(object, algo = algo)
    }
  }
)

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
          .doDigest(basenames3(x, nParentDirs), algo = algo)
        }
      })
    } else {
      .doDigest(basenames3(object, nParentDirs), algo = algo)
    }
  }
)

#' @rdname robustDigest
#' @export
setMethod(
  ".robustDigest",
  signature = "environment",
  definition = function(object, .objects, length, algo, quick, classOptions) {
    object <- .removeCacheAtts(object)
    if (is.null(classOptions[["prevEnvir"]])) {
      classOptions[["prevEnvir"]] <- list()
      doneAlready <- list(FALSE)
    } else {
      doneAlready <- lapply(classOptions[["prevEnvir"]], function(pe) identical(pe, object))
    }
    classOptions[["prevEnvir"]] <- unique(append(classOptions[["prevEnvir"]], object))

    if (!any(unlist(doneAlready))) {
      asList <- as.list(object, all.names = TRUE)
      da <- which(unlist(doneAlready))
      if (length(da)) {
        asList <- asList[-da]
      }
      rd <- .robustDigest(asList,
        .objects = .objects,
        length = length,
        algo = algo, quick = quick, classOptions = classOptions
      )
    } else {
      rd <- NULL
    }
    return(rd)
  }
)

#' @rdname robustDigest
#' @export
setMethod(
  ".robustDigest",
  signature = "list",
  definition = function(object, .objects, length, algo, quick, classOptions) {
    object <- .removeCacheAtts(object)
    # browser(expr = exists("._robustDigest_2"))
    if (!is.null(.objects)) object <- object[.objects]
    lapply(.sortDotsUnderscoreFirst(object), function(x) {
      .robustDigest(
        object = x, .objects = .objects,
        length = length,
        algo = algo, quick = quick, classOptions = classOptions
      )
    })
  }
)

#' @rdname robustDigest
#' @export
setMethod(
  ".robustDigest",
  signature = "data.frame",
  definition = function(object, .objects, length, algo, quick, classOptions) {
    #  Need a specific method for data.frame or else it get "list" method, which is wrong
    object <- .removeCacheAtts(object)
    dig <- lapply(object, .robustDigest, algo = algo, quick = quick, classOptions = classOptions)
    .robustDigest(unlist(dig), quick = TRUE, algo = algo, classOptions = classOptions)
  }
)


#' @rdname robustDigest
#' @export
setMethod(
  ".robustDigest",
  signature = "numeric",
  definition = function(object, .objects, length, algo, quick, classOptions) {
    #  Need a specific method for data.frame or else it get "list" method, which is wrong
    object <- .removeCacheAtts(object)
    # From ad hoc tests, 6 was the highest I could go to maintain consistent between Linux and Windows
    .doDigest(round(object, getOption("reproducible.digestDigits", 7)), algo = algo)
  }
)

#' @rdname robustDigest
#' @export
setMethod(
  ".robustDigest",
  signature = "matrix",
  definition = function(object, .objects, length, algo, quick, classOptions) {
    #  Need a specific method for data.frame or else it get "list" method, which is wrong
    object <- .removeCacheAtts(object)
    dim(object) <- NULL
    .robustDigest(object, classOptions = classOptions)
    # From ad hoc tests, 6 was the highest I could go to maintain consistent between Linux and Windows
  }
)

#' @rdname robustDigest
#' @export
setMethod(
  ".robustDigest",
  signature = "integer",
  definition = function(object, .objects, length, algo, quick, classOptions) {
    #  Need a specific method for data.frame or else it get "list" method, which is wrong
    object <- .removeCacheAtts(object)
    # From ad hoc tests, 7 was the highest I could go to maintain consistent between Linux and Windows
    .doDigest(object, algo = algo)
  }
)


basenames3 <- function(object, nParentDirs) {
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

#' Remove attributes that are highly varying
#'
#' @param x Any arbitrary R object that could have attributes
.removeCacheAtts <- function(x) {
  if (!is.null(attr(x, "tags"))) {
    attr(x, "tags") <- NULL
  }
  if (!is.null(attr(x, ".Cache"))) {
    attr(x, ".Cache") <- NULL
  }
  if (!is.null(attr(x, "call"))) {
    attr(x, "call") <- NULL
  }
  x
}

.CopyCacheAtts <- function(from, to) {
  onDiskRaster <- FALSE
  namesFrom <- names(from)
  if (!is.null(namesFrom)) { # has to have names
    onDiskRaster <- all(namesFrom %in% c("origRaster", "cacheRaster"))
    isSpatVector <- all(names(from) %in% c("x", "type", "atts", "crs"))

    if ((is(from, "list") || is(from, "environment")) && onDiskRaster %in% FALSE && isSpatVector %in% FALSE) {
      if (length(from) && length(to)) {
        nams <- grep("^\\.mods$|^\\._", namesFrom, value = TRUE, invert = TRUE)
        for (nam in nams) {
          to[[nam]] <- try(.CopyCacheAtts(from[[nam]], to[[nam]]))
        }
      }

      return(to)
    }
  }

  for (i in c("tags", ".Cache", "call")) {
    if (!is.null(attr(from, i))) {
      attr(to, i) <- attr(from, i)
    }
  }
  to
}

.robustDigestFormatOnly <- function(object, .objects, length, algo, quick,
                                    classOptions) {
  object <- .removeCacheAtts(object)
  .doDigest(format(object), algo = algo)
}

.doDigest <- function(x, algo, length = Inf, file,
                      newAlgo = NULL,
                      cacheSpeed = getOption("reproducible.cacheSpeed", "slow")) {
  if (missing(algo)) algo <- formals(.robustDigest)$algo

  out <- if (!missing(file)) {
    digest::digest(file = x, algo = algo, length = length)
  } else {
    if (cacheSpeed == "fast") {
      cacheSpeed <- 2L
    } else if (cacheSpeed == "slow") {
      cacheSpeed <- 1L
    }
    if (!.requireNamespace("fastdigest", stopOnFALSE = FALSE)) {
      cacheSpeed <- 1L
    }

    out <- if (cacheSpeed == 1) {
      digest(x, algo = algo)
    } else if (cacheSpeed == 2) {
      fastdigest::fastdigest(x)
    } else {
      stop("options('reproducible.cacheSpeed') must be 1, 2, 'slow' or 'fast'")
    }
    out
  }
  out
}
