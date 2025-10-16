#' Change the absolute path of a file
#'
#' `convertPaths` is simply a wrapper around `gsub` for changing the
#' first part of a path.
#' `convertRasterPaths` is useful for changing the path to a file-backed
#' raster (e.g., after copying the file to a new location).
#'
#' @param x             For `convertPaths`, a character vector of file paths.
#'                      For `convertRasterPaths`, a disk-backed `RasterLayer`
#'                      object, or a list of such rasters.
#' @param patterns      Character vector containing a pattern to match (see `?gsub`).
#' @param replacements  Character vector of the same length of `patterns`
#'                      containing replacement text (see `?gsub`).
#'
#' @author Eliot McIntire and Alex Chubaty
#' @export
#' @rdname convertPaths
#' @return
#' A normalized path with the `patterns` replaced by `replacements`. Or a list of such
#' objects if `x` was a list.
#'
#' @examples
#' filenames <- c("/home/user1/Documents/file.txt", "/Users/user1/Documents/file.txt")
#' oldPaths <- dirname(filenames)
#' newPaths <- c("/home/user2/Desktop", "/Users/user2/Desktop")
#' convertPaths(filenames, oldPaths, newPaths)
#'
convertPaths <- function(x, patterns, replacements) {
  stopifnot(is(x, "character"))
  stopifnot(length(patterns) == length(replacements))
  patterns <- normPath(patterns)
  replacements <- normPath(replacements)
  x <- normPath(x)
  for (i in seq_along(patterns)) {
    x <- gsub(x = x, pattern = patterns[i], replacement = replacements[i])
  }
  normPath(x)
}

#' @author Eliot McIntire and Alex Chubaty
#' @export
#' @rdname convertPaths
convertRasterPaths <- function(x, patterns, replacements) {
  .requireNamespace("raster", stopOnFALSE = TRUE)

  if (is.list(x)) {
    x <- lapply(x, convertRasterPaths, patterns, replacements)
  } else if (!is.null(x)) {
    if (is.character(x)) {
      if (length(x) > 1) {
        x <- lapply(x, convertRasterPaths, patterns, replacements)
      } else {
        x <- raster::raster(x)
      }
    }

    x@file@name <- convertPaths(raster::filename(x), patterns, replacements)
  }
  x # handles null case
}

#' Return the filename(s) from a `Raster*` object
#'
#' This is mostly just a wrapper around `filename` from the `raster` package, except that
#' instead of returning an empty string for a `RasterStack` object, it will return a vector of
#' length >1 for `RasterStack`.
#'
#' @param obj A `Raster*` object (i.e., `RasterLayer`, `RasterStack`, `RasterBrick`)
#' @param allowMultiple Logical. If `TRUE`, the default, then all relevant
#'   filenames will be returned, i.e., in cases such as `.grd` where multiple files
#'   are required. If `FALSE`, then only the first file will be returned,
#'   e.g., `filename.grd`, in the case of default Raster format in R.
#' @param returnList Default `FALSE`. If `FALSE`, then return format will be a
#' character vector. When `TRUE`, list or environment objects will return a list
#' of character strings or vectors. When returned as a character vector, then
#' the names of objects with >1 filename associated with them will be given a numeric
#' suffix, which means the name in the returned vector does not match the object in
#' the list or environment. When returned as a list, their names are preserved.
#'
#' @author Eliot McIntire
#' @details
#' New methods can be made for this generic.
#'
#' @return
#' A character vector of filenames that are part of the objects passed to `obj`.
#' This returns `NULL` is the object is not file-backed or does not have a method
#' to recover the file-backed filename.
#' @export
#' @rdname Filenames
setGeneric("Filenames", function(obj, allowMultiple = TRUE, returnList = FALSE) {
  standardGeneric("Filenames")
})

#' @export
#' @rdname Filenames
setMethod(
  "Filenames",
  signature = "ANY",
  definition = function(obj, allowMultiple, returnList = FALSE) {
    if (inherits(obj, "RasterStack")) {
      fns <- unlist(lapply(seq_along(names(obj)), function(index) {
        Filenames(obj[[index]], allowMultiple = allowMultiple, returnList = returnList)
      }))

      dups <- duplicated(fns)
      if (any(dups)) {
        theNames <- names(fns)
        fns <- fns[!dups]
        names(fns) <- theNames[!dups]
      }
    } else if (inherits(obj, "RasterLayer")) {
      fns <- raster::filename(obj)
      if (exists("._Filenames_1")) browser()
      if (length(fns) == 0 || all(nchar(fns) == 0)) {
        fns <- NULL
      }
      fns <- allowMultipleFNs(allowMultiple, fns)
    } else if (inherits(obj, "SpatRaster")) {
      if (!requireNamespace("terra", quietly = TRUE)) {
        stop("Please install terra package")
      }
      fns <- terra::sources(obj)
      if (any(fileExt(fns) %in% "vrt"))
        fns <- c(fns, terra::vrt_tiles(obj))

      fns <- allowMultipleFNs(allowMultiple, fns)
    } else {
      fns <- NULL
    }
    ## DON"T BE TEMPTED TO RM NZ i.e., MEMORY LAYERS -- NEED TO KNOW THAT THERE WERE SOME
    normPath(fns)
  }
)

#' @export
#' @rdname Filenames
setMethod(
  "Filenames",
  signature = "environment",
  definition = function(obj, allowMultiple = TRUE, returnList = FALSE) {
    rasterFilename <- Filenames(as.list(obj), allowMultiple = allowMultiple,
                                returnList = returnList)
    rasterFilenameDups <- lapply(rasterFilename, duplicated)

    if (any(unlist(rasterFilenameDups))) {
      rasterFilename <- rasterFilename[!unlist(rasterFilenameDups)]
    }
    return(rasterFilename)
  }
)

#' @export
#' @rdname Filenames
setMethod(
  "Filenames",
  signature = "list",
  definition = function(obj, allowMultiple = TRUE, returnList = FALSE) {
    ## convert a list to an environment -- this is to align it with a simList and environment
    if (is.null(names(obj))) {
      names(obj) <- as.character(seq(obj))
    }
    ll <- lapply(obj, function(o) Filenames(o, allowMultiple = allowMultiple,
                                            returnList = returnList))
    if (!isTRUE(returnList))
      ll <- unlist(ll)
    return(ll)
    # Filenames(as.environment(obj), allowMultiple = allowMultiple)
  }
)

#' @export
#' @rdname Filenames
setMethod(
  "Filenames",
  signature = "data.table",
  definition = function(obj, allowMultiple = TRUE, returnList = FALSE) {
    isCacheDB <- all(c(.cacheTableHashColName(), .cacheTableTagColName()) %in% colnames(obj))
    fromDsk <- NULL
    if (isCacheDB) {
      isFromDsk <- extractFromCache(obj, elem = "fromDisk") %in% "TRUE"
      if (any(isFromDsk)) {
        fromDsk <- extractFromCache(obj, elem = "origFilename")
      }
    }
    fromDsk
  }
)

#' @export
#' @rdname Filenames
setMethod(
  "Filenames",
  signature = "Path",
  definition = function(obj, allowMultiple = TRUE, returnList = FALSE) {
    obj <- allowMultipleFNs(allowMultiple, obj)
    # tags <- attr(obj, "tags")
    # if (!is.null(tags)) {
    #   tags1 <- parseTags(tags)
    #   if (allowMultiple)
    #   out <- tags1$tagValue[tags1$tagKey == "whichFiles"]
    # } else {
    #   out <- obj
    # }
    obj
  }
)


allowMultipleFNs <- function(allowMultiple, fns) {
  if (!is.null(fns)) {
    if (isTRUE(allowMultiple)) {
      anyGrd <- endsWith(fns, suffix = "grd")
      anyGri <- endsWith(fns, suffix = "gri")
      if (any(anyGrd) && !any(anyGri)) {
        nonGrd <- if (any(!anyGrd)) fns[!anyGrd] else NULL
        multiFns <- sort(c(fns[anyGrd], gsub("grd$", "gri", fns[anyGrd])))
        fnsNew <- c(nonGrd, multiFns)
        fns <- fnsNew[order(match(
          filePathSansExt(basename(fnsNew)),
          filePathSansExt(basename(fns))
        ))]
      }
    } else {
      fns <- grep("\\.gri$", fns, value = TRUE, invert = TRUE)
    }
  }
  fns

}
