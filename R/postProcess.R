#' Generic function to post process objects
#'
#' \if{html}{\figure{lifecycle-maturing.svg}{options: alt="maturing"}}
#'
#' @export
#' @param x  An object of postProcessing, e.g., `spatialClasses`.
#'           See individual methods. This can be provided as a
#'           `rlang::quosure` or a normal R object.
#' @importFrom utils capture.output
#' @importFrom sp spTransform
#' @seealso `prepInputs`
#' @inheritParams prepInputs
#' @rdname postProcess
postProcess <- function(x, ...) {
  UseMethod("postProcess")
}

#' @export
#' @rdname postProcess
postProcess.default <- function(x, ...) {
  x
}

#' @importFrom rlang eval_tidy
postProcess.quosure <- function(x, ...) {
  # browser(expr = exists("._postProcess.quosure_1"))
  postProcess(eval_tidy(x), ...)
}

#' @export
#' @rdname postProcess
postProcess.list <- function(x, ...) {
  lapply(x, function(y) postProcess(y, ...))
}

#' Post processing for `spatialClasses`
#'
#' The method for `spatialClasses` (`Raster*` and `Spatial*`) will
#' crop, reproject, and mask, in that order.
#' This is a wrapper for [cropInputs()], [fixErrors()],
#' [projectInputs()], [maskInputs()] and [writeOutputs()],
#' with a decent amount of data manipulation between these calls so that the crs match.
#'
#' @section Post processing sequence:
#'
#'   If the `rasterToMatch` or `studyArea` are passed, then
#'   the following sequence will occur:
#'
#'   \enumerate{
#'     \item Fix errors [fixErrors()]. Currently only errors fixed are for
#'            `SpatialPolygons` using `buffer(..., width = 0)`.
#'     \item Crop using [cropInputs()]
#'     \item Project using [projectInputs()]
#'     \item Mask using [maskInputs()]
#'     \item Determine file name [determineFilename()]
#'     \item Write that file name to disk, optionally [writeOutputs()]
#'   }
#'
#'   NOTE: checksumming does not occur during the post-processing stage, as
#'   there are no file downloads. To achieve fast results, wrap
#'   `prepInputs` with `Cache`
#'
#'   NOTE: `sf` objects are still very experimental.
#'
#' @inheritParams prepInputs
#'
#' @inheritParams cropInputs
#'
#' @param filename1  Character strings giving the file paths of
#'                   the *input* object (`filename1`) `filename1`
#'                   is only used for messaging (i.e., the object itself is passed
#'                   in as `x`) and possibly naming of output (see details
#'                   and `filename2`).
#'
#' @param filename2   `filename2` is optional, and is either
#'                   NULL (no writing of outputs to disk), or several options
#'                   for writing the object to disk. If
#'                   `TRUE` (the default), it will give it a file name determined by
#'                   `.prefix(basename(filename1), prefix)`. If
#'                   a character string, it will use this as its file name. See
#'                   [determineFilename()].
#'
#' @param useSAcrs Logical. If `FALSE`, the default, then the desired projection
#'                 will be taken from `rasterToMatch` or none at all.
#'                 If `TRUE`, it will be taken from `studyArea`. See table
#'                 in details below.
#'
#' @param ... Additional arguments passed to methods. For `spatialClasses`,
#'            these are: [cropInputs()], [fixErrors()],
#'            [projectInputs()], [maskInputs()],
#'            [determineFilename()], and [writeOutputs()].
#'            Each of these may also pass `...` into other functions, like
#'            [raster::writeRaster()], or `sf::st_write`.
#'            This might include potentially important arguments like `datatype`,
#'            `format`. Also passed to `projectRaster`,
#'            with likely important arguments such as `method = "bilinear"`.
#'            See details.
#'
#' \subsection{... passed to:}{
#'   \describe{
#'     \item{`cropInputs`:}{[raster::crop()]}
#'     \item{`projectInputs`}{[raster::projectRaster()]}
#'     \item{`maskInputs`}{[fastMask()] or [raster::intersect()]}
#'     \item{`fixErrors`}{[raster::buffer()]}
#'     \item{`writeOutputs`}{[raster::writeRaster()] or [raster::shapefile()]}
#'     \item{`determineFilename`}{}
#'   }
#'   * Can be overridden with `useSAcrs`
#'   ** Will mask with `NA`s from `rasterToMatch` if `maskWithRTM`
#' }
#'
#' @section Passing `rasterToMatch` and/or `studyArea`:
#'
#' Depending on which of these were passed, different things will happen to the
#' `targetFile` located at `filename1`.
#'
#' \subsection{If `targetFile` is a `Raster*` object:}{
#'   \tabular{lccc}{
#'                       \tab `rasterToMatch` \tab `studyArea` \tab             Both \cr
#'     `extent`     \tab Yes                  \tab   Yes        \tab `rasterToMatch` \cr
#'     `resolution` \tab Yes                  \tab   No         \tab `rasterToMatch` \cr
#'     `projection` \tab Yes                  \tab   No*        \tab `rasterToMatch`*\cr
#'     `alignment`  \tab Yes                  \tab   No         \tab `rasterToMatch` \cr
#'     `mask`       \tab No**                 \tab   Yes        \tab `studyArea`**   \cr
#'   }
#'   * Can be overridden with `useSAcrs`.
#'   ** Will mask with `NA`s from `rasterToMatch` if `maskWithRTM`.
#' }
#'
#' \subsection{If `targetFile` is a `Spatial*` object:}{
#'   \tabular{lccc}{
#'                       \tab `rasterToMatch` \tab `studyArea` \tab             Both \cr
#'     `extent`     \tab Yes                  \tab   Yes        \tab `rasterToMatch` \cr
#'     `resolution` \tab NA                   \tab   NA         \tab NA                   \cr
#'     `projection` \tab Yes                  \tab   No*        \tab `rasterToMatch`*\cr
#'     `alignment`  \tab NA                   \tab   NA         \tab NA                   \cr
#'     `mask`       \tab No                   \tab   Yes        \tab `studyArea`     \cr
#'   }
#'   * Can be overridden with `useSAcrs`
#' }
#'
#' @export
#' @example inst/examples/example_postProcess.R
#' @rdname postProcess
#' @return A GIS file (e.g., `RasterLayer`, `SpatRaster` etc.) that has been
#' appropriately cropped, reprojected, masked, depending on the inputs.
#'
postProcess.spatialClasses <- function(x, filename1 = NULL, filename2 = NULL,
                                       studyArea = NULL, rasterToMatch = NULL,
                                       overwrite = getOption("reproducible.overwrite", TRUE),
                                       useSAcrs = NULL,
                                       useCache = getOption("reproducible.useCache", FALSE),
                                       verbose = getOption("reproducible.verbose", 1),
                                       ...) {

  # Test if user supplied wrong type of file for "studyArea", "rasterToMatch"
  # browser(expr = exists("._postProcess.spatialClasses_1"))
  if (isTRUE(getOption("reproducible.useTerra"))) {
    if (isFALSE(useSAcrs)) useSAcrs <- NULL
    x1 <- postProcessTerra(from = x, studyArea = studyArea,
                           rasterToMatch = rasterToMatch, useCache = useCache,
                           filename1 = filename1, filename2 = filename2,
                           useSAcrs = useSAcrs, overwrite = overwrite,
                           verbose = verbose, ...)
  } else {

    on.exit(raster::removeTmpFiles(h = 0), add = TRUE)

    x1 <- postProcessAllSpatial(x = x, studyArea = eval_tidy(studyArea),
                                rasterToMatch = eval_tidy(rasterToMatch), useCache = useCache,
                                filename1 = filename1, filename2 = filename2,
                                useSAcrs = useSAcrs, overwrite = overwrite,
                                verbose = verbose, ...)
  }

  return(x1)
}

#' @export
postProcess.SpatRaster <- function(x, filename1 = NULL, filename2 = NULL,
                                   studyArea = NULL, rasterToMatch = NULL,
                                   overwrite = getOption("reproducible.overwrite", TRUE),
                                   useSAcrs = NULL,
                                   useCache = getOption("reproducible.useCache", FALSE),
                                   verbose = getOption("reproducible.verbose", 1),
                                   ...) {

  on.exit(removeTmpFiles(h = 0), add = TRUE)

  # Test if user supplied wrong type of file for "studyArea", "rasterToMatch"
  # browser(expr = exists("._postProcess.spatialClasses_1"))
  if (isFALSE(useSAcrs)) useSAcrs <- NULL

  x1 <- postProcessTerra(from = x, studyArea = studyArea,
                         rasterToMatch = rasterToMatch, useCache = useCache,
                         filename1 = filename1, filename2 = filename2,
                         useSAcrs = useSAcrs, overwrite = overwrite,
                         verbose = verbose, ...)
  return(x1)
}

#' @export
postProcess.SpatVector <- function(x, filename1 = NULL, filename2 = NULL,
                                   studyArea = NULL, rasterToMatch = NULL,
                                   overwrite = getOption("reproducible.overwrite", TRUE),
                                   useSAcrs = NULL,
                                   useCache = getOption("reproducible.useCache", FALSE),
                                   verbose = getOption("reproducible.verbose", 1),
                                   ...) {

  on.exit(removeTmpFiles(h = 0), add = TRUE)

  # Test if user supplied wrong type of file for "studyArea", "rasterToMatch"
  # browser(expr = exists("._postProcess.spatialClasses_1"))
  if (isFALSE(useSAcrs)) useSAcrs <- NULL

  x1 <- postProcessTerra(from = x, studyArea = studyArea,
                         rasterToMatch = rasterToMatch, useCache = useCache,
                         filename1 = filename1, filename2 = filename2,
                         useSAcrs = useSAcrs, overwrite = overwrite,
                         verbose = verbose, ...)
  return(x1)
}

#' @export
#' @rdname postProcess
postProcess.sf <- function(x, filename1 = NULL, filename2 = NULL,
                           studyArea = NULL, rasterToMatch = NULL,
                           overwrite = getOption("reproducible.overwrite", TRUE),
                           useSAcrs = NULL,
                           useCache = getOption("reproducible.useCache", FALSE),
                           verbose = getOption("reproducible.verbose", 1),
                           ...) {
  .requireNamespace("sf", stopOnFALSE = TRUE)

  if (isTRUE(getOption("reproducible.useTerra"))) {
    if (isFALSE(useSAcrs)) useSAcrs <- NULL
    x <- postProcessTerra(from = x, studyArea = studyArea,
                           rasterToMatch = rasterToMatch, useCache = useCache,
                           filename1 = filename1, filename2 = filename2,
                           useSAcrs = useSAcrs, overwrite = overwrite,
                           verbose = verbose, ...)
  } else {
    if (!requireNamespace("raster"))
      stop("raster package needs installing; install.packages('raster')")

    # Test if user supplied wrong type of file for "studyArea", "rasterToMatch"
    messagePrepInputs("postProcess with sf class objects is still experimental")
    if (!is.null(rasterToMatch)) {
      if (is.null(studyArea))
        stop("sf class objects are not yet working with rasterToMatch argument")
      messagePrepInputs("sf class objects can not be postProcessed directly from rasterToMatch yet;",
                        "using studyArea. ")
      rasterToMatch <- NULL
    }
    if (is(studyArea, "Spatial")) {
      studyArea <- sf::st_as_sf(studyArea)
    }

    x <- postProcessAllSpatial(x = x, studyArea = studyArea,
                               rasterToMatch = rasterToMatch, useCache = useCache,
                               filename1 = filename1, filename2 = filename2,
                               useSAcrs = useSAcrs, overwrite = overwrite, verbose = verbose, ...)
  }
  return(x)
}

#' Crop a `Spatial*` or `Raster*` object
#'
#' This function is deprecated. Use `cropTo`. If used, all arguments will be passed
#' to `cropTo` anyway.
#'
#' @param x A `Spatial*`, `sf`, or `Raster*` object.
#'
#' @param studyArea `SpatialPolygons*` object used for masking and possibly cropping
#'                  if no `rasterToMatch` is provided.
#'                  If not in same CRS, then it will be `spTransform`ed to
#'                  CRS of `x` before masking. Currently, this function will not reproject the
#'                  `x`. Optional in `postProcess`.
#'
#' @param rasterToMatch Template `Raster*` object used for cropping (so extent should be
#'                      the extent of desired outcome) and reprojecting (including changing the
#'                      resolution and projection).
#'                      See details in [postProcess()].
#'
#' @param ... Passed to `raster::crop`
#'
#' @param useCache Logical, default `getOption("reproducible.useCache", FALSE)`, whether
#'                 `Cache` is used internally.
#'
#' @inheritParams projectInputs
#'
#' @author Eliot McIntire, Jean Marchal, Ian Eddy, and Tati Micheletti
#' @export
#' @importFrom methods is
#' @importFrom sp SpatialPolygonsDataFrame spTransform CRS proj4string
#' @return A GIS file (e.g., RasterLayer, SpatRaster etc.) that has been
#' appropriately cropped.
#' @rdname deprecated
#' @examples
#' library(sp)
#' library(raster)
#'
#' # make a SpatialPolygon
#' coords1 <- structure(c(-123.98, -117.1, -80.2, -100, -123.98, 60.9, 67.73, 65.58, 51.79, 60.9),
#'                        .Dim = c(5L, 2L))
#' Sr1 <- Polygon(coords1)
#' Srs1 <- Polygons(list(Sr1), "s1")
#' shpEcozone <- SpatialPolygons(list(Srs1), 1L)
#' crs(shpEcozone) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
#'
#' # make a "study area" that is subset of larger dataset
#' coords <- structure(c(-118.98, -116.1, -99.2, -106, -118.98, 59.9, 65.73, 63.58, 54.79, 59.9),
#'                       .Dim = c(5L, 2L))
#' Sr1 <- Polygon(coords)
#' Srs1 <- Polygons(list(Sr1), "s1")
#' StudyArea <- SpatialPolygons(list(Srs1), 1L)
#' crs(StudyArea) <- crs(shpEcozone)
#' projString <- "+proj=utm +zone=15 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
#' StudyArea <- sp::spTransform(StudyArea, CRSobj = projString)
#' cropInputs(shpEcozone, StudyArea)
cropInputs <- function(x, studyArea, rasterToMatch, verbose = getOption("reproducible.verbose", 1), ...) {
  UseMethod("cropInputs")
}

#' @export
#' @rdname deprecated
cropInputs.default <- function(x, ...) {
  cropTo(x, ...)
}

# #' @export
# cropInputs.SpatVector <- function(x, studyArea = NULL, rasterToMatch = NULL,
#                                   verbose = getOption("reproducible.verbose", 1),
#                                   extentToMatch = NULL, extentCRS = NULL,
#                                   useGDAL = getOption("reproducible.useGDAL", FALSE),
#                                   useCache = getOption("reproducible.useCache", FALSE),
#                                   ...) {
#   if (!is.null(studyArea) || !is.null(rasterToMatch) || !is.null(extentToMatch)) {
#     isX_Sp <- is(x, "Spatial")
#     isX_Sf <- is(x, "sf")
#     if (!is.null(extentToMatch)) {
#       rasterToMatch <- suppressWarningsSpecific(falseWarnings = "CRS object has comment",
#                                                 raster(extentToMatch, crs = extentCRS))
#     }
#     cropTo <- if (!is.null(rasterToMatch)) {
#       rasterToMatch
#     } else {
#       if (is.na(crs(studyArea)))
#         stop("studyArea does not have a crs")
#       studyArea
#     }
#     x <- cropTo(from = x, cropTo = cropTo)
#   }
#   x
# }
#
# #' @export
# cropInputs.SpatRaster <- function(x, studyArea = NULL, rasterToMatch = NULL,
#                                   verbose = getOption("reproducible.verbose", 1),
#                                   extentToMatch = NULL, extentCRS = NULL,
#                                   useGDAL = getOption("reproducible.useGDAL", FALSE),
#                                   useCache = getOption("reproducible.useCache", FALSE),
#                                   ...) {
#   if (!is.null(studyArea) || !is.null(rasterToMatch) || !is.null(extentToMatch)) {
#     isX_Sp <- is(x, "Spatial")
#     isX_Sf <- is(x, "sf")
#     if (!is.null(extentToMatch)) {
#       rasterToMatch <- suppressWarningsSpecific(falseWarnings = "CRS object has comment",
#                                                 raster(extentToMatch, crs = extentCRS))
#     }
#     cropTo <- if (!is.null(rasterToMatch)) {
#       rasterToMatch
#     } else {
#       if (is.na(crs(studyArea)))
#         stop("studyArea does not have a crs")
#       studyArea
#     }
#     x <- cropTo(from = x, cropTo = cropTo)
#   }
#   x
# }
#'
# #' @param extentToMatch Optional. Can pass an extent here and a `crs` to
# #'                      `extentCRS` instead of `rasterToMatch`. These
# #'                      will override `rasterToMatch`, with a warning if both
# #'                      passed.
# #' @param extentCRS     Optional. Can pass a `crs` here with an extent to
# #'                      `extentTomatch` instead of `rasterToMatch`
# #'
# #' @export
# #' @rdname deprecated
# cropInputs.spatialClasses <- function(x, studyArea = NULL, rasterToMatch = NULL,
#                                       verbose = getOption("reproducible.verbose", 1),
#                                       extentToMatch = NULL, extentCRS = NULL,
#                                       useGDAL = getOption("reproducible.useGDAL", FALSE),
#                                       useCache = getOption("reproducible.useCache", FALSE),
#                                       ...) {
#   # browser(expr = exists("._cropInputs_1"))
#   useExtentToMatch <- useETM(extentToMatch = extentToMatch, extentCRS = extentCRS, verbose = verbose)
#   if (!useExtentToMatch) {
#     extentToMatch <- NULL
#     extentCRS <- NULL
#   }
#   transformToCRSX <- TRUE
#   if (!is.null(studyArea) || !is.null(rasterToMatch) || !is.null(extentToMatch)) {
#     isX_Sp <- is(x, "Spatial")
#     isX_Sf <- is(x, "sf")
#     if (!is.null(extentToMatch)) {
#       rasterToMatch <- suppressWarningsSpecific(falseWarnings = "CRS object has comment",
#                                                 raster(extentToMatch, crs = extentCRS))
#     }
#     cropTo <- if (!is.null(rasterToMatch)) {
#       rasterToMatch
#     } else {
#       if (is.na(crs(studyArea)))
#         stop("studyArea does not have a crs")
#       studyArea
#     }
#'
#     if (isTRUE(getOption("reproducible.useTerra"))) {
#       x <- cropTo(from = x, cropTo = cropTo)
#     } else {
#'
#       # have to project the extent to the x projection so crop will work -- this is temporary
#       #   once cropped, then cropExtent should be rm
#       crsX <- .crs(x)
#       crsCropTo <- .crs(cropTo)
#       if (compareCRS(crsX, crsCropTo)) {
#         cropExtent <- extent(cropTo)
#       } else {
#         if (!is.null(rasterToMatch)) {
#           cropExtent <- projectExtent(cropTo, crsX)
#         } else {
#           isSA_Sp <- is(studyArea, "Spatial")
#           isSA_Sf <- is(studyArea, "sf")
#'
#           # Here, basically, st_intersection doesn't work correctly on longlat data
#           #  So, need to do opposite transformation -- transform X to StudyArea
#           if ( (isX_Sp || isX_Sf) && (isSA_Sp || isSA_Sf) ) {
#             if (sf::st_is_longlat(crsX))
#               transformToCRSX <- FALSE
#           }
#'
#           if (transformToCRSX) {
#             if (isSA_Sp || isSA_Sf) {
#               if (isSA_Sp) {
#                 #theExtent <- as(extent(cropTo), "SpatialPolygons")
#                 #crs(theExtent) <- crsCropTo
#                 cropExtent <- raster::extent(spTransform(x = cropTo, CRSobj = crsX))
#               } else if (isSA_Sf) {
#                 .requireNamespace("sf", stopOnFALSE = TRUE)
#                 cropExtent <- extent(sf::st_transform(cropTo, crs = crsX))
#               }
#             } else {
#               messagePrepInputs("cropInputs must have a rasterToMatch raster, or studyArea Spatial or sf object. ",
#                                 "Returning result with no cropping.", verbose = verbose)
#               cropExtent <- NULL
#             }
#           } else {
#             cropExtent <- extent(cropTo)
#             if (isX_Sp) {
#               x <- sf::st_as_sf(x)
#             }
#             x <- sf::st_transform(x, crs = crsCropTo)
#           }
#         }
#       }
#'
#       isStack <- is(x, "RasterStack") # will return a RasterBrick -- keep track of this
#       isBrick <- is(x, "RasterBrick")
#       if (!is.null(cropExtent)) {
#         # crop it
#         if (!identical(cropExtent, extent(x))) {
#           messagePrepInputs("    cropping ...", verbose = verbose, verboseLevel = 0)
#           dots <- list(...)
#           if (is(x, "sf")) {
#             dots[.formalsNotInCurrentDots(sf::st_crop, ..., signature = is(x))] <- NULL
#           } else {
#             dots[.formalsNotInCurrentDots(raster::crop, ..., signature = is(x))] <- NULL
#           }
#'
#'
#           needOT <- if (!is.null(dots$datatype)) TRUE else FALSE
#'
#           if (is(x, "SpatialPolygonsDataFrame")) {
#             if (ncol(x) == 0) {
#               x <- as(x, "SpatialPolygons")
#               messagePrepInputs("x was a SpatialPolygonsDataFrame with no data; converting to SpatialPolygons object",
#                                 verbose = verbose)
#             }
#           }
#           # need to double check that gdal executable exists before going down this path
#           attemptGDAL <- attemptGDAL(x, useGDAL, verbose = verbose) #!raster::canProcessInMemory(x, n = 3) && isTRUE(useGDAL)
#'
#           cropExtentRounded <- roundToRes(cropExtent, x)
#'
#           isX_Sp_Int <- is(x, "Spatial")
#           isX_Sf_Int <- is(x, "sf")
#'
#           if (attemptGDAL && is(x, "Raster") &&
#               length(Filenames(x, allowMultiple = FALSE)) <= 1) {
#             message("GDAL is deprecated in cropInputs")
#           }
#'
#           if (isX_Sp || isX_Sf) { # raster::crop has stopped working on SpatialPolygons
#             yyy <- as(cropExtentRounded, "SpatialPolygons")
#             if (transformToCRSX) {
#               crs(yyy) <- crsX
#             } else {
#               crs(yyy) <- crsCropTo
#             }
#'
#             if (isX_Sp_Int) {
#               yy <- retry(retries = 2, silent = FALSE, exponentialDecayBase = 1,
#                           expr = quote(
#                             sf::st_as_sf(x)
#                           ),
#                           exprBetween = quote(
#                             x <- fixErrors(x, testValidity = NA, useCache = useCache)
#                           ))
#               x <- yy
#             }
#'
#             yyySF <- retry(retries = 2, silent = FALSE, exponentialDecayBase = 1,
#                            expr = quote(
#                              sf::st_as_sf(yyy)
#                            ),
#                            exprBetween = quote(
#                              yyy <- fixErrors(yyy, testValidity = NA, useCache = useCache)
#                            ))
#'
#'
#             # This tryCatch seems to be finding a bug in st_intersection:
#             #   The error was:
#             #   Error in geos_op2_geom("intersection", x, y) :
#             #      st_crs(x) == st_crs(y) is not TRUE
#             #   But the st_crs are identical
#             x <- tryCatch(sf::st_intersection(x, yyySF), error = function(xxx) {
#               x <- sf::st_transform(x, sf::st_crs(crsX))
#               sf::st_intersection(x, yyySF)
#             })
#'
#             if (!transformToCRSX) {
#               x <- sf::st_transform(x, crsX)
#             }
#             if (NROW(x) == 0)
#               stop("    polygons do not intersect.")
#             if (isX_Sp)
#               x <- as(x, "Spatial")
#'
#           } else {
#             if (!is.null(dots$datatype)) {
#               if (length(dots$datatype) > 1) {
#                 warning("datatype can only be length 1 for raster::crop. Using first value: ",
#                         dots$datatype[1])
#                 dots$datatype <- dots$datatype[1]
#               }
#             }
#             layerNamesNow <- names(x)
#             # Need to assign to "not x" so that retry can do its thing on fail
#             yy <- retry(retries = 2, silent = FALSE, exponentialDecayBase = 1,
#                         expr = quote(
#                           if (canProcessInMemory(x, 3)) {
#                             do.call(raster::crop, args = append(list(x = x, y = cropExtentRounded),
#                                                                 dots))
#                           } else {
#                             do.call(raster::crop,
#                                     args = append(list(x = x, y = cropExtentRounded,
#                                                        filename = paste0(tempfile(tmpdir = tmpDir()), ".tif")),
#                                                   dots))
#                           }
#                         ),
#                         exprBetween = quote(
#                           x <- fixErrors(x, testValidity = NA, useCache = useCache)
#                         ))
#             if (!identical(names(yy), layerNamesNow))
#               names(yy) <- layerNamesNow
#             x <- yy
#           }
#'
#           if (is.null(x)) {
#             messagePrepInputs("    polygons do not intersect.", verbose = verbose, verboseLevel = 0)
#           }
#         }
#       }
#       if (isStack) {
#         if (!is(x, "RasterStack"))
#           x <- raster::stack(x)
#       } else if (isBrick) {
#         if (!is(x, "RasterBrick"))
#           x <- raster::brick(x)
#       }
#     }
#   }
#   return(x)
# }
#'
# #' @export
# #' @rdname deprecated
# cropInputs.sf <- function(x, studyArea = NULL, rasterToMatch = NULL,
#                           verbose = getOption("reproducible.verbose", 1),
#                           extentToMatch = NULL, extentCRS = NULL,
#                           useCache = getOption("reproducible.useCache", FALSE),
#                           ...) {
#   .requireNamespace("sf", stopOnFALSE = TRUE)
#   useExtentToMatch <- useETM(extentToMatch = extentToMatch, extentCRS = extentCRS, verbose = verbose)
#   if (useExtentToMatch) {
#     extentToMatch <- NULL
#     extentCRS <- NULL
#   }
#   messagePrepInputs("cropInputs with sf class objects is still experimental", verbose = verbose)
#   if (!is.null(studyArea) || !is.null(rasterToMatch) || !is.null(extentToMatch)) {
#     if (!is.null(extentToMatch)) {
#       rasterToMatch <- raster(extentToMatch, crs = extentCRS)
#     }
#     cropTo <- if (!is.null(rasterToMatch)) {
#       rasterToMatch
#     } else {
#       studyArea
#     }
#'
#     if (isTRUE(getOption("reproducible.useTerra"))) {
#       x <- cropTo(from = x, cropTo = cropTo)
#     } else {
#'
#       # have to project the extent to the x projection so crop will work -- this is temporary
#       #   once cropped, then cropExtent should be rm
#       cropExtent <- if (compareCRS(x, cropTo)) {
#         extent(cropTo)
#       } else {
#         if (!is.null(rasterToMatch)) {
#           # stop("Can't work with rasterToMatch and sf objects yet in cropInputs")
#           projectExtent(cropTo, .crs(x))
#         } else {
#           if (is(studyArea, "sf")) {
#             sf::st_transform(x = cropTo, crs = sf::st_crs(x))
#           } else if (is(studyArea, "Spatial")) {
#             sf::st_transform(x = sf::st_as_sf(cropTo), crs = sf::st_crs(x))
#           } else {
#             NULL
#           }
#         }
#       }
#'
#       if (!is.null(cropExtent)) {
#         # crop it
#         if (!identical(cropExtent, extent(x))) {
#           messagePrepInputs("    cropping with st_crop ...", verbose = verbose, verboseLevel = 0)
#           dots <- list(...)
#           dots[.formalsNotInCurrentDots(sf::st_crop, ..., signature = is(x))] <- NULL
#           yy <- retry(retries = 2, silent = FALSE, exponentialDecayBase = 1,
#                       expr = quote(
#                         do.call(sf::st_crop, args = append(list(x = x, y = cropExtent), dots))
#                       ),
#                       exprBetween = quote(
#                         x <- fixErrors(x, testValidity = NA, useCache = useCache)
#                       ))
#           x <- yy
#'
#           if (all(sapply(extent(x), function(xx) is.na(xx)))) {
#             messagePrepInputs("    polygons do not intersect.", verbose = verbose)
#           }
#         }
#       }
#     }
#   }
#   return(x)
# }

#' Do some minor error fixing
#'
#' `fixErrors` --> `fixErrosTerra`
#'
#' @param x A `SpatialPolygons*` or `sf` object.
#'
#' @param objectName Optional. This is only for messaging; if provided, then messages relayed
#'                   to user will mention this.
#'
#' @param attemptErrorFixes Will attempt to fix known errors. Currently only some failures
#'        for `SpatialPolygons*` are attempted.
#'        Notably with `terra::buffer(..., width = 0)`.
#'        Default `TRUE`, though this may not be the right action for all cases.
#' @param useCache Logical, default `getOption("reproducible.useCache", FALSE)`, whether
#'                 Cache is used on the internal `terra::buffer` command.
#' @param testValidity Logical. If `TRUE`, the a test for validity will happen
#'                 before actually running buffering (which is the solution in most
#'                 cases). However, sometimes it takes longer to test for validity
#'                 than just buffer without testing (there are no consequences of
#'                 buffering if everything is valid). If `FALSE`, then the
#'                 test will be skipped and the buffering will happen regardless.
#'                 If `NA`, then all testing and buffering will be skipped.
#' @param ... Passed to methods. None currently implemented.
#'
#' @export
#' @rdname deprecated
#' @return A GIS file (e.g., RasterLayer, SpatRaster etc.) that has been
#' attempted to be fixed, if it finds errors.
#' @keywords internal
#' @seealso [fixErrorsTerra()], [postProcessTerra()], [postProcess()]
#'
#' @examples
#' library(sp)
#' library(raster)
#'
#' # make a SpatialPolygon
#' coords1 <- structure(c(-123.98, -117.1, -80.2, -100, -123.98, 60.9, 67.73, 65.58, 51.79, 60.9),
#'                        .Dim = c(5L, 2L))
#' Sr1 <- Polygon(coords1)
#' Srs1 <- Polygons(list(Sr1), "s1")
#' shpEcozone <- SpatialPolygons(list(Srs1), 1L)
#' crs(shpEcozone) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
#' fixErrors(shpEcozone)
fixErrors <- function(x, objectName, attemptErrorFixes = TRUE,
                      useCache = getOption("reproducible.useCache", FALSE),
                      verbose = getOption("reproducible.verbose", 1),
                      testValidity = getOption("reproducible.testValidity", TRUE),
                      ...) {
  UseMethod("fixErrors")
}

#' @export
#' @keywords internal
#' @rdname deprecated
fixErrors.default <- function(x, objectName, attemptErrorFixes = TRUE,
                              useCache = getOption("reproducible.useCache", FALSE),
                              verbose = getOption("reproducible.verbose", 1),
                              testValidity = getOption("reproducible.testValidity", TRUE),
                              ...) {
  fixErrorsTerra(x)
}

#
#
#  @export
#  @keywords internal
#  @rdname fixErrors
# fixErrors.Raster <- function(x, objectName, attemptErrorFixes = TRUE,
#                              useCache = getOption("reproducible.useCache", FALSE),
#                              verbose = getOption("reproducible.verbose", 1),
#                              testValidity = getOption("reproducible.testValidity", TRUE),
#                              ...) {
#
#   #rounding lon lat resolution will break the raster
#   if (!isLonLat(x)) {
#     origin(x) <- roundTo6Dec(origin(x))
#     xmin(x) <- roundTo6Dec(xmin(x))
#     ymin(x) <- roundTo6Dec(ymin(x))
#     xmax(x) <- roundTo6Dec(xmax(x))
#     ymax(x) <- roundTo6Dec(ymax(x))
#     res(x) <- roundTo6Dec(res(x))
#   }
#   # if (!identical(origin(x), round(origin(x), .Machine$double.eps))) {
#   #   roundedOrigin <- round(origin(x),6)
#   #   if (identical(origin(x), roundedOrigin))
#   #     origin(x) <- roundedOrigin
#   # }
#   # roundedRes <- round(res(x),6)
#   # if (identical(res(x), roundedRes))
#   #   res(x) <- roundedRes
#   # roundedExtent <- round(extent(x),6)
#   # if (identical(extent(x), roundedExtent))
#   #   extent(x) <- roundedExtent
#   x
# }
#
# # Fix `sf::st_is_valid` failures in `SpatialPolygons`
# #
# # This uses `terra::buffer(..., width = 0)` internally, which fixes some
# # failures to `sf::st_is_valid`
# #
# # @export
# # @rdname fixErrors
# fixErrors.SpatialPolygons <- function(x, objectName = NULL,
#                                       attemptErrorFixes = TRUE,
#                                       useCache = getOption("reproducible.useCache", FALSE),
#                                       verbose = getOption("reproducible.verbose", 1),
#                                       testValidity = getOption("reproducible.testValidity", TRUE),
#                                       ...) {
#   if (attemptErrorFixes) {
#     if (is.null(objectName)) objectName = "SpatialPolygon"
#     if (is(x, "SpatialPolygons")) {
#       messagePrepInputs("Checking for errors in ", objectName, verbose = verbose)
#
#       runBuffer <- if (requireNamespace("sf", quietly = TRUE) && isTRUE(testValidity)) {
#         x1 <- sf::st_as_sf(x)
#         anv <- any(!sf::st_is_valid(x1))
#         if (isTRUE(anv)) {
#           messagePrepInputs("Found errors in ", objectName, ". Attempting to correct.",
#                             verbose = verbose)
#         }
#         anv
#       } else if (is.na(testValidity)) {
#         FALSE
#       } else {
#         TRUE
#       }
#
#       if (runBuffer) {
#         # if (!requireNamespace("rgeos", quietly = TRUE)) stop(messageRgeosMissing)
#         messagePrepInputs("      Trying the buffer = 0 trick", verbose = verbose, verboseLevel = 2)
#         # prevent the warning about not projected, because we are buffering 0, which doesn't matter
#         x1 <-
#           suppressWarningsSpecific(falseWarnings = paste("Spatial object is not projected;",
#                                                          "GEOS expects planar coordinates"),
#                                    try(as(Cache(terra::buffer(terra::vect(x), width = 0),
#                                              # dissolve = FALSE,
#                                              useCache = useCache), "Spatial"))#,
#           )
#
#         x <- bufferWarningSuppress(#warn = attr(x1, "warning"),
#           objectName = objectName,
#           x1 = x1, bufferFn = "terra::buffer")
#       } else {
#         messagePrepInputs("  Found no errors.", verbose = verbose)
#       }
#     }
#   }
#   return(x)
# }
#
# # @export
# # @rdname fixErrors
# fixErrors.sf <- function(x, objectName = NULL, attemptErrorFixes = TRUE,
#                          useCache = getOption("reproducible.useCache", FALSE),
#                          verbose = getOption("reproducible.verbose", 1),
#                          testValidity = getOption("reproducible.testValidity", TRUE),
#                          ...) {
#   .requireNamespace("sf", stopOnFALSE = TRUE)
#   if (attemptErrorFixes) {
#     if (is.null(objectName))
#       objectName <- "SimpleFeature"
#
#     if ((is(sf::st_geometry(x), "sfc_MULTIPOLYGON") || is(sf::st_geometry(x), "sfc_GEOMETRY") ||
#          is(sf::st_geometry(x), "sfc_POLYGON")) && !(is(sf::st_geometry(x), "sfc_POINT"))) {
#       messagePrepInputs("Checking for errors in ", objectName, verbose = verbose)
#
#       ## sfc_GEOMETRY may itself contain points, so filter them out
#
#
#       ## BROWSER HERE -- WHY COLLECTION?
#
#       x <- suppressWarnings(sf::st_collection_extract(x, "POLYGON"))
#
#       ## too computationally intensive to buffer everything all the time, so only do for invalid geometries
#       runBuffer <- if (isTRUE(testValidity)) {
#         suppressWarnings(any(!sf::st_is_valid(x)))
#       } else if (is.na(testValidity)) {
#         FALSE
#       } else {
#         TRUE
#       }
#       if (isTRUE(runBuffer)) {
#         messagePrepInputs("Found errors in ", objectName, ". Attempting to correct.",
#                           verbose = verbose, verboseLevel = 1)
#
#         x1 <- suppressWarningsSpecific(falseWarnings = paste("Spatial object is not projected;",
#                                                              "GEOS expects planar coordinates"),
#                                        try(Cache(sf::st_make_valid(x), useCache = useCache, verbose = verbose)))
#         x <- bufferWarningSuppress(#warn = attr(x1, "warning"),
#           objectName = objectName,
#           x1 = x1, bufferFn = "sf::st_make_valid",
#           verbose = verbose)
#
#       } else {
#         messagePrepInputs("  Found no errors.", verbose = verbose, verboseLevel = 1)
#       }
#     }
#   }
#   return(x)
# }

#' Project `Raster*` or `Spatial*` or `sf` objects
#'
#' Deprecated. Use [projectTo()].
#'
#' @param x A `Raster*`, `Spatial*` or `sf` object
#'
#' @param targetCRS The CRS of x at the end  of this function (i.e., the goal)
#'
#' @param ... Passed to [raster::projectRaster()].
#'
#' @param rasterToMatch Template `Raster*` object passed to the `to` argument of
#'                      [raster::projectRaster()], thus will changing the
#'                      resolution and projection of `x`.
#'                      See details in [postProcess()].
#'
#' @param cores An `integer*` or `'AUTO'`. This will be used if `gdalwarp` is
#'              triggered. `'AUTO'*` will calculate 90% of the total
#'              number of cores in the system, while an integer or rounded
#'              float will be passed as the exact number of cores to be used.
#'
#' @return A file of the same type as starting, but with projection (and possibly
#' other characteristics, including resolution, origin, extent if changed).
#'
#' @export
#' @inheritParams prepInputs
#' @rdname deprecated
#' @seealso [projectTo()]
#' @return A GIS file (e.g., RasterLayer, SpatRaster etc.) that has been
#' appropriately reprojected.
#'
projectInputs <- function(x, targetCRS, verbose = getOption("reproducible.verbose", 1), ...) {
  UseMethod("projectInputs")
}

#' @export
#' @rdname deprecated
projectInputs.default <- function(x, targetCRS, ...) {
  projectTo(x, ...)
}

# @export
# @rdname deprecated
# @param useGDAL Logical or `"force"`. This is defunct; internals now can use
#     `terra` if `options("reproducible.useTerra" = TRUE)`, which is not (yet)
#     the default.
#
# @importFrom fpCompare %==%
# @importFrom stats na.omit
# projectInputs.Raster <- function(x, targetCRS = NULL,
#                                  verbose = getOption("reproducible.verbose", 1),
#                                  rasterToMatch = NULL, cores = NULL,
#                                  useGDAL = getOption("reproducible.useGDAL", FALSE),
#                                  ...) {
#   messagePrepInputs("    reprojecting ...", verbose = verbose, verboseLevel = 0)
#   dots <- list(...)
#   # browser(expr = exists("._projectInputs_1"))
#
#   isFactorRaster <- FALSE
#   isStack <- is(x, "RasterStack")
#   if (isTRUE(raster::is.factor(x))) {
#     isFactorRaster <- TRUE
#     rasterFactorLevels <- raster::levels(x)
#   }
#
#   if (is.null(rasterToMatch) && is.null(targetCRS)) {
#     messagePrepInputs("     no reprojecting because no rasterToMatch & targetCRS are FALSE (or NULL).",
#                       verbose = verbose, verboseLevel = 0)
#   } else if (is.null(rasterToMatch) & identical(.crs(x), targetCRS)) {
#     messagePrepInputs("    no reprojecting because target CRS is same as input CRS.", verbose = verbose,
#                       verboseLevel = 0)
#   } else {
#     if (is.null(targetCRS)) {
#       targetCRS <- .crs(rasterToMatch)
#     }
#     srcCRS <- .crs(x)
#
#     dontSpecifyResBCLongLat <- isLongLat(targetCRS, srcCRS)
#
#     doProjection <- FALSE
#     if (is.null(rasterToMatch)) {
#       if (!identical(.crs(x), targetCRS))  doProjection <- TRUE
#     } else if (differentRasters(x, rasterToMatch, targetCRS)) {
#       doProjection <- TRUE
#     }
#
#     if (doProjection) {
#       # need to double check that gdal executable exists before going down this path
#       attemptGDAL <- attemptGDAL(x, useGDAL, verbose = verbose) #!raster::canProcessInMemory(x, n = 3) && isTRUE(useGDAL)
#
#       if (attemptGDAL) {
#         message("GDAL is deprecated in cropInputs")
#       }
#         origDataType <- dataType(x)
#
#         # Capture problems that projectRaster has with objects of class integers,
#         #   which is different than if they are integers (i.e., a numeric class object)
#         #   can be integers, without being classified and stored in R as integer
#
#         # should be faster than assessDataType, as it is a class determination,
#         # not a numeric assessment:
#         isInteger <- if (is.integer(x[])) TRUE else FALSE
#
#         if (isInteger) {
#           if (!is.null(dots$method)) {
#             if (dots$method != "ngb") {
#               warning("This raster layer has integer values; it will be reprojected to float. ",
#                       "Did you want to pass 'method = \"ngb\"'?")
#             }
#           }
#         }
#
#         if (is.null(dots$method)) {
#           # not foolproof method of determining reclass method:
#           dots$method <- assessDataType(x, type = "projectRaster")
#           uniqueDotsMethod <- unique(dots$method)
#           if (length(uniqueDotsMethod) > 1) {
#             if (length(intersect(uniqueDotsMethod, "ngb")) == 1)
#               uniqueDotsMethod <- "ngb"
#             else
#               uniqueDotsMethod <- uniqueDotsMethod[1]
#             messagePrepInputs("There is more than one dataType in the layers of the Raster* object; reprojection will use",
#                               uniqueDotsMethod, verbose = verbose)
#           } else {
#             dots$method <- uniqueDotsMethod
#           }
#         }
#
#         messagePrepInputs("      reprojecting using ", dots$method, "...", verbose = verbose)
#
#         falseWarns <- paste0(projNotWKT2warn, "|input and ouput crs|no non-missing arguments")
#
#         if (requireNamespace("terra", quietly = TRUE) && getOption("reproducible.useTerra", FALSE)) {
#           m1 <- methodFormals(terra::project, "SpatRaster")
#           m2 <- methodFormals(terra::writeRaster, signature = c("SpatRaster", "character"))
#           if (!is.null(dots$method)) {
#             if (identical(dots$method, "ngb"))
#               dots$method <- "near"
#           }
#         }
#
#         if (is.null(rasterToMatch)) {
#           if (requireNamespace("terra", quietly = TRUE) && getOption("reproducible.useTerra", FALSE)) {
#             messagePrepInputs("Using terra::project for reprojection")
#             Args <- append(dots, list(x = terra::rast(x), y = targetCRS))
#             keepers <- na.omit(match(union(names(m1), names(m2)), names(Args)))
#             if (length(keepers))
#               Args <- Args[keepers]
#
#             x1 <- # captureWarningsToAttr( Eliot
#               suppressWarningsSpecific(falseWarnings = falseWarns,
#                                        do.call(terra::project, args = Args), verbose = verbose)
#             x <- if (terra::nlyr(x1) > 1) raster::stack(x1) else raster::raster(x1)
#           } else {
#             Args <- append(dots, list(from = x, crs = targetCRS))
#             x <- # captureWarningsToAttr( Eliot
#               suppressWarningsSpecific(do.call(projectRaster, args = Args),
#                                        falseWarnings = falseWarns)
#             #)
#             #warn <- attr(x, "warning")
#             #attr(x, "warning") <- NULL
#           }
#         } else {
#           # projectRaster does silly things with integers, i.e., it converts to numeric
#           if (is.na(targetCRS))
#             stop("rasterToMatch needs to have a projection (crs)")
#           tempRas <- suppressWarningsSpecific(
#             projectExtent(object = rasterToMatch, crs = targetCRS), projNotWKT2warn)
#           if (requireNamespace("terra", quietly = TRUE) &&
#               getOption("reproducible.useTerra", FALSE)) {
#             messagePrepInputs("      Using terra::project for reprojection")
#             Args <- append(dots, list(x = terra::rast(x), y = terra::rast(tempRas)))
#
#             keepers <- na.omit(match(union(names(m1), names(m2)), names(Args)))
#             if (length(keepers))
#               Args <- Args[keepers]
#
#             x1 <- # captureWarningsToAttr( Eliot
#               suppressWarningsSpecific(falseWarnings = falseWarns,
#                                        do.call(terra::project, args = Args), verbose = verbose)
#             x <- if (terra::nlyr(x1) > 1) raster::stack(x1) else raster::raster(x1)
#           } else {
#
#             Args <- append(dots, list(from = x, to = tempRas))
#             x <- # captureWarningsToAttr( Eliot
#               suppressWarningsSpecific(falseWarnings = falseWarns,
#                                        do.call(projectRaster, args = Args), verbose = verbose)
#             #)
#           }
#           if (isStack)
#             if (!is(x, "RasterStack")) x <- raster::stack(x)
#           # check for faulty datatype --> namely if it is an integer but classified as flt because of floating point problems
#           if (isTRUE(grepl("FLT", dataType(x)))) {
#             rrr <- round(x[], 0) %==% x[]
#             if (isTRUE(sum(!rrr[!is.na(rrr)]) == 0)) # if (isTRUE(sum(!na.omit(rrr)) == 0))
#               x[] <- round(x[], 0)
#           }
#           #warn <- attr(x, "warning")
#           #attr(x, "warning") <- NULL
#
#           if (identical(.crs(x), .crs(rasterToMatch)) & any(res(x) != res(rasterToMatch))) {
#             if (all(res(x) %==% res(rasterToMatch))) {
#               res(x) <- res(rasterToMatch) # TODO: This is irrelevant. Should not happen. TO Omit.
#             } else {
#               stop("Error: input and output resolutions are not similar after using projectRaster.",
#                    "\nTry increasing error tolerance in options('fpCompare.tolerance').")
#             }
#           }
#         }
#         if (!compareCRS(x, targetCRS)) {
#           crs(x) <- targetCRS # sometimes the proj4string is rearranged, so they are not identical:
#           #  they should be
#         }
#
#         # return the integer class to the data in the raster object
#         if (isTRUE(isInteger)) {
#           x[] <- as.integer(x[])
#           dataType(x) <- origDataType
#         }
#
#         #warn <- warn[!grepl("no non-missing arguments to m.*; returning .*Inf", warn)] # This is a bug in raster
#         #if (length(warn))
#         #  warnings(warn)
#         ## projectRaster doesn't always ensure equal res (floating point number issue)
#         ## if resolutions are close enough, re-write res(x)
#         ## note that when useSAcrs = TRUE, the different resolutions may be due to
#         ## the different projections (e.g. degree based and meter based). This should be fine
#
#      #}
#     } else {
#       messagePrepInputs("    no reprojecting because target characteristics same as input Raster.",
#                         verbose = verbose,
#                         verboseLevel = 0)
#     }
#   }
#
#   if (isFactorRaster) {
#     levels(x) <- rasterFactorLevels
#   }
#
#   x
# }


# @export
# @rdname deprecated
# projectInputs.SpatVector <- function(x, targetCRS, verbose = getOption("reproducible.verbose", 1), ...) {
#   projectTo(from = x, projectTo = targetCRS)
#
# }

# @export
# @rdname deprecated
# projectInputs.SpatRaster <- function(x, targetCRS = NULL,
#                                      verbose = getOption("reproducible.verbose", 1),
#                                      rasterToMatch = NULL, cores = NULL,
#                                      useGDAL = getOption("reproducible.useGDAL", FALSE),
#                                      ...) {
#
#   if (!is.null(rasterToMatch)) {
#     if (!is.null(targetCRS)) {
#       if (terra::crs(rasterToMatch) != targetCRS)
#         message("both rasterToMatch and targetCRS are supplied to projectInputs and they are different; using rasterToMatch")
#     }
#     projectTo(from = x, projectTo = rasterToMatch)
#   } else {
#     if (!is.null(targetCRS)) {
#       projectTo(from = x, projectTo = targetCRS)
#     }
#   }
#
#
# }

# @export
# @rdname deprecated
# projectInputs.sf <- function(x, targetCRS, verbose = getOption("reproducible.verbose", 1), ...) {
#   messagePrepInputs("    reprojecting ...", verbose = verbose, verboseLevel = 0)
#   .requireNamespace("sf", stopOnFALSE = TRUE)
#   if (!is.null(targetCRS)) {
#     if (isTRUE(getOption("reproducible.useTerra"))) {
#       x <- projectTo(from = x, projectTo = targetCRS)
#     } else {
#
#       warning("sf class objects not fully tested Use with caution.")
#       .requireNamespace("sf", stopOnFALSE = TRUE)
#       isValid <- sf::st_is_valid(x)
#       if (any(sf::st_is(x, c("POLYGON", "MULTIPOLYGON"))) && !any(isValid)) {
#         x[!isValid] <- sf::st_buffer(x[!isValid], dist = 0, ...)
#       }
#
#       if ("projargs" %in% slotNames(targetCRS) )
#         targetCRS <- sf::st_crs(targetCRS@projargs)
#       x <- sf::st_transform(x = x, crs = targetCRS, ...)
#       if (!identical(sf::st_crs(x), targetCRS)) {
#         ## sometimes the proj4string is rearranged, so they are not identical; they should be
#         sf::st_crs(x) <- targetCRS
#       }
#     }
#   }
#   x
# }

# @export
# @rdname deprecated
# projectInputs.Spatial <- function(x, targetCRS, verbose = getOption("reproducible.verbose", 1), ...) {
#   messagePrepInputs("    reprojecting ...", verbose = verbose, verboseLevel = 0)
#   if (!is.null(targetCRS)) {
#
#     if (isTRUE(getOption("reproducible.useTerra"))) {
#       x <- projectTo(from = suppressWarningsSpecific(x, shldBeChar),
#                      projectTo = targetCRS)
#     } else {
#       if (!is(targetCRS, "CRS")) {
#         if (!is.character(targetCRS)) {
#           if (is(targetCRS, "spatialClasses")) {
#             targetCRS <- .crs(targetCRS)
#           } else {
#             stop("targetCRS in projectInputs must be a CRS object or a class from",
#                  " which a crs can be extracted with raster::crs")
#           }
#         }
#       }
#       x <- spTransform(x = x, CRSobj = targetCRS)
#       if (!identical(.crs(x), targetCRS)) {
#         ## sometimes the proj4string is rearranged, so they are not identical; they should be
#         crs(x) <- targetCRS
#       }
#     }
#   }
#   x
# }

#' Hierarchically get crs from `Raster*`, `Spatial*`
#'
#' This is the function that follows the table of order of
#' preference for determining CRS. See [postProcess()]
#' @inheritParams postProcess.spatialClasses
#' @keywords internal
#' @rdname postProcessHelpers
.getTargetCRS <- function(useSAcrs, studyArea, rasterToMatch, targetCRS = NULL) {
  if (is.null(targetCRS)) {
    targetCRS <- if (useSAcrs) {
      .crs(studyArea)
    } else if (!is.null(rasterToMatch)) {
      .crs(rasterToMatch)
    } else {
      NULL # don't reproject a Raster if only has studyArea -- too lossy
    }
  }
  targetCRS
}

#' Mask module inputs
#'
#' This function can be used to mask inputs from data. Masking here is
#' equivalent to `raster::mask` (though [fastMask()] is used here)
#' or `raster::intersect`.
#'
#' @param x An object to do a geographic raster::mask/raster::intersect.
#'          See methods.
#' @param ... Passed to methods. None currently implemented.
#'
#' @inheritParams cropInputs
#'
#' @author Eliot McIntire and Jean Marchal
#' @export
#' @importFrom utils capture.output
#' @rdname maskInputs
#' @return A GIS file (e.g., RasterLayer, SpatRaster etc.) that has been
#' appropriately masked.
#' @seealso [maskTo()], [postProcess()] for related examples
#' @examples
#' library(sp)
#' library(raster)
#'
#' # make a SpatialPolygon
#' coords1 <- structure(c(-123.98, -117.1, -80.2, -100, -123.98, 60.9, 67.73, 65.58, 51.79, 60.9),
#'                        .Dim = c(5L, 2L))
#' Sr1 <- Polygon(coords1)
#' Srs1 <- Polygons(list(Sr1), "s1")
#' shpEcozone <- SpatialPolygons(list(Srs1), 1L)
#' crs(shpEcozone) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
#'
#' # make a "study area" that is subset of larger dataset
#' coords <- structure(c(-118.98, -116.1, -99.2, -106, -118.98, 59.9, 65.73, 63.58, 54.79, 59.9),
#'                       .Dim = c(5L, 2L))
#' Sr1 <- Polygon(coords)
#' Srs1 <- Polygons(list(Sr1), "s1")
#' StudyArea <- SpatialPolygons(list(Srs1), 1L)
#' crs(StudyArea) <- crs(shpEcozone)
#' projString <- "+proj=utm +zone=15 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
#' StudyArea <- sp::spTransform(StudyArea, CRSobj = projString)
#' maskInputs(shpEcozone, StudyArea)
#'
maskInputs <- function(x, studyArea, ...) {
  UseMethod("maskInputs")
}


maskInputs.default <- function(x, studyArea, rasterToMatch = NULL, maskWithRTM = NULL,
                              verbose = getOption("reproducible.verbose", 1), ...) {
  maskTo(x, ...)
}

# # @param maskWithRTM Logical. If `TRUE`, then the default,
# #
# # @export
# # @rdname maskInputs
# maskInputs.Raster <- function(x, studyArea, rasterToMatch = NULL, maskWithRTM = NULL,
#                               verbose = getOption("reproducible.verbose", 1), ...) {
#   messagePrepInputs("    masking ...", verbose = verbose, verboseLevel = 0)
#   # browser(expr = exists("._maskInputs_1"))
#   isStack <- is(x, "RasterStack")
#   if (is.null(studyArea) && !is.null(rasterToMatch) && is.null(maskWithRTM)) {
#     messagePrepInputs("      studyArea is NULL; rasterToMatch provided. Masking with rasterToMatch NA values. ",
#                       "To leave unmasked, set maskWithRTM = FALSE", verbose = verbose)
#     maskWithRTM <- TRUE
#   }
#   if (isTRUE(maskWithRTM)) {
#     x <- maskWithRasterNAs(x = x, y = rasterToMatch)
#   } else {
#     if (!is.null(studyArea)) {
#       # dots <- list(...)
#       x <- fastMask(x = x, y = studyArea, verbose = verbose, ..., skipDeprecastedMsg = TRUE)
#     } else {
#       messagePrepInputs("studyArea not provided, skipping masking.", verbose = verbose)
#     }
#   }
#   if (isStack) { # do this even if no masking; it takes 10 microseconds if already a RasterStack
#     x <- raster::stack(x)
#   }
#
#   return(x)
# }
#
# # @export
# # @rdname maskInputs
# maskInputs.Spatial <- function(x, studyArea, rasterToMatch = NULL, maskWithRTM = FALSE,
#                                verbose = getOption("reproducible.verbose", 1),
#                                useCache = getOption("reproducible.useCache", FALSE),
#                                ...) {
#
#   if (isTRUE(getOption("reproducible.useTerra"))) {
#     to <- if (isTRUE(maskWithRTM)) rasterToMatch else studyArea
#     maskTo(from = x, maskTo = to)
#   } else {
#     x <- sf::st_as_sf(x)
#
#     x <- retry(retries = 2, silent = FALSE, exponentialDecayBase = 1,
#                expr = quote(
#                  maskInputs(x, studyArea, rasterToMatch, maskWithRTM, verbose = verbose)
#                ),
#                exprBetween = quote(
#                  x <- fixErrors(x, testValidity = NA, useCache = useCache)
#                ))
#     x <- fixErrors(x, testValidity = NA, useCache = useCache) ## needed to deal with sfc issues
#     sf::as_Spatial(x)
#   }
# }
#
# # @export
# # @rdname maskInputs
# maskInputs.SpatVector <- function(x, studyArea, rasterToMatch = NULL, maskWithRTM = FALSE,
#                                   verbose = getOption("reproducible.verbose", 1),
#                                   useCache = getOption("reproducible.useCache", FALSE),
#                                   ...) {
#   to <- if (isTRUE(maskWithRTM)) rasterToMatch else studyArea
#   maskTo(from = x, maskTo = to)
#
# }
#
# # @export
# # @rdname maskInputs
# maskInputs.SpatRaster <- function(x, studyArea, rasterToMatch = NULL, maskWithRTM = FALSE,
#                                   verbose = getOption("reproducible.verbose", 1),
#                                   useCache = getOption("reproducible.useCache", FALSE),
#                                   ...) {
#   to <- if (isTRUE(maskWithRTM)) rasterToMatch else studyArea
#   maskTo(from = x, maskTo = to)
#
# }
#
#
# # @export
# # @rdname maskInputs
# maskInputs.sf <- function(x, studyArea, verbose = getOption("reproducible.verbose", 1),
#                           useCache = getOption("reproducible.useCache", FALSE),
#                           ...) {
#   .requireNamespace("sf", stopOnFALSE = TRUE)
#
#   if (!is.null(studyArea)) {
#     if (is(studyArea, "Spatial")) {
#       studyArea <- sf::st_as_sf(studyArea)
#     }
#
#     xOrigCRS <- sf::st_crs(x)
#     changedCRS <- FALSE
#     isXLongLat <- sf::st_is_longlat(x)
#     messagePrepInputs("maskInputs with sf class objects is still experimental", verbose = verbose)
#     messagePrepInputs("    intersecting ...", verbose = verbose, verboseLevel = 0)
#     #studyArea <- raster::aggregate(studyArea, dissolve = TRUE)
#     if (!identical(xOrigCRS, sf::st_crs(studyArea))) {
#       if (isXLongLat) {
#         changedCRS <- TRUE
#         x <- sf::st_transform(x, crs = sf::st_crs(studyArea))
#       } else {
#         studyArea <- sf::st_transform(studyArea, crs = xOrigCRS)
#       }
#     }
#
#     if (NROW(studyArea) > 1) {
#       studyArea <- sf::st_sf(sf::st_combine(studyArea))
#     }
#
#     if (is(sf::st_geometry(x), "sfc_POINT")) {
#       y1 <- sf::st_intersects(x, studyArea)
#       y2 <- vapply(y1, function(x) length(x) == 1, logical(1))
#       y <- x[y2, ]
#     } else {
#       x <- sf::st_set_precision(x, 1e5)
#       studyArea <- sf::st_set_precision(studyArea, 1e5)
#       studyArea <- fixErrors(studyArea, useCache = useCache)
#
#       y <- retry(retries = 2, silent = FALSE, exponentialDecayBase = 1,
#                  expr = quote(
#                    sf::st_intersection(x, studyArea)
#                  ),
#                  exprBetween = quote(
#                    x <- fixErrors(x, testValidity = NA, useCache = useCache)
#                  ))
#       # x <- sf::st_set_precision(x, 1e5) %>% fixErrors(.)
#       # studyArea <- sf::st_set_precision(studyArea, 1e5) %>% fixErrors(.)
#       # y <- sf::st_intersection(x, studyArea)
#       # y <- fixErrors(y)
#     }
#     if (!identical(.crs(y), .crs(x))) {
#       ## sometimes the proj4string is rearranged, so they are not identical; they should be
#       crs(y) <- .crs(x)
#     }
#     if (changedCRS) {
#       y <- sf::st_transform(y, xOrigCRS)
#     }
#
#     return(y)
#   } else {
#     return(x)
#   }
# }

#' Determine filename, either automatically or manually
#'
#' Determine the filename, given various combinations of inputs.
#'
#' @details
#' The post processing workflow, which includes this function,
#' addresses several scenarios, and depending on which scenario, there are
#' several file names at play. For example, `Raster` objects may have
#'   file-backed data, and so *possess a file name*, whereas `Spatial`
#'   objects do not. Also, if post processing is part of a [prepInputs()]
#'   workflow, there will always be a file downloaded. From the perspective of
#'   `postProcess`, these are the "inputs" or `filename1`.
#'   Similarly, there may or may not be a desire to write an
#'   object to disk after all post processing, `filename2`.
#'
#'   This subtlety means that there are two file names that may be at play:
#'   the "input" file name (`filename1`), and the "output" filename (`filename2`).
#'   When this is used within `postProcess`, it is straight forward.
#'
#'
#'   However, when `postProcess` is used within a `prepInputs` call,
#'   the `filename1` file is the file name of the downloaded file (usually
#'   automatically known following the downloading, and refered to as `targetFile`)
#'   and the `filename2` is the file name of the of post-processed file.
#'
#'   If `filename2` is `TRUE`, i.e., not an actual file name, then the cropped/masked
#'   raster will be written to disk with the original `filenam1/targetFile`
#'   name, with `prefix` prefixed to the basename(`targetFile`).
#'
#'   If `filename2` is a character string, it will be the path of the saved/written
#'   object e.g., passed to `writeOutput`. It will be tested whether it is an
#'   absolute or relative path and used as is if absolute or
#'   prepended with `destinationPath` if relative.
#'
#' @inheritParams postProcess.spatialClasses
#'
#' @param destinationPath Optional. If `filename2` is a relative file path, then this
#'                        will be the directory of the resulting absolute file path.
#'
#' @param prefix The character string to prepend to `filename1`, if `filename2`
#'               not provided.
#'
#' @include helpers.R
#'
#' @details
#'  If `filename2` is `logical`, then the output
#'  filename will be `prefix` prefixed to the basename(`filename1`).
#'  If a character string, it
#'  will be the path returned. It will be tested whether it is an
#'  absolute or relative path and used as is if absolute or prepended with
#'  `destinationPath` if provided, and if `filename2` is relative.
#'
#' @rdname determineFilename
determineFilename <- function(filename2 = NULL, filename1 = NULL,
                              destinationPath = getOption("reproducible.destinationPath", "."),
                              verbose = getOption("reproducible.verbose", 1),
                              prefix = "Small", ...) {
  if (!is.null(filename2)) {
    dots <- list(...)

    if (!is.null(dots$inputFilePath))  {
      stop("inputFilePath is being deprecated; use filename1")
    }

    if (!is.null(dots$postProcessedFilename))  {
      stop("postProcessedFilename is being deprecated; use filename2")
    }

    if (!is.null(dots$targetFilePath))  {
      stop("targetFilePath is being deprecated from determineFilename:\n",
           "  use filename2 and filename1.")
    }

    if (!(is.logical(filename2) || is.character(filename2) || is.null(filename2))) {
      stop("filename2 must be logical or character string or NULL")
    }

    filename2 <- if (!identical(filename2, FALSE)) { # allow TRUE or path
      if (isTRUE(filename2) ) {
        # 1. Take destinationPath, if it exists
        # 2. Take dirname of filename1, if it exists and is absolute path
        # 3. Take getwd()
        theDir <- destinationPath
        if (is.null(destinationPath)) {
          if (is.character(filename1)) {
            if (isAbsolutePath(filename1)) {
              theDir <- dirname(filename1)
              messagePrepInputs("filename2 is NULL; using dirname(filename1) as destinationPath",
                                verbose = verbose)
            }
          }
        }
        filename3 <- normPath(tempfile(tmpdir = theDir, fileext = ".tif"))
        .prefix(filename3, prefix)
      } else {
        iap <- isAbsolutePath(filename2)
        if (all(iap)) {
          filename2
        } else {
          if (any(iap)) {
            stop("filename2 must be all relative or all absolute paths")
          }
          if (!is.null(destinationPath)) {
            file.path(destinationPath, basename(filename2))
          } else {
            filename2 # accept relative
          }
        }
      }
    } else {
      NULL
    }
    if (exists("tmpfile", inherits = FALSE)) {
      messagePrepInputs("Saving output to ", filename2, ".",
                        "Specify filename1 or filename2 for more control, ",
                        "or set filename2 to NULL to prevent saving to disk", verbose = verbose)
    }
  }
  filename2
}

#' Write module inputs on disk
#'
#' See [writeTo()]
#'
#' @param x  The object save to disk i.e., write outputs
#'
#' @param overwrite Logical. Should file being written overwrite an existing file if it exists.
#'
#' @param filename2 File name passed to [raster::writeRaster()], or
#'                  [raster::shapefile()] or [sf::st_write()]
#'                  (`dsn` argument).
#'
#' @param ... Passed into [raster::shapefile()] or
#'             [raster::writeRaster()] or [sf::st_write()]
#'
#' @inheritParams prepInputs
#'
#' @author Eliot McIntire and Jean Marchal
#' @export
#' @return A GIS file (e.g., RasterLayer, SpatRaster etc.) that has been
#' appropriately written to disk. In the case of vector datasets, this will
#' be a side effect. In the case of gridded objects (Raster*, SpatRaster), the
#' object will have a file-backing.
#' @importFrom methods is
#' @importFrom terra writeRaster
#' @rdname deprecated
#'
#' @examples
#' library(sp)
#' library(raster)
#' r <- raster::raster(extent(0,100,0,100), vals = 1:1e2)
#'
#' tf <- tempfile(fileext = ".tif")
#' writeOutputs(r, tf)
writeOutputs <- function(x, filename2,
                         overwrite = getOption("reproducible.overwrite", NULL),
                         ...) {
  UseMethod("writeOutputs")
}

# @export
# @rdname deprecated
# writeOutputs.Raster <- function(x, filename2 = NULL,
#                                 overwrite = getOption("reproducible.overwrite", FALSE),
#                                 verbose = getOption("reproducible.verbose", 1),
#                                 ...) {
#   dots <- list(...)
#   datatype2 <- if (is.null(dots$datatype)) {
#     assessDataType(x, type = "writeRaster")
#   } else {
#     dots$datatype
#   }
#
#   if (!is.null(filename2)) {
#     messagePrepInputs("    writing to disk", verbose = verbose, verboseLevel = 0)
#     if (is.null(dots$datatype)) {
#       out <- lapply(paste("No 'datatype' chosen.",
#                           "Saving", names(x), "as", datatype2 ), messagePrepInputs, verbose = verbose)
#       dots$datatype <- datatype2
#     } else if (any(datatype2 != dots$datatype)) {
#       out <- lapply(paste("chosen 'datatype', ", dots$datatype, ", may be inadequate for the ",
#                           "range/type of values in ", names(x),
#                           "\n consider changing to ", datatype2), messagePrepInputs, verbose = verbose)
#     }
#
#     if (any(raster::is.factor(x))) {
#       filename3 <- gsub(filename2, pattern = "\\.tif", replacement = ".grd")
#       if (!identical(filename2, filename3)) {
#         warning(".tif format does not preserve factor levels using rgdal. Using ",
#                 filename3, " to preserve levels, instead of ", filename2)
#         filename2 <- filename3
#       }
#     }
#     # There is a weird thing that doing a writeRaster changes the digest of the file, even
#     #   when the object is identical, confirmed by loading each into R, and comparing everything
#     # So, skip that writeRaster if it is already a file-backed Raster, and just copy it
#     #    ERROR ALERT -- You can't change the dataType this way, so you will need to
#     #    go the writeRaster route if dots$datatype is passed and it isn't equal to dataType(x)
#     if (fromDisk(x) && all(dots$datatype == dataType(x))) {
#       theFilename <- Filenames(x, allowMultiple = FALSE)
#       if (fileExt(theFilename) == "grd") {
#         if (!fileExt(filename2) == "grd") {
#           if (fileExt(filename2) != ""){
#             warning("filename2 file type (", fileExt(filename2), ") was not same type (",
#                     fileExt(filename(x)),") ", "as the filename of the raster; ",
#                     "Changing filename2 so that it is ", fileExt(filename(x)))
#             # ^^ This doesn't Work for rasterStack
#             filename2 <- gsub(fileExt(filename2), "grd", filename2)
#           } else {
#             if (!is(x, "Raster")) {
#               stop("The object has no file extension and is not a Raster* class object. Please debug")
#               # Catch for weird cases where the filename is not present and the
#               # object is NOT a RasterStack
#             }
#             if (!identical(fileExt(filename(x[[1]])), fileExt(theFilename))) {
#               warning("filetype of filename2 provided (", fileExt(filename2),") does not ",
#                       "match the filetype of the object; ",
#                       "Changing filename2 so that it is ", fileExt(filename(x[[1]])))
#             }
#             filename2 <- paste0(filename2, ".grd")
#           }
#         }
#         theFilenameGri <- gsub("grd$", "gri", theFilename)
#         filename2Gri <- gsub("grd$", "gri", filename2)
#         if (file.exists(filename2Gri)) {
#           if (isTRUE(overwrite))
#             unlink(filename2Gri)
#         }
#         out <- hardLinkOrCopy(theFilenameGri, filename2Gri)
#
#         # out <- suppressWarningsSpecific(file.link(theFilenameGri, filename2Gri),
#         #                                 falseWarnings = "already exists|Invalid cross-device")
#         # # out <- suppressWarnings(file.link(theFilenameGri, filename2Gri))
#         # if (any(!out)) {
#         #   out <- file.copy(theFilenameGri[!out], filename2Gri[!out],
#         #                    overwrite = overwrite)
#         #
#         # }
#       }
#
#       if (file.exists(filename2)) {
#         if (isTRUE(overwrite))
#           unlink(filename2)
#       }
#       out <- hardLinkOrCopy(theFilename, filename2)
#       # out <- suppressWarningsSpecific(file.link(theFilename, filename2),
#       #                                 falseWarnings = "already exists|Invalid cross-device")
#       # # out <- suppressWarnings(file.link(theFilename, filename2))
#       # if (any(!out)) {
#       #   out <- file.copy(theFilename[!out], filename2[!out],
#       #                    overwrite = overwrite)
#       #
#       # }
#       x <- updateFilenameSlots(x, curFilenames = theFilename, newFilenames = filename2)
#       # if (any(dots$datatype != dataType(x))) {
#       #   if (is(x, "RasterStack")) {
#       #     newDT <- if (length(dots$datatype) == 1) {
#       #       rep(dots$datatype, nlayers2(x))
#       #     } else {
#       #       dots$datatype
#       #     }
#       #     for (ln in seq(names(x)))
#       #       dataType(x[[ln]]) <- newDT[ln]
#       #   } else {
#       #     dataType(x) <- dots$datatype
#       #   }
#       # }
#     } else {
#       argsForWrite <- append(list(filename = filename2, overwrite = overwrite), dots)
#       if (is(x, "RasterStack")) {
#         longerThanOne <- unlist(lapply(argsForWrite, function(x) length(unique(x)) > 1))
#         nLayers <- nlayers2(x)
#         if (any(unlist(longerThanOne))) {
#           if (!identical(nLayers, length(argsForWrite$filename))) {
#             argsForWrite$filename <- file.path(dirname(argsForWrite$filename),
#                                                paste0(names(x), "_", basename(argsForWrite$filename)))
#           }
#         }
#         if (length(argsForWrite$filename) == 1) {
#           argsForWrite <- lapply(argsForWrite, function(x) x[1])
#           xTmp <- do.call(writeRaster, args = c(x = x, argsForWrite))
#           names(xTmp) <- names(x)
#           # messagePrepInputs("Object was a RasterStack; only one filename provided so returning a RasterBrick;", verbose = verbose)
#           # messagePrepInputs("  layer names will likely be wrong.", verbose = verbose)
#         } else if (length(argsForWrite$filename) == nLayers) {
#           dups <- duplicated(argsForWrite$filename)
#           if (any(dups)) {
#             a <- argsForWrite$filename
#             out <- unlist(lapply(seq_along(a), function(ind) {
#               if (ind == 1)
#                 a[[1]] <<- file.path(dirname(a[[1]]), nextNumericName(basename(a[[1]])))
#               else
#                 a[[ind]] <<- file.path(dirname(a[[ind]]), basename(nextNumericName(basename(a[[ind - 1]]))))
#             }))
#             argsForWrite$filename <- out
#           }
#           argsForWrite[!longerThanOne] <- lapply(argsForWrite[!longerThanOne], function(x) rep(x, nLayers))
#           xTmp <- lapply(seq_len(nLayers), function(ind) {
#             inside <- progressBarCode(do.call(writeRaster, args = c(x = x[[ind]], lapply(argsForWrite, function(y) y[ind]))),
#                                       doProgress = ncell(x) > 2e6,
#                                       message = c("Writing ", argsForWrite$filename[ind], " to disk ..."),
#                                       colour = getOption("reproducible.messageColourPrepInputs"),
#                                       verbose = verbose)
#             names(inside) <- names(x)[ind]
#             inside
#           })
#
#         } else {
#           stop("filename2 must be length 1 or length nlayers(...) or nlyr(...)")
#         }
#         xTmp <- raster::stack(xTmp)
#       } else {
#         if (file.exists(argsForWrite$filename)) {
#           if (interactive() && isFALSE(argsForWrite$overwrite)) {
#             wantOverwrite <- readline(paste0("File ", argsForWrite$filename, " already exists; overwrite? Y or N: "))
#             if (identical(tolower(wantOverwrite), "y"))
#               argsForWrite$overwrite <- TRUE
#           }
#         }
#         origColors <- checkColors(x)
#         xTmp <- progressBarCode(do.call(writeRaster, args = c(x = x, argsForWrite)),
#                                 doProgress = ncell(x) > 2e6,
#                                 message = c("Writing ", argsForWrite$filename, " to disk ..."),
#                                 colour = getOption("reproducible.messageColourPrepInputs"),
#                                 verbose = verbose)
#
#         xTmp <- rebuildColors(xTmp, origColors)
#       }
#       #Before changing to do.call, dots were not being added.
#       # This is a bug in writeRaster was spotted with crs of xTmp became
#       # +proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs
#       # should have stayed at
#       # +proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0
#       if (!identical(.crs(xTmp), .crs(x)))
#         crs(xTmp) <- .crs(x)
#
#       x <- xTmp
#     }
#   }
#   x
# }

# @rdname deprecated
# writeOutputs.Spatial <- function(x, filename2 = NULL,
#                                  overwrite = getOption("reproducible.overwrite", TRUE),
#                                  ...) {
#   writeOutputs(sf::st_as_sf(x), filename2 = filename2, overwrite = overwrite)
#
#   # if (!is.null(filename2)) {
#   #   if (!grepl(".shp$", raster::extension(filename2))) {
#   #     filename2 <- paste0(filename2, ".shp")
#   #   }
#   #
#   #   dots <- list(...)
#   #   notWanted1 <- .formalsNotInCurrentDots(shapefile, ...)
#   #   formalNamesIn_rgdal_writeOGR <- c("obj", "dsn", "layer", "driver", "dataset_options", "layer_options",
#   #                "verbose", "check_exists", "overwrite_layer", "delete_dsn", "morphToESRI",
#   #                "encoding", "shp_edge_case_fix", "dumpSRS")
#   #   notWanted2 <- .formalsNotInCurrentDots(formalNames = formalNamesIn_rgdal_writeOGR, ...)
#   #   keepForDots <- c(setdiff(notWanted1, notWanted2), setdiff(names(dots), notWanted1))
#   #   dots <- dots[keepForDots]
#   #   # Internally in rgdal::writeOGR, it converts the row.names to integer with this test
#   #   #   it creates a warning there, so capture here instead
#   #   warn <- captureWarningsToAttr(as.integer(row.names(x)))
#   #   if (isTRUE(any(grepl("NAs introduced by coercion", attr(warn, "warning")))))
#   #     row.names(x) <- as.character(seq_along(row.names(x)))
#   #   do.call(sf::st_write, append(dots, list(obj = sf::st_as_sf(x), dsn = filename2, overwrite = overwrite)))
#   # }
#   x
# }

# @rdname deprecated
# writeOutputs.sf <- function(x, filename2 = NULL,
#                             overwrite = getOption("reproducible.overwrite", FALSE),
#                             verbose = getOption("reproducible.verbose", 1),
#                             ...) {
#   .requireNamespace("sf", stopOnFALSE = TRUE)
#   if (!is.null(filename2)) {
#     messagePrepInputs("    writing to disk", verbose = verbose, verboseLevel = 0)
#     # if (!nzchar(fileExt(filename2))) {
#     #   filename2 <- paste0(filename2, ".shp")
#     # }
#     if (!grepl(".shp$", raster::extension(filename2))) {
#       filename2 <- paste0(filename2, ".shp")
#     }
#     if (identical(".", dirname(filename2))) {
#       filename2 <- normPath(filename2)
#     }
#     dp <- list(...)$destinationPath
#     if (!is.null(dp)) {
#       if (!isTRUE(grepl(normPath(dp), filename2)))
#         filename2 <- normPath(file.path(dp, basename(filename2)))
#     }
#     if (!all(file.exists(filename2)))
#       overwrite = FALSE
#     muffld <- capture.output(
#       sf::st_write(obj = x, dsn = filename2, delete_dsn = overwrite)
#     )
#   }
#   x
# }

# @rdname deprecated
# writeOutputs.quosure <- function(x, filename2, ...) {
#   writeOutputs(eval_tidy(x), filename2 = filename2, ...)
# }

#' @rdname deprecated
writeOutputs.default <- function(x, filename2,
                                 overwrite = getOption("reproducible.overwrite", FALSE),
                                 verbose = getOption("reproducible.verbose", 1),
                                 ...) {

  writeTo(x, ...)
  # if (is(x, "SpatRaster")) {
  #   if (.requireNamespace("terra"))
  #     x <- terra::writeRaster(x, filename = filename2, overwrite = overwrite, ...)
  # } else if (is(x, "SpatVector")) {
  #   if (.requireNamespace("terra"))
  #     x <- terra::writeVector(x, filename = filename2, overwrite = overwrite, ...)
  # } else {
  #   stop("Don't know how to write object of class ", class(x), " on disk.")
  # }
  # return(x)
  #
}

#' Assess the appropriate raster layer data type
#'
#' When writing raster-type objects to disk, a `datatype` can be specified. These
#' functions help identify what smallest `datatype` can be used.
#'
#' @param ras  The `RasterLayer` or `RasterStack` for which data type will be assessed.
#' @param type Character. `"writeRaster"` (default) or `"GDAL"` to return the recommended
#'             data type for writing from the raster packages, respectively, or
#'             `"projectRaster"` to return recommended resampling type.
#'
#' @export
#' @return A character string indicating the data type of the spatial layer
#' (e.g., "INT2U"). See [terra::datatype()] or [raster::dataType()]
#' @rdname assessDataType
#'
#' @example inst/examples/example_assessDataType.R
assessDataType <- function(ras, type = 'writeRaster') {
  UseMethod("assessDataType")
}

#' @export
#' @importFrom terra ncell
#' @rdname assessDataType
assessDataType.default <- function(ras, type = "writeRaster") {
  ## using ras@data@... is faster, but won't work for @values in large rasters

  if (inherits(ras, "RasterStack")) {
    xs <- lapply(names(ras), FUN = function(x){
      y <- assessDataType(ras = ras[[x]], type)
      return(y)})

    return(unlist(xs))
  }

  N <- 1e5

  if (nlayers2(ras) > 1) { # for RasterStack, RasterBrick, SpatRaster of nlyr > 1
    xs <- lapply(seq(names(ras)), FUN = function(x){ # can't use names(ras) directly because can't have same name 2x in lapply, but can in SpatRaster
      y <- assessDataType(ras = ras[[x]], type)
      return(y)})

    return(unlist(xs))
  }

  # browser(expr = exists("._assessDataType_1"))
  datatype <- NULL
  if (ncell(ras) > 1e8) { # for very large rasters, try a different way
    maxValCurrent <- maxFn(ras)
    ras <- setMinMaxIfNeeded(ras)
    # if (maxValCurrent != maxValue(ras))
    datatype <- dataType2(ras)
  } else {
    ras <- setMinMaxIfNeeded(ras)
  }

  if (is.null(datatype)) {

    if (ncell(ras) > N) {
      rasVals <- tryCatch(suppressWarnings(raster::sampleRandom(x = ras, size = N)),
                          error = function(x) rep(NA_integer_, N))
    } else {
      rasVals <- values2(ras)
    }
    minVal <- minFn(ras) # min(ras@data@min)
    maxVal <- maxFn(ras) # max(ras@data@max)
    signVal <- minVal < 0
    doubVal <-  any(floor(rasVals) != rasVals, na.rm = TRUE)  ## faster than any(x %% 1 != 0)
    datatype <- if (doubVal) {
      names(MinValsFlts)[min(which(minVal >= MinValsFlts & maxVal <= MaxValsFlts))]
    } else {
      ## only check for binary if there are no decimals and no signs
      logi <- all(!is.na(.bincode(rasVals[!is.na(rasVals)], c(-1,1))))  ## range needs to include 0
      #logi <- all(!is.na(.bincode(na.omit(rasVals), c(-1,1))))  ## range needs to include 0
      if (logi) {
        "LOG1S"
      } else {
        names(MinVals)[min(which(minVal >= unlist(MinVals) & maxVal <= unlist(MaxVals)))]
      }
    }
  }
  #convert datatype if needed
  datatype <- switchDataTypes(datatype, type = type)
  datatype
}

# # @export
# # @rdname assessDataType
# assessDataType.RasterStack <- function(ras, type = "writeRaster") {
#
#   xs <- lapply(names(ras), FUN = function(x){
#     y <- assessDataType(ras = ras[[x]], type)
#     return(y)})
#
#   return(unlist(xs))
# }


# `assessDataTypeGDAL` assesses the appropriate raster layer data type for GDAL.
# It is a convenience function around `assessDataType(ras, type = "GDAL")`
#
# @param ras  The RasterLayer or RasterStack for which data type will be assessed.
# @author Eliot McIntire, Ceres Barros, Ian Eddy, and Tati Micheletti
# @export
# @rdname assessDataType
# assessDataTypeGDAL <- function(ras) {
#   assessDataType(ras, type = "GDAL")
# }

# @importFrom rlang eval_tidy
# postProcessChecks <- function(studyArea, rasterToMatch, dots,
#                               verbose = getOption("reproducible.verbose", 1)) {
#   if (!is.null(studyArea) & !is(studyArea, "Spatial")) {
#     if (!is.null(studyArea) & !is(studyArea, "sf")) {
#       stop(.msg$studyArea_Spatial)
#     }
#   }
#
#   if (!is.null(rasterToMatch) & !is(rasterToMatch, "RasterLayer")) {
#     stop(.msg$rasterToMatch_Raster)
#   }
#
#   filename1 <- NULL
#   if (!is.null(dots$inputFilePath))  {
#     messagePrepInputs("inputFilePath is being deprecated; use filename1", verbose = verbose)
#     filename1 <- dots$inputFilePath
#     dots$inputFilePath <- NULL
#   }
#
#   if (!is.null(dots$targetFilePath))  {
#     messagePrepInputs("targetFilePath is being deprecated; use filename1.", verbose = verbose)
#     filename1 <- dots$targetFilePath
#     dots$targetFilePath <- NULL
#   }
#   list(dots = dots, filename1 = filename1)
# }

#' @importFrom sp wkt
postProcessAllSpatial <- function(x, studyArea, rasterToMatch,
                                  useCache = getOption("reproducible.useCache", FALSE),
                                  filename1,
                                  filename2, useSAcrs, overwrite, targetCRS = NULL,
                                  useGDAL = getOption("reproducible.useGDAL", FALSE),
                                  cores = getOption("reproducible.GDALcores", 2),
                                  verbose = getOption("reproducible.verbose", 1),
                                  ...) {

  out <- postProcessTerra(x, ...)
  return(out)
  # dots <- list(...)
  # # browser(expr = exists("._postProcessAllSpatial_1"))
  # testValidity <- TRUE
  #
  # if (!is.null(studyArea))
  #   if (is(studyArea, "quosure"))
  #     studyArea <- rlang::eval_tidy(studyArea)
  #
  # if (!is.null(rasterToMatch))
  #   if (is(rasterToMatch, "quosure"))
  #     rasterToMatch <- rlang::eval_tidy(rasterToMatch)
  #
  # extraDots <- postProcessChecks(studyArea = studyArea, rasterToMatch = rasterToMatch, dots = dots)
  # dots <- extraDots$dots
  # if (!is.null(extraDots$filename1))
  #   filename1 <- extraDots$filename1
  #
  # if (!is.null(studyArea) || !is.null(rasterToMatch) || !is.null(targetCRS)) {
  #   # fix errors if methods available
  #   skipCacheMess <- "useCache is FALSE, skipping Cache"
  #   skipCacheMess2 <- "No cachePath supplied"
  #
  #   ##################################
  #   # cropInputs
  #   ##################################
  #   if (!is.null(rasterToMatch)) {
  #     extRTM <- extent(rasterToMatch)
  #     crsRTM <- .crs(rasterToMatch)
  #   } else {
  #     extRTM <- NULL
  #     crsRTM <- NULL
  #   }
  #   useBuffer <- FALSE
  #   bufferSA <- FALSE
  #
  #   if (is(x, "Raster")) {
  #     #if all CRS are projected, then check if buffer is necessary
  #     objsAreProjected <- list(x, studyArea, crsRTM)
  #     nonNulls <- !unlist(lapply(objsAreProjected, is.null))
  #     suppressWarningsSpecific({
  #       projections <- sapply(objsAreProjected[nonNulls],
  #                             # function(xx) grepl("(longitude).*(latitude)",
  #                             #                    tryCatch(wkt(xx), error = function(yy) NULL))))
  #                             function(xx) !isProjected(xx))
  #     }, falseWarnings = "wkt|CRS object has no comment")
  #
  #     if (!any(unlist(projections))) {
  #       if (is.null(rasterToMatch) || max(res(rasterToMatch)) < min(res(x))) {
  #         useBuffer <- TRUE
  #       }
  #     }
  #   }
  #
  #   if (useBuffer) {
  #     #replace extentRTM and crsRTM, because they will supersede all arguments
  #     #if (!requireNamespace("rgeos", quietly = TRUE)) stop(messageRgeosMissing)
  #     if (!is.null(rasterToMatch)) {
  #       #reproject rasterToMatch, extend by res
  #       newExtent <- suppressWarningsSpecific(projectExtent(rasterToMatch, crs = .crs(x)), projNotWKT2warn)
  #       tempPoly <- terra::vect(as(extent(newExtent), "SpatialPolygons"))
  #       crs(tempPoly) <- sp::wkt(x)
  #       #buffer the new polygon by 1.5 the resolution of X so edges aren't cropped out
  #       tempPoly <- as(terra::buffer(tempPoly, width = max(res(x))*1.5), "Spatial")
  #       extRTM <- tempPoly
  #       crsRTM <- suppressWarningsSpecific(falseWarnings = "CRS object has comment", .crs(tempPoly))
  #     } else {
  #       bufferSA <- TRUE
  #       origStudyArea <- studyArea
  #       bufferWidth <- max(res(x)) * 1.5
  #       crsX <- .crs(x)
  #       if (!is(studyArea, "sf")) {
  #         studyArea <- sp::spTransform(studyArea, CRSobj = crsX)
  #         studyArea <- terra::vect(studyArea)
  #         studyArea <- terra::buffer(studyArea, width = bufferWidth)
  #         studyArea <- as(studyArea, "Spatial")
  #       } else {
  #         .requireNamespace("sf", stopOnFALSE = TRUE)
  #         studyArea <- sf::st_transform(studyArea, crs = crsX)
  #         studyArea <- sf::st_buffer(studyArea, dist = bufferWidth)
  #       }
  #     }
  #   }
  #
  #   if (is.null(rasterToMatch) && !is.null(studyArea) && (is(x, "Spatial") || is(x, "sf")) &&
  #       getOption("reproducible.polygonShortcut", FALSE)) {
  #     message("Using an experimental shortcut of maskInputs for special, simple case that x is polygon, ",
  #             "rasterToMatch not provided, and studyArea provided.",
  #             " If this is causing problems, set options(reproducible.polygonShortcut = FALSE)")
  #     x <- fixErrors(x = x, useCache = useCache, verbose = verbose,
  #                    testValidity = testValidity, ...)
  #     x <- Cache(maskInputs(x = x, studyArea = studyArea, ...),
  #                useCache = useCache, verbose = verbose)
  #     x <- fixErrors(x = x, useCache = useCache, verbose = verbose,
  #                    testValidity = testValidity, ...)
  #   } else {
  #     if (!isTRUE(all.equal(extent(x), extRTM))) {
  #       useCacheOrig <- useCache
  #       useCache <- FALSE
  #       x <- Cache(cropInputs(x = x, studyArea = studyArea,
  #                             extentToMatch = extRTM,
  #                             extentCRS = crsRTM,
  #                             useGDAL = useGDAL, ...), useCache = useCache, verbose = verbose)
  #       useCache <- useCacheOrig
  #       testValidity <- NA # Crop will have done it
  #     } else {
  #       messageCache("  Skipping cropInputs; already same extents")
  #     }
  #
  #     if (bufferSA) {
  #       studyArea <- origStudyArea
  #     }
  #
  #     # cropInputs may have returned NULL if they don't overlap
  #     # browser(expr = exists("._postProcess.spatialClasses_3"))
  #     if (!is.null(x)) {
  #       objectName <- if (is.null(filename1)) NULL else basename(filename1)
  #       # x <- fixErrors(x = x, objectName = objectName,
  #       #                useCache = useCache, verbose = verbose,
  #       #                testValidity = testValidity, ...)
  #
  #       ##################################
  #       # projectInputs
  #       ##################################
  #       targetCRS <- .getTargetCRS(useSAcrs, studyArea, rasterToMatch, targetCRS)
  #
  #       runIt <- if (is(x, "Raster") && !is.null(rasterToMatch))
  #         differentRasters(x, rasterToMatch, targetCRS)
  #       else
  #         TRUE
  #       if (runIt) {
  #         x <- retry(retries = 2, silent = FALSE, exponentialDecayBase = 1,
  #                    expr = quote(
  #                      Cache(projectInputs(x = x, targetCRS = targetCRS,
  #                                          rasterToMatch = rasterToMatch,
  #                                          cores = cores, useGDAL = useGDAL, ...),
  #                            useCache = useCache, verbose = verbose)
  #                    ),
  #                    exprBetween = quote(
  #                      x <- fixErrors(x, objectName = objectName,
  #                                     testValidity = NA, useCache = useCache)
  #                    ))
  #       } else {
  #         messageCache("  Skipping projectInputs; identical crs, res, extent")
  #       }
  #
  #       ##################################
  #       # maskInputs
  #       ##################################
  #       yy <- retry(retries = 2, silent = FALSE, exponentialDecayBase = 1,
  #                   expr = quote(
  #                     maskInputs(x = x, studyArea = studyArea,
  #                                rasterToMatch = rasterToMatch, useCache = useCache,
  #                                verbose = verbose, useGDAL = useGDAL, ...)
  #                   ),
  #                   exprBetween = quote(
  #                     x <- fixErrors(x, objectName = objectName,
  #                                    testValidity = NA, useCache = useCache)
  #                   ))
  #       x <- yy
  #     }
  #   }
  #   ##################################
  #   # filename
  #   ##################################
  #   newFilename <- determineFilename(filename1 = filename1, filename2 = filename2, verbose = verbose, ...)
  #
  #   ##################################
  #   # writeOutputs
  #   ##################################
  #   if (!is.null(filename2)) {
  #     x <- suppressWarningsSpecific(
  #       do.call(writeOutputs, append(list(x = rlang::quo(x),
  #                                         filename2 = normPath(newFilename),
  #                                         overwrite = overwrite,
  #                                         verbose = verbose), dots)),
  #       proj6Warn)
  #   } else {
  #     messageCache("  Skipping writeOutputs; filename2 is NULL")
  #   }
  #
  #   # browser(expr = exists("._postProcess.spatialClasses_6"))
  #   if (dir.exists(bigRastersTmpFolder())) {
  #     ## Delete gdalwarp results in temp
  #     unlink(bigRastersTmpFolder(), recursive = TRUE)
  #   }
  #   #}
  # }
  # x
}

useETM <- function(extentToMatch, extentCRS, verbose) {
  passingExtents <- sum(!is.null(extentToMatch), !is.null(extentCRS))
  if (passingExtents == 1) {
    messagePrepInputs(paste("When passing extentToMatch, you must also pass extentCRS;",
                            "using rasterToMatch or studyArea instead"),
                      verbose = verbose)
  } else if (passingExtents == 2) {
    return(TRUE)
  }
  return(FALSE)
}

bufferWarningSuppress <- function(# warn,
  objectName,
  x1, bufferFn, verbose = getOption("reproducible.verbose", 1)) {
  if (is(x1, "try-error")) {
    messagePrepInputs("There are errors with ", objectName,
                      ". Couldn't fix them with ", bufferFn, "(..., width = 0)", verbose = verbose)
  } else {
    messagePrepInputs("  Some or all of the errors fixed.", verbose = verbose, verboseLevel = 1)
  }
  x1
}

roundToRes <- function(extent, x) {
  if (is(x, "Raster"))
    extent <- raster::extent(
      c(round(c(xmin(extent), xmax(extent))/res(x)[1],0)*res(x)[1],
        round(c(ymin(extent), ymax(extent))/res(x)[2],0)*res(x)[2]))
  extent
}

setMinMaxIfNeeded <- function(ras) {
  # special case where the colours already match the discrete values
  suppressWarnings(maxValCurrent <- maxFn(ras))
  needSetMinMax <- FALSE
  if (isTRUE(any(is.na(maxValCurrent)))) {
    needSetMinMax <- TRUE
  } else {

    # if the colors are set and are the same length of the integer sequence between min and max, don't override
    if (length(.getColors(ras)[[1]])) {
      if (!is.na(suppressWarnings(maxFn(ras))) && !is.na(suppressWarnings(minFn(ras))))
        if (length(.getColors(ras)[[1]]) == (maxFn(ras) - minFn(ras) + 1)) {
          return(ras)
        }
    }
    possibleShortCut <- maxValCurrent %in% c(unlist(MaxVals), unlist(MaxVals) + 1)
    if (isTRUE(all(possibleShortCut))) {
      needSetMinMax <- TRUE
    }
  }
  if (isTRUE(needSetMinMax)) {
    large <- if (nlayers2(ras) > 25 || ncell(ras) > 1e7) TRUE else FALSE
    if (large) message("  Large ",class(ras), " detected; setting minimum and maximum may take time")
    suppressWarnings(ras <- setMinMax(ras))
    if (large) message("  ... Done")
  }
  ras
}

differentRasters <- function(ras1, ras2, targetCRS) {

  (!isTRUE(all.equal(.crs(ras1), targetCRS)) |
     !isTRUE(all.equal(res(ras1), res(ras2))) |
     !isTRUE(all.equal(extent(ras1), extent(ras2))))
}

roundTo6Dec <- function(x) {
  # check if integer
  if (all(x %% 1 != 0)) {
    # First test whether they are remotely close to each other
    rounded <- round(x,6)
    if (!identical(x, rounded)) {
      x <- rounded
    }
  }
  x
}

#' @importFrom utils capture.output
suppressWarningsSpecific <- function(code, falseWarnings, verbose = getOption("reproducible.verbose", 1)) {
  warns <- list()
  suppressWarnings(withCallingHandlers({
    yy <- eval(code)
  },
  warning = function(xx) {
    trueWarnings <- grep(falseWarnings, xx$message, invert = TRUE, value = TRUE)
    if (length(trueWarnings)) {
      warns <<- paste(trueWarnings, collapse = "\n  ")
    }
  },
  error = function(xx) stop(xx$message),
  message = function(xx) xx))
  if (length(warns)) {
    lapply(warns, warning)
  }

  return(yy)
}

#' @importFrom utils capture.output
captureWarningsToAttr <- function(code, verbose = getOption("reproducible.verbose", 1)) {
  warn <- capture.output(type = "message",
                         suppressWarnings(withCallingHandlers({
                           yy <- eval(code)
                         }, warning = function(xx) {
                           messagePrepInputs(paste0("warn::", xx$messagePrepInputs), verbose = verbose)
                         })))
  trueWarnings <- grepl("warn::.*", warn)
  if (length(warn[!trueWarnings]))
    messagePrepInputs(paste(warn[!trueWarnings], collapse = "\n  "))
  warn <- gsub("warn::", "", warn[trueWarnings])
  attr(yy, "warning") <- paste(warn, collapse = "\n")
  return(yy)
}

dtp <- list()
dtp[["INT1"]] <- 255/2
dtp[["INT2"]] <- 65534/2
dtp[["INT4"]] <- 4294967296/2
dtp[["FLT4"]] <- 3.4e+38
dtp[["FLT8"]] <- Inf
#INT4S <- MaxVals$INT4U/2 - 1
#MaxVals$INT2S <- INT2UMax
#MaxVals$INT1S <- floor(INT1UMax/2) # 127
#ero <- 0
dtps <- c("INT1U", "INT1S", "INT2U", "INT2S", "INT4U", "INT4S", "FLT4S", "FLT8S")
names(dtps) <- dtps
datatypeVals <- lapply(dtps, function(namdtp) {
  d <- dtp[grep(substr(namdtp, 1, 4), names(dtp), value = TRUE)]
  div <- substr(namdtp, 5, 5)
  mult <- ifelse(div == "U", 2, 1)
  Max <- trunc(unlist(d)*mult)
  sign1 <- ifelse(div == "U", 0, -1)
  Min <- Max * sign1
  list(Min = Min, Max = Max)
})
MaxVals <- lapply(datatypeVals, function(x) unname(x$Max))
MinVals <- lapply(datatypeVals, function(x) unname(x$Min))
MinValsFlts <- MinVals[grep("FLT", names(MinVals), value = TRUE)]
MaxValsFlts <- MaxVals[grep("FLT", names(MinVals), value = TRUE)]
# , envir = asNamespace("reproducible")
#MaxVals <- c(INT1UMax, MaxVals$INT1S, INT2UMax, MaxVals$INT2S, MaxVals$INT4U, MaxVals$INT4S, MaxVals$FLT4S)
#MinVals <- c(0, -MaxVals$INT1S, 0, -MaxVals$INT2S, 0, -MaxVals$INT4S, -MaxVals$FLT4S)

projNotWKT2warn <- "Using PROJ not WKT2"

# specialBuffer <- function(studyArea, x) {
#   studyAreaExt2 <- extent(studyArea)
#   newXMin <- xmin(studyAreaExt2)
#   xMisAlignMin <- ((xmin(studyAreaExt2) - origin(x)[1]) %% res(x)[1])
#   if (xMisAlignMin < res(x)[1]/2) {
#     newXMin <- newXMin - xMisAlignMin
#   }
#
#   newYMin <- ymin(studyAreaExt2)
#   yMisAlignMin <- ((ymin(studyAreaExt2) - origin(x)[2]) %% res(x)[2])
#   if (yMisAlignMin < res(x)[2]/2) {
#     newYMin <- newYMin - yMisAlignMin
#   }
#
#   newXMax <- xmax(studyAreaExt2)
#   xMisAlignMax <- (res(x)[1] - ((xmax(studyAreaExt2) - origin(x)[1]) %% res(x)[1]))
#   if (xMisAlignMax < res(x)[1]/2) {
#     newXMax <- newXMax + xMisAlignMax
#   }
#
#   newYMax <- ymax(studyAreaExt2)
#   yMisAlignMax <- (res(x)[2] - ((ymax(studyAreaExt2) - origin(x)[2]) %% res(x)[2]))
#   if (yMisAlignMax < res(x)[2]/2) {
#     newYMax <- ymax(studyAreaExt2) + yMisAlignMax
#   }
#
#   newExtent <- extent(newXMin, newXMax, newYMin, newYMax)
#   studyArea2 <- as(newExtent, "SpatialPolygons")
#   crs(studyArea2) <- crs(studyArea)
#   studyArea2
# }



isLongLat <- function(targCRS, srcCRS = targCRS) {
  if (grepl("longlat", targCRS)) !grepl("longlat", srcCRS) else FALSE
}

.crs <- function(x, ...) {
  a <- suppressWarningsSpecific(falseWarnings = "CRS object has comment",
                           raster::crs(x, ...))
}

progressBarCode <- function(..., doProgress = TRUE, message,
                            colour = getOption("reproducible.messageColourCache"),
                            verbose = getOption("reproducible.verbose"),
                            verboseLevel = 1) {
  messageColoured(message, colour = colour, verbose = verbose, verboseLevel = verboseLevel)
  out <- eval(...)
  if (doProgress) messageColoured("\b Done!", colour = colour, verbose = verbose, verboseLevel = verboseLevel)
  out
}

isProjected <- function(x) {
  if (is(x, "sf")) {
    txt <- sf::st_crs(x)
  } else {
    txt <- suppressWarningsSpecific(falseWarnings = "no wkt comment", wkt(x))
  }

  if (identical(nchar(txt), 0L) || is.null(txt)) {
    txt <- crs(x)
    out <- any(!grepl("(longlat)", txt))
  } else {
    out <- tryCatch(any(!grepl("(longitude).*(latitude)", txt)), error = function(yy) NULL)
  }
  out
}

# messageRgeosMissing <- "Please run install.packages('rgeos') to address minor GIS issues"

switchDataTypes <- function(datatype, type) {
  datatype <- switch(type,
                     GDAL = {
                       switch(datatype,
                              LOG1S = "Byte",
                              INT1S = "Int16",
                              INT2S = "Int16",
                              INT4S = "Int32",
                              INT1U = "Byte",
                              INT2U = "UInt16",
                              INT4U = "UInt32",
                              datatype <- "Float32" #there is no GDAL FLT8S
                       )
                     },
                     projectRaster = {
                       switch(datatype,
                              Float32 = "bilinear",
                              Float64 = "bilinear",
                              datatype <- "ngb"
                       )
                     },
                     writeRaster = datatype,
                     stop("incorrect argument: type must be one of writeRaster, projectRaster, or GDAL")
  )
  return(datatype)
}

