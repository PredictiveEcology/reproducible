#' Generic function to post process objects
#'
#' \if{html}{\figure{lifecycle-maturing.svg}{options: alt="maturing"}}
#'
#' @export
#' @param x  An object of postProcessing, e.g., \code{spatialClasses}.
#'           See individual methods. This can be provided as a
#'           \code{rlang::quosure} or a normal R object.
#' @importFrom utils capture.output
#' @importFrom raster buffer
#' @importFrom sp spTransform
#' @seealso \code{prepInputs}
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

#' Post processing for \code{spatialClasses}
#'
#' The method for \code{spatialClasses} (\code{Raster*} and \code{Spatial*}) will
#' crop, reproject, and mask, in that order.
#' This is a wrapper for \code{\link{cropInputs}}, \code{\link{fixErrors}},
#' \code{\link{projectInputs}}, \code{\link{maskInputs}} and \code{\link{writeOutputs}},
#' with a decent amount of data manipulation between these calls so that the crs match.
#'
#' @section Post processing sequence:
#'
#'   If the \code{rasterToMatch} or \code{studyArea} are passed, then
#'   the following sequence will occur:
#'
#'   \enumerate{
#'     \item Fix errors \code{\link{fixErrors}}. Currently only errors fixed are for
#'            \code{SpatialPolygons} using \code{buffer(..., width = 0)}.
#'     \item Crop using \code{\link{cropInputs}}
#'     \item Project using \code{\link{projectInputs}}
#'     \item Mask using \code{\link{maskInputs}}
#'     \item Determine file name \code{\link{determineFilename}}
#'     \item Write that file name to disk, optionally \code{\link{writeOutputs}}
#'   }
#'
#'   NOTE: checksumming does not occur during the post-processing stage, as
#'   there are no file downloads. To achieve fast results, wrap
#'   \code{prepInputs} with \code{Cache}
#'
#'   NOTE: \code{sf} objects are still very experimental.
#'
#' @inheritParams prepInputs
#'
#' @inheritParams cropInputs
#'
#' @param filename1  Character strings giving the file paths of
#'                   the \emph{input} object (\code{filename1}) \code{filename1}
#'                   is only used for messaging (i.e., the object itself is passed
#'                   in as \code{x}) and possibly naming of output (see details
#'                   and \code{filename2}).
#'
#' @param filename2   \code{filename2} is optional, and is either
#'                   NULL (no writing of outputs to disk), or several options
#'                   for writing the object to disk. If
#'                   \code{TRUE} (the default), it will give it a file name determined by
#'                   \code{.prefix(basename(filename1), prefix)}. If
#'                   a character string, it will use this as its file name. See
#'                   \code{\link{determineFilename}}.
#'
#' @param useSAcrs Logical. If \code{FALSE}, the default, then the desired projection
#'                 will be taken from \code{rasterToMatch} or none at all.
#'                 If \code{TRUE}, it will be taken from \code{studyArea}. See table
#'                 in details below.
#'
#' @param ... Additional arguments passed to methods. For \code{spatialClasses},
#'            these are: \code{\link{cropInputs}}, \code{\link{fixErrors}},
#'            \code{\link{projectInputs}}, \code{\link{maskInputs}},
#'            \code{\link{determineFilename}}, and \code{\link{writeOutputs}}.
#'            Each of these may also pass \code{...} into other functions, like
#'            \code{\link[raster]{writeRaster}}, or \code{sf::st_write}.
#'            This might include potentially important arguments like \code{datatype},
#'            \code{format}. Also passed to \code{projectRaster},
#'            with likely important arguments such as \code{method = "bilinear"}.
#'            See details.
#'
#' \subsection{... passed to:}{
#'   \describe{
#'     \item{\code{cropInputs}:}{\code{\link[raster]{crop}}}
#'     \item{\code{projectInputs}}{\code{\link[raster]{projectRaster}}}
#'     \item{\code{maskInputs}}{\code{\link{fastMask}} or \code{\link[raster]{intersect}}}
#'     \item{\code{fixErrors}}{\code{\link[raster]{buffer}}}
#'     \item{\code{writeOutputs}}{\code{\link[raster]{writeRaster}} or \code{\link[raster]{shapefile}}}
#'     \item{\code{determineFilename}}{}
#'   }
#'   * Can be overridden with \code{useSAcrs}
#'   ** Will mask with \code{NA}s from \code{rasterToMatch} if \code{maskWithRTM}
#' }
#'
#' @section Passing \code{rasterToMatch} and/or \code{studyArea}:
#'
#' Depending on which of these were passed, different things will happen to the
#' \code{targetFile} located at \code{filename1}.
#'
#' \subsection{If \code{targetFile} is a \code{Raster*} object:}{
#'   \tabular{lccc}{
#'                       \tab \code{rasterToMatch} \tab \code{studyArea} \tab             Both \cr
#'     \code{extent}     \tab Yes                  \tab   Yes        \tab \code{rasterToMatch} \cr
#'     \code{resolution} \tab Yes                  \tab   No         \tab \code{rasterToMatch} \cr
#'     \code{projection} \tab Yes                  \tab   No*        \tab \code{rasterToMatch}*\cr
#'     \code{alignment}  \tab Yes                  \tab   No         \tab \code{rasterToMatch} \cr
#'     \code{mask}       \tab No**                 \tab   Yes        \tab \code{studyArea}**   \cr
#'   }
#'   * Can be overridden with \code{useSAcrs}.
#'   ** Will mask with \code{NA}s from \code{rasterToMatch} if \code{maskWithRTM}.
#' }
#'
#' \subsection{If \code{targetFile} is a \code{Spatial*} object:}{
#'   \tabular{lccc}{
#'                       \tab \code{rasterToMatch} \tab \code{studyArea} \tab             Both \cr
#'     \code{extent}     \tab Yes                  \tab   Yes        \tab \code{rasterToMatch} \cr
#'     \code{resolution} \tab NA                   \tab   NA         \tab NA                   \cr
#'     \code{projection} \tab Yes                  \tab   No*        \tab \code{rasterToMatch}*\cr
#'     \code{alignment}  \tab NA                   \tab   NA         \tab NA                   \cr
#'     \code{mask}       \tab No                   \tab   Yes        \tab \code{studyArea}     \cr
#'   }
#'   * Can be overridden with \code{useSAcrs}
#' }
#'
#' @export
#' @importFrom raster removeTmpFiles
#' @example inst/examples/example_postProcess.R
#' @rdname postProcess
postProcess.spatialClasses <- function(x, filename1 = NULL, filename2 = NULL,
                                       studyArea = NULL, rasterToMatch = NULL,
                                       overwrite = getOption("reproducible.overwrite", TRUE),
                                       useSAcrs = FALSE,
                                       useCache = getOption("reproducible.useCache", FALSE),
                                       verbose = getOption("reproducible.verbose", 1),
                                       ...) {

  on.exit(removeTmpFiles(h = 0), add = TRUE)

  # Test if user supplied wrong type of file for "studyArea", "rasterToMatch"
  # browser(expr = exists("._postProcess.spatialClasses_1"))
  x1 <- postProcessAllSpatial(x = x, studyArea = eval_tidy(studyArea),
                              rasterToMatch = eval_tidy(rasterToMatch), useCache = useCache,
                              filename1 = filename1, filename2 = filename2,
                              useSAcrs = useSAcrs, overwrite = overwrite,
                              verbose = verbose, ...)
  return(x1)
}

#' @export
#' @example inst/examples/example_postProcess.R
#' @rdname postProcess
postProcess.sf <- function(x, filename1 = NULL, filename2 = NULL,
                           studyArea = NULL, rasterToMatch = NULL,
                           overwrite = getOption("reproducible.overwrite", TRUE),
                           useSAcrs = FALSE,
                           useCache = getOption("reproducible.useCache", FALSE),
                           verbose = getOption("reproducible.verbose", 1),
                           ...) {
  .requireNamespace("sf", stopOnFALSE = TRUE)

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

  return(x)
}

#' Crop a \code{Spatial*} or \code{Raster*} object
#'
#' This function can be used to crop or reproject module inputs from raw data.
#'
#' @param x A \code{Spatial*}, \code{sf}, or \code{Raster*} object.
#'
#' @param studyArea \code{SpatialPolygons*} object used for masking and possibly cropping
#'                  if no \code{rasterToMatch} is provided.
#'                  If not in same CRS, then it will be \code{spTransform}ed to
#'                  CRS of \code{x} before masking. Currently, this function will not reproject the
#'                  \code{x}. Optional in \code{postProcess}.
#'
#' @param rasterToMatch Template \code{Raster*} object used for cropping (so extent should be
#'                      the extent of desired outcome) and reprojecting (including changing the
#'                      resolution and projection).
#'                      See details in \code{\link{postProcess}}.
#'
#' @param ... Passed to \code{raster::crop}
#'
#' @param useCache Logical, default \code{getOption("reproducible.useCache", FALSE)}, whether
#'                 \code{Cache} is used internally.
#'
#' @inheritParams projectInputs
#'
#' @author Eliot McIntire, Jean Marchal, Ian Eddy, and Tati Micheletti
#' @example inst/examples/example_postProcess.R
#' @export
#' @importFrom methods is
#' @importFrom raster buffer crop crs extent projectRaster res crs<-
#' @importFrom sp SpatialPolygonsDataFrame spTransform CRS proj4string
#' @rdname cropInputs
cropInputs <- function(x, studyArea, rasterToMatch, verbose = getOption("reproducible.verbose", 1), ...) {
  UseMethod("cropInputs")
}

#' @export
#' @rdname cropInputs
cropInputs.default <- function(x, studyArea, rasterToMatch, ...) {
  x
}

#' @param extentToMatch Optional. Can pass an extent here and a \code{crs} to
#'                      \code{extentCRS} instead of \code{rasterToMatch}. These
#'                      will override \code{rasterToMatch}, with a warning if both
#'                      passed.
#' @param extentCRS     Optional. Can pass a \code{crs} here with an extent to
#'                      \code{extentTomatch} instead of \code{rasterToMatch}
#'
#' @export
#' @importFrom raster compareCRS projectExtent tmpDir
#' @rdname cropInputs
cropInputs.spatialClasses <- function(x, studyArea = NULL, rasterToMatch = NULL,
                                      verbose = getOption("reproducible.verbose", 1),
                                      extentToMatch = NULL, extentCRS = NULL,
                                      useGDAL = getOption("reproducible.useGDAL", TRUE),
                                      useCache = getOption("reproducible.useCache", FALSE),
                                      ...) {
  # browser(expr = exists("._cropInputs_1"))
  useExtentToMatch <- useETM(extentToMatch = extentToMatch, extentCRS = extentCRS, verbose = verbose)
  if (!useExtentToMatch) {
    extentToMatch <- NULL
    extentCRS <- NULL
  }
  transformToCRSX <- TRUE
  if (!is.null(studyArea) || !is.null(rasterToMatch) || !is.null(extentToMatch)) {
    isX_Sp <- is(x, "Spatial")
    isX_Sf <- is(x, "sf")
    if (!is.null(extentToMatch)) {
      rasterToMatch <- suppressWarningsSpecific(falseWarnings = "CRS object has comment",
                                                raster(extentToMatch, crs = extentCRS))
    }
    cropTo <- if (!is.null(rasterToMatch)) {
      rasterToMatch
    } else {
      if (is.na(crs(studyArea)))
        stop("studyArea does not have a crs")
      studyArea
    }

    # have to project the extent to the x projection so crop will work -- this is temporary
    #   once cropped, then cropExtent should be rm
    crsX <- .crs(x)
    crsCropTo <- .crs(cropTo)
    if (compareCRS(crsX, crsCropTo)) {
      cropExtent <- extent(cropTo)
    } else {
      if (!is.null(rasterToMatch)) {
        cropExtent <- projectExtent(cropTo, crsX)
      } else {
        isSA_Sp <- is(studyArea, "Spatial")
        isSA_Sf <- is(studyArea, "sf")

        # Here, basically, st_intersection doesn't work correctly on longlat data
        #  So, need to do opposite transformation -- transform X to StudyArea
        if ( (isX_Sp || isX_Sf) && (isSA_Sp || isSA_Sf) ) {
          if (sf::st_is_longlat(crsX))
            transformToCRSX <- FALSE
        }

        if (transformToCRSX) {
          if (isSA_Sp || isSA_Sf) {
            if (isSA_Sp) {
              #theExtent <- as(extent(cropTo), "SpatialPolygons")
              #crs(theExtent) <- crsCropTo
              cropExtent <- raster::extent(spTransform(x = cropTo, CRSobj = crsX))
            } else if (isSA_Sf) {
              .requireNamespace("sf", stopOnFALSE = TRUE)
              cropExtent <- extent(sf::st_transform(cropTo, crs = crsX))
            }
          } else {
            messagePrepInputs("cropInputs must have a rasterToMatch raster, or studyArea Spatial or sf object. ",
                              "Returning result with no cropping.", verbose = verbose)
            cropExtent <- NULL
          }
        } else {
          cropExtent <- extent(cropTo)
          if (isX_Sp) {
            x <- sf::st_as_sf(x)
          }
          x <- sf::st_transform(x, crs = crsCropTo)
        }
      }
    }

    isStack <- is(x, "RasterStack") # will return a RasterBrick -- keep track of this
    isBrick <- is(x, "RasterBrick")
    if (!is.null(cropExtent)) {
      # crop it
      if (!identical(cropExtent, extent(x))) {
        messagePrepInputs("    cropping ...", verbose = verbose, verboseLevel = 0)
        dots <- list(...)
        if (is(x, "sf")) {
          dots[.formalsNotInCurrentDots(sf::st_crop, ..., signature = is(x))] <- NULL
        } else {
          dots[.formalsNotInCurrentDots(raster::crop, ..., signature = is(x))] <- NULL
        }


        needOT <- if (!is.null(dots$datatype)) TRUE else FALSE

        if (is(x, "SpatialPolygonsDataFrame")) {
          if (ncol(x) == 0) {
            x <- as(x, "SpatialPolygons")
            messagePrepInputs("x was a SpatialPolygonsDataFrame with no data; converting to SpatialPolygons object",
                              verbose = verbose)
          }
        }
        # need to double check that gdal executable exists before going down this path
        attemptGDAL <- attemptGDAL(x, useGDAL, verbose = verbose) #!raster::canProcessInMemory(x, n = 3) && isTRUE(useGDAL)

        cropExtentRounded <- roundToRes(cropExtent, x)

        isX_Sp_Int <- is(x, "Spatial")
        isX_Sf_Int <- is(x, "sf")

        if (attemptGDAL && is(x, "Raster") &&
            length(Filenames(x, allowMultiple = FALSE)) <= 1) {
          if (needOT) {
            datatype <- switchDataTypes(unique(dots$datatype)[[1]], "GDAL")
          }
          tmpfile <- paste0(tempfile(fileext = ".tif"))
          wasInMemory <- inMemory(x)
          if (wasInMemory)
            x <- suppressWarningsSpecific(falseWarnings = "NOT UPDATED FOR PROJ",
                                          writeRaster(x, filename = tempfile(fileext = ".tif")))
          # Need to create correct "origin" meaning the 0,0 are same. If we take the
          #   cropExtent directly, we will have the wrong origin if it doesn't align perfectly.
          # "-ot ", dType, # Why is this missing?

           gdalUtilities::gdalwarp(srcfile = Filenames(x, allowMultiple = FALSE),
                              dstfile = tmpfile,
                              tr = c(res(x)[1], res(x)[2]),
                              overwrite = TRUE,
                              s_srs = crsX,
                              t_srs = crsX,
                              if (needOT) ot = datatype,
                              te = c(cropExtentRounded[1], cropExtentRounded[3],
                                     cropExtentRounded[2], cropExtentRounded[4]),
                              te_srs = crsX,
                              tap = TRUE)
          if (isStack) {
            x <- raster::stack(tmpfile)
          } else if (isBrick) {
            x <- raster::brick(tmpfile)
          } else {
            x <- raster(tmpfile)
          }
          if (wasInMemory)
            x[] <- x[]
          x <- setMinMaxIfNeeded(x)

          # paste0(paste0(getOption("gdalUtils_gdalPath")[[1]]$path, "gdalwarp", exe, " "),
          #        "-s_srs \"", as.character(raster::crs(raster::raster(tempSrcRaster))), "\"",
          #        " -t_srs \"", targCRS, "\"",
          #        " -multi ", prll,
          #        "-ot ", dType,
          #        teRas,
          #        "-r ", dots$method,
          #        " -overwrite ",
          #        "-tr ", paste(tr, collapse = " "), " ",
          #        "\"", tempSrcRaster, "\"", " ",
          #        "\"", tempDstRaster, "\""),
        } else if (isX_Sp || isX_Sf) { # raster::crop has stopped working on SpatialPolygons
          yyy <- as(cropExtentRounded, "SpatialPolygons")
          if (transformToCRSX) {
            crs(yyy) <- crsX
          } else {
            crs(yyy) <- crsCropTo
          }

          if (isX_Sp_Int) {
            yy <- retry(retries = 2, silent = FALSE, exponentialDecayBase = 1,
                        expr = quote(
                          sf::st_as_sf(x)
                        ),
                        exprBetween = quote(
                          x <- fixErrors(x, testValidity = NA, useCache = useCache)
                        ))
            x <- yy
          }
          # suppressMessages({
          #   x <- fixErrors(x)
          # })

          yyySF <- retry(retries = 2, silent = FALSE, exponentialDecayBase = 1,
                      expr = quote(
                        sf::st_as_sf(yyy)
                      ),
                      exprBetween = quote(
                        yyy <- fixErrors(yyy, testValidity = NA, useCache = useCache)
                      ))

          # yyySF <- sf::st_as_sf(yyy)
          #
          # suppressMessages({
          #   yyySF <- fixErrors(yyySF)
          # })

          # This tryCatch seems to be finding a bug in st_intersection:
          #   The error was:
          #   Error in geos_op2_geom("intersection", x, y) :
          #      st_crs(x) == st_crs(y) is not TRUE
          #   But the st_crs are identical
          x <- tryCatch(sf::st_intersection(x, yyySF), error = function(xxx) {
            x <- sf::st_transform(x, sf::st_crs(crsX))
            sf::st_intersection(x, yyySF)
          })

          if (!transformToCRSX) {
            x <- sf::st_transform(x, crsX)
          }
          if (NROW(x) == 0)
            stop("    polygons do not intersect.")
          if (isX_Sp)
            x <- as(x, "Spatial")

        } else {
          if (!is.null(dots$datatype)) {
            if (length(dots$datatype) > 1) {
              warning("datatype can only be length 1 for raster::crop. Using first value: ",
                      dots$datatype[1])
              dots$datatype <- dots$datatype[1]
            }
          }
          layerNamesNow <- names(x)
          # Need to assign to "not x" so that retry can do its thing on fail
          yy <- retry(retries = 2, silent = FALSE, exponentialDecayBase = 1,
                      expr = quote(
                        if (canProcessInMemory(x, 3)) {
                          do.call(raster::crop, args = append(list(x = x, y = cropExtentRounded),
                                                              dots))
                        } else {
                          do.call(raster::crop,
                                  args = append(list(x = x, y = cropExtentRounded,
                                                     filename = paste0(tempfile(tmpdir = tmpDir()), ".tif")),
                                                dots))
                        }
                      ),
                      exprBetween = quote(
                        x <- fixErrors(x, testValidity = NA, useCache = useCache)
                      ))
          if (!identical(names(yy), layerNamesNow))
            names(yy) <- layerNamesNow
          x <- yy
        }

        if (is.null(x)) {
          messagePrepInputs("    polygons do not intersect.", verbose = verbose, verboseLevel = 0)
        }
      }
    }
    if (isStack) {
      if (!is(x, "RasterStack"))
        x <- raster::stack(x)
    } else if (isBrick) {
      if (!is(x, "RasterBrick"))
        x <- raster::brick(x)
    }
  }
  return(x)
}

#' @export
#' @importFrom raster compareCRS crs extent projectExtent raster
#' @rdname cropInputs
cropInputs.sf <- function(x, studyArea = NULL, rasterToMatch = NULL,
                          verbose = getOption("reproducible.verbose", 1),
                          extentToMatch = NULL, extentCRS = NULL,
                          useCache = getOption("reproducible.useCache", FALSE),
                          ...) {
  .requireNamespace("sf", stopOnFALSE = TRUE)
  useExtentToMatch <- useETM(extentToMatch = extentToMatch, extentCRS = extentCRS, verbose = verbose)
  if (useExtentToMatch) {
    extentToMatch <- NULL
    extentCRS <- NULL
  }
  messagePrepInputs("cropInputs with sf class objects is still experimental", verbose = verbose)
  if (!is.null(studyArea) || !is.null(rasterToMatch) || !is.null(extentToMatch)) {
    if (!is.null(extentToMatch)) {
      rasterToMatch <- raster(extentToMatch, crs = extentCRS)
    }
    cropTo <- if (!is.null(rasterToMatch)) {
      rasterToMatch
    } else {
      studyArea
    }

    # have to project the extent to the x projection so crop will work -- this is temporary
    #   once cropped, then cropExtent should be rm
    cropExtent <- if (compareCRS(x, cropTo)) {
      extent(cropTo)
    } else {
      if (!is.null(rasterToMatch)) {
        # stop("Can't work with rasterToMatch and sf objects yet in cropInputs")
        projectExtent(cropTo, .crs(x))
      } else {
        if (is(studyArea, "sf")) {
          sf::st_transform(x = cropTo, crs = sf::st_crs(x))
        } else if (is(studyArea, "Spatial")) {
          sf::st_transform(x = sf::st_as_sf(cropTo), crs = sf::st_crs(x))
        } else {
          NULL
        }
      }
    }

    if (!is.null(cropExtent)) {
      # crop it
      if (!identical(cropExtent, extent(x))) {
        messagePrepInputs("    cropping with st_crop ...", verbose = verbose, verboseLevel = 0)
        dots <- list(...)
        dots[.formalsNotInCurrentDots(sf::st_crop, ..., signature = is(x))] <- NULL
        yy <- retry(retries = 2, silent = FALSE, exponentialDecayBase = 1,
                    expr = quote(
                      do.call(sf::st_crop, args = append(list(x = x, y = cropExtent), dots))
                    ),
                    exprBetween = quote(
                      x <- fixErrors(x, testValidity = NA, useCache = useCache)
                    ))
        x <- yy

        if (all(sapply(extent(x), function(xx) is.na(xx)))) {
          messagePrepInputs("    polygons do not intersect.", verbose = verbose)
        }
      }
    }
  }
  return(x)
}

#' Do some minor error fixing
#'
#' These must be very common for this function to be useful. Currently, the only
#' meaningful method is on \code{SpatialPolygons}, and it runs \code{sf::st_is_valid}.
#' If \code{FALSE}, then it runs a buffer of width 0.
#'
#' @param x A \code{SpatialPolygons*} or \code{sf} object.
#'
#' @param objectName Optional. This is only for messaging; if provided, then messages relayed
#'                   to user will mention this.
#'
#' @param attemptErrorFixes Will attempt to fix known errors. Currently only some failures
#'        for \code{SpatialPolygons*} are attempted.
#'        Notably with \code{raster::buffer(..., width = 0)}.
#'        Default \code{TRUE}, though this may not be the right action for all cases.
#' @param useCache Logical, default \code{getOption("reproducible.useCache", FALSE)}, whether
#'                 Cache is used on the internal \code{raster::buffer} command.
#' @param testValidity Logical. If \code{TRUE}, the a test for validity will happen
#'                 before actually running buffering (which is the solution in most
#'                 cases). However, sometimes it takes longer to test for validity
#'                 than just buffer without testing (there are no consequences of
#'                 buffering if everything is valid). If \code{FALSE}, then the
#'                 test will be skipped and the buffering will happen regardless.
#'                 If \code{NA}, then all testing and buffering will be skipped.
#' @param ... Passed to methods. None currently implemented.
#'
#' @export
#' @keywords internal
#'
#' @example inst/examples/example_postProcess.R
fixErrors <- function(x, objectName, attemptErrorFixes = TRUE,
                      useCache = getOption("reproducible.useCache", FALSE),
                      verbose = getOption("reproducible.verbose", 1),
                      testValidity = getOption("reproducible.testValidity", TRUE),
                      ...) {
  UseMethod("fixErrors")
}

#' @export
#' @keywords internal
#' @rdname fixErrors
fixErrors.default <- function(x, objectName, attemptErrorFixes = TRUE,
                              useCache = getOption("reproducible.useCache", FALSE),
                              verbose = getOption("reproducible.verbose", 1),
                              testValidity = getOption("reproducible.testValidity", TRUE),
                              ...) {
  x
}

#' @export
#' @keywords internal
#' @rdname fixErrors
#' @importFrom raster isLonLat origin origin<- xmax<- xmin<- ymax<- ymin<-
fixErrors.Raster <- function(x, objectName, attemptErrorFixes = TRUE,
                             useCache = getOption("reproducible.useCache", FALSE),
                             verbose = getOption("reproducible.verbose", 1),
                             testValidity = getOption("reproducible.testValidity", TRUE),
                             ...) {

  #rounding lon lat resolution will break the raster
  if (!isLonLat(x)) {
    origin(x) <- roundTo6Dec(origin(x))
    xmin(x) <- roundTo6Dec(xmin(x))
    ymin(x) <- roundTo6Dec(ymin(x))
    xmax(x) <- roundTo6Dec(xmax(x))
    ymax(x) <- roundTo6Dec(ymax(x))
    res(x) <- roundTo6Dec(res(x))
  }
  # if (!identical(origin(x), round(origin(x), .Machine$double.eps))) {
  #   roundedOrigin <- round(origin(x),6)
  #   if (identical(origin(x), roundedOrigin))
  #     origin(x) <- roundedOrigin
  # }
  # roundedRes <- round(res(x),6)
  # if (identical(res(x), roundedRes))
  #   res(x) <- roundedRes
  # roundedExtent <- round(extent(x),6)
  # if (identical(extent(x), roundedExtent))
  #   extent(x) <- roundedExtent
  x
}

#' Fix \code{sf::st_is_valid} failures in \code{SpatialPolygons}
#'
#' This uses \code{raster::buffer(..., width = 0)} internally, which fixes some
#' failures to \code{sf::st_is_valid}
#'
#' @export
#' @rdname fixErrors
fixErrors.SpatialPolygons <- function(x, objectName = NULL,
                                      attemptErrorFixes = TRUE,
                                      useCache = getOption("reproducible.useCache", FALSE),
                                      verbose = getOption("reproducible.verbose", 1),
                                      testValidity = getOption("reproducible.testValidity", TRUE),
                                      ...) {
  if (attemptErrorFixes) {
    if (is.null(objectName)) objectName = "SpatialPolygon"
    if (is(x, "SpatialPolygons")) {
      messagePrepInputs("Checking for errors in ", objectName, verbose = verbose)

      runBuffer <- if (requireNamespace("sf", quietly = TRUE) && isTRUE(testValidity)) {
        x1 <- sf::st_as_sf(x)
        anv <- any(!sf::st_is_valid(x1))
        if (isTRUE(anv)) {
          messagePrepInputs("Found errors in ", objectName, ". Attempting to correct.",
                            verbose = verbose)
        }
        anv
      } else if (is.na(testValidity)) {
        FALSE
      } else {
        TRUE
      }

      if (runBuffer) {
        if (!requireNamespace("rgeos", quietly = TRUE)) stop(messageRgeosMissing)
        messagePrepInputs("      Trying the buffer = 0 trick", verbose = verbose, verboseLevel = 2)
        # prevent the warning about not projected, because we are buffering 0, which doesn't matter
        x1 <-
          suppressWarningsSpecific(falseWarnings = paste("Spatial object is not projected;",
                                                         "GEOS expects planar coordinates"),
                                   try(Cache(raster::buffer, x, width = 0, dissolve = FALSE, useCache = useCache))#,
          )

        x <- bufferWarningSuppress(#warn = attr(x1, "warning"),
          objectName = objectName,
          x1 = x1, bufferFn = "raster::buffer")
      } else {
        messagePrepInputs("  Found no errors.", verbose = verbose)
      }
    }
  }
  return(x)
}

#' @export
#' @rdname fixErrors
fixErrors.sf <- function(x, objectName = NULL, attemptErrorFixes = TRUE,
                         useCache = getOption("reproducible.useCache", FALSE),
                         verbose = getOption("reproducible.verbose", 1),
                         testValidity = getOption("reproducible.testValidity", TRUE),
                         ...) {
  .requireNamespace("sf", stopOnFALSE = TRUE)
  if (attemptErrorFixes) {
    if (is.null(objectName))
      objectName <- "SimpleFeature"

    if ((is(sf::st_geometry(x), "sfc_MULTIPOLYGON") || is(sf::st_geometry(x), "sfc_GEOMETRY") ||
         is(sf::st_geometry(x), "sfc_POLYGON")) && !(is(sf::st_geometry(x), "sfc_POINT"))) {
      messagePrepInputs("Checking for errors in ", objectName, verbose = verbose)

      ## sfc_GEOMETRY may itself contain points, so filter them out


      ## BROWSER HERE -- WHY COLLECTION?

      x <- suppressWarnings(sf::st_collection_extract(x, "POLYGON"))

      ## too computationally intensive to buffer everything all the time, so only do for invalid geometries
      runBuffer <- if (isTRUE(testValidity)) {
        suppressWarnings(any(!sf::st_is_valid(x)))
      } else if (is.na(testValidity)) {
        FALSE
      } else {
        TRUE
      }
      if (isTRUE(runBuffer)) {
        messagePrepInputs("Found errors in ", objectName, ". Attempting to correct.",
                          verbose = verbose)

        x1 <- suppressWarningsSpecific(falseWarnings = paste("Spatial object is not projected;",
                                                             "GEOS expects planar coordinates"),
                                       try(Cache(sf::st_buffer, x, dist = 0, useCache = useCache)))
        x <- bufferWarningSuppress(#warn = attr(x1, "warning"),
          objectName = objectName,
          x1 = x1, bufferFn = "sf::st_buffer")

      } else {
        messagePrepInputs("  Found no errors.", verbose = verbose)
      }
    }
  }
  return(x)
}

#' Project \code{Raster*} or {Spatial*} or \code{sf} objects
#'
#' A simple wrapper around the various different tools for these GIS types.
#'
#' @param x A \code{Raster*}, \code{Spatial*} or \code{sf} object
#'
#' @param targetCRS The CRS of x at the end  of this function (i.e., the goal)
#'
#' @param ... Passed to \code{\link[raster]{projectRaster}}.
#'
#' @param rasterToMatch Template \code{Raster*} object passed to the \code{to} argument of
#'                      \code{\link[raster]{projectRaster}}, thus will changing the
#'                      resolution and projection of \code{x}.
#'                      See details in \code{\link{postProcess}}.
#'
#' @param cores An \code{integer*} or \code{'AUTO'}. This will be used if gdalwarp is
#'                      triggered. \code{'AUTO'*} will calculate 90% of the total
#'                      number of cores in the system, while an integer or rounded
#'                      float will be passed as the exact number of cores to be used.
#'
#' @return A file of the same type as starting, but with projection (and possibly
#' other characteristics, including resolution, origin, extent if changed).
#'
#' @export
#' @inheritParams prepInputs
#' @importFrom raster canProcessInMemory
#' @rdname projectInputs
#'
#' @example inst/examples/example_postProcess.R
projectInputs <- function(x, targetCRS, verbose = getOption("reproducible.verbose", 1), ...) {
  UseMethod("projectInputs")
}

#' @export
#' @rdname projectInputs
projectInputs.default <- function(x, targetCRS, ...) {
  x
}

#' @export
#' @rdname projectInputs
#' @param useGDAL Logical or \code{"force"}.
#'     Defaults to \code{getOption("reproducible.useGDAL" = TRUE)}.
#'     If \code{TRUE}, then this function will use \code{gdalwarp} only when not
#'     small enough to fit in memory (i.e., \emph{if the operation fails} the
#'     \code{raster::canProcessInMemory(x, 3)} test). Using \code{gdalwarp} will
#'     usually be faster than \code{raster::projectRaster}, the function used
#'     if this is \code{FALSE}. Since since the two options use different algorithms,
#'     there may be different projection results. \code{"force"} will cause it to
#'     use GDAL regardless of the memory test described here.
#'
#' @importFrom fpCompare %==%
#' @importFrom raster crs dataType res res<- dataType<-
projectInputs.Raster <- function(x, targetCRS = NULL,
                                 verbose = getOption("reproducible.verbose", 1),
                                 rasterToMatch = NULL, cores = NULL,
                                 useGDAL = getOption("reproducible.useGDAL", TRUE),
                                 ...) {
  messagePrepInputs("    reprojecting ...", verbose = verbose, verboseLevel = 0)
  dots <- list(...)
  # browser(expr = exists("._projectInputs_1"))

  isFactorRaster <- FALSE
  isStack <- is(x, "RasterStack")
  if (isTRUE(raster::is.factor(x))) {
    isFactorRaster <- TRUE
    rasterFactorLevels <- raster::levels(x)
  }

  if (is.null(rasterToMatch) && is.null(targetCRS)) {
    messagePrepInputs("     no reprojecting because no rasterToMatch & targetCRS are FALSE (or NULL).",
                      verbose = verbose, verboseLevel = 0)
  } else if (is.null(rasterToMatch) & identical(.crs(x), targetCRS)) {
    messagePrepInputs("    no reprojecting because target CRS is same as input CRS.", verbose = verbose,
                      verboseLevel = 0)
  } else {
    if (is.null(targetCRS)) {
      targetCRS <- .crs(rasterToMatch)
    }
    srcCRS <- .crs(x)

    dontSpecifyResBCLongLat <- isLongLat(targetCRS, srcCRS)

    doProjection <- FALSE
    if (is.null(rasterToMatch)) {
      if (!identical(.crs(x), targetCRS))  doProjection <- TRUE
    } else if (differentRasters(x, rasterToMatch, targetCRS)) {
      doProjection <- TRUE
    }

    if (doProjection) {
      # need to double check that gdal executable exists before going down this path
      attemptGDAL <- attemptGDAL(x, useGDAL, verbose = verbose) #!raster::canProcessInMemory(x, n = 3) && isTRUE(useGDAL)

      if (attemptGDAL) {
        ## the raster is in memory, but large enough to trigger this function: write it to disk
        messagePrepInputs("   large raster: reprojecting after writing to temp drive...",
                          verbose = verbose)
        ## rasters need to go to same file so it can be unlinked at end without losing other temp files
        tmpRasPath <- checkPath(bigRastersTmpFolder(), create = TRUE)
        tempSrcRaster <- bigRastersTmpFile()
        tempDstRaster <- file.path(tmpRasPath, paste0(x@data@names, "a_reproj.tif")) # fails if x = stack

        if (!is.null(rasterToMatch)) {
          tr <- res(rasterToMatch)
        } else {
          tr <- res(x)
        }

        if (isWindows()) {
          exe <- ".exe"
        } else {
          exe <- ""
        }

        if (is.null(dots$method)) {
          dots$method <- assessDataType(x, type = "projectRaster")
        }

        if (dots$method == "ngb") {
          dots$method <- "near"
        }

        if (inMemory(x)) { #must be written to disk
          dType <- assessDataType(x, type = "writeRaster")
          dTypeGDAL <- assessDataType(x, type = "GDAL")
          writeRaster(x, filename = tempSrcRaster, datatype = dType, overwrite = TRUE)
          rm(x) #Saves memory if this was a huge raster, but be careful
          gc()
        } else {
          tempSrcRaster <- x@file@name #Keep original raster
          dTypeGDAL <- assessDataType(raster(tempSrcRaster), type = "GDAL")
        }

        teRas <- " " #This sets extents in GDAL
        if (!is.null(rasterToMatch)) {
          teRas <- paste0(" -te ", paste0(extent(rasterToMatch)@xmin, " ",
                                          extent(rasterToMatch)@ymin, " ",
                                          extent(rasterToMatch)@xmax, " ",
                                          extent(rasterToMatch)@ymax, " "))
        }

        cores <- dealWithCores(cores)
        prll <- paste0("-wo NUM_THREADS=", cores, " ")

        # browser(expr = exists("._projectInputs_2"))
        # This will clear the Windows error that sometimes occurs:
        #  ERROR 1: PROJ: pj_obj_create: Cannot find proj.db ## Eliot Jan 22, 2020
        if (identical(.Platform[["OS.type"]], "windows")) {
          oldProjLib <- Sys.getenv("PROJ_LIB")
          if (!isTRUE(grepl("proj.db", dir(oldProjLib)))) {
            possNewDir <- dir(file.path(dirname(getOption("gdalUtils_gdalPath")[[1]]$path), "share", "proj"),
                              recursive = TRUE, pattern = "proj.db", full.names = TRUE)
            if (length(possNewDir)) {
              Sys.setenv(PROJ_LIB = dirname(possNewDir))
              on.exit(add = TRUE, {
                Sys.setenv(PROJ_LIB = oldProjLib)
              })
            }
          }
        }

        targCRS <- as.character(targetCRS)
        if (FALSE) {
          # There is a new-ish warning " +init=epsg:XXXX syntax is deprecated. It might return a CRS with a non-EPSG compliant axis order."
          #  This next clears all the extraneous stuff after the EPSG... but that may not be correct.
          #  I think leave it with the warning.
          targCRS <- gsub(".*(epsg:.[0123456789]*)( ).*", "\\1", targCRS)
        }
        system(
          paste0(paste0(getOption("gdalUtils_gdalPath")[[1]]$path, "gdalwarp", exe, " "),
                 "-s_srs \"", as.character(.crs(raster::raster(tempSrcRaster))), "\"",
                 " -t_srs \"", targCRS, "\"",
                 " -multi ", prll,
                 "-ot ", dTypeGDAL,
                 teRas,
                 "-r ", dots$method,
                 " -overwrite ",
                 if (!dontSpecifyResBCLongLat) {paste("-tr ", paste(tr, collapse = " "))},
                 " ",
                 "\"", tempSrcRaster, "\"", " ",
                 "\"", tempDstRaster, "\""),
          wait = TRUE)

        x <- raster(tempDstRaster)
        x <- setMinMaxIfNeeded(x)
        crs(x) <- targetCRS #sometimes the crs is correct but the character string is not identical
        #file exists in temp drive. Can copy to filename2
      } else {
        origDataType <- dataType(x)

        # Capture problems that projectRaster has with objects of class integers,
        #   which is different than if they are integers (i.e., a numeric class object)
        #   can be integers, without being classified and stored in R as integer

        # should be faster than assessDataType, as it is a class determination,
        # not a numeric assessment:
        isInteger <- if (is.integer(x[])) TRUE else FALSE

        if (isInteger) {
          if (!is.null(dots$method)) {
            if (dots$method != "ngb") {
              warning("This raster layer has integer values; it will be reprojected to float. ",
                      "Did you want to pass 'method = \"ngb\"'?")
            }
          }
        }

        if (is.null(dots$method)) {
          # not foolproof method of determining reclass method:
          dots$method <- assessDataType(x, type = "projectRaster")
          uniqueDotsMethod <- unique(dots$method)
          if (length(uniqueDotsMethod) > 1) {
            if (length(intersect(uniqueDotsMethod, "ngb")) == 1)
              uniqueDotsMethod <- "ngb"
            else
              uniqueDotsMethod <- uniqueDotsMethod[1]
            messagePrepInputs("There is more than one dataType in the layers of the Raster* object; reprojection will use",
                              uniqueDotsMethod, verbose = verbose)
          } else {
            dots$method <- uniqueDotsMethod
          }
        }

        messagePrepInputs("      reprojecting using ", dots$method, "...", verbose = verbose)

        falseWarns <- paste0(projNotWKT2warn, "|input and ouput crs|no non-missing arguments")
        if (is.null(rasterToMatch)) {
          Args <- append(dots, list(from = x, crs = targetCRS))
          x <- # captureWarningsToAttr( Eliot
            suppressWarningsSpecific(do.call(projectRaster, args = Args),
                                     falseWarnings = falseWarns)
          #)
          #warn <- attr(x, "warning")
          #attr(x, "warning") <- NULL

        } else {
          # projectRaster does silly things with integers, i.e., it converts to numeric
          if (is.na(targetCRS))
            stop("rasterToMatch needs to have a projection (crs)")
          tempRas <- suppressWarningsSpecific(
            projectExtent(object = rasterToMatch, crs = targetCRS), projNotWKT2warn)
          Args <- append(dots, list(from = x, to = tempRas))
          x <- # captureWarningsToAttr( Eliot
            suppressWarningsSpecific(falseWarnings = falseWarns,
                                     do.call(projectRaster, args = Args), verbose = verbose)
          #)
          if (isStack)
            if (!is(x, "RasterStack")) x <- raster::stack(x)
          # check for faulty datatype --> namely if it is an integer but classified as flt because of floating point problems
          if (isTRUE(grepl("FLT", dataType(x)))) {
            rrr <- round(x[], 0) %==% x[]
            if (isTRUE(sum(!rrr[!is.na(rrr)]) == 0)) # if (isTRUE(sum(!na.omit(rrr)) == 0))
              x[] <- round(x[], 0)
          }
          #warn <- attr(x, "warning")
          #attr(x, "warning") <- NULL

          if (identical(.crs(x), .crs(rasterToMatch)) & any(res(x) != res(rasterToMatch))) {
            if (all(res(x) %==% res(rasterToMatch))) {
              res(x) <- res(rasterToMatch) # TODO: This is irrelevant. Should not happen. TO Omit.
            } else {
              stop("Error: input and output resolutions are not similar after using projectRaster.",
                   "\nTry increasing error tolerance in options('fpCompare.tolerance').")
            }
          }
        }
        if (!identical(.crs(x), targetCRS)) {
          crs(x) <- targetCRS # sometimes the proj4string is rearranged, so they are not identical:
          #  they should be
        }

        # return the integer class to the data in the raster object
        if (isTRUE(isInteger)) {
          x[] <- as.integer(x[])
          dataType(x) <- origDataType
        }

        #warn <- warn[!grepl("no non-missing arguments to m.*; returning .*Inf", warn)] # This is a bug in raster
        #if (length(warn))
        #  warnings(warn)
        ## projectRaster doesn't always ensure equal res (floating point number issue)
        ## if resolutions are close enough, re-write res(x)
        ## note that when useSAcrs = TRUE, the different resolutions may be due to
        ## the different projections (e.g. degree based and meter based). This should be fine

      }
    } else {
      messagePrepInputs("    no reprojecting because target characteristics same as input Raster.",
                        verbose = verbose,
                        verboseLevel = 0)
    }
  }

  if (isFactorRaster) {
    levels(x) <- rasterFactorLevels
  }

  x
}

#' @export
#' @rdname projectInputs
projectInputs.sf <- function(x, targetCRS, verbose = getOption("reproducible.verbose", 1), ...) {
  messagePrepInputs("    reprojecting ...", verbose = verbose, verboseLevel = 0)
  .requireNamespace("sf", stopOnFALSE = TRUE)
  if (!is.null(targetCRS)) {
    warning("sf class objects not fully tested Use with caution.")
    .requireNamespace("sf", stopOnFALSE = TRUE)
    isValid <- sf::st_is_valid(x)
    if (any(sf::st_is(x, c("POLYGON", "MULTIPOLYGON"))) && !any(isValid)) {
      x[!isValid] <- sf::st_buffer(x[!isValid], dist = 0, ...)
    }

    if ("projargs" %in% slotNames(targetCRS) )
      targetCRS <- sf::st_crs(targetCRS@projargs)
    x <- sf::st_transform(x = x, crs = targetCRS, ...)
    if (!identical(sf::st_crs(x), targetCRS)) {
      ## sometimes the proj4string is rearranged, so they are not identical; they should be
      sf::st_crs(x) <- targetCRS
    }
  }
  x
}

#' @export
#' @rdname projectInputs
#' @importFrom raster crs
projectInputs.Spatial <- function(x, targetCRS, verbose = getOption("reproducible.verbose", 1), ...) {
  messagePrepInputs("    reprojecting ...", verbose = verbose, verboseLevel = 0)
  if (!is.null(targetCRS)) {
    if (!is(targetCRS, "CRS")) {
      if (!is.character(targetCRS)) {
        if (is(targetCRS, "spatialClasses")) {
          targetCRS <- .crs(targetCRS)
        } else {
          stop("targetCRS in projectInputs must be a CRS object or a class from",
               " which a crs can be extracted with raster::crs")
        }
      }
    }
    x <- spTransform(x = x, CRSobj = targetCRS)
    if (!identical(.crs(x), targetCRS)) {
      ## sometimes the proj4string is rearranged, so they are not identical; they should be
      crs(x) <- targetCRS
    }
  }
  x
}

#' Hierarchically get crs from \code{Raster*}, \code{Spatial*}
#'
#' This is the function that follows the table of order of
#' preference for determining CRS. See \code{\link{postProcess}}
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
#' equivalent to \code{raster::mask} (though \code{\link{fastMask}} is used here)
#' or \code{raster::intersect}.
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
#' @example inst/examples/example_postProcess.R
#'
maskInputs <- function(x, studyArea, ...) {
  UseMethod("maskInputs")
}

#' @param maskWithRTM Logical. If \code{TRUE}, then the default,
#'
#' @export
#' @importFrom raster stack
#' @rdname maskInputs
maskInputs.Raster <- function(x, studyArea, rasterToMatch, maskWithRTM = NULL,
                              verbose = getOption("reproducible.verbose", 1), ...) {
  messagePrepInputs("    masking ...", verbose = verbose, verboseLevel = 0)
  # browser(expr = exists("._maskInputs_1"))
  isStack <- is(x, "RasterStack")
  if (is.null(studyArea) && !is.null(rasterToMatch) && is.null(maskWithRTM)) {
    messagePrepInputs("      studyArea is NULL; rasterToMatch provided. Masking with rasterToMatch NA values. ",
                      "To leave unmasked, set maskWithRTM = FALSE", verbose = verbose)
    maskWithRTM <- TRUE
  }
  if (isTRUE(maskWithRTM)) {
    x <- maskWithRasterNAs(x = x, y = rasterToMatch)
  } else {
    if (!is.null(studyArea)) {
      # dots <- list(...)
      x <- fastMask(x = x, y = studyArea, verbose = verbose, ...)
    } else {
      messagePrepInputs("studyArea not provided, skipping masking.", verbose = verbose)
    }
  }
  if (isStack) { # do this even if no masking; it takes 10 microseconds if already a RasterStack
    x <- raster::stack(x)
  }

  return(x)
}

#' @export
#' @rdname maskInputs
maskInputs.Spatial <- function(x, studyArea, rasterToMatch, maskWithRTM = FALSE,
                               verbose = getOption("reproducible.verbose", 1),
                               useCache = getOption("reproducible.useCache", FALSE),
                               ...) {

  x <- sf::st_as_sf(x)

  x <- retry(retries = 2, silent = FALSE, exponentialDecayBase = 1,
             expr = quote(
               maskInputs(x, studyArea, rasterToMatch, maskWithRTM, verbose = verbose)
             ),
             exprBetween = quote(
               x <- fixErrors(x, testValidity = NA, useCache = useCache)
             ))

  as(x, "Spatial")
}

#' @export
#' @rdname maskInputs
maskInputs.sf <- function(x, studyArea, verbose = getOption("reproducible.verbose", 1),
                          useCache = getOption("reproducible.useCache", FALSE),
                          ...) {
  .requireNamespace("sf", stopOnFALSE = TRUE)

  if (!is.null(studyArea)) {
    if (is(studyArea, "Spatial")) {
      studyArea <- sf::st_as_sf(studyArea)
    }

    xOrigCRS <- sf::st_crs(x)
    changedCRS <- FALSE
    isXLongLat <- sf::st_is_longlat(x)
    messagePrepInputs("maskInputs with sf class objects is still experimental", verbose = verbose)
    messagePrepInputs("    intersecting ...", verbose = verbose, verboseLevel = 0)
    #studyArea <- raster::aggregate(studyArea, dissolve = TRUE)
    if (!identical(xOrigCRS, sf::st_crs(studyArea))) {
      if (isXLongLat) {
        changedCRS <- TRUE
        x <- sf::st_transform(x, crs = sf::st_crs(studyArea))
      } else {
        studyArea <- sf::st_transform(studyArea, crs = xOrigCRS)
      }
    }

    if (NROW(studyArea) > 1) {
      studyArea <- sf::st_sf(sf::st_combine(studyArea))
    }

    if (is(sf::st_geometry(x), "sfc_POINT")) {
      y1 <- sf::st_intersects(x, studyArea)
      y2 <- vapply(y1, function(x) length(x) == 1, logical(1))
      y <- x[y2, ]
    } else {
      x <- sf::st_set_precision(x, 1e5)
      studyArea <- sf::st_set_precision(studyArea, 1e5)
      studyArea <- fixErrors(studyArea, useCache = useCache)

      y <- retry(retries = 2, silent = FALSE, exponentialDecayBase = 1,
                 expr = quote(
                   sf::st_intersection(x, studyArea)
                 ),
                 exprBetween = quote(
                   x <- fixErrors(x, testValidity = NA, useCache = useCache)
                 ))
      # x <- sf::st_set_precision(x, 1e5) %>% fixErrors(.)
      # studyArea <- sf::st_set_precision(studyArea, 1e5) %>% fixErrors(.)
      # y <- sf::st_intersection(x, studyArea)
      # y <- fixErrors(y)
    }
    if (!identical(.crs(y), .crs(x))) {
      ## sometimes the proj4string is rearranged, so they are not identical; they should be
      crs(y) <- .crs(x)
    }
    if (changedCRS) {
      y <- sf::st_transform(y, xOrigCRS)
    }

    return(y)
  } else {
    return(x)
  }
}

#' Determine filename, either automatically or manually
#'
#' Determine the filename, given various combinations of inputs.
#'
#' @details
#' The post processing workflow, which includes this function,
#' addresses several scenarios, and depending on which scenario, there are
#' several file names at play. For example, \code{Raster} objects may have
#'   file-backed data, and so \emph{possess a file name}, whereas \code{Spatial}
#'   objects do not. Also, if post processing is part of a \code{\link{prepInputs}}
#'   workflow, there will always be a file downloaded. From the perspective of
#'   \code{postProcess}, these are the "inputs" or \code{filename1}.
#'   Similarly, there may or may not be a desire to write an
#'   object to disk after all post processing, \code{filename2}.
#'
#'   This subtlety means that there are two file names that may be at play:
#'   the "input" file name (\code{filename1}), and the "output" filename (\code{filename2}).
#'   When this is used within \code{postProcess}, it is straight forward.
#'
#'
#'   However, when \code{postProcess} is used within a \code{prepInputs} call,
#'   the \code{filename1} file is the file name of the downloaded file (usually
#'   automatically known following the downloading, and refered to as \code{targetFile})
#'   and the \code{filename2} is the file name of the of post-processed file.
#'
#'   If \code{filename2} is \code{TRUE}, i.e., not an actual file name, then the cropped/masked
#'   raster will be written to disk with the original \code{filenam1/targetFile}
#'   name, with \code{prefix} prefixed to the basename(\code{targetFile}).
#'
#'   If \code{filename2} is a character string, it will be the path of the saved/written
#'   object e.g., passed to \code{writeOutput}. It will be tested whether it is an
#'   absolute or relative path and used as is if absolute or
#'   prepended with \code{destinationPath} if relative.
#'
#' @inheritParams postProcess.spatialClasses
#'
#' @param destinationPath Optional. If \code{filename2} is a relative file path, then this
#'                        will be the directory of the resulting absolute file path.
#'
#' @param prefix The character string to prepend to \code{filename1}, if \code{filename2}
#'               not provided.
#'
#' @include helpers.R
#'
#' @details
#'  If \code{filename2} is \code{logical}, then the output
#'  filename will be \code{prefix} prefixed to the basename(\code{filename1}).
#'  If a character string, it
#'  will be the path returned. It will be tested whether it is an
#'  absolute or relative path and used as is if absolute or prepended with
#'  \code{destinationPath} if provided, and if \code{filename2} is relative.
#'
#' @importFrom raster tmpDir
#' @rdname determineFilename
#' @example inst/examples/example_postProcess.R
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
#' Can be used to write prepared inputs on disk.
#'
#' @param x  The object save to disk i.e., write outputs
#'
#' @param overwrite Logical. Should file being written overwrite an existing file if it exists.
#'
#' @param filename2 File name passed to \code{\link[raster]{writeRaster}}, or
#'                  \code{\link[raster]{shapefile}} or \code{\link[sf]{st_write}}
#'                  (\code{dsn} argument).
#'
#' @param ... Passed into \code{\link[raster]{shapefile}} or
#'             \code{\link[raster]{writeRaster}} or \code{\link[sf]{st_write}}
#'
#' @inheritParams prepInputs
#'
#' @author Eliot McIntire and Jean Marchal
#' @export
#' @importFrom methods is
#' @importFrom raster shapefile writeRaster
#' @rdname writeOutputs
#'
#' @example inst/examples/example_postProcess.R
writeOutputs <- function(x, filename2,
                         overwrite = getOption("reproducible.overwrite", NULL),
                         ...) {
  UseMethod("writeOutputs")
}

#' @export
#' @rdname writeOutputs
writeOutputs.Raster <- function(x, filename2 = NULL,
                                overwrite = getOption("reproducible.overwrite", FALSE),
                                verbose = getOption("reproducible.verbose", 1),
                                ...) {
  dots <- list(...)
  datatype2 <- if (is.null(dots$datatype)) {
    assessDataType(x, type = "writeRaster")
  } else {
    dots$datatype
  }

  if (!is.null(filename2)) {
    messagePrepInputs("    writing to disk", verbose = verbose, verboseLevel = 0)
    if (is.null(dots$datatype)) {
      out <- lapply(paste("No 'datatype' chosen.",
                          "Saving", names(x), "as", datatype2 ), messagePrepInputs, verbose = verbose)
      dots$datatype <- datatype2
    } else if (any(datatype2 != dots$datatype)) {
      out <- lapply(paste("chosen 'datatype', ", dots$datatype, ", may be inadequate for the ",
                          "range/type of values in ", names(x),
                          "\n consider changing to ", datatype2), messagePrepInputs, verbose = verbose)
    }

    if (any(raster::is.factor(x))) {
      filename3 <- gsub(filename2, pattern = "\\.tif", replacement = ".grd")
      if (!identical(filename2, filename3)) {
        warning(".tif format does not preserve factor levels using rgdal. Using ",
                filename3, " to preserve levels, instead of ", filename2)
        filename2 <- filename3
      }
    }
    # There is a weird thing that doing a writeRaster changes the digest of the file, even
    #   when the object is identical, confirmed by loading each into R, and comparing everything
    # So, skip that writeRaster if it is already a file-backed Raster, and just copy it
    #    ERROR ALERT -- You can't change the dataType this way, so you will need to
    #    go the writeRaster route if dots$datatype is passed and it isn't equal to dataType(x)
    if (fromDisk(x) && all(dots$datatype == dataType(x))) {
      theFilename <- Filenames(x, allowMultiple = FALSE)
      if (fileExt(theFilename) == "grd") {
        if (!fileExt(filename2) == "grd") {
          warning("filename2 file type (", fileExt(filename2), ") was not same type (",
                  fileExt(filename(x)),") ", "as the filename of the raster; ",
                  "Changing filename2 so that it is ", fileExt(filename(x)))
          filename2 <- gsub(fileExt(filename2), "grd", filename2)
        }
        theFilenameGri <- gsub("grd$", "gri", theFilename)
        filename2Gri <- gsub("grd$", "gri", filename2)
        if (file.exists(filename2Gri)) {
          if (isTRUE(overwrite))
            unlink(filename2Gri)
        }
        out <- hardLinkOrCopy(theFilenameGri, filename2Gri)

        # out <- suppressWarningsSpecific(file.link(theFilenameGri, filename2Gri),
        #                                 falseWarnings = "already exists|Invalid cross-device")
        # # out <- suppressWarnings(file.link(theFilenameGri, filename2Gri))
        # if (any(!out)) {
        #   out <- file.copy(theFilenameGri[!out], filename2Gri[!out],
        #                    overwrite = overwrite)
        #
        # }
      }

      if (file.exists(filename2)) {
        if (isTRUE(overwrite))
          unlink(filename2)
      }
      out <- hardLinkOrCopy(theFilename, filename2)
      # out <- suppressWarningsSpecific(file.link(theFilename, filename2),
      #                                 falseWarnings = "already exists|Invalid cross-device")
      # # out <- suppressWarnings(file.link(theFilename, filename2))
      # if (any(!out)) {
      #   out <- file.copy(theFilename[!out], filename2[!out],
      #                    overwrite = overwrite)
      #
      # }
      x <- updateFilenameSlots(x, curFilenames = theFilename, newFilenames = filename2)
      # if (any(dots$datatype != dataType(x))) {
      #   if (is(x, "RasterStack")) {
      #     newDT <- if (length(dots$datatype) == 1) {
      #       rep(dots$datatype, nlayers(x))
      #     } else {
      #       dots$datatype
      #     }
      #     for (ln in seq(names(x)))
      #       dataType(x[[ln]]) <- newDT[ln]
      #   } else {
      #     dataType(x) <- dots$datatype
      #   }
      # }
    } else {
      argsForWrite <- append(list(filename = filename2, overwrite = overwrite), dots)
      if (is(x, "RasterStack")) {
        longerThanOne <- unlist(lapply(argsForWrite, function(x) length(unique(x)) > 1))
        nLayers <- raster::nlayers(x)
        if (any(unlist(longerThanOne))) {
          if (!identical(nLayers, length(argsForWrite$filename))) {
            argsForWrite$filename <- file.path(dirname(argsForWrite$filename), paste0(names(x), "_", basename(argsForWrite$filename)))
          }
        }
        if (length(argsForWrite$filename) == 1) {
          argsForWrite <- lapply(argsForWrite, function(x) x[1])
          xTmp <- do.call(writeRaster, args = c(x = x, argsForWrite))
          names(xTmp) <- names(x)
          # messagePrepInputs("Object was a RasterStack; only one filename provided so returning a RasterBrick;", verbose = verbose)
          # messagePrepInputs("  layer names will likely be wrong.", verbose = verbose)
        } else if (length(argsForWrite$filename) == nLayers) {
          dups <- duplicated(argsForWrite$filename)
          if (any(dups)) {
            a <- argsForWrite$filename
            out <- unlist(lapply(seq_along(a), function(ind) {
              if (ind == 1)
                a[[1]] <<- file.path(dirname(a[[1]]), nextNumericName(basename(a[[1]])))
              else
                a[[ind]] <<- file.path(dirname(a[[ind]]), basename(nextNumericName(basename(a[[ind - 1]]))))
            }))
            argsForWrite$filename <- out
          }
          argsForWrite[!longerThanOne] <- lapply(argsForWrite[!longerThanOne], function(x) rep(x, nLayers))
          xTmp <- lapply(seq_len(nLayers), function(ind) {
            inside <- progressBarCode(do.call(writeRaster, args = c(x = x[[ind]], lapply(argsForWrite, function(y) y[ind]))),
                                      doProgress = ncell(x) > 2e6,
                                      message = c("Writing ", argsForWrite$filename[ind], " to disk ..."),
                                      colour = getOption("reproducible.messageColourPrepInputs"),
                                      verbose = verbose)
            names(inside) <- names(x)[ind]
            inside
          })

        } else {
          stop("filename2 must be length 1 or length nlayers(...)")
        }
        xTmp <- raster::stack(xTmp)
      } else {
        if (file.exists(argsForWrite$filename)) {
          if (interactive() && .isFALSE(argsForWrite$overwrite)) {
            wantOverwrite <- readline(paste0("File ", argsForWrite$filename, " already exists; overwrite? Y or N: "))
            if (identical(tolower(wantOverwrite), "y"))
              argsForWrite$overwrite <- TRUE
          }
        }
        origColors <- checkColors(x)
        xTmp <- progressBarCode(do.call(writeRaster, args = c(x = x, argsForWrite)),
                                doProgress = ncell(x) > 2e6,
                                message = c("Writing ", argsForWrite$filename, " to disk ..."),
                                colour = getOption("reproducible.messageColourPrepInputs"),
                                verbose = verbose)

        xTmp <- rebuildColors(xTmp, origColors)
      }
      #Before changing to do.call, dots were not being added.
      # This is a bug in writeRaster was spotted with crs of xTmp became
      # +proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs
      # should have stayed at
      # +proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0
      if (!identical(.crs(xTmp), .crs(x)))
        crs(xTmp) <- .crs(x)

      x <- xTmp
    }
  }
  x
}

#' @rdname writeOutputs
writeOutputs.Spatial <- function(x, filename2 = NULL,
                                 overwrite = getOption("reproducible.overwrite", TRUE),
                                 ...) {
  writeOutputs(sf::st_as_sf(x), filename2 = filename2, overwrite = overwrite)

  # if (!is.null(filename2)) {
  #   if (!grepl(".shp$", raster::extension(filename2))) {
  #     filename2 <- paste0(filename2, ".shp")
  #   }
  #
  #   dots <- list(...)
  #   notWanted1 <- .formalsNotInCurrentDots(shapefile, ...)
  #   formalNamesIn_rgdal_writeOGR <- c("obj", "dsn", "layer", "driver", "dataset_options", "layer_options",
  #                "verbose", "check_exists", "overwrite_layer", "delete_dsn", "morphToESRI",
  #                "encoding", "shp_edge_case_fix", "dumpSRS")
  #   notWanted2 <- .formalsNotInCurrentDots(formalNames = formalNamesIn_rgdal_writeOGR, ...)
  #   keepForDots <- c(setdiff(notWanted1, notWanted2), setdiff(names(dots), notWanted1))
  #   dots <- dots[keepForDots]
  #   # Internally in rgdal::writeOGR, it converts the row.names to integer with this test
  #   #   it creates a warning there, so capture here instead
  #   warn <- captureWarningsToAttr(as.integer(row.names(x)))
  #   if (isTRUE(any(grepl("NAs introduced by coercion", attr(warn, "warning")))))
  #     row.names(x) <- as.character(seq_along(row.names(x)))
  #   do.call(sf::st_write, append(dots, list(obj = sf::st_as_sf(x), dsn = filename2, overwrite = overwrite)))
  # }
  x
}

#' @rdname writeOutputs
writeOutputs.sf <- function(x, filename2 = NULL,
                            overwrite = getOption("reproducible.overwrite", FALSE),
                            verbose = getOption("reproducible.verbose", 1),
                            ...) {
  .requireNamespace("sf", stopOnFALSE = TRUE)
  if (!is.null(filename2)) {
    messagePrepInputs("    writing to disk", verbose = verbose, verboseLevel = 0)
    # if (!nzchar(fileExt(filename2))) {
    #   filename2 <- paste0(filename2, ".shp")
    # }
    if (!grepl(".shp$", raster::extension(filename2))) {
      filename2 <- paste0(filename2, ".shp")
    }
    if (identical(".", dirname(filename2))) {
      filename2 <- normPath(filename2)
    }
    dp <- list(...)$destinationPath
    if (!is.null(dp)) {
      if (!isTRUE(grepl(normPath(dp), filename2)))
        filename2 <- normPath(file.path(dp, basename(filename2)))
    }
    if (!all(file.exists(filename2)))
      overwrite = FALSE
    muffld <- capture.output(
      sf::st_write(obj = x, dsn = filename2, delete_dsn = overwrite)
    )
  }
  x
}

#' @rdname writeOutputs
writeOutputs.quosure <- function(x, filename2, ...) {
  writeOutputs(eval_tidy(x), filename2 = filename2, ...)
}

#' @rdname writeOutputs
writeOutputs.default <- function(x, filename2, ...) {
  stop("Don't know how to write object of class ", class(x), " on disk.")
}

#' Assess the appropriate raster layer data type
#'
#' Can be used to write prepared inputs on disk.
#'
#' @param ras  The \code{RasterLayer} or \code{RasterStack} for which data type will be assessed.
#' @param type Character. \code{"writeRaster"} (default) or \code{"GDAL"} to return the recommended
#'             data type for writing from the raster and gdalUtils packages, respectively, or
#'             \code{"projectRaster"} to return recommended resampling type.
#' @return The appropriate data type for the range of values in \code{ras}.
#'         See \code{\link[raster]{dataType}} for details.
#'
#' @author Eliot McIntire
#' @author Ceres Barros
#' @author Ian Eddy
#' @author Eliot McIntire
#' @export
#' @importFrom raster getValues sampleRandom
#' @rdname assessDataType
#'
#' @example inst/examples/example_assessDataType.R
assessDataType <- function(ras, type = 'writeRaster') {
  UseMethod("assessDataType")
}

#' @export
#' @importFrom raster getValues maxValue ncell
#' @rdname assessDataType
assessDataType.Raster <- function(ras, type = "writeRaster") {
  ## using ras@data@... is faster, but won't work for @values in large rasters
  N <- 1e5

  # browser(expr = exists("._assessDataType_1"))
  datatype <- NULL
  if (ncell(ras) > 1e8) { # for very large rasters, try a different way
    maxValCurrent <- maxValue(ras)
    ras <- setMinMaxIfNeeded(ras)
    # if (maxValCurrent != maxValue(ras))
    datatype <- dataType(ras)
  } else {
    ras <- setMinMaxIfNeeded(ras)
  }

  if (is.null(datatype)) {

    if (ncell(ras) > N) {
      rasVals <- tryCatch(suppressWarnings(raster::sampleRandom(x = ras, size = N)),
                          error = function(x) rep(NA_integer_, N))
    } else {
      rasVals <- raster::getValues(ras)
    }
    minVal <- min(ras@data@min)
    maxVal <- max(ras@data@max)
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

    #   if (!doubVal & !signVal) {
    #     ## only check for binary if there are no decimals and no signs
    #     logi <- all(!is.na(.bincode(na.omit(rasVals), c(-1,1))))  ## range needs to include 0
    #
    #     if (logi) {
    #       datatype <- "LOG1S"
    #     } else {
    #       ## if() else is faster than if
    #       datatype <- if (maxVal <= MaxVals$INT1U) "INT1U" else
    #         if (maxVal <= MaxVals$INT2U) "INT2U" else
    #           if (maxVal <= MaxVals$INT4U) "INT4U" else  ## note: ?dataType advises against INT4U
    #             if (maxVal > MaxVals$FLT4S) "FLT8S" else "FLT4S"
    #     }
    #   } else {
    #     if (signVal & !doubVal) {
    #       ## if() else is faster than if
    #       datatype <- if (minVal >= -MaxVals$INT1S & maxVal <= MaxVals$INT1S) "INT1S" else
    #         if (minVal >= -MaxVals$INT2S & maxVal <= MaxVals$INT2S) "INT2S" else
    #           if (minVal >= -MaxVals$INT4S & maxVal <=  MaxVals$INT4S) "INT4S" else  ## note: ?dataType advises against INT4U
    #             if (minVal < -MaxVals$FLT4S | maxVal > MaxVals$FLT4S) "FLT8S" else "FLT4S"
    #     } else {
    #       if (doubVal)
    #         datatype <- if (minVal < -MaxVals$FLT4S | maxVal > MaxVals$FLT4S) "FLT8S" else "FLT4S"
    #     }
    #   }
    #   if (!identical(datatype, datatype1))
  }
  #convert datatype if needed
  datatype <- switchDataTypes(datatype, type = type)
  datatype
}

#' @export
#' @rdname assessDataType
assessDataType.RasterStack <- function(ras, type = "writeRaster") {

  xs <- lapply(names(ras), FUN = function(x){
    y <- assessDataType(ras = ras[[x]], type)
    return(y)})

  return(unlist(xs))
}

#' @export
#' @rdname assessDataType
assessDataType.default <- function(ras, type = "writeRaster") {
  stop("No method for assessDataType for class ", class(ras))
}

#' Assess the appropriate raster layer data type for GDAL
#'
#' This is a convenience function around \code{assessDataType(ras, type = "GDAL")}
#'
#' @param ras  The RasterLayer or RasterStack for which data type will be assessed.
#' @return The appropriate data type for the range of values in \code{ras} for using GDAL.
#'         See \code{\link[raster]{dataType}} for details.
#' @author Eliot McIntire, Ceres Barros, Ian Eddy, and Tati Micheletti
#' @example inst/examples/example_assessDataTypeGDAL.R
#' @export
#' @rdname assessDataType
assessDataTypeGDAL <- function(ras) {
  assessDataType(ras, type = "GDAL")
}

#' @importFrom rlang eval_tidy
postProcessChecks <- function(studyArea, rasterToMatch, dots,
                              verbose = getOption("reproducible.verbose", 1)) {
  if (!is.null(studyArea) & !is(studyArea, "Spatial")) {
    if (!is.null(studyArea) & !is(studyArea, "sf")) {
      stop("The 'studyArea' provided is not a Spatial* object.")
    }
  }

  if (!is.null(rasterToMatch) & !is(rasterToMatch, "RasterLayer")) {
    stop("The 'rasterToMatch' provided is not a Raster* object.")
  }

  filename1 <- NULL
  if (!is.null(dots$inputFilePath))  {
    messagePrepInputs("inputFilePath is being deprecated; use filename1", verbose = verbose)
    filename1 <- dots$inputFilePath
    dots$inputFilePath <- NULL
  }

  if (!is.null(dots$targetFilePath))  {
    messagePrepInputs("targetFilePath is being deprecated; use filename1.", verbose = verbose)
    filename1 <- dots$targetFilePath
    dots$targetFilePath <- NULL
  }
  list(dots = dots, filename1 = filename1)
}

#' @importFrom raster projectExtent
#' @importFrom sp wkt
#' @importFrom Require normPath
postProcessAllSpatial <- function(x, studyArea, rasterToMatch,
                                  useCache = getOption("reproducible.useCache", FALSE),
                                  filename1,
                                  filename2, useSAcrs, overwrite, targetCRS = NULL,
                                  useGDAL = getOption("reproducible.useGDAL", TRUE),
                                  cores = getOption("reproducible.GDALcores", 2),
                                  verbose = getOption("reproducible.verbose", 1),
                                  ...) {
  dots <- list(...)
  # browser(expr = exists("._postProcessAllSpatial_1"))
  testValidity <- TRUE

  if (!is.null(studyArea))
    if (is(studyArea, "quosure"))
      studyArea <- rlang::eval_tidy(studyArea)

  if (!is.null(rasterToMatch))
    if (is(rasterToMatch, "quosure"))
      rasterToMatch <- rlang::eval_tidy(rasterToMatch)

  extraDots <- postProcessChecks(studyArea = studyArea, rasterToMatch = rasterToMatch, dots = dots)
  dots <- extraDots$dots
  if (!is.null(extraDots$filename1))
    filename1 <- extraDots$filename1

  if (!is.null(studyArea) || !is.null(rasterToMatch) || !is.null(targetCRS)) {
    attemptGDALAllAtOnce <- if (is(x, "RasterLayer")) attemptGDAL(x, useGDAL = useGDAL, verbose = verbose) else FALSE
    if (isTRUE(attemptGDALAllAtOnce) ) {
      x <- cropReprojMaskWGDAL(x, studyArea, rasterToMatch, targetCRS, cores, dots, filename2,
                               useSAcrs, verbose = verbose, ...)
    } else {
      # fix errors if methods available
      skipCacheMess <- "useCache is FALSE, skipping Cache"
      skipCacheMess2 <- "No cacheRepo supplied"

      ##################################
      # cropInputs
      ##################################
      if (!is.null(rasterToMatch)) {
        extRTM <- extent(rasterToMatch)
        crsRTM <- .crs(rasterToMatch)
      } else {
        extRTM <- NULL
        crsRTM <- NULL
      }
      useBuffer <- FALSE
      bufferSA <- FALSE

      if (is(x, "Raster")) {
        #if all CRS are projected, then check if buffer is necessary
        objsAreProjected <- list(x, studyArea, crsRTM)
        nonNulls <- !unlist(lapply(objsAreProjected, is.null))
        suppressWarningsSpecific(falseWarnings = "wkt|CRS object has no comment",
                                 projections <- sapply(objsAreProjected[nonNulls],
                                                       # function(xx) grepl("(longitude).*(latitude)",
                                                       #                    tryCatch(wkt(xx), error = function(yy) NULL))))
                                                       function(xx) !isProjected(xx)))

        if (!any(unlist(projections))) {
          if (is.null(rasterToMatch) || max(res(rasterToMatch)) < min(res(x))) {
            useBuffer <- TRUE
          }
        }
      }

      if (useBuffer) {
        #replace extentRTM and crsRTM, because they will supersede all arguments
        if (!requireNamespace("rgeos", quietly = TRUE)) stop(messageRgeosMissing)
        if (!is.null(rasterToMatch)) {
          #reproject rasterToMatch, extend by res
          newExtent <- suppressWarningsSpecific(projectExtent(rasterToMatch, crs = .crs(x)), projNotWKT2warn)
          tempPoly <- as(extent(newExtent), "SpatialPolygons")
          crs(tempPoly) <- .crs(x)
          #buffer the new polygon by 1.5 the resolution of X so edges aren't cropped out
          tempPoly <- raster::buffer(tempPoly, width = max(res(x))*1.5)
          extRTM <- tempPoly
          crsRTM <- suppressWarningsSpecific(falseWarnings = "CRS object has comment", .crs(tempPoly))
        } else {
          bufferSA <- TRUE
          origStudyArea <- studyArea
          bufferWidth <- max(res(x)) * 1.5
          crsX <- .crs(x)
          if (!is(studyArea, "sf")) {
            studyArea <- sp::spTransform(studyArea, CRSobj = crsX)
            studyArea <- raster::buffer(studyArea, width = bufferWidth)
          } else {
            .requireNamespace("sf", stopOnFALSE = TRUE)
            studyArea <- sf::st_transform(studyArea, crs = crsX)
            studyArea <- sf::st_buffer(studyArea, dist = bufferWidth)
          }
        }
      }

      if (is.null(rasterToMatch) && !is.null(studyArea) && (is(x, "Spatial") || is(x, "sf")) &&
          getOption("reproducible.polygonShortcut", TRUE)) {
        message("Using an experimental shortcut of maskInputs for special, simple case that x is polygon, ",
                "rasterToMatch not provided, and studyArea provided.",
                " If this is causing problems, set options(reproducible.polygonShortcut = FALSE)")
        x <- fixErrors(x = x, useCache = useCache, verbose = verbose,
                       testValidity = testValidity, ...)
        x <- Cache(maskInputs, x = x, studyArea = studyArea,
                   useCache = useCache, verbose = verbose, ...)
        x <- fixErrors(x = x, useCache = useCache, verbose = verbose,
                       testValidity = testValidity, ...)
      } else {
        # browser(expr = exists("._postProcess.spatialClasses_2"))
        if (!isTRUE(all.equal(extent(x), extRTM))) {
          useCacheOrig <- useCache
          useCache <- FALSE
          x <- Cache(cropInputs, x = x, studyArea = studyArea,
                     extentToMatch = extRTM,
                     extentCRS = crsRTM,
                     useCache = useCache, verbose = verbose, ...)
          useCache <- useCacheOrig
          testValidity <- NA # Crop will have done it
        } else {
          messageCache("  Skipping cropInputs; already same extents")
        }

        if (bufferSA) {
          studyArea <- origStudyArea
        }

        # cropInputs may have returned NULL if they don't overlap
        # browser(expr = exists("._postProcess.spatialClasses_3"))
        if (!is.null(x)) {
          objectName <- if (is.null(filename1)) NULL else basename(filename1)
          # x <- fixErrors(x = x, objectName = objectName,
          #                useCache = useCache, verbose = verbose,
          #                testValidity = testValidity, ...)

          ##################################
          # projectInputs
          ##################################
          targetCRS <- .getTargetCRS(useSAcrs, studyArea, rasterToMatch, targetCRS)

          runIt <- if (is(x, "Raster") && !is.null(rasterToMatch))
            differentRasters(x, rasterToMatch, targetCRS)
          else
            TRUE
          if (runIt) {
            x <- retry(retries = 2, silent = FALSE, exponentialDecayBase = 1,
                       expr = quote(
                         Cache(projectInputs, x = x, targetCRS = targetCRS,
                               rasterToMatch = rasterToMatch, useCache = useCache,
                               cores = cores, verbose = verbose, ...)
                       ),
                       exprBetween = quote(
                         x <- fixErrors(x, objectName = objectName,
                                        testValidity = NA, useCache = useCache)
                       ))
          } else {
            messageCache("  Skipping projectInputs; identical crs, res, extent")
          }

          ##################################
          # maskInputs
          ##################################
          yy <- retry(retries = 2, silent = FALSE, exponentialDecayBase = 1,
                      expr = quote(
                        maskInputs(x = x, studyArea = studyArea,
                                   rasterToMatch = rasterToMatch, useCache = useCache,
                                   verbose = verbose, ...)
                      ),
                      exprBetween = quote(
                        x <- fixErrors(x, objectName = objectName,
                                       testValidity = NA, useCache = useCache)
                      ))
          x <- yy

        }
      }
      ##################################
      # filename
      ##################################
      newFilename <- determineFilename(filename1 = filename1, filename2 = filename2, verbose = verbose, ...)

      ##################################
      # writeOutputs
      ##################################
      if (!is.null(filename2)) {
        x <- suppressWarningsSpecific(
          do.call(writeOutputs, append(list(x = rlang::quo(x),
                                            filename2 = normPath(newFilename),
                                            overwrite = overwrite,
                                            verbose = verbose), dots)),
          proj6Warn)
      } else {
        messageCache("  Skipping writeOutputs; filename2 is NULL")
      }

      # browser(expr = exists("._postProcess.spatialClasses_6"))
      if (dir.exists(bigRastersTmpFolder())) {
        ## Delete gdalwarp results in temp
        unlink(bigRastersTmpFolder(), recursive = TRUE)
      }
    }
  }
  x
}

useETM <- function(extentToMatch, extentCRS, verbose) {
  passingExtents <- sum(!is.null(extentToMatch), !is.null(extentCRS))
  if (passingExtents == 1) {
    messagePrepInputs("When passing extentToMatch, you must also pass extentCRS; using rasterToMatch or studyArea instead",
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
                      ". Couldn't fix them with ",bufferFn,"(..., width = 0)", verbose = verbose)
  } else {
    messagePrepInputs("  Some or all of the errors fixed.", verbose = verbose)
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
  suppressWarnings(maxValCurrent <- maxValue(ras))
  needSetMinMax <- FALSE
  if (isTRUE(any(is.na(maxValCurrent)))) {
    needSetMinMax <- TRUE
  } else {

    # if the colors are set and are the same length of the integer sequence between min and max, don't override
    if (length(.getColors(ras)[[1]])) {
      if (!is.na(suppressWarnings(maxValue(ras))) && !is.na(suppressWarnings(minValue(ras))))
        if (length(.getColors(ras)[[1]]) == (maxValue(ras) - minValue(ras) + 1)) {
          return(ras)
        }
    }
    possibleShortCut <- maxValCurrent %in% c(unlist(MaxVals), unlist(MaxVals) + 1)
    if (isTRUE(all(possibleShortCut))) {
      needSetMinMax <- TRUE
    }
  }
  if (isTRUE(needSetMinMax)) {
    large <- if (nlayers(ras) > 25 || ncell(ras) > 1e7) TRUE else FALSE
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
    trueWarnings <- grep(falseWarnings, xx$message,
                         invert = TRUE, value = TRUE)
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


#' @importFrom raster extension
#' @importFrom gdalUtilities gdalwarp
cropReprojMaskWGDAL <- function(x, studyArea = NULL, rasterToMatch = NULL,
                                targetCRS, cores = 1, dots = list(), filename2, useSAcrs = FALSE,
                                destinationPath = getOption("reproducible.destinationPath", "."),
                                verbose = getOption("reproducible.verbose", 1),
                                ...) {
  messagePrepInputs("crop, reproject, mask is using one-step gdalwarp")

  # browser(expr = exists("._cropReprojMaskWGDAL_1"))

  # rasters need to go to same directory that can be unlinked at end without losing other temp files
  tempSrcRaster <- bigRastersTmpFile()
  returnToRAM <- FALSE
  if (missing(filename2)) filename2 <- NULL
  if (!.isFALSE(filename2)) {
    filename2 <- determineFilename(filename2, destinationPath = destinationPath, verbose = verbose)
  }

  if (is.null(filename2) | .isFALSE(filename2)) {
    returnToRAM <- TRUE
    tmpRasPath <- checkPath(bigRastersTmpFolder(), create = TRUE)
    filename2 <- file.path(tmpRasPath, paste0(x@data@names, "_", rndstr(1, 8)))
    layerName <- names(x)
  }
  if (nchar(extension(filename2)) == 0)
    filename2 <- paste0(filename2, ".tif")
  filename2 <- normPath(filename2)

  # GDAL can't deal with grd filename extensions (and possibly others) --> go with .tif and update at end
  filename2Orig <- filename2
  if (!grepl(".tif$", filename2)) filename2 <- paste0(filename2, ".tif")
  needRenameAtEnd <- !identical(filename2Orig, filename2)

  isFactor <- raster::is.factor(x)
  if (isFactor) {
    factorDF <- raster::levels(x)
  }
  # GDAL will to a reprojection without an explicit crop
  # the raster could be in memory if it wasn't reprojected
  if (inMemory(x)) {
    dType <- assessDataType(x, type = "writeRaster")
    dTypeGDAL <- assessDataType(x, type = "GDAL")

    x <- progressBarCode(writeRaster(x, filename = tempSrcRaster,
                                     datatype = dType, overwrite = TRUE),
                         doProgress = ncell(x) > 2e6,
                         message = "Writing temporary raster to disk for GDAL ...",
                         colour = getOption("reproducible.messageColourPrepInputs"),
                         verbose = verbose)
    gc()
  } else {
    tempSrcRaster <- x@file@name #Keep original raster.
    dTypeGDAL <- assessDataType(raster(tempSrcRaster), type = "GDAL")
  }
  tempSrcRaster <- normPath(tempSrcRaster)

  srcCRS <- as.character(.crs(raster::raster(tempSrcRaster)))

  needCutline <- NULL
  needReproject <- FALSE
  needNewRes <- FALSE
  tempSrcShape <- NULL
  cropExtent <- NULL

  if (!is.null(studyArea)) {
    # studyAreaCRSx <- spTransform(studyArea, crs(x))
    needCutline <- TRUE
    tempSrcShape <- normPath(file.path(tempfile(tmpdir = raster::tmpDir()), ".shp", fsep = ""))

    studyAreasf <- sf::st_as_sf(studyArea)
    if (isTRUE(useSAcrs)) {
      targCRS <- .crs(studyArea)
    } else {
      if (!is.null(rasterToMatch)) {
        targCRS <- crs(rasterToMatch)
      } else {
        targCRS <- srcCRS
      }
      studyAreasf <- sf::st_transform(studyAreasf, crs = targCRS)
    }
    # write the studyArea to disk -- go via sf because faster
    cropExtent <- extent(studyAreasf)
    if (!(grepl("longlat", targCRS)))
      cropExtent <- roundToRes(cropExtent, x = x)

    muffld <- capture.output(
      sf::st_write(studyAreasf, tempSrcShape)
    )



  } else if (!is.null(rasterToMatch)) {

    cropExtent <- extent(rasterToMatch)
    targCRS <- .crs(rasterToMatch)
  }
  dontSpecifyResBCLongLat <- isLongLat(targCRS, srcCRS)

  if (!is.null(rasterToMatch)) {
    needNewRes <- !identical(res(x), res(rasterToMatch))
  } else if (isTRUE(useSAcrs) ) {
    if (!isProjected(x))
      stop("Cannot set useSAcrs to TRUE if x is longitude and latitude; please provide a rasterToMatch")
  }

  ## GDAL requires file path to cutline - write to disk
  tr <- if (needNewRes) res(rasterToMatch) else res(x)

  if (!compareCRS(srcCRS, targCRS) ) {
    needReproject <- TRUE
  }

  if (isWindows()) {
    messagePrepInputs("Using gdal via gdalUtilities")
    exe <- ".exe"
  } else {
    exe <- ""
  }

  if (needReproject) {
    if (is.null(dots$method)) {
      dots$method <- assessDataType(x, type = "projectRaster")
    }
    if (dots$method == "ngb") {
      dots$method <- "near"
    }
  }

  if (!is.character(targCRS)) {
    targCRS <- as.character(targCRS)
  }

  #convert extent to string
  if (!is.null(cropExtent)) {
    te <-   paste(c(cropExtent[1], cropExtent[3],
                    cropExtent[2], cropExtent[4]))
  } else {
    te <- NULL
  }

  cores <- dealWithCores(cores)
  prll <- paste0("-wo NUM_THREADS=", cores, " ")

  #this is a workaround to passing NULL arguments to gdal_warp, which does not work
  gdalArgs <- list(srcfile = tempSrcRaster, dstfile = filename2, s_srs = srcCRS, te = te,
                   t_srs = targCRS, cutline = tempSrcShape, crop_to_cutline = NULL, srcnodata = NA,
                   dstnodata = NA, tr = tr, ot = dTypeGDAL, multi = TRUE, wo = prll, overwrite = TRUE)
  gdalArgs <- gdalArgs[!unlist(lapply(gdalArgs, is.null))]
  do.call(gdalUtilities::gdalwarp, gdalArgs)

  x <- raster(filename2)

  x <- setMinMaxIfNeeded(x)
  if (returnToRAM) {
    origColors <- checkColors(x)
    x[] <- x[]
    x <- rebuildColors(x, origColors)
    names(x) <- layerName
  }
  if (needRenameAtEnd) {
    x <- progressBarCode(writeRaster(x, filename = filename2Orig, overwrite = TRUE),
                         doProgress = ncell(x) > 2e6,
                         message = "Writing correct raster file to disk following GDAL ...",
                         colour = getOption("reproducible.messageColourPrepInputs"),
                         verbose = verbose)
  }

  if (isFactor) {
    levels(x) <- factorDF[[1]]
  }

  x
}

isLongLat <- function(targCRS, srcCRS = targCRS) {
  if (grepl("longlat", targCRS)) !grepl("longlat", srcCRS) else FALSE
}

.crs <- function(x, ...) {
  suppressWarningsSpecific(falseWarnings = "CRS object has comment",
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

messageRgeosMissing <- "Please run install.packages('rgeos') to address minor GIS issues"

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
