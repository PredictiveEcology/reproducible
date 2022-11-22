#' Generic function to post process objects
#'
#' \if{html}{\figure{lifecycle-maturing.svg}{options: alt="maturing"}}
#'
#' @export
#' @param x  An object of postProcessing, e.g., returns `TRUE` with `isSpatialAny(x)`.
#'           See individual methods. This can be provided as a
#'           \code{rlang::quosure} or a normal R object.
#' @importFrom utils capture.output
#' @seealso \code{prepInputs}
#' @inheritParams prepInputs
#' @rdname postProcess
postProcess <- function(x, filename1 = NULL, filename2 = NULL,
                        studyArea = NULL, rasterToMatch = NULL,
                        overwrite = getOption("reproducible.overwrite", TRUE),
                        useSAcrs = FALSE,
                        useCache = getOption("reproducible.useCache", FALSE),
                        verbose = getOption("reproducible.verbose", 1),
                        ...) {
  UseMethod("postProcess")
}



#' Post processing for spatial objects
#'
#' The method for spatial objects (i.e., returns \code{TRUE} with `isSpatialAny(x)`)
#' will crop, reproject, and mask, in that order.
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
#' @param ... Additional arguments passed to internal methods, which are:
#'            \code{\link{cropInputs}}, \code{\link{fixErrors}},
#'            \code{\link{projectInputs}}, \code{\link{maskInputs}},
#'            \code{\link{determineFilename}}, and \code{\link{writeOutputs}}.
#'            Each of these may also pass \code{...} into other functions, like
#'            \code{\link[terra]{writeRaster}}, or \code{sf::st_write}.
#'            This might include potentially important arguments like \code{datatype},
#'            \code{format}. Also passed to \code{projectRaster},
#'            with likely important arguments such as \code{method = "bilinear"}.
#'            See details.
#'
#' \subsection{... passed to:}{
#'   \describe{
#'     \item{\code{cropInputs}:}{\code{\link[terra]{crop}}}
#'     \item{\code{projectInputs}}{\code{\link[terra]{project}}}
#'     \item{\code{maskInputs}}{\code{\link{fastMask}} or \code{\link[terra]{intersect}}}
#'     \item{\code{fixErrors}}{\code{\link[terra]{buffer}}}
#'     \item{\code{writeOutputs}}{\code{\link[terra]{writeRaster}} or \code{\link[terra]{vect}}}
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
#' @example inst/examples/example_postProcess.R
#' @rdname postProcess
postProcess.default <- function(x, filename1 = NULL, filename2 = NULL,
                                       studyArea = NULL, rasterToMatch = NULL,
                                       overwrite = getOption("reproducible.overwrite", TRUE),
                                       useSAcrs = FALSE,
                                       useCache = getOption("reproducible.useCache", FALSE),
                                       verbose = getOption("reproducible.verbose", 1),
                                       ...) {

  if (inherits(x, "quosure")) {
    x <- eval_tidy(x)
  }

  if (is(x, "list")) {
    x <- lapply(x, function(y) postProcess(y, ...))
  }


  if (isSpatialAny(x)) {
    .requireNamespace("terra", stopOnFALSE = TRUE)
    .requireNamespace("raster", stopOnFALSE = TRUE)
    on.exit(raster::removeTmpFiles(h = 0), add = TRUE)
    x <- postProcessTerra(from = x, studyArea = studyArea,
                          rasterToMatch = rasterToMatch, useCache = useCache,
                          filename1 = filename1, filename2 = filename2,
                          useSAcrs = useSAcrs, overwrite = overwrite,
                          verbose = verbose, ...)
  }
  return(x)
}




#' Crop a \code{Spatial*} or \code{Raster*} object
#'
#' This function can be used to crop or reproject module inputs from raw data.
#'
#' @param x A \code{Spatial*}, \code{sf}, or \code{Raster*} object.
#'
#' @inheritParams projectInputs
#'
#' @param ... Passed to \code{terra::crop}
#'
#' @inheritParams projectInputs
#' @inheritParams Cache
#'
#' @author Eliot McIntire, Jean Marchal, Ian Eddy, and Tati Micheletti
#' @example inst/examples/example_postProcess.R
#' @export
#' @importFrom methods is
#' @rdname cropInputs
cropInputs <- function(x, studyArea, rasterToMatch, verbose = getOption("reproducible.verbose", 1), ...) {
  UseMethod("cropInputs")
}

#' @export
#' @rdname cropInputs
cropInputs.default <- function(x, studyArea, rasterToMatch, ...) {

  if (missing(rasterToMatch)) rasterToMatch <- NULL

  if (isSpatialAny(x) && (isSpatialAny(studyArea) || isSpatialAny(rasterToMatch))) {
    cropTo <- if (!is.null(rasterToMatch)) {
      rasterToMatch
    } else {
      if (is.na(.crs(studyArea)))
        stop("studyArea does not have a crs")
      studyArea
    }
    x <- cropTo(from = x, cropTo = cropTo)
  }

  x
}

#' Do some minor error fixing
#'
#' These must be very common for this function to be useful. Currently, the only
#' meaningful method is on \code{SpatialPolygons}, and it runs \code{sf::st_is_valid}.
#' If \code{FALSE}, then it runs  \code{st_make_valid} or \code{terra::buffer},
#' depending on whether x is \code{sf} or \code{SpatialPolygons*}, respectively.
#'
#' @param x A \code{SpatialPolygons*} or \code{sf} object.
#'
#' @param objectName Optional. This is only for messaging; if provided, then messages relayed
#'                   to user will mention this.
#'
#' @param attemptErrorFixes Will attempt to fix known errors. Currently only some failures
#'        for \code{SpatialPolygons*} are attempted.
#'        Notably with \code{terra::buffer(..., width = 0)}.
#'        Default \code{TRUE}, though this may not be the right action for all cases.
#' @param useCache Logical, default \code{getOption("reproducible.useCache", FALSE)}, whether
#'                 Cache is used on the internal \code{terra::buffer} command.
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
  fixErrorsTerra(x)
}

#' @keywords internal
#' @rdname fixErrors
fixErrorsRaster <- function(x, objectName, attemptErrorFixes = TRUE,
                             useCache = getOption("reproducible.useCache", FALSE),
                             verbose = getOption("reproducible.verbose", 1),
                             testValidity = getOption("reproducible.testValidity", TRUE),
                             ...) {

  #rounding lon lat resolution will break the raster
  .requireNamespace("terra", stopOnFALSE = TRUE)
  if (!terra::is.lonlat(x)) {
    terra::origin(x) <- roundTo6Dec(terra::origin(x))
    terra::xmin(x) <- roundTo6Dec(terra::xmin(x))
    terra::ymin(x) <- roundTo6Dec(terra::ymin(x))
    terra::xmax(x) <- roundTo6Dec(terra::xmax(x))
    terra::ymax(x) <- roundTo6Dec(terra::ymax(x))
    terra::res(x) <- roundTo6Dec(terra::res(x))
  }
  x
}

#' Fix \code{sf::st_is_valid} failures in \code{SpatialPolygons}
#'
#' This uses \code{terra::buffer(..., width = 0)} internally, which fixes some
#' failures to \code{sf::st_is_valid}
#'
#' @rdname fixErrors
fixErrorsSpatialPolygons <- function(x, objectName = NULL,
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
        # if (!requireNamespace("rgeos", quietly = TRUE)) stop(messageRgeosMissing)
        messagePrepInputs("      Trying the buffer = 0 trick", verbose = verbose, verboseLevel = 2)
        # prevent the warning about not projected, because we are buffering 0, which doesn't matter
        x1 <-
          suppressWarningsSpecific(falseWarnings = paste("Spatial object is not projected;",
                                                         "GEOS expects planar coordinates"),
                                   try(as(Cache(terra::buffer, terra::vect(x), width = 0,
                                             # dissolve = FALSE,
                                             useCache = useCache), "Spatial"))#,
          )

        x <- bufferWarningSuppress(#warn = attr(x1, "warning"),
          objectName = objectName,
          x1 = x1, bufferFn = "terra::buffer")
      } else {
        messagePrepInputs("  Found no errors.", verbose = verbose)
      }
    }
  }
  return(x)
}

#' @rdname fixErrors
fixErrorssf <- function(x, objectName = NULL, attemptErrorFixes = TRUE,
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
                                       try(Cache(sf::st_make_valid, x, useCache = useCache)))
        x <- bufferWarningSuppress(#warn = attr(x1, "warning"),
          objectName = objectName,
          x1 = x1, bufferFn = "sf::st_make_valid")

      } else {
        messagePrepInputs("  Found no errors.", verbose = verbose)
      }
    }
  }
  return(x)
}

#' Project \code{Raster*} or \code{Spatial*} or \code{sf} objects
#'
#' A simple wrapper around the various different tools for these GIS types.
#'
#' @param x A \code{Raster*}, \code{Spatial*} or \code{sf} object
#'
#' @param targetCRS The CRS of x at the end  of this function (i.e., the goal)
#'
#' @param rasterToMatch Template \code{Raster*} or \code{SpatRast} object passed to
#'                      the \code{to} argument of
#'                      \code{\link[terra]{project}}, thus will changing the
#'                      resolution and projection of \code{x}.
#'                      See details in \code{\link{postProcess}}.
#'
#' @param studyArea Template \code{SpatialPolygons*} or \code{SpatVect} object
#'                  passed to the \code{to} argument of
#'                  \code{\link[terra]{project}}, thus will changing the
#'                  resolution and projection of \code{x}.
#'                  See details in \code{\link{postProcess}}.
#'
#' @param ... Passed to \code{\link[terra]{project}}.
#'
#' @return A file of the same type as starting, but with projection (and possibly
#' other characteristics, including resolution, origin, extent if changed).
#'
#' @export
#' @inheritParams postProcess
#' @inheritParams Cache # for verbose
#' @rdname projectInputs
#'
#' @example inst/examples/example_postProcess.R
projectInputs <- function(x, rasterToMatch = NULL, targetCRS, studyArea = NULL, verbose = getOption("reproducible.verbose", 1), ...) {
  UseMethod("projectInputs")
}

#' @export
#' @rdname projectInputs
projectInputs.default <- function(x, rasterToMatch = NULL, targetCRS = NULL, studyArea = NULL, ...) {

  postProcessTerra(x, projectTo = rasterToMatch, targetCRS = targetCRS, studyArea = studyArea)
}



#' Hierarchically get crs from \code{Raster*}, \code{Spatial*}
#'
#' This is the function that follows the table of order of
#' preference for determining CRS. See \code{\link{postProcess}}
#' @inheritParams postProcess.default
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
#' equivalent to \code{terra::mask} (though \code{\link{fastMask}} is used here)
#' or \code{terra::intersect}.
#'
#' @param x An object to do a geographic terra::mask/terra::intersect.
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

#' @export
maskInputs.default <- function(x, studyArea = NULL, rasterToMatch = NULL, maskWithRTM = NULL,
                             verbose = getOption("reproducible.verbose", 1), ...) {
  maskTo <- if (isTRUE(maskWithRTM)) rasterToMatch else studyArea
  postProcessTerra(x, maskTo = maskTo, ...)
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
#' @inheritParams postProcess.default
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
#' @param filename2 File name passed to \code{\link[terra]{writeRaster}}, or
#'                  \code{\link[terra]{vect}} or \code{\link[sf]{st_write}}
#'                  (\code{dsn} argument).
#'
#' @param ... Passed into \code{\link[terra]{vect}} or
#'             \code{\link[terra]{writeRaster}} or \code{\link[sf]{st_write}}
#'
#' @inheritParams prepInputs
#'
#' @author Eliot McIntire and Jean Marchal
#' @export
#' @importFrom methods is
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

    if (any(terra::is.factor(x))) {
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
    if (!terra::inMemory(x) && all(dots$datatype == terra::datatype(x))) {
      theFilename <- Filenames(x, allowMultiple = FALSE)
      if (fileExt(theFilename) == "grd") {
        if (!fileExt(filename2) == "grd") {
          if (fileExt(filename2) != ""){
            warning("filename2 file type (", fileExt(filename2), ") was not same type (",
                    fileExt(Filenames(x)),") ", "as the filename of the raster; ",
                    "Changing filename2 so that it is ", fileExt(Filenames(x)))
            # ^^ This doesn't Work for rasterStack
            filename2 <- gsub(fileExt(filename2), "grd", filename2)
          } else {
            if (!is(x, "Raster")) {
              stop("The object has no file extension and is not a Raster* class object. Please debug")
              # Catch for weird cases where the filename is not present and the
              # object is NOT a RasterStack
            }
            if (!identical(fileExt(Filenames(x[[1]])), fileExt(theFilename))) {
              warning("filetype of filename2 provided (", fileExt(filename2),") does not ",
                      "match the filetype of the object; ",
                      "Changing filename2 so that it is ", fileExt(Filenames(x[[1]])))
            }
            filename2 <- paste0(filename2, ".grd")
          }
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
      x <- updateFilenameSlots(x, curFilenames = theFilename, newFilenames = filename2)
    } else {
      argsForWrite <- append(list(filename = filename2, overwrite = overwrite), dots)
      if (is(x, "RasterStack")) {
        longerThanOne <- unlist(lapply(argsForWrite, function(x) length(unique(x)) > 1))
        nLayers <- nlyr2(x)
        if (any(unlist(longerThanOne))) {
          if (!identical(nLayers, length(argsForWrite$filename))) {
            argsForWrite$filename <- file.path(dirname(argsForWrite$filename),
                                               paste0(names(x), "_", basename(argsForWrite$filename)))
          }
        }
        if (length(argsForWrite$filename) == 1) {
          argsForWrite <- lapply(argsForWrite, function(x) x[1])
          xTmp <- do.call(terra::writeRaster, args = c(x = x, argsForWrite))
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
            inside <- progressBarCode(do.call(terra::writeRaster, args = c(x = x[[ind]], lapply(argsForWrite, function(y) y[ind]))),
                                      doProgress = terra::ncell(x) > 2e6,
                                      message = c("Writing ", argsForWrite$filename[ind], " to disk ..."),
                                      colour = getOption("reproducible.messageColourPrepInputs"),
                                      verbose = verbose)
            names(inside) <- names(x)[ind]
            inside
          })

        } else {
          stop("filename2 must be length 1 or length nlayers(...)")
        }
        xTmp <- terra::rast(xTmp)
      } else {
        if (file.exists(argsForWrite$filename)) {
          if (interactive() && isFALSE(argsForWrite$overwrite)) {
            wantOverwrite <- readline(paste0("File ", argsForWrite$filename, " already exists; overwrite? Y or N: "))
            if (identical(tolower(wantOverwrite), "y"))
              argsForWrite$overwrite <- TRUE
          }
        }
        origColors <- checkColors(x)
        xTmp <- progressBarCode(do.call(terra::writeRaster, args = c(x = x, argsForWrite)),
                                doProgress = terra::ncell(x) > 2e6,
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
        terra::crs(xTmp) <- .crs(x)

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
    if (!grepl(".shp$", fileExt(filename2))) {
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
#'             data type for writing from the terra packages, respectively, or
#'             \code{"projectRaster"} to return recommended resampling type.
#' @return The appropriate data type for the range of values in \code{ras}.
#'         See \code{\link[raster]{dataType}} for details.
#'
#' @author Eliot McIntire
#' @author Ceres Barros
#' @author Ian Eddy
#' @author Eliot McIntire
#' @export
#' @rdname assessDataType
#'
#' @example inst/examples/example_assessDataType.R
assessDataType <- function(ras, type = 'writeRaster') {
  UseMethod("assessDataType")
}

#' @export
#' @rdname assessDataType
assessDataTypeRaster <- function(ras, type = "writeRaster") {
  ## using ras@data@... is faster, but won't work for @values in large rasters
  N <- 1e5

  # browser(expr = exists("._assessDataType_1"))
  datatype <- NULL
  .requireNamespace("terra", stopOnFALSE = TRUE)
  .requireNamespace("raster", stopOnFALSE = TRUE)
  if (terra::ncell(ras) > 1e8) { # for very large rasters, try a different way
    maxValCurrent <- maxValue2(ras)
    ras <- setMinMaxIfNeeded(ras)
    # if (maxValCurrent != maxValue(ras))
    datatype <- terra::datatype(ras)
  } else {
    ras <- setMinMaxIfNeeded(ras)
  }

  if (is.null(datatype)) {

    if (terra::ncell(ras) > N) {
      rasVals <- tryCatch(suppressWarnings(terra::spatSample(x = ras, size = N, method = "random")),
                          error = function(x) rep(NA_integer_, N))
    } else {
      rasVals <- terra::values(ras)
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
assessDataTypeRasterStack <- function(ras, type = "writeRaster") {
  xs <- lapply(names(ras), FUN = function(x){
    y <- assessDataType(ras = ras[[x]], type)
    return(y)})

  return(unlist(xs))
}

#' @export
#' @rdname assessDataType
assessDataType.default <- function(ras, type = "writeRaster") {
  if (inherits(ras, "RasterStack")) {
    adt <- assessDataTypeRasterStack(ras, type)
  } else if (inherits(ras, "Raster")) {
    adt <- assessDataTypeRaster(ras, type)
  } else {
    stop("No method for assessDataType for class ", class(ras))
  }
  return(adt)

}

#' Assess the appropriate raster layer data type for GDAL
#'
#' This is a convenience function around \code{assessDataType(ras, type = "GDAL")}
#'
#' @param ras  The RasterLayer or RasterStack for which data type will be assessed.
#' @return The appropriate data type for the range of values in \code{ras} for using GDAL.
#'         See \code{\link[terra]{datatype}} for details.
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
    messagePrepInputs("  Some or all of the errors fixed.", verbose = verbose)
  }
  x1
}

setMinMaxIfNeeded <- function(ras) {
  # special case where the colours already match the discrete values
  suppressWarnings(maxValCurrent <- maxValue2(ras)) # 2nd is max
  needSetMinMax <- FALSE
  if (isTRUE(any(is.na(maxValCurrent)))) {
    needSetMinMax <- TRUE
  } else {

    # if the colors are set and are the same length of the integer sequence between min and max, don't override
    if (length(.getColors(ras)[[1]])) {
      if (!is.na(suppressWarnings(maxValue2(ras))) && !is.na(suppressWarnings(minValue2(ras))))
        if (length(.getColors(ras)[[1]]) == (maxValue2(ras) - minValue2(ras) + 1)) {
          return(ras)
        }
    }
    possibleShortCut <- maxValCurrent %in% c(unlist(MaxVals), unlist(MaxVals) + 1)
    if (isTRUE(all(possibleShortCut))) {
      needSetMinMax <- TRUE
    }
  }
  if (isTRUE(needSetMinMax)) {
    large <- if (nlyr2(ras) > 25 || terra::ncell(ras) > 1e7) TRUE else FALSE
    if (large) message("  Large ",class(ras), " detected; setting minimum and maximum may take time")
    suppressWarnings(ras <- terra::setMinMax(ras))
    if (large) message("  ... Done")
  }
  ras
}

# Some functions where terra and raster are not compatible with a single terra function
terraOrRaster <- function(ras, terraFn = "nlyr", rasterFn = "nlayers",
                          terraIndex, rasterIndex) {
  isRaster <- inherits(ras, "Raster")
  if (isRaster) {
    if (.requireNamespace("raster", stopOnFALSE = TRUE))
      out <- getFromNamespace(rasterFn, "raster")(ras)
    if (!missing(rasterIndex))
      out[rasterIndex]
  } else {
    out <- getFromNamespace(terraFn, "terra")(ras)
    if (!missing(terraIndex))
      out[terraIndex]
  }
  out
}

# These are a set of terra or raster pkg fns
nlyr2 <- function(ras) terraOrRaster(ras, "nlyr", "nlayers")

ncell2 <- function(ras) terraOrRaster(ras, "ncell", "ncell")

maxValue2 <- function(ras) terraOrRaster(ras, "minmax", "maxValue", terraIndex = 2)

minValue2 <- function(ras) terraOrRaster(ras, "minmax", "minValue", terraIndex = 1)

# nlyr2 <- function(ras) {
#   isRaster <- is(ras, "Raster")
#   if (isRaster) {
#     if (.requireNamespace("raster", stopOnFALSE = TRUE))
#       out <- raster::nlayers(ras)
#   } else {
#     out <- terra::nlyr(ras)
#   }
#   out
# }
#
# maxValue2 <- function(ras) {
#   isRaster <- is(ras, "Raster")
#   if (isRaster) {
#     if (.requireNamespace("raster", stopOnFALSE = TRUE))
#       out <- raster::maxValue(ras)
#   } else {
#     out <- terra::minmax(ras)[2]
#   }
#   out
# }
#
# minValue2 <- function(ras) {
#   isRaster <- is(ras, "Raster")
#   if (isRaster) {
#     if (.requireNamespace("raster", stopOnFALSE = TRUE))
#       out <- raster::minValue(ras)
#   } else {
#     out <- terra::minmax(ras)[1]
#   }
#   out
# }

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




isLongLat <- function(targCRS, srcCRS = targCRS) {
  if (grepl("longlat", targCRS)) !grepl("longlat", srcCRS) else FALSE
}

.crs <- function(x, ...) {
  .requireNamespace("terra", stopOnFALSE = TRUE)
  suppressWarningsSpecific(falseWarnings = "CRS object has comment",
                           terra::crs(x, ...))
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
    txt <- suppressWarningsSpecific(falseWarnings = "no wkt comment", sp::wkt(x))
  }

  if (identical(nchar(txt), 0L) || is.null(txt)) {
    txt <- .crs(x)
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

