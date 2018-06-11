#' Generic function to post process objects
#'
#' There may be many methods developed. See e.g.,
#' \code{\link{postProcess.spatialObjects}}
#' @export
#' @param pp  An object of postProcessing. See individual methods.
#' @importClassesFrom quickPlot spatialObjects
#' @importFrom utils capture.output
#' @seealso \code{prepInputs}, \code{\link{postProcess.spatialObjects}}
#' @param ... Passed to internal functions. None implemented for the generic.
#' @inheritParams prepInputs
#' @rdname postProcess
#'
postProcess <- function(pp, ...) {
  UseMethod("postProcess")
}

#' @export
postProcess.default <- function(pp, ...) {
  pp
}

#' Post processing for \code{spatialObjects}
#'
#' The method for spatialObjects (\code{Raster*} and \code{Spatial*}) will
#' crop, reproject, and mask, in that order.  This function is a wrapper for
#' \code{\link{cropInputs}}, \code{\link{fixErrors}}, \code{\link{projectInputs}},
#' \code{\link{maskInputs}} and
#' \code{\link{writeOutputs}}, with a decent amount of data manipulating
#' between these calls so that the crs match.
#'
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
#' @section File naming:
#'
#'   Post processing addresses several scenarios, and depending on which scenario,
#'   file names are subtly different. For example, \code{Raster} objects may have
#'   file-backed data, and so \emph{possess a file name}, whereas \code{Spatial}
#'   objects do not. Similarly, there may or may not be a desire to write an
#'   object to disk after all post processing. This subtlety means that there are
#'   2 file names that may be at play: the "input" file name (\code{filename1}),
#'   and the "output" filename (\code{filename2}).
#'   When this is used \emph{only} with post process, it is straight forward. However,
#'   when \code{postProcess} is used within a \code{prepInputs} call, the \code{filename1}
#'   file is actually the file name of the downloaded file (i.e., what name to give to the
#'   downloaded  object) and the \code{filename2} is the
#'   file name of the of post-processed file.
#'
#'
#' @inheritParams prepInputs
#'
#' @inheritParams cropInputs
#'
#' @param pp   A \code{Spatial*}, \code{sf}, or \code{Raster*} object.
#'
#' @param filename1 Character string giving the file path of the \emph{input} object,
#'                      if it has one. This is then used if \code{filename2}
#'                      is specified here as \code{TRUE} (the default,
#'                      and passed to \code{\link{determineFilename}})
#'                      to name the output file, where the resulting
#'                      post-processed filename will be
#'                      \code{.prefix(basename(filename1), "Small")}.
#'                      Mostly used by \code{\link{prepInputs}},
#'                      where \code{filename2} is missing.
#'
#' @param useSAcrs Logical. If \code{FALSE}, the default, then the desired projection
#'                 will be taken from \code{rasterToMatch} or none at all.
#'                 If \code{TRUE}, it will be taken from \code{studyArea}.
#'
#' @param ... Additonal arguments passed to \code{\link{cropInputs}},
#'            \code{\link{projectInputs}}, \code{\link{maskInputs}},
#'            \code{\link{determineFilename}}, and \code{\link{writeOutputs}}.
#'            These then pass \code{...} into other functions, like
#'            \code{\link[raster]{writeRaster}}, or \code{sf::st_write}.
#'            This might include potentially important arguments like \code{datatype},
#'            \code{format}. Also passed to \code{projectRaster},
#'            with likely important arguments such as \code{method = "bilinear"}.
#'            See details.
#'
#' \subsection{... passed to:}{
#'   \tabular{lccc}{
#'      Function                \tab Arguments \cr
#'     \code{cropInputs}        \tab \code{\link[raster]{crop}} \cr
#'     \code{projectInputs}     \tab \code{\link[raster]{projectRaster}}\cr
#'     \code{maskInputs}        \tab \code{\link{fastMask}} or \code{\link[raster]{intersect}}\cr
#'     \code{fixErrors}         \tab \code{\link[raster]{buffer}}\cr
#'     \code{writeOutputs}      \tab \code{\link[raster]{writeRaster}} or \code{\link[raster]{shapefile}}\cr
#'     \code{determineFilename} \tab \cr
#'   }
#'   * Can be overridden with \code{useSAcrs}
#'   ** Will mask with \code{NA}s from \code{rasterToMatch} if \code{maskWithRTM}
#' }
#'
#' @section Passing \code{rasterToMatch} and/or \code{studyArea}:
#'
#' Depending on which of these were passed, different things will happen to the \code{targetFile}
#' located at \code{filename1}.
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
#'   * Can be overridden with \code{useSAcrs}
#'   ** Will mask with \code{NA}s from \code{rasterToMatch} if \code{maskWithRTM}
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
#' @examples
#'
#' # download a zip file from internet, unzip all files, load as shapefile, Cache the call
#' dPath <- file.path(tempdir(), "ecozones")
#' shpEcozone <- prepInputs(destinationPath = dPath,
#'                          url = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip")
#'
#'
#' #' # Add a study area to Crop and Mask to
#' # Create a "study area"
#' library(sp)
#' library(raster)
#' coords <- structure(c(-122.98, -116.1, -99.2, -106, -122.98, 59.9, 65.73, 63.58, 54.79, 59.9),
#'                     .Dim = c(5L, 2L))
#' Sr1 <- Polygon(coords)
#' Srs1 <- Polygons(list(Sr1), "s1")
#' StudyArea <- SpatialPolygons(list(Srs1), 1L)
#' crs(StudyArea) <- "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
#'
#'
#' ##########
#' shpEcozonePostProcessed <- postProcess(shpEcozone, studyArea = StudyArea)
#'
#' # Try manually, individual pieces
#' shpEcozoneCropped <- cropInputs(shpEcozone, StudyArea)
#' shpEcozoneClean <- fixErrors(shpEcozone)
#' shpEcozoneMasked <- maskInputs(shpEcozone, StudyArea)
#'
postProcess.spatialObjects <- function(pp, filename1 = NULL,
                                       studyArea = NULL, rasterToMatch = NULL,
                                       overwrite = TRUE, useSAcrs = FALSE,
                                       useCache = getOption("reproducible.useCache", FALSE),
                                       ...) {

  # Test if user supplied wrong type of file for "studyArea", "rasterToMatch"
  if (!is.null(studyArea) & !is(studyArea, "Spatial")) {
    stop("The 'studyArea' provided is not a Spatial* object.")
  }

  if (!is.null(rasterToMatch) & !is(rasterToMatch, "RasterLayer")) {
    stop("The 'rasterToMatch' provided is not a Raster* object.")
  }

  dots <- list(...)

  if (!is.null(dots$inputFilePath))  {
    message("inputFilePath is being deprecated; use filename1")
    filename1 <- dots$inputFilePath
    dots$inputFilePath <- NULL
  }

  if (!is.null(dots$targetFilePath))  {
    message("targetFilePath is being deprecated; use filename1.")
    filename1 <- dots$targetFilePath
    dots$targetFilePath <- NULL
  }

  if (!is.null(studyArea) || !is.null(rasterToMatch)) {

    # fix errors if methods available
    if (identical(useCache, FALSE)) {
      message("useCache is FALSE, skipping Cache during post-processing.")
    }
    skipCacheMess <- "useCache is FALSE, skipping Cache"
    skipCacheMess2 <- "No cacheRepo supplied"

    # cropInputs -- pass the extent and crs so Caching is faster than whole Raster
    if (!is.null(rasterToMatch)) {
      extRTM <- extent(rasterToMatch)
      crsRTM <- crs(rasterToMatch)
    } else {
      extRTM <- NULL
      crsRTM <- NULL
    }

    mess <- capture.output(type = "message",
                           pp <- Cache(cropInputs, ci = pp, studyArea = studyArea,
                                      extentToMatch = extRTM,
                                      extentCRS = crsRTM,
                                      useCache = useCache, ...))

    .groupedMessage(mess, omitPattern = paste(skipCacheMess, skipCacheMess2, sep = "|"))

    # cropInputs may have returned NULL if they don't overlap
    if (!is.null(pp)) {
      objectName <- if (is.null(filename1)) NULL else basename(filename1)
      mess <- capture.output(type = "message", # no Cache at the method level because may be just passed through if raster
                             pp <- fixErrors(fe = pp, objectName = objectName,
                                            useCache = useCache, ...))
      .groupedMessage(mess, omitPattern = skipCacheMess)

      # projectInputs
      targetCRS <- .getTargetCRS(useSAcrs, studyArea, rasterToMatch)

      mess <- capture.output(type = "message",
                             pp <- Cache(projectInputs, pi = pp, targetCRS = targetCRS,
                                        rasterToMatch = rasterToMatch, useCache = useCache, ...))

      .groupedMessage(mess, omitPattern = paste(skipCacheMess, skipCacheMess2, sep = "|"))

      # maskInputs
      mess <- capture.output(type = "message",
                             pp <- Cache(maskInputs, mi = pp, studyArea = studyArea,
                                        rasterToMatch = rasterToMatch, useCache = useCache, ...))

      .groupedMessage(mess, omitPattern = paste(skipCacheMess, skipCacheMess2, sep = "|"))

      # filename
      newFilename <- determineFilename(filename1 = filename1, ...)
      if (!is.null(dots$filename1)) stop("Can't pass filename1; use filename2")
      if (!is.null(dots$filename2)) dots$filename2 <- NULL

      # writeOutputs
      pp <- do.call(writeOutputs, append(list(wo = pp, filename2 = newFilename,
                                              overwrite = overwrite), dots))
    }
  }
  return(pp)
}

#' Crop a \code{Spatial*} or \code{Raster*} object
#'
#' This function can be used to crop or reproject module inputs from raw data.
#'
#' @param ci A \code{Spatial*}, \code{sf}, or \code{Raster*} object.
#'
#' @param studyArea \code{SpatialPolygons*} object used for masking and possibly cropping
#'                  if no \code{rasterToMatch} is provided.
#'                  If not in same CRS, then it will be \code{spTransform}ed to
#'                  CRS of \code{ci} before masking. Currently, this function will not reproject the
#'                  \code{ci}. Optional in \code{postProcess}. \code{\link{postProcess.spatialObjects}}
#'
#' @param rasterToMatch Template \code{Raster*} object used for cropping (so extent should be
#'                      the extent of desired outcome) and reprojecting (including changing the
#'                      resolution and projection).
#'                      See details in \code{\link{postProcess.spatialObjects}}.
#'
#' @author Eliot McIntire
#' @author Jean Marchal
#' @export
#' @importFrom methods is
#' @importFrom raster buffer crop crs extent projectRaster res crs<-
#' @importFrom rgeos gIsValid
#' @importFrom sp SpatialPolygonsDataFrame spTransform CRS
#' @rdname postProcess.spatialObjects
cropInputs <- function(ci, studyArea, rasterToMatch, ...) {
  UseMethod("cropInputs")
}

#' @export
#' @rdname postProcess.spatialObjects
cropInputs.default <- function(ci, studyArea, rasterToMatch, ...) {
  ci
}

#' @export
#' @rdname postProcess.spatialObjects
#' @importFrom raster projectExtent
#' @param extentToMatch Optional. Can pass an extent here and a \code{crs} to
#'                      \code{extentCRS} instead of \code{rasterToMatch}. These
#'                      will override \code{rasterToMatch}, with a warning if both
#'                      passed.
#' @param extentCRS     Optional. Can pass a \code{crs} here with an extent to
#'                      \code{extentTomatch} instead of \code{rasterToMatch}
cropInputs.spatialObjects <- function(ci, studyArea, rasterToMatch = NULL, extentToMatch = NULL,
                                      extentCRS = NULL, ...) {


  if (!is.null(studyArea) ||
      !is.null(rasterToMatch) || !is.null(extentToMatch)) {
    rasterToMatch <- if (!is.null(extentToMatch)) {
      raster(extentToMatch, crs = extentCRS)
    }
    cropTo <-
      if (!is.null(rasterToMatch)) {
        rasterToMatch
      } else {
        studyArea
      }

    # have to project the extent to the ci projection so crop will work -- this is temporary
    #   once cropped, then cropExtent should be rm

    cropExtent <- if (identical(crs(ci), crs(cropTo))) {
      extent(cropTo)
    } else {
      if (!is.null(rasterToMatch)) {
        projectExtent(cropTo, crs(ci))
      } else {
        if (is(studyArea, "Spatial")) {
          spTransform(x = cropTo, CRSobj = crs(ci))
        } else {
          NULL
        }
      }
    }

    if (!is.null(cropExtent)) {
      # crop it
      if (!identical(cropExtent, extent(ci))) {
        message("    cropping ...")
        dots <- list(...)
        dots[.formalsNotInCurrentDots("crop", ...)] <- NULL
        ci <- do.call(raster::crop, args = append(list(x = ci, y = cropExtent), dots))
        if (is.null(ci)) {
          message("    polygons do not intersect.")
        }
      }
    }
  }
  return(ci)
}


#' Do some minor error fixing
#'
#' These must be very common for this function to be useful. Currently, the only
#' meaningful method is on SpatialPolygons, and it runs \code{rgeos::gIsValid}. If
#' \code{FALSE}, then it runs a buffer of width 0.
#' @inheritParams prepInputs
#' @param fe Any object that could be fixed for errors.
#'          See \code{\link{fixErrors.SpatialPolygons}}
#' @export
#' @keywords internal
#' @param objectName Optional. This is only for messaging; if provided, then messages relayed
#'                   to user will mention this.
#' @param attemptErrorFixes Will attempt to fix known errors. Currently only some failures
#'        for SpatialPolygons* are attempted. Notably with \code{raster::buffer(..., width = 0)}.
#'        Default \code{TRUE}, though this may not be the right action for all cases.
#' @param useCache Logical, default \code{getOption("reproducible.useCache", FALSE)}, whether
#'                 Cache is used on the internal \code{raster::buffer} command.
fixErrors <- function(fe, objectName, attemptErrorFixes = TRUE,
                      useCache = getOption("reproducible.useCache", FALSE), ...) {
  UseMethod("fixErrors")
}

#' @export
#' @keywords internal
fixErrors.default <- function(fe, objectName, attemptErrorFixes = TRUE,
                              useCache = getOption("reproducible.useCache", FALSE), ...) {
  fe
}

#' Fix \code{rgeos::gIsValid} failures in \code{SpatialPolygons}
#'
#' This uses \code{raster::buffer(..., width = 0)} internally, which fixes some
#' failures to \code{rgeos::gIsValid}
#'
#' @export
#' @param fe A \code{SpatialPolygons} object
#' @inheritParams fixErrors
fixErrors.SpatialPolygons <- function(fe, objectName = NULL,
                                      attemptErrorFixes = TRUE,
                                      useCache = getOption("reproducible.useCache", FALSE), ...) {
  if (attemptErrorFixes) {
    if (is.null(objectName)) objectName = "SpatialPolygon"
    if (is(fe, "SpatialPolygons")) {
      message("Checking for errors in ", objectName)
      if (suppressWarnings(any(!rgeos::gIsValid(fe, byid = TRUE)))) {
        message("Found errors in ", objectName, ". Attempting to correct.")
        x1 <- try(Cache(raster::buffer, fe, width = 0, dissolve = FALSE, useCache = useCache))
        if (is(x1, "try-error")) {
          message("There are errors with ", objectName,
                  ". Couldn't fix them with raster::buffer(..., width = 0)")
        } else {
          fe <- x1
          message("  Some or all of the errors fixed.")
        }

      } else {
        message("  Found no errors.")
      }
    }
  }
  return(fe)
}


#' Project \code{Raster*} or {Spatial*} or \code{sf} objects
#'
#' A simple wrapper around the various different tools for these GIS types.
#'
#' @export
#' @param pi A \code{Raster*}, \code{Spatial*} or \code{sf} object
#' @param targetCRS The CRS of pi at the end  of this function (i.e., the goal)
#'
#' @rdname postProcess.spatialObjects
#' @return
#' A file of the same type as starting, but with projection (and possibly other
#' characteristics, including resolution, origin, extent if changed.
projectInputs <- function(pi, targetCRS, ...) {
  UseMethod("projectInputs")
}

#' @export
#' @rdname postProcess.spatialObjects
projectInputs.Raster <- function(pi, targetCRS = NULL, rasterToMatch = NULL, ...) {

  if (!is.null(rasterToMatch)) {
    if (!is.null(targetCRS)) {
      if (!identical(crs(pi), targetCRS) |
          !identical(res(pi), res(rasterToMatch)) |
          !identical(extent(pi), extent(rasterToMatch))) {
        message("    reprojecting ...")
        pi <- projectRaster(from = pi, to = rasterToMatch, ...)
      } else {
        message("    no reprojecting because target characteristics same as input Raster.")
      }
    } else {
      message("    no reprojecting because no rasterToMatch & useSAcrs are FALSE.")
    }
  } else {
    message("    no reprojecting because no rasterToMatch.")
  }
  pi
}

#' @export
#' @rdname postProcess.spatialObjects
projectInputs.sf <- function(pi, targetCRS, ...) {
  warning("sf class objects not fully implemented. Use with projectInputs.sf caution.")
  if (requireNamespace("sf")) {
    if (any(sf::st_is(pi, c("POLYGON", "MULTIPOLYGON"))) && !any(isValid <- sf::st_is_valid(pi))) {
      pi[!isValid] <- sf::st_buffer(pi[!isValid], dist = 0, ...)
    }

    pi <- sf::st_transform(x = pi, crs = sf::st_crs(targetCRS@projargs), ...)

  } else {
    stop("Please install sf package: https://github.com/r-spatial/sf")
  }
}

#' @export
#' @rdname postProcess.spatialObjects
projectInputs.Spatial <- function(pi, targetCRS, ...) {
  if (!is.null(targetCRS)) {
    pi <- spTransform(x = pi, CRSobj = targetCRS)
  }
  pi
}

#' Hierachically get crs from \code{Raster*}, \code{Spatial*}
#'
#' This is the function that follows the table of order of
#' preference for determining CRS. See \code{\link{postProcess.spatialObjects}}
#' @inheritParams postProcess.spatialObjects
#' @rdname postProcessHelpers
#' @keywords internal
.getTargetCRS <- function(useSAcrs, studyArea, rasterToMatch) {
  targetCRS <- if (useSAcrs) {
    crs(studyArea)
  } else if (!is.null(rasterToMatch)) {
    crs(rasterToMatch)
  } else {
    NULL # don't reproject a Raster if only has studyArea -- too lossy
  }
  targetCRS
}

#' Mask module inputs
#'
#' This function can be used to mask module inputs from raw data.
#'
#' @param mi          A \code{Raster*} object
#'
#' @author Eliot McIntire
#' @author Jean Marchal
#' @export
#' @inheritParams cropInputs
#' @importFrom utils capture.output
#' @rdname postProcess.spatialObjects
#'
maskInputs <- function(mi, studyArea, ...) {
  UseMethod("maskInputs")
}

#' @export
#' @param maskWithRTM Logical. If \code{TRUE}, then the default,
#' @rdname postProcess.spatialObjects
maskInputs.Raster <- function(mi, studyArea, rasterToMatch, maskWithRTM = FALSE, ...) {

  message("    masking...")
  if (isTRUE(maskWithRTM)) {
    mi[is.na(rasterToMatch)] <- NA
  } else {
    if (!is.null(studyArea)) {
      msg <- capture.output(type = "message",
                            mi <- fastMask(x = mi, y = studyArea))
      message(paste0("      ", paste(msg, collapse = "\n      ")))
    } else {
      message("studyArea not provided, skipping masking.")
    }
  }
  return(mi)
}

#' @export
#' @rdname postProcess.spatialObjects
maskInputs.Spatial <- function(mi, studyArea, ...) {

  if (!is.null(studyArea)) {
    message("    intersecting ...")
    studyArea <- raster::aggregate(studyArea, dissolve = TRUE)
    studyArea <- spTransform(studyArea, CRSobj = crs(mi))
    suppressWarnings(studyArea <- fixErrors(studyArea, "studyArea"))
    mi <- tryCatch(raster::intersect(mi, studyArea), error = function(e) {
      warning("  Could not mask with studyArea, for unknown reasons.",
              " Returning object without masking.")
      return(mi)
    }
    )
    return(mi)
  } else {
    message("studyArea not provided, skipping masking.")
    return(mi)
  }
}

#' Determine filename, either automatically or manually
#'
#' If \code{filename2} is \code{logical}, then the cropped/masked
#' raster will be written to disk with the original \code{targetFile} name, with
#' \code{"Small"} prefixed to the basename(\code{targetFilename}).
#' If a character string, it will be the path of the saved raster.
#' It will be tested whether it is an absolute or relative path and used as is
#' if absolute or prepended with \code{destinationPath} if relative.
#'
#' @inheritParams postProcess.spatialObjects
#'
#' @param filename2 Logical or character string (a file path) for the
#'                  output file, after post processing, if saving is desired. See details.
#'
#' @param destinationPath Optional. If \code{filename2} is a relative file path, then this
#'                        will be the directory of the resulting absolute file path.
#'
#' @include helpers.R
#'
#' @details
#'  If \code{filename2} is \code{logical}, then the output
#'  filename will be \code{"Small"} prefixed to the basename(\code{filename1}).
#'  If a character string, it
#'  will be the path returned. It will be tested whether it is an
#'  absolute or relative path and used as is if absolute or prepended with
#'  \code{destinationPath} if provided, and if \code{filename2} is relative.
#'
#' @rdname postProcess.spatialObjects
determineFilename <- function(filename2 = TRUE, filename1 = NULL,
                              destinationPath = NULL, ...) {

  dots <- list(...)

  if (!is.null(dots$inputFilePath))  {
    message("inputFilePath is being deprecated; use filename1")
    filename1 <- dots$inputFilePath
    dots$inputFilePath <- NULL
  }

  if (!is.null(dots$postProcessedFilename))  {
    message("postProcessedFilename is being deprecated; use filename2")
    filename2 <- dots$postProcessedFilename
    dots$postProcessedFilename <- NULL
  }

  if (!is.null(dots$targetFilePath))  {
    message("targetFilePath is being deprecated from determineFilename:\n",
            "  use filename2 and filename1.")
    if (is.null(filename1)) {
      filename1 <- dots$targetFilePath
      dots$targetFilePath <- NULL
    }
  }

  if (!(is.logical(filename2) || is.character(filename2))) {
    stop("filename2 must be logical or character string")
  }

  newFilename <- if (!identical(filename2, FALSE)) { # allow TRUE or path
    if (isTRUE(filename2) ) {
      if (is.null(filename1)) {
        tmpfile <- basename(tempfile())
        message("Please provide filename2; will use: ", tmpfile)
        filename1 <- tmpfile
      }
      .prefix(filename1, "Small")
    } else {
      if (isAbsolutePath(filename2)) {
        filename2
      } else {
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
  newFilename
}

#' Write module inputs on disk
#'
#' Can be used to write prepared inputs on disk.
#'
#' @inheritParams postProcess
#' @param wo  The object save to disk i.e., write outputs
#' @param overwrite Logical. Should file being written overwrite an existing file if it
#'                  exists.
#' @author Eliot McIntire
#' @author Jean Marchal
#' @export
#' @importFrom methods is
#' @importFrom raster shapefile writeRaster
#' @rdname postProcess.spatialObjects
#'
writeOutputs <- function(wo, filename2, overwrite, ...) {
  UseMethod("writeOutputs")
}

#' @rdname postProcess.spatialObjects
writeOutputs.Raster <- function(wo, filename2, overwrite = FALSE, ...) {
  if (!is.null(filename2)) {
    xTmp <- writeRaster(x = wo, filename = filename2, overwrite = overwrite, ...)

    # This is a bug in writeRaster was spotted with crs of xTmp became
    # +proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs
    # should have stayed at
    # +proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0
    if (!identical(crs(xTmp), crs(wo)))
      crs(xTmp) <- crs(wo)

    wo <- xTmp
  }
  wo
}

#' @rdname postProcess.spatialObjects
writeOutputs.Spatial <- function(wo, filename2, overwrite = FALSE, ...) {
  if (!is.null(filename2)) {
    shapefile(x = wo, filename = filename2, overwrite = overwrite)
  }
  wo
}

#' @rdname postProcess.spatialObjects
writeOutputs.sf <- function(wo, filename2, overwrite = FALSE, ...) {
  if (!is.null(filename2)) {
    if (requireNamespace("sf")) {
      wo <- sf::st_write(obj = wo, delete_dsn = TRUE, dsn = filename2, delete_dsn = overwrite)
    } else {
      stop("Please install sf package: https://github.com/r-spatial/sf")
    }
  }
  wo
}

#' @rdname postProcess.spatialObjects
writeOutputs.default <- function(wo, filename2, ...) {
  stop("Don't know how to write object of class ", class(wo), " on disk.")
}

