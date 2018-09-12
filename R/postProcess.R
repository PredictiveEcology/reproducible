#' Generic function to post process objects
#'
#' @export
#' @param x  An object of postProcessing, e.g., \code{spatialObjects}.
#'           See individual methods.
#' @importClassesFrom quickPlot spatialObjects
#' @importFrom utils capture.output
#' @seealso \code{prepInputs}
#' @inheritParams prepInputs
#' @rdname postProcess
#'
postProcess <- function(x, ...) {
  UseMethod("postProcess")
}

#' @export
postProcess.default <- function(x, ...) {
  x
}

#' @export
postProcess.list <- function(x, ...) {
  lapply(x, function(y) postProcess(y, ...))
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
#' @param ... Additional arguments passed to methods. For \code{spatialObjects},
#'            these are: \code{\link{cropInputs}},
#'            \code{\link{fixErrors}},
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
#' @example inst/examples/example_postProcess.R
#' @rdname postProcess
postProcess.spatialObjects <- function(x, filename1 = NULL, filename2 = TRUE,
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

    x <- Cache(cropInputs, x = x, studyArea = studyArea,
               extentToMatch = extRTM,
               extentCRS = crsRTM,
               useCache = useCache, ...)

    # cropInputs may have returned NULL if they don't overlap
    if (!is.null(x)) {
      objectName <- if (is.null(filename1)) NULL else basename(filename1)
      #mess <- capture.output(type = "message", # no Cache at the method level because may be just passed through if raster
                             x <- fixErrors(x = x, objectName = objectName,
                                            useCache = useCache, ...)
                             #)
      #.groupedMessage(mess, omitPattern = skipCacheMess)

      # projectInputs
      targetCRS <- .getTargetCRS(useSAcrs, studyArea, rasterToMatch)

      #mess <- capture.output(type = "message",
                             x <- Cache(projectInputs, x = x, targetCRS = targetCRS,
                                        rasterToMatch = rasterToMatch, useCache = useCache, ...)
                             #)

      #.groupedMessage(mess, omitPattern = paste(skipCacheMess, skipCacheMess2, sep = "|"))

      # maskInputs
      #mess <- capture.output(type = "message",
                             x <- Cache(maskInputs, x = x, studyArea = studyArea,
                                        rasterToMatch = rasterToMatch, useCache = useCache, ...)
                             #)

      #.groupedMessage(mess, omitPattern = paste(skipCacheMess, skipCacheMess2, sep = "|"))

      # filename
      newFilename <- determineFilename(filename1 = filename1, filename2 = filename2, ...)

      # writeOutputs
      x <- do.call(writeOutputs, append(list(x = x, filename2 = newFilename,
                                              overwrite = overwrite), dots))
    }
  }
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
#' @param ... Passed to raster::crop
#' @author Eliot McIntire
#' @author Jean Marchal
#' @export
#' @importFrom methods is
#' @importFrom raster buffer crop crs extent projectRaster res crs<-
#' @importFrom rgeos gIsValid
#' @importFrom sp SpatialPolygonsDataFrame spTransform CRS
#' @rdname cropInputs
#' @example inst/examples/example_postProcess.R
cropInputs <- function(x, studyArea, rasterToMatch, ...) {
  UseMethod("cropInputs")
}

#' @export
#' @rdname cropInputs
cropInputs.default <- function(x, studyArea, rasterToMatch, ...) {
  x
}

#' @export
#' @rdname cropInputs
#' @importFrom raster projectExtent
#' @param extentToMatch Optional. Can pass an extent here and a \code{crs} to
#'                      \code{extentCRS} instead of \code{rasterToMatch}. These
#'                      will override \code{rasterToMatch}, with a warning if both
#'                      passed.
#' @param extentCRS     Optional. Can pass a \code{crs} here with an extent to
#'                      \code{extentTomatch} instead of \code{rasterToMatch}
cropInputs.spatialObjects <- function(x, studyArea = NULL, rasterToMatch = NULL, extentToMatch = NULL,
                                      extentCRS = NULL, ...) {

  if (!is.null(studyArea) ||
      !is.null(rasterToMatch) || !is.null(extentToMatch)) {
    if (!is.null(extentToMatch)) {
      rasterToMatch <- raster(extentToMatch, crs = extentCRS)
    }
    cropTo <-
      if (!is.null(rasterToMatch)) {
        rasterToMatch
      } else {
        studyArea
      }

    # have to project the extent to the x projection so crop will work -- this is temporary
    #   once cropped, then cropExtent should be rm

    cropExtent <- if (identical(crs(x), crs(cropTo))) {
      extent(cropTo)
    } else {
      if (!is.null(rasterToMatch)) {
        projectExtent(cropTo, crs(x))
      } else {
        if (is(studyArea, "Spatial")) {
          spTransform(x = cropTo, CRSobj = crs(x))
        } else {
          NULL
        }
      }
    }

    if (!is.null(cropExtent)) {
      # crop it
      if (!identical(cropExtent, extent(x))) {
        message("    cropping ...")
        dots <- list(...)
        dots[.formalsNotInCurrentDots("crop", ...)] <- NULL
        x <- do.call(raster::crop, args = append(list(x = x, y = cropExtent), dots))
        if (is.null(x)) {
          message("    polygons do not intersect.")
        }
      }
    }
  }
  return(x)
}


#' Do some minor error fixing
#'
#' These must be very common for this function to be useful. Currently, the only
#' meaningful method is on SpatialPolygons, and it runs \code{rgeos::gIsValid}. If
#' \code{FALSE}, then it runs a buffer of width 0.
#' @inheritParams prepInputs
#' @param x Any object that could be fixed for errors.
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
#' @param ... Passed to methods. None currently implemented.
#' @example inst/examples/example_postProcess.R
fixErrors <- function(x, objectName, attemptErrorFixes = TRUE,
                      useCache = getOption("reproducible.useCache", FALSE), ...) {
  UseMethod("fixErrors")
}

#' @export
#' @keywords internal
fixErrors.default <- function(x, objectName, attemptErrorFixes = TRUE,
                              useCache = getOption("reproducible.useCache", FALSE), ...) {
  x
}

#' Fix \code{rgeos::gIsValid} failures in \code{SpatialPolygons}
#'
#' This uses \code{raster::buffer(..., width = 0)} internally, which fixes some
#' failures to \code{rgeos::gIsValid}
#'
#' @export
#' @param x A \code{SpatialPolygons} object
#' @inheritParams fixErrors
fixErrors.SpatialPolygons <- function(x, objectName = NULL,
                                      attemptErrorFixes = TRUE,
                                      useCache = getOption("reproducible.useCache", FALSE), ...) {
  if (attemptErrorFixes) {
    if (is.null(objectName)) objectName = "SpatialPolygon"
    if (is(x, "SpatialPolygons")) {
      message("Checking for errors in ", objectName)
      if (suppressWarnings(any(!rgeos::gIsValid(x, byid = TRUE)))) {
        message("Found errors in ", objectName, ". Attempting to correct.")
        x1 <- try(Cache(raster::buffer, x, width = 0, dissolve = FALSE, useCache = useCache))
        if (is(x1, "try-error")) {
          message("There are errors with ", objectName,
                  ". Couldn't fix them with raster::buffer(..., width = 0)")
        } else {
          x <- x1
          message("  Some or all of the errors fixed.")
        }

      } else {
        message("  Found no errors.")
      }
    }
  }
  return(x)
}


#' Project \code{Raster*} or {Spatial*} or \code{sf} objects
#'
#' A simple wrapper around the various different tools for these GIS types.
#'
#' @export
#' @param x A \code{Raster*}, \code{Spatial*} or \code{sf} object
#' @param targetCRS The CRS of x at the end  of this function (i.e., the goal)
#' @param ... Passed to \code{\link[raster]{projectRaster}}.
#' @param rasterToMatch Template \code{Raster*} object passed to the \code{to} argument of
#'                      \code{\link[raster]{projectRaster}}, thus will changing the
#'                      resolution and projection of \code{x}.
#'                      See details in \code{\link{postProcess}}.
#'
#' @rdname projectInputs
#' @importFrom raster canProcessInMemory
#' @return
#' A file of the same type as starting, but with projection (and possibly other
#' characteristics, including resolution, origin, extent if changed.
#' @example inst/examples/example_postProcess.R
projectInputs <- function(x, targetCRS, ...) {
  UseMethod("projectInputs")
}


#' @export
#' @rdname projectInputs
projectInputs.default <- function(x, targetCRS, ...) {
  x
}

#' @export
#' @rdname projectInputs
#' @importFrom fpCompare %==%
#' @importFrom raster crs res res<- dataType
#' @importFrom gdalUtils gdalwarp
projectInputs.Raster <- function(x, targetCRS = NULL, rasterToMatch = NULL, ...) {

  dots <- list(...)
  if (!is.null(rasterToMatch)) {
    if (is.null(targetCRS)) {
      targetCRS <- crs(rasterToMatch)
    }

    if (!identical(crs(x), targetCRS) |
        !identical(res(x), res(rasterToMatch)) |
        !identical(extent(x), extent(rasterToMatch))) {
      message("    reprojecting ...")
      if (canProcessInMemory(x, 4)) {
        tempRas <- projectExtent(object = rasterToMatch, crs = targetCRS) ## make a template RTM, with targetCRS
        warn <- capture_warnings(x <- projectRaster(from = x, to = tempRas, ...))
        warn <- warn[!grepl("no non-missing arguments to m.*; returning .*Inf", warn)] # This is a bug in raster
        warnings(warn)
        ## projectRaster doesn't always ensure equal res (floating point number issue)
        ## if resolutions are close enough, re-write res(x)
        ## note that when useSAcrs = TRUE, the different resolutions may be due to
        ## the different projections (e.g. degree based and meter based). This should be fine
        if (crs(x) == crs(rasterToMatch) &
            any(res(x) != res(rasterToMatch))) {
          if (all(res(x) %==% res(rasterToMatch))) {
            res(x) <- res(rasterToMatch)
          } else {
            stop(paste0("Error: input and output resolutions are not similar after using projectRaster.\n",
                 "You can try increasing error tolerance in options('fpCompare.tolerance')."))
          }
        }
      } else {
        message("   large raster: reprojecting after writing to temp drive...")
        tempSrcRaster <- file.path(tempfile(), ".tif", fsep = "")
        tempDstRaster <- file.path(dirname(tempfile()),
                                   paste0(x@data@names,"_reproj", ".tif"))
        writeRaster(x, filename = tempSrcRaster, datatype = assessDataType(x), overwrite = TRUE)
        gdalUtils::gdalwarp(srcfile = tempSrcRaster,
                            dstfile = tempDstRaster,
                            s_srs = as.character(crs(x)),
                            t_srs = as.character(targetCRS),
                            tr = res(rasterToMatch),
                            tap = TRUE, overwrite = TRUE)
        x <- raster(tempDstRaster)
        x[] <- x[]    ## bring it to memory to update metadata
        file.remove(c(tempSrcRaster, tempDstRaster))
      }
    } else {
      message("    no reprojecting because target characteristics same as input Raster.")
    }
  } else {
    if (!is.null(targetCRS)) {
      if (!identical(crs(x), targetCRS)) {
        message("    reprojecting ...")
        x <- projectRaster(from = x, crs = targetCRS, ...)
      } else {
        message("    no reprojecting because target CRS is same as input CRS.")
      }
    } else {
      message("     no reprojecting because no rasterToMatch & useSAcrs are FALSE.")
    }
  }
  x
}

#' @export
#' @rdname projectInputs
projectInputs.sf <- function(x, targetCRS, ...) {
  warning("sf class objects not fully implemented. Use with projectInputs.sf caution.")
  if (requireNamespace("sf")) {
    if (any(sf::st_is(x, c("POLYGON", "MULTIPOLYGON"))) && !any(isValid <- sf::st_is_valid(x))) {
      x[!isValid] <- sf::st_buffer(x[!isValid], dist = 0, ...)
    }

    x <- sf::st_transform(x = x, crs = sf::st_crs(targetCRS@projargs), ...)

  } else {
    stop("Please install sf package: https://github.com/r-spatial/sf")
  }
}

#' @export
#' @rdname projectInputs
#' @importFrom raster crs
projectInputs.Spatial <- function(x, targetCRS, ...) {
  if (!is.null(targetCRS)) {
    if (!is(targetCRS, "CRS")) {
      if (!is.character(targetCRS)) {
        if (is(targetCRS, "spatialObjects")) {
          targetCRS <- crs(targetCRS)
        } else {
          stop("targetCRS in projectInputs must be a CRS object or a class from",
               " which a crs can be extracted with raster::crs")
        }
      }
    }
    x <- spTransform(x = x, CRSobj = targetCRS)
  }
  x
}

#' Hierarchically get crs from \code{Raster*}, \code{Spatial*}
#'
#' This is the function that follows the table of order of
#' preference for determining CRS. See \code{\link{postProcess}}
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
#' This function can be used to mask inputs from data. Masking here is
#' equivalent to \code{raster::mask} (though \code{\link{fastMask}} is used here)
#' or \code{raster::intersect}.
#'
#' @param x An object to do a geographic raster::mask/raster::intersect.
#'          See methods.
#' @param ... Passed to methods. None currently implemented.
#'
#' @author Eliot McIntire
#' @author Jean Marchal
#' @export
#' @inheritParams cropInputs
#' @importFrom utils capture.output
#' @rdname maskInputs
#' @example inst/examples/example_postProcess.R
#'
maskInputs <- function(x, studyArea, ...) {
  UseMethod("maskInputs")
}

#' @export
#' @param maskWithRTM Logical. If \code{TRUE}, then the default,
#' @rdname maskInputs
maskInputs.Raster <- function(x, studyArea, rasterToMatch, maskWithRTM = FALSE, ...) {

  message("    masking...")
  if (isTRUE(maskWithRTM)) {
    x[is.na(rasterToMatch)] <- NA
  } else {
    if (!is.null(studyArea)) {
      #msg <- capture.output(type = "message",
                            x <- fastMask(x = x, y = studyArea)
                            #)
      #message(paste0("      ", paste(msg, collapse = "\n      ")))
    } else {
      message("studyArea not provided, skipping masking.")
    }
  }
  return(x)
}

#' @export
#' @rdname maskInputs
maskInputs.Spatial <- function(x, studyArea, ...) {
  if (!is.null(studyArea)) {
    message("    intersecting ...")
    studyArea <- raster::aggregate(studyArea, dissolve = TRUE)
    studyArea <- spTransform(studyArea, CRSobj = crs(x))
    suppressWarnings(studyArea <- fixErrors(studyArea, "studyArea"))
    x <- tryCatch(raster::intersect(x, studyArea), error = function(e) {
      warning("  Could not mask with studyArea, for unknown reasons.",
              " Returning object without masking.")
      return(x)
    })
    return(x)
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
#' @inheritParams postProcess.spatialObjects
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
determineFilename <- function(filename2 = TRUE, filename1 = NULL,
                              destinationPath = NULL, prefix = "Small", ...) {

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

  if (!(is.logical(filename2) || is.character(filename2) || is.null(filename2))) {
    stop("filename2 must be logical or character string or NULL")
  }

  newFilename <- if (!identical(filename2, FALSE)) { # allow TRUE or path
    if (isTRUE(filename2) ) {
      if (is.null(filename1)) {
        tmpfile <- basename(tempfile())
        filename1 <- tmpfile
      }
      .prefix(filename1, prefix)
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
  if (exists("tmpfile", inherits = FALSE)) {
    message("Saving output to ", newFilename, ". Specify filename1 or filename2 for more control")
  }

  newFilename
}

#' Write module inputs on disk
#'
#' Can be used to write prepared inputs on disk.
#'
#' @inheritParams postProcess
#' @param x  The object save to disk i.e., write outputs
#' @param overwrite Logical. Should file being written overwrite an existing file if it
#'                  exists.
#' @param filename2 File name passed to \code{\link[raster]{writeRaster}}, or
#'                  \code{\link[raster]{shapefile}} or \code{\link[sf]{st_write}}
#'                  (\code{dsn} argument).
#' @param ... Passed into \code{\link[raster]{shapefile}} or
#'             \code{\link[raster]{writeRaster}} or \code{\link[sf]{st_write}}
#' @author Eliot McIntire
#' @author Jean Marchal
#' @export
#' @importFrom methods is
#' @importFrom raster shapefile writeRaster
#' @rdname writeOutputs
#' @example inst/examples/example_postProcess.R
#'
writeOutputs <- function(x, filename2, overwrite, ...) {
  UseMethod("writeOutputs")
}

#' @rdname writeOutputs
writeOutputs.Raster <- function(x, filename2 = NULL, overwrite = FALSE, ...) {
  dots <- list(...)
  datatype2 <- assessDataType(x)

  if (!is.null(filename2)) {
    if (is.null(dots$datatype)) {
      message(paste("no 'datatype' chosen.",
                    "\n saving", names(x), "as", datatype2))
      dots$datatype <- datatype2
    } else if (datatype2 != dots$datatype)
      message("chosen 'datatype', ",dots$datatype,", may be inadequate for the ",
                    "range/type of values in ", names(x),
                    "\n consider changing to ", datatype2)

    xTmp <- writeRaster(x = x, filename = filename2, overwrite = overwrite, ...)

    # This is a bug in writeRaster was spotted with crs of xTmp became
    # +proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs
    # should have stayed at
    # +proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0
    if (!identical(crs(xTmp), crs(x)))
      crs(xTmp) <- crs(x)

    x <- xTmp
  }
  x
}

#' @rdname writeOutputs
writeOutputs.Spatial <- function(x, filename2 = NULL, overwrite = FALSE, ...) {
  if (!is.null(filename2)) {
    dots <- list(...)
    notWanted1 <- .formalsNotInCurrentDots(shapefile, ...)
    notWanted2 <- .formalsNotInCurrentDots(rgdal::writeOGR, ...)
    keepForDots <- c(setdiff(notWanted1, notWanted2), setdiff(names(dots), notWanted1))
    dots <- dots[keepForDots]
    do.call(shapefile, append(dots, list(x = x, filename = filename2, overwrite = overwrite)))

  }
  x
}

#' @rdname writeOutputs
writeOutputs.sf <- function(x, filename2 = NULL, overwrite = FALSE, ...) {
  if (!is.null(filename2)) {
    if (requireNamespace("sf")) {
      x <- sf::st_write(obj = x, delete_dsn = TRUE, dsn = filename2, delete_dsn = overwrite,
                        ...)
    } else {
      stop("Please install sf package: https://github.com/r-spatial/sf")
    }
  }
  x
}

#' @rdname writeOutputs
writeOutputs.default <- function(x, filename2, ...) {
  stop("Don't know how to write object of class ", class(x), " on disk.")
}


#' Assess the appropriate raster layer data type
#'
#' Can be used to write prepared inputs on disk.
#'
#' @param ras  The RasterLayer or RasterStack for which data type will be assessed.
#' @author Eliot McIntire
#' @author Ceres Barros
#' @export
#' @rdname assessDataType
#' @importFrom raster getValues
#' @example inst/examples/example_assessDataType.R
#' @return The appropriate data type for the range of values in \code{ras}. See \code{\link[raster]{dataType}} for details.

assessDataType <- function(ras) {
  UseMethod("assessDataType")
}

#' @export
#' @rdname assessDataType
assessDataType.Raster <- function(ras) {
  ## using ras@data@... is faster, but won't work for @values in large rasters
  rasVals <- getValues(ras)
  minVal <- ras@data@min
  maxVal <- ras@data@max
  signVal <- minVal < 0
  doubVal <-  any(floor(rasVals) != rasVals, na.rm = TRUE)  ## faster than any(x %% 1 != 0)

  ## writeRaster deals with infinite values as FLT8S
  # infVal <- any(!is.finite(minVal), !is.finite(maxVal))   ## faster than |

  if(!doubVal & !signVal) {
    ## only check for binary if there are no decimals and no signs
    logi <- all(!is.na(.bincode(na.omit(rasVals), c(-1,1))))  ## range needs to include 0

    if(logi) {
      datatype <- "LOG1S"
    } else {
      ## if() else is faster than if
      datatype <- if(maxVal <= 255) "INT1U" else
        if(maxVal <= 65534) "INT2U" else
          if(maxVal <= 4294967296) "INT4U" else    ## note that: dataType doc. advises against INT4U
            if(maxVal > 3.4e+38) "FLT8S" else "FLT4S"
    }
  } else {
    if(signVal & !doubVal) {
      ## if() else is faster than if
      datatype <- if(minVal >= -127 & maxVal <= 127) "INT1S" else
        if(minVal >= -32767 & maxVal <= 32767) "INT2S" else
          if(minVal >= -2147483647 & maxVal <=  2147483647) "INT4S" else    ## note that: dataType doc. advises against INT4U
            if(minVal < -3.4e+38 | maxVal > 3.4e+38) "FLT8S" else "FLT4S"
    } else
      if(doubVal)
        datatype <- if(minVal < -3.4e+38 | maxVal > 3.4e+38) "FLT8S" else "FLT4S"
  }
  datatype
}

#' @export
#' @rdname assessDataType
assessDataType.RasterStack <- function(ras) {
  unlist(lapply(names(ras), function(x) assessDataType(ras[[x]])))
}

#' @export
#' @rdname assessDataType
assessDataType.default <- function(ras) {
  stop("No method for assessDataType for class ", class(ras))
}
