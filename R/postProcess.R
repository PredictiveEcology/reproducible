#' Generic function to post process objects
#'
#' There may be many methods developed. See e.g.,
#' \code{\link{postProcess.spatialObjects}}
#' @export
#' @keywords internal
#' @param x  An object of postProcessing. See individual methods.
#' @importClassesFrom quickPlot spatialObjects
#' @importFrom utils capture.output
#' @seealso \code{prepInputs}, \code{\link{postProcess.spatialObjects}}
#' @param targetFilePath Full path of the target file
#' @param ... Passed to internal functions. None implemented for the generic.
#' @inheritParams prepInputs
#'
postProcess <- function(x, ...) {
  UseMethod("postProcess")
}

#' @export
#' @keywords internal
postProcess.default <- function(x, ...) {
  x
}

#' Post processing for \code{spatialObjects}
#'
#' The method for spatialObjects (\code{Raster*} and \code{Spatial*}) will
#' crop, reproject, and mask, in that order.  This function is a wrapper for
#' \code{\link{cropInputs}}, \code{\link{maskInputs}} and
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
#'     \item Fix errors. Currently only errors fixed are for \code{SpatialPolygons} using
#'            \code{buffer(..., width = 0)}.
#'     \item Crop using \code{\link{cropInputs}}
#'     \item Project using \code{\link{projectInputs}} \item Mask using \code{\link{maskInputs}}
#'     \item Determine file name \code{\link{determineFilename}} via \code{postProcessedFilename}
#'     \item Write that file name to disk, optionally \code{\link{writeOutputs}}
#'   }
#'
#'   NOTE: checksumming does not occur during the post-processing stage, as
#'   there are no file downloads. To achieve fast results, wrap
#'   \code{prepInputs} with \code{Cache}
#'
#'   NOTE: \code{sf} objects are still very experimental.
#'
#'  \subsection{Understanding various combinations of \code{rasterToMatch}
#'   and/or \code{studyArea}}{ Please see \code{\link{postProcess.spatialObjects}}
#'  }
#'
#' @inheritParams prepInputs
#'
#' @inheritParams cropInputs
#'
#' @param x A \code{Spatial*}, \code{sf} or \code{Raster*} object.
#'
#' @param postProcessedFilename Character string. If provided, then it is passed to
#'                 \code{determineFilename} and then \code{writeOutputs}.
#'
#' @param inputFilePath Character string giving the file path of the \emph{input} object,
#'                      if it has one. This is then used if \code{postProcessedFilename}
#'                      is \code{TRUE} to name the output file, where the resulting
#'                      post-processed filename will be
#'                      \code{.prefix(basename(inputFilePath), "Small")}.
#'                      Mostly used by \code{\link{prepInputs}},
#'                      where \code{postProcessedFilename} is missing.
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
#'
#' @export
#'
#' @section Passing \code{rasterToMatch} and/or \code{studyArea}:
#'
#' Depending on which of these were passed, different things will happen to the \code{targetFile}
#' located at \code{inputFilePath}.
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
postProcess.spatialObjects <- function(x, inputFilePath = NULL,
                                       studyArea = NULL, rasterToMatch = NULL,
                                       overwrite = TRUE, useSAcrs = FALSE,
                                       useCache = getOption("reproducible.useCache", FALSE),
                                       postProcessedFilename = NULL,
                                       ...) {

  # Test if user supplied wrong type of file for "studyArea", "rasterToMatch"
  if (!is.null(studyArea) & !is(studyArea, "Spatial")) {
    stop("The 'studyArea' provided is not a Spatial* object.")
  }

  if (!is.null(rasterToMatch) & !is(rasterToMatch, "RasterLayer")) {
    stop("The 'rasterToMatch' provided is not a Raster* object.")
  }

  dots <- list(...)

  if (!is.null(dots$targetFilePath))  {
    message("targetFilePath is being deprecated; use inputFilePath.")
    inputFilePath <- dots$targetFilePath
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
                           x <- Cache(cropInputs, x, studyArea = studyArea,
                                      extentToMatch = extRTM,
                                      extentCRS = crsRTM,
                                      useCache = useCache, ...))

    .groupedMessage(mess, omitPattern = paste(skipCacheMess, skipCacheMess2, sep = "|"))

    # cropInputs may have returned NULL if they don't overlap
    if (!is.null(x)) {
      objectName <- if (is.null(inputFilePath)) NULL else basename(inputFilePath)
      mess <- capture.output(type = "message", # no Cache at the method level because may be just passed through if raster
                             x <- fixErrors(x, objectName = objectName,
                                            useCache = useCache, ...))
      .groupedMessage(mess, omitPattern = skipCacheMess)

      # projectInputs
      targetCRS <- getTargetCRS(useSAcrs, studyArea, rasterToMatch)

      mess <- capture.output(type = "message",
                             x <- Cache(projectInputs, x, targetCRS = targetCRS,
                                        rasterToMatch = rasterToMatch, useCache = useCache, ...))

      .groupedMessage(mess, omitPattern = paste(skipCacheMess, skipCacheMess2, sep = "|"))

      # maskInputs
      mess <- capture.output(type = "message",
                             x <- Cache(maskInputs, x, studyArea = studyArea,
                                        rasterToMatch = rasterToMatch, useCache = useCache, ...))

      .groupedMessage(mess, omitPattern = paste(skipCacheMess, skipCacheMess2, sep = "|"))

      # filename
      if (is.null(postProcessedFilename)) {
        postProcessedFilename <- TRUE
      }
      newFilename <- determineFilename(inputFilePath = inputFilePath,
                                       postProcessedFilename = postProcessedFilename,
                                       ...)
      if (!is.null(list(...)$filename)) stop("Can't pass filename; use postProcessedFilename")

      # writeOutputs
      x <- writeOutputs(x = x, filename = newFilename, overwrite = overwrite, ... )
    }
  }
  return(x)
}

#' Reproject, crop a \code{Spatial*} or \code{Raster*} object
#'
#' This function can be used to crop or reproject module inputs from raw data.
#'
#' @param x A \code{Spatial*}, \code{sf}, or \code{Raster*} object.
#'
#' @param studyArea Template \code{SpatialPolygons*} object used for masking, after cropping.
#'                  If not in same CRS, then it will be \code{spTransform}ed to
#'                  CRS of \code{x} before masking. Currently, this function will not reproject the
#'                  \code{x}. \code{\link{postProcess.spatialObjects}}
#'
#' @param rasterToMatch Template \code{Raster*} object used for cropping (so extent should be
#'                      the extent of desired outcome), reprojecting (including changing the
#'                      resolution and projection).
#'                      See details in \code{\link{postProcess.spatialObjects}}.
#' @param ... Passed to \code{projectRaster} and \code{Cache}
#' cropping.
#'
#' @author Eliot McIntire
#' @author Jean Marchal
#' @export
#' @importFrom methods is
#' @importFrom raster buffer crop crs extent projectRaster res crs<-
#' @importFrom rgeos gIsValid
#' @importFrom sp SpatialPolygonsDataFrame spTransform CRS
#' @rdname cropInputs
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
cropInputs.spatialObjects <- function(x, studyArea, rasterToMatch = NULL, extentToMatch = NULL,
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
        x <- raster::crop(x = x, y = cropExtent)
        if (is.null(x)) {
          message("    polygons do not intersect.")
        }
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
#' @param ... Passed into \code{\link[raster]{projectRaster}},
#'            \code{\link[sp]{spTransform}} or \code{\link[sf]{st_transform}}
#'
#' @return
#' A file of the same type as starting, but with projection (and possibly other
#' characteristics, including resolution, origin, extent if changed.
projectInputs <- function(x, targetCRS, ...) {
  UseMethod("projectInputs")
}

#' @export
projectInputs.Raster <- function(x, targetCRS = NULL, rasterToMatch = NULL, ...) {

  if (!is.null(rasterToMatch)) {
    if (!is.null(targetCRS)) {
      if (!identical(crs(x), targetCRS) |
          !identical(res(x), res(rasterToMatch)) |
          !identical(extent(x), extent(rasterToMatch))) {
        message("    reprojecting ...")
        x <- projectRaster(from = x, to = rasterToMatch, ...)
      } else {
        message("    no reprojecting because target characteristics same as input Raster.")
      }
    } else {
      message("    no reprojecting because no rasterToMatch & useSAcrs is FALSE.")
    }
  } else {
    message("    no reprojecting because no rasterToMatch.")
  }
  x
}

#' @export
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
projectInputs.Spatial <- function(x, targetCRS, ...) {
  if (!is.null(targetCRS)) {
    x <- spTransform(x = x, CRSobj = targetCRS)
  }
  x
}

#' Hierachically get crs from \code{Raster*}, \code{Spatial*}
#'
#' This is the function that follows the table of order of
#' preference for determining CRS. See \code{\link{postProcess.spatialObjects}}
#' @inheritParams postProcess.spatialObjects
#' @export
getTargetCRS <- function(useSAcrs, studyArea, rasterToMatch) {
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
#' @param x          A \code{Raster*} object
#'
#' @param studyArea  A \code{SpatialPolygons*} object
#' @param ... Arguments passed to methods
#'
#'
#' @author Eliot McIntire
#' @author Jean Marchal
#' @export
#' @inheritParams cropInputs
#' @importFrom utils capture.output
#' @rdname maskInputs
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
      msg <- capture.output(type = "message",
                            x <- fastMask(x = x, y = studyArea))
      message(paste0("      ", paste(msg, collapse = "\n      ")))
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
    }
    )
    return(x)
  } else {
    message("studyArea not provided, skipping masking.")
    return(x)
  }
}

#' Determine filename, either automatically or manually
#'
#' If \code{postProcessedFilename} is \code{logical}, then the cropped/masked
#' raster will be written to disk with the original \code{targetFile} name, with
#' \code{"Small"} prefixed to the basename(\code{targetFilename}).
#' If a character string, it will be the path of the saved raster.
#' It will be tested whether it is an absolute or relative path and used as is
#' if absolute or prepended with \code{destinationPath} if relative.
#'
#' @inheritParams postProcess.spatialObjects
#'
#' @param postProcessedFilename Logical or character string (a file path). See details.
#'
#' @param inputFilePath Optional. Filename (with or without full path). Only used if
#'                       \code{postProcessedFilename} is \code{TRUE}, in which case,
#'                       this is used to help name the output.
#'
#' @param destinationPath Optional. If \code{postProcessedFilename} is a relative file path, then this
#'                        will be the directory of the resulting absolute file path.
#'
#' @include helpers.R
#'
#' @details
#'  If \code{postProcessedFilename} is \code{logical}, then the output
#'  filename will be \code{"Small"} prefixed to the basename(\code{inputFilePath}).
#'  If a character string, it
#'  will be the path returned. It will be tested whether it is an
#'  absolute or relative path and used as is if absolute or prepended with
#'  \code{destinationPath} if provided, and if \code{postProcessedFilename} is relative.
#'
determineFilename <- function(postProcessedFilename = TRUE, inputFilePath = NULL,
                              destinationPath = NULL, ...) {

  dots <- list(...)
  if (!is.null(dots$targetFilePath))  {
    message("targetFilePath is being deprecated from determineFilename:\n",
            "  use postProcessedFilename and inputFilePath.")
    if (is.null(inputFilePath)) {
      inputFilePath <- dots$targetFilePath
      dots$targetFilePath <- NULL
    }
  }

  if (!(is.logical(postProcessedFilename) || is.character(postProcessedFilename))) {
    stop("postProcessedFilename must be logical or character string")
  }

  newFilename <- if (!identical(postProcessedFilename, FALSE)) { # allow TRUE or path
    if (isTRUE(postProcessedFilename) ) {
      .prefix(inputFilePath, "Small")
    } else {
      if (isAbsolutePath(postProcessedFilename)) {
        postProcessedFilename
      } else {
        if (!is.null(destinationPath)) {
          file.path(destinationPath, basename(postProcessedFilename))
        } else {
          postProcessedFilename # accept relative
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
#' @param filename The filename to save the output object to disk (a \code{Raster*} or
#'                 \code{Spatial*} object)
#' @param overwrite Logical. Should file being written overwrite an existing file if it
#'                  exists.
#' @param ... Passed to \code{\link[raster]{writeRaster}}, such as \code{datatype},
#'            and \code{\link[raster]{shapefile}}
#'
#' @author Eliot McIntire
#' @author Jean Marchal
#' @export
#' @importFrom methods is
#' @importFrom raster shapefile writeRaster
#' @rdname writeOutputs
#'
writeOutputs <- function(x, filename, overwrite, ...) {
  UseMethod("writeOutputs")
}

writeOutputs.Raster <- function(x, filename, overwrite = FALSE, ...) {
  if (!is.null(filename)) {
    xTmp <- writeRaster(x = x, filename = filename, overwrite = overwrite, ...)

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

writeOutputs.Spatial <- function(x, filename, overwrite = FALSE, ...) {
  if (!is.null(filename)) {
    shapefile(x = x, filename = filename, overwrite = overwrite)
  }
  x
}

writeOutputs.sf <- function(x, filename, overwrite = FALSE, ...) {
  if (!is.null(filename)) {
    if (requireNamespace("sf")) {
      x <- sf::st_write(obj = x, delete_dsn = TRUE, dsn = filename, delete_dsn = overwrite)
    } else {
      stop("Please install sf package: https://github.com/r-spatial/sf")
    }
  }
  x
}

writeOutputs.default <- function(x, filename, ...) {
  stop("Don't know how to write object of class ", class(x), " on disk.")
}
