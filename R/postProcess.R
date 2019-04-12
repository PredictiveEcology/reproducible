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
  postProcess(rlang::eval_tidy(x), ...)
}

#' @export
#' @rdname postProcess
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
                                       overwrite = getOption("reproducible.overwrite", TRUE),
                                       useSAcrs = FALSE,
                                       useCache = getOption("reproducible.useCache", FALSE),
                                       ...) {
  # Test if user supplied wrong type of file for "studyArea", "rasterToMatch"
  x <- postProcessAllSpatial(x = x, studyArea = studyArea,
                             rasterToMatch = rasterToMatch, useCache = useCache,
                             filename1 =filename1, filename2 = filename2,
                             useSAcrs = useSAcrs, overwrite = overwrite,
                             ...)
  return(x)
}

#' @export
#' @example inst/examples/example_postProcess.R
#' @importFrom sf st_as_sf
#' @rdname postProcess
postProcess.sf <- function(x, filename1 = NULL, filename2 = TRUE,
                           studyArea = NULL, rasterToMatch = NULL,
                           overwrite = getOption("reproducible.overwrite", TRUE),
                           useSAcrs = FALSE,
                           useCache = getOption("reproducible.useCache", FALSE),
                           ...) {

  # Test if user supplied wrong type of file for "studyArea", "rasterToMatch"
  message("postProcess with sf class objects is still experimental")
  if (!is.null(rasterToMatch)) {
    stop("sf class objects are not yet working with rasterToMatch argument")
  }
  if (is(studyArea, "Spatial")) {
    studyArea <- st_as_sf(studyArea)
  }

  x <- postProcessAllSpatial(x = x, studyArea = studyArea,
                             rasterToMatch = rasterToMatch, useCache = useCache,
                             filename1 =filename1, filename2 = filename2,
                             useSAcrs = useSAcrs, overwrite = overwrite,
                             ...)

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
#' @author Eliot McIntire, Jean Marchal, Ian Eddy, and Tati Micheletti
#' @example inst/examples/example_postProcess.R
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

#' @param extentToMatch Optional. Can pass an extent here and a \code{crs} to
#'                      \code{extentCRS} instead of \code{rasterToMatch}. These
#'                      will override \code{rasterToMatch}, with a warning if both
#'                      passed.
#' @param extentCRS     Optional. Can pass a \code{crs} here with an extent to
#'                      \code{extentTomatch} instead of \code{rasterToMatch}
#'
#' @export
#' @importFrom raster projectExtent tmpDir
#' @rdname cropInputs
cropInputs.spatialObjects <- function(x, studyArea = NULL, rasterToMatch = NULL,
                                      extentToMatch = NULL, extentCRS = NULL, ...) {
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
    cropExtent <- if (identical(crs(x), crs(cropTo))) {
      extent(cropTo)
    } else {
      if (!is.null(rasterToMatch)) {
        projectExtent(cropTo, crs(x))
      } else {
        if (is(studyArea, "Spatial")) {
          #theExtent <- as(extent(cropTo), "SpatialPolygons")
          #crs(theExtent) <- crs(cropTo)
          raster::extent(spTransform(x = cropTo, CRSobj = crs(x)))
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
        if (canProcessInMemory(x, 3)) {
          x <- do.call(raster::crop, args = append(list(x = x, y = cropExtent), dots))
        } else {
          x <- do.call(raster::crop,
                       args = append(list(x = x, y = cropExtent,
                                          filename = paste0(tempfile(tmpdir = tmpDir()), ".tif")),
                                     dots))
        }
        if (is.null(x)) {
          message("    polygons do not intersect.")
        }
      }
    }
  }
  return(x)
}

#' @export
#' @importFrom raster crs extent projectExtent raster
#' @importFrom sf st_crop st_crs st_transform
#' @rdname cropInputs
cropInputs.sf <- function(x, studyArea = NULL, rasterToMatch = NULL,
                          extentToMatch = NULL, extentCRS = NULL, ...) {
  message("cropInputs with sf class objects is still experimental")
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
    cropExtent <- if (identical(crs(x), crs(cropTo))) {
      extent(cropTo)
    } else {
      if (!is.null(rasterToMatch)) {
        stop("Can't work with rasterToMatch and sf objects yet in cropInputs")
        projectExtent(cropTo, crs(x))
      } else {
        if (is(studyArea, "sf")) {
          sf::st_transform(x = cropTo, crs = st_crs(x))
        } else if (is(studyArea, "Spatial")) {
          sf::st_transform(x = sf::st_as_sf(cropTo), crs = st_crs(x))
        } else {
          NULL
        }
      }
    }

    if (!identical(crs(x), crs(cropExtent))) {
      crs(cropExtent) <- crs(x)
    }

    if (!is.null(cropExtent)) {
      # crop it
      if (!identical(cropExtent, extent(x))) {
        message("    cropping ...")
        dots <- list(...)
        dots[.formalsNotInCurrentDots("crop", ...)] <- NULL
        x <- do.call(sf::st_crop, args = append(list(x = x, y = cropExtent), dots))
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
#' @rdname fixErrors
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
#' @importFrom testthat capture_warnings
#' @rdname fixErrors
fixErrors.SpatialPolygons <- function(x, objectName = NULL,
                                      attemptErrorFixes = TRUE,
                                      useCache = getOption("reproducible.useCache", FALSE), ...) {
  if (attemptErrorFixes) {
    if (is.null(objectName)) objectName = "SpatialPolygon"
    if (is(x, "SpatialPolygons")) {
      message("Checking for errors in ", objectName)
      if (suppressWarnings(any(!rgeos::gIsValid(x, byid = TRUE)))) {
        message("Found errors in ", objectName, ". Attempting to correct.")
        warn <- capture_warnings(
          x1 <- try(Cache(raster::buffer, x, width = 0, dissolve = FALSE, useCache = useCache))
        )

        # prevent the warning about not projected, because we are buffering 0, which doesn't matter
        warnAboutNotProjected <- startsWith(warn, "Spatial object is not projected; GEOS expects planar coordinates")
        if (any(warnAboutNotProjected))
          warn <- warn[!warnAboutNotProjected]
        if (length(warn))
          warning(warn)

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

#' @inheritParams fixErrors
#' @param x A \code{SpatialPolygons} object
#'
#' @export
#' @importFrom sf st_buffer st_geometry st_is_valid
#' @importFrom testthat capture_warnings
#' @rdname fixErrors
fixErrors.sf <- function(x, objectName = NULL, attemptErrorFixes = TRUE,
                         useCache = getOption("reproducible.useCache", FALSE), ...) {
  if (attemptErrorFixes) {
    if (is.null(objectName)) objectName = "SimpleFeature"
    if (is(st_geometry(x), "sfc_MULTIPOLYGON") || is(st_geometry(x), "sfc_GEOMETRY")) {
      message("Checking for errors in ", objectName)
      if (suppressWarnings(any(!sf::st_is_valid(x)))) {
        message("Found errors in ", objectName, ". Attempting to correct.")
        warn <- capture_warnings(
          x1 <- try(Cache(sf::st_buffer, x, dist = 0, useCache = useCache))
        )

        # prevent the warning about not projected, because we are buffering 0, which doesn't matter
        warnAboutNotProjected <- startsWith(warn, "Spatial object is not projected; GEOS expects planar coordinates")
        if (any(warnAboutNotProjected))
          warn <- warn[!warnAboutNotProjected]
        if (length(warn))
          warning(warn)

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
#' @param x A \code{Raster*}, \code{Spatial*} or \code{sf} object
#' @param targetCRS The CRS of x at the end  of this function (i.e., the goal)
#' @param ... Passed to \code{\link[raster]{projectRaster}}.
#' @param rasterToMatch Template \code{Raster*} object passed to the \code{to} argument of
#'                      \code{\link[raster]{projectRaster}}, thus will changing the
#'                      resolution and projection of \code{x}.
#'                      See details in \code{\link{postProcess}}.
#' @param cores An \code{integer*} or \code{'AUTO'}. This will be used if gdalwarp is
#'                      triggered. \code{'AUTO'*} will calculate 90% of the total
#'                      number of cores in the system, while an integer or rounded
#'                      float will be passed as the exact number of cores to be used.
#'
#' @return A file of the same type as starting, but with projection (and possibly
#' other characteristics, including resolution, origin, extent if changed).
#'
#' @export
#' @importFrom raster canProcessInMemory
#' @rdname projectInputs
#'
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
#' @param useGDAL Logical, defaults to getOption("reproducible.useGDAL" = TRUE).
#'     If \code{TRUE}, then this function will use \code{gdalwarp} only when not
#'     small enough to fit in memory (i.e., \emph{if the operation fails} the
#'     \code{raster::canProcessInMemory(x, 3)} test). Using \code{gdalwarp} will
#'     usually be faster than \code{raster::projectRaster}, the function used
#'     if this is \code{FALSE}. Since since the two options use different algorithms,
#'     there may be different projection results.
#'
#' @importFrom fpCompare %==%
#' @importFrom gdalUtils gdal_setInstallation gdalwarp
#' @importFrom parallel detectCores
#' @importFrom raster crs dataType res res<- dataType<-
projectInputs.Raster <- function(x, targetCRS = NULL, rasterToMatch = NULL, cores = NULL,
                                 useGDAL = getOption("reproducible.useGDAL", TRUE),
                                 ...) {
  dots <- list(...)
  isFactorRaster <- FALSE
  if (isTRUE(raster::is.factor(x))) {
    isFactorRaster <- TRUE
    rasterFactorLevels <- raster::levels(x)
  }

  if (is.null(rasterToMatch) & is.null(targetCRS)) {
    message("     no reprojecting because no rasterToMatch & targetCRS are FALSE (or NULL).")
  } else if (is.null(rasterToMatch) & identical(crs(x), targetCRS)) {
    message("    no reprojecting because target CRS is same as input CRS.")
  } else {
    if (is.null(targetCRS)) {
      targetCRS <- crs(rasterToMatch)
    }

    doProjection <- FALSE
    if (is.null(rasterToMatch)) {
      if (!identical(crs(x), targetCRS))  doProjection <- TRUE
    } else if (!identical(crs(x), targetCRS) |
               !identical(res(x), res(rasterToMatch)) |
               !identical(extent(x), extent(rasterToMatch))) {
      doProjection <- TRUE
    }
    if (doProjection) {
      if (!canProcessInMemory(x, 3) && isTRUE(useGDAL)) {
        ## the raster is in memory, but large enough to trigger this function: write it to disk
        message("   large raster: reprojecting after writing to temp drive...")
        ## rasters need to go to same file so it can be unlinked at end without losing other temp files
        tmpRasPath <- checkPath(file.path(raster::tmpDir(), "bigRasters"), create = TRUE)
        tempSrcRaster <- file.path(tmpRasPath, "bigRasInput.tif")
        tempDstRaster <- file.path(tmpRasPath, paste0(x@data@names, "a_reproj.tif")) #fails if x = stack

        if (!is.null(rasterToMatch)) {
          tr <- res(rasterToMatch)
        } else {
          tr <- res(x)
        }

        gdalUtils::gdal_setInstallation()
        if (.Platform$OS.type == "windows") {
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
          writeRaster(x, filename = tempSrcRaster, datatype = dType, overwrite = TRUE)
          rm(x) #Saves memory if this was a huge raster, but be careful
          gc()
        } else {
          tempSrcRaster <- x@file@name #Keep original raster
        }

        teRas <- " " #This sets extents in GDAL
        if (!is.null(rasterToMatch)) {
          teRas <- paste0(" -te ", paste0(extent(rasterToMatch)@xmin, " ",
                                          extent(rasterToMatch)@ymin, " ",
                                          extent(rasterToMatch)@xmax, " ",
                                          extent(rasterToMatch)@ymax, " "))
        }
        if (is.null(cores) || cores =="AUTO") {
          cores <- as.integer(parallel::detectCores()*0.9)
          prll <- paste0("-wo NUM_THREADS=", cores, " ")
        } else {
          if (!is.integer(cores)) {
            if (is.character(cores) | is.logical(cores)) {
              stop ("'cores' needs to be passed as numeric or 'AUTO'")
            } else {
              prll <- paste0("-wo NUM_THREADS=", as.integer(cores), " ")
            }
          } else {
            prll <- paste0("-wo NUM_THREADS=", cores, " ")
          }
        }

        dType <- assessDataType(raster(tempSrcRaster), type = "GDAL")
        system(
          paste0(paste0(getOption("gdalUtils_gdalPath")[[1]]$path, "gdalwarp", exe, " "),
                 "-s_srs \"", as.character(raster::crs(raster::raster(tempSrcRaster))), "\"",
                 " -t_srs \"", as.character(targetCRS), "\"",
                 " -multi ", prll,
                 "-ot ", dType,
                 teRas,
                 "-r ", dots$method,
                 " -overwrite ",
                 "-tr ", paste(tr, collapse = " "), " ",
                 "\"", tempSrcRaster, "\"", " ",
                 "\"", tempDstRaster, "\""),
          wait = TRUE)
        ##
        x <- raster(tempDstRaster)
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
        }

        message(paste0("reprojecting using ", dots$method, "..."))

        if (is.null(rasterToMatch)) {
          Args <- append(dots, list(from = x, crs = targetCRS))
          warn <- capture_warnings(x <- do.call(projectRaster, args = Args))

        } else {
          # projectRaster does silly things with integers, i.e., it converts to numeric
          tempRas <- projectExtent(object = rasterToMatch, crs = targetCRS)
          Args <- append(dots, list(from = x, to = tempRas))
          warn <- capture_warnings(x <- do.call(projectRaster, args = Args))

          if (identical(crs(x), crs(rasterToMatch)) & any(res(x) != res(rasterToMatch))) {
            if (all(res(x) %==% res(rasterToMatch))) {
              res(x) <- res(rasterToMatch)
            } else {
              stop(paste0("Error: input and output resolutions are not similar after using projectRaster.\n",
                          "You can try increasing error tolerance in options('fpCompare.tolerance')."))
            }
          }
        }
        if (!identical(crs(x), targetCRS)) {
          crs(x) <- targetCRS # sometimes the proj4string is rearranged, so they are not identical:
                              #  they should be
        }

        # return the integer class to the data in the raster object
        if (isTRUE(isInteger)) {
          dataType(x) <- origDataType
          x[] <- as.integer(x[])
        }

        warn <- warn[!grepl("no non-missing arguments to m.*; returning .*Inf", warn)] # This is a bug in raster
        warnings(warn)
        ## projectRaster doesn't always ensure equal res (floating point number issue)
        ## if resolutions are close enough, re-write res(x)
        ## note that when useSAcrs = TRUE, the different resolutions may be due to
        ## the different projections (e.g. degree based and meter based). This should be fine

      }
    } else {
      message("    no reprojecting because target characteristics same as input Raster.")
    }
  }

  if (isFactorRaster) {
    levels(x) <- rasterFactorLevels
  }

  x
}

#' @export
#' @importFrom sf st_buffer st_crs st_is st_is_valid st_transform
#' @rdname projectInputs
projectInputs.sf <- function(x, targetCRS, ...) {
  if (!is.null(targetCRS)) {
    warning("sf class objects not fully tested Use with caution.")
    if (requireNamespace("sf")) {
      if (any(sf::st_is(x, c("POLYGON", "MULTIPOLYGON"))) && !any(isValid <- sf::st_is_valid(x))) {
        x[!isValid] <- sf::st_buffer(x[!isValid], dist = 0, ...)
      }

      if ("projargs" %in% slotNames(targetCRS) )
        targetCRS <- sf::st_crs(targetCRS@projargs)
      x <- sf::st_transform(x = x, crs = targetCRS, ...)
      if (!identical(sf::st_crs(x), targetCRS)) {
        sf::st_crs(x) <- targetCRS # sometimes the proj4string is rearranged, so they are not identical:
        #  they should be
      }

    } else {
      stop("Please install sf package: https://github.com/r-spatial/sf")
    }
  }
  x
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
    if (!identical(crs(x), targetCRS)) {
      crs(x) <- targetCRS # sometimes the proj4string is rearranged, so they are not identical:
      #  they should be
    }
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
.getTargetCRS <- function(useSAcrs, studyArea, rasterToMatch, targetCRS = NULL) {
  if (is.null(targetCRS)) {
    targetCRS <- if (useSAcrs) {
      crs(studyArea)
    } else if (!is.null(rasterToMatch)) {
      crs(rasterToMatch)
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
#' @author Eliot McIntire and Jean Marchal
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
      dots <- list(...)
      x <- fastMask(x = x, y = studyArea, cores = dots$cores)
    } else {
      message("studyArea not provided, skipping masking.")
    }
  }
  return(x)
}

#' @export
#' @rdname maskInputs
#' @importFrom sf st_as_sf st_intersects st_join
maskInputs.Spatial <- function(x, studyArea, ...) {
  if (!is.null(studyArea)) {
    message("    intersecting ...")
    if (NROW(studyArea) > 1)
      studyArea <- raster::aggregate(studyArea, dissolve = TRUE)
    if (!identical(crs(x), crs(studyArea)))
      studyArea <- spTransform(studyArea, CRSobj = crs(x))
    suppressWarnings(studyArea <- fixErrors(studyArea, "studyArea"))
    # raster::intersect -- did weird things in case of SpatialPolygonsDataFrame
    #  specifically ecodistricts.shp . It created an invalid object with
    #  non-unique row names
    y <- try(raster::intersect(x, studyArea))

    trySF <- if (is(y, "try-error")) {
      TRUE
    } else if (!identical(length(unique(row.names(y))), length(row.names(y)))) {
      TRUE
    } else {
      FALSE
    }
    if (trySF) {
      "raster intersect did not work correctly, trying sf"
      xTmp <- sf::st_join(st_as_sf(x), st_as_sf(studyArea), join = st_intersects)
      y <- as(xTmp, "Spatial")
    }
    if (!identical(crs(y), crs(x))) {
      crs(y) <- crs(x) # sometimes the proj4string is rearranged, so they are not identical:
      #  they should be
    }

    return(y)
  } else {
    return(x)
  }
}

#' @export
#' @rdname maskInputs
#' @importFrom sf st_as_sf st_combine st_geometry st_intersection st_intersects st_join st_sf
maskInputs.sf <- function(x, studyArea, ...) {
  if (!is.null(studyArea)) {
    if (is(studyArea, "Spatial"))
      studyArea <- sf::st_as_sf(studyArea)

    message("maskInputs with sf class objects is still experimental")
    message("    intersecting ...")
    #studyArea <- raster::aggregate(studyArea, dissolve = TRUE)
    if (!identical(st_crs(x), st_crs(studyArea)))
      studyArea <- sf::st_transform(studyArea, crs = st_crs(x))
    if (NROW(studyArea) > 1)
      studyArea <- sf::st_combine(studyArea)

    studyArea <- sf::st_sf(studyArea)
    if (is(sf::st_geometry(x), "sfc_POINT")) {
      y1 <- sf::st_intersects(x, studyArea)
      y2 <- sapply(y1, function(x) length(x) == 1)
      ## TODO: usevapply instead of sapply; sapply is not type-safe
      #y2 <- vapply(y1, function(x) length(x) == 1, logical(1))
      y <- x[y2,]
    } else {
      studyArea <- fixErrors(studyArea)
      y <- sf::st_intersection(x, studyArea)
    }
    if (!identical(crs(y), crs(x))) {
      crs(y) <- crs(x) # sometimes the proj4string is rearranged, so they are not identical:
      #  they should be
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
#' @importFrom raster tmpDir
#' @rdname determineFilename
#' @example inst/examples/example_postProcess.R
determineFilename <- function(filename2 = TRUE, filename1 = NULL,
                              destinationPath = getOption("reproducible.destinationPath"),
                              prefix = "Small", ...) {
  if (!is.null(filename2)) {
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

    filename2 <- if (!identical(filename2, FALSE)) { # allow TRUE or path
      if (isTRUE(filename2) ) {
        if (is.null(filename1)) {
          tmpfile <- basename(tempfile(tmpdir = tmpDir()))
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
      message("Saving output to ", filename2, ". Specify filename1 or filename2 for more control",
              "\n  or set filename2 to NULL to prevent saving to disk")
    }
  }
  filename2
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
#'
#' @author Eliot McIntire and Jean Marchal
#' @export
#' @importFrom methods is
#' @importFrom raster shapefile writeRaster
#' @rdname writeOutputs
#' @example inst/examples/example_postProcess.R
#'
writeOutputs <- function(x, filename2,
                         overwrite = getOption("reproducible.overwrite", NULL),
                         ...) {
  UseMethod("writeOutputs")
}

#' @rdname writeOutputs
writeOutputs.Raster <- function(x, filename2 = NULL,
                                overwrite = getOption("reproducible.overwrite", FALSE),
                                ...) {
  dots <- list(...)
  datatype2 <- assessDataType(x, type = "writeRaster")

  if (!is.null(filename2)) {
    if (is.null(dots$datatype)) {
      message(paste("no 'datatype' chosen.",
                    "\n saving", names(x), "as", datatype2))
      dots$datatype <- datatype2
    } else if (datatype2 != dots$datatype) {
      message("chosen 'datatype', ",dots$datatype,", may be inadequate for the ",
              "range/type of values in ", names(x),
              "\n consider changing to ", datatype2)
    }

    if (raster::is.factor(x)) {
      filename3 <- gsub(filename2, pattern = "\\.tif", replacement = ".grd")
      if (!identical(filename2, filename3)) {
        warning(".tif format does not preserve factor levels using rgdal. Using ",
                filename3, " to preserve levels, instead of ", filename2)
        filename2 <- filename3
      }
    }
    xTmp <- do.call(writeRaster, args = c(x = x, filename = filename2, overwrite = overwrite, dots))
    #Before changing to do.call, dots were not being added.
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
writeOutputs.Spatial <- function(x, filename2 = NULL,
                                 overwrite = getOption("reproducible.overwrite", TRUE),
                                 ...) {
  if (!is.null(filename2)) {
    dots <- list(...)
    notWanted1 <- .formalsNotInCurrentDots(shapefile, ...)
    notWanted2 <- .formalsNotInCurrentDots(rgdal::writeOGR, ...)
    keepForDots <- c(setdiff(notWanted1, notWanted2), setdiff(names(dots), notWanted1))
    dots <- dots[keepForDots]
    # Internally in rgdal::writeOGR, it converts the row.names to integer with this test
    #   it creates a warning there, so capture here instead
    warn <- testthat::capture_warnings(as.integer(row.names(x)))
    if (isTRUE(any(grepl("NAs introduced by coercion", warn))))
      row.names(x) <- as.character(seq_along(row.names(x)))
    do.call(shapefile, append(dots, list(x = x, filename = filename2, overwrite = overwrite)))
  }
  x
}

#' @importFrom sf st_write
#' @importFrom tools file_ext
#' @rdname writeOutputs
writeOutputs.sf <- function(x, filename2 = NULL,
                            overwrite = getOption("reproducible.overwrite", FALSE),
                            ...) {
  if (!is.null(filename2)) {
    if (!nzchar(tools::file_ext(filename2))) {
      filename2 <- paste0(filename2, ".shp")
    }
    if (identical(".", dirname(filename2))) {
      filename2 <- normPath(filename2)
    }
    if (!all(file.exists(filename2)))
      overwrite = FALSE

    sf::st_write(obj = x, dsn = filename2, delete_dsn = overwrite,
                 ...)
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
#' @param type Character. 'writeRaster' or 'GDAL' to return the recommended data type for writing from the raster and gdalUtils packages, respectively, or 'projectRaster' to return recommended resampling type. Default is 'writeRaster'.
#' @return The appropriate data type for the range of values in \code{ras}. See \code{\link[raster]{dataType}} for details.
#'
#' @author Eliot McIntire
#' @author Ceres Barros
#' @author Ian Eddy
#' @export
#' @importFrom raster getValues
#' @rdname assessDataType
#'
#' @example inst/examples/example_assessDataType.R
assessDataType <- function(ras, type = 'writeRaster') {
  UseMethod("assessDataType")
}

#' @export
#' @importFrom raster getValues ncell
#' @rdname assessDataType
assessDataType.Raster <- function(ras, type = "writeRaster") {
  ## using ras@data@... is faster, but won't work for @values in large rasters
  N <- 1e5
  if (ncell(ras) > N) {
    rasVals <- suppressWarnings(raster::sampleRandom(x = ras, size = N))
  } else {
    rasVals <- raster::getValues(ras)
  }
  minVal <- ras@data@min
  maxVal <- ras@data@max
  signVal <- minVal < 0
  doubVal <-  any(floor(rasVals) != rasVals, na.rm = TRUE)  ## faster than any(x %% 1 != 0)

  ## writeRaster deals with infinite values as FLT8S
  # infVal <- any(!is.finite(minVal), !is.finite(maxVal))   ## faster than |

  if (!doubVal & !signVal) {
    ## only check for binary if there are no decimals and no signs
    logi <- all(!is.na(.bincode(na.omit(rasVals), c(-1,1))))  ## range needs to include 0

    if (logi) {
      datatype <- "LOG1S"
    } else {
      ## if() else is faster than if
      datatype <- if (maxVal <= 255) "INT1U" else
        if (maxVal <= 65534) "INT2U" else
          if (maxVal <= 4294967296) "INT4U" else    ## note that: dataType doc. advises against INT4U
            if (maxVal > 3.4e+38) "FLT8S" else "FLT4S"
    }
  } else {
    if (signVal & !doubVal) {
      ## if() else is faster than if
      datatype <- if (minVal >= -127 & maxVal <= 127) "INT1S" else
        if (minVal >= -32767 & maxVal <= 32767) "INT2S" else
          if (minVal >= -2147483647 & maxVal <=  2147483647) "INT4S" else    ## note that: dataType doc. advises against INT4U
            if (minVal < -3.4e+38 | maxVal > 3.4e+38) "FLT8S" else "FLT4S"
    } else {
      if (doubVal)
        datatype <- if (minVal < -3.4e+38 | maxVal > 3.4e+38) "FLT8S" else "FLT4S"
    }
  }
  #convert datatype if needed
  switch(type,
         GDAL = {
           switch(datatype,
                  LOG1S = {datatype <- "Byte"},
                  INT2S = {datatype <- "Int16"},
                  INT4S = {datatype <- "Int32"},
                  INT1U = {datatype <- "Byte"},
                  INT2U = {datatype <- "UInt16"},
                  INT4U = {datatype <- "UInt32"},
                  datatype <- "Float32" #there is no GDAL FLT8S
           )
         },
         projectRaster = {
           switch(datatype,
                  Float32 = {datatype <- "bilinear"},
                  Float64 = {datatype <- "bilinear"},
                  datatype <- "ngb"
           )
         },
         writeRaster = {},
         stop("incorrect argument: type must be one of writeRaster, projectRaster, or GDAL")
  )
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
#' Can be used to write prepared inputs on disk.
#'
#' @param ras  The RasterLayer or RasterStack for which data type will be assessed.
#' @return The appropriate data type for the range of values in \code{ras} for using GDAL.
#'         See \code{\link[raster]{dataType}} for details.
#' @author Eliot McIntire, Ceres Barros, Ian Eddy, and Tati Micheletti
#' @example inst/examples/example_assessDataTypeGDAL.R
#' @export
#' @importFrom raster getValues ncell sampleRandom
#' @rdname assessDataTypeGDAL
assessDataTypeGDAL <- function(ras) {
  ## using ras@data@... is faster, but won't work for @values in large rasters
  minVal <- ras@data@min
  maxVal <- ras@data@max
  signVal <- minVal < 0

  if (ras@file@datanotation != "FLT4S") {
    ## gdal deals with infinite values as Float32
    # infVal <- any(!is.finite(minVal), !is.finite(maxVal))   ## faster than |

    if (!signVal) {
      ## only check for binary if there are no decimals and no signs
      datatype <- if (maxVal <= 255) "Byte" else
        if (maxVal <= 65534) "UInt16" else
          if (maxVal <= 4294967296) "UInt32" else "Float32" #else transform your units
    } else {
      if (minVal >= -32767 & maxVal <= 32767) "Int16" else #there is no INT8 for gdal
        if (minVal >= -2147483647 & maxVal <=  2147483647) "Int32" else "Float32"
    }
  } else {
    if (ncell(ras) > 100000) {
      rasVals <- raster::sampleRandom(x = ras, size = 100000)
    } else {
      rasVals <- raster::getValues(ras)
    }

    #This method is slower but safer than getValues. Alternatives?
    doubVal <-  any(floor(rasVals) != rasVals, na.rm = TRUE)

    if (signVal & !doubVal) {
      datatype <- if (minVal >= -32767 & maxVal <= 32767) "Int16" else #there is no INT8 for gdal
        if (minVal >= -2147483647 & maxVal <=  2147483647) "Int32" else "Float32"
    } else
      if (doubVal) {
        datatype <- "Float32"
      } else {
        #data was FLT4S but doesn't need sign or decimal
        datatype <- if (maxVal <= 255) "Byte" else
          if (maxVal <= 65534) "UInt16" else
            if (maxVal <= 4294967296) "UInt32" else "Float32" #else transform your units
      }
  }

  datatype
}

#' @importFrom rlang eval_tidy
postProcessChecks <- function(studyArea, rasterToMatch, dots) {
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
    message("inputFilePath is being deprecated; use filename1")
    filename1 <- dots$inputFilePath
    dots$inputFilePath <- NULL
  }

  if (!is.null(dots$targetFilePath))  {
    message("targetFilePath is being deprecated; use filename1.")
    filename1 <- dots$targetFilePath
    dots$targetFilePath <- NULL
  }
  list(dots = dots, filename1 = filename1)
}

postProcessAllSpatial <- function(x, studyArea, rasterToMatch, useCache, filename1,
                                  filename2, useSAcrs, overwrite, targetCRS = NULL, ...) {
  dots <- list(...)

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
    # fix errors if methods available
    skipCacheMess <- "useCache is FALSE, skipping Cache"
    skipCacheMess2 <- "No cacheRepo supplied"

    ##################################
    # cropInputs
    ##################################
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
      x <- fixErrors(x = x, objectName = objectName,
                     useCache = useCache, ...)

      ##################################
      # projectInputs
      ##################################
      targetCRS <- .getTargetCRS(useSAcrs, studyArea, rasterToMatch,
                                 targetCRS)

      x <- Cache(projectInputs, x = x, targetCRS = targetCRS,
                 rasterToMatch = rasterToMatch, useCache = useCache, ...)
      # may need to fix again
      x <- fixErrors(x = x, objectName = objectName,
                     useCache = useCache, ...)

      ##################################
      # maskInputs
      ##################################
      x <- Cache(maskInputs, x = x, studyArea = studyArea,
                 rasterToMatch = rasterToMatch, useCache = useCache, ...)

      ##################################
      # filename
      ##################################
      newFilename <- determineFilename(filename1 = filename1, filename2 = filename2, ...)

      ##################################
      # writeOutputs
      ##################################
      x <- do.call(writeOutputs, append(list(x = quote(x), filename2 = newFilename,
                                             overwrite = overwrite), dots))

      if (dir.exists(file.path(raster::tmpDir(), "bigRasters"))) {
        ## Delete gdalwarp results in temp
        unlink(file.path(raster::tmpDir(), "bigRasters"), recursive = TRUE)
      }
    }
  }
  x
}
