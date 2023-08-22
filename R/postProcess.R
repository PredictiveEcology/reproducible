#' Generic function to post process objects
#'
#' \if{html}{\figure{lifecycle-maturing.svg}{options: alt="maturing"}}
#'
#' @export
#' @param x  A GIS object of postProcessing,
#'           e.g., Spat* or sf*. This can be provided as a
#'           `rlang::quosure` or a normal R object.
#' @importFrom utils capture.output
#' @seealso `prepInputs`
#' @rdname postProcess
postProcess <- function(x, ...) {
  UseMethod("postProcess")
}



#' @export
#' @rdname postProcess
postProcess.list <- function(x, ...) {
  lapply(x, function(y) postProcess(y, ...))
}

#' Post processing for GIS objects
#'
#' The method for GIS objects (terra `Spat*` & sf classes) will
#' crop, reproject, and mask, in that order.
#' This is a wrapper for [cropTo()], [fixErrorsIn()],
#' [projectTo()], [maskTo()] and [writeTo()],
#' with a required amount of data manipulation between these calls so that the crs match.
#'
#' @section Post processing sequence:
#'
#'   If the `rasterToMatch` or `studyArea` are passed, then
#'   the following sequence will occur:
#'
#'   \enumerate{
#'     \item Fix errors [fixErrorsIn()]. Currently only errors fixed are for
#'            `SpatialPolygons` using `buffer(..., width = 0)`.
#'     \item Crop using [cropTo()]
#'     \item Project using [projectTo()]
#'     \item Mask using [maskTo()]
#'     \item Determine file name [determineFilename()]
#'     \item Write that file name to disk, optionally [writeTo()]
#'   }
#'
#'   NOTE: checksumming does not occur during the post-processing stage, as
#'   there are no file downloads. To achieve fast results, wrap
#'   `prepInputs` with `Cache`
#'
#'
#' @param ... Additional arguments passed to methods. For `spatialClasses`,
#'            these are: [cropTo()], [fixErrorsIn()],
#'            [projectTo()], [maskTo()],
#'            [determineFilename()], and [writeTo()].
#'            Each of these may also pass `...` into other functions, like
#'            [writeTo()].
#'            This might include potentially important arguments like `datatype`,
#'            `format`. Also passed to `terra::project`,
#'            with likely important arguments such as `method = "bilinear"`.
#'            See details.
#'
#'
#' @section Backwards compatibility with `rasterToMatch` and/or `studyArea` arguments:
#'
#' For backwards compatibility, `postProcess` will continue to allow passing
#'  `rasterToMatch` and/or `studyArea` arguments. Depending on which of these
#'  are passed, different things will happen to the `targetFile` located at `filename1`.
#'
#' See *Use cases* section in [postProcessTo()] for post processing behaviour with
#'   the new `from` and `to` arguments.
#'
#' \subsection{If `targetFile` is a raster (`Raster*`, or `SpatRaster`) object:}{
#'   \tabular{lccc}{
#'                  \tab `rasterToMatch`      \tab `studyArea`  \tab  Both               \cr
#'     `extent`     \tab Yes                  \tab   Yes        \tab `rasterToMatch`     \cr
#'     `resolution` \tab Yes                  \tab   No         \tab `rasterToMatch`     \cr
#'     `projection` \tab Yes                  \tab   No*        \tab `rasterToMatch`*    \cr
#'     `alignment`  \tab Yes                  \tab   No         \tab `rasterToMatch`     \cr
#'     `mask`       \tab No**                 \tab   Yes        \tab `studyArea`**       \cr
#'   }
#'  *Can be overridden with `useSAcrs`.
#'
#'  **Will mask with `NA`s from `rasterToMatch` if `maskWithRTM`.
#' }
#'
#' \subsection{If `targetFile` is a vector (`Spatial*`, `sf` or `SpatVector`) object:}{
#'   \tabular{lccc}{
#'                  \tab `rasterToMatch`      \tab `studyArea`  \tab Both               \cr
#'     `extent`     \tab Yes                  \tab   Yes        \tab `rasterToMatch`    \cr
#'     `resolution` \tab NA                   \tab   NA         \tab NA                 \cr
#'     `projection` \tab Yes                  \tab   No*        \tab `rasterToMatch`*   \cr
#'     `alignment`  \tab NA                   \tab   NA         \tab NA                 \cr
#'     `mask`       \tab No                   \tab   Yes        \tab `studyArea`        \cr
#'   }
#'  *Can be overridden with `useSAcrs`
#' }
#'
#'
#' @export
#' @example inst/examples/example_postProcess.R
#' @rdname postProcess
#' @return A GIS file (e.g., `RasterLayer`, `SpatRaster` etc.) that has been
#' appropriately cropped, reprojected, masked, depending on the inputs.
#'
postProcess.default <- function(x, ...) {
  if (inherits(x, "quosure")) {
    .requireNamespace("rlang", stopOnFALSE = TRUE)
    x <- rlang::eval_tidy(x)
  }
  return(postProcessTo(from = x, ...))
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
#' @param ... Passed to `[cropTo()]`
#'
#' @param useCache Logical, default `getOption("reproducible.useCache", FALSE)`, whether
#'                 `Cache` is used internally.
#'
#' @inheritParams projectInputs
#'
#' @author Eliot McIntire, Jean Marchal, Ian Eddy, and Tati Micheletti
#' @export
#' @importFrom methods is
#' @return A GIS file (e.g., RasterLayer, SpatRaster etc.) that has been
#' appropriately cropped.
#' @rdname deprecated
#' @name deprecated
cropInputs <- function(x, studyArea, rasterToMatch, verbose = getOption("reproducible.verbose", 1), ...) {
  UseMethod("cropInputs")
}

#' @export
#' @rdname deprecated
cropInputs.default <- function(x, ...) {
  cropTo(x, ...)
}


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
#' @seealso [fixErrorsIn()], [postProcessTo()], [postProcess()]
#'
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
  fixErrorsIn(x)
}


#' Project `Raster*` or `Spatial*` or `sf` objects
#'
#' Deprecated. Use [projectTo()].
#'
#' @param x A `Raster*`, `Spatial*` or `sf` object
#'
#' @param targetCRS The CRS of x at the end  of this function (i.e., the goal)
#'
#' @param ... Passed to [projectTo()].
#'
#' @param rasterToMatch Template `Raster*` object passed to the `to` argument of
#'                      [projectTo()], thus will changing the
#'                      resolution and projection of `x`.
#'                      See details in [postProcessTo()].
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
  projectTo(x, projectTo = targetCRS, ...)
}





#' Mask module inputs
#'
#' `maskInputs` is deprecated. Use [maskTo()]
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
#' @rdname deprecated
#' @return A GIS file (e.g., RasterLayer, SpatRaster etc.) that has been
#' appropriately masked.
#' @seealso [maskTo()], [postProcessTo()] for related examples
maskInputs <- function(x, studyArea, ...) {
  UseMethod("maskInputs")
}


#' @export
maskInputs.default <- function(x, studyArea, rasterToMatch = NULL, maskWithRTM = NULL,
                               verbose = getOption("reproducible.verbose", 1), ...) {
  maskTo(x, ...)
}


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
#' @inheritParams projectInputs
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
#' @inheritParams postProcess
determineFilename <- function(filename2 = NULL, filename1 = NULL,
                              destinationPath = getOption("reproducible.destinationPath", "."),
                              verbose = getOption("reproducible.verbose", 1),
                              prefix = "Small", ...) {
  if (!is.null(filename2)) {
    dots <- list(...)

    if (!is.null(dots$inputFilePath)) {
      stop("inputFilePath is being deprecated; use filename1")
    }

    if (!is.null(dots$postProcessedFilename)) {
      stop("postProcessedFilename is being deprecated; use filename2")
    }

    if (!is.null(dots$targetFilePath)) {
      stop(
        "targetFilePath is being deprecated from determineFilename:\n",
        "  use filename2 and filename1."
      )
    }

    if (!(is.logical(filename2) || is.character(filename2) || is.null(filename2))) {
      stop("filename2 must be logical or character string or NULL")
    }

    filename2 <- if (!identical(filename2, FALSE)) { # allow TRUE or path
      if (isTRUE(filename2)) {
        # 1. Take destinationPath, if it exists
        # 2. Take dirname of filename1, if it exists and is absolute path
        # 3. Take getwd()
        theDir <- destinationPath
        if (is.null(destinationPath)) {
          if (is.character(filename1)) {
            if (isAbsolutePath(filename1)) {
              theDir <- dirname(filename1)
              messagePrepInputs("filename2 is NULL; using dirname(filename1) as destinationPath",
                verbose = verbose
              )
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
        "or set filename2 to NULL to prevent saving to disk",
        verbose = verbose
      )
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
#' @param ... Passed into [writeTo()]
#'
#' @inheritParams prepInputs
#'
#' @author Eliot McIntire and Jean Marchal
#' @return A GIS file (e.g., SpatRaster etc.) that has been
#' appropriately written to disk. In the case of vector datasets, this will
#' be a side effect. In the case of gridded objects (Raster*, SpatRaster), the
#' object will have a file-backing.
#' @importFrom methods is
#' @export
#' @rdname deprecated
#'
#' @examples
#' if (requireNamespace("terra", quietly = TRUE)) {
#'   r <- terra::rast(terra::ext(0, 100, 0, 100), vals = 1:1e2)
#'
#'   tf <- tempfile(fileext = ".tif")
#'   writeOutputs(r, tf)
#' }
writeOutputs <- function(x, ..., # filename2,
                         overwrite = getOption("reproducible.overwrite", NULL),
                         verbose = getOption("reproducible.verbose", 1)) {
  UseMethod("writeOutputs")
}

#' @rdname deprecated
#' @export
writeOutputs.default <- function(x, ..., # filename2,
                                 overwrite = getOption("reproducible.overwrite", FALSE),
                                 verbose = getOption("reproducible.verbose", 1)) {
  writeTo(x, ..., overwrite = overwrite, verbose = verbose)
}



#' Assess the appropriate raster layer data type
#'
#' When writing raster-type objects to disk, a `datatype` can be specified. These
#' functions help identify what smallest `datatype` can be used.
#'
#' @param ras  The `RasterLayer` or `RasterStack` for which data type will be assessed.
#' @param type Character. `"writeRaster"` (default) or `"GDAL"` (defunct)
#'             to return the recommended
#'             data type for writing from the raster packages, respectively, or
#'             `"projectRaster"` to return recommended resampling type.
#'
#' @export
#' @return A character string indicating the data type of the spatial layer
#' (e.g., "INT2U"). See `terra::datatype()`
#' @rdname assessDataType
#'
#' @example inst/examples/example_assessDataType.R
assessDataType <- function(ras, type = "writeRaster") {
  UseMethod("assessDataType")
}

#' @export
#' @rdname assessDataType
assessDataType.default <- function(ras, type = "writeRaster") {
  ## using ras@data@... is faster, but won't work for @values in large rasters

  if (inherits(ras, "RasterStack")) {
    xs <- lapply(names(ras), FUN = function(x) {
      y <- assessDataType(ras = ras[[x]], type)
      return(y)
    })

    return(unlist(xs))
  }

  N <- 1e5

  if (nlayers2(ras) > 1) { # for RasterStack, RasterBrick, SpatRaster of nlyr > 1
    xs <- lapply(seq(names(ras)), FUN = function(x) { # can't use names(ras) directly because can't have same name 2x in lapply, but can in SpatRaster
      y <- assessDataType(ras = ras[[x]], type)
      return(y)
    })

    return(unlist(xs))
  }

  datatype <- NULL
  .requireNamespace("terra", stopOnFALSE = TRUE)
  if (terra::ncell(ras) > 1e8) { # for very large rasters, try a different way
    maxValCurrent <- maxFn(ras)
    ras <- setMinMaxIfNeeded(ras)
    # if (maxValCurrent != maxValue(ras))
    datatype <- dataType2(ras)
  } else {
    ras <- setMinMaxIfNeeded(ras)
  }

  if (is.null(datatype)) {
    if (terra::ncell(ras) > N) {
      rasVals <- tryCatch(suppressWarnings(sampRand(x = ras, size = N)),
        error = function(x) rep(NA_integer_, N)
      )
      if (is.factor(rasVals))
        rasVals <- as.integer(rasVals)
    } else {
      rasVals <- values2(ras)
    }
    minVal <- minFn(ras) # min(ras@data@min)
    maxVal <- maxFn(ras) # max(ras@data@max)
    signVal <- minVal < 0
    doubVal <- any(floor(rasVals) != rasVals, na.rm = TRUE) ## faster than any(x %% 1 != 0)
    datatype <- if (doubVal) {
      names(MinValsFlts)[min(which(minVal >= MinValsFlts & maxVal <= MaxValsFlts))]
    } else {
      ## only check for binary if there are no decimals and no signs
      logi <- all(!is.na(.bincode(rasVals[!is.na(rasVals)], c(-1, 1)))) ## range needs to include 0
      if (logi) {
        "LOG1S"
      } else {
        names(MinVals)[min(which(minVal >= unlist(MinVals) & maxVal <= unlist(MaxVals)))]
      }
    }
  }
  # convert datatype if needed
  datatype <- switchDataTypes(datatype, type = type)
  datatype
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
      if (!is.na(suppressWarnings(maxFn(ras))) && !is.na(suppressWarnings(minFn(ras)))) {
        if (length(.getColors(ras)[[1]]) == (maxFn(ras) - minFn(ras) + 1)) {
          return(ras)
        }
      }
    }
    possibleShortCut <- maxValCurrent %in% c(unlist(MaxVals), unlist(MaxVals) + 1)
    if (isTRUE(all(possibleShortCut))) {
      needSetMinMax <- TRUE
    }
  }
  if (isTRUE(needSetMinMax)) {
    .requireNamespace("terra", stopOnFALSE = TRUE)
    large <- if (nlayers2(ras) > 25 || terra::ncell(ras) > 1e7) TRUE else FALSE
    if (large) message("  Large ", class(ras), " detected; setting minimum and maximum may take time")
    suppressWarnings(ras <- terra::setMinMax(ras))
    if (large) message("  ... Done")
  }
  ras
}


roundTo6Dec <- function(x) {
  # check if integer
  if (all(x %% 1 != 0)) {
    # First test whether they are remotely close to each other
    rounded <- round(x, 6)
    if (!identical(x, rounded)) {
      x <- rounded
    }
  }
  x
}

#' @importFrom utils capture.output
suppressWarningsSpecific <- function(code, falseWarnings, verbose = getOption("reproducible.verbose", 1)) {
  warns <- list()
  suppressWarnings(withCallingHandlers(
    {
      yy <- eval(code)
    },
    warning = function(xx) {
      trueWarnings <- grep(falseWarnings, xx$message, invert = TRUE, value = TRUE)
      if (length(trueWarnings)) {
        warns <<- paste(trueWarnings, collapse = "\n  ")
      }
    },
    error = function(xx) stop(xx$message),
    message = function(xx) xx
  ))
  if (length(warns)) {
    lapply(warns, warning)
  }

  return(yy)
}

#' @importFrom utils capture.output
captureWarningsToAttr <- function(code, verbose = getOption("reproducible.verbose", 1)) {
  warn <- capture.output(
    type = "message",
    suppressWarnings(withCallingHandlers(
      {
        yy <- eval(code)
      },
      warning = function(xx) {
        messagePrepInputs(paste0("warn::", xx$messagePrepInputs), verbose = verbose)
      }
    ))
  )
  trueWarnings <- grepl("warn::.*", warn)
  if (length(warn[!trueWarnings])) {
    messagePrepInputs(paste(warn[!trueWarnings], collapse = "\n  "))
  }
  warn <- gsub("warn::", "", warn[trueWarnings])
  attr(yy, "warning") <- paste(warn, collapse = "\n")
  return(yy)
}

dtp <- list()
dtp[["INT1"]] <- 255 / 2
dtp[["INT2"]] <- 65534 / 2
dtp[["INT4"]] <- 4294967296 / 2
dtp[["FLT4"]] <- 3.4e+38
dtp[["FLT8"]] <- Inf
dtps <- c("INT1U", "INT1S", "INT2U", "INT2S", "INT4U", "INT4S", "FLT4S", "FLT8S")
names(dtps) <- dtps
datatypeVals <- lapply(dtps, function(namdtp) {
  d <- dtp[grep(substr(namdtp, 1, 4), names(dtp), value = TRUE)]
  div <- substr(namdtp, 5, 5)
  mult <- ifelse(div == "U", 2, 1)
  Max <- trunc(unlist(d) * mult)
  sign1 <- ifelse(div == "U", 0, -1)
  Min <- Max * sign1
  list(Min = Min, Max = Max)
})
MaxVals <- lapply(datatypeVals, function(x) unname(x$Max))
MinVals <- lapply(datatypeVals, function(x) unname(x$Min))
MinValsFlts <- MinVals[grep("FLT", names(MinVals), value = TRUE)]
MaxValsFlts <- MaxVals[grep("FLT", names(MinVals), value = TRUE)]
projNotWKT2warn <- "Using PROJ not WKT2"


progressBarCode <- function(..., doProgress = TRUE, message,
                            colour = getOption("reproducible.messageColourCache"),
                            verbose = getOption("reproducible.verbose"),
                            verboseLevel = 1) {
  messageColoured(message, colour = colour, verbose = verbose, verboseLevel = verboseLevel)
  out <- eval(...)
  if (doProgress) messageColoured("\b Done!", colour = colour, verbose = verbose, verboseLevel = verboseLevel)
  out
}


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
        datatype <- "Float32" # there is no GDAL FLT8S
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


sampRand <- function(x, size, method, ...) {
  if (isRaster(r)) {
    .requireNamespace("raster")
    raster::sampleRandom(x, size = size, ...)
  } else {
    .requireNamespace("terra")
    out <- terra::spatSample(x, size = size, ...)
    out <- out[, 1] # it is returned as a df
  }
}
