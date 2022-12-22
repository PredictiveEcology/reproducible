#' Transform a GIS dataset so it has the properties (extent, projection, mask) of another
#'
#' This function provides a single step to achieve the GIS operations "crop", "project",
#' "mask" and possibly "write". This is intended to completely replace [postProcess()]
#' (which primarily used GDAL, `Raster` and `sp`).
#' It uses primarily the `terra` package internally
#' (with some minor functions from `sf` and `raster`)
#' in an attempt to be as efficient as possible.
#' For this function, Gridded means a `Raster*` class object from `raster` or
#' a `SpatRaster` class object from `terra`.
#' Vector means a `Spatial*` class object from `sp`, a `sf` class object
#' from `sf`, or a `SpatVector` class object from `terra`. This function is currently
#' part of the internals for some cases encountered by [postProcess()].
#'
#' @section Use Cases:
#'
#' The table below shows what will result from passing different classes to `from`
#' and `to`:
#'
#' \tabular{lll}{
#'   `from`\tab `to`\tab `from` will have:\cr
#'   `Gridded`\tab `Gridded` \tab the extent, projection, origin, resolution and
#'                         masking where there are `NA` from the `to`\cr
#'   `Gridded`\tab `Vector` \tab the projection, origin, and mask from `to`, and extent will
#'                        be a round number of pixels that fit within the extent
#'                        of `to`. Resolution will be the same as `from` \cr
#'   `Vector`\tab `Vector` \tab the projection, origin, extent and mask from `to`\cr
#' }
#'
#'
#' If one or more of the `*To` arguments are supplied, these will
#' override individual components of `to`. If `to` is omitted or `NULL`,
#' then only the `*To` arguments that are used will be performed. In all cases,
#' setting a `*To` argument to `NA` will prevent that step from happening.
#'
#' @section Backwards compatibility with `postProcess`:
#'
#' \subsection{`rasterToMatch` and `studyArea`:}{
#'
#'   If these are supplied, `postProcessTerra` will use them instead
#'   of `to`. If only `rasterToMatch` is supplied, it will be assigned to
#'   `to`. If only `studyArea` is supplied, it will be used for `cropTo`
#'   and `maskTo`; it will only be used for `projectTo` if `useSAcrs = TRUE`.
#'   If both `rasterToMatch` and `studyArea` are supplied,
#'   `studyArea` will only be applied to `maskTo` (and optionally `projectTo` if
#'   `useSAcrs = TRUE`); everything else will be from `rasterToMatch`.
#' }
#'
#' \subsection{`targetCRS`, `filename2`, `useSAcrs`:}{
#'
#'   `targetCRS` if supplied will be assigned to `projectTo`. `filename2` will
#'   be assigned to `writeTo`. If `useSAcrs` is set, then the `studyArea`
#'   will be assigned to `projectTo`. All of these will override any existing values
#'   for these arguments.
#' }
#'
#'
#' @section Cropping:
#' If `cropTo` is not `NA`, postProcessTerra does cropping twice, both the first and last steps.
#' It does it first for speed, as cropping is a very fast algorithm. This will quickly remove
#' a bunch of pixels that are not necessary. But, to not create bias, this first crop is padded
#' by  `2 * res(from)[1]`), so that edge cells still have a complete set of neighbours.
#' The second crop is at the end, after projecting and masking. After the projection step,
#' the crop is no longer tight. Under some conditions, masking will effectively mask and crop in
#' one step, but under some conditions, this is not true, and the mask leaves padded NAs out to
#' the extent of the `from` (as it is after crop, project, mask). Thus the second
#' crop removes all NA cells so they are tight to the mask.
#'
#'
#' @return
#' An object of the same class as `from`, but potentially cropped (via [cropTo()]),
#' projected (via [projectTo()]), masked (via [maskTo()]), and written to disk
#' (via [writeTo()]).
#'
#' @param from A Gridded or Vector dataset on which to do one or more of:
#'   crop, project, mask, and write
#' @param to A Gridded or Vector dataset which is the object
#'   whose metadata will be the target for cropping, projecting, and masking of `from`.
#' @param cropTo Optional Gridded or Vector dataset which,
#'   if supplied, will supply the extent with which to crop `from`. To omit
#'   cropping completely, set this to `NA`. If supplied, this will override `to`
#'   for the cropping step. Defaults to `NULL`, which means use `to`
#' @param projectTo Optional Gridded or Vector dataset, or `crs` object (e.g., sf::st_crs).
#'   If Gridded it will supply
#'   the `crs`, `extent`, `res`, and `origin`
#'   to project the `from` to. If Vector, it will provide the `crs` only.
#'   The resolution and extent will be taken from `res(from)` (i.e. `ncol(from)*nrow(from)`).
#'   If a Vector, the extent of the `projectTo` is not used (unless it is also passed to `cropTo`.
#'   To omit projecting, set this to `NA`.
#'   If supplied, this will override `to`
#'   for the projecting step. Defaults to `NULL`, which means use `to`
#' @param maskTo Optional Gridded or Vector dataset which,
#'   if supplied, will supply the extent with which to mask `from`. If Gridded,
#'   it will mask with the `NA` values on the `maskTo`; if Vector, it will
#'   mask on the `terra::aggregate(maskTo)`. To omit
#'   masking completely, set this to `NA`. If supplied,
#'   this will override `to` for the masking step.
#'   Defaults to `NULL`, which means use `to`
#' @param writeTo Optional character string of a filename to use `writeRaster` to save the final
#'   object. Default is `NULL`, which means there is no `writeRaster`
#' @param method Used if `projectTo` is not `NULL`, and is the method used for
#'   interpolation. See `terra::project`. Defaults to `"bilinear"`
#' @param datatype A character string, used if `writeTo` is not `NULL`. See `raster::writeRaster`
#' @param overwrite Logical. Used if `writeTo` is not `NULL`; also if `terra` determines
#'   that the object requires writing to disk during a `crop`, `mask` or `project` call
#'   e.g., because it is too large.
#' @param ... Currently can be either `rasterToMatch`, `studyArea`, `filename2`,
#'   `useSAcrs`, or `targetCRS` to allow backwards
#'   compatibility with `postProcess`. See section below for details.
#' @inheritParams Cache
#' @export
#'
#' @seealso This function is meant to replace [postProcess()] with the more efficient
#' and faster `terra` functions.
#'
postProcessTerra <- function(from, to, cropTo = NULL, projectTo = NULL, maskTo = NULL,
                             writeTo = NULL, method = NULL, datatype = "FLT4S",
                             overwrite = TRUE, ...) {

  startTime <- Sys.time()
  dots <- list(...)
  if (!is.null(dots$studyArea)) {
    messagePrepInputs("studyArea is supplied (deprecated); assigning it to `cropTo` & `maskTo`")
    if (isSpatial(dots$studyArea))
      dots$studyArea <- terra::vect(dots$studyArea)
    maskTo <- dots$studyArea
    cropTo <- dots$studyArea
  }

  if (!is.null(dots$rasterToMatch)) {
    messagePrepInputs("rasterToMatch is supplied (deprecated); assigning it to `projectTo` and `cropTo`")
    to <- dots$rasterToMatch
    projectTo <- dots$rasterToMatch # be explicit here in case studyArea is supplied
    cropTo <- dots$rasterToMatch # be explicit here in case studyArea is supplied
  }

  # These are unambiguous
  if (!is.null(dots$filename2)) {
    messagePrepInputs("filename2 is supplied (deprecated); assigning it to `writeTo`")
    writeTo <- dots$filename2
  }
  if (!is.null(dots$targetCRS)) {
    messagePrepInputs("targetCRS is supplied (deprecated); assigning it to `projectTo`")
    projectTo <- dots$targetCRS
  }
  if (isTRUE(dots$useSAcrs)) {
    messagePrepInputs("useSAcrs is supplied (deprecated); assigning studyArea to `projectTo`")
    projectTo <- dots$studyArea
  }

  if (is.null(method)) method <- "bilinear"

  # Deal with combinations of to and *To
  if (missing(to)) to <- NULL
  if (is.null(to)) {
    if (missing(cropTo)) cropTo <- NULL
    if (missing(maskTo)) maskTo <- NULL
    if (missing(projectTo)) projectTo <- NULL
  } else {
    # case where all *To are NULL --> use to as in the arg defaults
    if (is.null(cropTo)) cropTo <- to
    if (is.null(maskTo)) maskTo <- to
    if (is.null(projectTo)) projectTo <- to
  }


  # ASSERTION STEP
  postProcessTerraAssertions(from, to, cropTo, maskTo, projectTo)

  # Get the original class of from so that it can be recovered
  origFromClass <- is(from)
  isRaster <- any(origFromClass == "Raster")
  isRasterLayer <- any(origFromClass == "RasterLayer")
  isStack <- any(origFromClass == "RasterStack")
  isBrick <- any(origFromClass == "RasterBrick")
  isSF <- any(origFromClass == "sf")
  isSpatial <- any(startsWith(origFromClass, "Spatial"))
  isSpatRaster <- any(origFromClass == "SpatRaster")
  isVectorNonTerra <- isVector(from) && !isSpat(from)

  # converting sf to terra then cropping is slower than cropping then converting to terra
  #    so if both are vector datasets, and sf format, crop first
  from <- cropSF(from, cropTo)

  if (isRaster) {
    from <- terra::rast(from)
  } # else if (isVectorNonTerra) {
  #   osFrom <- object.size(from)
  #   lg <- osFrom > 5e8
  #   if (lg) {
  #     st <- Sys.time()
  #     messagePrepInputs("  `from` is large, converting to terra object will take some time ...")
  #   }
  #   from <- suppressWarningsSpecific(terra::vect(sf::st_as_sf(from)), shldBeChar)
  #   if (lg) {
  #     messagePrepInputs("  done in ", format(difftime(Sys.time(), st),
  #                                            units = "secs", digits = 3))
  #
  #   }
  # }

  #############################################################
  # crop project mask sequence ################################
  #############################################################
  # browser()
  from <- cropTo(from, cropTo, needBuffer = TRUE, overwrite = overwrite) # crop first for speed
  from <- projectTo(from, projectTo, method = method, overwrite = overwrite) # need to project with edges intact
  from <- maskTo(from, maskTo, overwrite = overwrite)
  from <- cropTo(from, cropTo, overwrite = overwrite) # need to recrop to trim excess pixels in new projection

  # Put this message near the end so doesn't get lost
  if (is.naSpatial(cropTo) && isVector(maskTo))  {
    messagePrepInputs("    ** cropTo is NA, but maskTo is a Vector dataset; ")
    messagePrepInputs("      this has the effect of cropping anyway")
  }

  # from <- terra::setMinMax(from)

  # WRITE STEP
  from <- writeTo(from, writeTo, overwrite, isStack, isBrick, isRaster, isSpatRaster,
                  datatype = datatype)

  # REVERT TO ORIGINAL INPUT CLASS
  from <- revertClass(from, isStack, isBrick, isRasterLayer, isSF, isSpatial)
  messagePrepInputs("  postProcessTerra done in ", format(difftime(Sys.time(), startTime),
                                                          units = "secs", digits = 3))
  from
}

isSpatial <- function(x) inherits(x, "Spatial")
isSpat <- function(x) is(x, "SpatRaster") || is(x, "SpatVector")
isSpat2 <- function(origClass) any(origClass %in% c("SpatVector", "SpatRaster"))
isGridded <- function(x) is(x, "SpatRaster") || is(x, "Raster")
isVector <-  function(x) is(x, "SpatVector") || is(x, "Spatial") || isSF(x)
isSpatialAny <- function(x) isGridded(x) || isVector(x)
isSF <- function(x) is(x, "sf") || is(x, "sfc")


#' Fix common errors in GIS layers, using `terra`
#'
#' Currently, this only tests for validity of a SpatVect file, then if there is a problem,
#' it will run `terra::makeValid`
#' @export
#' @param x The SpatStat or SpatVect object to try to fix.
#' @param error The error message, e.g., coming from `try(...)`
#' @inheritParams Cache
#' @param fromFnName The function name that produced the error, e.g., `maskTo`
#'
#' @return
#' An object of the same class as `x`, but with some errors fixed via `terra::makeValid()`
#'
#'
fixErrorsTerra <- function(x, error = NULL, verbose = getOption("reproducible.verbose"), fromFnName = "") {
  if (isVector(x)) {
    if (!is.null(error))
      messageDeclareError(error, fromFnName, verbose)
    if (isSF(x)) {
      xValids <- sf::st_is_valid(x)
      if (any(!xValids))
        x <- sf::st_make_valid(x)
    } else {
      xValids <- terra::is.valid(x)
      if (any(!xValids))
        x <- terra::makeValid(x)
    }

  }
  x
}

#' @export
#' @rdname postProcessTerra
#' @param touches See `terra::mask`
maskTo <- function(from, maskTo, touches = FALSE, overwrite = FALSE,
                   verbose = getOption("reproducible.verbose")) {
  if (!is.null(maskTo)) {
    if (!is.naSpatial(maskTo)) {
      origFromClass <- class(from)

      if (is(maskTo, "Raster"))
        maskTo <- terra::rast(maskTo)
      if (is(maskTo, "Spatial"))
        maskTo <- sf::st_as_sf(maskTo)
      # if (isSF(maskTo))
      #   maskTo <- terra::vect(maskTo)
      if (!isSpat(from) && !isSF(from)) {
        if (isVector(from)) {
          from <- terra::vect(from)
        } else {
          from <- terra::rast(from)
        }
      }


      if (!sf::st_crs(from) == sf::st_crs(maskTo)) {
        if (isGridded(maskTo)) {
          maskTo <- terra::project(maskTo, from, overwrite = overwrite)
        } else {
          if (isSF(maskTo)) {
            maskTo <- sf::st_transform(maskTo, sf::st_crs(from))
          } else {
            maskTo <- terra::project(maskTo, from)
          }

        }
      }
      messagePrepInputs("    masking...", appendLF = FALSE)
      st <- Sys.time()


      # There are 2 tries; first is for `maskTo`, second is for `from`, rather than fix both in one step, which may be unnecessary
      maskAttempts <- 0
      env <- environment()

      attempt <- 1
      triedFrom <- NA
      while (attempt <= 2) {
        fromInt <- try({
          if (isVector(maskTo))
            if (length(maskTo) > 1) {
              if (isSF(maskTo)) {
                maskTo <- sf::st_union(maskTo)
              } else {
                maskTo <- terra::aggregate(maskTo)
              }
            }

          if (isVector(from)) {
            if (isSF(from)) {
              sf::st_intersection(from, maskTo)
            } else {
              terra::intersect(from, maskTo)
            }

          } else {
            if (isGridded(maskTo)) {
              terra::mask(from, maskTo, overwrite = overwrite)
            } else {
              if (isSF(maskTo)) {
                maskTo <- terra::vect(maskTo) # alternative is stars, and that is not Suggests
              }
              terra::mask(from, maskTo, touches = touches, overwrite = overwrite)
            }
          }
        }, silent = TRUE)
        if (is(fromInt, "try-error")) {
          if (attempt >= 1) {
            whichFailed <- grepl("geom 0", fromInt)
            if (isTRUE(whichFailed) && !(triedFrom %in% TRUE)) { # don't try same one again
              from <- fixErrorsTerra(from, error = fromInt, fromFnName = "maskTo", verbose = verbose)
              triedFrom <- TRUE
            } else {
              maskTo <- fixErrorsTerra(maskTo, error = fromInt, fromFnName = "maskTo", verbose = verbose)
              triedFrom <- FALSE
            }
          } else {
            stop(fromInt)
          }
        } else {
          if (attempt > 1)
            messagePrepInputs("...fixed!", verbose = verbose, verboseLevel = 1 , appendLF = FALSE)
          break
        }
        attempt <- attempt + 1
      }

      from <- fromInt
      messagePrepInputs("...done in ", format(difftime(Sys.time(), st), units = "secs", digits = 3))
      from <- revertClass(from, origFromClass = origFromClass)
    }
  }
  from
}

#' @export
#' @rdname postProcessTerra
projectTo <- function(from, projectTo, method, overwrite = FALSE) {
  if (!is.null(projectTo)) {
    origFromClass <- is(from)
    if (!is.naSpatial(projectTo)) {
      if (is(projectTo, "Raster"))
        projectTo <- terra::rast(projectTo)

      projectToOrig <- projectTo # keep for below
      sameProj <- sf::st_crs(projectTo) == sf::st_crs(from)
      isProjectToVecOrCRS <- is(projectTo, "crs") || isVector(projectTo)
      sameRes <- if (isVector(from) || isProjectToVecOrCRS) {
        TRUE
      } else {
        all(res(projectTo) == res(from))
      }

      if (sameProj && sameRes) {
        messagePrepInputs("    projection of from is same as projectTo, not projecting")
      } else {
        messagePrepInputs("    projecting...", appendLF = FALSE)
        st <- Sys.time()
        if (isProjectToVecOrCRS) {
          projectToTmp <- sf::st_as_sfc(sf::st_bbox(from))
          if (isVector(projectTo))
            projectTo <- sf::st_crs(projectTo)
          projectToTmp <- sf::st_transform(projectToTmp, projectTo)
          projectTo <- terra::vect(projectToTmp)
        }

        if (isVector(projectTo)) {
          if (isGridded(from)) {
            if (!isSpat(projectTo))
              projectTo <- terra::vect(projectTo)

            # if (sf::st_crs("epsg:4326") != sf::st_crs(from)) {
            #   projectTo <- terra::rast(projectTo, resolution = res(from))
            # }
            messagePrepInputs("")
            messagePrepInputs("         projectTo is a Vector dataset, which does not define all metadata required. ")
            if (sf::st_crs("epsg:4326") != sf::st_crs(from)) {
              newRes <- res(from)
              messagePrepInputs("         Using resolution of ",paste(newRes, collapse = "x"),"m; ")
              projectTo <- terra::rast(projectTo, resolution = newRes)
            } else {
              projectTo <- terra::rast(ncols = terra::ncol(from), nrows = terra::nrow(from),
                                       crs = sf::st_crs(projectTo)$wkt, extent = terra::ext(projectTo))
              messagePrepInputs("         Projecting to ", paste(collapse = "x", round(res(projectTo), 2))," resolution (same # pixels as `from`)")
            }

            messagePrepInputs("         in the projection of `projectTo`, using the origin and extent")
            messagePrepInputs("         from `ext(from)` (in the projection from `projectTo`).")
            messagePrepInputs("         If this is not correct, create a template gridded object and pass that to projectTo...")
            messagePrepInputs("         ", appendLF = FALSE)

          } else {
            projectTo <- sf::st_crs(projectTo)$wkt
          }
          #
          # projectToIsMaskTo <- FALSE
        }

        # Since we only use the crs when projectTo is a Vector, no need to "fixErrorsTerra"
        from <- if (isVector(from)) {
          isSpatial <- is(from, "Spatial")
          if (isSpatial)
            from <- suppressWarningsSpecific(terra::vect(from), shldBeChar)
          isSF <- isSF(from)
          if (isSF) {
            from <- sf::st_transform(from, projectTo)
          } else {
            from <- terra::project(from, projectTo)
          }

          if (isSpatial) from <- as(from, "Spatial")
          from
        } else {
          terra::project(from, projectTo, method = method, overwrite = overwrite)
        }
        messagePrepInputs("done in ", format(difftime(Sys.time(), st), units = "secs", digits = 3))
      }
    }
    from <- revertClass(from, origFromClass = origFromClass)
  }
  from
}

#' @param needBuffer Logical. Defaults to `TRUE`, meaning nothing is done out
#'   of the ordinary. If `TRUE`, then a buffer around the cropTo, so that if a reprojection
#'   has to happen on the `cropTo` prior to using it as a crop layer, then a buffer
#'   of 1.5 * res(cropTo) will occur prior, so that no edges are cut off.
#' @export
#' @rdname postProcessTerra
cropTo <- function(from, cropTo = NULL, needBuffer = TRUE, overwrite = FALSE,
                   verbose = getOption("reproducible.verbose")) {
  if (!is.null(cropTo)) {
    omit <- FALSE
    origFromClass <- is(from)

    if (!isSpatialAny(cropTo))
      if (is.na(cropTo)) omit <- TRUE

    if (isSpatial(cropTo))
      cropTo <- terra::vect(cropTo)

    if (!omit) {
      messagePrepInputs("    cropping..." , appendLF = FALSE)
      st <- Sys.time()

      ext <- sf::st_as_sfc(sf::st_bbox(cropTo)) # create extent as an object; keeps crs correctly
      if (!sf::st_crs(from) == sf::st_crs(ext)) { # This is sf way of comparing CRS -- raster::compareCRS doesn't work for newer CRS
        if (isVector(cropTo)) {
          if (isSpat(cropTo)) {
            cropToInFromCRS <- terra::project(cropTo, terra::crs(from))
          } else {
            cropToInFromCRS <- sf::st_transform(sf::st_as_sf(cropTo), sf::st_crs(from))
          }
        } else {
          cropToInFromCRS <- terra::project(cropTo, terra::crs(from))
          # THIS IS WHAN cropTo is a Gridded object
        }
        ext <- sf::st_as_sfc(sf::st_bbox(cropToInFromCRS)) # create extent as an object; keeps crs correctly
      }
      if (isVector(from) && isSpat(from))
        ext <- terra::vect(ext)

      # This is only needed if crop happens before a projection... need to cells beyond edges so projection is accurate
      if (needBuffer)
        if (isGridded(from)) {
          res <- res(from)
          if (!isSpat(ext))
            ext <- terra::vect(ext)
          extTmp <- terra::ext(ext)
          ext <- terra::extend(extTmp, res[1] * 2)
        }

      attempt <- 1
      while (attempt <= 2) {
        if (isGridded(from)) {
          fromInt <- try(terra::crop(from, ext, overwrite = overwrite), silent = TRUE)
        } else {
          if (isSF(from)) {
            fromInt <- try(sf::st_crop(from, ext), silent = TRUE)
          } else {
            fromInt <- try(terra::crop(from, ext), silent = TRUE)
          }

        }
        if (is(fromInt, "try-error")) {
          if (attempt == 1) {
            from <- fixErrorsTerra(from, error = fromInt, fromFnName = "cropTo", verbose = verbose)
          } else {
            stop(fromInt)
          }
        } else {
          if (attempt > 1)
            messagePrepInputs("...fixed!", verbose = verbose, verboseLevel = 1,
                              appendLF = FALSE)
          break
        }
        attempt <- attempt + 1
      }
      from <- fromInt
      messagePrepInputs("...done in ", format(difftime(Sys.time(), st), units = "secs", digits = 3))
    }
    from <- revertClass(from, origFromClass = origFromClass)
  }
  from
}

#' @export
#' @rdname postProcessTerra
#' @param isStack,isBrick,isRaster,isSpatRaster Logical. Default `FALSE`. Used to convert `from`
#'   back to these classes prior to writing.
#'
writeTo <- function(from, writeTo, overwrite, isStack = FALSE, isBrick = FALSE, isRaster = FALSE,
                    isSpatRaster = FALSE, datatype = "FLT4S") {
  if (isStack) from <- raster::stack(from)
  if (isBrick) from <- raster::brick(from)

  if (!is.null(writeTo))
    if (!is.na(writeTo)) {
      messagePrepInputs("    writing...", appendLF = FALSE)
      st <- Sys.time()
      if (isRaster)
        from <- raster::writeRaster(from, filename = writeTo, overwrite = overwrite,
                                    datatype = datatype)
      if (isSpatRaster)
        from <- terra::writeRaster(from, filename = writeTo, overwrite = overwrite,
                                   datatype = datatype)
      if (is(from, "SpatVector")) {
        if (isTRUE(overwrite))
          unlink(writeTo, force = TRUE, recursive = TRUE)
        written <- terra::writeVector(from, filename = writeTo, overwrite = overwrite)
      }
      messagePrepInputs("...done in ", format(difftime(Sys.time(), st), units = "secs", digits = 3))

    }

  from
}

postProcessTerraAssertions <- function(from, to, cropTo, maskTo, projectTo) {
  if (!requireNamespace("terra", quietly = TRUE) && getOption("reproducible.useTerra", FALSE))
    stop("Need terra and sf: install.packages(c('terra', 'sf'))")
  if (!requireNamespace("sf", quietly = TRUE)) stop("Need sf: install.packages('sf')")

  if (!(isSpatialAny(from))) stop("from must be a Raster* or SpatRaster")

  if (!missing(to)) {
    if (!is.null(to)) {
      if (!isSpatialAny(to)) stop("to must be a Raster*, Spat*, sf or Spatial object")
      if (isVector(from)) if (!isVector(to)) stop("if from is a Vector object, to must also be a Vector object")
    }
  }

  if (!missing(cropTo)) {
    if (!is.naSpatial(cropTo))
      if (!is.null(cropTo)) {
        if (!isSpatialAny(cropTo)) stop("cropTo must be a Raster*, Spat*, sf or Spatial object")
        if (isVector(from)) if (!isVector(cropTo)) stop("if from is a Vector object, cropTo must also be a Vector object")
      }
  }
  if (!missing(maskTo)) {
    if (!is.naSpatial(maskTo))
      if (!is.null(maskTo)) {
        if (!isSpatialAny(maskTo)) stop("maskTo must be a Raster*, Spat*, sf or Spatial object")
        if (isVector(from)) if (!isVector(maskTo)) stop("if from is a Vector object, maskTo must also be a Vector object")
      }
  }
  if (!missing(projectTo)) {
    if (!is.naSpatial(projectTo))
      if (!is.null(projectTo)) {
        if (is.character(projectTo))
          projectTo <- try(silent = TRUE, sf::st_crs(projectTo))
        if (!is(projectTo, "crs")) {
          if (!isSpatialAny(projectTo)) stop("projectTo must be a Raster*, Spat*, sf or Spatial object")
          if (isVector(from)) if (!isVector(projectTo)) stop("if from is a Vector object, projectTo must also be a Vector object")
        }
      }
  }

  return(invisible())
}

is.naSpatial <- function(x) {
  isna <- FALSE
  if (!is.null(x))
    if (!isSpatialAny(x)) {
      if (all(is.na(x))) isna <- TRUE
    }
  isna
}

cropSF <- function(from, cropToVect, verbose = getOption("reproducible.verbose")) {
  st <- Sys.time()
  if (isSF(from) && (isSF(cropToVect) || is(cropToVect, "Spatial"))) {
    messagePrepInputs("    pre-cropping because `from` is sf and cropTo is sf/Spatial*")
    attempt <- 1
    while (attempt <= 2) {

      from2 <- try(sf::st_crop(from, sf::st_transform(sf::st_as_sfc(sf::st_bbox(cropToVect)),
                                                        sf::st_crs(from))),
                     silent = TRUE)
      if (is(from2, "try-error")) {
        if (attempt == 1) {
          messageDeclareError(error = from2, fromFnName = "cropSF", verbose)
          from <- fixErrors(from, useCache = FALSE, verbose = verbose - 1)
        } else {
          stop(from2)
        }
      } else {
        if (attempt > 1)
          messagePrepInputs("...fixed!", verbose = verbose, verboseLevel = 1,
                            appendLF = FALSE)
        break
      }
      attempt <- attempt + 1


    }
    if (!is(from2, "try-error"))
      from <- from2
    messagePrepInputs("  done in ", format(difftime(Sys.time(), st),
                                           units = "secs", digits = 3))

  }
  from
}

shldBeChar <- "should be a character value"

revertClass <- function(from, isStack = FALSE, isBrick = FALSE, isRasterLayer = FALSE,
                        isSF = FALSE, isSpatial = FALSE, origFromClass = NULL) {
  if (!isSpat2(origFromClass)) {
    if (!is.null(origFromClass)) {
      # overrides all others!
      isStack <- any(origFromClass == "RasterStack")
      isBrick <- any(origFromClass == "RasterBrick")
      isRasterLayer <- any(origFromClass == "RasterLayer")
      isSF <- any(origFromClass == "sf")
      isSpatial <- any(startsWith(origFromClass, "Spatial"))

    }
    if (isStack && !is(from, "RasterStack")) from <- raster::stack(from) # coming out of writeRaster, becomes brick
    if (isBrick && !is(from, "RasterBrick")) from <- raster::brick(from) # coming out of writeRaster, becomes brick
    if (isRasterLayer && !is(from, "RasterLayer")) from <- raster::raster(from) # coming out of writeRaster, becomes brick
    if (isSF || isSpatial) {
      from <- sf::st_as_sf(from)
      if (isSpatial) {
        from <- sf::as_Spatial(from)
      }
    }
  }
  from
}

messageDeclareError <- function(error, fromFnName, verbose) {
  errWOWordError <- gsub("Error {0,1}: ", "", error)
  messagePrepInputs("    ", fromFnName, " resulted in following error: \n    - ", errWOWordError, "    --> attempting to fix",
                    appendLF = FALSE, verbose = verbose, verboseLevel = 1)
}
