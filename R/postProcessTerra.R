#' Transform a GIS dataset to have the metadata of another
#'
#' This function provides a single step to achieve the GIS operations "crop", "project",
#' "mask" and possibly "write". It uses primarily the \code{terra} package internally
#' (with some minor functions from \code{sf} and \code{raster})
#' in an attempt to be as efficient as possible.
#' For this function, Gridded means a \code{Raster*} class object from \code{raster} or
#' a \code{SpatRaster} class object from \code{terra}.
#' Vector means a \code{Spatial*} class object from \code{sp}, a \code{sf} class object
#' from \code{sf}, or a \code{SpatVector} class object from \code{terra}.

#'
#' @details
#'
#' @section Use Cases:
#'
#' The table below shows what will result from passing different classes to \code{from}
#' and \code{to}:
#'
#' \tabular{lll}{
#'   \code{from}\tab \code{to}\tab \code{from} will have:\cr
#'   \code{Gridded}\tab \code{Gridded} \tab the extent, projection, origin, resolution and
#'                         masking where there are \code{NA} from the {to}\cr
#'   \code{Gridded}\tab \code{Vector} \tab the projection, origin, and mask from \code{to}, and extent will
#'                        be a round number of pixels that fit within the extent
#'                        of \code{to}. Resolution will be the same as \code{from} \cr
#'   \code{Vector}\tab \code{Vector} \tab the projection, origin, extent and mask from \code{to}\cr
#' }
#'
#'
#' If one or more of the \code{*To} arguments are supplied, these will
#' override individual components of \code{to}. If \code{to} is omitted or \code{NULL},
#' then only the \code{*To} arguments that are used will be performed. In all cases,
#' setting a \code{*To} argument to \code{NA} will prevent that step from happening.
#'
#' @section Backwards compatibility with \code{postProcess}:
#'
#' \subsection{\code{rasterToMatch} and \code{studyArea}:}{
#'
#'   If these are supplied, \code{postProcessTerra} will use them instead
#'   of \code{to}. If only \code{rasterToMatch} is supplied, it will be assigned to
#'   \code{to}. If only \code{studyArea} is supplied, it will be used for \code{cropTo}
#'   and \code{maskTo}; it will only be used for \code{projectTo} if \code{useSAcrs = TRUE}.
#'   If both \code{rasterToMatch} and \code{studyArea} are supplied,
#'   \code{studyArea} will only be applied to \code{maskTo} (and optionally \code{projectTo} if
#'   \code{useSAcrs = TRUE}); everything else will be from \code{rasterToMatch}.
#' }
#'
#' \subsection{\code{targetCRS}, \code{filename2}, \code{useSAcrs}:}{
#'
#'   \code{targetCRS} if supplied will be assigned to \code{projectTo}. \code{filename2} will
#'   be assigned to \code{writeTo}. If \code{useSAcrs} is set, then the \code{studyArea}
#'   will be assigned to \code{projectTo}. All of these will override any existing values
#'   for these arguments.
#' }
#'
#'
#' @section Cropping:
#' If `cropTo` is not `NA`, postProcessTerra does cropping twice, both the first and last steps.
#' It does it first for speed, as cropping is a very fast algorithm. This will quickly remove
#' a bunch of pixels that are not necessary. But, to not create bias, this first crop is padded
#' by  \code{2 * res(from)[1]}), so that edge cells still have a complete set of neighbours.
#' The second crop is at the end, after projecting and masking. After the projection step,
#' the crop is no longer tight. Under some conditions, masking will effectively mask and crop in
#' on step, but under some conditions, this is not true, and the mask leaves padded NAs out to
#' the extent of the \code{from} (as it is after crop, project, mask). Thus the second
#' crop removes all NA cells so they are tight to the mask.
#'
#'
#' @return
#' An object of the same class as \code{from}, but potentially cropped, projected, masked,
#' and written to disk.
#'
#' @param from A Gridded or Vector dataset on which to do one or more of:
#'   crop, project, mask, and write
#' @param to A Gridded or Vector dataset which is the object
#'   whose metadata will be the target for cropping, projecting, and masking of \code{from}.
#' @param cropTo Optional Gridded or Vector dataset which,
#'   if supplied, will supply the extent with which to crop \code{from}. To omit
#'   cropping completely, set this to \code{NA}. If supplied, this will override \code{to}
#'   for the cropping step. Defaults to \code{NULL}, which means use \code{to}
#' @param projectTo Optional Gridded or Vector dataset, or \code{crs} object (e.g., sf::st_crs).
#'   If Gridded it will supply
#'   the \code{crs}, \code{extent}, \code{res}, and \code{origin}
#'   to project the \code{from} to. If Vector, it will provide the \code{crs} only.
#'   The resolution and extent will be taken from \code{res(from)} (i.e. \code{ncol(from)*nrow(from)}).
#'   If a Vector, the extent of the \code{projectTo} is not used (unless it is also passed to \code{cropTo}.
#'   To omit projecting, set this to \code{NA}.
#'   If supplied, this will override \code{to}
#'   for the projecting step. Defaults to \code{NULL}, which means use \code{to}
#' @param maskTo Optional Gridded or Vector dataset which,
#'   if supplied, will supply the extent with which to mask \code{from}. If Gridded,
#'   it will mask with the \code{NA} values on the \code{maskTo}; if Vector, it will
#'   mask on the \code{terra::aggregate(maskTo)}. To omit
#'   masking completely, set this to \code{NA}. If supplied,
#'   this will override \code{to} for the masking step.
#'   Defaults to \code{NULL}, which means use \code{to}
#' @param writeTo Optional character string of a filename to use `writeRaster` to save the final
#'   object. Default is \code{NULL}, which means there is no `writeRaster`
#' @param method Used if \code{projectTo} is not \code{NULL}, and is the method used for
#'   interpolation. See \code{terra::project}. Defaults to \code{"bilinear"}
#' @param datatype A character string, used if \code{writeTo} is not \code{NULL}. See \code{raster::writeRaster}
#' @param overwrite Logical. Used if \code{writeTo} is not \code{NULL}
#' @param ... Currently can be either \code{rasterToMatch}, \code{studyArea}, \code{filename2},
#'   \code{useSAcrs}, or \code{targetCRS} to allow backwards
#'   compatibility with \code{postProcess}. See section below for details.
#' @export
postProcessTerra <- function(from, to, cropTo = NULL, projectTo = NULL, maskTo = NULL,
                             writeTo = NULL, method = NULL, datatype = "FLT4S",
                             overwrite = TRUE, ...) {

  startTime <- Sys.time()
  dots <- list(...)
  if (!is.null(dots$studyArea)) {
    messagePrepInputs("studyArea is supplied (deprecated); assigning it to `cropTo` & `maskTo`")
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
  isRaster <- is(from, "Raster")
  isRasterLayer <- is(from, "RasterLayer")
  isStack <- is(from, "RasterStack")
  isBrick <- is(from, "RasterBrick")
  isSF <- is(from, "sf")
  isSpatial <- is(from, "Spatial")
  isSpatRaster <- is(from, "SpatRaster")
  isVectorNonTerra <- isVector(from) && !isSpat(from)

  # converting sf to terra then cropping is slower than cropping then converting to terra
  #    so if both are vector datasets, and sf format, crop first
  from <- cropSF(from, cropTo)

  if (isRaster) {
    from <- terra::rast(from)
  } else if (isVectorNonTerra) {
    osFrom <- object.size(from)
    lg <- osFrom > 5e8
    if (lg) {
      st <- Sys.time()
      messagePrepInputs("  `from` is large, converting to terra object will take some time ...")
    }
    from <- terra::vect(from)
    if (lg) {
      messagePrepInputs("  done in ", format(difftime(Sys.time(), st),
                                             units = "secs", digits = 3))

    }
  }

  #############################################################
  # crop project mask sequence ################################
  #############################################################
  from <- cropTo(from, cropTo, needBuffer = TRUE) # crop first for speed
  from <- projectTo(from, projectTo, method = method) # need to project with edges intact
  from <- maskTo(from, maskTo)
  from <- cropTo(from, cropTo) # need to recrop to trim excess pixels in new projection

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
  if (isStack && !is(from, "RasterStack")) from <- raster::stack(from) # coming out of writeRaster, becomes brick
  if (isBrick && !is(from, "RasterBrick")) from <- raster::brick(from) # coming out of writeRaster, becomes brick
  if (isRasterLayer && !is(from, "RasterLayer")) from <- raster::raster(from) # coming out of writeRaster, becomes brick
  if (isSF || isSpatial) {
    from <- sf::st_as_sf(from)
    if (isSpatial) {
      from <- sf::as_Spatial(from)
    }
  }
  messagePrepInputs("  postProcessTerra done in ", format(difftime(Sys.time(), startTime),
                                                          units = "secs", digits = 3))
  from
}

isSpat <- function(x) is(x, "SpatRaster") || is(x, "SpatVector")
isGridded <- function(x) is(x, "SpatRaster") || is(x, "Raster")
isVector <-  function(x) is(x, "SpatVector") || is(x, "Spatial") || is(x, "sf")
isSpatialAny <- function(x) isGridded(x) || isVector(x)


fixErrorsTerra <- function(x) {
  if (isVector(x)) {
    xValids <- terra::is.valid(x)
    if (any(!xValids))
      x <- terra::makeValid(x)
  }
  x
}

maskTo <- function(from, maskTo) {
  if (!is.null(maskTo)) {
    if (!is.naSpatial(maskTo)) {

      if (is(maskTo, "Raster"))
        maskTo <- terra::rast(maskTo)
      if (is(maskTo, "Spatial"))
        maskTo <- sf::st_as_sf(maskTo)
      if (is(maskTo, "sf"))
        maskTo <- terra::vect(maskTo)

      if (!sf::st_crs(from) == sf::st_crs(maskTo))
        maskTo <- terra::project(maskTo, from)
      messagePrepInputs("    masking...")
      st <- Sys.time()


      # There are 2 retry statements; first is for `maskTo`, second is for `from`, rather than fix both in one step, which may be unnecessary
      maskAttempts <- 0
      env <- environment()
      fromInt <- retry(retries = 2, silent = FALSE, exponentialDecayBase = 1, messageFn = messagePrepInputs,
                       expr = quote({

                       retry(retries = 2, silent = FALSE, exponentialDecayBase = 1,
                             messageFn = messagePrepInputs,
                             expr = quote(
                               {
                                 assign("maskAttempts", get("maskAttempts", envir = env, inherits = FALSE) + 1,
                                        envir = env)
                                 if (isVector(maskTo))
                                   if (length(maskTo) > 1)
                                     maskTo <- terra::aggregate(maskTo)
                                 if (isVector(from)) {
                                   terra::intersect(from, maskTo)
                                 } else {
                                   terra::mask(from, maskTo)
                                 }
                               }
                             ),
                             exprBetween = quote({
                               if (get("maskAttempts", envir = env, inherits = FALSE) == 1) {
                                 messagePrepInputs("    Mask results in an error; attempting to fix `maskTo`",
                                                   appendLF = FALSE)
                                 maskTo <- fixErrorsTerra(maskTo)
                               }
                               maskTo <- maskTo
                             }))
                       }),     exprBetween = quote({
                         messagePrepInputs("    Mask is still failing after fixing `maskTo`; attempting to fix `from`",
                                           appendLF = FALSE)
                         from <- fixErrorsTerra(from)
                       }))
      from <- fromInt
      messagePrepInputs("       done in ", format(difftime(Sys.time(), st), units = "secs", digits = 3))
    }
  }

  from
}

projectTo <- function(from, projectTo, method) {
  if (!is.null(projectTo)) {
    if (!is.naSpatial(projectTo)) {
      if (is(projectTo, "Raster"))
        projectTo <- terra::rast(projectTo)

      projectToOrig <- projectTo # keep for below

      if (sf::st_crs(projectTo) == sf::st_crs(from)) {
        messagePrepInputs("    projection of from is same as projectTo, not projecting")
      } else {
        messagePrepInputs("    projecting...")
        st <- Sys.time()
        if (is(projectTo, "crs") || isVector(projectTo)) {
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
            messagePrepInputs("         If this is not correct, create a template gridded object and pass that to projectTo")

          } else {
            projectTo <- sf::st_crs(projectTo)$wkt
          }
          #
          # projectToIsMaskTo <- FALSE
        }

        # Since we only use the crs when projectTo is a Vector, no need to "fixErrorsTerra"
        from <- if (isVector(from)) {
          terra::project(from, projectTo)
        } else {
          terra::project(from, projectTo, method = method)
        }
        messagePrepInputs("       done in ", format(difftime(Sys.time(), st), units = "secs", digits = 3))
      }
    }
  }
  from
}

cropTo <- function(from, cropTo, needBuffer = FALSE) {
  if (!is.null(cropTo)) {
    omit <- FALSE
    if (!isSpatialAny(cropTo))
      if (is.na(cropTo)) omit <- TRUE

    if (!omit) {
      messagePrepInputs("    cropping...")
      st <- Sys.time()

      ext <- sf::st_as_sfc(sf::st_bbox(cropTo)) # create extent as an object; keeps crs correctly
      if (!sf::st_crs(from) == sf::st_crs(ext)) { # This is sf way of comparing CRS -- raster::compareCRS doesn't work for newer CRS
        ext <- sf::st_transform(ext, sf::st_crs(from))
      }
      if (isVector(from))
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

      fromInt <- retry(retries = 2, silent = FALSE, exponentialDecayBase = 1,
                       messageFn = messagePrepInputs,
                       expr = quote(
                         terra::crop(from, ext)
                       ),
                       exprBetween = quote({
                         # check validities
                         messagePrepInputs("    Crop results in an error; attempting to fix",
                                           appendLF = FALSE)
                         from <- fixErrorsTerra(from)
                       }))
      from <- fromInt
      messagePrepInputs("       done in ", format(difftime(Sys.time(), st), units = "secs", digits = 3))
    }
  }
  from
}

writeTo <- function(from, writeTo, overwrite, isStack = FALSE, isBrick = FALSE, isRaster = FALSE,
                    isSpatRaster = FALSE, datatype = "FLT4S") {
  if (isStack) from <- raster::stack(from)
  if (isBrick) from <- raster::brick(from)

  if (!is.null(writeTo))
    if (!is.na(writeTo)) {
      messagePrepInputs("    writing...")
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
      messagePrepInputs("       done in ", format(difftime(Sys.time(), st), units = "secs", digits = 3))

    }

  from
}

postProcessTerraAssertions <- function(from, to, cropTo, maskTo, projectTo) {
  if (!requireNamespace("terra") && getOption("reproducible.useTerra", FALSE))
    stop("Need terra and sf: Require::Require(c('terra', 'sf'), require = FALSE)")
  if (!requireNamespace("sf")) stop("Need sf: Require::Require(c('sf'), require = FALSE)")

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
      if (is.na(x)) isna <- TRUE
    }
  isna
}

cropSF <- function(from, cropToVect) {
  if (is(from, "sf") && (is(cropToVect, "sf") || is(cropToVect, "Spatial"))) {
    messagePrepInputs("    pre-cropping because `from` is sf and cropTo is sf/Spatial*")
    from2 <- try(retry(retries = 2, silent = FALSE, exponentialDecayBase = 1,
                   messageFn = messagePrepInputs,
                   expr = quote(
                     {
                       sf::st_crop(from, sf::st_transform(sf::st_as_sfc(sf::st_bbox(cropToVect)), sf::st_crs(from)))
                     }
                   ),
                   exprBetween = quote({
                     from <- fixErrors(from, useCache = FALSE)
                   })))
    if (!is(from2, "try-error"))
      from <- from2
  }
  from
}
