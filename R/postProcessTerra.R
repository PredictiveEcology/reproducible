#' Transform a GIS dataset to have the metadata of another
#'
#' This function provides a single step to achieve the GIS operations "crop", "project",
#' "mask" and possibly "write". It uses primarily the \code{terra} package internally
#' (with some minor functions from \code{sf} and \code{raster})
#' in an attempt to be as efficient as possible.
#'
#' @details
#'
#' Below, "gridded" means a \code{Raster*} or \code{SpatRaster} class object.
#' "Vector" means \code{Spatial*} from \code{sp}, \code{sf} from \code{sf}, or
#' \code{SpatVector} from \code{terra}.
#'
#' @section \code{from} and \code{to} are both gridded:
#' Use case one is supply 2 gridded objects, \code{from} and \code{to}. This function
#' will make \code{from} look like \code{to}, i.e., the extent, projection, origin, and masking
#' where there are \code{NA} in the {to} will be the same as \code{from}.
#'
#' @section \code{from} is gridded, \code{to} is Vector:
#' Use case two is \code{from} is a gridded dataset (\code{Raster*} or \code{SpatRaster})
#' and \code{to} is a Vector gis dataset (specifically:
#' (e.g., \code{Spatial*} from \code{sp}, \code{sf} from \code{sf}, or
#' \code{SpatVector} from \code{terra}) that has information we want to impose on \code{from}.
#' It can be supplied as \code{to}, \code{projectTo}, \code{cropTo} or \code{maskTo}.
#'
#' @section any of \code{*To} arguments:
#' if one or more of the \code{*To} arguments are supplied, these will
#' override individual components of \code{to}. If \code{to} is omitted, then only
#' the \code{*To} arguments that are used will be performed.
#'
#' @section \code{from} and \code{to} are both Vector:
#' Use case four is when \code{from} is a Vector dataset and \code{to} or any of the \code{*To}
#' arguments are also Vector datasets. Similar to case 2 above, it will convert the \code{from}
#' by doing one or more of crop, project, mask, and write to disk. In the case of the
#' mask step, it will first aggregate (dissolve) all polygons so it is one single
#' polygon prior to doing the mask (which is actually \code{terra::intersect})
#'
#' @return
#' An object of the same class as \code{from}, but potentially cropped, projected, masked,
#' and written to disk.
#'
#' @param from A RasterLayer, RasterStack, RasterBrick, SpatRaster to do one or more of:
#'   crop, project, mask, and write
#' @param to A gridded (\code{Raster*} or \code{SpatRaster}) or polygonal
#'   (\code{Spatial*}, \code{sf} or \code{Spat*}) class object which is the object
#'   whose metadata will be the target for cropping, projecting, and masking of \code{from}.
#' @param cropTo Optional \code{Raster*}, \code{Spatial*}, \code{sf} or \code{Spat*} which,
#'   if supplied, will supply the extent with which to crop \code{from}. To omit
#'   cropping completed, set this to \code{NA}. If supplied, this will be used instead of \code{to}
#'   for the cropping step. Defaults to \code{NULL}, which means use \code{to}
#' @param projectTo Optional \code{Raster*} or \code{SpatRaster} which,
#'   if supplied, will supply the \code{crs}, \code{extent}, \code{res}, and \code{origin}
#'   to project the \code{from} to. To omit projecting, set this to NA.
#'   If supplied, this will be used instead of \code{to}
#'   for the projecting step. Defaults to \code{NULL}, which means use \code{to}
#' @param maskTo Optional \code{Spatial*}, \code{Raster*}, \code{sf} or \code{Spat*} which,
#'   if supplied, will supply the extent with which to mask \code{from}. To omit
#'   masking completed, set this to \code{NA}. If supplied, this will be used instead of \code{to}
#'   for the masking step. Defaults to \code{NULL}, which means use \code{to}
#' @param writeTo Optional character string of a filename to use `writeRaster` to save the final
#'   object. Default is \code{NULL}, which means there is no `writeRaster`
#' @param method Used if \code{projectTo} is not \code{NULL}, and is the method used for
#'   interpolation. See \code{terra::project}. Defaults to \code{"bilinear"}
#' @param datatype A character string, used if \code{writeTo} is not \code{NULL}. See \code{raster::writeRaster}
#' @param overwrite Logical. Used if \code{writeTo} is not \code{NULL}
#' @param ... Currently can be either \code{rasterToMatch}, \code{studyArea}, \code{filename2}
#'   to allow backwards
#'   compatibility with \code{postProcess}. \code{rasterToMatch} will be assigned to \code{to}
#'   and \code{studyArea} will be assigned to \code{cropTo} & \code{maskTo}. These will override any values
#'   passed to \code{to} or \code{maskTo}. \code{filename2} will be passed to \code{writeTo}.
#' @export
postProcessTerra <- function(from, to, cropTo = NULL, projectTo = NULL, maskTo = NULL,
                             writeTo = NULL, method = NULL, datatype = "FLT4S",
                             overwrite = TRUE, ...) {

  startTime <- Sys.time()
  dots <- list(...)
  if (!is.null(dots$rasterToMatch)) {
    messagePrepInputs("rasterToMatch is supplied (deprecated); assigning it to `to`")
    to <- dots$rasterToMatch
  }
  if (!is.null(dots$studyArea)) {
    messagePrepInputs("studyArea is supplied (deprecated); assigning it to `cropTo` & `maskTo`")
    maskTo <- dots$studyArea
    cropTo <- dots$studyArea
    if (dots$useSAcrs) {
      messagePrepInputs("useSAcrs is supplied (deprecated); assigning it to `projectTo`")
      projectTo <- dots$studyArea
    }
  }

  # These are unambiguous
  if (!is.null(dots$filename2)) writeTo <- dots$filename2
  if (!is.null(dots$targetCRS)) projectTo <- dots$targetCRS
  if (!is.null(dots$useSAcrs)) projectTo <- dots$studyArea

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
  isSpatRaster <- is(from, "SpatRaster")
  isVectorNonTerra <- isVector(from) && !isSpat(from)

  if (isRaster) {
    from <- terra::rast(from)
  } else if (isVectorNonTerra) {
    from <- terra::vect(from)
  }

  #############################################################
  # crop project mask sequence ################################
  #############################################################
  from <- cropTo(from, cropTo, needBuffer = TRUE) # crop first for speed
  from <- projectTo(from, projectTo, method = method) # need to project with edges intact
  from <- maskTo(from, maskTo)
  from <- cropTo(from, cropTo) # need to recrop to trim excess pixels in new projection

  # Put this message near the end so doesn't get lost
  if (is.naSpatial(cropTo) && isVector(from) && isVector(maskTo))  {
    messagePrepInputs("    ** cropTo is NA, but masking with 2 vectors (`from` & `maskTo`)")
    messagePrepInputs("      has the effect of cropping because it uses terra::intersect")
  }

  # from <- terra::setMinMax(from)

  # WRITE STEP
  from <- writeTo(from, writeTo, overwrite, isStack, isBrick, isRaster, isSpatRaster,
                  datatype = datatype)

  if (isStack && !is(from, "RasterStack")) from <- raster::stack(from) # coming out of writeRaster, becomes brick
  if (isBrick && !is(from, "RasterBrick")) from <- raster::brick(from) # coming out of writeRaster, becomes brick
  if (isRasterLayer && !is(from, "RasterLayer")) from <- raster::raster(from) # coming out of writeRaster, becomes brick
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
        if (isVector(projectTo)) {
          if (isGridded(from)) {
            if (!isSpat(projectTo))
              projectTo <- terra::vect(projectTo)

            # if (sf::st_crs("epsg:4326") != sf::st_crs(from)) {
            #   projectTo <- terra::rast(projectTo, resolution = res(from))
            # }
            messagePrepInputs("        projectTo is a vector dataset, which does not define")
            messagePrepInputs("        all metadata required. ")
            if (sf::st_crs("epsg:4326") != sf::st_crs(from)) {
              newRes <- res(from)
              messagePrepInputs("        Using resolution of ",paste(newRes, collapse = "x"),"m; ")
            } else {
              newRes <- 250
              messagePrepInputs("        projectTo also is longlat; projecting to 250m resolution; ")
            }
            projectTo <- terra::rast(projectTo, resolution = newRes)
            messagePrepInputs("        origin and extent from `ext(projectTo)`, and projection from `projectTo`.")
            messagePrepInputs("        If this is not correct, create a template gridded object and pass that to projectTo")

          } else {
            projectTo <- sf::st_crs(projectTo)$wkt
          }
          #
          # projectToIsMaskTo <- FALSE
        } else if (is(projectTo, "crs")) {
          projectTo <- projectTo$wkt
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
      ext <- sf::st_as_sfc(sf::st_bbox(cropTo)) # create extent as an object; keeps crs correctly
      if (!sf::st_crs(from) == sf::st_crs(ext)) { # This is sf way of comparing CRS -- raster::compareCRS doesn't work for newer CRS
        ext <- sf::st_transform(ext, sf::st_crs(from))
      }
      if (isVector(from))
        ext <- terra::vect(ext)
      messagePrepInputs("    cropping...")

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
      messagePrepInputs("       done!")
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
  if (!requireNamespace("terra")) stop("Need terra and sf: Require::Require(c('terra', 'sf'), require = FALSE)")
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
