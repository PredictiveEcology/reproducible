#' Transform a GIS dataset so it has the properties (extent, projection, mask) of another
#'
#' This function provides a single step to achieve the GIS operations
#' "pre-crop-with-buffer-to-speed-up-projection", "project",
#' "post-projection-crop", "mask" and possibly "write".
#' It uses primarily the `terra` package internally
#' (with some minor functions from `sf`)
#' in an attempt to be as efficient as possible. Currently, this function is tested
#' with `sf`, `SpatVector`, `SpatRaster`, `Raster*` and `Spatial*` objects passed
#' to `from`, and the same plus `SpatExtent`, and `crs` passed to `to` or the
#' relevant `*to` functions.
#' For this function, Gridded means a `Raster*` class object from `raster` or
#' a `SpatRaster` class object from `terra`.
#' Vector means a `Spatial*` class object from `sp`, a `sf` class object
#' from `sf`, or a `SpatVector` class object from `terra`.
#' This function is also used internally with the deprecated family [postProcess()],
#' `*Inputs`, such as [cropInputs()].
#'
#' @details
#' `postProcessTo` is a wrapper around (an initial "wide" crop for speed)
#' `cropTo(needBuffer = TRUE)`, `projectTo`,
#' `cropTo` (the actual crop for precision), `maskTo`, `writeTo`.
#'  Users can call each of these individually.
#'
#' `postProcessTerra` is the early name of this function that is now `postProcessTo`.
#'
#' @section Use Cases:
#'
#' The table below shows what will result from passing different classes to `from`
#' and `to`:
#'
#' \tabular{lll}{
#'   **`from`**\tab **`to`**   \tab **`from`** will have:                             \cr
#'   `Gridded` \tab `Gridded`  \tab the extent, projection, origin, resolution
#'                                  and masking where there are `NA` from the `to`    \cr
#'   `Gridded` \tab `Vector`   \tab the projection, origin, and mask from `to`, and
#'                                  extent will be a round number of pixels that
#'                                  fit within the extent of `to`. Resolution will
#'                                  be the same as `from`.  See section
#'                                  below about `projectTo`. \cr
#'   `Vector` \tab `Vector`    \tab the projection, origin, extent and mask from `to` \cr
#' }
#'
#'
#' If one or more of the `*To` arguments are supplied, these will
#' override individual components of `to`. If `to` is omitted or `NULL`,
#' then only the `*To` arguments that are used will be performed. In all cases,
#' setting a `*To` argument to `NA` will prevent that step from happening.
#'
#' @section `projectTo`:
#' Since these functions use the gis capabilities of `sf` and `terra`, they will only
#' be able to do things that those functions can do. One key caution, which is
#' stated clearly in `?terra::project` is that projection of a raster (i.e., gridded)
#' object should always be with another gridded object. If the user chooses to
#' supply a `projectTo` that is a vector object for a `from` that is gridded,
#' there may be unexpected failures due e.g., to extents not overlapping during
#' the `maskTo` stage.
#'
#' @section Backwards compatibility with `postProcess`:
#'
#' \subsection{`rasterToMatch` and `studyArea`:}{
#'
#'   If these are supplied, `postProcessTo` will use them instead
#'   of `to`. If only `rasterToMatch` is supplied, it will be assigned to
#'   `to`. If only `studyArea` is supplied, it will be used for `cropTo`
#'   and `maskTo`; it will only be used for `projectTo` if `useSAcrs = TRUE`.
#'   If both `rasterToMatch` and `studyArea` are supplied,
#'   `studyArea` will only be applied to `maskTo` (unless `maskWithRTM = TRUE`),
#'    and, optionally, to `projectTo` (if `useSAcrs = TRUE`); everything else
#'    will be from `rasterToMatch`.
#'
#' }
#'
#' \subsection{`targetCRS`, `filename2`, `useSAcrs`, `maskWithRTM`:}{
#'
#'   `targetCRS` if supplied will be assigned to `projectTo`. `filename2` will
#'   be assigned to `writeTo`. If `useSAcrs` is set, then the `studyArea`
#'   will be assigned to `projectTo`. If `maskWithRTM` is used, then the
#'   `rasterToMath` will be assigned to `maskTo`. All of these will override
#'   any existing values for these arguments.
#'
#' }
#'
#'  See also [postProcess()] documentation section on
#'  *Backwards compatibility with `rasterToMatch` and/or `studyArea`* for further
#'  detail.
#'
#' @section Cropping:
#' If `cropTo` is not `NA`, `postProcessTo` does cropping twice, both the first and last steps.
#' It does it first for speed, as cropping is a very fast algorithm. This will quickly remove
#' a bunch of pixels that are not necessary. But, to not create bias, this first crop is padded
#' by  `2 * res(from)[1]`), so that edge cells still have a complete set of neighbours.
#' The second crop is at the end, after projecting and masking. After the projection step,
#' the crop is no longer tight. Under some conditions, masking will effectively mask and crop in
#' one step, but under some conditions, this is not true, and the mask leaves padded NAs out to
#' the extent of the `from` (as it is after crop, project, mask). Thus the second
#' crop removes all NA cells so they are tight to the mask.
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
#'   If supplied, this will override `to` for the projecting step.
#'   Defaults to `NULL`, which means use `to`.
#'   **Attention.** Conflicts may arise with when `projectTo` is a Vector/CRS object with a
#'   distinct CRS from `to`. Because `to` is used for masking *after* `from` is re-projected using
#'   `projectTo`, the extents of `to` and `from` may no longer overlap (as in *align*)
#'   perfectly leading to failure during
#'   the masking step. We  recommend passing a raster templates to `projectTo` whose extent and CRS
#'   are both compatible with the object used later for masking (either `to` or `maskTo`).
#' @param maskTo Optional Gridded or Vector dataset which,
#'   if supplied, will supply the extent with which to mask `from`.
#'   If Gridded, it will mask with the `NA` values on the `maskTo`;
#'   if Vector, it will mask on the `terra::aggregate(maskTo)`.
#'   To omit masking completely, set this to `NA`.
#'   If supplied, this will override `to` for the masking step.
#'   Defaults to `NULL`, which means use `to`
#' @param writeTo Optional character string of a filename to use `writeRaster` to save the final
#'   object. Default is `NULL`, which means there is no `writeRaster`
#' @param overwrite Logical. Used if `writeTo` is not `NULL`; also if `terra` determines
#'   that the object requires writing to disk during a `crop`, `mask` or `project` call
#'   e.g., because it is too large.
#' @param ... Arguments passed to `terra::mask` (for `maskTo`), `terra::project` (for `projectTo`)
#'   or `terra::writeRaster` (for `writeTo`) and not used for `cropTo`, as well `postProcess`'s
#'   `rasterToMatch` and `studyArea` arguments (see below). Commonly used arguments might be
#'   `method`, `touches`, and `datatype`. If `filename` is passed, it will be ignored; use
#'   `writeTo = `.
#' @inheritParams Cache
#' @export
#'
#' @seealso This function is meant to replace [postProcess()] with the more efficient
#' and faster `terra` functions.
#'
postProcessTo <- function(from, to,
                          cropTo = NULL, projectTo = NULL, maskTo = NULL, writeTo = NULL,
                          overwrite = TRUE, verbose = getOption("reproducible.verbose"),
                          ...) {
  startTime <- Sys.time()
  remapOldArgs(...) # converts studyArea, rasterToMatch, filename2, useSAcrs, targetCRS


  # Deal with combinations of to and *To
  if (missing(to)) to <- NULL
  if (is.null(to)) {
    if (missing(cropTo)) cropTo <- NULL
    if (missing(maskTo)) maskTo <- NULL
    if (missing(projectTo)) projectTo <- NULL
  } else {
    if (isRaster(to)) {
      .requireNamespace("terra", stopOnFALSE = TRUE)
      to <- terra::rast(to)
    }
    # case where all *To are NULL --> use to as in the arg defaults
    if (is.null(cropTo)) cropTo <- to
    if (is.null(maskTo)) maskTo <- to
    if (is.null(projectTo)) projectTo <- to
  }

  # ASSERTION STEP
  postProcessToAssertions(from, to, cropTo, maskTo, projectTo)

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
    fromCRS <- terra::crs(from)
    from <- terra::rast(from)
    if (!nzchar(terra::crs(from))) {
      terra::crs(from) <- fromCRS
    } # $input
  } else if (isSpatial) {
    osFrom <- object.size(from)
    lg <- osFrom > 5e8
    if (lg) {
      st <- Sys.time()
      messagePrepInputs("  `from` is large, converting to terra object will take some time ...",
        verbose = verbose
      )
    }
    from <- suppressWarningsSpecific(terra::vect(from), shldBeChar)
    if (lg) {
      messagePrepInputs("  done in ", format(difftime(Sys.time(), st),
        units = "secs", digits = 3
      ),
      verbose = verbose
      )
    }
  }

  #############################################################
  # crop project mask sequence ################################
  #############################################################
  from <- cropTo(from, cropTo, needBuffer = TRUE, ..., overwrite = overwrite) # crop first for speed
  from <- projectTo(from, projectTo, ..., overwrite = overwrite) # need to project with edges intact
  from <- maskTo(from, maskTo, ..., overwrite = overwrite)
  from <- cropTo(from, cropTo, needBuffer = FALSE, ..., overwrite = overwrite) # need to recrop to trim excess pixels in new projection

  # Put this message near the end so doesn't get lost
  if (is.naSpatial(cropTo) && isVector(maskTo)) {
    messagePrepInputs("    ** cropTo is NA, but maskTo is a Vector dataset; ",
      verbose = verbose
    )
    messagePrepInputs("      this has the effect of cropping anyway",
      verbose = verbose
    )
  }

  # from <- terra::setMinMax(from)

  # WRITE STEP
  from <- writeTo(
    from, writeTo, overwrite, isStack, isBrick, isRaster, isSpatRaster,
    ...
  )

  # REVERT TO ORIGINAL INPUT CLASS
  from <- revertClass(from, isStack, isBrick, isRasterLayer, isSF, isSpatial)
  messagePrepInputs("  postProcessTo done in ", format(difftime(Sys.time(), startTime),
    units = "secs", digits = 3
  ),
  verbose = verbose
  )
  from
}

#' @export
#' @rdname postProcessTo
postProcessTerra <- postProcessTo

isSpatial <- function(x) inherits(x, "Spatial")
isSpatVector <- function(x) is(x, "SpatVector")
isSpat <- function(x) is(x, "SpatRaster") || isSpatVector(x)
isSpat2 <- function(origClass) any(origClass %in% c("SpatVector", "SpatRaster"))
isGridded <- function(x) is(x, "SpatRaster") || is(x, "Raster")
isVector <- function(x) isSpatVector(x) || is(x, "Spatial") || isSF(x)
isSpatialAny <- function(x) isGridded(x) || isVector(x)
isSF <- function(x) is(x, "sf") || is(x, "sfc")
isRaster <- function(x) is(x, "Raster")
isCRSANY <- function(x) isCRSSF(x) || isCRScharacter(x) || isCRSTerra(x)
isCRSSF <- function(x) is(x, "crs")
isCRScharacter <- function(x) is.character(x) && (grepl("DATUM", x) || grepl("+proj", x))
isCRSTerra <- function(x) is(x, "CRS")

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
fixErrorsIn <- function(x, error = NULL, verbose = getOption("reproducible.verbose"), fromFnName = "") {
  if (isVector(x)) {
    os <- 0
    if (!is.null(error)) {
      messageDeclareError(error, fromFnName, verbose)
      os <- objSize(x)
      if (os > 1e9) {
        messagePrepInputs("... this may take a long time because the object is large (",
          format(os), ")",
          verbose = verbose
        )
      }
    }
    if (isSF(x)) {
      .requireNamespace("sf", stopOnFALSE = TRUE)
      xValids <- sf::st_is_valid(x)
      if (any(!xValids)) {
        if (os > 1e9) {
          messagePrepInputs("... found invalid components ... running sf::st_make_valid",
            verbose = verbose
          )
        }

        x <- sf::st_make_valid(x)
      }
    } else {
      if (!isSpat(x)) {
        origClass <- class(x)
        isSp <- isSpatial(x)
        x <- terra::vect(x)
      }
      if (os > 1e9 && isTRUE(getOption("reproducible.useCache"))) {
        messagePrepInputs("... Caching the fixErrorTerra call on this large object", verbose = verbose)
        x <- Cache(makeVal(x), .functionName = "make.valid")
      } else {
        x <- makeVal(x)
      }
      if (exists("origClass", inherits = FALSE)) {
        x <- revertClass(x, isSpatial = isSp, origFromClass = origClass)
      }
    }
  }
  x
}

makeVal <- function(x) {
  xValids <- terra::is.valid(x)

  if (any(!xValids)) {
    x <- terra::makeValid(x)
  }

  x
  #
  # whValid <- which(xValids)
  # se <- seq(NROW(x))
  # if (length(whValid)) {
  #   whInValid <- se[-whValid]
  # } else {
  #   whInValid <- se
  # }
  #
  # if (any(!xValids)) {
  #   xGood <- terra::makeValid(x[whInValid])
  #   if (length(whValid)) {
  #     r <- rbind(x[whValid, ], xGood[, ])
  #     x <- r[order(c(whValid, whInValid)),]
  #   } else {
  #     x <- xGood
  #   }
  #
  # }
}


#' @export
#' @rdname postProcessTo
maskTo <- function(from, maskTo, # touches = FALSE,
                   overwrite = FALSE,
                   verbose = getOption("reproducible.verbose"), ...) {
  remapOldArgs(...) # converts studyArea, rasterToMatch, filename2, useSAcrs, targetCRS

  if (!is.null(maskTo) && !extntNA(from) && !extntNA(maskTo)) {
    if (!is.naSpatial(maskTo)) {
      omit <- FALSE
      origFromClass <- class(from)
      if (isRaster(maskTo)) {
        maskTo <- terra::rast(maskTo)
      }
      if (isGridded(maskTo) && isVector(from)) {
        omit <- TRUE
      }
      if (!isSpatialAny(maskTo)) {
        if (is.na(maskTo) || isCRSANY(maskTo)) omit <- TRUE
      }

      if (!omit) {
        if (isSF(from) || isSF(projectTo)) {
          .requireNamespace("sf", stopOnFALSE = TRUE)
        }
        if (isSpatial(from)) {
          from <- sf::st_as_sf(from)
        }
        if (isSF(from)) {
          if (!isSF(maskTo)) {
            maskTo <- sf::st_as_sf(maskTo)
          }
        }
        if (isSpat(from) && isVector(from)) {
          if (!isSpat(maskTo)) {
            maskTo <- terra::vect(maskTo)
          }
        }
        if (!isSpat(from) && !isSF(from)) {
          if (isVector(from)) {
            from <- terra::vect(from)
          } else {
            from <- terra::rast(from)
          }
        }

        if (.requireNamespace("sf")) {
          sameCRS <- sf::st_crs(from) == sf::st_crs(maskTo)
        } else {
          sameCRS <- terra::same.crs(from, maskTo)
        }

        if (!sameCRS) {
          if (isGridded(maskTo)) {
            maskTo <- terra::project(maskTo, from, overwrite = overwrite)
          } else {
            if (isSF(maskTo)) {
              maskTo <- sf::st_transform(maskTo, sf::st_crs(from))
            } else {
              if (isSpatial(maskTo)) {
                maskTo <- terra::vect(maskTo)
              }
              maskTo <- terra::project(maskTo, from)
            }
          }
        }
        messagePrepInputs("    masking...", appendLF = FALSE, verbose = verbose)
        st <- Sys.time()

        # There are 2 tries; first is for `maskTo`, second is for `from`, rather than fix both in one step, which may be unnecessary
        maskAttempts <- 0
        env <- environment()

        attempt <- 1
        triedFrom <- NA
        while (attempt <= 2) {
          fromInt <- try(
            {
              if (isVector(maskTo)) {
                if (length(maskTo) > 1) {
                  if (isSF(maskTo)) {
                    maskTo <- sf::st_union(maskTo)
                  } else {
                    maskTo <- terra::aggregate(maskTo)
                  }
                }
              }

              if (isVector(from)) {
                if (isSF(from)) {
                  sf::st_intersection(from, maskTo)
                } else {
                  if (getRversion() == "4.3.0") { # TODO: this is a work around for R crashing; shouldn't b/c this is in a `try`
                    maskTo <- fixErrorsIn(maskTo)
                  }
                  terra::intersect(from, maskTo)
                }
              } else {
                if (isGridded(maskTo)) {
                  if (terra::ext(from) > terra::ext(maskTo)) {
                    from <- terra::crop(from, maskTo)
                  }
                  if (terra::ext(maskTo) > terra::ext(from)) {
                    maskTo <- terra::crop(maskTo, from)
                  }
                  terra::mask(from, maskTo, overwrite = overwrite)
                } else {
                  if (isSF(maskTo) || isSpatial(maskTo)) {
                    maskTo <- terra::vect(maskTo) # alternative is stars, and that is not Suggests
                  }

                  dotArgs <- intersect(...names(), c(writeRasterArgs, maskArgs))
                  if (length(dotArgs)) {
                    dotArgs <- list(...)[dotArgs]
                  }
                  ll <- append(list(from, maskTo, overwrite = overwrite), dotArgs)
                  do.call(terra::mask, ll)

                  # terra::mask(from, maskTo, touches = touches, overwrite = overwrite)
                }
              }
            },
            silent = TRUE
          )
          if (is(fromInt, "try-error")) {
            if (attempt == 1) {
              whichFailed <- grepl("geom 0|Loop 0", fromInt)
              if (isTRUE(whichFailed) && !(triedFrom %in% TRUE)) { # don't try same one again
                from <- fixErrorsIn(from, error = fromInt, fromFnName = "maskTo", verbose = verbose)
                triedFrom <- TRUE
              } else {
                maskTo <- fixErrorsIn(maskTo, error = fromInt, fromFnName = "maskTo", verbose = verbose)
                triedFrom <- FALSE
              }
            } else {
              stop(fromInt)
            }
          } else {
            if (attempt > 1) {
              messagePrepInputs("...fixed!", verbose = verbose, verboseLevel = 1, appendLF = FALSE)
            }
            break
          }
          attempt <- attempt + 1
        }

        from <- fromInt
        messagePrepInputs("...done in ",
          format(difftime(Sys.time(), st), units = "secs", digits = 3),
          verbose = verbose
        )
        from <- revertClass(from, origFromClass = origFromClass)
      }
    }
  }
  from
}

#' @export
#' @rdname postProcessTo
projectTo <- function(from, projectTo, overwrite = FALSE,
                      verbose = getOption("reproducible.verbose"), ...) {
  remapOldArgs(...) # converts studyArea, rasterToMatch, filename2, useSAcrs, targetCRS

  hasMethod <- which(...names() %in% "method")
  method <- if (length(hasMethod)) {
    method <- assessDataTypeOuter(from, ...elt(hasMethod))
  } else {
    NULL
  }

  if (!is.null(projectTo) && !extntNA(from) && !extntNA(projectTo)) {
    origFromClass <- is(from)
    if (!is.naSpatial(projectTo)) {
      if (isRaster(projectTo)) {
        projectTo <- terra::rast(projectTo)
      }

      projectToOrig <- projectTo # keep for below
      sameProj <- try(terra::same.crs(projectTo, from), silent = TRUE)
      if (is(sameProj, "try-error")) {
        .requireNamespace("sf", stopOnFALSE = TRUE)
        sameCRS <- sf::st_crs(from) == sf::st_crs(maskTo)
      }

      isProjectToVecOrCRS <- isCRSANY(projectTo) || (isVector(projectTo))
      sameRes <- if (isVector(from) || isProjectToVecOrCRS) {
        TRUE
      } else {
        all(terra::res(projectTo) == terra::res(from))
      }

      # if (sameProj && sameRes) {
      #   messagePrepInputs("    projection of from is same as projectTo, not projecting",
      #                     verbose = verbose)
      # } else {
      if (isSF(from) || isSF(projectTo)) {
        .requireNamespace("sf", stopOnFALSE = TRUE)
      }
      messagePrepInputs("    projecting...",
        appendLF = FALSE,
        verbose = verbose
      )
      st <- Sys.time()
      if (isProjectToVecOrCRS && (isSF(projectTo) || isSpatial(projectTo))) {
        projectToTmp <- sf::st_as_sfc(sf::st_bbox(from))
        if (isVector(projectTo)) {
          projectTo <- sf::st_crs(projectTo)
        }
        projectToTmp <- sf::st_transform(projectToTmp, projectTo)
        projectTo <- terra::vect(projectToTmp)
      }

      if (isVector(projectTo)) {
        if (isGridded(from)) {
          if (!isSpat(projectTo)) {
            projectTo <- terra::vect(projectTo)
          }

          messagePrepInputs("", verbose = verbose)
          messagePrepInputs("         projectTo is a Vector dataset, which does not define all metadata required. ",
            verbose = verbose
          )
          if (!terra::is.lonlat(from)) {
            # if (sf::st_crs("epsg:4326") != sf::st_crs(from)) {
            newRes <- terra::res(from)
            messagePrepInputs("         Using resolution of ", paste(newRes, collapse = "x"), "m; ",
              verbose = verbose
            )
            projectTo <- terra::rast(projectTo, resolution = newRes)
          } else {
            projectTo <- terra::crs(projectTo)
          }

          messagePrepInputs("         in the projection of `projectTo`, using the origin and extent",
            verbose = verbose
          )
          messagePrepInputs("         from `ext(from)` (in the projection from `projectTo`).",
            verbose = verbose
          )
          messagePrepInputs("         If this is not correct, create a template gridded object and pass that to projectTo...",
            verbose = verbose
          )
          messagePrepInputs("         ",
            appendLF = FALSE,
            verbose = verbose
          )
        } else {
          projectTo <- terra::crs(projectTo)
          # projectTo <- sf::st_crs(projectTo)$wkt
        }
      }

      # Since we only use the crs when projectTo is a Vector, no need to "fixErrorsIn"
      from <- if (isVector(from)) {
        isSpatial <- isSpatial(from)
        if (isSpatial) {
          from <- suppressWarningsSpecific(terra::vect(from), shldBeChar)
        }
        withCallingHandlers({
          attempt <- 1
          while (attempt <= 2) {
            isSF <- isSF(from)
            if (isSF) {
              if (isGridded(projectTo)) {
                projectTo <- sf::st_crs(projectTo)
              }
              from13 <- sf::st_transform(from, projectTo)
            } else {
              from13 <- terra::project(from, projectTo)
            }
            attempt <- attempt + 2
          }
        }, warning = function(w) {
          if (any(grepl(warningCertificateGrep, w$message))) {
            if (!isSF) {
              w$message <- paste(w$message, "\n ... attempting to use `sf` instead")
              warning(w)
              from <<- sf::st_as_sf(from)
              attempt <<- 0
            }
            invokeRestart("muffleWarning")
          }
        })
        from <- from13

        if (isSpatial) from <- as(from, "Spatial")
        from
      } else {
        dotArgs <- intersect(...names(), c(writeRasterArgs, projectArgs))
        if (length(dotArgs)) {
          dotArgs <- list(...)[dotArgs]
        }
        sameGeom <- if (isSpat(from) && isSpat(projectTo) ||
          (isRaster(from) || isRaster(projectTo))) {
          terra::compareGeom(from, projectTo, stopOnError = FALSE)
        } else {
          FALSE
        }
        if (!isTRUE(sameGeom)) {
          ll <- append(list(from, projectTo, overwrite = overwrite), dotArgs)
          do.call(terra::project, ll)
        } else {
          from
        }
      }
      messagePrepInputs("done in ", format(difftime(Sys.time(), st), units = "secs", digits = 3),
        verbose = verbose
      )
    }
    # }
    from <- revertClass(from, origFromClass = origFromClass)
  }
  from
}

#' @param needBuffer Logical. Defaults to `TRUE`, meaning nothing is done out
#'   of the ordinary. If `TRUE`, then a buffer around the cropTo, so that if a reprojection
#'   has to happen on the `cropTo` prior to using it as a crop layer, then a buffer
#'   of 1.5 * res(cropTo) will occur prior, so that no edges are cut off.
#' @export
#' @rdname postProcessTo
cropTo <- function(from, cropTo = NULL, needBuffer = FALSE, overwrite = FALSE,
                   verbose = getOption("reproducible.verbose"), ...) {
  remapOldArgs(...) # converts studyArea, rasterToMatch, filename2, useSAcrs, targetCRS

  if (!is.null(cropTo) && !extntNA(from) && !extntNA(cropTo)) {
    if (isSF(from) || isSF(cropTo)) {
      .requireNamespace("sf", stopOnFALSE = TRUE)
    }
    omit <- FALSE
    origFromClass <- is(from)

    if (isRaster(cropTo)) {
      cropToCRS <- sf::st_crs(cropTo)
      cropTo <- terra::rast(cropTo)
      if (!nzchar(terra::crs(cropTo))) {
        terra::crs(cropTo) <- cropToCRS$input
      }
    }

    if (!isSpatialAny(cropTo)) {
      if (is.na(cropTo) || isCRSANY(cropTo)) omit <- TRUE
    }

    if (!omit) {
      if (isSpatial(cropTo)) {
        cropTo <- terra::vect(cropTo)
      }
      if (isSpatial(from)) {
        from <- terra::vect(from)
      }

      messagePrepInputs("    cropping...",
        appendLF = FALSE,
        verbose = verbose
      )
      st <- Sys.time()

      if (.requireNamespace("sf")) {
        ext <- sf::st_as_sfc(sf::st_bbox(cropTo)) # create extent as an object; keeps crs correctly
        sameCRS <- sf::st_crs(from) == sf::st_crs(ext) # This is sf way of comparing CRS -- raster::compareCRS doesn't work for newer CRS
      } else {
        ext <- terra::ext(cropTo) # create extent as an object; keeps crs correctly
        sameCRS <- terra::same.crs(from, cropTo)
      }

      if (!sameCRS) {
        withCallingHandlers({
          attempt <- 1
          while (attempt <= 2) {
            isSF <- isSF(cropTo)

            if (isVector(cropTo) && !isSpat(cropTo)) {
              cropToInFromCRS <- sf::st_transform(sf::st_as_sf(cropTo), sf::st_crs(from))
              ext <- sf::st_as_sfc(sf::st_bbox(cropToInFromCRS)) # create extent as an object; keeps crs correctly
            } else {
              terraCRSFrom <- terra::crs(from)
              if (packageVersion("terra") <= "1.5.21") { # an older terra issue; may not be precise version
                if (length(slotNames(terraCRSFrom)) > 0) {
                  terraCRSFrom <- terraCRSFrom@projargs
                }
              }
              cropToInFromCRS <- terra::project(cropTo, terraCRSFrom)
              ext <- terra::ext(cropToInFromCRS) # create extent as an object; keeps crs correctly
            }
            attempt <- attempt + 2
          }

        }, warning = function(w) {
          if (any(grepl(warningCertificateGrep, w$message))) {
            if (!isSF) {
              w$message <- paste(w$message, "\n ... attempting to use `sf` instead")
              warning(w)
              cropTo <<- sf::st_as_sf(cropTo)
              attempt <<- 0
            }
            invokeRestart("muffleWarning")
          }
        })

      }
      if (isVector(from) && !isSF(from)) {
        ext <- terra::vect(ext)
      }

      # This is only needed if crop happens before a projection... need to cells beyond edges so projection is accurate
      if (needBuffer) {
        if (isGridded(from) || isGridded(cropTo)) {
          if (isGridded(from)) {
            res <- terra::res(from)
          } else if (isGridded(cropTo)) {
            res <- terra::res(cropTo)
          }
          if (!isSpat(ext)) {
            ext <- terra::vect(ext)
            terra::crs(ext) <- terra::crs(from)
          }
          extTmp <- terra::ext(ext)
          if (isTRUE(suppressWarnings(terra::is.lonlat(ext)))) { # warning is about "crs not defined"
            extTmp2 <- terra::extend(extTmp, 0.1) # hard code 0.1 lat/long, as long as it isn't past the from extent
            extFrom <- terra::ext(from)
            exts <- c(
              xmin = max(terra::xmin(extTmp2), terra::xmin(extFrom)),
              ymin = max(terra::ymin(extTmp2), terra::ymin(extFrom)),
              xmax = min(terra::xmax(extTmp2), terra::xmax(extFrom)),
              ymax = min(terra::ymax(extTmp2), terra::ymax(extFrom))
            )
            ext <- if (packageVersion("terra") <= "1.5-21") {
              terra::ext(exts)
            } else {
              terra::ext(xy = TRUE, exts)
            }
          } else {
            ext <- terra::extend(extTmp, res[1] * 2)
          }
        }
      }

      attempt <- 1
      while (attempt <= 2) {
        if (isGridded(from)) {
          if (!isSpat(from)) { # terra::crop can handle Raster but only if ext is `extent`
            .requireNamespace("raster", stopOnFALSE = TRUE)
            ext <- raster::extent(ext[])
          }
          fromInt <- try(terra::crop(from, ext, overwrite = overwrite), silent = TRUE)
        } else {
          if (isSF(from)) {
            fromInt <- try(sf::st_crop(from, ext), silent = TRUE)
          } else {
            fromInt <- try(terra::crop(from, ext), silent = TRUE)
          }
        }

        wasError <- is(fromInt, "try-error")
        noOverlap <- NROW(fromInt) == 0
        if (noOverlap || wasError) {
          fail <- FALSE
          if (wasError) {
            if (grepl("extents do not overlap", fromInt)) {
              fail <- TRUE
            }
          }
          if (NROW(fromInt) == 0) { # likely don't overlap
            messagePrepInputs("It looks like the cropping results in no data ",
              "(do not overlap or no intersection)",
              verbose = verbose
            )
            fail <- FALSE
          }
          if (fail) {
            stop(fromInt)
          }
        }



        if (is(fromInt, "try-error")) {
          if (attempt == 1) {
            from <- fixErrorsIn(from, error = fromInt, fromFnName = "cropTo", verbose = verbose)
          } else {
            stop(fromInt)
          }
        } else {
          if (attempt > 1) {
            messagePrepInputs("...fixed!",
              verbose = verbose, verboseLevel = 1,
              appendLF = FALSE
            )
          }
          break
        }
        attempt <- attempt + 1
      }
      from <- fromInt
      messagePrepInputs("...done in ", format(difftime(Sys.time(), st), units = "secs", digits = 3),
        verbose = verbose
      )
    }
    from <- revertClass(from, origFromClass = origFromClass)
  }
  from
}

#' @export
#' @rdname postProcessTo
#' @param isStack,isBrick,isRaster,isSpatRaster Logical. Default `NULL`. Used to convert `from`
#'   back to these classes prior to writing, if provided.
#'
writeTo <- function(from, writeTo, overwrite = getOption("reproducible.overwrite"),
                    isStack = NULL, isBrick = NULL, isRaster = NULL,
                    isSpatRaster = NULL,
                    verbose = getOption("reproducible.verbose"), ...) {
  remapOldArgs(...) # converts studyArea, rasterToMatch, filename2, useSAcrs, targetCRS
  if (!missing(writeTo)) {
    if (!is.null(writeTo)) {
      dPath <- which(...names() %in% "destinationPath")
      destinationPath <- if (length(dPath)) {
        destinationPath <- ...elt(dPath)
      } else {
        getOption("reproducible.destinationPath", ".")
      }
      if (all(isAbsolutePath(writeTo))) {
        destinationPath <- dirname(writeTo)
        writeTo <- basename(writeTo)
      }

      writeTo <- determineFilename(writeTo, destinationPath = destinationPath, verbose = verbose)

      hasDatatype <- which(...names() %in% "datatype")
      datatype <- if (length(hasDatatype)) ...elt(hasDatatype) else NULL

      if (isTRUE(isStack)) from <- raster::stack(from)
      if (isTRUE(isBrick)) from <- raster::brick(from)

      writeDone <- FALSE

      if (!any(is.na(writeTo))) {
        .requireNamespace("terra", stopOnFALSE = TRUE)
        messagePrepInputs("    writing...",
          appendLF = FALSE,
          verbose = verbose
        )
        st <- Sys.time()

        if (is.null(isSpatRaster)) isSpatRaster <- isSpat(from) && isGridded(from)
        if (is.null(isRaster)) isRaster <- inherits(from, "Raster")

        if (isSpatRaster || isVector(from)) {
          ## trying to prevent write failure and subsequent overwrite error with terra::writeRaster
          if (any(file.exists(writeTo))) {
            if (isFALSE(overwrite)) {
              stop(writeTo, " already exists and `overwrite = FALSE`; please set `overwrite = TRUE` and run again.")
            }
            unlink(writeTo, force = TRUE, recursive = TRUE)
          }
          if (isSpatRaster) {
            ## if the file still exists it's probably already "loaded"
            ## and `terra` can't overwrite it even if `overwrite = TRUE`
            ## this can happen when multiple modules touch the same object
            if (!any(file.exists(writeTo))) {
              from <- terra::writeRaster(from,
                filename = writeTo, overwrite = FALSE,
                datatype = datatype
              )
              writeDone <- TRUE
            } else {
              stop("File can't be unliked for overwrite")
            }
          } else {
            if (isSF(from)) {
              written <- sf::st_write(from, dsn = writeTo)
            } else {
              written <- terra::writeVector(from, filename = writeTo, overwrite = FALSE)
            }
            writeDone <- TRUE
          }
        } else if (isRaster) {
          nlyrsFrom <- nlayers2(from)
          if (nlyrsFrom == 1 || length(writeTo) == 1) {
            from <- terra::writeRaster(from,
              filename = writeTo, overwrite = overwrite,
              datatype = datatype
            )
          } else {
            outs <- lapply(seq(nlyrsFrom), function(ind) {
              out <- terra::writeRaster(from[[ind]],
                filename = writeTo[ind], overwrite = overwrite,
                datatype = datatype
              )
            })
            from <- raster::stack(outs)
          }
          writeDone <- TRUE
        } else {
          fe <- .fileExtsKnown()
          whType <- fe$extension %in% tools::file_ext(writeTo)
          if (any(whType)) {
            eval(parse(text = fe$saveFun[whType]))(from, writeTo)
          } else {
            messagePrepInputs("... nothing written; object not a known object type to write.",
              verbose = verbose
            )
          }
        }
        if (isTRUE(writeDone)) {
          messagePrepInputs("...done in ", format(difftime(Sys.time(), st), units = "secs", digits = 3),
            verbose = verbose
          )
        } else {
          messagePrepInputs("", verbose = verbose) # need to "end" the line
        }
      }
    }
  }

  from
}

saveTo <- function(x, file) {
  fe <- .fileExtsKnown()
  whType <- fe$extension %in% tools::file_ext(file)
  if (any(whType)) {
    eval(parse(text = fe$saveFun[whType]))(x, file)
  }
}

readFrom <- function(file) {
  fe <- .fileExtsKnown()
  whType <- fe$extension %in% tools::file_ext(file)
  if (any(whType)) {
    eval(parse(text = fe$fun[whType]))(file)
  }
}

postProcessToAssertions <- function(from, to, cropTo, maskTo, projectTo,
                                    verbose = getOption("reproducible.verbose")) {
  # sometimes there are quosures
  for (y in ls()) {
    ll <- get(y, inherits = FALSE)
    if (inherits(ll, "quosure")) {
      .requireNamespace("rlang", stopOnFALSE = TRUE)
      assign(y, rlang::eval_tidy(ll))
    }
  }

  if (isSpat(from) || isSpat(to)) {
    if (!requireNamespace("terra", quietly = TRUE)) {
      stop("Need terra and sf: install.packages(c('terra', 'sf'))")
    }
  }
  if (isSF(from) || isSF(to)) {
    if (!requireNamespace("sf", quietly = TRUE)) stop("Need sf: install.packages('sf')")
  }

  if (!(isSpatialAny(from))) stop("from must be a Spat* or sf*")

  if (!missing(to)) {
    if (!is.null(to)) {
      if (!isSpatialAny(to) && !isCRSANY(to)) stop("to must be a ", .msg$anySpatialClass)
      # if (isVector(from))
      #   if (!isVector(to) && !isCRSANY(to)) {
      #     # as long as maskTo and projectTo are supplied, then it is OK
      #     if (!isVector(maskTo) && !isVector(projectTo))
      #       stop("if from is a Vector object, to must also be a Vector or crs object")
      #   }
    }
  }

  if (!missing(cropTo)) {
    if (!is.naSpatial(cropTo)) {
      if (!is.null(cropTo)) {
        if (!isSpatialAny(cropTo) && !isCRSANY(cropTo)) {
          stop("cropTo must be a ", .msg$anySpatialClass)
        }
        # apparently, cropTo can be a gridded object no matter what
        # if (isVector(from)) if (!isVector(cropTo) && !isCRSANY(cropTo))
        #   stop("if from is a Vector object, cropTo must also be a Vector object")
      }
    }
  }
  if (!missing(maskTo)) {
    if (!is.naSpatial(maskTo)) {
      if (!is.null(maskTo)) {
        if (!isSpatialAny(maskTo) && !isCRSANY(maskTo)) {
          stop("maskTo must be a ", .msg$anySpatialClass)
        }
        # if (isVector(from)) if (!isVector(maskTo) && !isCRSANY(maskTo))
        #   stop("if from is a Vector object, maskTo must also be a Vector object")
      }
    }
  }
  if (!missing(projectTo)) {
    if (!is.naSpatial(projectTo)) {
      if (!is.null(projectTo)) {
        if (isCRScharacter(projectTo)) {
          projectTo <- try(silent = TRUE, sf::st_crs(projectTo))
        }
        if (!isCRSANY(projectTo)) {
          if (!isSpatialAny(projectTo)) {
            stop("projectTo must be a ", .msg$anySpatialClass)
          }
          # if (isVector(from)) if (!isVector(projectTo))
          # stop("if from is a Vector object, projectTo must also be a Vector object")
        }
      }
    }
  }

  return(invisible())
}

is.naSpatial <- function(x) {
  isna <- FALSE
  if (!is.null(x)) {
    if (!isSpatialAny(x)) {
      if (all(is.na(x))) isna <- TRUE
    }
  }
  isna
}

cropSF <- function(from, cropToVect, verbose = getOption("reproducible.verbose")) {
  st <- Sys.time()
  if (isSF(from) && (isSF(cropToVect) || is(cropToVect, "Spatial"))) {
    .requireNamespace("sf", stopOnFALSE = TRUE)
    messagePrepInputs("    pre-cropping because `from` is sf and cropTo is sf/Spatial*",
      verbose = verbose
    )
    attempt <- 1
    while (attempt <= 2) {
      from2 <- try(
        sf::st_crop(from, sf::st_transform(
          sf::st_as_sfc(sf::st_bbox(cropToVect)),
          sf::st_crs(from)
        )),
        silent = TRUE
      )
      if (is(from2, "try-error")) {
        if (attempt == 1) {
          messageDeclareError(error = from2, fromFnName = "cropSF", verbose)
          from <- fixErrors(from, useCache = FALSE, verbose = verbose - 1)
        } else {
          stop(from2)
        }
      } else {
        if (attempt > 1) {
          messagePrepInputs("...fixed!",
            verbose = verbose, verboseLevel = 1,
            appendLF = FALSE
          )
        }
        break
      }

      attempt <- attempt + 1
    }
    if (extntNA(from2)) {
      messagePrepInputs("    resulting extent is NA, probably because objects don't overlap",
        verbose = verbose
      )
    }
    if (!is(from2, "try-error")) {
      from <- from2
    }

    messagePrepInputs("  done in ", format(difftime(Sys.time(), st),
      units = "secs", digits = 3
    ),
    verbose = verbose
    )
  }
  from
}

shldBeChar <- "should be a character value"

revertClass <- function(from, isStack = FALSE, isBrick = FALSE, isRasterLayer = FALSE,
                        isSF = FALSE, isSpatial = FALSE, origFromClass = NULL) {
  # if (!isSpat2(origFromClass)) {
  if (!is.null(origFromClass)) {
    # overrides all others!
    isStack <- any(origFromClass == "RasterStack")
    isBrick <- any(origFromClass == "RasterBrick")
    isRasterLayer <- any(origFromClass == "RasterLayer")
    isSF <- any(origFromClass == "sf")
    isSpatial <- any(startsWith(origFromClass, "Spatial"))
    isSV <- any(origFromClass == "SpatVector")

    if (isSV && !is(from, "SpatVector")) {
      from <- terra::vect(from)
    } else if (isStack && !is(from, "RasterStack")) {
      from <- raster::stack(from) # coming out of writeRaster, becomes brick
    } else if (isBrick && !is(from, "RasterBrick")) {
      from <- raster::brick(from) # coming out of writeRaster, becomes brick
    } else if (isRasterLayer && !is(from, "RasterLayer")) {
      from <- raster::raster(from) # coming out of writeRaster, becomes brick
    } else if (isSF || isSpatial) {
      .requireNamespace("sf", stopOnFALSE = TRUE)
      from <- sf::st_as_sf(from)
      if (isSpatial) {
        from <- sf::as_Spatial(from)
      }

    }
  }
  from
}

messageDeclareError <- function(error, fromFnName, verbose = getOption("reproducible.verbose")) {
  errWOWordError <- gsub("Error {0,1}: ", "", error)
  messagePrepInputs("    ", fromFnName, " resulted in following error: \n    - ", errWOWordError, "    --> attempting to fix",
    appendLF = FALSE, verbose = verbose, verboseLevel = 1
  )
}

#' @importFrom stats na.omit
remapOldArgs <- function(..., fn = sys.function(sys.parent()), envir = parent.frame(),
                         verbose = getOption("reproducible.verbose")) {
  forms <- formals(fn)
  dots <- list(...)
  dots <- dots[!vapply(dots, is.null, FUN.VALUE = logical(1))]

  ret <- list()

  # First, what is supplied has to be one of the .remappings
  oldUsed <- intersect(names(dots), names(.remappings))
  oldUsed <- oldUsed[na.omit(match(names(.remappings), oldUsed))] # order provided in .remappings; not user

  # Second, what is supplied
  remap <- .remappings[oldUsed]

  newHereAll <- intersect(
    unname(unlist(remap)),
    names(forms)
  )

  if (length(newHereAll)) {
    # remove iterative duplication e.g., cropTo should only come from rtm if both rtm and sa supplied
    newHere <- character()
    Map(re = rev(remap), nam = names(rev(remap)), function(re, nam) {
      newOnes <- setdiff(re, newHere)
      newHere <<- c(newHere, newOnes)
      remap[[nam]] <<- newOnes
    })

    Map(
      elem = oldUsed, newHere = remap,
      function(elem, newHere) {
        if (length(elem)) {
          mes <- paste(newHere, collapse = ", ")
          messagePrepInputs(elem, " is supplied (deprecated); assigning it to ", mes,
            verbose = verbose
          )
          lapply(newHere, function(nh) ret[nh] <<- list(dots[[elem]]))
        }
      }
    )
    if (isTRUE(dots$useSAcrs)) {
      ret$projectTo <- dots$studyArea
    }

    if (isFALSE(dots$useSAcrs)) {
      ret$projectTo <- NULL
    }

    if (isTRUE(dots$maskWithRTM)) {
      ret$maskTo <- dots$rasterToMatch
    }

    # ret <- ret[!vapply(ret, is.null, FUN.VALUE = logical(1))]
    list2env(ret, envir)
  }
  invisible(ret)
}


.remappings <- list(
  studyArea = c("cropTo", "maskTo"),
  rasterToMatch = c("cropTo", "projectTo", "to"),
  filename2 = "writeTo",
  targetCRS = "projectTo",
  useSAcrs = "projectTo"
)


assessDataTypeOuter <- function(from, method) {
  if (isGridded(from)) {
    if (is.null(method)) {
      method <- assessDataType(from, type = "projectRaster")
    }
    whMethodBilin <- method == "bilinear"

    isInt <- if (inherits(from, "Raster")) {
      if (nlayers2(from) == 1) is.integer(from[]) else apply(from[], 2, is.integer)
    } else {
      terra::is.int(from)
    }

    if (isTRUE(any(whMethodBilin & isInt))) {
      warning("method is bilinear, but the data are integer; please confirm this is correct")
    }
    whMethodNGB <- method == "ngb"
    if (any(whMethodNGB)) {
      method[whMethodNGB] <- "near"
    }
  }
  method
}


writeRasterArgs <- c(
  "filename", "overwrite", "ncopies", "steps", "filetype", "progressbar", "tempdir",
  "todisk", "memfrac", "progress", "verbose", "memmin", "filetype",
  "verbose", "names", "tolerance", "overwrite", "datatype", "memmax"
)

projectArgs <- c("x", "y", "method", "mask", "align", "gdal", "res", "origin", "threads", "filename")

maskArgs <- c("x", "inverse", "mask", "updatevalue", "touches", "filename")

extntNA <- function(x) {
  out <- if (isSF(x)) {
    sf::st_bbox(x)
  } else {
    if (isSpat(x) || isRaster(x)) {
      terra::ext(x)
    } else {
      FALSE
    }
  }
  out <- anyNA(as.numeric(out[]))
  return(out)
}

warningCertificateGrep <- "CertGetCertificateChain trust error CERT_TRUST_IS_PARTIAL_CHAIN"
