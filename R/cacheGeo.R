utils::globalVariables(c("X", "Y"))

checkNameHasGeom <- function(existingObj) {
  hasGeomNamedCol <- names(existingObj) %in% "geom"
  if (any(hasGeomNamedCol)) {
    if (is(existingObj, "sf")) {
      # renaming the column alone leaves attr "sf_column" pointing at "geom"
      sf::st_geometry(existingObj) <- "geometry"
    } else {
      names(existingObj)[hasGeomNamedCol] <- "geometry"
    }
  }
  existingObj
}

## Planar geometry (GEOS), not s2: s2 refuses polygons with a repeated vertex when they are joined.
## sf's notes that coordinates are treated as planar are not repeated at every call.
.geoPlanar <- function(expr) {
  s2 <- suppressMessages(sf::sf_use_s2(FALSE))
  on.exit(suppressMessages(sf::sf_use_s2(s2)))
  planarNote <- "assumes\\s+that\\s+they\\s+are\\s+planar|assumed to be in decimal degrees|spatially constant"
  withCallingHandlers(expr,
    warning = function(w) if (grepl(planarNote, conditionMessage(w))) invokeRestart("muffleWarning"),
    message = function(m) if (grepl(planarNote, conditionMessage(m))) invokeRestart("muffleMessage"))
}

extractPolygonIfWithin <- function(domain, existingObjSF, bufferOK, existingObj, verbose = TRUE,
                                   tolerance = 0) {
  # Coverage-based check using st_intersects + st_difference: unlike the original
  # st_within approach (single feature only), the `domain` may be covered by the
  # *union* of more than one existing feature ("multiple overlap, not within").
  # When `bufferOK`, an incremental buffer is tried if the raw union does not
  # quite cover the domain.
  wh <- sf::st_intersects(domain, existingObjSF, sparse = FALSE)
  wh1 <- NULL # default for all cases below: domain is not (yet) covered
  if (any(wh)) {
    existingObjSF <- existingObjSF[apply(wh, 2, any), ]
    existingObj <- existingObj[apply(wh, 2, any), ]

    # Is the domain fully covered by the union of the intersecting features?
    # (Nothing left after differencing the domain against their union.)
    eosf <- .geoUnionFilled(existingObjSF)
    # Gaps narrower than `tolerance` do not count
    gap <- .geoPlanar(suppressWarnings(
      sf::st_difference(sf::st_geometry(domain), sf::st_geometry(eosf))))
    if (!NROW(gap) || all(.geoShrunkEmpty(sf::st_geometry(gap), tolerance))) {
      wh1 <- as.vector(wh)
    }
  }

  # Not (yet) covered: optionally retry with an incremental buffer. This runs
  # even when nothing intersected, since a buffer may extend coverage to reach
  # the domain.
  if (is.null(wh1) && isTRUE(bufferOK)) {
    diffs <- mapply(minmax = list(c("xmin", "xmax"), c("ymin", "ymax")), function(minmax)
      round(abs(diff(sf::st_bbox(existingObjSF)[minmax])), 0))
    meanBuffKm <- round(mean(diffs) * 0.025 / 1e3, 1)
    message("domain is not within existing object; trying a ", meanBuffKm, " km buffer")
    bufferRes <- bufferIncremental(existingObjSF, domain)
    if (!NROW(bufferRes$dif)) { # buffered union now covers the domain
      existingObjSF <- bufferRes$existingObjSF
      wh1 <- TRUE
    }
  }

  domainExisted <- !is.null(wh1)
  if (isTRUE(domainExisted) && isTRUE(verbose)) {
    if (isTRUE(bufferOK)) {
      message("domain is within the buffered object; returning the existing parameters")
    } else {
      message(.message$cacheGeoDomainContained)
    }
  }
  if (all(wh1 %in% FALSE)) { # wh1 is NULL (not covered) -> all(logical(0)) is TRUE
    existingObj <- NULL
    existingObjSF <- NULL
  }

  list(existingObj = existingObj, existingObjSF = existingObjSF,
       domainExisted = domainExisted)
}


  #  from https://github.com/zhukovyuri/SUNGEO/blob/master/R/update_bbox.R
  update_bbox <- function(sfobj){
    # Manually calculate bounds from coordinates
    new_bb <- data.table::as.data.table(sf::st_coordinates(sfobj))[,c(min(X),min(Y),max(X),max(Y))]
  # Rename columns
  names(new_bb) <- c("xmin", "ymin", "xmax", "ymax")
  # Change object class
  attr(new_bb, "class") <- "bbox"
  # Assign to bbox slot of sfobj
  attr(sf::st_geometry(sfobj), "bbox") <- new_bb

  return(sfobj)
}

## The union of the features, with any holes filled, as a one-row sf. Done with sf alone: a round
## trip through terra moves the coordinates a little, which leaves slivers when a polygon is
## differenced from its own copy.
.geoUnionFilled <- function(x) {
  .geoPlanar({
    polys <- sf::st_cast(sf::st_union(sf::st_geometry(x)), "POLYGON")
    filled <- lapply(polys, function(p) sf::st_polygon(p[1]))
    sf::st_sf(geometry = sf::st_union(sf::st_sfc(filled, crs = sf::st_crs(x))))
  })
}

bufferIncremental <- function(existingObjSF, domain) {
  for (i in 1:2) {
    eosf <- .geoUnionFilled(existingObjSF)
    dif <- sf::st_difference(domain, eosf)
    if (NROW(dif)) {
      existingObjSF <- sf::st_buffer(existingObjSF, dist = 10000)
    } else {
      break
    }
  }
  list(existingObjSF = existingObjSF, dif = dif)
}
