#' Cache-like function for spatial domains
#'
#' \if{html}{\figure{lifecycle-experimental.svg}{options: alt="experimental"}}
#'
#' This function is a combination of `Cache` and `prepInputs` but for spatial
#' domains. This differs from `Cache` in that the current function call doesn't
#' have to have an identical function call previously run. Instead, it needs
#' to have had a previous function call where the `domain` being passes is
#' *within* the geographic limits of the `targetFile`.
#' This is similar to a geospatial operation on a remote GIS server, with 2 differences:
#' 1. This downloads the object first before doing the GIS locally, and 2. it will
#' optionally upload an updated object if the geographic area did not yet exist.
#'
#' @details
#' This has a very specific use case: assess whether an existing `sf` polygon
#' or multipolygon object (local or remote) covers the spatial
#' area of a `domain` of interest. If it does, then return only that
#' part of the `sf` object that completely covers the `domain`.
#' If it does not, then run `FUN`. It is expected that `FUN` will produce an `sf`
#' polygon or multipolygon class object. The result of `FUN` will then be
#' appended to the `sf` object as a new entry (feature) or it will replace
#' the existing "same extent" entry in the `sf` object.
#'
#'
#' @param targetFile The (optional) local file (or path to file) name for a `sf`
#'   object or `data.frame` that can be coerced to a `sf` object (i.e., has a `geometry`
#'   column). If `cloudFolderID` is specified, then this will be the name of the
#'   file stored and/or accessed in that cloud folder.
#' @param domain An sf polygon object that is the spatial area of interest. If `NULL`,
#'   then this will return the whole object in `targetFile`.
#' @param FUN A function call that will be called if there is the `domain` is
#'   not already contained within the `sf` object at `targetFile`. This function
#'   call MUST return either a `sf` class object or a `data.frame` class object
#'   that has a geometry column (which can then be converted to `sf` with `sf::st_as_sf`)
#' @param cloudFolderID If this is specified, then it must be either 1) a Google Drive
#'   url to a folder where the `targetFile` will be read from or written to, or
#'   2) a `googledrive` id or 3) an absolute path to a (possibly non-existent yet)
#'   folder on your Google drive.
#' @param useCloud A logical.
#' @param bufferOK A logical. If `TRUE`, then after testing whether the domain is
#'   within the `targetFile` spatial object, and if it returns `FALSE`, then the function
#'   will create a larger object, buffered by 2.5% of the extent of the object. If
#'   `FALSE`, then it will be strict about whether the `domain` is within the `targetFile`.
#' @param ... All named objects that are needed for FUN, including the function itself,
#'   if it is not in a package.
#'
#' @inheritParams prepInputs
#' @inheritParams Cache
#' @param action A character string, with one of c("nothing", "update",
#'   "replace", "append"). Partial matching is used ("n" is sufficient).
#'   `nothing` will prevent any updating of the `targetFile`,
#'   i.e., "read only". `append` will add the spatial elements in domain to
#'   `targetFile` (and writing it back to disk). `update` will do the same as
#'   `append`, but will also remove any identical geometries before appending.
#'   `replace` does nothing currently.
#'
#' @return Returns an object that results from `FUN`, which will possibly be a subset
#' of a larger spatial object that is specified with `targetFile`.
#'
#' @export
#' @examples
#' \donttest{
#'
#' if (requireNamespace("sf", quietly = TRUE) &&
#'     requireNamespace("terra", quietly = TRUE)) {
#'   dPath <- checkPath(file.path(tempdir2()), create = TRUE)
#'   localFileLux <- system.file("ex/lux.shp", package = "terra")
#'
#'   # 1 step for each layer
#'   # 1st step -- get study area
#'   full <- prepInputs(localFileLux, destinationPath = dPath) # default is sf::st_read
#'   zoneA <- full[3:6, ]
#'   zoneB <- full[8, ] # not in A
#'   zoneC <- full[3, ] # yes in A
#'   zoneD <- full[7:8, ] # not in A, B or C
#'   zoneE <- full[3:5, ] # yes in A
#'   # 2nd step: re-write to disk as read/write is lossy; want all "from disk" for this ex.
#'   writeTo(zoneA, writeTo = "zoneA.shp", destinationPath = dPath)
#'   writeTo(zoneB, writeTo = "zoneB.shp", destinationPath = dPath)
#'   writeTo(zoneC, writeTo = "zoneC.shp", destinationPath = dPath)
#'   writeTo(zoneD, writeTo = "zoneD.shp", destinationPath = dPath)
#'   writeTo(zoneE, writeTo = "zoneE.shp", destinationPath = dPath)
#'   # Must re-read to get identical columns
#'   zoneA <- sf::st_read(file.path(dPath, "zoneA.shp"))
#'   zoneB <- sf::st_read(file.path(dPath, "zoneB.shp"))
#'   zoneC <- sf::st_read(file.path(dPath, "zoneC.shp"))
#'   zoneD <- sf::st_read(file.path(dPath, "zoneD.shp"))
#'   zoneE <- sf::st_read(file.path(dPath, "zoneE.shp"))
#'
#'   # The function that is to be run. This example returns a data.frame because
#'   #    saving `sf` class objects with list-like columns does not work with
#'   #    many st_driver()
#'   fun <- function(domain, newField) {
#'     domain |>
#'       as.data.frame() |>
#'       cbind(params = I(lapply(seq_len(NROW(domain)), function(x) newField)))
#'   }
#'
#'   # Run sequence -- A, B will add new entries in targetFile, C will not,
#'   #                 D will, E will not
#'   for (z in list(zoneA, zoneB, zoneC, zoneD, zoneE)) {
#'     out <- CacheGeo(
#'       targetFile = "fireSenseParams.rds",
#'       domain = z,
#'       FUN = fun(domain, newField = I(list(list(a = 1, b = 1:2, c = "D")))),
#'       fun = fun, # pass whatever is needed into the function
#'       destinationPath = dPath,
#'       action = "update"
#'       # , cloudFolderID = "cachedObjects" # to upload/download from cloud
#'     )
#'   }
#' }
#' }
CacheGeo <- function(targetFile = NULL,
                     domain,
                     FUN,
                     destinationPath = getOption("reproducible.destinationPath", "."),
                     useCloud = getOption("reproducible.useCloud", FALSE),
                     cloudFolderID = NULL,
                     purge = FALSE, useCache = getOption("reproducible.useCache"),
                     overwrite = getOption("reproducible.overwrite"),
                     action = c("nothing", "update", "replace", "append"),
                     bufferOK = FALSE,
                     ...) {
  objExisted <- TRUE
  if (is.null(targetFile)) {
    objExisted <- FALSE
  }

  if (!missing(targetFile)) {
    if (!isAbsolutePath(targetFile)) {
      targetFileWithDP <- file.path(destinationPath, targetFile)
    }
  } else {
    stop("Either targetFile must be supplied")
  }
  if (!file.exists(targetFileWithDP)) {
    objExisted <- FALSE
  }
  domainExisted <- objExisted # !missing(domain) # objExisted
  urlThisTargetFile <- NULL

  alreadyOnRemote <- FALSE
  cacheExtra <- NULL

  if (isTRUE(useCloud) || !is.null(cloudFolderID)) {
    .requireNamespace("googledrive", stopOnFALSE = TRUE)
    objsInGD <- googledrive::drive_ls(cloudFolderID)
    if (!(nchar(cloudFolderID) == 33 || grepl("https://drive", cloudFolderID))) {
      googledrive::with_drive_quiet(folderExists <- googledrive::drive_get(cloudFolderID))
      if (NROW(folderExists)) {
        cloudFolderID <- googledrive::as_id(folderExists$id)
      }
    } else {
      cloudFolderID <- googledrive::as_id(cloudFolderID)
      folderExists <- googledrive::drive_get(cloudFolderID)
    }
    folderExists <- NROW(folderExists) > 0

    if (!folderExists) {
      cloudFolderID <- googledrive::drive_mkdir(cloudFolderID)
    }
    # objsInGD <- googledrive::drive_ls(cloudFolderID)
    objID <- objsInGD[objsInGD$name %in% basename2(targetFile), ]

    if (NROW(objID)) urlThisTargetFile <- objID$drive_resource[[1]]$webViewLink

    objExisted <- if (NROW(objID)) TRUE else FALSE
    if (is.null(targetFile)) {
      targetFile <- objID$name
      targetFileWithDP <- file.path(destinationPath, objID$name)
    }
    if (file.exists(targetFileWithDP) && NROW(objID)) {
      md5Checksum <- digest::digest(file = targetFileWithDP)
      alreadyOnRemote <- identical(objID$drive_resource[[1]]$md5Checksum, md5Checksum)
      cacheExtra <- objID$drive_resource[[1]]$md5Checksum
    }
  }

  if (isTRUE(objExisted)) {

    aa <- quote(prepInputs(
      targetFile = asPath(targetFile),
      url = urlThisTargetFile,
      destinationPath = destinationPath, # domain = domain,
      useCache = useCache,
      purge = purge, # It isn't relevant if the file is different than the Checksums
      overwrite = overwrite
    ))
    existingObj <- eval(aa) |>
      Cache(.cacheExtra = cacheExtra, .functionName = paste0("prepInputs_", basename(targetFile))) # cacheExtra is the md5Checksum on GDrive

    # existingObj <- prepInputs(
    #   targetFile = asPath(targetFile),
    #   url = urlThisTargetFile,
    #   destinationPath = destinationPath, # domain = domain,
    #   useCache = useCache,
    #   purge = purge, # It isn't relevant if the file is different than the Checksums
    #   overwrite = overwrite
    # ) |> Cache(.cacheExtra = cacheExtra) # cacheExtra is the md5Checksum on GDrive

    existingObjOrig <- existingObj

    cn <- colnames(existingObj)
    # Assumption that data.frame should be data.table
    if (is(existingObj, "list")) {
      existingObj <- lapply(existingObj, function(x) if (is.data.frame(x[[1]])) I(list(as.data.table(x[[1]]))) else x) |>
        as.data.frame()
    } else {
      if (!is(existingObj, "data.frame"))
        existingObj <- as.data.frame(existingObj)
    }
    colnames(existingObj) <- cn

    # existingObjSF <- if (is(df, "sf")) df else sf::st_as_sf(df)

    existingObjSF <- if (is(existingObj, "sf")) existingObj else sf::st_as_sf(existingObj)
    existingObjSF <- update_bbox(existingObjSF)
    existingObjSFOrig <- existingObjSF
    if (!missing(domain)) {
      #must be sf for st_within
      if (!inherits(domain, "sf")) domain <- sf::st_as_sf(domain)
      #must have same crs for st_within
      existingObjSF <- sf::st_transform(existingObjSF, sf::st_crs(domain))

      outs <- extractPolygonIfWithin(domain, existingObjSF, bufferOK, existingObj)
      list2env(outs, envir = environment()) # existingObjSF, existingObj, domainExisted

      # wh <- sf::st_within(domain, existingObjSF, sparse = FALSE)
      # if (isTRUE(wh %in% FALSE) && isTRUE(bufferOK)) {
      #   diffs <- mapply(minmax = list(c("xmin", "xmax"), c("ymin", "ymax")), function(minmax)
      #     round(abs(diff(sf::st_bbox(existingObjSF)[minmax])), 0))
      #   buff <- diffs * 0.025
      #   meanBuff <- mean(buff)
      #   meanBuffKm <- round(meanBuff/1e3, 1)
      #   message("domain is not within existing object; trying a 2.5% (", meanBuffKm, "km) buffer")
      #   existingObjSF_wider <- sf::st_buffer(existingObjSF, dist = meanBuff)
      #   wh <- sf::st_within(domain, existingObjSF_wider, sparse = FALSE)
      # }
      # wh1 <- apply(wh, 1, any)
      # wh2 <- apply(wh, 2, any) # This is "are the several domains inside the several existingObjSF"
      # # length of existingObjSF
      # domainExisted <- all(wh1)
      # if (domainExisted) {
      #
      #   # THIS IS "PULL OUT INDIVIDUAL SF POLYGON FROM THE MANY"
      #   if (isTRUE(bufferOK)) # the message will be previously given
      #     message("domain is within the buffered object; returning the existing parameters")
      #   else
      #     message(.message$cacheGeoDomainContained)
      #   existingObj <- existingObj[wh2, ]
      # }
      # if (all(wh1 %in% FALSE)) {
      #   existingObj <- NULL
      # }
    } else {
      domainExisted <- TRUE
      message("Spatial domain is missing; returning entire spatial domain")
    }
  }
  msgActionIsNothing <- "action was 'nothing'; nothing done"
  if ( (isFALSE(objExisted) || isFALSE(domainExisted) ) && !missing(FUN)) {
    message(.message$cacheGeoDomainNotContained)
    FUNcaptured <- substitute(FUN)
    env <- environment()
    list2env(list(...), envir = env) # need the ... to be "here"
    newObj <- try(eval(FUNcaptured, envir = env), silent = TRUE)
    newObjSF <- if (is(newObj, "sf")) newObj else sf::st_as_sf(newObj)
    if (isFALSE(objExisted)) {
      existingObj <- newObj
    }
  } else {
    if (isFALSE(objExisted)) {
      message("targetFile (", targetFile,") does not exist")
      existingObj <- NULL
    }
    if (isFALSE(domainExisted))
      message("domain does not exist in targetFile (", targetFile, ")")
    if (missing(FUN) && isFALSE(domainExisted)) {
      if (!action %in% "nothing")
        message("FUN is missing; no evaluation possible")
      if (action %in% "nothing")
        message(msgActionIsNothing)
    }



  }
  existingObjSF <- if (!is(existingObj, "sf"))
    sf::st_as_sf(existingObj) else existingObj
  if (isFALSE(domainExisted)) {
    if (isTRUE(objExisted)) {
      if (any(grepl("^a|^u", action[1], ignore.case = TRUE))) {
        # .gpkg seems to change geometry to "geom"
        existingObjSF <- checkNameHasGeom(existingObjSF)
        if (!any(is(sf::st_geometry(newObjSF), "sfc_MULTIPOLYGON"))) {
          newObjSF <- sf::st_cast(newObjSF, "MULTIPOLYGON")
          # newObj <- sf::st_cast(newObj, "MULTIPOLYGON")
        }

        # THE APPEND LINE
        existingObj <- as.data.frame(rbindlist(
          list(as.data.table(existingObjOrig), as.data.table(newObj)), fill = TRUE, use.names = TRUE))
        existingObjSF <- sf::st_as_sf(existingObj)

        outs <- extractPolygonIfWithin(domain, existingObjSF, bufferOK, existingObj)
        list2env(outs) # existingObjSF, existingObj, domainExisted

        # existingObj <- rbind(existingObj, newObj)
        if (any(duplicated(existingObj))) {
          existingObj <- unique(existingObj)
        }
      }
    }
    if (!any(grepl("^n", action[1], ignore.case = TRUE))) {
      if (!isAbsolutePath(targetFileWithDP)) {
        targetFileWithDP <- file.path(destinationPath, targetFile)
      }
      # if (is(existingObj, "sf")) existingObj <- as.data.frame(existingObj)

      # Put it in order
      if (!is.null(existingObj[["polygonID"]])) {
        polygonIDnum <- as.numeric(gsub("(\\..)\\.", "\\1", existingObj$polygonID))
        ord <- order(polygonIDnum)
        existingObj <- existingObj[ord,]
      }
      ## end of putting it in order

      if (identical("rds", fs::path_ext(targetFileWithDP)))  {
        saveRDS(existingObj, file = targetFileWithDP)
      } else {
        writeTo(existingObj, writeTo = targetFileWithDP, overwrite = TRUE, append = TRUE)
      }
    } else {
      if (!missing(FUN)) {
        message("The spatial domain is new, and should be added, but\n")
        message(msgActionIsNothing)
      }
    }
  }
  # Cloud
  if (isTRUE(useCloud) || !is.null(cloudFolderID)) {
    if (!isAbsolutePath(targetFile)) targetFileWithDP <- file.path(destinationPath, targetFile)
    if (NROW(objID)) {
      md5Checksum <- digest::digest(file = targetFileWithDP)
      alreadyOnRemote <- identical(objID$drive_resource[[1]]$md5Checksum, md5Checksum)
    }
    if (!alreadyOnRemote) {
      if (!any(grepl("^n", action[1], ignore.case = TRUE))) {
        out <- googledrive::drive_put(
          media = targetFileWithDP,
          path = googledrive::as_id(cloudFolderID)
        )
        attr(existingObj, "id") <- out
      } else {
        msgSkippingUpload <- "skipping googledrive upload;"
        message(msgSkippingUpload)
        message(msgActionIsNothing)
      }

    }
  }
  # if (is(existingObj, "data.frame")) {
  #   existingObj <- sf::st_as_sf(existingObj)
  #   existingObjSF <- existingObj
  # }

  if (exists("existingObjOrig", inherits = FALSE)) {
    messageColoured("To get the full object from googledrive, which looks like this:\n")
    useOrig <- (NROW(existingObj) < NROW(existingObjOrig))
    objLooksLike <- if (useOrig) existingObjOrig else existingObj
    cat(cli::col_yellow(capture.output(objLooksLike)), sep = "\n")
    messageColoured("... run the following:\n")
    # aa <- quote(prepInputs(
    #   targetFile = asPath(targetFile),
    #   url = urlThisTargetFile,
    #   destinationPath = destinationPath, # domain = domain,
    #   useCache = useCache,
    #   purge = purge, # It isn't relevant if the file is different than the Checksums
    #   overwrite = overwrite
    # ))
    enn <- environment()
    ll <- lapply(aa, function(x) eval(x, envir = enn))
    coo <- capture.output(as.call(append(list(quote(prepInputs)), ll[-1])))#, sep = "\n")
    cat(cli::col_yellow(coo), sep = "\n")
  }

    existingObjSF <- sf::st_as_sf(existingObj)
  existingObjSF
}

checkNameHasGeom <- function(existingObj) {
  hasGeomNamedCol <- names(existingObj) %in% "geom"
  if (any(hasGeomNamedCol)) {
    names(existingObj)[hasGeomNamedCol] <- "geometry"
  }
  existingObj
}




extractPolygonIfWithin <- function(domain, existingObjSF, bufferOK, existingObj, verbose = TRUE) {
  wh <- sf::st_within(domain, existingObjSF, sparse = FALSE)
  if (isTRUE(wh %in% FALSE) && isTRUE(bufferOK)) {
    diffs <- mapply(minmax = list(c("xmin", "xmax"), c("ymin", "ymax")), function(minmax)
      round(abs(diff(sf::st_bbox(existingObjSF)[minmax])), 0))
    buff <- diffs * 0.025
    meanBuff <- mean(buff)
    meanBuffKm <- round(meanBuff/1e3, 1)
    message("domain is not within existing object; trying a 2.5% (", meanBuffKm, "km) buffer")
    existingObjSF_wider <- sf::st_buffer(existingObjSF, dist = meanBuff)
    wh <- sf::st_within(domain, existingObjSF_wider, sparse = FALSE)
  }
  wh1 <- apply(wh, 1, any)
  wh2 <- apply(wh, 2, any) # This is "are the several domains inside the several existingObjSF"
  # length of existingObjSF
  domainExisted <- all(wh1)
  if (domainExisted) {

    # THIS IS "PULL OUT INDIVIDUAL SF POLYGON FROM THE MANY"
    if (isTRUE(verbose)) {
      if (isTRUE(bufferOK)) # the message will be previously given
        message("domain is within the buffered object; returning the existing parameters")
      else
        message(.message$cacheGeoDomainContained)
    }
    existingObj <- existingObj[wh2, ]
    existingObjSF <- existingObjSF[wh2, ]
  }
  if (all(wh1 %in% FALSE)) {
    existingObj <- NULL
    existingObjSF <- NULL
  }
  list(existingObj = existingObj, existingObjSF = existingObjSF, domainExisted = domainExisted)
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
