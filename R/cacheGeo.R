#' Cache-like function for spatial domains
#'
#' \if{html}{\figure{lifecycle-experimental.svg}{options: alt="experimental"}}
#'
#' This function is a combination of `Cache` and `prepInputs` but for spatial
#' domains. This differs from `Cache` in that the current function call doesn't
#' have to have an identical function call previously run. Instead, it needs
#' to have had a previous function call where the `domain` being passes is
#' *within* the geographic limits of the `targetFile` or file located at the `url`.
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
#' @param url The (optional) url of the object on Google Drive (the only option currently).
#'   This is only for downloading and uploading to.
#' @param targetFile The (optional) local file (or path to file)
#' @param domain An sf polygon object that is the spatial area of interest. If `NULL`,
#'   then this will return the whole object in `targetFile`.
#' @param FUN A function call that will be called if there is the `domain` is
#'   not already contained within the `sf` object at `url` or `targetFile`. This function
#'   call MUST return either a `sf` class object or a `data.frame` class object
#'   that has a geometry column (which can then be converted to `sf` with `st_as_sf`)
#' @param cloudFolderID If this is specified, then it must be either 1) a googledrive
#'   url to a folder where the `targetFile` will be read from or written to, or
#'   2) a googledrive id or 3) an absolute path to a (possibly non-existent yet)
#'   folder on your google drive.
#' @param useCloud A logical.
#' @param ... Any named objects that are needed for FUN
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
#' of a larger spatial object that is specified with `targetFile` or `url`.
#'
#' @export
#' @examples
#'
#' \donttest{
#'
#' if (requireNamespace("sf", quietly = TRUE) &&
#'     requireNamespace("terra", quietly = TRUE)) {
#' dPath <- checkPath(file.path(tempdir2()), create = TRUE)
#' localFileLux <- system.file("ex/lux.shp", package = "terra")
#'
#' # 1 step for each layer
#' # 1st step -- get study area
#' full <- prepInputs(localFileLux, dest = dPath) # default is sf::st_read
#' zoneA <- full[3:6,]
#' zoneB <- full[8,] # not in A
#' zoneC <- full[3,] # yes in A
#' zoneD <- full[7:8,] # not in A, B or C
#' zoneE <- full[3:5,] # yes in A
#' # 2nd step: re-write to disk as read/write is lossy; want all "from disk" for this ex.
#' writeTo(zoneA, writeTo = "zoneA.shp", destinationPath = dPath)
#' writeTo(zoneB, writeTo = "zoneB.shp", destinationPath = dPath)
#' writeTo(zoneC, writeTo = "zoneC.shp", destinationPath = dPath)
#' writeTo(zoneD, writeTo = "zoneD.shp", destinationPath = dPath)
#' writeTo(zoneE, writeTo = "zoneE.shp", destinationPath = dPath)
#' # Must re-read to get identical columns
#' zoneA <- sf::st_read(file.path(dPath, "zoneA.shp"))
#' zoneB <- sf::st_read(file.path(dPath, "zoneB.shp"))
#' zoneC <- sf::st_read(file.path(dPath, "zoneC.shp"))
#' zoneD <- sf::st_read(file.path(dPath, "zoneD.shp"))
#' zoneE <- sf::st_read(file.path(dPath, "zoneE.shp"))
#'
#' # The function that is to be run. This example returns a data.frame because
#' #    saving `sf` class objects with list-like columns does not work with
#' #    many st_driver()
#' fun <- function(domain, newField) {
#'   domain |>
#'     as.data.frame() |>
#'     cbind(params = I(lapply(seq_len(NROW(domain)), function(x) newField)))
#' }
#'
#' # Run sequence -- A, B will add new entries in targetFile, C will not,
#' #                 D will, E will not
#' for (z in list(zoneA, zoneB, zoneC, zoneD, zoneE))
#'   out <- CacheGeo(
#'       targetFile = "fireSenseParams.rds",
#'       domain = z,
#'       FUN = fun(domain, newField = I(list(list(a = 1, b = 1:2, c = "D")))),
#'       fun = fun, # pass whatever is needed into the function
#'       destinationPath = dPath,
#'       action = "update"
#'       #, cloudFolderID = "cachedObjects" # to upload/download from cloud
#'       )
#' }
#' }
CacheGeo <- function(targetFile = NULL, url = NULL, domain,
                     FUN,
                     destinationPath = getOption("reproducible.destinationPath", "."),
                     useCloud = getOption("reproducible.useCloud", FALSE),
                     cloudFolderID = NULL,
                     purge = FALSE, useCache = getOption("reproducible.useCache"),
                     overwrite = getOption("reproducible.overwrite"),
                     action = c("nothing", "update", "replace", "append"),
                     ...) {
  objExisted <- TRUE
  if (is.null(targetFile) && is.null(url))
    objExisted <- FALSE
  if (!isAbsolutePath(targetFile))
    targetFile <- file.path(destinationPath, targetFile)
  if (!file.exists(targetFile) && is.null(url))
    objExisted <- FALSE
  domainExisted <- objExisted

  if (!is.null(url) || isTRUE(useCloud) || !is.null(cloudFolderID)) {
    .requireNamespace("googledrive", stopOnFALSE = TRUE)
    alreadyOnRemote <- FALSE
    if (is.null(url)) {
      if (!(nchar(cloudFolderID) == 33 || grepl("https://drive", cloudFolderID))) {
        googledrive::with_drive_quiet(folderExists <- googledrive::drive_get(cloudFolderID))
        if (NROW(folderExists))
          cloudFolderID <- googledrive::as_id(folderExists$id)
      } else {
        cloudFolderID <- googledrive::as_id(cloudFolderID)
        folderExists <- googledrive::drive_get(cloudFolderID)
      }
      folderExists <- NROW(folderExists) > 0

      if (!folderExists)
        cloudFolderID <- googledrive::drive_mkdir(cloudFolderID)
      objsInGD <- googledrive::drive_ls(cloudFolderID)
      objID <- objsInGD[objsInGD$name %in% basename2(targetFile),]
      if (NROW(objID))
        url <- objID$drive_resource[[1]]$webViewLink
    } else {
      objID <- googledrive::drive_get(id = url)
    }
    if (NROW(objID))
      objExisted <- TRUE
    if (is.null(targetFile)) {
      targetFile <- file.path(destinationPath, objID$name)
    }
    if (file.exists(targetFile) && NROW(objID)) {
      md5Checksum <- digest::digest(file = targetFile)
      alreadyOnRemote <- identical(objID$drive_resource[[1]]$md5Checksum, md5Checksum)
      if (isTRUE(alreadyOnRemote))
        url <- NULL # browser()
    }
  }

  if (isTRUE(objExisted)) {
    existingObj <- prepInputs(targetFile = targetFile, url = url,
                              destinationPath = destinationPath, # domain = domain,
                              useCache = useCache, purge = purge,
                              overwrite = overwrite)
    existingObjSF <- if (is(existingObj, "sf")) existingObj else sf::st_as_sf(existingObj)
    if (!missing(domain)) {
      wh <- sf::st_within(domain, existingObjSF, sparse = FALSE)
      wh1 <- apply(wh, 1, any)
      domainExisted <- all(wh1)
      if (domainExisted) {
        message("Spatial domain is contained within the url; returning the object")
        existingObj <- existingObj[wh,]
      }
    } else {
      domainExisted <- TRUE
      message("Spatial domain is missing; returning entire spatial domain")
    }
  }
  if (isFALSE(objExisted) || isFALSE(domainExisted)) {
    message("Domain is not contained within the targetFile; running FUN")
    FUNcaptured <- substitute(FUN)
    list2env(list(...), envir = environment()) # need the ... to be "here"
    newObj <- eval(FUNcaptured, envir = environment())
    newObjSF <- if (is(newObj, "sf")) newObj else sf::st_as_sf(newObj)
  }
  if (isFALSE(domainExisted)) {
    if (isTRUE(objExisted)) {
      if (any(grepl("^a|^u", action[1], ignore.case = TRUE))) {
        existingObj <- rbind(existingObj, newObj)
        if (any(duplicated(existingObj)))
          existingObj <- unique(existingObj)
      }
    }
    if (isFALSE(objExisted)) {
      existingObj <- newObj
    }
    if (!any(grepl("^n", action[1], ignore.case = TRUE))) {
      if (!isAbsolutePath(targetFile))
        browser()
      writeTo(existingObj, writeTo = targetFile, overwrite = TRUE)
    }
  }
  # Cloud
  if (!is.null(url) || isTRUE(useCloud) || !is.null(cloudFolderID)) {
    if (NROW(objID)) {
      md5Checksum <- digest::digest(file = targetFile)
      alreadyOnRemote <- identical(objID$drive_resource[[1]]$md5Checksum, md5Checksum)
    }
    if (!alreadyOnRemote) {
      out <- googledrive::drive_put(media = targetFile,
                                    path = googledrive::as_id(cloudFolderID))
    } else {
      message("skipping googledrive upload; md5sum is same")
    }
  }

  existingObj
}

