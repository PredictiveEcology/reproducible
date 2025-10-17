#' Alternative to `prepInputs` that can use Spatial Tiles stored locally or on Google Drive
#'
#' Downloads, processes and optionally uploads a `SpatRaster` object through a tiling intermediary.
#' If the original `url` is for a very large object, but `to` is a relatively small subset
#' of the area represented by the spatial file at `url`, then this function will
#' potentially by-pass the download of the large file at `url` and instead only download
#' the minimum number of tiles necessary to cover the `to` area. When `doUploads` is
#' TRUE, then this function will potentially create and upload the tiles to `tileFolder`,
#' prior to returning the spatial object, `postProcess`ed to `to`. This function supports
#' both Google Drive and HTTP(S) URLs.
#'
#'
#' @param targetFile Character. Name of the target file to be downloaded or processed.
#'   If missing, it will be inferred from the URL or Google Drive metadata.
#' @param url Character. URL to the full dataset (Google Drive or HTTP/S).
#' @param destinationPath Character. Path to the directory where files will be downloaded and processed.
#' @param to A spatial object (e.g., `SpatRaster`, `SpatVector`, `sf`, or `Spatial*`) defining the area of interest.
#' @param tilesFolder A local file path to put tiles. If this is an absolute path, then
#'   that will be used; if it is a relative path, then it will be
#'   `file.path(destinationPath, tilesFolder)`
#' @param urlTiles Character. URL to the tile source (e.g., Google Drive folder or HTTP/S endpoint). Default is `getOption("reproducible.prepInputsUrlTiles", NULL)`.
#' @param doUploads Logical. Whether to upload processed tiles.
#'   Default is `getOption("reproducible.prepInputsDoUploads", FALSE)`.
#' @param tileGrid Either length 3 character string, such as "CAN", to be sent to `geodata::gadm(...)`
#'   or an actual `SpatVector` object with a grid of polygons
#' @param numTiles Integer. Number of tiles to generate. Optional.
#' @param plot.grid Logical. Whether to plot the tile grid and area of interest. Default is `FALSE`.
#' @param verbose Logical or numeric. Controls verbosity of messages. Default is `getOption("reproducible.verbose")`.
#' @param ... Passed to `writeRaster`, e.g., `datatype`.
#'
#' @return A `SpatRaster` object cropped to the area of interest (`to`), composed of the necessary tiles.
#' If the post-processed file already exists locally, it will be returned directly.
#'
#' @details
#' This function can be triggered *inside* `prepInputs`
#' if the `to` is supplied and both `url` and `urlTiles` are supplied. **NOTE**:
#' `urlTiles` can be supplied using the
#' `option(reproducible.prepInputsUrlTiles = someGoogleDriveFolderURL`), so the original
#' `prepInputs` function call can remain unaffected.
#'
#' This function is useful for working with large spatial datasets, but where the user
#' only requires a "relatively small" section of that dataset. This function will
#' potentially bypass the full download and download only the tiles that are necessary
#' for the `to`.
#' It handles downloading only the required tiles based on spatial intersection
#' with the target area, and supports resumable downloads from Google Drive or HTTP/S sources.
#'
#' If `targetFile` is missing, the function attempts to infer it from the URL
#' using the `Content-Disposition` header or the basename of the URL.
#' For Google Drive URLs, it uses the file metadata.
#'
#' @seealso [googledrive::drive_get()], [terra::rast()], [terra::crop()], [terra::merge()]
#'
#' @examples
#' \dontrun{
#' to <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(-123.3656, 48.4284)), crs = 4326))
#' result <- prepInputsWithTiles(
#'   url = "https://example.com/data.tif",
#'   destinationPath = tempdir(),
#'   to = to,
#'   urlTiles = "https://example.com/tiles/",
#'   tileGrid = "CAN"
#' )
#' }
#'
#' @export
prepInputsWithTiles <- function(targetFile, url, destinationPath,
                                to,
                                tilesFolder = "tiles",
                                urlTiles = getOption("reproducible.prepInputsUrlTiles", NULL),
                                doUploads = getOption("reproducible.prepInputsDoUploads", FALSE),
                                tileGrid = "CAN",
                                numTiles = NULL,
                                plot.grid = FALSE,
                                purge = FALSE,
                                verbose = getOption("reproducible.verbose"), ...) {

  st <- Sys.time()
  if (missing(to) || is.null(urlTiles)) {
    messagePreProcess(
      "prepInputsWithTiles must have `urlTiles` and `url` plus a `to` spatial object",
      "returning `'NULL'`", verbose = verbose)
    return("NULL")
  }

  datatype <- "FLT4S"
  dtype <- list(...)$datatype
  if (!is.null(dtype))
    datatype <- dtype

  # Preview intersecting tile IDs
  url <- gsub("(?<!:)//+", "/", url, perl = TRUE) # removes double // except in http://
  isGDid <- isGoogleID(url)
  isGDurl <- isGoogleDriveURL(url)
  if (isGDid) {
    url <- googledriveIDtoHumanURL(url)
    isGDurl <- TRUE
  }

  remoteMetadata <- getRemoteMetadata(targetFile, isGDurl, url)

  remoteHashFile <- makeRemoteHashFile(url, dPath, remoteMetadata$targetFile, remoteMetadata$remoteHash)
  purge <- checkHaveCorrectHashedVersion(remoteHashFile, remoteMetadata$remoteHash, purge, verbose)

  dig <- .robustDigest(to)

  if (is.null(remoteMetadata$targetFile)) {
    stop("Please supply `targetFile` or a url from which `targetFile` can be extracted from")
  }
  targetFileFullPath <- file.path(destinationPath, remoteMetadata$targetFile)
  messagePreProcess("Preparing ", .messageFunctionFn(targetFileFullPath), verbose = verbose)
  targetFilePostProcessedFullPath <- .suffix(targetFileFullPath, dig)

  if (isTRUE(purge)) {
    purgeLocals(targetFilePostProcessedFullPath, targetFileFullPath, remoteHashFile, verbose)
  }

  if (file.exists(targetFilePostProcessedFullPath) && doUploads %in% FALSE) {
    messagePreProcess("Correct post processed file exists (",
                             .messageFunctionFn(targetFilePostProcessedFullPath),
                             ");\nreturning it now...", verbose = verbose)
    messagePreProcess("prepInputsWithTiles ", gsub("^\b", "", messagePrefixDoneIn),
                      format(difftime(Sys.time(), st), units = "secs", digits = 3),
                      verbose = verbose)
    return(terra::rast(targetFilePostProcessedFullPath))
  }

  tilesFolderFullPath <- file.path(tilesFolder, filePathSansExt(remoteMetadata$targetFile))
  if (fs::is_absolute_path(tilesFolder) %in% FALSE) {
    tilesFolderFullPath <- file.path(destinationPath, tilesFolderFullPath)
  }
  dirTilesFolder <- dir(tilesFolderFullPath, recursive = TRUE, all.files = TRUE)

  if (isTRUE(purge) && length(dirTilesFolder)) {
    dirTilesFolder <- purgeLocalTiles(tilesFolderFullPath, verbose)
  }

  # Need to get target object crs targetObjCRS; first try local file, then local tile,
  #     then gdrive tile, then full remote file
  targetObjCRS <- getTargetCRS(targetFileFullPath, dirTilesFolder, tilesFolderFullPath, remoteMetadata$targetFile,
                           url, urlTiles, remoteMetadata$fileSize, remoteMetadata$remoteHash, purge, doUploads, verbose)
  # need to rerun because there may have been a rm in previous line
  dirTilesFolder <- dir(tilesFolderFullPath, recursive = TRUE, all.files = TRUE)

  noTiles <- FALSE

  tileGridAndArea <- makeAndPlotTileGrid(tileGrid, theArea, numTiles, targetObjCRS,
                                         plot.grid, to, verbose)

  # Find intersecting tiles
  all_tile_names <- sort(makeTileNames(tileGridAndArea$tileGrid$tile_id))

  to_inTileGrid <- postProcessTo(to, to = targetObjCRS, verbose = verbose - 2)
  intersecting_tiles <- terra::intersect(tileGridAndArea$tileGrid, terra::ext(to_inTileGrid))
  needed_tile_names <- makeTileNames(intersecting_tiles$tile_id)
  needed_tile_names <- sort(needed_tile_names)

  missingTilesLocal <- setdiff(needed_tile_names, dirTilesFolder)

  missingTilesLocalAll <- setdiff(all_tile_names, dirTilesFolder)
  tilesToGet <- missingTilesLocal
  haveLocalTiles <- FALSE
    messagePreProcess("Need to load/get these tiles:\n", verbose = verbose) # use message because of line wrap
    if (verbose > 0) {
      messagePreProcess(.messageFunctionFn(paste(needed_tile_names, collapse =  ", ")), verbose = verbose)
  }
  haveAllNeededTiles <- if (doUploads %in% TRUE) length(missingTilesLocalAll) == 0 else TRUE


  if (length(missingTilesLocal) == 0){# && (haveAllNeededTiles)) {
    messagePreProcess(
      "✅ All needed tiles are available locally. Proceeding to load them",
      verbose = verbose)
    haveLocalTiles <- TRUE
  } else {
    messagePreProcess(
      "⚠️ Tiles are missing locally. Will try to download these:\n",
      verbose = verbose)
    messagePreProcess(.messageFunctionFn(paste(missingTilesLocal, collapse = ", ")), verbose = verbose)
    messagePreProcess(paste0("... from urlTiles (",.messageFunctionFn(urlTiles),")"), verbose = verbose)
  }

  for (ii in 1:2) { # try twice in case a local tile is corrupt; if yes, delete it, redownload, reload
    if (haveLocalTiles %in% FALSE || doUploads) {
      needed_tile_names <- downloadMakeAndUploadTiles(url, urlTiles, remoteMetadata$targetFile, targetFileFullPath,
                                                      needed_tile_names, tilesToGet, all_tile_names, haveLocalTiles,
                                                      tilesFolderFullPath, tileGridAndArea$tileGrid, tileGridAndArea$numTiles,
                                                      to_inTileGrid, doUploads, datatype, verbose)
    }
    tile_rasters <- rastTiles(needed_tile_names, tilesFolderFullPath)
    if (any(sapply(tile_rasters, is.null))) {
      missingTilesLocal <- setdiff(needed_tile_names, dir(tilesFolderFullPath))
      if (length(missingTilesLocal))
        haveLocalTiles <- FALSE
    } else {
      break
    }
  }
  noData <- FALSE

  if (file.exists(targetFilePostProcessedFullPath)) {
    messagePreProcess("Correct post processed file exists (",
                             .messageFunctionFn(targetFilePostProcessedFullPath),
                      ");\n returning it now...", verbose = verbose)
    return(terra::rast(targetFilePostProcessedFullPath))
  }

  if (noTiles %in% FALSE) {
    rfull <- sprcMosaicRast(url, tile_rasters, to_inTileGrid, targetFilePostProcessedFullPath,
                            remoteMetadata$fileSize, needed_tile_names, tilesFolderFullPath,
                            noData, datatype, verbose)
  }
  messagePreProcess("prepInputsWithTiles ", gsub("^\b", "", messagePrefixDoneIn),
                    format(difftime(Sys.time(), st), units = "secs", digits = 3),
                    verbose = verbose)
  rfull
}

tile_raster_write_auto <- function(raster_path, out_dir, tileGrid, all_tile_names, nx = 10, ny = 5,
                                   datatype = NULL,
                                   verbose = getOption("reproducible.verbose")) {
  r <- terra::rast(raster_path)

  ext <- terra::ext(r)
  x_breaks <- seq(ext[1], ext[2], length.out = nx + 1)
  y_breaks <- seq(ext[3], ext[4], length.out = ny + 1)

  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  # Build tile specs
  tile_specs <- list()
  tile_id <- 1

  for (i in 1:nx) {
    for (j in 1:ny) {
      tile_ext <- terra::ext(x_breaks[i], x_breaks[i + 1], y_breaks[j], y_breaks[j + 1])
      tile_path <- file.path(out_dir, paste0(all_tile_names[tile_id]))
      #                       paste0("tile_", sprintf("%02d", tile_id),
      #                              ".tif"))
      tile_specs[[tile_id]] <- list(ext = tile_ext, path = tile_path)
      tile_id <- tile_id + 1
    }
  }

  # Worker function
  process_tile <- function(spec) {
    if (!file.exists(spec$path)) {
      tile <- terra::crop(r, spec$ext)
      # isAllNA <- terra::allNA(tile)[1] %in% TRUE
      # if (isAllNA %in% FALSE) {

        terra::writeRaster(tile, spec$path, datatype = datatype,
                           overwrite = FALSE,
                           gdal = c("COMPRESS=LZW", "TILED=YES"))
        return(paste("✅ Saved:", spec$path))
      # }
    } else {
      return(paste("⏩ Skipped (already exists):", spec$path))
    }
  }

  messagePreProcess("🧩 Creating tiles ...", verbose = verbose)

  # Choose parallel or sequential based on OS
  if (.Platform$OS.type == "unix") {
    numCoresToUse <- numCoresToUse(max = length(tile_specs))
    results <- parallel::mclapply(
      tile_specs, process_tile,
      mc.cores = numCoresToUse, datatype = datatype)
  } else {
    results <- lapply(tile_specs, process_tile)
  }

  # Print results
  for (msg in results[!sapply(results, is.null)]) messagePreProcess(msg, verbose = verbose)
  messagePreProcess("🎉 Tiling complete.", verbose = verbose)
}

extract_drive_id <- function(url) {
  # Try to match folder ID
  folder_match <- sub(".*?/folders/([a-zA-Z0-9_-]+).*", "\\1", url)
  # Try to match file ID if folder match didn't change the string
  if (identical(folder_match, url)) {
    file_match <- sub(".*?/file/d/([a-zA-Z0-9_-]+).*", "\\1", url)
    return(file_match)
  }
  return(folder_match)
}

upload_tiles_to_drive_url_parallel <- function(local_dir, drive_folder_url, thisFilename,
                                               verbose = getOption("reproducible.verbose")) {
  # Extract parent folder ID from URL
  parent_id <- extract_drive_id(drive_folder_url)

  # Create subfolder named after original raster filename
  subfolder_name <- basename(tools::file_path_sans_ext(thisFilename))
  subfolder <- googledrive::drive_find(q = paste0("name = '", subfolder_name, "' and '", parent_id, "' in parents"))

  if (nrow(subfolder) == 0) {
    subfolder <- googledrive::drive_mkdir(subfolder_name, path = googledrive::as_id(parent_id))
    messagePreProcess("📁 Created subfolder: ", .messageFunctionFn(subfolder_name), verbose = verbose)
  } else {
    messagePreProcess("📁 Found existing subfolder: ", .messageFunctionFn(subfolder_name), verbose = verbose)
  }

  # List local .tif files
  tif_files <- dir(local_dir, pattern = "\\.tif$", full.names = TRUE)

  # Get existing files in Drive subfolder
  existingAll <- googledrive::drive_ls(subfolder$id)
  existing_names <- existingAll$name

  # Upload helper
  upload_one <- function(file_path) {
    file_name <- basename(file_path)
    if (!(file_name %in% existing_names)) {
      googledrive::drive_upload(file_path, path = googledrive::as_id(subfolder$id))
      return(paste("✅ Uploaded:", file_name))
    } else {
      return(paste("⏩ Skipped (already exists):", file_name))
    }
  }

  # Upload in parallel on Linux/macOS, sequential on Windows
  if (.Platform$OS.type == "unix") {
    numCoresToUse <- numCoresToUse(max = 7) # more than 7 on a fast internet connection
                         # tends to be slower; but this will depend on connection speed
    results <- parallel::mclapply(
      tif_files, upload_one,
      mc.cores = numCoresToUse)
  } else {
    results <- lapply(tif_files, upload_one)
  }

  # Print results
  for (msg in results) messagePreProcess(msg, verbose = verbose)
  messagePreProcess("🎉 Upload complete.", verbose = verbose)
}

makeTileGrid <- function(ext, crs, numTiles) {
  if (missing(crs)) crs <- proj4stringSCANFI

  # ext <- terra::ext(c(xmin = -2341500, xmax = 3010500, ymin = 5863500, ymax = 9436500))
  areaV <- terra::as.polygons(ext, crs = crs)
  areaGrid <- sf::st_make_grid(sf::st_as_sfc(sf::st_as_sf(areaV)), n = numTiles) |>
    terra::vect()
  m <- t(matrix(seq(prod(numTiles)), nrow = numTiles[[2]], byrow = F))
  areaGrid[["tile_id"]] <- makePaddedNamesForTiles(as.character(m))
  areaGrid
}

proj4stringSCANFI <- "+proj=lcc +lat_0=0 +lon_0=-95 +lat_1=49 +lat_2=77 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

makeTileNames <- function(tileIds) {
  paddedTileNumbers <- makePaddedNamesForTiles(tileIds)
  paste0("tile_", paddedTileNumbers, ".tif")
}

rastTiles <- function(tiles, tilesFolderFullPath) {
  tile_rasters <- Map(x = tiles, function(x) {
    a <- try(terra::rast(file.path(tilesFolderFullPath, x)), silent = TRUE)
    if (is(a, "try-error")) {
      a <- rmRastIfTryError(a, tilesFolderFullPath, x)
    }
    a
    })
}

build_albers_proj4_longlat <- function(ext_obj) {
  if (!inherits(ext_obj, "SpatExtent")) stop("Input must be a terra::ext object")

  # Extract geographic bounds
  lon_min <- ext_obj[1]
  lon_max <- ext_obj[2]
  lat_min <- ext_obj[3]
  lat_max <- ext_obj[4]

  # Central meridian and latitude of origin
  lon_0 <- (lon_min + lon_max) / 2
  lat_0 <- (lat_min + lat_max) / 2

  # Standard parallels: 1/6 from top and bottom
  lat_1 <- lat_min + (lat_max - lat_min) / 6
  lat_2 <- lat_max - (lat_max - lat_min) / 6

  # Build PROJ.4 string
  proj4string <- paste(
    "+proj=aea",
    paste0("+lat_1=", round(lat_1, 6)),
    paste0("+lat_2=", round(lat_2, 6)),
    paste0("+lat_0=", round(lat_0, 6)),
    paste0("+lon_0=", round(lon_0, 6)),
    "+x_0=0 +y_0=0",
    "+datum=WGS84 +units=m +no_defs"
  )

  return(proj4string)
}

build_lambert_proj4 <- function(ext_obj) {
  if (!inherits(ext_obj, "SpatExtent")) stop("Input must be a terra::ext object")

  # Extract geographic bounds
  lon_min <- ext_obj[1]
  lon_max <- ext_obj[2]
  lat_min <- ext_obj[3]
  lat_max <- ext_obj[4]

  # Central meridian and latitude of origin
  lon_0 <- (lon_min + lon_max) / 2
  lat_0 <- (lat_min + lat_max) / 2

  # Standard parallels: 1/6 from top and bottom
  lat_1 <- lat_min + (lat_max - lat_min) / 6
  lat_2 <- lat_max - (lat_max - lat_min) / 6

  # Build PROJ.4 string
  proj4string <- paste(
    "+proj=lcc",
    paste0("+lat_1=", round(lat_1, 6)),
    paste0("+lat_2=", round(lat_2, 6)),
    paste0("+lat_0=", round(lat_0, 6)),
    paste0("+lon_0=", round(lon_0, 6)),
    "+x_0=0 +y_0=0",
    "+datum=WGS84 +units=m +no_defs"
  )

  return(proj4string)
}

best_square_grid <- function(m, n, min_tiles = 1, max_tiles = 1000) {
  best_diff <- Inf
  best_grid <- NULL

  for (tiles in seq(min_tiles, max_tiles)) {
    for (nx in 1:tiles) {
      ny <- ceiling(tiles / nx)
      tile_w <- m / nx
      tile_h <- n / ny
      aspect_ratio <- tile_w / tile_h
      diff <- abs(log(aspect_ratio))  # closer to 0 = more square

      if (diff < best_diff) {
        best_diff <- diff
        best_grid <- list(
          nx = nx,
          ny = ny,
          tile_width = tile_w,
          tile_height = tile_h,
          total_tiles = nx * ny,
          aspect_ratio = round(aspect_ratio, 3)
        )
      }
    }
  }

  return(best_grid)
}


makeTileGridFromGADMcode <- function(tileGrid, numTiles = NULL, crs) {
  g <- geodata::gadm(tileGrid, resolution = 2) |> Cache()
  if (is.null(g) || (is.character(g) && isTRUE(g == "NULL"))) {
    # most likely geodata server is down
    tileExt <- terra::ext(c(xmin = -2342000, xmax = 3011000, ymin = 5860000, ymax = 9436000))
    tilePoly2 <- tileExt
  } else {

    tilePoly <- {terra::aggregate(g)} |> Cache()

    if (grepl("CAN", substr(tileGrid, 1, 3), ignore.case = TRUE)) {
      vals <- terra::ext(tilePoly)[]
      vals[["ymax"]] <- 70
      tilePoly <- terra::crop(tilePoly, terra::ext(vals))
    }
    if (missing(crs))
      crs <- build_lambert_proj4(terra::ext(tilePoly))
    tilePoly2 <- postProcess(tilePoly, to = crs) |> Cache()
    vals <- terra::ext(tilePoly2)[]
    tileExt <- c(xmin = floor(vals[["xmin"]]/1e3) * 1e3,
                 xmax = ceiling(vals[["xmax"]]/1e3) * 1e3,
                 ymin = floor(vals[["ymin"]]/1e3) * 1e3,
                 ymax = ceiling(vals[["ymax"]]/1e3) * 1e3)
    tileExt <- terra::ext(tileExt)

  }
  if (is.null(numTiles)) {
    bsg <- best_square_grid(m = tileExt[][["xmax"]] - tileExt[][["xmin"]],
                            n = tileExt[][["ymax"]] - tileExt[][["ymin"]]
                            , min_tiles = 100, max_tiles = 200)
    numTiles <- unlist(bsg[c("nx", "ny")])
  }
  tg <- makeTileGrid(tileExt, crs = crs, numTiles = numTiles)
  list(tileGrid = tg, numTiles = numTiles, area = tilePoly2)
}

makePaddedNamesForTiles <- function(tileIds) {
  ncharNeeded <- max(nchar(tileIds))
  sprintf(paste0("%0", ncharNeeded,"d"), as.integer(tileIds))
}

lsExistingTilesOnGoogleDrive <- function(urlTiles, targetFile) {
  urlTilesID <- googledrive::as_id(extract_drive_id(urlTiles))
  tile_folder_onGoogleDrive <- googledrive::drive_get(urlTilesID)
  # targetFile <- "alnu_rub.tif"

  # List all files in the folder
  existing_tiles <- googledrive::drive_ls(tile_folder_onGoogleDrive)
  hasSubfolder <- grep(filePathSansExt(targetFile), existing_tiles$name)
  if (length(hasSubfolder)) {
    tile_subfolder <- existing_tiles[hasSubfolder, ]$id
    existing_tiles <- googledrive::drive_ls(tile_subfolder)
  } else {
    existing_tiles <- NULL
  }
  existing_tiles
}

crsFromLocalTile <- function(tilesFolderFullPath, dirTilesFolder) {
  for (iii in 1:3) { # try a few in case there is a corrupt one
    theFile <- file.path(tilesFolderFullPath, dirTilesFolder[iii])
    targetObjCRS <- tryRastThenGetCRS(theFile)
    if (!is.null(targetObjCRS))
      break
  }
  targetObjCRS
}

crsFromGoogleDriveTile <- function(tilesFolderFullPath, existing_tiles, fileSize, verbose = getOption("reproducible.verbose")) {
  ogwd <- getwd()
  if (dir.exists(tilesFolderFullPath) %in% FALSE)
    dir.create(tilesFolderFullPath, recursive = TRUE, showWarnings = FALSE)
  setwd(tilesFolderFullPath)
  on.exit(setwd(ogwd))
  download_resumable_httr2(existing_tiles$id[1], existing_tiles$name[1], fileSize, verbose = verbose - 1)
  targetObjCRS <- tryRastThenGetCRS(file.path(tilesFolderFullPath, existing_tiles$name[1]))
  setwd(ogwd)
  targetObjCRS
}

crsFromLocalFile <- function(targetFileFullPath, targetObjCRS) {
  tryRastThenGetCRS(targetFileFullPath)
  # targetObj <- try(terra::rast(targetFileFullPath))
  # if (is(targetObj, "try-error")) {
  #   # unlink(targetFileFullPath, force = TRUE)
  #   message("File appears to be corrupt; deleting it and trying local tiles, then remotes")
  # } else {
  #   targetObjCRS <- terra::crs(targetObj)
  # }
}

getTargetCRS <- function(targetFileFullPath, dirTilesFolder, tilesFolderFullPath,
                         targetFile,
                         url, urlTiles, fileSize, remoteHash, purge, doUploads, verbose) {

  targetObjCRS <- NULL # don't know it yet
  if (file.exists(targetFileFullPath)) {
    targetObjCRS <- crsFromLocalFile(targetFileFullPath, targetObjCRS)
  }
  # need to get the targetObjCRS to know what the tiles will look like
  if (is.null(targetObjCRS)) {
    targetObjCRS <- crsFromLocalOrGDTiles(targetObjCRS, dirTilesFolder, tilesFolderFullPath, urlTiles,
                                          targetFile, purge, doUploads, fileSize, verbose)
  }
  if (is.null(targetObjCRS)) {
    # still doesn't have it
    messagePreProcess("Downloading full file (", .messageFunctionFn(targetFile),") from\n", url, verbose = verbose)
    if (!exists("fileSize", inherits = FALSE))
      messageAboutFilesize(fileSize, verbose = verbose)
    download_resumable_httr2(url, targetFileFullPath, fileSize = fileSize)

    # rfull <- terra::rast(targetFileFullPath)
    targetObjCRS <- terra::crs(terra::rast(targetFileFullPath))
  }
  makeRemoteHashFile(url, dPath, targetFile, remoteHash, write = TRUE)
  targetObjCRS
}

plotGridAndArea <- function(tileGrid, theArea, to) {
  a <- terra::centroids(tileGrid)
  terra::plot(tileGrid)
  terra::text(a, labels = a$tile_id, col = "blue", cex = 1.2)
  if (is(theArea, "SpatExtent")) {
    theArea <- terra::as.polygons(theArea, crs = to)
  }
  tilePolyTG <- terra::project(theArea, tileGrid)
  terra::plot(tilePolyTG, add = TRUE)
  terra::plot(to, add = TRUE, col = "red")
}

getTilesFromGoogleDrive <- function(tilesToGet, existing_tiles, tilesFolderFullPath) {
  whGet <- match(tilesToGet, existing_tiles$name)
  tileIDSToGet <- existing_tiles[whGet, ]
  ogwd <- getwd()
  if (dir.exists(tilesFolderFullPath) %in% FALSE)
    dir.create(tilesFolderFullPath, recursive = TRUE, showWarnings = FALSE)
  setwd(tilesFolderFullPath)
  on.exit(setwd(ogwd))
  by(tileIDSToGet, seq_len(NROW(tileIDSToGet)), function(i) {
    download_resumable_httr2(i$id, i$name, gdriveDetails = i,
                             fileSize = as.numeric(i$drive_resource[[1]]$size))
  })
  haveLocalTiles <- TRUE
  setwd(ogwd)
  haveLocalTiles
}

downloadMakeAndUploadTiles <- function(url, urlTiles, targetFile, targetFileFullPath,
                                       needed_tile_names, tilesToGet, all_tile_names, haveLocalTiles,
                                       tilesFolderFullPath, tileGrid, numTiles,
                                       to_inTileGrid, doUploads, datatype, verbose) {
  existing_tiles <- lsExistingTilesOnGoogleDrive(urlTiles, targetFile)

  available_tile_names_onGoogleDrive <- existing_tiles$name

  # Determine which tiles are missing
  missingTilesOnRemote <- setdiff(needed_tile_names, available_tile_names_onGoogleDrive)
  # tilesToGet <- intersect(needed_tile_names, available_tile_names_onGoogleDrive)

  haveRemoteTiles <- all(all_tile_names %in% existing_tiles$name)
  # Preview decision
  needUploads <- TRUE
  doTileDownload <- FALSE
  missingTilesRemoteAll <- setdiff(all_tile_names, existing_tiles$name)

  tilesFullOnRemote <- TRUE
  if (doUploads %in% TRUE) tilesFullOnRemote <- length(missingTilesRemoteAll) == 0

  if (length(missingTilesOnRemote) == 0) {
    doTileDownload <- haveLocalTiles %in% FALSE
    messagePreProcess("✅ All needed tiles are available on Google Drive.  ",
                      verbose = verbose)
    needUploads <- tilesFullOnRemote %in% FALSE
    if (doTileDownload) {
      messagePreProcess("Proceeding to download only the needed tiles...", verbose = verbose)
    } else {
      messagePreProcess("Nothing to download", verbose = verbose)
    }
  } else {
    messagePreProcess("⚠️ Some tiles are missing on Google Drive:")
    missingOnes <- if (doUploads) missingTilesRemoteAll else missingTilesOnRemote
    if (verbose > 0) message(paste(missingOnes, collapse = ", "))
  }

  if (needUploads && length(missingTilesOnRemote) == 0) {
    messagePreProcess("Some 'unneeded' tiles are missing, but doUploads is TRUE and local tiles exist: ",
                      "uploading: ", verbose = verbose)
    messagePreProcess(.messageFunctionFn(paste(missingTilesRemoteAll, collapse = ", ")), verbose = verbose)
  }

  if (haveLocalTiles %in% FALSE && doTileDownload %in% TRUE) {
    haveLocalTiles <- getTilesFromGoogleDrive(tilesToGet, existing_tiles, tilesFolderFullPath)
  }

  if (needUploads %in% TRUE || (doUploads %in% TRUE && haveRemoteTiles %in% FALSE)) {
    fe <- file.exists(targetFileFullPath)
    if (fe %in% FALSE)
      download_resumable_httr2(url, targetFileFullPath)


    if (haveLocalTiles %in% FALSE || (doUploads %in% TRUE && needUploads))
      tile_raster_write_auto(targetFileFullPath, tilesFolderFullPath, tileGrid,
                             all_tile_names = all_tile_names, datatype = datatype,
                             nx = numTiles[[1]], ny = numTiles[[2]],
                             verbose = verbose)
    if (needUploads %in% FALSE && doUploads %in% TRUE)
      messagePreProcess("Nothing to upload", verbose = verbose)

    upload_tiles_to_drive_url_parallel(tilesFolderFullPath, urlTiles, targetFileFullPath,
                                       verbose = verbose)
    tile_paths <- dir(tilesFolderFullPath, pattern = "\\.tif$")
    saExt <- terra::ext(to_inTileGrid)

    # Filter tiles that intersect the study area
    intersecting_tiles2 <- purrr::keep(tile_paths, function(path) {
      tile_ext <- terra::ext(terra::rast(file.path(tilesFolderFullPath, path)))

      # Check for bounding box overlap
      !(tile_ext[1] > saExt[2] || tile_ext[2] < saExt[1] ||  # x overlap
          tile_ext[3] > saExt[4] || tile_ext[4] < saExt[3])    # y overlap
    })
    if (!identical(needed_tile_names, intersecting_tiles2)) {
      messagePreProcess("`to` does not overlap with any tiles on file at:\n",
                        .messageFunctionFn(url), verbose = verbose)
      # the intersecting_tiles2 from the newly created need to be the same as the
      # expected from the grid
    }

  }
  needed_tile_names
}


messageAboutFilesizeCompare <- function(fileSize, needed_tile_names,
                                        targetFilePostProcessedFullPath,  tilesFolderFullPath,
                                        verbose) {
  # fileSize <- file$drive_resource[[1]]$size
  messageAboutFilesize(fileSize, verbose = verbose, msgMiddle = " on remote url ")
  fsLocal <- file.size(targetFilePostProcessedFullPath)
  dd1 <- dir(tilesFolderFullPath)
  dd2 <- dir(tilesFolderFullPath, full.names = TRUE)
  tilesUsed <- dd2[match(needed_tile_names, dd1)]
  messageAboutFilesize(file.size(tilesUsed), verbose = verbose, msgMiddle = " on local drive using tiles ")
}



tryRastThenGetCRS <- function(targetFileFullPath) {
  targetObj <- try(terra::rast(targetFileFullPath))
  if (is(targetObj, "try-error")) {
    rmRastIfTryError(targetObj, dirname(targetFileFullPath), basename(targetFileFullPath))
    # unlink(targetFileFullPath, force = TRUE)
    targetObjCRS <- NULL
    # message("File (", targetFileFullPath, ") appears to be corrupt")#; deleting it and trying local tiles, then remotes")
  } else {
    targetObjCRS <- terra::crs(targetObj)
  }
  targetObjCRS
}



#' Estimate Number of CPU Cores to Use for Parallel Processing
#'
#' This function estimates the number of CPU cores that can be safely used for
#' parallel processing, taking into account a minimum threshold, the total
#' number of physical cores, and currently active threads.
#'
#' @param min An integer specifying the minimum number of cores to use. Default
#'   is `2`.
#' @param max An integer specifying the maximum number of cores available,
#'   typically the number of physical cores. Default is
#'   `parallel::detectCores(logical = FALSE)`.
#'
#' @return An integer representing the number of cores that can be used for
#'   parallel tasks, ensuring at least `min` cores are used, while subtracting
#'   one for the current process and an estimate of actively used threads (via
#'   `detectActiveCores()`).
#'
#' @examples
#' \dontrun{
#'   numCoresToUse()
#'   numCoresToUse(min = 4)
#' }
#'
#' @note This function depends on `detectActiveCores()` and is not supported on
#'   Windows systems.
#'
#' @seealso [detectActiveCores()]
#'
numCoresToUse <- function(min = 2, max) {
  if (is.null(.pkgEnv$detectedCores))
    .pkgEnv$detectedCores <- parallel::detectCores(logical = FALSE)
  dc <- .pkgEnv$detectedCores
  if (missing(max))
    max <- dc
  max <- min(dc -  # total
               1 - # remove one for the current process
               detectActiveCores(), # estimate actively used ones
             max)
  max(min, max)
}

makeRemoteHashFile <- function(url, dPath, targetFile, remoteHash, write = FALSE) {
  url_no_protocol <- sub("^https?://", "", url)
  # Replace all slashes with underscores
  urlWithUnderscores <- gsub("/", "_", file.path(basename(targetFile), dirname(url_no_protocol)))
  remoteHashFile <- file.path(dPath, paste0(urlWithUnderscores, ".hash"))
  if (isTRUE(write) && !file.exists(remoteHashFile))
    writeLines(remoteHash, remoteHashFile)
  return(remoteHashFile)
}



checkHaveCorrectHashedVersion <- function(remoteHashFile, remoteHash, purge, verbose) {
  haveCorrectVersion <- FALSE
  fe <- file.exists(remoteHashFile)
  if (fe)
    haveCorrectVersion <- identical(readLines(remoteHashFile), remoteHash)
  if (isTRUE(fe)) {
    if (haveCorrectVersion %in% FALSE) {
      message("The local version is not the version that matches the remote version")
      message("Do you want to purge all local data and redownload? Y or N")
      yorn <- readline(" ")
      yorn <- substr(tolower(yorn), 1, 1)
      if (identical("y", yorn))
        purge <- TRUE
    } else {
      if (!purge %in% TRUE)
        messagePreProcess("Local files match the current remote file version; proceeding",
                          verbose = verbose)
    }
  }
  purge
}

getRemoteMetadata <- function(targetFile, isGDurl, url) {
  if (missing(targetFile) && isGDurl) {
    file <- googledrive::drive_get(url) |>
      Cache(verbose = FALSE, notOlderThan = Sys.time() - 60*60) # refresh every hour
    fileSize <- file$drive_resource[[1]]$size
    # file_id <- file$id
    remoteHash <- file$drive_resource[[1]]$md5Checksum
    targetFile <- file$name
    timestampOnline <- file$drive_resource[[1]]$modifiedTime

  }

  if (missing(targetFile)) {
    response <- httr2::request(url) |> httr2::req_method("HEAD") |> httr2::req_perform()
    remoteHash <- httr2::resp_headers(response)[["etag"]] |>
      gsub(pattern = "^\"|\"$", replacement = "")

    content_disposition <- httr2::resp_header(response, "content-disposition")
    fileSize <- httr2::resp_header(response, "content-length") |> as.numeric()
    timestampOnline <- httr2::resp_header(response, "Date")
    if (isTRUE(!(is.na(content_disposition)))) {
      targetFile <- sub('.*filename="([^"]+)".*', '\\1', content_disposition)
    } else {
      # Fallback: extract from URL
      targetFile <- basename(url)
    }
  }
  list(targetFile = targetFile, fileSize = fileSize, remoteHash = remoteHash, timestampOnline = timestampOnline)
}

sprcMosaicRast <- function(url, tile_rasters, to_inTileGrid, targetFilePostProcessedFullPath,
                           fileSize, needed_tile_names, tilesFolderFullPath, noData, datatype, verbose) {
  allNull <- all(sapply(tile_rasters, is.null))
  if (allNull %in% FALSE) {
    anyNull <- any(sapply(tile_rasters, is.null))
    if (anyNull) {
      stop("For unknown reasons, the tiles are not available")
    }
    mosaic_raster <- terra::sprc(tile_rasters)
    # mosaic_raster <- terra::vrt(mosaic_raster)
    intersects <- terra::intersect(terra::ext(mosaic_raster), terra::ext(to_inTileGrid))
    if (!is.null(intersects)) {
      messagePrepInputs("cropping ... ", verbose = verbose)
      st1 <- Sys.time()
      final <- terra::crop(mosaic_raster, to_inTileGrid)
      messagePreProcess("  ", gsub("^\b", "", messagePrefixDoneIn),
                        format(difftime(Sys.time(), st1), units = "secs", digits = 3),
                        verbose = verbose)

      st3 <- Sys.time()
      messagePrepInputs("merging tiles ", .messageFunctionFn(targetFilePostProcessedFullPath), " ...", verbose = verbose)
      merged <- terra::merge(final)
      messagePreProcess("  ", gsub("^\b", "", messagePrefixDoneIn),
                        format(difftime(Sys.time(), st3), units = "secs", digits = 3),
                        verbose = verbose)

      st2 <- Sys.time()
      messagePrepInputs("writing ", .messageFunctionFn(targetFilePostProcessedFullPath), " ...", verbose = verbose)
      rfull <- terra::writeRaster(merged, filename = targetFilePostProcessedFullPath,
                                  datatype = datatype,
                                  overwrite = TRUE)
      messagePreProcess("  ", gsub("^\b", "", messagePrefixDoneIn),
                        format(difftime(Sys.time(), st2), units = "secs", digits = 3),
                        verbose = verbose)

      if (exists("fileSize", inherits = FALSE)) {
        messageAboutFilesizeCompare(fileSize, needed_tile_names,
                                    targetFilePostProcessedFullPath,  tilesFolderFullPath,
                                    verbose)
      }
    } else {
      noData <- TRUE
    }
  } else {
    noData <- TRUE
  }
  if (isTRUE(noData)) {
    messagePreProcess("The dataset at \n", url, "\ndoes not have data inside `to`; ",
                      "returning NULL", verbose = verbose)
    rfull <- NULL
  }
  rfull
}

purgeLocals <- function(targetFilePostProcessedFullPath, targetFileFullPath, remoteHashFile, verbose) {
  messagePreProcess("purge = TRUE; purging local targetFile", verbose = verbose)
  if (file.exists(targetFilePostProcessedFullPath))
    unlink(targetFilePostProcessedFullPath)
  if (file.exists(targetFileFullPath))
    unlink(targetFileFullPath)
  if (file.exists(remoteHashFile))
    unlink(remoteHashFile)
}

purgeLocalTiles <- function(tilesFolderFullPath, verbose) {
  messagePreProcess("purge = TRUE; purging local tiles", verbose = verbose)
  dirTilesFolder2 <- dir(tilesFolderFullPath, recursive = TRUE, all.files = TRUE, full.names = TRUE)
  unlink(dirTilesFolder2)
  dirTilesFolder <- NULL
  dirTilesFolder
}

purgeGoogleTiles <- function(urlTiles, targetFile, verbose) {
  messagePreProcess("purging GoogleDrive tiles...", verbose = verbose)
  folderID <- googledrive::drive_ls(googledrive::as_id(extract_drive_id(urlTiles)),
                                    pattern = filePathSansExt(targetFile))
  googledrive::drive_rm(folderID)
  existing_tiles <- NULL
  existing_tiles
}


rmRastIfTryError <- function(obj, tilesFolderFullPath, x) {
  if (any(grepl("cannot open this file as a SpatRaster", obj))) {
    fn <- file.path(tilesFolderFullPath, x)
    message("Tile ", fn, " appears to be corrupt; deleting and redownloading")
    unlink(fn)
    obj <- NULL
  }
  obj
}



crsFromLocalOrGDTiles <- function(targetObjCRS, dirTilesFolder, tilesFolderFullPath,
                                  urlTiles, targetFile, purge, doUploads, fileSize, verbose) {
  existing_tiles <- NULL
  for (i in 1:2) { # try local file, then googledrive, then back to local after googledrive download
    if (length(dirTilesFolder))  {
      targetObjCRS <- crsFromLocalTile(tilesFolderFullPath, dirTilesFolder)
      if (!is.null(targetObjCRS)) break
    }
    if (is.null(targetObjCRS) && is.null(existing_tiles)) {
      existing_tiles <- lsExistingTilesOnGoogleDrive(urlTiles, targetFile)
      if (!is.null(existing_tiles) && NROW(existing_tiles) > 0) {
        if (isTRUE(purge) && doUploads %in% TRUE) {
          existing_tiles <- purgeGoogleTiles(urlTiles, targetFile, verbose)
        } else {
          targetObjCRS <- crsFromGoogleDriveTile(tilesFolderFullPath, existing_tiles, fileSize, verbose = verbose)
        }
      }
    }
    if (is.null(targetObjCRS)) {
      dirTilesFolder <- dir(tilesFolderFullPath, recursive = TRUE, all.files = TRUE)
      if ( (is.null(existing_tiles) || NROW(existing_tiles) == 0) &&
           length(dirTilesFolder) == 0)
        break
    } else {
      break
    }
  }
  targetObjCRS
}

makeAndPlotTileGrid <- function(tileGrid, theArea, numTiles, targetObjCRS, plot.grid, to, verbose) {
  if (is.character(tileGrid)) {
    tg <- makeTileGridFromGADMcode(tileGrid, numTiles, crs = targetObjCRS) |> Cache(verbose = verbose - 1)
    tileGrid <- tg$tileGrid
    numTiles <- tg$numTiles
    theArea <- tg$area
  }
  if (missing(theArea)) {
    theArea <- terra::ext(tileGrid)
  }
  if (isTRUE(plot.grid)) {
    plotGridAndArea(tileGrid, theArea, to)
  }
  list(tileGrid = tileGrid, numTiles = numTiles)
}
