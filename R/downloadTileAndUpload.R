#' @param tileGrid Either length 3 character string, such as "CAN", to be sent to `geodata::gadm(...)`
#'   or an actual `SpatVector` object with a grid of polygons
prepInputsWithTiles <- function(targetFile, url, destinationPath, tilesFolder = "tiles", urlTiles,
                                to, doUploads = getOption("reproducible.prepInputsDoUploads", FALSE),
                                tileGrid = "CAN", numTiles = NULL, plot.grid = FALSE,
                                verbose = getOption("reproducible.verbose")) {

  if (missing(to) || missing(urlTiles)) {
    messagePreProcess(
      "prepInputsWithTiles must have `to` argument specified with a spatial object",
      verbose = verbose)
    return("NULL")
  } else {
    # Preview intersecting tile IDs
    urlAsID <- extract_drive_id(url)
    if (identical(as.character(urlAsID), as.character(url))) {
      url <- googledriveIDtoHumanURL(url)
    }

    if (missing(targetFile)) {
      file <- googledrive::drive_get(url) |>
        Cache(verbose = FALSE, notOlderThan = Sys.time() - 60*60) # refresh every hour
      file_id <- file$id
      targetFile <- file$name
      dPath <- destinationPath# "~/testing/"
    }
    dig <- .robustDigest(to)

    # If the postprocessed final object is available; pull the plug, if not in dev mode
    targetFileFullPath <- file.path(dPath, targetFile)
    targetFilePostProcessedFullPath <- .suffix(targetFileFullPath, dig)
    if (file.exists(targetFilePostProcessedFullPath) && doUploads %in% FALSE) {
      message("Correct post processed file exists; returning it now...")
      return(terra::rast(targetFilePostProcessedFullPath))
    }
    if (fs::is_absolute_path(tilesFolder)) {
      tilesFolderFullPath <- file.path(tilesFolder, filePathSansExt(targetFile))
    } else {
      tilesFolderFullPath <- file.path(dPath, tilesFolder, filePathSansExt(targetFile))
    }
    # haveLocalFullFile <- file.exists(targetFileFullPath)

    targetObjCRS <- NULL # don't know it yet
    if (file.exists(targetFileFullPath)) {
      targetObj <- try(terra::rast(targetFileFullPath))
      if (is(targetObj, "try-error")) {
        unlink(targetFileFullPath, force = TRUE)
        message("File appears to be corrupt; deleting it and trying local tiles, then remotes")
      } else {
        targetObjCRS <- terra::crs(targetObj)
      }
    }
    # need to get the targetObjCRS to know what the tiles will look like
    dd <- dir(tilesFolderFullPath, recursive = TRUE, all.files = TRUE)
    if (is.null(targetObjCRS)) {
      for (i in 1:2) {
        if (length(dd))  {
          singleTile <- terra::rast(file.path(tilesFolderFullPath, dd[1]))
          targetObjCRS <- terra::crs(singleTile)
          break
        }
        if (is.null(targetObjCRS)) {
          existing_tiles <- lsExistingTilesOnGoogleDrive(urlTiles, targetFile)
          if (!is.null(existing_tiles)) {
            ogwd <- getwd()
            if (dir.exists(tilesFolderFullPath) %in% FALSE)
              dir.create(tilesFolderFullPath, recursive = TRUE, showWarnings = FALSE)
            setwd(tilesFolderFullPath)
            on.exit(setwd(ogwd))
            download_resumable_httr2(existing_tiles$id[1], existing_tiles$name[1])
            singleTile <- terra::rast(file.path(tilesFolderFullPath, existing_tiles$name[1]))
            targetObjCRS <- terra::crs(singleTile)
            setwd(ogwd)

          }
        }
      }
    }
    if (is.null(targetObjCRS)) {
      # still doesn't have it
        messagePreProcess("Downloading full file (", targetFile,") from\n", url, verbose = verbose)
        fileSize <- file$drive_resource[[1]]$size
        if (!is.null(fileSize))
          messageAboutFilesize(fileSize, verbose = verbose)

        download_resumable_httr2(url, targetFileFullPath)
        rfull <- terra::rast(targetFileFullPath)
        targetObjCRS <- terra::crs(rfull)
        # }

    }

    noTiles <- FALSE

    if (is.character(tileGrid)) {
      tg <- makeTileGridFromGADMcode(tileGrid, numTiles, crs = targetObjCRS) |> Cache(verbose = verbose - 1)
      tileGrid <- tg$tileGrid
      numTiles <- tg$numTiles
      theArea <- tg$area
    }
    if (isTRUE(plot.grid)) {
      a <- terra::centroids(tileGrid)
      terra::plot(tileGrid)
      terra::text(a, labels = a$tile_id, col = "blue", cex = 1.2)
      tilePolyTG <- terra::project(theArea, tileGrid)
      terra::plot(tilePolyTG, add = TRUE)
      terra::plot(to, add = TRUE, col = "red")
    }
    # Find intersecting tiles
    all_tile_names <- sort(makeTileNames(tileGrid$tile_id))


    to_inTileGrid <- postProcessTo(to, to = targetObjCRS, verbose = -2)
    intersecting_tiles <- terra::intersect(tileGrid, terra::ext(to_inTileGrid))
    needed_tile_names <- makeTileNames(intersecting_tiles$tile_id)
    needed_tile_names <- sort(needed_tile_names)

    missingTilesLocal <- setdiff(needed_tile_names, dd)

    missingTilesLocalAll <- setdiff(all_tile_names, dd)
    tilesToGet <- intersect(needed_tile_names, dd)
    haveLocalTiles <- FALSE
    messagePreProcess("Need to load/get these tiles:\n", verbose = verbose)
    messagePreProcess(paste(needed_tile_names, collapse =  ", "), verbose = verbose)
    haveAllNeededTiles <- if (doUploads %in% TRUE) length(missingTilesLocalAll) == 0 else TRUE


    if (length(missingTilesLocal) == 0 && (haveAllNeededTiles)) {
      messagePreProcess(
        "✅ All needed tiles are available locally. Proceeding to load only those.",
        verbose = verbose)
      haveLocalTiles <- TRUE
    } else {
      messagePreProcess(
        "⚠️ Tiles are missing locally. Will try to download these:\n",
        verbose = verbose)
      messagePreProcess(paste(missingTilesLocal, collapse = ", "), verbose = verbose)
      messagePreProcess(paste0("... from urlTiles (",urlTiles,")"), verbose = verbose)
    }

    if (haveLocalTiles %in% FALSE || doUploads) {
      existing_tiles <- lsExistingTilesOnGoogleDrive(urlTiles, targetFile)

      available_tile_names_onGoogleDrive <- existing_tiles$name

      # Determine which tiles are missing
      missingTilesOnRemote <- setdiff(needed_tile_names, available_tile_names_onGoogleDrive)
      tilesToGet <- intersect(needed_tile_names, available_tile_names_onGoogleDrive)

      haveRemoteTiles <- all(all_tile_names %in% existing_tiles$name)
      # Preview decision
      needUploads <- TRUE
      doTileDownload <- FALSE
      missingTilesRemoteAll <- setdiff(all_tile_names, existing_tiles$name)

      tilesFullOnRemote <- TRUE
      if (doUploads %in% TRUE) tilesFullOnRemote <- length(missingTilesRemoteAll) == 0

      if (length(missingTilesOnRemote) == 0 && tilesFullOnRemote) {
        doTileDownload <- haveLocalTiles %in% FALSE
        messagePreProcess("✅ All needed tiles are available on Google Drive.  ",
                          verbose = verbose)
        needUploads <- FALSE
        if (doTileDownload) {
          messagePreProcess("Proceeding to download only the needed tiles...", verbose = verbose)
        } else {
          messagePreProcess("Nothing to download", verbose = verbose)
        }


      } else {
        messagePreProcess("⚠️ Some tiles are missing on Google Drive:")

        missingOnes <- if (doUploads) missingTilesRemoteAll else missingTilesOnRemote

        message(paste(missingOnes, collapse = ", "), verbose = verbose)
      }

      # if (haveLocal %in% FALSE && needUploads %in% FALSE) {
      if (haveLocalTiles %in% FALSE && doTileDownload %in% TRUE) {
        whGet <- match(tilesToGet, existing_tiles$name)
        tileIDSToGet <- existing_tiles[whGet, ]
        ogwd <- getwd()
        if (dir.exists(tilesFolderFullPath) %in% FALSE)
          dir.create(tilesFolderFullPath, recursive = TRUE, showWarnings = FALSE)
        setwd(tilesFolderFullPath)
        on.exit(setwd(ogwd))
        by(tileIDSToGet, seq_len(NROW(tileIDSToGet)), function(i) {
          download_resumable_httr2(i$id, i$name)
        })
        haveLocalTiles <- TRUE
        setwd(ogwd)
      }

      fe <- file.exists(targetFileFullPath)

      if (needUploads %in% TRUE || (doUploads %in% TRUE && haveRemoteTiles %in% FALSE)) {
        if (haveLocalTiles %in% FALSE)
          tile_raster_write_auto(targetFileFullPath, tilesFolderFullPath, tileGrid,
                                 all_tile_names = all_tile_names,
                                 nx = numTiles[[1]], ny = numTiles[[2]],
                                 verbose = verbose)
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
          browser() # the intersecting_tiles2 from the newly created need to be the same as the
          # expected from the grid
        }
        # tile_rasters <- rastTiles(needed_tile_names, tilesFolderFullPath)
      }
    }
    tile_rasters <- rastTiles(needed_tile_names, tilesFolderFullPath)
    if (noTiles %in% FALSE) {
      mosaic_raster <- terra::sprc(tile_rasters)
      final <- terra::crop(mosaic_raster, to_inTileGrid)
      rfull <- terra::writeRaster(terra::merge(final), filename = targetFilePostProcessedFullPath,
                                  overwrite = TRUE)
      if (exists("file", inherits = FALSE)) {
        fileSize <- file$drive_resource[[1]]$size
        messageAboutFilesize(fileSize, verbose = verbose)
        fsLocal <- file.size(targetFilePostProcessedFullPath)
        dd1 <- dir(tilesFolderFullPath)
        dd2 <- dir(tilesFolderFullPath, full.names = TRUE)
        tilesUsed <- dd2[match(needed_tile_names, dd1)]
        messageAboutFilesize(file.size(tilesUsed), verbose = verbose, msgMiddle = " on local drive using tiles ")

      }
    }
    rfull
  }
}

library(terra)
library(parallel)
library(fs)

tile_raster_write_auto <- function(raster_path, out_dir, tileGrid, all_tile_names, nx = 10, ny = 5,
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
        terra::writeRaster(tile, spec$path,
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
    results <- mclapply(tile_specs, process_tile, mc.cores = detectCores(logical = FALSE))
  } else {
    results <- lapply(tile_specs, process_tile)
  }

  # Print results
  for (msg in results[!sapply(results, is.null)]) messagePreProcess(msg, verbose = verbose)
  messagePreProcess("🎉 Tiling complete.", verbose = verbose)
}

# tile_raster_write_all <- function(raster_path, out_dir, nx = 10, ny = 5,
#                                   verbose = getOption("reproducible.verbose")) {
#   r <- terra::rast(raster_path)
#
#   ext <- terra::ext(r)
#   x_breaks <- seq(ext[1], ext[2], length.out = nx + 1)
#   y_breaks <- seq(ext[3], ext[4], length.out = ny + 1)
#
#   dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
#
#   tile_id <- 1
#   messagePreProcess("Creating tiles from full SpatRaster ... ", verbose = verbose)
#   for (i in 1:nx) {
#     for (j in 1:ny) {
#       tile_ext <- terra::ext(x_breaks[i], x_breaks[i + 1], y_breaks[j], y_breaks[j + 1])
#       tile_path <- file.path(out_dir, paste0("tile_", sprintf("%02d", tile_id), ".tif"))
#
#       if (!file.exists(tile_path)) {
#         tile <- terra::crop(r, tile_ext)
#         terra::writeRaster(tile, tile_path,
#                            # datatype = gdal_type,
#                            overwrite = FALSE,
#                            gdal = c("COMPRESS=LZW", "TILED=YES"))
#         messagePreProcess("✅ Saved: ", tile_path, verbose = verbose)
#       } else {
#         messagePreProcess("⏩ Skipped (already exists): ", tile_path, verbose = verbose)
#       }
#
#       tile_id <- tile_id + 1
#     }
#   }
#
#   messagePreProcess("🎉 Tiling complete.", verbose = verbose)
# }

extract_drive_id <- function(url) {
  sub(".*folders/([a-zA-Z0-9_-]+).*", "\\1", url)
}

# upload_tiles_to_drive_url <- function(local_dir, drive_folder_url, thisFilename,
#                                       verbose = getOption("reproducible.verbose")) {
# #   drive_auth()
#
#   # Extract parent folder ID from URL
#   parent_id <- extract_drive_id(drive_folder_url)
#
#   # Create subfolder named after original raster filename
#   subfolder_name <- basename(tools::file_path_sans_ext(thisFilename))
#   subfolder <- googledrive::drive_find(q = paste0("name = '", subfolder_name, "' and '", parent_id, "' in parents"))
#
#   if (nrow(subfolder) == 0) {
#     subfolder <- googledrive::drive_mkdir(subfolder_name, path = googledrive::as_id(parent_id))
#     messagePreProcess("📁 Created subfolder: ", subfolder_name, verbose = verbose)
#   } else {
#     messagePreProcess("📁 Found existing subfolder: ", subfolder_name, verbose = verbose)
#   }
#
#   # List local .tif files
#   tif_files <- dir(local_dir, pattern = "\\.tif$", full.names = TRUE)
#
#   # Upload each file if not already present
#   existingAll <- googledrive::drive_ls(subfolder$id)
#   for (file_path in tif_files) {
#     file_name <- basename(file_path)
#     # existing <- googledrive::drive_find(q = paste0("name = '", file_name, "' and '", subfolder$id, "' in parents"))
#
#     existing <- file_name %in% existingAll$name
#     if (existing %in% FALSE) {
#       googledrive::drive_upload(file_path, path = googledrive::as_id(subfolder$id))
#       messagePreProcess("✅ Uploaded: ", file_name, verbose = verbose)
#     } else {
#       messagePreProcess("⏩ Skipped (already exists): ", file_name, verbose = verbose)
#     }
#   }
#
#   messagePreProcess("🎉 Upload complete.", verbose = verbose)
# }


upload_tiles_to_drive_url_parallel <- function(local_dir, drive_folder_url, thisFilename,
                                               verbose = getOption("reproducible.verbose")) {
  # Extract parent folder ID from URL
  parent_id <- extract_drive_id(drive_folder_url)

  # Create subfolder named after original raster filename
  subfolder_name <- basename(tools::file_path_sans_ext(thisFilename))
  subfolder <- googledrive::drive_find(q = paste0("name = '", subfolder_name, "' and '", parent_id, "' in parents"))

  if (nrow(subfolder) == 0) {
    subfolder <- googledrive::drive_mkdir(subfolder_name, path = googledrive::as_id(parent_id))
    messagePreProcess("📁 Created subfolder: ", subfolder_name, verbose = verbose)
  } else {
    messagePreProcess("📁 Found existing subfolder: ", subfolder_name, verbose = verbose)
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
    results <- parallel::mclapply(tif_files, upload_one,
                                  mc.cores = min(3, parallel::detectCores(logical = FALSE)))
  } else {
    results <- lapply(tif_files, upload_one)
  }

  # Print results
  for (msg in results) messagePreProcess(msg, verbose = verbose)
  messagePreProcess("🎉 Upload complete.", verbose = verbose)
}

# makeCanadaGrid <- function(numTiles = c(10, 5), crs) {
#   if (missing(crs)) crs <- proj4stringSCANFI
#
#   canadaFullExt <- terra::ext(c(xmin = -2341500, xmax = 3010500, ymin = 5863500, ymax = 9436500))
#   areaV <- terra::as.polygons(canadaFullExt, crs = crs)
#   areaGrid <- sf::st_make_grid(sf::st_as_sfc(sf::st_as_sf(areaV)), n = numTiles) |>
#     terra::vect()
#   m <- t(matrix(1:50, nrow = numTiles[[2]], byrow = F))
#   areaGrid[["tile_id"]] <- as.character(m)
#   list(canada = areaV, areaGrid = areaGrid)
# }

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
SCANFIextInLatLong <- terra::ext(c(xmin = -153.178769014699, xmax = -29.4065490445664,
                                   ymin = 35.8597256039508,
                                   ymax = 70))

makeTileNames <- function(tileIds) {
  paddedTileNumbers <- makePaddedNamesForTiles(tileIds)
  paste0("tile_", paddedTileNumbers, ".tif")
}

rastTiles <- function(tiles, tilesFolderFullPath) {
  tile_rasters <- Map(x = tiles, function(x) terra::rast(file.path(tilesFolderFullPath, x))  )
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

build_albers_proj4 <- function(ext_obj) {
  if (!inherits(ext_obj, "SpatExtent")) stop("Input must be a terra::ext object")

  # Extract lat/lon bounds
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
  if (is.null(g) || ("NULL" == g)) {
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
