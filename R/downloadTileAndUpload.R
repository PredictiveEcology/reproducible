prepInputsWithTiles <- function(url, destinationPath, tilesFolder = "tiles", urlTiles,
                                to, doUploads = getOption("reproducible.prepInputsDoUploads", FALSE)) {

  if (missing(to) || missing(urlTiles)) {
    return("NULL")
  } else {
    numTiles2D <- c(10, 5)
    canadaGrid <- makeCanadaGrid(numTiles2D = numTiles2D)
    if (FALSE) {
      a <- terra::centroids(canadaGrid)
      terra::plot(canadaGrid)
      terra::text(a, labels = a$tile_id, col = "blue", cex = 1.2)
    }

    # Preview intersecting tile IDs

    file <- googledrive::drive_get(url) |> Cache()
    file_id <- file$id
    targetFile <- file$name
    dPath <- destinationPath# "~/testing/"
    targetFileFullPath <- file.path(dPath, targetFile)

    dig <- .robustDigest(to)

    # If the postprocessed final object is available; pull the plug, if not in dev mode
    targetFilePostProcessedFullPath <- .suffix(targetFileFullPath, dig)
    if (file.exists(targetFilePostProcessedFullPath) && doUploads %in% FALSE)
      return(terra::rast(targetFilePostProcessedFullPath))

    haveLocalFullFile <- file.exists(targetFileFullPath)

    if (fs::is_absolute_path(tilesFolder)) {
      tilesFolderFullPath <- file.path(tilesFolder, filePathSansExt(targetFile))
    } else {
      tilesFolderFullPath <- file.path(dPath, tilesFolder, filePathSansExt(targetFile))
    }

    noTiles <- FALSE

    # Find intersecting tiles
    all_tile_names <- makeTileNames(canadaGrid$tile_id)

    intersecting_tiles <- terra::intersect(canadaGrid, to)
    needed_tile_names <- makeTileNames(intersecting_tiles$tile_id)

    #
    # paddedTilenames <- sprintf("%02d", as.integer(intersecting_tiles$tile_id))
    # needed_tile_names <- paste0("tile_", paddedTilenames, ".tif")

    dd <- dir(tilesFolderFullPath, recursive = TRUE, all.files = TRUE)
    missingTilesLocal <- setdiff(needed_tile_names, dd)

    missingTilesLocalAll <- setdiff(all_tile_names, dd)
    tilesToGet <- intersect(needed_tile_names, dd)
    haveLocalTiles <- FALSE
    if (length(missingTilesLocal) == 0 && (length(missingTilesLocalAll) == 0 && doUploads %in% TRUE)) {
      message("✅ All needed tiles are available locally. Proceeding to load only those.")
      haveLocalTiles <- TRUE
    } else {
      message("⚠️ Some tiles are missing locally. Will try to download tiles from from url2.")
      print(missingTilesLocal)
    }
    # if (doUploads && !all(all_tile_names %in% dd)) {
    #   haveLocalTiles <- FALSE
    # }

    if (haveLocalTiles %in% FALSE || doUploads) {

      # Get the folder at url2 (replace with actual folder name or ID)
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

      available_tile_names <- existing_tiles$name

      # Determine which tiles are missing
      missingTilesOnRemote <- setdiff(needed_tile_names, available_tile_names)
      tilesToGet <- intersect(needed_tile_names, available_tile_names)

      haveRemoteTiles <- all(all_tile_names %in% existing_tiles$name)
      # Preview decision
      needUploads <- TRUE
      doTileDownload <- FALSE
      missingTilesRemoteAll <- setdiff(all_tile_names, existing_tiles$name)

      tilesFullOnRemote <- TRUE
      if (doUploads %in% TRUE) tilesFullOnRemote <- length(missingTilesRemoteAll) == 0

      if (length(missingTilesOnRemote) == 0 && tilesFullOnRemote) {
        message("✅ All needed tiles are available on Google Drive.  Proceeding to download only those.")
        needUploads <- FALSE
        doTileDownload <- haveLocalTiles %in% FALSE
      } else {
        message("⚠️ Some tiles are missing on Google Drive. Will download full file from url1.")
        print(missingTilesOnRemote)
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
      needDownloadFull <- haveLocalTiles %in% FALSE && haveRemoteTiles %in% FALSE
      if (any(fe)) {
        if (file.size(targetFileFullPath) < file$drive_resource[[1]]$size) {
          unlink(targetFileFullPath, force = TRUE)
        } else {
          needDownloadFull <- FALSE
        }
      }
      if (needDownloadFull) {
        message("Downloading full file (", targetFile,") from ", url)
        if (!is.null(file$drive_resource[[1]]$size))
          message()

        download_resumable_httr2(url, targetFileFullPath)
        rfull <- terra::rast(targetFileFullPath)
      }

      if (needUploads %in% TRUE || (doUploads %in% TRUE && haveRemoteTiles %in% FALSE)) {
        tile_raster_write_auto(targetFileFullPath, tilesFolderFullPath, nx = numTiles2D[[1]], ny = numTiles2D[[2]])
        upload_tiles_to_drive_url_parallel(tilesFolderFullPath, urlTiles, targetFileFullPath)
        tile_paths <- dir(tilesFolderFullPath, pattern = "\\.tif$")
        saExt <- terra::ext(to)

        # Filter tiles that intersect the study area
        intersecting_tiles2 <- purrr::keep(tile_paths, function(path) {
          tile_ext <- terra::ext(terra::rast(file.path(tilesFolderFullPath, path)))

          # Check for bounding box overlap
          !(tile_ext[1] > saExt[2] || tile_ext[2] < saExt[1] ||  # x overlap
              tile_ext[3] > saExt[4] || tile_ext[4] < saExt[3])    # y overlap
        })
        tile_rasters <- Map(x = intersecting_tiles2, function(x) terra::rast(file.path(tilesFolderFullPath, x))  )
      } # else {
      #   noTiles <- TRUE
      # }


    }
    if (haveLocalTiles %in% TRUE) {
      tile_rasters <- Map(x = tilesToGet, function(x) terra::rast(file.path(tilesFolderFullPath, x))  )
    }
    if (noTiles %in% FALSE) {
      mosaic_raster <- terra::sprc(tile_rasters)
      final <- terra::crop(mosaic_raster, to)
      rfull <- terra::writeRaster(terra::merge(final), filename = targetFilePostProcessedFullPath, overwrite = TRUE)
    }
    rfull
  }
}

library(terra)
library(parallel)
library(fs)

tile_raster_write_auto <- function(raster_path, out_dir, nx = 10, ny = 5) {
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
      tile_path <- file.path(out_dir, paste0("tile_", sprintf("%02d", tile_id), ".tif"))
      tile_specs[[tile_id]] <- list(ext = tile_ext, path = tile_path)
      tile_id <- tile_id + 1
    }
  }

  # Worker function
  process_tile <- function(spec) {
    if (!file.exists(spec$path)) {
      tile <- terra::crop(r, spec$ext)
      terra::writeRaster(tile, spec$path,
                         overwrite = FALSE,
                         gdal = c("COMPRESS=LZW", "TILED=YES"))
      return(paste("✅ Saved:", spec$path))
    } else {
      return(paste("⏩ Skipped (already exists):", spec$path))
    }
  }

  message("🧩 Creating tiles ...")

  # Choose parallel or sequential based on OS
  if (.Platform$OS.type == "unix") {
    results <- mclapply(tile_specs, process_tile, mc.cores = detectCores(logical = FALSE))
  } else {
    results <- lapply(tile_specs, process_tile)
  }

  # Print results
  for (msg in results) message(msg)
  message("🎉 Tiling complete.")
}

tile_raster_write_all <- function(raster_path, out_dir, nx = 10, ny = 5) {
  r <- terra::rast(raster_path)

  ext <- terra::ext(r)
  x_breaks <- seq(ext[1], ext[2], length.out = nx + 1)
  y_breaks <- seq(ext[3], ext[4], length.out = ny + 1)

  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  tile_id <- 1
  message("Creating tiles from full SpatRaster ... ")
  for (i in 1:nx) {
    for (j in 1:ny) {
      tile_ext <- terra::ext(x_breaks[i], x_breaks[i + 1], y_breaks[j], y_breaks[j + 1])
      tile_path <- file.path(out_dir, paste0("tile_", sprintf("%02d", tile_id), ".tif"))

      if (!file.exists(tile_path)) {
        tile <- terra::crop(r, tile_ext)
        terra::writeRaster(tile, tile_path,
                           # datatype = gdal_type,
                           overwrite = FALSE,
                           gdal = c("COMPRESS=LZW", "TILED=YES"))
        message("✅ Saved: ", tile_path)
      } else {
        message("⏩ Skipped (already exists): ", tile_path)
      }

      tile_id <- tile_id + 1
    }
  }

  message("🎉 Tiling complete.")
}

extract_drive_id <- function(url) {
  sub(".*folders/([a-zA-Z0-9_-]+).*", "\\1", url)
}

upload_tiles_to_drive_url <- function(local_dir, drive_folder_url, thisFilename) {
#   drive_auth()

  # Extract parent folder ID from URL
  parent_id <- extract_drive_id(drive_folder_url)

  # Create subfolder named after original raster filename
  subfolder_name <- basename(tools::file_path_sans_ext(thisFilename))
  subfolder <- googledrive::drive_find(q = paste0("name = '", subfolder_name, "' and '", parent_id, "' in parents"))

  if (nrow(subfolder) == 0) {
    subfolder <- googledrive::drive_mkdir(subfolder_name, path = googledrive::as_id(parent_id))
    message("📁 Created subfolder: ", subfolder_name)
  } else {
    message("📁 Found existing subfolder: ", subfolder_name)
  }

  # List local .tif files
  tif_files <- dir(local_dir, pattern = "\\.tif$", full.names = TRUE)

  # Upload each file if not already present
  existingAll <- googledrive::drive_ls(subfolder$id)
  for (file_path in tif_files) {
    file_name <- basename(file_path)
    # existing <- googledrive::drive_find(q = paste0("name = '", file_name, "' and '", subfolder$id, "' in parents"))

    existing <- file_name %in% existingAll$name
    if (existing %in% FALSE) {
      googledrive::drive_upload(file_path, path = googledrive::as_id(subfolder$id))
      message("✅ Uploaded: ", file_name)
    } else {
      message("⏩ Skipped (already exists): ", file_name)
    }
  }

  message("🎉 Upload complete.")
}


upload_tiles_to_drive_url_parallel <- function(local_dir, drive_folder_url, thisFilename) {
  # Extract parent folder ID from URL
  parent_id <- extract_drive_id(drive_folder_url)

  # Create subfolder named after original raster filename
  subfolder_name <- basename(tools::file_path_sans_ext(thisFilename))
  subfolder <- googledrive::drive_find(q = paste0("name = '", subfolder_name, "' and '", parent_id, "' in parents"))

  if (nrow(subfolder) == 0) {
    subfolder <- googledrive::drive_mkdir(subfolder_name, path = googledrive::as_id(parent_id))
    message("📁 Created subfolder: ", subfolder_name)
  } else {
    message("📁 Found existing subfolder: ", subfolder_name)
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
  for (msg in results) message(msg)
  message("🎉 Upload complete.")
}
if (FALSE) {

  url <- "https://drive.google.com/file/d/1fmdDfOstKNRSyV5-tw3thw_dBFK2lYQS/view?usp=drive_link"
  saOrig <- terra::as.polygons(terra::ext(c(xmin = -2166196, xmax = -1903177,
                                            ymin = 7204892, ymax = 7437562)),
                               crs = proj4stringSCANFI)
  dPath <- "~/testing"
  urlTiles <- "https://drive.google.com/drive/folders/1IfeQ9rZ3-RIQwtcdo2T5Kn51NJJRWeox?usp=drive_link"

  piOrig <- prepInputs(url = url, destinationPath = dPath, to = saOrig, urlTiles = urlTiles)
}

makeCanadaGrid <- function(numTiles2D = c(10, 5), crs) {
  if (missing(crs)) crs <- proj4stringSCANFI

  canadaFullExt <- terra::ext(c(xmin = -2341500, xmax = 3010500, ymin = 5863500, ymax = 9436500))
  canadaV <- terra::as.polygons(canadaFullExt, crs = crs)
  canadaGrid <- sf::st_make_grid(sf::st_as_sfc(sf::st_as_sf(canadaV)), n = numTiles2D) |>
    terra::vect()
  m <- t(matrix(1:50, nrow = numTiles2D[[2]], byrow = F))
  canadaGrid[["tile_id"]] <- as.character(m)
  canadaGrid
}

proj4stringSCANFI <- "+proj=lcc +lat_0=0 +lon_0=-95 +lat_1=49 +lat_2=77 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

makeTileNames <- function(tileIds) {
  paddedTilenames <- sprintf("%02d", as.integer(tileIds))
  paste0("tile_", paddedTilenames, ".tif")
}

rastTiles <- function(tiles, tilesFolderFullPath) {
  tile_rasters <- Map(x = tiles, function(x) terra::rast(file.path(tilesFolderFullPath, x))  )
}
