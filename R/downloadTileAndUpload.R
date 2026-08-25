#' Alternative to `prepInputs` that can use Spatial Tiles stored locally or on Google Drive
#'
#' @description
#' `r lifecycle::badge("experimental")`
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
#'   or an actual `SpatVector` object with a grid of polygons.
#'
#'   When a character code is used, the GADM boundaries must be downloaded, and
#'   `geodata` requires somewhere to put them. That location is resolved in this
#'   order, preferring somewhere persistent so the download happens only once:
#'   \enumerate{
#'     \item `geodata::geodata_path()`, if the user has configured one;
#'     \item `getOption("reproducible.destinationPathShared")` (or its
#'       deprecated alias `reproducible.inputPaths`), the option intended for
#'       large files reused across projects;
#'     \item `getOption("reproducible.inputPath")`, the package default, which
#'       is under `tempdir()` and so is re-downloaded each session.
#'   }
#'   If the download cannot be completed -- no path, server outage, no network --
#'   a warning is issued and a fixed Canada-wide extent is used instead, which
#'   may not match the area of interest.
#' @param numTiles Integer. Number of tiles to generate. Optional.
#' @param plot.grid Logical. Whether to plot the tile grid and area of interest. Default is `FALSE`.
#' @param purge Logical or Integer. `0/FALSE` (default) keeps existing `CHECKSUMS.txt` file and
#'   `prepInputs` will write or append to it. `1/TRUE` will deleted the entire `CHECKSUMS.txt` file.
#' @param verbose Logical or numeric. Controls verbosity of messages. Default is `getOption("reproducible.verbose")`.
#' @param ... Either `maskTo`, `cropTo` (which will be used if `to` is not supplied, or
#'   arguments passed to `writeRaster`, e.g., `datatype` (used when writing tiles).
#'
#' @return A single, merged `SpatRaster` object `postProcess`ed to the area of interest (`to`),
#' composed of the necessary tiles.
#' If the post-processed file already exists locally, it will be returned directly.
#'
#' @details
#' This function can be triggered *inside* `prepInputs`
#' if the `to` is supplied and both `url` and `urlTiles` are supplied. **NOTE**:
#' `urlTiles` can be supplied using the
#' `option(reproducible.prepInputsUrlTiles = someGoogleDriveFolderURL`), so the original
#' `prepInputs` function call can remain unaffected.
#'
#' This function also uses a different checksumming procedure compared to the normal `prepInputs`.
#' This function will assess the remote url for a hash. If that hash exists, then
#' it will compare it to a local file with `targetFile` name, suffixed with `.hash`. If the
#' two hashes differ (remote and local), then it will be redownloaded; otherwise the local
#' one will be returned.
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
#'
#' if (FALSE) {
#'   to <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(-123.3656, 48.4284)), crs = 4326))
#'   result <- prepInputsWithTiles(
#'     url = "https://example.com/data.tif",
#'     destinationPath = tempdir(),
#'     to = to,
#'     urlTiles = "https://example.com/tiles/",
#'     tileGrid = "CAN"
#'   )
#' }
#'
#' @export
prepInputsWithTiles <- function(targetFile, url, destinationPath,
                                to,
                                tilesFolder = file.path(getOption("reproducible.inputPath"), "tiles"),
                                urlTiles = getOption("reproducible.prepInputsUrlTiles", NULL),
                                doUploads = getOption("reproducible.prepInputsDoUploads", FALSE),
                                tileGrid = "CAN",
                                numTiles = NULL,
                                plot.grid = FALSE,
                                purge = FALSE,
                                verbose = getOption("reproducible.verbose"), ...) {

  st <- Sys.time()

  env <- environment()

  # deal with `to` first, to identify the tiles, then rest can be Cached easily, even
  #  if the to changes slightly
  maskToCropTo <- c("maskTo", "cropTo")
  whMaskToCropTo <- match(maskToCropTo, ...names())
  if ( (missing(to) && anyNA(whMaskToCropTo)) || is.null(urlTiles)) {
    messagePreProcess(
      "prepInputsWithTiles must have `urlTiles` and `url` plus a `to`, `cropTo` or ",
      "`maskTo` spatial object",
      verbose = verbose)
    return("NULL")
  }
  if (missing(to)) # take the first of maskTo or cropTo, which are in the ...
    if (!anyNA(whMaskToCropTo)) to <- ...elt(whMaskToCropTo[1])
  if (.isSpatRaster(to))
    to <- boundaryPolygon(to)
  dig <- .robustDigest(to)

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
  remoteHashFile <- makeRemoteHashFile(url, destinationPath,
                                       remoteMetadata$targetFile, remoteMetadata$remoteHash)
  if (!is.null(.isArchive(remoteMetadata$targetFile)))  {
    messagePreProcess(
      "prepInputsWithTiles does not work with archives yet",
      verbose = verbose)
    return("NULL")
  }

  if (is.null(remoteMetadata$targetFile)) {
    stop("Please supply `targetFile` or a url from which `targetFile` can be extracted from")
  }

  targetFileFullPath <- file.path(destinationPath, remoteMetadata$targetFile)
  purge <- checkHaveCorrectHashedVersion(targetFileFullPath, remoteHashFile, remoteMetadata, purge, verbose)
  messagePreProcess("Preparing ", .messageFunctionFn(targetFileFullPath), verbose = verbose)
  targetFilePostProcessedFullPath <- .suffix(targetFileFullPath, dig)

  if (isTRUE(purge)) {
    purgeLocals(targetFilePostProcessedFullPath, targetFileFullPath, remoteHashFile, verbose)
  }

  if (file.exists(targetFilePostProcessedFullPath) && doUploads < 1) {
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
                               destinationPath = destinationPath,
                           url, urlTiles, remoteMetadata$fileSize, remoteMetadata$remoteHash,
                           remoteAlgorithm = remoteMetadata$remoteAlgorithm,
                           purge, doUploads, verbose)
  # need to rerun because there may have been a rm in previous line
  dirTilesFolder <- dir(tilesFolderFullPath, recursive = TRUE, all.files = TRUE)

  noTiles <- FALSE

  to_inTileGrid <- postProcessTo(to, to = targetObjCRS, verbose = verbose - 2)
  tileGridAndArea <- makeAndPlotTileGrid(tileGrid, numTiles, targetObjCRS,
                                         plot.grid, to = to_inTileGrid, verbose)

  # Find intersecting tiles
  all_tile_names <- sort(makeTileNames(tileGridAndArea$tileGrid$tile_id))

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
  haveAllNeededTiles <- if (doUploads > 0) length(missingTilesLocalAll) == 0 else TRUE

  if (length(missingTilesLocal) == 0) {# && (haveAllNeededTiles)) {
    messagePreProcess(
      "All needed tiles are available locally. Proceeding to load them",
      verbose = verbose)
    haveLocalTiles <- TRUE
  } else {
    messagePreProcess(
      "Tiles are missing locally in:\n",.messageFunctionFn(tilesFolderFullPath),
      "\nWill try to download these:\n", verbose = verbose)
    messagePreProcess(.messageFunctionFn(paste(missingTilesLocal, collapse = ", ")), verbose = verbose)
    messagePreProcess(paste0("... from urlTiles (",.messageFunctionFn(urlTiles),")"), verbose = verbose)
  }

  for (ii in 1:2) { # try twice in case a local tile is corrupt; if yes, delete it, redownload, reload
    if (haveLocalTiles %in% FALSE || doUploads > 0) {
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
## mclapply() signals a dead worker by returning a try-error for EVERY value
## that worker was given, so a single failure loses a whole slice of the work.
## Callers that do not inspect the results end up silently missing tiles or
## uploads. Retry the failed slice serially, and fail loudly if that also fails.
.retryFailedSerially <- function(results, items, FUN, what, verbose, ...) {
  failed <- vapply(results, function(x) inherits(x, c("try-error", "error")), logical(1))
  if (!any(failed)) {
    return(results)
  }
  messagePreProcess(.message$forkChildFailed(sum(failed), what), verbose = verbose)
  results[failed] <- lapply(items[failed], function(i) {
    tryCatch(FUN(i, ...), error = function(e) e)
  })
  stillFailed <- vapply(results, function(x) inherits(x, c("try-error", "error")), logical(1))
  if (any(stillFailed)) {
    stop(.message$forkChildFailedHard(sum(stillFailed), what), call. = FALSE)
  }
  results
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
  process_tile <- function(spec, datatype) {
    if (!file.exists(spec$path)) {
      tile <- terra::crop(r, spec$ext)
      # isAllNA <- terra::allNA(tile)[1] %in% TRUE
      # if (isAllNA %in% FALSE) {

        ## NUM_THREADS=1 is load-bearing, not a tuning knob. mclapply() forks,
        ## and fork() carries only the calling thread into the child; a child
        ## that then asks GDAL for its own worker pool deadlocks on mutex state
        ## inherited from the parent's pool, and hangs forever. Measured: with a
        ## default write in the child this deadlocks whenever the parent has
        ## ever done a plain terra::writeRaster() (which allocates a pool that
        ## is never released); with NUM_THREADS=1 it completes. The parent
        ## having a pool is harmless on its own -- it is the child creating one
        ## that hangs -- so this one option is the whole fix, and it holds
        ## regardless of what the caller did beforehand or which terra they run.
        ## `datatype = NULL` together with `gdal =` throws Rcpp::not_compatible
        ## in terra (>= 1.9.34, still in 1.9.46), so drop the NULL rather than
        ## pass it -- `datatype` here defaults to NULL.
        wrArgs <- list(tile, spec$path, overwrite = FALSE,
                       gdal = c("COMPRESS=LZW", "TILED=YES", "NUM_THREADS=1"))
        if (!is.null(datatype)) wrArgs$datatype <- datatype
        do.call(terra::writeRaster, wrArgs)
        return(paste("Saved:", spec$path))
      # }
    } else {
      return(paste("Skipped (already exists):", spec$path))
    }
  }

  messagePreProcess("Creating tiles ...", verbose = verbose)

  # Choose parallel or sequential based on OS
  if (isUnix() && requireNamespace("parallel")) {
    numCoresToUse <- .parallelCores(maxN = length(tile_specs))
    results <- parallel::mclapply(
      tile_specs, process_tile,
      mc.cores = numCoresToUse, datatype = datatype)
    results <- .retryFailedSerially(results, tile_specs, process_tile,
                                    what = "tiles", verbose = verbose,
                                    datatype = datatype)
  } else {
    results <- lapply(tile_specs, process_tile, datatype = datatype)
  }

  # Print results
  for (msg in results[!sapply(results, is.null)]) messagePreProcess(msg, verbose = verbose)
  messagePreProcess("Tiling complete.", verbose = verbose)
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
  stopifnot(requireNamespace("googledrive", quietly = TRUE))

  # Extract parent folder ID from URL
  parent_id <- extract_drive_id(drive_folder_url)

  # Create subfolder named after original raster filename
  subfolder_name <- basename(tools::file_path_sans_ext(thisFilename))
  subfolder <- googledrive::with_drive_quiet(
    googledrive::drive_find(q = paste0("name = '", subfolder_name, "' and '", parent_id, "' in parents")))

  if (nrow(subfolder) == 0) {
    subfolder <- googledrive::with_drive_quiet(googledrive::drive_mkdir(subfolder_name,
                                                                        path = googledrive::as_id(parent_id)))
    messagePreProcess("Created subfolder: ", .messageFunctionFn(subfolder_name), verbose = verbose)
  } else {
    messagePreProcess("Found existing subfolder: ", .messageFunctionFn(subfolder_name), verbose = verbose)
  }

  # List local .tif files
  tif_files <- dir(local_dir, pattern = "\\.tif$", full.names = TRUE)

  # Get existing files in Drive subfolder
  existingAll <- googledrive::with_drive_quiet(googledrive::drive_ls(subfolder$id))
  existing_names <- existingAll$name

  # Upload helper
  upload_one <- function(file_path) {
    file_name <- basename(file_path)
    if (!(file_name %in% existing_names)) {
      googledrive::drive_upload(file_path, path = googledrive::as_id(subfolder$id))
      return(paste("Uploaded:", file_name))
    } else {
      return(paste("Skipped (already exists):", file_name))
    }
  }

  # Upload in parallel on Linux/macOS, sequential on Windows
  if (isUnix() && requireNamespace("parallel")) {
    ## network-bound, so NOT core-derived and not capped by `mc.cores`: more than
    ## ~7 concurrent uploads tends to be slower, depending on connection speed
    numCoresToUse <- .parallelUpload()
    results <- parallel::mclapply(
      tif_files, upload_one,
      mc.cores = numCoresToUse)
    results <- .retryFailedSerially(results, tif_files, upload_one,
                                    what = "uploads", verbose = verbose)
  } else {
    results <- lapply(tif_files, upload_one)
  }

  # Print results
  for (msg in results) messagePreProcess(msg, verbose = verbose)
  messagePreProcess("Upload complete.", verbose = verbose)
}

makeTileGrid <- function(ext, crs, numTiles) {
  stopifnot(
    requireNamespace("sf", quietly = TRUE),
    requireNamespace("terra", quietly = TRUE)
  )

  if (missing(crs)) crs <- proj4stringSCANFI

  # ext <- terra::ext(c(xmin = -2341500, xmax = 3010500, ymin = 5863500, ymax = 9436500))
  areaV <- terra::as.polygons(ext, crs = crs)
  areaGrid <- sf::st_make_grid(sf::st_as_sfc(sf::st_as_sf(areaV)), n = numTiles) |>
    terra::vect()
  m <- t(matrix(seq(prod(numTiles)), nrow = numTiles[[2]], byrow = FALSE))
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


## `geodata::gadm()` has no default for `path`: it falls back to geodata_path(),
## which errors ("you need to provide a path, or set a default path") on any
## machine where the user has not configured one -- CI, and a new user. Prefer
## their geodata config if they have one, then reproducible's shared-downloads
## location (the option meant for exactly this: large files reused across
## projects), and only then the package's own input default, which is under
## tempdir() and so is safe to write to without asking.
.gadmPath <- function() {
  if (.requireNamespace("geodata")) {
    p <- tryCatch(geodata::geodata_path(), error = function(e) "")
    if (length(p) && nzchar(p[1])) {
      return(p[1])
    }
  }
  shared <- .getDestinationPathShared()
  base <- if (length(shared) && nzchar(shared[1])) {
    shared[1]
  } else {
    getOption("reproducible.inputPath", file.path(tempdir(), "reproducible", "input"))
  }
  checkPath(file.path(base, "geodata"), create = TRUE)
}

makeTileGridFromGADMcode <- function(tileGrid, numTiles = NULL, crs) {
  ## an error here (no path, server down, network) routes into the same fallback
  ## as a NULL return, just below
  gadmErr <- NULL
  ## resolving the location can itself fail (e.g. an unwritable directory), so
  ## it is inside the same fallback
  gadmPath <- tryCatch(.gadmPath(), error = function(e) {
    gadmErr <<- conditionMessage(e)
    NULL
  })
  g <- if (is.null(gadmPath)) {
    NULL
  } else {
    tryCatch(geodata::gadm(tileGrid, resolution = 2, path = gadmPath) |> Cache(),
             error = function(e) {
               gadmErr <<- conditionMessage(e)
               NULL
             })
  }
  if (is.null(g) || (is.character(g) && isTRUE(g == "NULL"))) {
    ## geodata unavailable: no path configured, server down, or no network. Say
    ## so -- the fallback silently changes which area gets tiled.
    warning(.message$gadmFallback(
      tileGrid,
      if (is.null(gadmPath)) "<no download location could be resolved>" else gadmPath,
      gadmErr
    ), call. = FALSE)
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
  existing_tiles <- googledrive::with_drive_quiet(googledrive::drive_ls(tile_folder_onGoogleDrive))
  whFolders <- sapply(seq(NROW(existing_tiles)), function(x)
    isGoogleDriveDirectoryFromTibble(existing_tiles[x,]))
  hasSubfolder <- grep(filePathSansExt(targetFile), existing_tiles$name[whFolders])
  if (length(hasSubfolder)) {
    tile_subfolder <- existing_tiles[hasSubfolder, ]$id
    existing_tiles <- googledrive::with_drive_quiet(googledrive::drive_ls(tile_subfolder))
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
  download_resumable_httr2(existing_tiles$id[1], existing_tiles$name[1],
                           gdriveDetails = existing_tiles[1, ], fileSize, verbose = verbose - 1)
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
                         targetFile, destinationPath,
                         url, urlTiles, fileSize, remoteHash,
                         remoteAlgorithm = .classifyRemoteHashAlgo(remoteHash),
                         purge, doUploads, verbose) {

  targetObjCRS <- NULL # don't know it yet
  if (file.exists(targetFileFullPath)) {
    targetObjCRS <- crsFromLocalFile(targetFileFullPath, targetObjCRS)
  }
  # need to get the targetObjCRS to know what the tiles will look like
  if (is.null(targetObjCRS)) {
    targetObjCRS <- crsFromLocalOrGDTiles(targetObjCRS, dirTilesFolder, tilesFolderFullPath,
                                          urlTiles,
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
  makeRemoteHashFile(url, destinationPath, targetFile, remoteHash,
                     algorithm = remoteAlgorithm, write = TRUE)
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
  toForPlot <- if (!any(terra::compareGeom(tileGrid, to))) postProcess(to, terra::crs(tileGrid)) else to
  terra::plot(toForPlot, add = TRUE, col = "red")
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
  if (!requireNamespace("terra")) {
    stop("Please install.packages('terra')")
  }

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
  if (doUploads > 1) tilesFullOnRemote <- length(missingTilesRemoteAll) == 0

  if (length(missingTilesOnRemote) == 0) {
    doTileDownload <- haveLocalTiles %in% FALSE
    messagePreProcess("All needed tiles are available on Google Drive.  ",
                      verbose = verbose)
    needUploads <- tilesFullOnRemote %in% FALSE
    if (doTileDownload) {
      messagePreProcess("Proceeding to download only the needed tiles...", verbose = verbose)
    } else {
      messagePreProcess("Nothing to download", verbose = verbose)
    }
  } else {
    messagePreProcess("Some tiles are missing on Google Drive:")
    missingOnes <- if (doUploads > 1) missingTilesRemoteAll else missingTilesOnRemote
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

  if (needUploads %in% TRUE || (doUploads > 0 && haveRemoteTiles %in% FALSE)) {
    fe <- file.exists(targetFileFullPath)
    if (fe %in% FALSE)
      download_resumable_httr2(url, targetFileFullPath)


    if (haveLocalTiles %in% FALSE || (doUploads > 0 && needUploads))
      tile_raster_write_auto(targetFileFullPath, tilesFolderFullPath, tileGrid,
                             all_tile_names = all_tile_names, datatype = datatype,
                             nx = numTiles[[1]], ny = numTiles[[2]],
                             verbose = verbose)
    if (needUploads %in% FALSE && doUploads > 0)
      messagePreProcess("Nothing to upload", verbose = verbose)

    upload_tiles_to_drive_url_parallel(tilesFolderFullPath, urlTiles, targetFileFullPath,
                                       verbose = verbose)
    tile_paths <- dir(tilesFolderFullPath, pattern = "\\.tif$")
    saExt <- terra::ext(to_inTileGrid)

    # Filter tiles that intersect the study area
    keep_idx <- vapply(tile_paths, function(path) {
      tile_ext <- terra::ext(terra::rast(file.path(tilesFolderFullPath, path)))

      # bounding box overlap (x then y)
      !(tile_ext[1] > saExt[2] || tile_ext[2] < saExt[1] ||  # x no-overlap
          tile_ext[3] > saExt[4] || tile_ext[4] < saExt[3])    # y no-overlap
    }, logical(1))

    intersecting_tiles2 <- tile_paths[keep_idx]

    # intersecting_tiles2 <- purrr::keep(tile_paths, function(path) {
    #   tile_ext <- terra::ext(terra::rast(file.path(tilesFolderFullPath, path)))
    #
    #   # Check for bounding box overlap
    #   !(tile_ext[1] > saExt[2] || tile_ext[2] < saExt[1] ||  # x overlap
    #       tile_ext[3] > saExt[4] || tile_ext[4] < saExt[3])    # y overlap
    # })
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
#' @param min An integer specifying the minimum number of cores to use. Default is `2`.
#' @param max An integer specifying the maximum number of cores available,
#'   typically the number of physical cores. Default is
#'   `max(1L, getOption("Ncpus", 1L), parallel::detectCores() - 1, logical = FALSE, na.rm = TRUE)`.
#'
#' @return An integer representing the number of cores that can be used for
#'   parallel tasks, ensuring at least `min` cores are used, while subtracting
#'   one for the current process and an estimate of actively used threads (via
#'   `detectActiveCores()`).
#'
#' @examples
#' if (FALSE) {
#'   numCoresToUse()
#'   numCoresToUse(min = 4)
#' }
#'
#' @note This function depends on `detectActiveCores()` and is not supported on
#'   Windows systems.
#'
#' @export
#' @seealso [detectActiveCores()]
numCoresToUse <- function(min = 2, max = NULL) {
  if (.requireNamespace("parallelly")) {
    nctu <- max(min, min(max, parallelly::freeCores()))
    return(nctu)
  }
  # if (is.null(.pkgEnv$detectedCores)) {
  #   ## see <https://parallelly.futureverse.org/#availablecores-vs-paralleldetectcores>
  #   .pkgEnv$detectedCores <- max(1L, getOption("Ncpus", 1L), parallel::detectCores() - 1,
  #                                logical = FALSE, na.rm = TRUE)
  # }
  # dc <- .pkgEnv$detectedCores
  # if (is.null(max)) {
  #   max <- dc
  # }
  # max <- min(dc -  # total
  #              1 - # remove one for the current process
  #              detectActiveCores(), # estimate actively used ones
  #            max)
  # max(min, max)
}

## CPU-bound parallelism (tiling). Unlike the network knobs this one *is* core
## shaped, and `mc.cores` still applies because it is the standard base-R control
## for forking. `reproducible.parallel.cores` overrides the detected default.
.parallelCores <- function(maxN = NULL) {
  n <- .parallelOptInt(getOption("reproducible.parallel.cores", NULL))
  if (is.null(n)) {
    ## numCoresToUse() returns NULL when the Suggested `parallelly` is absent
    ## (e.g. an `_R_CHECK_DEPENDS_ONLY_` leg); without a fallback that would
    ## silently collapse to 1 and serialize all CPU work.
    n <- .parallelOptInt(numCoresToUse(max = maxN))
    if (is.null(n)) {
      dc <- suppressWarnings(as.integer(parallel::detectCores())[1])
      n <- if (is.na(dc) || dc < 2L) 2L else max(2L, dc - 1L)
      if (!is.null(maxN)) n <- min(n, maxN)
    }
  } else if (!is.null(maxN)) {
    n <- min(n, maxN)
  }
  mcc <- .parallelOptInt(getOption("mc.cores", NULL))
  if (!is.null(mcc)) n <- min(n, mcc)
  .forkLimit(n)
}

# Classify a remote-supplied hash string into a content-hash algorithm or
# "etag-opaque" when no positive trust is possible. Google Drive ETag-shaped
# strings are forced to "md5" via the isGDurl override at the call site.
#
# Heuristic:
#   ^[0-9a-f]{32}$ -> md5   (most common content-hash ETag; Google Drive)
#   ^[0-9a-f]{40}$ -> sha1
#   ^[0-9a-f]{64}$ -> sha256
#   anything else  -> "etag-opaque" (weak ETags, server-derived, unknown)
.classifyRemoteHashAlgo <- function(hash, isGDurl = FALSE) {
  if (isTRUE(isGDurl)) return("md5")
  if (is.null(hash) || is.na(hash) || !is.character(hash) || !nzchar(hash))
    return("etag-opaque")
  hl <- tolower(hash)
  if (grepl("^[0-9a-f]{32}$", hl)) return("md5")
  if (grepl("^[0-9a-f]{40}$", hl)) return("sha1")
  if (grepl("^[0-9a-f]{64}$", hl)) return("sha256")
  "etag-opaque"
}

# Parse the contents of a `.hash` sidecar file. Format is `<algo>:<hash>`
# (one line). For backward compatibility with legacy single-hash sidecars
# written before this format change, fall back to inferring `algo` from the
# hash length (32/40/64 hex => md5/sha1/sha256).
#
# Returns list(algorithm = ..., hash = ...) or NULL on read error.
.parseRemoteHashFile <- function(remoteHashFile) {
  if (!file.exists(remoteHashFile)) return(NULL)
  txt <- try(readLines(remoteHashFile, warn = FALSE), silent = TRUE)
  if (is(txt, "try-error") || !length(txt)) return(NULL)
  txt <- txt[nzchar(txt)]
  if (!length(txt)) return(NULL)

  splitLine <- function(line) {
    if (!grepl(":", line, fixed = TRUE)) return(NULL)
    parts <- strsplit(line, ":", fixed = TRUE)[[1L]]
    if (length(parts) < 2L) return(NULL)
    list(key = parts[[1L]], value = paste(parts[-1L], collapse = ":"))
  }

  entries <- Filter(Negate(is.null), lapply(txt, splitLine))
  keys <- vapply(entries, `[[`, character(1), "key")

  etag <- NULL
  whEtag <- which(keys == "etag")
  if (length(whEtag)) etag <- entries[[whEtag[[1L]]]]$value

  # the digest line is any keyed line that is not the etag
  whDigest <- which(keys != "etag")
  if (length(whDigest)) {
    e <- entries[[whDigest[[1L]]]]
    return(list(algorithm = e$key, hash = e$value, etag = etag))
  }
  if (!is.null(etag)) {
    # ETag only: keep algorithm/hash populated so callers that predate the
    # `etag` field continue to behave as they did.
    return(list(algorithm = "etag", hash = etag, etag = etag))
  }

  # Legacy single-hash sidecar: infer algorithm from hash length.
  line <- txt[[1L]]
  list(algorithm = .classifyRemoteHashAlgo(line), hash = line, etag = NULL)
}

makeRemoteHashFile <- function(url, destinationPath, targetFile, remoteHash,
                               algorithm = NULL, write = FALSE, etag = NULL) {
  url_no_protocol <- sub("^https?://", "", url)
  # Replace all slashes with underscores
  urlWithUnderscores <- gsub("/", "_", file.path(basename(targetFile), dirname(url_no_protocol)))
  # Hide the sidecar with a leading "." so it doesn't show up in dir() listings
  # (e.g. test patterns like `dir(tmpdir, pattern = "Shapefile")` would otherwise
  # match `Shapefile1.zip_drive.google.com_..._.hash` and inflate file counts).
  remoteHashFile <- file.path(destinationPath, paste0(".", urlWithUnderscores, ".hash"))
  if (isTRUE(write) && !file.exists(remoteHashFile)) {
    # A digest and an ETag answer different questions, so record both when the
    # remote offers both:
    #   digest -- pins the bytes; can be recomputed locally to confirm that a
    #             download was not corrupted, and compared to what the server
    #             advertises later.
    #   ETag   -- the server's own "you already have this" token; the only
    #             thing that works when the digest is opaque, via If-None-Match.
    lines <- character()
    if (!is.null(algorithm) && !is.na(algorithm) && nzchar(algorithm) &&
        !identical(algorithm, "etag")) {
      lines <- c(lines, paste0(algorithm, ":", remoteHash))
    } else if (is.null(algorithm) || is.na(algorithm) || !nzchar(algorithm)) {
      # Legacy callers: write hash-only line.
      lines <- c(lines, remoteHash)
    }
    if (!is.null(etag) && !is.na(etag) && nzchar(etag)) {
      lines <- c(lines, paste0("etag:", etag))
    } else if (identical(algorithm, "etag")) {
      lines <- c(lines, paste0("etag:", remoteHash))
    }
    if (length(lines)) writeLines(lines, remoteHashFile)
  }
  return(remoteHashFile)
}



checkHaveCorrectHashedVersion <- function(targetFile, remoteHashFile, remoteMetadata, purge, verbose) {
  haveCorrectVersion <- FALSE
  askAboutPurge <- FALSE
  fe <- file.exists(remoteHashFile)
  # But still could be incomplete
  if (isTRUE(fe)) {
    parsed <- .parseRemoteHashFile(remoteHashFile)
    haveCorrectVersion <- !is.null(parsed) &&
      identical(parsed$hash, remoteMetadata$remoteHash)
    if (haveCorrectVersion %in% FALSE) {
      askAboutPurge <- TRUE
    } else {
      if (file.exists(targetFile)) {
        if (!identical(file.size(targetFile), as.numeric(remoteMetadata$fileSize) )) {
          askAboutPurge <- TRUE
        }
      }
    }
  }
  if (isTRUE(askAboutPurge)) {
    message("The local version is not the version that matches the remote version")
    message("Do you want to purge all local data and redownload? Y or N")
    yorn <- readline(" ")
    yorn <- substr(tolower(yorn), 1, 1)
    if (identical("y", yorn))
      purge <- TRUE

  }
  if (!purge %in% TRUE)
    messagePreProcess("Local files match the current remote file version; proceeding",
                      verbose = verbose)

  purge
}

getRemoteMetadata <- function(targetFile, isGDurl, url) {
  # browser()
  if (missing(isGDurl))
    isGDurl <- isGoogleDriveURL(url) || isGoogleID(url)
  if (isGDurl) {
    # Cache indefinitely — same rationale as assessGoogle().
    file <- googledrive::drive_get(url) |>
      Cache(verbose = FALSE)
    fileSize <- file$drive_resource[[1]]$size
    remoteHash <- file$drive_resource[[1]]$md5Checksum
    if (missing(targetFile)) {
      targetFile <- file$name
    }
    timestampOnline <- file$drive_resource[[1]]$modifiedTime
  }

  if (!isGDurl) {
    # Always ask the remote, even when `targetFile` was supplied. The ETag and
    # size are wanted for the sidecar regardless of whether the caller needed
    # help naming the file -- and previously, supplying `targetFile` skipped
    # this block entirely, leaving `remoteHash` undefined and erroring below.
    response <- httr2::request(url) |> httr2::req_method("HEAD") |> httr2::req_perform()
    etagRaw <- httr2::resp_headers(response)[["etag"]]
    remoteHash <- etagRaw |>
      gsub(pattern = "^\"|\"", replacement = "")

    content_disposition <- httr2::resp_header(response, "content-disposition")
    fileSize <- httr2::resp_header(response, "content-length") |> as.numeric()
    timestampOnline <- httr2::resp_header(response, "Date")
    if (missing(targetFile)) {
      if (isTRUE(!(is.na(content_disposition)))) {
        targetFile <- sub('.*filename="([^"]+)".*', '\\1', content_disposition)
      } else {
        # Fallback: extract from URL
        targetFile <- basename(url)
      }
    }
  }
  remoteAlgorithm <- .classifyRemoteHashAlgo(remoteHash, isGDurl = isGDurl)
  if (!exists("etagRaw", inherits = FALSE)) etagRaw <- NULL
  list(targetFile = targetFile, fileSize = fileSize, remoteHash = remoteHash,
       remoteAlgorithm = remoteAlgorithm, timestampOnline = timestampOnline,
       etag = etagRaw)
}

# Conditional revalidation of a remote file using its ETag.
#
# An ETag is an opaque cache validator, not a content hash -- servers are free
# to generate it however they like (a git blob SHA, an S3 checksum, an edge
# server's own token). It cannot be recomputed locally, but it CAN be handed
# back via `If-None-Match`: a 304 means the representation is unchanged, a 200
# means it is not.
#
# `etag` must be the raw header value, including its quotes and any `W/` weak
# prefix, and the request must be made the same way the ETag was obtained
# (httr2 negotiates gzip by default, and a server may serve a different ETag
# for the compressed representation).
#
# Returns list(unchanged = TRUE/FALSE/NA, etag = <current etag or NULL>).
# `unchanged = NA` means the remote could not be reached, so the caller should
# fall back to whatever it would have done offline rather than re-download.
.remoteEtagRevalidate <- function(url, etag) {
  if (!requireNamespace("httr2", quietly = TRUE) ||
      is.null(etag) || !nzchar(etag))
    return(list(unchanged = NA, etag = NULL))

  tryCatch({
    resp <- httr2::request(url) |>
      httr2::req_method("HEAD") |>
      httr2::req_headers(`If-None-Match` = etag) |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform()
    status <- httr2::resp_status(resp)
    if (identical(status, 304L)) {
      list(unchanged = TRUE, etag = etag)
    } else if (status >= 200L && status < 300L) {
      list(unchanged = FALSE, etag = httr2::resp_header(resp, "etag"))
    } else {
      list(unchanged = NA, etag = NULL)
    }
  }, error = function(e) list(unchanged = NA, etag = NULL))
}

sprcMosaicRast <- function(url, tile_rasters, to_inTileGrid, targetFilePostProcessedFullPath,
                           fileSize, needed_tile_names, tilesFolderFullPath, noData,
                           datatype, verbose) {
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
      messagePrepInputs("merging tiles in ", .messageFunctionFn(tilesFolderFullPath), " ...", verbose = verbose)
      rfull <- terra::merge(final)
      messagePreProcess("  ", gsub("^\b", "", messagePrefixDoneIn),
                        format(difftime(Sys.time(), st3), units = "secs", digits = 3),
                        verbose = verbose)

      st2 <- Sys.time()
      # messagePrepInputs("writing ", .messageFunctionFn(targetFilePostProcessedFullPath), " ...", verbose = verbose)
      # rfull <- terra::writeRaster(merged, filename = targetFilePostProcessedFullPath,
      #                             datatype = datatype,
      #                             overwrite = TRUE)
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
    messagePreProcess("The dataset at \n", url, "\ndoes not have data that overlaps with `to`",
                      verbose = verbose)
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
        if (isTRUE(purge) && doUploads > 1) {
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

makeAndPlotTileGrid <- function(tileGrid, numTiles, targetObjCRS, plot.grid, to, verbose) {
  if (is.character(tileGrid)) {
    tg <- makeTileGridFromGADMcode(tileGrid, numTiles, crs = targetObjCRS) |>
      Cache(verbose = verbose - 1)
    tileGrid <- tg$tileGrid
    numTiles <- tg$numTiles
    theArea <- tg$area
  } else {
    stop("tileGrid must be a character string")
  }
  # if (missing(theArea)) {
  #   theArea <- terra::ext(tileGrid)
  # }
  if (isTRUE(plot.grid) && !missing(to)) {
    plotGridAndArea(tileGrid, theArea, to)
  }
  list(tileGrid = tileGrid, numTiles = numTiles)
}


# Get resolution
boundaryPolygon <- function(r) {
  res_x <- terra::res(r)[1]
  res_y <- terra::res(r)[2]

  # Get raster extent
  ext <- terra::ext(r)

  # Generate coordinates of pixel corners along the boundary
  # Top edge (left to right)
  top <- cbind(seq(ext[1], ext[2] - res_x, by = res_x), rep(ext[4], ncol(r)))

  # Right edge (top to bottom)
  right <- cbind(rep(ext[2], nrow(r)), seq(ext[4], ext[3] + res_y, by = -res_y))

  # Bottom edge (right to left)
  bottom <- cbind(seq(ext[2], ext[1] + res_x, by = -res_x), rep(ext[3], ncol(r)))

  # Left edge (bottom to top)
  left <- cbind(rep(ext[1], nrow(r)), seq(ext[3], ext[4] - res_y, by = res_y))

  # Combine all edges into one closed polygon
  boundary_coords <- rbind(top, right, bottom, left, top[1, , drop = FALSE])

  # Create polygon
  terra::vect(list(boundary_coords), type = "polygons", crs = terra::crs(r))
}
