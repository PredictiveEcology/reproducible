## Google Drive round-trip for the tiling workflow in R/downloadTileAndUpload.R.
##
## Companion to test-tileHelpersLocal.R, which covers everything that can run
## without credentials. This file covers the half that cannot: uploading tiles
## to Drive, listing them back, downloading them again, reading a CRS off a
## remote tile, and purging them.
##
## Deliberately NOT a prepInputsWithTiles() end-to-end test. That entry point
## takes `tileGrid` as a character GADM code and routes through
## makeTileGridFromGADMcode() -> geodata::gadm("CAN", resolution = 2), which is
## a large download on every cold CI cache. Driving the helpers directly gets
## the same code covered in seconds instead, with no geodata dependency.
##
## Kept to a single test_that with one 20x20 raster split into four tiles, so
## the whole thing is a handful of small Drive calls. skip_on_cran() plus the
## needGoogleDriveAuth gate in testInit() keep it off CRAN and off any machine
## without credentials.

test_that("tiles round-trip through Google Drive", {
  skip_on_cran()
  skip_if_not_installed("terra")
  ## Upload round-trips hit a shared Drive folder, so run them on exactly one
  ## runner: several legs doing this at once is needless traffic and can race.
  ## Nothing here is platform-specific -- the macOS and Linux paths are identical.
  skip_if_not_releaseVer_Linux()
  testInit(c("terra", "googledrive"), needGoogleDriveAuth = TRUE)

  ## --- local fixture: one small raster, cut into 4 tiles --------------------
  ## tile_raster_write_auto() tiles via parallel::mclapply(). R CMD check sets
  ## _R_CHECK_LIMIT_CORES_, under which mclapply refuses more than two processes
  ## ("3 simultaneous processes spawned"), so cap it. One core is plenty here.
  withr::local_options(mc.cores = 1)

  r <- terra::rast(
    nrows = 20, ncols = 20, xmin = 0, xmax = 100, ymin = 0, ymax = 100,
    crs = "EPSG:3347", vals = seq_len(400)
  )
  srcPath <- file.path(tmpdir, "src.tif")
  terra::writeRaster(r, srcPath, overwrite = TRUE)

  grid <- makeTileGrid(terra::ext(r), crs = terra::crs(r), numTiles = c(2, 2))
  nms <- makeTileNames(as.character(seq_len(4)))
  tilesDir <- file.path(tmpdir, "tiles")
  ## datatype must be explicit -- writeRaster(datatype = NULL) fails inside every
  ## mclapply worker and mclapply swallows it, yielding zero tiles silently.
  tile_raster_write_auto(srcPath, tilesDir, grid, nms, nx = 2, ny = 2,
                         datatype = "FLT4S", verbose = 0)
  expect_length(dir(tilesDir), 4L)

  ## --- a folder of our own on Drive ----------------------------------------
  ## Inside this session's root (see .cloudTestRoot in helper-allEqual.R) so
  ## concurrent CI jobs cannot see or delete each other's tiles.
  root <- retry(quote(googledrive::drive_mkdir(
    name = paste0("tileRoundTrip-", rndstr(1, 6)), path = .cloudTestRoot()
  )))
  on.exit(try(googledrive::drive_rm(root), silent = TRUE), add = TRUE)
  rootId <- as.character(root$id)

  ## --- upload ---------------------------------------------------------------
  ## Tiles land in a subfolder named after the target file's stem; that naming
  ## is what lsExistingTilesOnGoogleDrive() looks for on the way back.
  upload_tiles_to_drive_url_parallel(tilesDir, rootId, "src.tif", verbose = 0)

  existing <- lsExistingTilesOnGoogleDrive(rootId, "src.tif")
  expect_equal(NROW(existing), 4L)
  expect_setequal(existing$name, nms)

  ## A different target file has no subfolder of its own -> NULL, not an error.
  expect_null(lsExistingTilesOnGoogleDrive(rootId, "somethingElse.tif"))

  ## --- download back --------------------------------------------------------
  backDir <- file.path(tmpdir, "back")
  expect_true(getTilesFromGoogleDrive(existing$name, existing, backDir))
  expect_setequal(dir(backDir), nms)

  ## Round trip is faithful: the tiles that come back are readable and carry
  ## the CRS they went up with.
  for (nm in dir(backDir)) {
    tl <- terra::rast(file.path(backDir, nm))
    expect_identical(terra::crs(tl), terra::crs(r))
  }

  ## ... and they still mosaic back to the source extent, which is the whole
  ## point of the tiling workflow.
  mosaicked <- sprcMosaicRast(
    url = "https://example.com/src.tif",
    tile_rasters = rastTiles(dir(backDir), backDir),
    to_inTileGrid = terra::ext(r),
    targetFilePostProcessedFullPath = file.path(tmpdir, "mosaic.tif"),
    fileSize = 1, needed_tile_names = dir(backDir),
    tilesFolderFullPath = backDir, noData = NA, datatype = "FLT4S", verbose = 0
  )
  expect_s4_class(mosaicked, "SpatRaster")
  expect_equal(as.vector(terra::ext(mosaicked)), as.vector(terra::ext(r)), tolerance = 1e-6)

  ## --- CRS straight off a remote tile ---------------------------------------
  ## Downloads one tile into a fresh directory purely to read its CRS -- the
  ## path taken when there is no local copy to inspect.
  crsDir <- file.path(tmpdir, "crsOnly")
  fileSize <- as.numeric(existing$drive_resource[[1]]$size)
  expect_identical(
    crsFromGoogleDriveTile(crsDir, existing, fileSize, verbose = 0),
    terra::crs(r)
  )

  ## --- purge ----------------------------------------------------------------
  expect_null(purgeGoogleTiles(rootId, "src.tif", verbose = 0))
  expect_equal(NROW(googledrive::drive_ls(root)), 0L)
})

test_that("prepInputsWithTiles returns early without urlTiles", {
  ## No credentials needed: this is the guard before any Drive work.
  testInit()

  mess <- capture_messages({
    out <- prepInputsWithTiles(
      targetFile = "src.tif", url = "https://example.com/src.tif",
      destinationPath = tmpdir, to = 1, urlTiles = NULL, verbose = 1
    )
  })

  ## Returns the string "NULL" rather than NULL or an error -- pinned because
  ## callers branch on it.
  expect_identical(out, "NULL")
  expect_true(any(grepl("must have `urlTiles`", mess)))
})
