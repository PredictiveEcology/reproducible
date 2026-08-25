## Local (no-network, no-Drive) coverage for the tiling workflow in
## R/downloadTileAndUpload.R.
##
## prepInputsWithTiles() itself cannot run without Google Drive -- it returns
## early unless `urlTiles` is supplied, and that is inherently a Drive folder.
## But the machinery it drives is local: cut a raster into tiles, read CRS back
## off those tiles, mosaic them together again, and tidy up. All of that is
## exercised here with a small dummy SpatRaster, so it runs everywhere including
## CRAN and stays fast.
##
## The Drive-dependent half (prepInputsWithTiles, downloadMakeAndUploadTiles,
## the upload/download helpers) is covered separately by a Drive-gated test.

## A small raster plus its tiles. 20x20 over a 100x100 extent split 2x2 keeps
## the whole fixture well under a millisecond of I/O.
mkTiledFixture <- function(dir, nx = 2, ny = 2) {
  ## tile_raster_write_auto() tiles via parallel::mclapply(), sizing itself with
  ## min(getOption("mc.cores"), numCoresToUse(...)). R CMD check sets
  ## _R_CHECK_LIMIT_CORES_, under which mclapply refuses more than two processes:
  ##   Error in .check_ncores(cores): 3 simultaneous processes spawned
  ## Passes locally and fails on every CI leg without this. One core is also
  ## plenty for four tiny tiles.
  withr::local_options(mc.cores = 1, .local_envir = parent.frame())

  r <- terra::rast(
    nrows = 20, ncols = 20, xmin = 0, xmax = 100, ymin = 0, ymax = 100,
    crs = "EPSG:3347", vals = seq_len(400)
  )
  srcPath <- file.path(dir, "src.tif")
  terra::writeRaster(r, srcPath, overwrite = TRUE)

  grid <- makeTileGrid(terra::ext(r), crs = terra::crs(r), numTiles = c(nx, ny))
  nms <- makeTileNames(as.character(seq_len(nx * ny)))
  tilesDir <- file.path(dir, "tiles")

  ## datatype must be explicit: writeRaster(datatype = NULL) fails inside every
  ## mclapply worker, and mclapply swallows the error, so it silently produces
  ## no tiles at all.
  tile_raster_write_auto(srcPath, tilesDir, grid, nms, nx = nx, ny = ny,
                         datatype = "FLT4S", verbose = 0)

  list(r = r, srcPath = srcPath, grid = grid, names = nms, tilesDir = tilesDir)
}

test_that("tile_raster_write_auto cuts a raster into one file per tile", {
  skip_if_not_installed("terra")
  testInit("terra")

  fx <- mkTiledFixture(tmpdir, nx = 2, ny = 2)

  written <- dir(fx$tilesDir)
  expect_length(written, 4L)
  expect_setequal(written, fx$names)

  ## Every tile is readable and inherits the source CRS.
  for (nm in written) {
    tl <- terra::rast(file.path(fx$tilesDir, nm))
    expect_s4_class(tl, "SpatRaster")
    expect_identical(terra::crs(tl), terra::crs(fx$r))
  }

  ## Re-running skips what is already on disk rather than rewriting it, so the
  ## workflow is resumable after an interrupted tiling.
  before <- file.mtime(file.path(fx$tilesDir, written))
  tile_raster_write_auto(fx$srcPath, fx$tilesDir, fx$grid, fx$names,
                         nx = 2, ny = 2, datatype = "FLT4S", verbose = 0)
  expect_identical(file.mtime(file.path(fx$tilesDir, written)), before)
})

test_that("sprcMosaicRast merges tiles back into one raster", {
  skip_if_not_installed("terra")
  testInit("terra")

  fx <- mkTiledFixture(tmpdir)
  tiles <- rastTiles(dir(fx$tilesDir), fx$tilesDir)
  expect_length(tiles, 4L)

  out <- sprcMosaicRast(
    url = "http://example.com/x.tif", tile_rasters = tiles,
    to_inTileGrid = terra::ext(fx$r),
    targetFilePostProcessedFullPath = file.path(tmpdir, "mosaic.tif"),
    fileSize = 1, needed_tile_names = dir(fx$tilesDir),
    tilesFolderFullPath = fx$tilesDir, noData = NA,
    datatype = "FLT4S", verbose = 0
  )

  expect_s4_class(out, "SpatRaster")
  ## Round trip: mosaicking the tiles recovers the source extent.
  expect_equal(as.vector(terra::ext(out)), as.vector(terra::ext(fx$r)), tolerance = 1e-6)

  ## NB the writeRaster inside is commented out, so the merged raster is
  ## returned in memory and the target path is NOT written. Asserted so that
  ## re-enabling it is a deliberate, visible change.
  expect_false(file.exists(file.path(tmpdir, "mosaic.tif")))
})

test_that("sprcMosaicRast returns NULL when nothing overlaps or no tiles load", {
  skip_if_not_installed("terra")
  testInit("terra")

  fx <- mkTiledFixture(tmpdir)
  tiles <- rastTiles(dir(fx$tilesDir), fx$tilesDir)

  ## An extent far away from the data -> no overlap -> NULL, with a message
  ## naming the url so the caller can tell which source came up empty.
  far <- terra::ext(1e6, 1e6 + 10, 1e6, 1e6 + 10)
  mess <- capture_messages({
    out <- sprcMosaicRast(
      url = "http://example.com/x.tif", tile_rasters = tiles, to_inTileGrid = far,
      targetFilePostProcessedFullPath = file.path(tmpdir, "m2.tif"),
      fileSize = 1, needed_tile_names = dir(fx$tilesDir),
      tilesFolderFullPath = fx$tilesDir, noData = NA, datatype = "FLT4S", verbose = 1
    )
  })
  expect_null(out)
  expect_true(any(grepl("does not have data that overlaps", mess)))

  ## All tiles failed to load -> also NULL rather than an error.
  outAllNull <- sprcMosaicRast(
    url = "http://example.com/x.tif", tile_rasters = list(NULL, NULL),
    to_inTileGrid = terra::ext(fx$r),
    targetFilePostProcessedFullPath = file.path(tmpdir, "m3.tif"),
    fileSize = 1, needed_tile_names = character(),
    tilesFolderFullPath = fx$tilesDir, noData = NA, datatype = "FLT4S", verbose = 0
  )
  expect_null(outAllNull)
})

test_that("tryRastThenGetCRS reads a CRS and discards a corrupt file", {
  skip_if_not_installed("terra")
  testInit("terra")

  fx <- mkTiledFixture(tmpdir)
  expect_identical(tryRastThenGetCRS(fx$srcPath), terra::crs(fx$r))

  ## A file that is not a raster: NULL, and the corrupt file is removed so the
  ## workflow can re-fetch it rather than failing on it forever.
  bad <- file.path(tmpdir, "corrupt.tif")
  writeLines("definitely not a GeoTIFF", bad)
  expect_true(file.exists(bad))
  suppressWarnings(expect_null(tryRastThenGetCRS(bad)))
  expect_false(file.exists(bad))
})

test_that("rmRastIfTryError only deletes on the SpatRaster-open error", {
  skip_if_not_installed("terra")
  testInit("terra")

  f <- file.path(tmpdir, "victim.tif")
  writeLines("x", f)

  ## Unrelated error text -> file left alone, object passed through untouched.
  other <- "some completely different problem"
  expect_identical(rmRastIfTryError(other, tmpdir, "victim.tif"), other)
  expect_true(file.exists(f))

  ## The specific message -> file removed and NULL returned.
  expect_null(rmRastIfTryError("cannot open this file as a SpatRaster", tmpdir, "victim.tif"))
  expect_false(file.exists(f))
})

test_that("crsFromLocalTile / crsFromLocalFile read CRS off local files", {
  skip_if_not_installed("terra")
  testInit("terra")

  fx <- mkTiledFixture(tmpdir)

  expect_identical(crsFromLocalFile(fx$srcPath), terra::crs(fx$r))
  expect_identical(crsFromLocalTile(fx$tilesDir, dir(fx$tilesDir)), terra::crs(fx$r))
})

test_that("crsFromLocalOrGDTiles resolves from local tiles without touching Drive", {
  skip_if_not_installed("terra")
  testInit("terra")

  fx <- mkTiledFixture(tmpdir)

  ## With local tiles present the loop breaks before any Drive call, so this
  ## needs no credentials. urlTiles is deliberately nonsense: reaching it would
  ## error, which is what makes this a real assertion about short-circuiting.
  out <- crsFromLocalOrGDTiles(
    targetObjCRS = NULL, dirTilesFolder = dir(fx$tilesDir),
    tilesFolderFullPath = fx$tilesDir, urlTiles = "not-a-real-url",
    targetFile = "src.tif", purge = FALSE, doUploads = FALSE,
    fileSize = 1, verbose = 0
  )
  expect_identical(out, terra::crs(fx$r))
})

test_that("getTargetCRS takes the local-file branch and writes a hash sidecar", {
  skip_if_not_installed("terra")
  testInit("terra")

  fx <- mkTiledFixture(tmpdir)
  url <- "https://example.com/data/src.tif"

  out <- getTargetCRS(
    targetFileFullPath = fx$srcPath, dirTilesFolder = dir(fx$tilesDir),
    tilesFolderFullPath = fx$tilesDir, targetFile = "src.tif",
    destinationPath = tmpdir, url = url, urlTiles = "not-a-real-url",
    fileSize = 1, remoteHash = strrep("a", 32),
    purge = FALSE, doUploads = FALSE, verbose = 0
  )
  expect_identical(out, terra::crs(fx$r))

  ## Having resolved the CRS locally it records the remote hash for next time.
  hf <- makeRemoteHashFile(url, tmpdir, "src.tif", strrep("a", 32))
  expect_true(file.exists(hf))
  expect_identical(.parseRemoteHashFile(hf),
                   list(algorithm = "md5", hash = strrep("a", 32), etag = NULL, url = url))
})

test_that("checkHaveCorrectHashedVersion accepts a matching local hash", {
  testInit()

  url <- "https://example.com/data/x.tif"
  target <- file.path(tmpdir, "x.tif")
  writeLines("payload", target)
  hash <- strrep("b", 32)

  hf <- makeRemoteHashFile(url, tmpdir, "x.tif", hash, algorithm = "md5", write = TRUE)
  meta <- list(remoteHash = hash, fileSize = file.size(target))

  ## Hash and size both match -> no purge, and no interactive prompt. (The
  ## mismatch branch calls readline(), so it is deliberately not exercised
  ## here -- it would block or consume stdin under R CMD check.)
  mess <- capture_messages({
    purge <- checkHaveCorrectHashedVersion(target, hf, meta, purge = FALSE, verbose = 1)
  })
  expect_false(isTRUE(purge))
  expect_true(any(grepl("match the current remote file version", mess)))
})

test_that("purgeLocals and purgeLocalTiles remove what they claim to", {
  skip_if_not_installed("terra")
  testInit("terra")

  fx <- mkTiledFixture(tmpdir)
  post <- file.path(tmpdir, "post.tif"); writeLines("a", post)
  hashFile <- file.path(tmpdir, ".x.hash"); writeLines("md5:abc", hashFile)

  purgeLocals(post, fx$srcPath, hashFile, verbose = 0)
  expect_false(file.exists(post))
  expect_false(file.exists(fx$srcPath))
  expect_false(file.exists(hashFile))

  ## Tiles are emptied and NULL comes back, which is what the caller uses to
  ## decide the tiles must be rebuilt.
  expect_length(dir(fx$tilesDir), 4L)
  expect_null(purgeLocalTiles(fx$tilesDir, verbose = 0))
  expect_length(dir(fx$tilesDir), 0L)
})

test_that("makeAndPlotTileGrid rejects a non-character tileGrid", {
  skip_if_not_installed("terra")
  testInit("terra")

  ## The character branch calls makeTileGridFromGADMcode(), which downloads GADM
  ## data, so only the guard is exercised here.
  expect_error(
    makeAndPlotTileGrid(tileGrid = 42, numTiles = c(2, 2), targetObjCRS = "EPSG:3347",
                        plot.grid = FALSE, verbose = 0),
    "must be a character string"
  )
})

test_that("plotGridAndArea draws the grid, area and target", {
  skip_if_not_installed("terra")
  testInit("terra")

  fx <- mkTiledFixture(tmpdir)
  to <- terra::as.polygons(terra::ext(10, 40, 10, 40), crs = terra::crs(fx$r))

  ## Plotting is a side effect; assert only that it completes on a null device.
  ## Not expect_silent(): it reprojects via postProcess(), which reports.
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_no_error(suppressMessages(plotGridAndArea(fx$grid, terra::ext(fx$r), to)))
})
