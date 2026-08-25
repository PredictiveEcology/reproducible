## Unit tests for the pure helpers in R/downloadTileAndUpload.R.
##
## The tiling/upload workflow itself (prepInputsWithTiles) needs Google Drive
## write access, so it is exercised only in the Drive-gated integration test.
## These helpers, however, are ordinary functions -- grid arithmetic, name
## padding, proj4 assembly, hash-sidecar parsing -- with no network, no Drive
## and no database. They are tested here so the file is not left effectively
## untested just because its entry point is hard to run.

test_that("extract_drive_id pulls ids out of folder and file urls", {
  testInit()

  expect_identical(
    extract_drive_id("https://drive.google.com/drive/folders/1An8s2YLFPopQKr4BWK9o06fLSXx"),
    "1An8s2YLFPopQKr4BWK9o06fLSXx"
  )
  ## trailing query string must not be captured
  expect_identical(
    extract_drive_id("https://drive.google.com/drive/folders/abc_DEF-123?usp=sharing"),
    "abc_DEF-123"
  )
  expect_identical(
    extract_drive_id("https://drive.google.com/file/d/1XyZ_abc-987/view?usp=drive_link"),
    "1XyZ_abc-987"
  )
  ## Neither shape -> returns the input unchanged (both sub() calls no-op).
  expect_identical(extract_drive_id("notAUrl"), "notAUrl")
})

test_that("tile name padding is width-consistent", {
  testInit()

  ## Width comes from the widest id, so a set spanning 1..100 pads to 3.
  expect_identical(makePaddedNamesForTiles(c("1", "10", "100")), c("001", "010", "100"))
  ## Single-width input needs no padding.
  expect_identical(makePaddedNamesForTiles(c("1", "9")), c("1", "9"))

  expect_identical(makeTileNames(c("1", "10", "100")),
                   c("tile_001.tif", "tile_010.tif", "tile_100.tif"))
  expect_true(all(endsWith(makeTileNames(as.character(1:12)), ".tif")))
  ## All names are the same length -> they sort lexically in numeric order.
  nms <- makeTileNames(as.character(1:12))
  expect_length(unique(nchar(nms)), 1L)
  expect_identical(sort(nms), nms)
})

test_that("build_lambert_proj4 centres on the extent and validates input", {
  skip_if_not_installed("terra")
  testInit("terra")

  ext <- terra::ext(c(xmin = -100, xmax = -90, ymin = 40, ymax = 60))
  p4 <- build_lambert_proj4(ext)

  expect_true(is.character(p4) && length(p4) == 1L)
  expect_true(startsWith(p4, "+proj=lcc"))
  ## Central meridian / latitude of origin are the extent midpoints.
  expect_match(p4, "\\+lon_0=-95\\b")
  expect_match(p4, "\\+lat_0=50\\b")
  ## Standard parallels sit 1/6 in from each edge: 40 + 20/6 and 60 - 20/6.
  expect_match(p4, "\\+lat_1=43\\.333333\\b")
  expect_match(p4, "\\+lat_2=56\\.666667\\b")

  expect_error(build_lambert_proj4("not an extent"), "terra::ext object")
})

test_that("best_square_grid finds a near-square tiling", {
  testInit()

  ## A square area should come back with an aspect ratio at (or very near) 1.
  sq <- best_square_grid(1000, 1000, min_tiles = 1, max_tiles = 12)
  expect_true(is.list(sq))
  expect_named(sq, c("nx", "ny", "tile_width", "tile_height",
                     "total_tiles", "aspect_ratio"))
  expect_equal(sq$aspect_ratio, 1, tolerance = 1e-6)
  expect_identical(sq$total_tiles, sq$nx * sq$ny)

  ## A 2:1 area is squarest when split twice as finely across x as y.
  wide <- best_square_grid(2000, 1000, min_tiles = 2, max_tiles = 8)
  expect_equal(wide$aspect_ratio, 1, tolerance = 1e-6)
  expect_true(wide$nx >= wide$ny)
  expect_equal(wide$tile_width, wide$tile_height, tolerance = 1e-6)
})

test_that("makeTileGrid returns one labelled polygon per tile", {
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")
  testInit(c("terra", "sf"))

  ext <- terra::ext(c(xmin = 0, xmax = 1000, ymin = 0, ymax = 2000))
  numTiles <- c(2, 4)
  tg <- makeTileGrid(ext, crs = "EPSG:3347", numTiles = numTiles)

  expect_s4_class(tg, "SpatVector")
  expect_identical(nrow(tg), prod(numTiles))
  ## tile_id is zero-padded to a common width and unique per tile.
  ids <- tg[["tile_id"]][[1]]
  expect_length(unique(ids), prod(numTiles))
  expect_length(unique(nchar(ids)), 1L)

  ## Default crs is the package's SCANFI lambert string.
  tgDefault <- makeTileGrid(ext, numTiles = c(2, 2))
  expect_identical(nrow(tgDefault), 4)
})

test_that("rastTiles reads the tiles it is given", {
  skip_if_not_installed("terra")
  testInit("terra")

  tilesDir <- checkPath(file.path(tmpdir, "tiles"), create = TRUE)
  nms <- c("tile_1.tif", "tile_2.tif")
  for (nm in nms) {
    r <- terra::rast(nrows = 4, ncols = 4, vals = seq_len(16))
    terra::writeRaster(r, file.path(tilesDir, nm), overwrite = TRUE)
  }

  out <- rastTiles(nms, tilesDir)
  expect_length(out, 2L)
  expect_true(all(vapply(out, inherits, logical(1), "SpatRaster")))

  ## A missing tile routes through the try-error branch rather than erroring.
  outMissing <- suppressWarnings(rastTiles(c(nms[[1]], "tile_absent.tif"), tilesDir))
  expect_length(outMissing, 2L)
})

test_that("numCoresToUse respects its floor and ceiling", {
  skip_if_not_installed("parallelly")
  testInit()

  expect_true(numCoresToUse(min = 2) >= 2)
  ## The ceiling wins even when many cores are free.
  expect_true(numCoresToUse(min = 1, max = 1) <= 1)
  ## The floor wins even when the ceiling is lower than it.
  expect_identical(numCoresToUse(min = 3, max = 1), 3)
})

test_that(".classifyRemoteHashAlgo maps hash width to algorithm", {
  testInit()

  expect_identical(.classifyRemoteHashAlgo(strrep("a", 32)), "md5")
  expect_identical(.classifyRemoteHashAlgo(strrep("a", 40)), "sha1")
  expect_identical(.classifyRemoteHashAlgo(strrep("a", 64)), "sha256")
  ## Case-insensitive.
  expect_identical(.classifyRemoteHashAlgo(strrep("A", 32)), "md5")

  ## Anything that isn't a recognised hex width is untrusted.
  expect_identical(.classifyRemoteHashAlgo("W/\"abc123\""), "etag-opaque")
  expect_identical(.classifyRemoteHashAlgo(strrep("z", 32)), "etag-opaque")
  expect_identical(.classifyRemoteHashAlgo(""), "etag-opaque")
  expect_identical(.classifyRemoteHashAlgo(NULL), "etag-opaque")
  expect_identical(.classifyRemoteHashAlgo(NA_character_), "etag-opaque")

  ## Google Drive urls always report md5, whatever the string looks like.
  expect_identical(.classifyRemoteHashAlgo("anything", isGDurl = TRUE), "md5")
})

test_that(".parseRemoteHashFile reads both the current and legacy sidecar formats", {
  testInit()

  expect_null(.parseRemoteHashFile(file.path(tmpdir, "does-not-exist.hash")))

  ## Current format: "<algo>:<hash>".
  f1 <- file.path(tmpdir, "current.hash")
  writeLines("sha256:abc123", f1)
  expect_identical(.parseRemoteHashFile(f1),
                   list(algorithm = "sha256", hash = "abc123", etag = NULL, url = NULL))

  ## A hash containing colons keeps them (only the first colon splits).
  f2 <- file.path(tmpdir, "colons.hash")
  writeLines("md5:aa:bb:cc", f2)
  expect_identical(.parseRemoteHashFile(f2)$hash, "aa:bb:cc")

  ## Legacy format: bare hash, algorithm inferred from its width.
  f3 <- file.path(tmpdir, "legacy.hash")
  writeLines(strrep("a", 32), f3)
  expect_identical(.parseRemoteHashFile(f3),
                   list(algorithm = "md5", hash = strrep("a", 32), etag = NULL, url = NULL))

  ## Empty file -> NULL rather than a malformed result.
  f4 <- file.path(tmpdir, "empty.hash")
  file.create(f4)
  expect_null(.parseRemoteHashFile(f4))
})

test_that("makeRemoteHashFile builds a hidden sidecar and round-trips", {
  testInit()

  url <- "https://drive.google.com/file/d/1abc/view"
  hashFile <- makeRemoteHashFile(url, tmpdir, "target.tif", "deadbeef")

  ## Hidden (leading dot) so dir() patterns in other tests don't match it.
  expect_true(startsWith(basename(hashFile), "."))
  expect_true(endsWith(hashFile, ".hash"))
  ## write = FALSE (the default) must not create anything.
  expect_false(file.exists(hashFile))

  ## With an algorithm -> current format, and parses back to what went in.
  written <- makeRemoteHashFile(url, tmpdir, "target.tif", "deadbeef",
                                algorithm = "md5", write = TRUE)
  expect_true(file.exists(written))
  expect_identical(.parseRemoteHashFile(written),
                   list(algorithm = "md5", hash = "deadbeef", etag = NULL, url = url))

  ## Without an algorithm -> legacy hash-only line.
  written2 <- makeRemoteHashFile(url, tmpdir, "other.tif", strrep("b", 40),
                                 write = TRUE)
  ## legacy hash-only line, now followed by the url that produced it
  expect_identical(readLines(written2, warn = FALSE)[[1L]], strrep("b", 40))
  expect_identical(.parseRemoteHashFile(written2)$url, url)
  expect_identical(.parseRemoteHashFile(written2)$algorithm, "sha1")
})

test_that("boundaryPolygon traces the raster edge", {
  skip_if_not_installed("terra")
  testInit("terra")

  r <- terra::rast(nrows = 5, ncols = 10, xmin = 0, xmax = 10, ymin = 0, ymax = 5,
                   crs = "EPSG:4326", vals = 1)
  bp <- boundaryPolygon(r)

  expect_s4_class(bp, "SpatVector")
  expect_identical(terra::geomtype(bp), "polygons")
  ## The traced boundary spans the raster's own extent.
  expect_equal(as.vector(terra::ext(bp)), as.vector(terra::ext(r)), tolerance = 1e-8)
  expect_identical(terra::crs(bp), terra::crs(r))
})

test_that("a sidecar can record both a digest and an ETag", {
  # They answer different questions: the digest pins the bytes (and can be
  # recomputed locally to confirm a download was not corrupted), while the ETag
  # is the server's own "you already have this" token, usable via If-None-Match
  # even when it is opaque. Record both when the remote offers both.
  testInit(verbose = -1)
  url <- "https://example.com/data/target.tif"

  written <- makeRemoteHashFile(url, tmpdir, "target.tif", strrep("d", 32),
                                algorithm = "md5", write = TRUE,
                                etag = "W/\"opaque-token\"")
  parsed <- .parseRemoteHashFile(written)

  expect_identical(parsed$algorithm, "md5")
  expect_identical(parsed$hash, strrep("d", 32))
  expect_identical(parsed$etag, "W/\"opaque-token\"")

  ## ETag only -> algorithm/hash stay populated so older callers still work
  written2 <- makeRemoteHashFile(url, tmpdir, "other.tif", "W/\"only-etag\"",
                                 algorithm = "etag", write = TRUE)
  parsed2 <- .parseRemoteHashFile(written2)
  expect_identical(parsed2$algorithm, "etag")
  expect_identical(parsed2$hash, "W/\"only-etag\"")
  expect_identical(parsed2$etag, "W/\"only-etag\"")
})

test_that("sidecar names are not lossy: different URLs get different files", {
  # The old scheme dropped the scheme and turned every "/" into "_", so
  # `my_data/sub_dir` and `my/data/sub/dir` collided -- two different URLs
  # sharing one sidecar, and therefore one ETag.
  testInit(verbose = -1)
  f <- getFromNamespace(".remoteHashFilePath", "reproducible")

  a <- f("https://example.com/my_data/sub_dir/DEM.tif", tmpdir, "DEM.tif")
  b <- f("https://example.com/my/data/sub/dir/DEM.tif", tmpdir, "DEM.tif")
  cc <- f("http://example.com/my_data/sub_dir/DEM.tif", tmpdir, "DEM.tif")

  expect_false(identical(a, b))   # "/" vs "_" no longer ambiguous
  expect_false(identical(a, cc))  # scheme is part of the identity
  expect_identical(a, f("https://example.com/my_data/sub_dir/DEM.tif", tmpdir, "DEM.tif"))
})

test_that("a legacy-named sidecar is still found", {
  # Existing caches must not be invalidated by the rename.
  testInit(verbose = -1)
  legacyPath <- getFromNamespace(".legacyRemoteHashFilePath", "reproducible")
  findIt <- getFromNamespace(".findRemoteHashFile", "reproducible")

  url <- "https://example.com/data/DEM.tif"
  leg <- legacyPath(url, tmpdir, "DEM.tif")
  writeLines("md5:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", leg)

  expect_identical(findIt(url, tmpdir, "DEM.tif"), leg)
  expect_identical(.parseRemoteHashFile(findIt(url, tmpdir, "DEM.tif"))$algorithm, "md5")
})

test_that("the sidecar records the URL it came from", {
  testInit(verbose = -1)
  url <- "https://example.com/data/target.tif"
  written <- makeRemoteHashFile(url, tmpdir, "target.tif", strrep("d", 32),
                                algorithm = "md5", write = TRUE,
                                etag = "W/\"tok\"")
  parsed <- .parseRemoteHashFile(written)
  expect_identical(parsed$url, url)
  expect_identical(parsed$hash, strrep("d", 32))
  expect_identical(parsed$etag, "W/\"tok\"")
})

test_that("preProcessCheckURLs acts only on the sidecars whose remote changed", {
  testInit(verbose = -1)
  url <- "https://example.com/data/target.tif"
  sc <- makeRemoteHashFile(url, tmpdir, "target.tif", strrep("d", 32),
                           algorithm = "md5", write = TRUE, etag = "W/\"tok\"")

  ## unchanged -> kept
  testthat::with_mocked_bindings(
    .remoteEtagRevalidate = function(...) list(unchanged = TRUE, etag = "W/\"tok\""),
    { res <- suppressMessages(preProcessCheckURLs(tmpdir, redownload = "nextPreProcess", verbose = -1)) }
  )
  expect_identical(res$status, "unchanged")
  expect_true(file.exists(sc))

  ## unreachable -> kept, reported
  testthat::with_mocked_bindings(
    .remoteEtagRevalidate = function(...) list(unchanged = NA, etag = NULL),
    { res <- suppressMessages(preProcessCheckURLs(tmpdir, redownload = "nextPreProcess", verbose = -1)) }
  )
  expect_identical(res$status, "unreachable")
  expect_true(file.exists(sc))

  ## changed, redownload = "no" -> reported but kept
  testthat::with_mocked_bindings(
    .remoteEtagRevalidate = function(...) list(unchanged = FALSE, etag = "W/\"new\""),
    { res <- suppressMessages(preProcessCheckURLs(tmpdir, redownload = "no", verbose = -1)) }
  )
  expect_identical(res$status, "changed")
  expect_true(file.exists(sc))

  ## changed, redownload = "nextPreProcess" -> sidecar removed so the next
  ## ordinary preProcess() re-downloads just this one
  testthat::with_mocked_bindings(
    .remoteEtagRevalidate = function(...) list(unchanged = FALSE, etag = "W/\"new\""),
    { res <- suppressMessages(preProcessCheckURLs(tmpdir, redownload = "nextPreProcess", verbose = -1)) }
  )
  expect_identical(res$status, "changed")
  expect_false(file.exists(sc))
})

test_that("preProcessCheckURLs reports sidecars that predate the recorded URL", {
  testInit(verbose = -1)
  sc <- file.path(tmpdir, ".old_deadbeef.hash")
  writeLines("md5:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", sc)

  res <- suppressMessages(preProcessCheckURLs(tmpdir, redownload = "nextPreProcess", verbose = -1))
  expect_identical(res$status, "noURL")
  expect_true(file.exists(sc))    # nothing to check against -> left alone
})

test_that("preProcessCheckURLs redownload = 'immediate' fetches the changed file now", {
  testInit(verbose = -1)
  url <- "https://example.com/data/target.tif"
  sc <- makeRemoteHashFile(url, tmpdir, "target.tif", strrep("d", 32),
                           algorithm = "md5", write = TRUE, etag = "W/\"tok\"")
  fetched <- 0L

  testthat::with_mocked_bindings(
    .remoteEtagRevalidate = function(...) list(unchanged = FALSE, etag = "W/\"new\""),
    preProcess = function(...) { fetched <<- fetched + 1L; invisible(NULL) },
    { res <- suppressMessages(preProcessCheckURLs(tmpdir, redownload = "immediate",
                                                  verbose = -1)) }
  )

  expect_identical(res$status, "changed")
  expect_identical(res$action, "redownloaded")
  expect_identical(fetched, 1L)
})

test_that("preProcessCheckURLs reports a failed immediate redownload rather than erroring", {
  testInit(verbose = -1)
  url <- "https://example.com/data/target.tif"
  makeRemoteHashFile(url, tmpdir, "target.tif", strrep("d", 32),
                     algorithm = "md5", write = TRUE, etag = "W/\"tok\"")

  testthat::with_mocked_bindings(
    .remoteEtagRevalidate = function(...) list(unchanged = FALSE, etag = "W/\"new\""),
    preProcess = function(...) stop("remote exploded"),
    { res <- expect_no_error(
        suppressMessages(preProcessCheckURLs(tmpdir, redownload = "immediate",
                                             verbose = -1))) }
  )

  expect_identical(res$status, "changed")
  expect_identical(res$action, "redownloadFailed")
})

test_that("preProcessCheckURLs leaves unchanged files alone whatever redownload says", {
  testInit(verbose = -1)
  url <- "https://example.com/data/target.tif"
  sc <- makeRemoteHashFile(url, tmpdir, "target.tif", strrep("d", 32),
                           algorithm = "md5", write = TRUE, etag = "W/\"tok\"")

  for (rd in c("immediate", "nextPreProcess", "no")) {
    testthat::with_mocked_bindings(
      .remoteEtagRevalidate = function(...) list(unchanged = TRUE, etag = "W/\"tok\""),
      { res <- suppressMessages(preProcessCheckURLs(tmpdir, redownload = rd, verbose = -1)) }
    )
    expect_identical(res$status, "unchanged", info = rd)
    expect_identical(res$action, "none", info = rd)
    expect_true(file.exists(sc), info = rd)
  }
})
