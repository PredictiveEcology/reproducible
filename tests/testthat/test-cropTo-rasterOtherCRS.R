## cropTo() with a raster `cropTo` in another CRS only needs that raster's extent in the
## CRS of `from`. It used to terra::project() the whole raster to get it, which was slow
## and off by up to one output cell. It now projects points along the extent's edges.
## A lon/lat `cropTo` is the hard case: its edges are lines of latitude, so the 4 corners
## (or terra::densify(), which follows great circles) miss most of the southern bulge.

test_that("cropTo with a lon/lat raster covers its true footprint without projecting cells", {
  testInit("terra", needGoogleDriveAuth = FALSE)

  ## `from` must be larger than the footprint, or the crop stops at its edges
  from <- terra::rast(xmin = -6e6, xmax = 6e6, ymin = -3e6, ymax = 8e6, resolution = 10000,
                      crs = "EPSG:3978", vals = 1)
  cropToLL <- terra::rast(xmin = -140, xmax = -50, ymin = 42, ymax = 70, resolution = 0.25,
                          crs = "EPSG:4326", vals = 1)

  ## true footprint: every cell corner of cropToLL, projected
  xs <- seq(terra::xmin(cropToLL), terra::xmax(cropToLL), by = terra::res(cropToLL)[1])
  ys <- seq(terra::ymin(cropToLL), terra::ymax(cropToLL), by = terra::res(cropToLL)[2])
  truth <- terra::ext(terra::project(terra::vect(as.matrix(expand.grid(xs, ys)),
                                                 crs = "EPSG:4326"), "EPSG:3978"))

  ## the tracer runs in terra's frame, so record the call in an environment it can reach
  calls <- new.env()
  calls$projectedRaster <- FALSE
  suppressMessages(trace("project", signature = "SpatRaster", where = asNamespace("terra"),
                         print = FALSE,
                         tracer = bquote(assign("projectedRaster", TRUE, envir = .(calls)))))
  on.exit(suppressMessages(untrace("project", signature = "SpatRaster",
                                   where = asNamespace("terra"))), add = TRUE)

  out <- cropTo(from, cropToLL, verbose = FALSE)

  expect_false(calls$projectedRaster)
  ## crop snaps to the nearest `from` cell edge, so every side is within half a cell of
  ## the footprint; the 4 corners (or terra::densify()) miss ymin by ~1,100 km here
  e <- as.vector(terra::ext(out))
  halfCell <- rep(terra::res(from) / 2, each = 2)
  expect_true(all(abs(e - as.vector(truth)) <= halfCell),
              info = paste("cropped:", paste(e, collapse = ", "),
                           "| footprint:", paste(round(as.vector(truth)), collapse = ", ")))
})
