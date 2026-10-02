## postProcessTo(rasterize = ...): a Vector `from` onto a Gridded `to`.

rasterizeFixture <- function() {
  to <- terra::rast(nrows = 10, ncols = 10, xmin = 0, xmax = 10, ymin = 0, ymax = 10,
                    crs = "EPSG:3005", vals = 1)
  to[1:10] <- NA # top row is outside the study area: masked
  p1 <- terra::as.polygons(terra::ext(0, 5, 0, 10), crs = "EPSG:3005")
  p2 <- terra::as.polygons(terra::ext(3, 10, 0, 10), crs = "EPSG:3005")
  from <- rbind(p1, p2)
  from$R <- c(100, 200)
  list(to = to, from = from)
}

test_that("rasterize = TRUE gives a SpatRaster on the grid of `to`, masked by it", {
  skip_if_not_installed("terra")
  f <- rasterizeFixture()
  out <- postProcessTo(f$from, f$to, rasterize = TRUE, verbose = -2)
  expect_s4_class(out, "SpatRaster")
  expect_true(terra::compareGeom(out, f$to, stopOnError = FALSE))
  v <- terra::values(out)[, 1]
  expect_true(all(is.na(v[1:10])))  # masked by the NA row of `to`
  expect_true(all(v[-(1:10)] == 1))  # every other cell is covered by a polygon
})

test_that("rasterize as a list passes field and fun to terra::rasterize", {
  skip_if_not_installed("terra")
  f <- rasterizeFixture()
  out <- postProcessTo(f$from, f$to, rasterize = list(field = "R", fun = "max"), verbose = -2)
  m <- matrix(terra::values(out)[, 1], nrow = 10, byrow = TRUE)
  expect_true(all(m[-1, 1:3] == 100))  # p1 only
  expect_true(all(m[-1, 4:5] == 200))  # overlap: the max
  expect_true(all(m[-1, 6:10] == 200)) # p2 only
})

test_that("rasterize works from sf and a different crs, and writes a raster", {
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")
  f <- rasterizeFixture()
  fromSF <- sf::st_as_sf(terra::project(f$from, "EPSG:4326"))
  tmp <- withr::local_tempfile(fileext = ".tif")
  out <- postProcessTo(fromSF, f$to, rasterize = list(field = "R", fun = "max"),
                       writeTo = tmp, verbose = -2)
  expect_s4_class(out, "SpatRaster")
  expect_true(file.exists(tmp))
  expect_setequal(unique(na.omit(terra::values(terra::rast(tmp))[, 1])), c(100, 200))
})

test_that("rasterize needs a Vector from and a Gridded to", {
  skip_if_not_installed("terra")
  f <- rasterizeFixture()
  expect_error(postProcessTo(f$to, f$to, rasterize = TRUE, verbose = -2), "rasterize needs")
  toV <- terra::as.polygons(terra::ext(f$to), crs = "EPSG:3005")
  expect_error(postProcessTo(f$from, toV, rasterize = TRUE, verbose = -2), "rasterize needs")
})
