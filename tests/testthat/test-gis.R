test_that("fastMask produces correct results", {
  Sr1 <- sp::Polygon(cbind(c(2, 4, 4, 1, 2), c(2, 3, 5, 4, 2))) # nolint
  Sr2 <- sp::Polygon(cbind(c(5, 4, 2, 5), c(2, 3, 2, 2))) # nolint
  Sr3 <- sp::Polygon(cbind(c(4, 4, 5, 7.4, 4), c(5, 3, 2, 5, 5))) # nolint

  Srs1 <- sp::Polygons(list(Sr1), "s1") # nolint
  Srs2 <- sp::Polygons(list(Sr2), "s2") # nolint
  Srs3 <- sp::Polygons(list(Sr3), "s3") # nolint

  shp <- sp::SpatialPolygons(list(Srs1, Srs2, Srs3), 1:3)
  d <- data.frame(vals = 1:3, other = letters[1:3])
  row.names(d) <- names(shp)
  shpDF <- sp::SpatialPolygonsDataFrame(shp, data = d)
  poly <- list()
  poly[[1]] <- raster::raster(raster::extent(shpDF), vals = 0, res = c(0.5, 0.5))
  poly[[2]] <- raster::raster(raster::extent(shpDF), vals = 1, res = c(0.5, 0.5))
  origStack <- raster::stack(poly)

  ## mask
  newStack1 <- raster::stack(raster::mask(origStack, mask = shpDF))
  newStack2 <- fastMask(x = origStack, y = shpDF)
  expect_equal(newStack1, newStack2)

  newStack1 <- raster::mask(origStack[[2]], mask = shpDF)
  newStack2 <- fastMask(x = origStack[[2]], y = shpDF)
  expect_equivalent(newStack1, newStack2)

  crs(shpDF) <- "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGS84"
  crs(shp) <- "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGS84"
  crs(origStack[[2]]) <- "+proj=lcc +lat_1=49 +lat_2=33 +lon_0=-100 +ellps=WGS84"

  newStack2 <- fastMask(x = origStack[[2]], y = shpDF)

  # test non-spatial polygons data frame
  newStack2 <- fastMask(x = origStack[[2]], y = shp)

  ### getGDALversion
  expect_silent(a <- getGDALVersion())
  if (is.na(a)) {
    expect_true(is.numeric(a))
  } else {
    expect_true(is(a, "numeric_version"))
  }

  expect_true(checkGDALVersion("1.0.0"))
  expect_error(checkGDALVersion())
  expect_false(checkGDALVersion("4.0.0"))

})

