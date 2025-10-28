if (require("terra", quietly = TRUE)) {
  # prepare dummy data -- 3 SpatRasters, 2 SpatVectors
  # need 2 SpatRaster
  rf <- system.file("ex/elev.tif", package = "terra")
  elev1 <- terra::rast(rf)
  
  ras2 <- terra::deepcopy(elev1)
  ras2[ras2 > 200 & ras2 < 300] <- NA_integer_
  terra::values(elev1) <- rep(1L, terra::ncell(ras2))
  
  # a polygon vector
  f <- system.file("ex/lux.shp", package = "terra")
  vOrig <- terra::vect(f)
  v <- vOrig[1:2, ]
  
  utm <- terra::crs("epsg:23028") # $wkt
  vInUTM <- terra::project(vOrig, utm)
  vAsRasInLongLat <- terra::rast(vOrig, resolution = 0.008333333)
  res100 <- 100
  rInUTM <- terra::rast(vInUTM, resolution = res100, vals = 1)
  # crop, reproject, mask, crop a raster with a vector in a different projection
  #  --> gives message about not enough information
  t1 <- postProcessTo(elev1, to = vInUTM)
  # crop, reproject, mask a raster to a different projection, then mask
  t2a <- postProcessTo(elev1, to = vAsRasInLongLat, maskTo = vInUTM)

  # using gdal directly --> slightly different mask
  opts <- options(reproducible.gdalwarp = TRUE)
  t2b <- postProcessTo(elev1, to = vAsRasInLongLat, maskTo = vInUTM)
  t3b <- postProcessTo(elev1, to = rInUTM, maskTo = vInUTM)
  options(opts)
}
