# Add a study area to Crop and Mask to
# Create a "study area"
library(sp)
library(raster)
ow <- setwd(tempdir())

# make a SpatialPolygon
coords1 <- structure(c(-123.98, -117.1, -80.2, -100, -123.98, 60.9, 67.73, 65.58, 51.79, 60.9),
                     .Dim = c(5L, 2L))
Sr1 <- Polygon(coords1)
Srs1 <- Polygons(list(Sr1), "s1")
shpEcozone <- SpatialPolygons(list(Srs1), 1L)
crs(shpEcozone) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# make a "study area" that is subset of larger dataset
coords <- structure(c(-118.98, -116.1, -99.2, -106, -118.98, 59.9, 65.73, 63.58, 54.79, 59.9),
                    .Dim = c(5L, 2L))
Sr1 <- Polygon(coords)
Srs1 <- Polygons(list(Sr1), "s1")
StudyArea <- SpatialPolygons(list(Srs1), 1L)
crs(StudyArea) <- crs(shpEcozone)
projString <- "+proj=utm +zone=15 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
StudyArea <- sp::spTransform(StudyArea, CRSobj = projString)

##########
shpEcozonePostProcessed <- postProcess(shpEcozone, studyArea = StudyArea)


# Try manually, individual pieces
shpEcozoneReprojected <- projectInputs(shpEcozone, StudyArea)
shpEcozoneCropped <- cropInputs(shpEcozone, StudyArea)
shpEcozoneClean <- fixErrors(shpEcozone)
shpEcozoneMasked <- maskInputs(shpEcozone, StudyArea)

# With terra
if (require("terra")) {
  vectEcozone <- terra::vect(sf::st_as_sf(shpEcozone)) # direct conversion throws warning

  # If input is Spatial object --> return will also be Spatial
  shpEcozonePostProcessed <- postProcess(shpEcozone, studyArea = StudyArea)
  # Try manually, individual pieces -- Note functions are different
  shpEcozoneReprojected <- projectInputs(shpEcozone, StudyArea)
  shpEcozoneMasked <- maskInputs(shpEcozone, StudyArea)
  shpEcozoneCropped <- cropInputs(shpEcozone, StudyArea)

  # If input is Spat object --> return will also be Spat
  vectEcozonePostProcessed <- postProcess(vectEcozone, studyArea = StudyArea)
  # Try manually, individual pieces -- Note functions are different
  vectEcozoneMasked <- maskInputs(vectEcozone, StudyArea)
  VectEcozoneReprojected <- projectInputs(vectEcozone, StudyArea)
  vectEcozoneCropped <- cropInputs(vectEcozone, StudyArea)



  # Note these two have different function names --> methods for cropInputs and fixErrors
  #    are not implemented yet
  shpEcozoneClean <- fixErrorsTerra(vectEcozone)

  options(opts)
}

setwd(ow)
