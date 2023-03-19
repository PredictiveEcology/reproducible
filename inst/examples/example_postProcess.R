# Add a study area to Crop and Mask to
# Create a "study area"
library(reproducible)
# download a zip file from internet, unzip all files, load as shapefile, Cache the call
# First time: don't know all files - prepInputs will guess, if download file is an archive,
#   then extract all files, then if there is a .shp, it will load with raster::shapefile
dPath <- file.path(tempdir(), "ecozones")
shpUrl <- "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip"

# Wrapped in a try because this particular url can be flaky
shpEcozone <- try(prepInputs(destinationPath = dPath,
                             url = shpUrl))

# Add a study area to Crop and Mask to
# Create a "study area"
coords <- structure(c(-122.98, -116.1, -99.2, -106, -122.98, 59.9, 65.73, 63.58, 54.79, 59.9),
                    .Dim = c(5L, 2L))
studyArea <- terra::vect(coords, "polygons")
terra::crs(studyArea) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# With terra
if (require("terra")) {
  opts <- options("reproducible.useTerra" = TRUE)
  vectEcozone <- terra::vect(shpEcozone)

  # warnings: "attribute variables are assumed to be spatially constant throughout all geometries"
  #  --> these are relevant for a user to know, but can be safely ignored in examples
  # If input is Spatial object --> return will also be Spatial
  shpEcozonePostProcessed <- suppressWarnings(postProcessTo(vectEcozone, studyArea = studyArea))
  # Try manually, individual pieces -- Note functions are different
  shpEcozoneReprojected <- projectTo(shpEcozone, studyArea)
  shpEcozoneMasked <- suppressWarnings(maskTo(shpEcozone, studyArea))
  shpEcozoneCropped <- suppressWarnings(cropTo(shpEcozone, studyArea))

  # If input is Spat object --> return will also be Spat
  vectEcozonePostProcessed <- postProcessTo(vectEcozone, studyArea = studyArea)
  # Try manually, individual pieces -- Note functions are different
  vectEcozoneMasked <- maskTo(vectEcozone, studyArea)
  VectEcozoneReprojected <- projectTo(vectEcozone, studyArea)
  vectEcozoneCropped <- cropTo(vectEcozone, studyArea)

  # fixErrorsIn --> generally not called on its own
  shpEcozoneClean <- fixErrorsIn(vectEcozone)

  options(opts)
}

