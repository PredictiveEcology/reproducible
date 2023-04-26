# This is an incomplete file; it will be slowly transitioned to have all messaging here
# Any new message should be written as a .msgGrep entry, then used in the functions
# with the mess*

messSkipDownload <- "Skipping download of url; local copy already exists and passes checksums"

.msgGrep <- list(
  studyArea_Spatial = "The \\'studyArea\\' provided is not a Spatial\\* object.",
  rasterToMatch_Raster = "The \\'rasterToMatch\\' provided is not a Raster\\* object.",
  anySpatialClass = "Raster\\*, Spat\\*, sf or Spatial object"
)

.msg <- lapply(.msgGrep, gsub, pattern = "\\\\", replacement = "")
