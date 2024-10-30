if (requireNamespace("terra", quietly = TRUE) && requireNamespace("sf", quietly = TRUE)) {
  library(reproducible)
  od <- setwd(tempdir2())
  # download a (spatial) file from remote url (which often is an archive) load into R
  # need 3 files for this example; 1 from remote, 2 local
  dPath <- file.path(tempdir2())
  remoteTifUrl <- "https://github.com/rspatial/terra/raw/master/inst/ex/elev.tif"

  localFileLuxSm <- system.file("ex/luxSmall.shp", package = "reproducible")
  localFileLux <- system.file("ex/lux.shp", package = "terra")

  # 1 step for each layer
  # 1st step -- get study area
  studyArea <- prepInputs(localFileLuxSm, fun = "terra::vect") # default is sf::st_read

  # 2nd step: make the input data layer like the studyArea map
  # Test only relevant if connected to internet -- so using try just in case
  elevForStudy <- try(prepInputs(url = remoteTifUrl, to = studyArea, res = 250,
                             destinationPath = dPath, useCache = FALSE))

  # Alternate way, one step at a time. Must know each of these steps, and perform for each layer
  \donttest{
    dir.create(dPath, recursive = TRUE, showWarnings = FALSE)
    file.copy(localFileLuxSm, file.path(dPath, basename(localFileLuxSm)))
    studyArea2 <- terra::vect(localFileLuxSm)
    if (!all(terra::is.valid(studyArea2))) studyArea2 <- terra::makeValid(studyArea2)
    tf <- tempfile(fileext = ".tif")
    download.file(url = remoteTifUrl, destfile = tf, mode = "wb", quiet = TRUE)
    Checksums(dPath, write = TRUE, files = tf)
    elevOrig <- terra::rast(tf)
    studyAreaCrs <- terra::crs(studyArea)
    elevForStudy2 <- terra::project(elevOrig, studyAreaCrs, res = 250) |>
      terra::mask(studyArea2) |>
      terra::crop(studyArea2)

    isTRUE(all.equal(elevForStudy, elevForStudy2)) # TRUE!
  }

  # sf class
  studyAreaSmall <- prepInputs(localFileLuxSm)
  studyAreas <- list()
  studyAreas[["orig"]] <- prepInputs(localFileLux)
  studyAreas[["reprojected"]] <- projectTo(studyAreas[["orig"]], studyAreaSmall)
  studyAreas[["cropped"]] <- suppressWarnings(cropTo(studyAreas[["orig"]], studyAreaSmall))
  studyAreas[["masked"]] <- suppressWarnings(maskTo(studyAreas[["orig"]], studyAreaSmall))

  # SpatVector-- note: doesn't matter what class the "to" object is, only the "from"
  studyAreas <- list()
  studyAreas[["orig"]] <- prepInputs(localFileLux, fun = "terra::vect")
  studyAreas[["reprojected"]] <- projectTo(studyAreas[["orig"]], studyAreaSmall)
  studyAreas[["cropped"]] <- suppressWarnings(cropTo(studyAreas[["orig"]], studyAreaSmall))
  studyAreas[["masked"]] <- suppressWarnings(maskTo(studyAreas[["orig"]], studyAreaSmall))
  if (interactive()) {
    par(mfrow = c(2,2));
    out <- lapply(studyAreas, function(x) terra::plot(x))
  }

  setwd(od)
}
