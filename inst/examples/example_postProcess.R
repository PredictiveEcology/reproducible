if (requireNamespace("terra", quietly = TRUE) &&
    requireNamespace("withr", quietly = TRUE)) {
  library(reproducible)
  withr::local_dir(withr::local_tempdir())
  withr::local_options(reproducible.inputPaths = NULL)
  # od <- setwd(tempdir2())
  # download a (spatial) file from remote url (which often is an archive) load into R
  # need 3 files for this example; 1 from remote, 2 local
  dPath <- file.path(tempdir2())
  # the `CRAN` tag tracks terra's current CRAN release; `master` moves and is flakier
  remoteTifUrl <- "https://github.com/rspatial/terra/raw/CRAN/inst/ex/elev.tif"

  localFileLuxSm <- system.file("ex/luxSmall/luxSmall.shp", package = "reproducible")
  localFileLux <- system.file("ex/lux.shp", package = "terra")

  # 1 step for each layer
  # 1st step -- get study area
  studyArea <- prepInputs(localFileLuxSm, fun = "terra::vect") # default is sf::st_read

  # Requires internet -- gated on NOT_CRAN so CRAN's machines are never asked to
  #   reach a third-party server, and wrapped in try() for when the server is down
  if (identical(Sys.getenv("NOT_CRAN"), "true")) {
    # 2nd step: make the input data layer like the studyArea map
    elevForStudy <- try(prepInputs(url = remoteTifUrl, to = studyArea, res = 250,
                                   destinationPath = dPath, useCache = FALSE))
  }

  ## The single call above is equivalent to all the steps below, which must be known and
  ##   repeated for every layer. Left commented out: it is an explanation, not a test.
  ##
  ## dir.create(dPath, recursive = TRUE, showWarnings = FALSE)
  ## file.copy(localFileLuxSm, file.path(dPath, basename(localFileLuxSm)))
  ## studyArea2 <- terra::vect(localFileLuxSm)
  ## if (!all(terra::is.valid(studyArea2))) studyArea2 <- terra::makeValid(studyArea2)
  ## tf <- tempfile(fileext = ".tif")
  ## download.file(url = remoteTifUrl, destfile = tf, mode = "wb", quiet = TRUE)
  ## Checksums(dPath, write = TRUE, files = tf)
  ## elevOrig <- terra::rast(tf)
  ## studyAreaCrs <- terra::crs(studyArea)
  ## # Build an explicit target raster (same CRS as studyArea, res = 250) and project to
  ## # it. Avoids the recursive `terra::project(x, char_crs, res = N)` shorthand that
  ## # recent terra (~1.9) regressed with `[write] unknown option(s): xscale,yscale`.
  ## elevTarget <- terra::project(terra::rast(elevOrig), studyAreaCrs)
  ## elevTarget <- terra::rast(terra::ext(elevTarget),
  ##                           crs = terra::crs(elevTarget),
  ##                           resolution = 250)
  ## elevForStudy2 <- terra::project(elevOrig, elevTarget) |>
  ##   terra::mask(studyArea2) |>
  ##   terra::crop(studyArea2)
  ##
  ## isTRUE(all.equal(elevForStudy, elevForStudy2)) # TRUE!

  # sf class
  if (requireNamespace("sf", quietly = TRUE)) {
    studyAreaSmall <- prepInputs(localFileLuxSm, fun = "sf::st_read")
    studyAreas <- list()
    studyAreas[["orig"]] <- prepInputs(localFileLux)
    studyAreas[["reprojected"]] <- projectTo(studyAreas[["orig"]], studyAreaSmall)
    studyAreas[["cropped"]] <- suppressWarnings(cropTo(studyAreas[["orig"]], studyAreaSmall))
    studyAreas[["masked"]] <- suppressWarnings(maskTo(studyAreas[["orig"]], studyAreaSmall))
  }

  # SpatVector-- note: doesn't matter what class the "to" object is, only the "from"
  studyAreas <- list()
  studyAreaSmall <- prepInputs(localFileLuxSm)
  studyAreas[["orig"]] <- prepInputs(localFileLux)
  studyAreas[["reprojected"]] <- projectTo(studyAreas[["orig"]], studyAreaSmall)
  studyAreas[["cropped"]] <- suppressWarnings(cropTo(studyAreas[["orig"]], studyAreaSmall))
  studyAreas[["masked"]] <- suppressWarnings(maskTo(studyAreas[["orig"]], studyAreaSmall))
  if (interactive()) {
    par(mfrow = c(2,2));
    out <- lapply(studyAreas, function(x) terra::plot(x))
  }

  withr::deferred_run()
  # setwd(od)
}
