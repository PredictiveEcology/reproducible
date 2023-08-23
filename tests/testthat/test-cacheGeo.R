test_that("lightweight tests for code coverage", {
  skip_on_cran()
  testInit(c("sf", "terra"),
    opts = list(
      "reproducible.overwrite" = TRUE,
      "reproducible.inputPaths" = NULL
    ),
    needGoogleDriveAuth = TRUE
  )
  dPath <- checkPath(file.path(tempdir2()), create = TRUE)
  localFileLux <- system.file("ex/lux.shp", package = "terra")

  # 1 step for each layer
  # 1st step -- get study area
  full <- prepInputs(localFileLux, dest = dPath) # default is sf::st_read
  zoneA <- full[3:6, ]
  zoneB <- full[8, ] # not in A
  zoneC <- full[3, ] # yes in A
  zoneD <- full[7:8, ] # not in A, B or C
  zoneE <- full[3:5, ] # yes in A
  # 2nd step: re-write to disk as read/write is lossy; want all "from disk" for this ex.
  co <- capture.output({
    writeTo(zoneA, writeTo = "zoneA.shp", destinationPath = dPath)
    writeTo(zoneB, writeTo = "zoneB.shp", destinationPath = dPath)
    writeTo(zoneC, writeTo = "zoneC.shp", destinationPath = dPath)
    writeTo(zoneD, writeTo = "zoneD.shp", destinationPath = dPath)
    writeTo(zoneE, writeTo = "zoneE.shp", destinationPath = dPath)
    # Must re-read to get identical columns
    zoneA <- sf::st_read(file.path(dPath, "zoneA.shp"))
    zoneB <- sf::st_read(file.path(dPath, "zoneB.shp"))
    zoneC <- sf::st_read(file.path(dPath, "zoneC.shp"))
    zoneD <- sf::st_read(file.path(dPath, "zoneD.shp"))
    zoneE <- sf::st_read(file.path(dPath, "zoneE.shp"))
  })

  # The function that is to be run. This example returns a data.frame because
  #    saving `sf` class objects with list-like columns does not work with
  #    many st_driver()
  fun <- function(domain, newField) {
    domain |>
      as.data.frame() |>
      cbind(params = I(lapply(seq_len(NROW(domain)), function(x) newField)))
  }

  # Run sequence -- A, B will add new entries in targetFile, C will not,
  #                 D will, E will not
  for (z in list(zoneA, zoneB, zoneC, zoneD, zoneE)) {
    if (identical(z, zoneA) || identical(z, zoneB) || identical(z, zoneD)) {
      mess <- "Domain is not contained within the targetFile"
    }
    if (identical(z, zoneC) || identical(z, zoneE)) {
      mess <- "Spatial domain is contained within the url"
    }
    expect_message(out <- CacheGeo(
      targetFile = "fireSenseParams.rds",
      domain = z,
      FUN = fun(domain, newField = I(list(list(a = 1, b = 1:2, c = "D")))),
      fun = fun, # pass whatever is needed into the function
      destinationPath = dPath,
      action = "update"
    ), mess)
  }
})
