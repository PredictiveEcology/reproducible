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
  dPath2 <- checkPath(file.path(tempdir2()), create = TRUE)

  cloudFolderID <- "https://drive.google.com/drive/folders/1An8s2YLFPopQKr4BWK9o06fLSXx-Zggw"
  targetFile <- "fireSenseParams.rds"
  localFileLux <- system.file("ex/lux.shp", package = "terra")

  # 1 step for each layer
  # 1st step -- get study area
  (full <- prepInputs(localFileLux, destinationPath = dPath)) |> capture.output() -> co# default is sf::st_read
  zoneA <- full[3:6, c("NAME_1", "AREA")]
  zoneB <- full[8, c("NAME_1", "AREA")] # not in A
  zoneC <- full[3, c("NAME_1", "AREA")] # yes in A
  zoneD <- full[7:8, c("NAME_1", "AREA")] # not in A, B or C
  zoneE <- full[3:5, c("NAME_1", "AREA")] # yes in A

  # This will be 1, 2 and 3 -- THIS IS THE INTERESTING ONE ... it will mean that a
  #  test below will have 2 different polygons the are "contains", so, result of
  #  Cache will be not just one polygon, but 2
  zoneF <- aggregate(full[, c("AREA")], by = list(NAME_1 = c(rep(1,3), rep(2,NROW(full) - 3))), sum)
  zoneF <- zoneF[zoneF$NAME_1 == 1,]

  # zoneF[, "AREA"] <- sf::st_area(zoneF)/1e6
  # 2nd step: re-write to disk as read/write is lossy; want all "from disk" for this ex.
  co <- capture.output({
    writeTo(zoneA, writeTo = "zoneA.shp", destinationPath = dPath)
    writeTo(zoneB, writeTo = "zoneB.shp", destinationPath = dPath)
    writeTo(zoneC, writeTo = "zoneC.shp", destinationPath = dPath)
    writeTo(zoneD, writeTo = "zoneD.shp", destinationPath = dPath)
    writeTo(zoneE, writeTo = "zoneE.shp", destinationPath = dPath)
    writeTo(zoneF, writeTo = "zoneF.shp", destinationPath = dPath)
    # Must re-read to get identical columns
    zoneA <- sf::st_read(file.path(dPath, "zoneA.shp"))
    zoneB <- sf::st_read(file.path(dPath, "zoneB.shp"))
    zoneC <- sf::st_read(file.path(dPath, "zoneC.shp"))
    zoneD <- sf::st_read(file.path(dPath, "zoneD.shp"))
    zoneE <- sf::st_read(file.path(dPath, "zoneE.shp"))
    zoneF <- sf::st_read(file.path(dPath, "zoneF.shp"))
  })

  # The function that is to be run. This example returns a data.frame because
  #    saving `sf` class objects with list-like columns does not work with
  #    many st_driver()
  fun <- function(domain, newField) {
    domain |>
      as.data.frame() |>
      cbind(params = I(lapply(seq_len(NROW(domain)), function(x) newField)))
  }

  fun2 <- function(domain, newField) {
    domain |> as.data.frame() |>
      dplyr::mutate(params2 = list(list(a = seq_len(NROW(domain)),
                                    b = LETTERS[seq_len(NROW(domain))],
                                    d = TRUE)))
  }

  # Run sequence -- A, B will add new entries in targetFile, C will not,
  #                 D will, E will not
  for (z in list(zoneA, zoneB, zoneC, zoneD, zoneE, zoneF)) {
    if (identical(z, zoneA) || identical(z, zoneB) || identical(z, zoneD) || identical(z, zoneF)) {
      mess <- "Domain is not contained within the targetFile"
    }
    if (identical(z, zoneC) || identical(z, zoneE)) {
      mess <- "Spatial domain is contained within the url"
    }
    expect_message(out <- CacheGeo(
      targetFile = targetFile,
      domain = z,
      FUN = fun2(domain, newField = I(list(list(a = 1, b = 1:2, c = "D")))),
      fun2 = fun2, # pass whatever is needed into the function
      destinationPath = dPath,
      action = "update"
    ), mess)

  }

  outSF <- sf::st_as_sf(out)

  gls <- googledrive::drive_ls(cloudFolderID)
  alreadyThere <- gls$name %in% targetFile
  if (any(alreadyThere)) {
    googledrive::drive_rm(gls$id[[which(alreadyThere)]])
  }
  on.exit({
    gls <- googledrive::drive_ls(cloudFolderID)
    googledrive::drive_rm(gls[gls$name %in% targetFile,])
  })



  for (z in list(zoneA, zoneB, zoneC, zoneD, zoneE, zoneF)) {
    if (identical(z, zoneA) || identical(z, zoneB) || identical(z, zoneD) || identical(z, zoneF)) {
      mess <- "Domain is not contained within the targetFile"
    }
    if (identical(z, zoneC) || identical(z, zoneE)) {
      mess <- "Spatial domain is contained within the url"
    }
    # With directory url
    out <- CacheGeo(
      # url = "https://drive.google.com/file/d/1st4lUiCgXJp8SdpMP056smPOh9gbZEoQ",
      targetFile = targetFile,
      domain = z,
      useCloud = TRUE,
      cloudFolderID = cloudFolderID,
      FUN = fun2(domain, newField = I(list(list(a = 1, b = 1:2, c = "D")))),
      fun2 = fun2, # pass whatever is needed into the function
      destinationPath = dPath2,
      action = "update"
    )

  }
  outSFCloud <- sf::st_as_sf(out)
  expect_true(identical(outSFCloud, outSF))

  keeps <- sf::st_contains(outSF, outSF[1, 1], sparse = FALSE)

  polysWithParams <- outSF[keeps, ]

  expect_true(NROW(polysWithParams) == 2)

  smaller <- sf::st_as_sf(terra::buffer(terra::vect(polysWithParams[1, ]), width = -2000))

  plot(polysWithParams[2, 1], reset = FALSE)
  plot(polysWithParams[1, 1], add = TRUE, col = "red", reset = FALSE)
  smaller <- sf::st_as_sf(terra::buffer(terra::vect(polysWithParams[1, ]), width = -2000))
  plot(smaller[1, 1], add = TRUE, col = "green")

  out <- CacheGeo(
    targetFile = targetFile,
    domain = smaller,
    useCloud = TRUE,
    cloudFolderID = "https://drive.google.com/drive/folders/1An8s2YLFPopQKr4BWK9o06fLSXx-Zggw",
    FUN = fun(domain, newField = I(list(list(a = 1, b = 1:2, c = "D")))),
    fun = fun, # pass whatever is needed into the function
    destinationPath = dPath2,
    action = "nothing"
  )
  outSFCloudSmaller <- sf::st_as_sf(out)
  out[, "params2"]
  expect_identical(as.data.frame(outSFCloudSmaller)[, "params2"], out[, "params2"])

})



