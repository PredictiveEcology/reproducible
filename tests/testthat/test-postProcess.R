test_that("prepInputs doesn't work (part 3)", {

  if (requireNamespace("rgeos")) {
    testInitOut <- testInit(c("raster", "sf", "rgeos"), opts = list(
      "rasterTmpDir" = file.path(tempdir(), "raster"),
      "reproducible.inputPaths" = NULL,
      "reproducible.overwrite" = TRUE)
    )
    on.exit({
      testOnExit(testInitOut)
    }, add = TRUE)

    options("reproducible.cachePath" = tmpdir)

    # Add a study area to Crop and Mask to
    # Create a "study area"
    coords <- structure(c(-122.98, -116.1, -99.2, -106, -122.98, 59.9, 65.73, 63.58, 54.79, 59.9),
                        .Dim = c(5L, 2L))
    coords2 <- structure(c(-115.98, -116.1, -99.2, -106, -122.98, 59.9, 65.73, 63.58, 54.79, 59.9),
                        .Dim = c(5L, 2L))
    Sr1 <- Polygon(coords)
    Srs1 <- Polygons(list(Sr1), "s1")
    StudyArea <- SpatialPolygons(list(Srs1), 1L)
    crs(StudyArea) <- crsToUse

    Sr1 <- Polygon(coords2)
    Srs1 <- Polygons(list(Sr1), "s1")
    StudyArea2 <- SpatialPolygons(list(Srs1), 1L)
    crs(StudyArea2) <- crsToUse

    nonLatLongProj <- "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"
    #dPath <- file.path(tmpdir, "ecozones")
    nc <- st_as_sf(StudyArea)#system.file("shape/nc.shp", package="sf"))
    nc1 <- st_transform(nc, nonLatLongProj)
    #ncSmall <- st_buffer(nc1, dist = -10000)
    ncSmall <- st_as_sf(StudyArea2)
    ncSmall <- st_transform(ncSmall, nonLatLongProj)
    ncSmall <- st_buffer(ncSmall, dist = -10000)
    b <- postProcess(nc1, studyArea = ncSmall, filename2 = NULL)
    expect_true(is(b, "sf"))
    expect_true(identical(extent(b), extent(ncSmall)))
    expect_true(st_area(b) < st_area(nc1))

    r <- raster(nc1, res = 1000)
    rSmall <- raster(ncSmall, res = 1000)

    # now raster with sf
    if (requireNamespace("fasterize")) {
      r1 <- fasterize::fasterize(nc1, r)
      r2 <- postProcess(r1, studyArea = ncSmall, filename2 = NULL)
      expect_true(is(r2, "RasterLayer"))
      expect_true(ncell(r2) < ncell(r1))
      expect_true( (xmin(extent(ncSmall)) - xmin(r2)) < res(r2)[1] * 2)
      expect_true( (ymin(extent(ncSmall)) - ymin(r2)) < res(r2)[2] * 2)
      expect_true( (ymax(extent(ncSmall)) - ymax(r2)) > -(res(r2)[2] * 2))
      expect_true( (xmax(extent(ncSmall)) - xmax(r2)) > -(res(r2)[2] * 2))


      # postProcess
      expect_true(identical(1, postProcess(1)))
      expect_true(identical(list(1, 1), postProcess(list(1, 1))))
      expect_error(postProcess(nc1, rasterToMatch = r), "sf class objects are not yet")
      nc2 <- postProcess(nc1, studyArea = as(ncSmall, "Spatial"))
      expect_true(identical(st_area(nc2), st_area(ncSmall)))

      # cropInputs
      expect_true(identical(1, cropInputs(1)))
      nonLatLongProj2 <- "+proj=lcc +lat_1=51 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"
      nc3 <- spTransform(as(nc1, "Spatial"), CRSobj = CRS(nonLatLongProj2))
      nc4 <- cropInputs(nc3, studyArea = ncSmall)
      ncSmall2 <- spTransform(as(ncSmall, "Spatial"), CRSobj = CRS(nonLatLongProj2))
      expect_true(isTRUE(all.equal(extent(nc4), extent(ncSmall2))))

      mess <- capture_messages({
        nc4 <- cropInputs(nc3, studyArea = 1)
      })
      expect_true(grepl("cropInputs must have a rasterToMatch", mess))

      ncSmallShifted <- ncSmall + 10000000
      ncSmallShifted <- st_as_sf(ncSmallShifted)
      st_crs(ncSmallShifted) <- st_crs(ncSmall)
      mess <- capture_messages(cropInputs(as(ncSmall, "Spatial"), studyArea = ncSmallShifted))
      expect_true(any(grepl("polygons do not intersect", mess)))
      expect_true(any(grepl("with no data", mess)))


      #LINEARRING Example
      p6 = readWKT("POLYGON ((0 60, 0 0, 60 0, 60 20, 100 20, 60 20, 60 60, 0 60))")
      mess <- capture_messages({
        p6a <- fixErrors(p6)
      })
      expect_true(any(grepl("Found errors", mess)))
      expect_true(any(grepl("Some or all of the errors fixed", mess)))

      # cropInputs.sf
      nc3 <- st_transform(nc1, crs = CRS(nonLatLongProj2))
      nc4 <- cropInputs(nc3, studyArea = ncSmall)
      ncSmall2 <- st_transform(ncSmall, crs = CRS(nonLatLongProj2))
      expect_true(isTRUE(all.equal(extent(nc4), extent(ncSmall2))))

      # studyArea as spatial object
      nc5 <- cropInputs(nc3, studyArea = as(ncSmall, "Spatial"))
      ncSmall2 <- st_transform(ncSmall, crs = CRS(nonLatLongProj2))
      expect_true(isTRUE(all.equal(extent(nc5), extent(ncSmall2))))
      expect_true(isTRUE(all.equal(extent(nc5), extent(nc4))))

      # studyArea pass through
      nc5 <- cropInputs(nc3, studyArea = 1)
      expect_identical(nc5, nc3)

      # rasterToMatch
      nc5 <- cropInputs(nc3, rasterToMatch = r)
      nc5Extent_r <- st_transform(nc5, crs = crs(r))
      expect_true(isTRUE(abs(xmin(r) - xmin(as(nc5Extent_r, "Spatial"))) < res(r)[1]))
      expect_true(isTRUE(abs(ymin(r) - ymin(as(nc5Extent_r, "Spatial"))) < res(r)[1]))
      expect_true(isTRUE(abs(xmax(r) - xmax(as(nc5Extent_r, "Spatial"))) < res(r)[1]))
      expect_true(isTRUE(abs(ymax(r) - ymax(as(nc5Extent_r, "Spatial"))) < res(r)[1]))

      ncSmallShifted <- ncSmall + 10000000
      ncSmallShifted <- st_as_sf(ncSmallShifted)
      st_crs(ncSmallShifted) <- st_crs(ncSmall)
      mess <- capture_messages(cropInputs(ncSmall, studyArea = ncSmallShifted))
      expect_true(any(grepl("polygons do not intersect", mess)))

      # LINEARRING Example
      p6 = readWKT("POLYGON ((0 60, 0 0, 60 0, 60 20, 100 20, 60 20, 60 60, 0 60))")
      mess <- capture_messages({
        p6a <- fixErrors(st_as_sf(p6))
      })
      expect_true(any(grepl("Found errors", mess)))
      expect_true(any(grepl("Some or all of the errors fixed", mess)))

      # projectInputs pass through
      nc5 <- projectInputs(x = 1)
      expect_identical(nc5, 1)

      expect_error(determineFilename(inputFilePath = "a"))
      expect_error(determineFilename(postProcessedFilename = "a"))
      expect_error(determineFilename(targetFilePath = "a"))
    }
  }
})
