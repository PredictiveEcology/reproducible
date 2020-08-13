test_that("fastMask produces correct results", {
  testInitOut <- testInit(needGoogle = FALSE, c("sp", "raster"))
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  Sr1 <- Polygon(cbind(c(2, 4, 4, 1, 2), c(2, 3, 5, 4, 2))) # nolint
  Sr2 <- Polygon(cbind(c(5, 4, 2, 5), c(2, 3, 2, 2))) # nolint
  Sr3 <- Polygon(cbind(c(4, 4, 5, 7.4, 4), c(5, 3, 2, 5, 5))) # nolint

  Srs1 <- Polygons(list(Sr1), "s1") # nolint
  Srs2 <- Polygons(list(Sr2), "s2") # nolint
  Srs3 <- Polygons(list(Sr3), "s3") # nolint

  shp <- SpatialPolygons(list(Srs1, Srs2, Srs3), 1:3)
  d <- data.frame(vals = 1:3, other = letters[1:3])
  row.names(d) <- names(shp)
  shpDF <- sp::SpatialPolygonsDataFrame(shp, data = d)
  poly <- list()
  poly[[1]] <- raster(extent(shpDF), vals = 0, res = c(0.5, 0.5))
  poly[[2]] <- raster(extent(shpDF), vals = 1, res = c(0.5, 0.5))
  origStack <- raster::stack(poly)

  ## mask
  newStack1 <- raster::stack(raster::mask(origStack, mask = shpDF))
  newStack2 <- fastMask(x = origStack, y = shpDF)
  expect_equal(newStack1, newStack2)

  newStack1 <- mask(origStack[[2]], mask = shpDF)
  newStack2 <- fastMask(x = origStack[[2]], y = shpDF)
  expect_equivalent(newStack1, newStack2)

  # Run same as above but with different internal pathway
  testthat::with_mock(
    "raster::canProcessInMemory" = function(x, n) {
      FALSE
    },
    suppressWarnings({
      newStack3 <- fastMask(x = origStack[[2]], y = shpDF)
    })
  )
  newStack3[] <- newStack3[]
  names(newStack3) <- names(newStack1)
  expect_equivalent(newStack1, newStack3)
  # Run same as above but with different internal pathway
  if (requireNamespace("gdalUtils", quietly = TRUE)) {
    suppressWarnings(gdalUtils::gdal_setInstallation())

    # if it doesn't find gdal installed
    hasGDALInstalled <- !is.null(getOption("gdalUtils_gdalPath"))

    testthat::with_mock(
      "raster::canProcessInMemory" = function(x, n) {
        FALSE
      },
      "reproducible::isWindows" = function() {
        TRUE
      },
      # The warning is "data type "LOG" is not available in GDAL -- not relevant here
      {
        if (hasGDALInstalled) {
          mess <- capture_messages({
            out <- fastMask(x = origStack[[2]], y = shpDF)
          })
          expect_true(any(grepl("GDAL because crs", mess)))
        }
      }
    )
    mess <- capture_messages({
      out <- fastMask(x = origStack[[2]], y = shpDF, cores = "none")
    })
    expect_true(any(grepl("useGDAL is TRUE, but problem is small enough for RA", mess)))

    crs(shpDF) <- "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGS84"
    crs(shp) <- "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGS84"
    crs(origStack[[2]]) <- "+proj=lcc +lat_1=49 +lat_2=33 +lon_0=-100 +ellps=WGS84"

    # Test "force" even for a small problem
    warn <- capture_warnings({
      mess <- capture_messages({
        out <- fastMask(x = origStack[[2]], y = shpDF, useGDAL = "force")
      })
    })
    expect_false(any(grepl("useGDAL is TRUE, but problem is small enough for RA", mess)))

    newStack2 <- fastMask(x = origStack[[2]], y = shpDF)

    # test non-spatial polygons data frame
    newStack2 <- fastMask(x = origStack[[2]], y = shp)
}})

test_that("checkGDALVersion", {
  testInitOut <- testInit(needGoogle = FALSE, c("sp", "raster"))
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  ### getGDALversion
    if (.requireNamespace("rgdal")) {
      expect_silent({
        a <- getGDALVersion()
      })
      if (is.na(a)) {
        expect_true(is.numeric(a))
      } else {
        expect_true(is(a, "numeric_version"))
      }

      expect_true(checkGDALVersion("1.0.0"))
      expect_error(checkGDALVersion())
      expect_false(checkGDALVersion("4.0.0"))
    }
})

test_that("testing prepInputs with deauthorized googledrive", {
  if (!requireNamespace("googledrive", quietly = TRUE))
    stop(requireNamespaceMsg("googledrive", "to use google drive files"))
  testInitOut <- testInit(needGoogle = FALSE, "googledrive")
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  googledrive::drive_deauth()
  testthat::with_mock(
    "reproducible::isInteractive" = function() {
      FALSE
    }, {
    warn <- capture_warnings({
      BCR6_VT <- prepInputs(
        url = "https://drive.google.com/open?id=1sEiXKnAOCi-f1BF7b4kTg-6zFlGr0YOH",
        targetFile = "BCR6.shp",
        overwrite = TRUE
      )
    })
  })
  expect_true(is(BCR6_VT, "Spatial"))

  if (interactive()) {
    NFDB_PT <- Cache(
      prepInputs,
      url = "http://cwfis.cfs.nrcan.gc.ca/downloads/nfdb/fire_pnt/current_version/NFDB_point.zip",
      overwrite = TRUE,
      #targetFile = "NFDB_point_20181129.shp",
    #  alsoExtract = "similar",
      fun = "sf::st_read"
    )
    expect_is(NFDB_PT, "sf")
    expect_true(all(c("zip", "sbx", "shp", "xml", "shx", "sbn") %in%
                      fileExt(dir(pattern = "NFDB_point"))))

    warn <- capture_warnings(NFDB_PT_BCR6 <- Cache(
      postProcess,
      NFDB_PT,
      studyArea = BCR6_VT
    )) # warning is "attribute variables are assumed to be spatially constant"
    if (!all(grepl("attribute variables are assumed to be spatially constant", warn)))
      warnings(warn)
  }
})
