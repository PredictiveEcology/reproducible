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
  newStack1 <- raster::stack(raster::mask(origStack, mask = shp))
  newStack2 <- fastMask(x = origStack, y = shp)
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

  testthat::with_mock(
    "raster::canProcessInMemory" = function(x, n) {
      FALSE
    },
    "reproducible::isWindows" = function() {
      TRUE
    },
    # The warning is "data type "LOG" is not available in GDAL -- not relevant here
    {
      mess <- capture_messages({
        out <- fastMask(x = origStack[[2]], y = shpDF)
      })
      expect_true(any(grepl("GDAL because crs", mess)))
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
})

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

  testthat::with_mock(
    "reproducible::isInteractive" = function() {
      FALSE
    }, {
      noisyOutput <- capture.output(
        warn <- capture_warnings({
          BCR6_VT <- prepInputs(
            url = "https://drive.google.com/open?id=1sEiXKnAOCi-f1BF7b4kTg-6zFlGr0YOH",
            targetFile = "BCR6.shp",
            overwrite = TRUE
          )
        })
      )
  })
  expect_true(is(BCR6_VT, shapefileClassDefault()))

  if (interactive()) {
    NFDB_PT <- #Cache(
      prepInputs(
      url = "http://cwfis.cfs.nrcan.gc.ca/downloads/nfdb/fire_pnt/current_version/NFDB_point.zip",
      overwrite = TRUE,
      #targetFile = "NFDB_point_20181129.shp",
    #  alsoExtract = "similar",
      fun = "sf::st_read"
    )
    expect_is(NFDB_PT, "sf")
    expect_true(all(c("zip", "sbx", "shp", "xml", "shx", "sbn") %in%
                      fileExt(dir(pattern = "NFDB_point"))))

    noisyOutput <- capture.output(
      warn <- capture_warnings(NFDB_PT_BCR6 <- Cache(
        postProcess,
        NFDB_PT,
        studyArea = BCR6_VT
      ))
    )# warning is "attribute variables are assumed to be spatially constant"
    if (!all(grepl("attribute variables are assumed to be spatially constant", warn)))
      warnings(warn)
  }
})

test_that("testing rebuildColors", {
  testInitOut <- testInit(needGoogle = FALSE, "raster")
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  x <- raster::raster(extent(0, 10, 0, 10), vals = runif(100, 0, 197))
  origColors <- list(origColors = character(0), origMinValue = 0, origMaxValue = 197.100006103516)
  expect_is(rebuildColors(x, origColors), "Raster")
})


test_that("testing terra", {
  testInitOut <- testInit(needGoogle = FALSE,
                          opts = list(reproducible.useMemoise = TRUE))

  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  if (requireNamespace("terra")) {
    f <- system.file("ex/elev.tif", package="terra")
    tf <- tempfile(fileext = ".tif")
    tf1 <- tempfile(fileext = ".tif")
    tf2 <- tempfile(fileext = ".tif")
    tf3 <- tempfile(fileext = ".tif")
    tf4 <- tempfile(fileext = ".tif")
    file.copy(f, tf)
    file.copy(f, tf1)
    file.copy(f, tf2)
    file.copy(f, tf3)
    file.copy(f, tf4)
    r <- list(terra::rast(f), terra::rast(tf))
    r1 <- list(terra::rast(tf1), terra::rast(tf2))
    r2 <- list(terra::rast(tf3), terra::rast(tf4))
    rmem <- r2
    terra::values(rmem[[1]]) <- terra::values(rmem[[1]])
    terra::values(rmem[[2]]) <- terra::values(rmem[[2]])

    fn <- function(listOf) {
      listOf
    }

    # Test Cache of various nested and non nested SpatRaster
    # double nest
    b <- Cache(fn, list(r, r1), cacheRepo = tmpCache)
    expect_true(is(b, "list"))
    expect_true(is(b[[1]], "list"))
    expect_true(is(b[[1]][[1]], "SpatRaster"))

    # Single nest
    b <- Cache(fn, r, cacheRepo = tmpCache)
    expect_true(is(b, "list"))
    expect_true(is(b[[1]], "SpatRaster"))

    # mixed nest
    b <- Cache(fn, list(r[[1]], r1), cacheRepo = tmpCache)
    expect_true(is(b, "list"))
    expect_true(is(b[[1]], "SpatRaster"))
    expect_true(is(b[[2]][[1]], "SpatRaster"))

    # mix memory and disk
    b <- Cache(fn, list(r[[1]], r1, rmem), cacheRepo = tmpCache)
    expect_true(is(b, "list"))
    expect_true(is(b[[1]], "SpatRaster"))
    expect_true(is(b[[2]][[1]], "SpatRaster"))
    expect_true(terra::inMemory(b[[3]][[1]]))
    expect_true(!terra::inMemory(b[[2]][[1]]))
    expect_true(!terra::inMemory(b[[1]]))




    f <- system.file("ex/lux.shp", package="terra")
    v <- terra::vect(f)
    v <- v[1:2,]
    rf <- system.file("ex/elev.tif", package="terra")
    xOrig <- terra::rast(rf)
    x <- xOrig
    xCut <- terra::classify(xOrig, rcl = 5)
    xVect <- terra::as.polygons(xCut)

    y <- copy(x)
    y[y > 200 & y < 300] <- NA
    x[] <- 1
    vRast <- terra::rast(v, res = 0.008333333)

    # SR, SR
    t1 <- postProcessTerra(x, y)
    expect_true(sum(is.na(t1[]) != is.na(y[])) == 0)

    t7 <- postProcessTerra(x, projectTo = y)
    expect_true(identical(t7, x))

    t8 <- postProcessTerra(x, maskTo = y)
    expect_true(all.equal(t8, t1))

    t9 <- postProcessTerra(x, cropTo = vRast)
    expect_true(terra::ext(v) <= terra::ext(t9))


    # SR, SV
    t2 <- postProcessTerra(x, v)

    # No crop
    t3 <- postProcessTerra(x, maskTo = v)
    expect_true(terra::ext(t3) == terra::ext(x))

    t4 <- postProcessTerra(x, cropTo = v, maskTo = v)
    expect_true(terra::ext(t4) == terra::ext(t2))

    t5 <- postProcessTerra(x, cropTo = v, maskTo = v, projectTo = v)
    expect_true(identical(t5[],t2[]))


    t6 <- extract(x, v, mean, na.rm=TRUE)
    expect_true(all(t6$elevation == 1))
    expect_true(NROW(t6) == 2)


    ################

    t10 <- postProcessTerra(xVect, v)
    expect_true(terra::ext(t10) < terra::ext(xVect))

    ################
    tf1 <- tempfile(fileext = ".shp")
    t11 <- postProcessTerra(xVect, v, writeTo = tf1)
    vv <- terra::vect(tf1)
    expect_identical(terra::wrap(vv), terra::wrap(t11))

    # Test fixErrorTerra
    v1 <- terra::simplify(v)
    gv1 <- terra::geom(v1)
    gv1[gv1[, "geom"] == 2, "geom"] <- 1
    # gv1[9,"y"] <- 51
    v2 <- terra::vect(gv1, "polygons")
    # plot(v2)
    # v2 <- is.valid(v2)

    terra::crs(v2) <- terra::crs(v)
    t10 <- try(postProcessTerra(xVect, v2))
    expect_true(!is(t10, "try-error"))

    # Projection -->
    albers <- sf::st_crs("epsg:5070")$wkt
    valbers <- terra::project(v, albers)


    t11 <- postProcessTerra(x, valbers)
    expect_true(sf::st_crs(t11) == sf::st_crs(valbers))

    # no projection
    t12 <- postProcessTerra(x, cropTo = valbers, maskTo = valbers)
    expect_true(sf::st_crs(t12) != sf::st_crs(valbers))

    # projection with errors
    valbersErrors <- terra::project(v2, albers)
    mess <- capture_messages(t13 <- postProcessTerra(xVect, valbersErrors))
    expect_true(sum(grepl("error", mess)) == 2)
    expect_true(sum(grepl("fixed", mess)) == 2)
    expect_true(is(t13, "SpatVector"))

  }
})

