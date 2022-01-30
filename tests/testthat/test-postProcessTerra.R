test_that("testing terra", {
  if (interactive()) {
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
      ralbers <- terra::rast(valbers, res = 100)


      # use vector dataset -- force the 250m resolution
      t11 <- postProcessTerra(x, valbers)
      expect_true(identical(res(t11)[1], 250))
      expect_true(sf::st_crs(t11) == sf::st_crs(valbers))

      # use raster dataset -- take the projectTo resolution, i.e., 100
      t13 <- postProcessTerra(x, ralbers)
      expect_true(identical(res(t13)[1], 100))
      expect_true(sf::st_crs(t13) == sf::st_crs(valbers))

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
  }
})

