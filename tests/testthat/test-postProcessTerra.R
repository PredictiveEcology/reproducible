test_that("testing terra", {
  #if (interactive()) {
  testInitOut <- testInit(needGoogle = FALSE,
                          opts = list(reproducible.useMemoise = FALSE,
                                      reproducible.useTerra = TRUE,
                                      "rgdal_show_exportToProj4_warnings"="none"))

  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  if (!(requireNamespace("terra", quietly = TRUE) && getOption("reproducible.useTerra", FALSE)))
    skip("no terra or no reproducible.useTerra = TRUE")
  f <- system.file("ex/elev.tif", package = "terra")
  tf <- tempfile(fileext = ".tif")
  tf1 <- tempfile(fileext = ".tif")
  tf2 <- tempfile(fileext = ".tif")
  tf3 <- tempfile(fileext = ".tif")
  tf4 <- tempfile(fileext = ".tif")
  tf5 <- tempfile(fileext = ".tif")
  tf6 <- tempfile(fileext = ".tif")
  file.copy(f, tf)
  file.copy(f, tf1)
  file.copy(f, tf2)
  file.copy(f, tf3)
  file.copy(f, tf4)
  file.copy(f, tf5)
  file.copy(f, tf6)
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

  f <- system.file("ex/lux.shp", package = "terra")
  vOrig <- terra::vect(f)
  v <- vOrig[1:2,]
  rf <- system.file("ex/elev.tif", package = "terra")
  xOrig <- terra::rast(rf)
  elevRas <- terra::deepcopy(xOrig)
  xCut <- terra::classify(xOrig, rcl = 5)
  xVect <- terra::as.polygons(xCut)
  xVect2 <- terra::deepcopy(xVect)

  y <- terra::deepcopy(elevRas)
  y[y > 200 & y < 300] <- NA
  terra::values(elevRas) <- rep(1L, ncell(y))
  vRast <- terra::rast(v, res = 0.008333333)

  # SR, SR
  t1 <- postProcessTerra(elevRas, y)
  expect_true(sum(is.na(t1[]) != is.na(y[])) == 0)

  t7 <- postProcessTerra(elevRas, projectTo = y)
  expect_true(identical(t7, elevRas))

  t8 <- postProcessTerra(elevRas, maskTo = y)
  expect_true(all.equal(t8, t1))

  t9 <- postProcessTerra(elevRas, cropTo = vRast)
  expect_true(terra::ext(v) <= terra::ext(t9))

  # SR, SV
  t2 <- postProcessTerra(elevRas, v)

  # No crop
  t3 <- postProcessTerra(elevRas, maskTo = v)
  expect_true(terra::ext(t3) == terra::ext(elevRas))

  vsf <- sf::st_as_sf(v)
  vOrigsf <- sf::st_as_sf(vOrig)

  t4 <- postProcessTerra(elevRas, cropTo = v, maskTo = v)
  expect_true(terra::ext(t4) == terra::ext(t2))

  t5 <- postProcessTerra(elevRas, cropTo = v, maskTo = v, projectTo = v)
  expect_true(identical(t5[],t2[]))

  t5sf <- postProcessTerra(elevRas, cropTo = vsf, maskTo = vsf, projectTo = vsf)
  expect_true(identical(t5sf[],t2[]))

  t6 <- extract(elevRas, v, mean, na.rm = TRUE)
  expect_true(all(t6$elevation == 1))
  expect_true(NROW(t6) == 2)

  ################

  t10 <- postProcessTerra(xVect, v)
  expect_true(terra::ext(t10) < terra::ext(xVect))

  ################
  ## following #253
  # https://github.com/PredictiveEcology/reproducible/issues/253#issuecomment-1263562631
  tf1 <- tempfile(fileext = ".shp")
  t11 <- suppressWarnings({
    postProcessTerra(xVect, v, writeTo = tf1)
  }) ## WARNING: Discarded datum Unknown based on GRS80 ellipsoid in Proj4 definition
  tw_t11 <- terra::wrap(t11)
  vv <- terra::vect(tf1)
  tw_vv <- terra::wrap(vv)
  expect_true(sf::st_crs(tw_vv@crs) == sf::st_crs(tw_t11@crs))

  ## following #253 with different driver
  ## https://github.com/PredictiveEcology/reproducible/issues/253#issuecomment-1263562631
  tf1 <- tempfile(fileext = ".gpkg")
  t11 <- suppressWarnings({
    postProcessTerra(xVect, v, writeTo = tf1)
  }) ## WARNING: GDAL Message 6: dataset does not support layer creation option ENCODING
  tw_t11 <- terra::wrap(t11)
  vv <- terra::vect(tf1)
  tw_vv <- terra::wrap(vv)
  expect_equivalent(tw_vv, tw_t11) ## TODO: not identical

  # Test fixErrorTerra
  v1 <- terra::simplifyGeom(v)
  gv1 <- terra::geom(v1)
  gv1[gv1[, "geom"] == 2, "geom"] <- 1
  # gv1[9,"y"] <- 51
  v2 <- terra::vect(gv1, "polygons")
  # plot(v2)
  # v2 <- is.valid(v2)

  terra::crs(v2) <- terra::crs(v)
  t10 <- try(postProcessTerra(xVect, v2))
  ## Error : TopologyException: Input geom 1 is invalid:
  ##  Self-intersection at 6.0905735768254896 49.981782482072084
  expect_true(!is(t10, "try-error"))

  # Projection --> BAD BUG HERE ... CAN"T REPRODUCE ALWAYS --> use sf for testing Dec 9, 2022
  if (FALSE) {
    utm <- terra::crs("epsg:23028")#sf::st_crs("epsg:23028")$wkt
    # albers <- sf::st_crs("epsg:5070")$wkt
    vutm <- terra::project(v, utm)
  }

  utm <- sf::st_crs("epsg:23028")#$wkt
  # vsf <- sf::st_as_sf(v)
  vsfutm <- sf::st_transform(vsf, utm)
  vutm <- terra::vect(vsfutm)
  res100 <- 100
  rutm <- terra::rast(vutm, res = res100)
  rutm <- terra::rasterize(vutm, rutm, field = "NAME_2")

  vsfInUTMviaCRS <- postProcessTerra(vsf, sf::st_crs(rutm))
  expect_true(is(vsfInUTMviaCRS, "sf"))
  expect_true(sf::st_crs(vsfInUTMviaCRS) == sf::st_crs(rutm))

  # from is sf, to is SpatRast --> skip maskTo
  if (getRversion() >= "4.1" && isWindows()) {
    vsfInUTMviaSpatRast <-
      suppressWarningsSpecific(falseWarnings = "attribute variables are assumed",
                               postProcessTerra(vOrigsf, rutm))
    expect_true(is(vsfInUTMviaSpatRast, "sf"))
    expect_true(sf::st_crs(vsfInUTMviaSpatRast) == sf::st_crs(rutm))
    expect_true(isTRUE(all.equal(round(terra::ext(rutm), 6),
                                 round(terra::ext(vsfInUTMviaSpatRast), 6))))
  }

  # if (Sys.info()["user"] %in% "emcintir") {
  #   env <- new.env(parent = emptyenv())
  #   suppressWarnings(
  #     b <- lapply(ls(), function(xx) if (isSpat(get(xx))) try(assign(xx, envir = env, terra::wrap(get(xx)))))
  #   )
  #   save(list = ls(envir = env), envir = env, file = "~/tmp2.rda")
  #   # load(file = "~/tmp2.rda")
  #   # env <- environment()
  #   # b <- lapply(ls(), function(xx) if (is(get(xx, env), "PackedSpatRaster") || is(get(xx, env), "PackedSpatVector")) try(assign(xx, envir = env, terra::unwrap(get(xx)))))
  # }

  t11 <- postProcessTerra(elevRas, vutm)
  expect_true(sf::st_crs(t11) == sf::st_crs(vutm))

  # use raster dataset -- take the projectTo resolution, i.e., res100
  t13 <- postProcessTerra(elevRas, rutm)
  expect_true(identical(res(t13)[1], res100))
  expect_true(sf::st_crs(t13) == sf::st_crs(vutm))

  # no projection
  t12 <- postProcessTerra(elevRas, cropTo = vutm, maskTo = vutm)
  expect_true(sf::st_crs(t12) != sf::st_crs(vutm))

  # projection with errors
  if (getRversion() >= "4.1" && isWindows()) { # bug in older `terra` that is not going to be fixed here
    utm <- terra::crs("epsg:23028") # This is same as above, but terra way
    vutmErrors <- terra::project(v2, utm)
    mess <- capture_messages({
      t13a <- postProcessTerra(xVect, vutmErrors)
    })
    if (getRversion() != "4.3.0") { # this same error crashes the session in R 4.3.0 when it is R-devel
      ## Error : TopologyException: Input geom 1 is invalid:
      ##  Self-intersection at 6095858.7074040668 6626138.068126983
      expect_true(sum(grepl("error", mess)) %in% 1:2) # not sure why crop does not throw error in R >= 4.2
      expect_true(sum(grepl("fixed", mess)) %in% 1:2) # not sure why crop does not throw error in R >= 4.2
    }
    expect_true(is(t13a, "SpatVector"))

    # Switch from qs to rds with Cache
    opts <- options(reproducible.cacheSaveFormat = "qs")
    t13a <- Cache(postProcessTerra(xVect, vutmErrors))
    opts <- options(reproducible.cacheSaveFormat = "rds")
    t13a <- Cache(postProcessTerra(xVect, vutmErrors))
    opts <- options(reproducible.cacheSaveFormat = "qs")
    t13a <- try(Cache(postProcessTerra(xVect, vutmErrors)), silent = TRUE)
    a <- try(ncol(t13a), silent = TRUE)
    expect_false(is(a, "try-error"))
  }

  # try NA to *To
  # Vectors
  vutmSF <- sf::st_as_sf(vutm)
  xVect2SF <- sf::st_as_sf(xVect2)
  t14 <- postProcessTerra(xVect2, vutm, projectTo = NA)
  expect_true(sf::st_crs(t14) == sf::st_crs(xVect2))
  expect_true(sf::st_crs(t14) != sf::st_crs(vutm))

  if (getRversion() >= "4.1" && isWindows()) { # bug in older `terra` that is not going to be fixed here
    suppressWarningsSpecific(falseWarnings = "attribute variables",
                             t14SF <- postProcessTerra(xVect2SF, vutmSF, projectTo = NA)
    )
    expect_true(sf::st_crs(t14SF) == sf::st_crs(xVect2SF))
    expect_true(sf::st_crs(t14SF) != sf::st_crs(vutmSF))
  }

  t15 <- postProcessTerra(xVect2, vutm, maskTo = NA)
  expect_true(sf::st_crs(t15) != sf::st_crs(xVect2))
  expect_true(sf::st_crs(t15) == sf::st_crs(vutm))

  if (getRversion() >= "4.1" && isWindows()) { # bug in older `terra` that is not going to be fixed here
    suppressWarningsSpecific(falseWarnings = "attribute variables",
                             t15SF <- postProcessTerra(xVect2SF, vutmSF, maskTo = NA)
    )
    expect_true(sf::st_crs(t15SF) != sf::st_crs(xVect2SF))
    expect_true(sf::st_crs(t15SF) == sf::st_crs(vutmSF))
  }

  t18 <- postProcessTerra(xVect2, vutm, cropTo = NA)
  expect_true(sf::st_crs(t18) != sf::st_crs(xVect2))
  expect_true(sf::st_crs(t18) == sf::st_crs(vutm))

  suppressWarningsSpecific(falseWarnings = "attribute variables",
                           t18SF <- postProcessTerra(xVect2SF, vutmSF, cropTo = NA)
  )
  expect_true(sf::st_crs(t18SF) != sf::st_crs(xVect2SF))
  expect_true(sf::st_crs(t18SF) == sf::st_crs(vutmSF))

  # Rasters
  t16 <- postProcessTerra(elevRas, rutm, cropTo = NA)
  expect_true(sf::st_crs(t16) != sf::st_crs(elevRas))
  expect_true(sf::st_crs(t16) == sf::st_crs(rutm))
  expect_true(terra::ext(t16) >= terra::ext(rutm))

  t17 <- postProcessTerra(elevRas, rutm, projectTo = NA)
  expect_true(sf::st_crs(t17) == sf::st_crs(elevRas))
  expect_true(sf::st_crs(t17) != sf::st_crs(rutm))

  t19 <- postProcessTerra(elevRas, rutm, maskTo = NA)
  expect_true(sf::st_crs(t19) != sf::st_crs(elevRas))
  expect_true(sf::st_crs(t19) == sf::st_crs(vutm))
  expect_true(sum(terra::values(t19), na.rm = TRUE) > sum(terra::values(t13), na.rm = TRUE))

  # Raster with Vector
  t16 <- postProcessTerra(elevRas, vutm, cropTo = NA)
  expect_true(sf::st_crs(t16) != sf::st_crs(elevRas))
  expect_true(sf::st_crs(t16) == sf::st_crs(vutm))

  t16SF <- postProcessTerra(elevRas, vutmSF, cropTo = NA)
  expect_true(sf::st_crs(t16SF) != sf::st_crs(elevRas))
  expect_true(sf::st_crs(t16SF) == sf::st_crs(vutmSF))

  t17 <- postProcessTerra(elevRas, vutm, projectTo = NA)
  expect_true(sf::st_crs(t17) == sf::st_crs(elevRas))
  expect_true(sf::st_crs(t17) != sf::st_crs(vutm))

  t17SF <- postProcessTerra(elevRas, vutmSF, projectTo = NA)
  expect_true(sf::st_crs(t17SF) == sf::st_crs(elevRas))
  expect_true(sf::st_crs(t17SF) != sf::st_crs(vutmSF))

  t19 <- postProcessTerra(elevRas, vutm, maskTo = NA)
  expect_true(sf::st_crs(t19) != sf::st_crs(elevRas))
  expect_true(sf::st_crs(t19) == sf::st_crs(vutm))
  # expect_true(sum(terra::values(t19), na.rm = TRUE) > sum(terra::values(t13), na.rm = TRUE))

  t19SF <- postProcessTerra(elevRas, vutmSF, maskTo = NA)
  expect_true(sf::st_crs(t19SF) != sf::st_crs(elevRas))
  expect_true(sf::st_crs(t19SF) == sf::st_crs(vutmSF))
  # expect_true(sum(terra::values(t19SF), na.rm = TRUE) > sum(terra::values(t13), na.rm = TRUE))

  t21 <- postProcessTerra(elevRas, projectTo = vutm)
  t21SF <- postProcessTerra(elevRas, projectTo = vutmSF)
  t20 <- postProcessTerra(elevRas, projectTo = sf::st_crs(vutm))
  expect_true(all.equal(t20, t21))
  expect_true(all.equal(t20, t21SF))
  expect_true(identical(terra::size(elevRas), terra::size(t20)))

  ## same projection change resolution only (will likely affect extent)
  y2 <- terra::rast(crs = crs(y), res = 0.008333333*2, extent = terra::ext(y))
  y2 <- terra::setValues(y2, rep(1, ncell(y2)))

  t22 <- postProcessTerra(elevRas, to = y2, overwrite = TRUE) # not sure why need this; R devel on Winbuilder Nov 26, 2022
  expect_true(sf::st_crs(t22) == sf::st_crs(elevRas))
  expect_true(terra::ext(t22) == terra::ext(y2))   ## "identical" may say FALSE (decimal plates?)
  expect_true(identical(res(t22), res(y2)))
  expect_false(identical(res(t22), res(elevRas)))

  vutmSF <- sf::st_as_sf(vutm)
  xVectSF <- sf::st_as_sf(xVect)
  ## It is a real warning about geometry stuff, but not relevant here
  err <- capture_error( # there is an error in R 4.0 Windows
    warn <- capture_warnings({
      t22 <- postProcessTerra(xVectSF, vutmSF)
    })
  )
  #  }



  if (.requireNamespace("raster")) {
    ras1 <- raster::raster(tf5)
    ras2 <- raster::raster(tf6)
    ras1[] <- ras1[]
    ras2[] <- ras2[]
    r3 <- list(ras1, ras2) # this is for
    r3[[1]][] <- r3[[1]][]; r3[[2]][] <- r3[[2]][] # bring to RAM

    # Raster & SpatVect
    ras1Small <- cropTo(ras1, t18)
    ras1SmallMasked <- maskTo(ras1, t18)
    # The warning is about some datum
    suppressWarnings(ras1SmallAll <- postProcessTerra(ras1, t18))
    expect_true(is(ras1Small, "Raster"))
    expect_true(is(ras1SmallMasked, "Raster"))
    expect_true(is(ras1SmallAll, "Raster"))

    # Check that extents are similar
    expect_true(terra::ext(ras1SmallAll) < terra::extend(terra::ext(t18), res(ras1SmallAll) * 2))

    # SpatRaster & Raster
    t20CroppedByRas <- cropTo(t20, ras1SmallAll)
    t20MaskedByRas <- maskTo(t20, ras1SmallAll)
    t20ProjectedByRas <- projectTo(t20, ras1SmallAll)
    t20AllByRas <- postProcessTerra(t20, ras1SmallAll) # only does terra::rast 1x
    expect_true(is(t20AllByRas, "SpatRaster"))
    expect_true(is(t20CroppedByRas, "SpatRaster"))
    expect_true(is(t20MaskedByRas, "SpatRaster"))
    expect_true(is(t20ProjectedByRas, "SpatRaster"))
    expect_equal(terra::res(t20ProjectedByRas), terra::res(ras1SmallAll))
    expect_equal(terra::res(t20AllByRas), terra::res(ras1SmallAll))
    expect_equal(terra::ext(t20AllByRas), terra::ext(ras1SmallAll))
    expect_equal(terra::ext(t20ProjectedByRas), terra::ext(ras1SmallAll))
    expect_true(
      sum(!is.na(terra::values(ras1SmallAll))) <
        sum(!is.na(terra::values(t20ProjectedByRas)))
    )
    expect_true(
      sum(!is.na(terra::values(ras1SmallAll))) ==
        sum(!is.na(terra::values(t20AllByRas)))
    )

    # The below was slightly off because RasterLayer was not exactly same as t20 proj
    spatRas1SmallAll <- projectTo(terra::rast(ras1SmallAll), t20)
    expect_true( #  these are off b/c of projection probably
      abs(sum(!is.na(terra::values(spatRas1SmallAll))) -
        sum(!is.na(terra::values(t20MaskedByRas)))) <= 0
    )

    if (interactive()) {
      terra::plot(ras1SmallAll)
      terra::plot(t18, add = TRUE)
      terra::plot(t20AllByRas)
      terra::plot(t20CroppedByRas)
      terra::plot(t20MaskedByRas)
      terra::plot(t20ProjectedByRas)
    }

    w <- terra::vect("POLYGON ((0 -5, 10 0, 10 -10, 0 -5))")
    w1 <- terra::vect("POLYGON ((0 -5, 10 0, 10 -10, 4 -2, 0 -5))")
    w1a <- terra::vect("POLYGON ((20 15, 30 20, 30 10, 24 18, 20 15))")
    w2 <- rbind(w, w1, w, w1a, w)
    w3 <- fixErrorsTerra(w2)
    expect_true(!all(terra::is.valid(w2)))
    expect_true(all(terra::is.valid(w3)))


  }

})
