test_that("testing terra", {
  # if (interactive()) {
  testInit("terra",
    needGoogleDriveAuth = FALSE,
    opts = list(
      reproducible.useMemoise = FALSE,
      reproducible.cacheSaveFormat = .qsFormat,
      "rgdal_show_exportToProj4_warnings" = "none"
    )
  )
  withr::local_options(reproducible.cachePath = tmpCache)

  skip_if_not_installed("terra")
  f <- system.file("ex/elev.tif", package = "terra")
  tf <- tempfile(fileext = ".tif")
  tf1 <- tempfile(fileext = ".tif")
  tf2 <- tempfile(fileext = ".tif")
  tf3 <- tempfile(fileext = ".tif")
  tf4 <- tempfile(fileext = ".tif")
  tf5 <- tempfile(fileext = ".tif")
  tf6 <- tempfile(fileext = ".tif")
  tf7 <- tempfile(fileext = ".tif") # don't create it: testing writeTo
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
  terra::values(rmem[[1]]) <- values2(rmem[[1]])
  terra::values(rmem[[2]]) <- values2(rmem[[2]])

  fn <- function(listOf) {
    listOf
  }

  # Test Cache of various nested and non nested SpatRaster
  # double nest
  b <- Cache(fn, list(r, r1), cachePath = tmpCache)
  expect_true(is(b, "list"))
  expect_true(is(b[[1]], "list"))
  expect_true(is(b[[1]][[1]], "SpatRaster"))

  # Single nest
  b <- Cache(fn, r, cachePath = tmpCache)
  expect_true(is(b, "list"))
  expect_true(is(b[[1]], "SpatRaster"))

  # mixed nest
  b <- Cache(fn, list(r[[1]], r1), cachePath = tmpCache)
  expect_true(is(b, "list"))
  expect_true(is(b[[1]], "SpatRaster"))
  expect_true(is(b[[2]][[1]], "SpatRaster"))

  # mix memory and disk
  b <- Cache(fn, list(r[[1]], r1, rmem), cachePath = tmpCache)
  expect_true(is(b, "list"))
  expect_true(is(b[[1]], "SpatRaster"))
  expect_true(is(b[[2]][[1]], "SpatRaster"))
  expect_true(terra::inMemory(b[[3]][[1]]))
  expect_true(!terra::inMemory(b[[2]][[1]]))
  expect_true(!terra::inMemory(b[[1]]))

  f <- system.file("ex/lux.shp", package = "terra")
  vOrig <- terra::vect(f)
  v <- vOrig[1:2, ]
  rf <- system.file("ex/elev.tif", package = "terra")
  xOrig <- terra::rast(rf)
  elevRas <- terra::deepcopy(xOrig)
  xCut <- terra::classify(xOrig, rcl = 5)
  xVect <- terra::as.polygons(xCut)
  xVect2 <- terra::deepcopy(xVect)

  y <- terra::deepcopy(elevRas)
  y[y > 200 & y < 300] <- NA_integer_
  terra::values(elevRas) <- rep(1L, terra::ncell(y))
  vRast <- terra::rast(v, resolution = 0.008333333)

  # SR, SR
  t1 <- postProcessTo(elevRas, y)
  expect_true(sum(is.na(t1[]) != is.na(y[])) == 0)

  t7 <- postProcessTo(elevRas, projectTo = y)
  expect_true(identical(t7, elevRas))

  t8 <- postProcessTo(elevRas, maskTo = y)
  expect_true(all.equal(t8, t1))

  t9 <- postProcessTo(elevRas, cropTo = vRast)
  expect_true(terra::ext(v) <= terra::ext(t9))

  # SR, SV
  t2 <- postProcessTo(elevRas, v)

  # No crop
  t3 <- postProcessTo(elevRas, maskTo = v)
  expect_true(terra::ext(t3) == terra::ext(elevRas))

  if (.requireNamespace("sf")) {
    vsf <- sf::st_as_sf(v)
    vOrigsf <- sf::st_as_sf(vOrig)
  }

  t4 <- postProcessTo(elevRas, cropTo = v, maskTo = v)
  expect_true(terra::ext(t4) == terra::ext(t2))

  t5 <- postProcessTo(elevRas, cropTo = v, maskTo = v, projectTo = v)
  expect_true(identical(t5[], t2[]))

  if (.requireNamespace("sf")) {
    t5sf <- postProcessTo(elevRas, cropTo = vsf, maskTo = vsf, projectTo = vsf)
    expect_true(identical(t5sf[], t2[]))
  }

  t6 <- terra::extract(elevRas, v, mean, na.rm = TRUE)
  expect_true(all(t6$elevation == 1))
  expect_true(NROW(t6) == 2)

  # Only writeTo
  expect_false(file.exists(tf7))
  t11a <- suppressWarnings({
    postProcessTo(elevRas, writeTo = tf7)
  }) ## WARNING: Discarded datum Unknown based on GRS80 ellipsoid in Proj4 definition
  expect_true(file.exists(tf2))
  expect_equivalent(elevRas, t11a)


  t10 <- postProcessTo(xVect, v)
  expect_true(terra::ext(t10) < terra::ext(xVect))

  #
  ## following #253
  # https://github.com/PredictiveEcology/reproducible/issues/253#issuecomment-1263562631
  tf1 <- tempfile(fileext = ".shp")
  tf2 <- tempfile(fileext = ".shp")
  t11 <- suppressWarnings({
    postProcessTo(xVect, v, writeTo = tf1)
  }) ## WARNING: Discarded datum Unknown based on GRS80 ellipsoid in Proj4 definition

  # Only writeTo
  t11a <- suppressWarnings({
    postProcessTo(xVect, writeTo = tf2)
  }) ## WARNING: Discarded datum Unknown based on GRS80 ellipsoid in Proj4 definition
  expect_true(file.exists(tf2))
  expect_true(identical(xVect, t11a))

  tw_t11 <- terra::wrap(t11)
  vv <- terra::vect(tf1)
  tw_vv <- terra::wrap(vv)
  expect_true(terra::same.crs(tw_vv@crs, tw_t11@crs))

  ## following #253 with different driver
  ## https://github.com/PredictiveEcology/reproducible/issues/253#issuecomment-1263562631
  tf1 <- tempfile(fileext = ".gpkg")
  t11 <- suppressWarnings({
    postProcessTo(xVect, v, writeTo = tf1)
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
  # v2 <- terra::makeValid(v2)
  if (getRversion() < "4.3.0") { # this same error crashes the session in R 4.3.0 when it is R-devel
    t10 <- try(postProcessTo(xVect, v2))
    ## Error : TopologyException: Input geom 1 is invalid:
    ##  Self-intersection at 6.0905735768254896 49.981782482072084
    expect_true(!is(t10, "try-error"))
  }

  # Projection --> BAD BUG HERE ... CAN"T REPRODUCE ALWAYS --> use sf for testing Dec 9, 2022
  if (FALSE) {
    utm <- terra::crs("epsg:23028") # sf::st_crs("epsg:23028")$wkt
    # albers <- sf::st_crs("epsg:5070")$wkt
    vutm <- terra::project(v, utm)
  }

  if (.requireNamespace("sf")) {
    # utm <- sf::st_crs("epsg:23028")#$wkt
    utm <- terra::crs("epsg:23028") # $wkt

    vsfutm <- sf::st_transform(vsf, utm)
    vutm <- terra::vect(vsfutm)
    res100 <- 100
    rutm <- terra::rast(vutm, resolution = res100)
    rutm <- terra::rasterize(vutm, rutm, field = "NAME_2")

    vsfInUTMviaCRS <- postProcessTo(vsf, sf::st_crs(rutm))
    expect_true(is(vsfInUTMviaCRS, "sf"))
    expect_true(terra::same.crs(vsfInUTMviaCRS, rutm))

    # from is sf, to is SpatRast --> skip maskTo
    if (getRversion() >= "4.1" && isWindows()) {
      vsfInUTMviaSpatRast <-
        suppressWarningsSpecific(
          falseWarnings = "attribute variables are assumed",
          postProcessTo(vOrigsf, rutm)
        )
      expect_true(is(vsfInUTMviaSpatRast, "sf"))
      expect_true(sf::st_crs(vsfInUTMviaSpatRast) == sf::st_crs(rutm))
      expect_true(isTRUE(all.equal(
        round(terra::ext(rutm), 6),
        round(terra::ext(vsfInUTMviaSpatRast), 6)
      )))
    }

    # Check for cases where `to` does not overlap with `from`
    ext1 <- terra::ext(vsf) + c(-2, 2, -2, 2)
    ext1SR <- terra::rast(ext1)
    if (.requireNamespace("raster")) {
      ext1Ra <- raster::raster(ext1SR)
      ext1Ex <- raster::extent(ext1Ra)
    }
    ext2 <- terra::vect(ext1)
    terra::crs(ext2) <- terra::crs(vsf)
    ext3 <- sf::st_as_sf(ext2)
    if (.requireNamespace("sf")) {
      expect_warning(expect_message(postProcessTo(vOrigsf, ext3))) # sf gives warning too
      expect_warning(expect_message(postProcessTo(terra::vect(vOrigsf), ext2)),
                     "no intersection")
      # expect_warning(expect_error(postProcessTo(vOrigsf, ext3))) # sf gives warning too
      # expect_error(postProcessTo(terra::vect(vOrigsf), ext2))
    }
    if (.requireNamespace("sf")) {
      if (.requireNamespace("sp")) {
        expect_error(postProcessTo(as(vOrigsf, "Spatial"), as(ext2, "Spatial")))
      }
      if (.requireNamespace("raster")) {
        expect_error(postProcessTo(as(vOrigsf, "Spatial"), ext1Ra))
      }
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

    t11 <- postProcessTo(elevRas, vutm)
    expect_true(terra::same.crs(t11, vutm))

    # use raster dataset -- take the projectTo resolution, i.e., res100
    t13 <- postProcessTo(elevRas, rutm)
    expect_true(identical(terra::res(t13)[1], res100))
    expect_true(terra::same.crs(t13, vutm))

    # no projection
    t12 <- postProcessTo(elevRas, cropTo = vutm, maskTo = vutm)
    expect_false(terra::same.crs(t12, vutm))

    # projection with errors
    if (getRversion() >= "4.1" && isWindows()) { # bug in older `terra` that is not going to be fixed here
      utm <- terra::crs("epsg:23028") # This is same as above, but terra way
      if (getRversion() < "4.3.0") { # this same error crashes the session in R 4.3.0 when it is R-devel
        vutmErrors <- terra::project(v2, utm)
        mess <- capture_messages({
          t13a <- postProcessTo(xVect, vutmErrors)
        })
        ## Error : TopologyException: Input geom 1 is invalid:
        ##  Self-intersection at 6095858.7074040668 6626138.068126983
        expect_true(sum(grepl("error", mess)) %in% 1:2) # not sure why crop does not throw error in R >= 4.2
        expect_true(sum(grepl("fixed", mess)) %in% 1:2) # not sure why crop does not throw error in R >= 4.2
        expect_true(is(t13a, "SpatVector"))
      } else {
        v2 <- terra::makeValid(v2)
        vutmErrors <- terra::project(v2, utm)
      }

      # Switch from qs to rds with Cache
      if (requireNamespace(.qsFormat)) {
        opts <- options(reproducible.cacheSaveFormat = .qsFormat)
        t13a <- Cache(postProcessTo(xVect, vutmErrors))
        opts <- options(reproducible.cacheSaveFormat = .rdsFormat)
        t13a <- Cache(postProcessTo(xVect, vutmErrors))
        opts <- options(reproducible.cacheSaveFormat = .qsFormat)
        t13b <- Cache(postProcessTo(xVect, vutmErrors))
        expect_equal(t13a, t13b)
        # a <- try(ncol(t13a), silent = TRUE)
        # expect_false(is(a, "try-error"))
      }
    }

    # try NA to *To
    # Vectors
    vutmSF <- sf::st_as_sf(vutm)
    xVect2SF <- sf::st_as_sf(xVect2)
    t14 <- postProcessTo(xVect2, vutm, projectTo = NA)

    expect_true(terra::same.crs(t14, xVect2))
    expect_false(terra::same.crs(t14, vutm))

    if (getRversion() >= "4.1" && isWindows()) { # bug in older `terra` that is not going to be fixed here
      suppressWarningsSpecific(
        falseWarnings = "attribute variables",
        t14SF <- postProcessTo(xVect2SF, vutmSF, projectTo = NA)
      )
      expect_true(terra::same.crs(t14SF, xVect2SF))
      expect_false(terra::same.crs(t14SF, vutmSF))
    }

    t15 <- postProcessTo(xVect2, vutm, maskTo = NA)

    expect_false(terra::same.crs(t15, xVect2))
    expect_true(terra::same.crs(t15, vutm))

    if (getRversion() >= "4.1" && isWindows()) { # bug in older `terra` that is not going to be fixed here
      suppressWarningsSpecific(
        falseWarnings = "attribute variables",
        t15SF <- postProcessTo(xVect2SF, vutmSF, maskTo = NA)
      )
      expect_false(terra::same.crs(t15SF, xVect2SF))
      expect_true(terra::same.crs(t15SF, vutmSF))
    }

    t18 <- postProcessTo(xVect2, vutm, cropTo = NA)
    expect_false(terra::same.crs(t18, xVect2))
    expect_true(terra::same.crs(t18, vutm))

    suppressWarningsSpecific(
      falseWarnings = "attribute variables",
      t18SF <- postProcessTo(xVect2SF, vutmSF, cropTo = NA)
    )
    expect_false(terra::same.crs(t18SF, xVect2SF))
    expect_true(terra::same.crs(t18SF, vutmSF))

    # Rasters
    t16 <- postProcessTo(elevRas, rutm, cropTo = NA)
    expect_false(terra::same.crs(t16, elevRas))
    expect_true(terra::same.crs(t16, rutm))
    expect_true(terra::ext(t16) >= terra::ext(rutm))

    t17 <- postProcessTo(elevRas, rutm, projectTo = NA)
    expect_true(terra::same.crs(t17, elevRas))
    expect_false(terra::same.crs(t17, rutm))

    t19 <- postProcessTo(elevRas, rutm, maskTo = NA)
    expect_false(terra::same.crs(t19, elevRas))
    expect_true(terra::same.crs(t19, vutm))
    expect_true(sum(values2(t19), na.rm = TRUE) > sum(values2(t13), na.rm = TRUE))

    # Raster with Vector
    t16 <- postProcessTo(elevRas, vutm, cropTo = NA)
    expect_false(terra::same.crs(t16, elevRas))
    expect_true(terra::same.crs(t16, vutm))

    t16SF <- postProcessTo(elevRas, vutmSF, cropTo = NA)
    expect_false(terra::same.crs(t16SF, elevRas))
    expect_true(terra::same.crs(t16SF, vutmSF))

    t17 <- postProcessTo(elevRas, vutm, projectTo = NA)
    expect_true(terra::same.crs(t17, elevRas))
    expect_false(terra::same.crs(t17, vutm))

    t17SF <- postProcessTo(elevRas, vutmSF, projectTo = NA)
    expect_true(terra::same.crs(t17SF, elevRas))
    expect_false(terra::same.crs(t17SF, vutmSF))

    t19 <- postProcessTo(elevRas, vutm, maskTo = NA)
    expect_false(terra::same.crs(t19, elevRas))
    expect_true(terra::same.crs(t19, vutm))
    # expect_true(sum(values2(t19), na.rm = TRUE) > sum(values2(t13), na.rm = TRUE))

    t19SF <- postProcessTo(elevRas, vutmSF, maskTo = NA)
    expect_false(terra::same.crs(t19SF, elevRas))
    expect_true(terra::same.crs(t19SF, vutmSF))
    # expect_true(sum(values2(t19SF), na.rm = TRUE) > sum(values2(t13), na.rm = TRUE))

    t21 <- postProcessTo(elevRas, projectTo = vutm)
    t21SF <- postProcessTo(elevRas, projectTo = vutmSF)
    t20 <- postProcessTo(elevRas, projectTo = terra::crs(vutm))
    expect_true(all.equal(t20, t21))
    expect_true(all.equal(t20, t21SF))
    # This is now (Apr 24, 2023) because passing all to terra::project --> which keep square pixels
    expect_false(identical(terra::size(elevRas), terra::size(t20)))

    t20res250 <- postProcessTo(elevRas, projectTo = terra::crs(vutm), res = 250)
    expect_true(all(terra::res(t20res250) == 250))

    ## same projection change resolution only (will likely affect extent)
    y2 <- terra::rast(crs = terra::crs(y), resolution = 0.008333333 * 2, extent = terra::ext(y))
    y2 <- terra::setValues(y2, rep(1, terra::ncell(y2)))

    t22 <- postProcessTo(elevRas, to = y2, overwrite = TRUE) # not sure why need this; R devel on Winbuilder Nov 26, 2022
    expect_true(terra::same.crs(t22, elevRas))
    expect_true(terra::ext(t22) == terra::ext(y2)) ## "identical" may say FALSE (decimal plates?)
    expect_true(identical(terra::res(t22), terra::res(y2)))
    expect_false(identical(terra::res(t22), terra::res(elevRas)))

    vutmSF <- sf::st_as_sf(vutm)
    xVectSF <- sf::st_as_sf(xVect)
    ## It is a real warning about geometry stuff, but not relevant here
    err <- capture_error( # there is an error in R 4.0 Windows
      warn <- capture_warnings({
        t22 <- postProcessTo(xVectSF, vutmSF)
      })
    )
    #  }

    if (.requireNamespace("raster")) {
      ras1 <- raster::raster(tf5)
      ras2 <- raster::raster(tf6)
      ras1[] <- ras1[]
      ras2[] <- ras2[]
      r3 <- list(ras1, ras2) # this is for
      r3[[1]][] <- r3[[1]][]
      r3[[2]][] <- r3[[2]][] # bring to RAM

      # Raster & SpatVect
      ras1Small <- cropTo(ras1, t18)
      ras1SmallMasked <- maskTo(ras1, t18)
      # The warning is about some datum
      suppressWarnings(ras1SmallAll <- postProcessTo(ras1, t18))
      expect_true(is(ras1Small, "Raster"))
      expect_true(is(ras1SmallMasked, "Raster"))
      expect_true(is(ras1SmallAll, "Raster"))

      # Check that extents are similar
      expect_true(terra::ext(ras1SmallAll) < terra::extend(terra::ext(t18), terra::res(ras1SmallAll) * 2))

      # SpatRaster & Raster
      t20CroppedByRas <- cropTo(t20, ras1SmallAll)
      t20MaskedByRas <- maskTo(t20, ras1SmallAll)
      t20ProjectedByRas <- projectTo(t20, ras1SmallAll)
      t20AllByRas <- postProcessTo(t20, ras1SmallAll) # only does terra::rast 1x
      expect_true(is(t20AllByRas, "SpatRaster"))
      expect_true(is(t20CroppedByRas, "SpatRaster"))
      expect_true(is(t20MaskedByRas, "SpatRaster"))
      expect_true(is(t20ProjectedByRas, "SpatRaster"))
      expect_equal(terra::res(t20ProjectedByRas), terra::res(ras1SmallAll))
      expect_equal(terra::res(t20AllByRas), terra::res(ras1SmallAll))
      expect_equal(terra::ext(t20AllByRas), terra::ext(ras1SmallAll))
      expect_equal(terra::ext(t20ProjectedByRas), terra::ext(ras1SmallAll))
      expect_true(
        sum(!is.na(values2(ras1SmallAll))) <
          sum(!is.na(values2(t20ProjectedByRas)))
      )
      expect_true(
        sum(!is.na(values2(ras1SmallAll))) ==
          sum(!is.na(values2(t20AllByRas)))
      )

      # The below was slightly off because RasterLayer was not exactly same as t20 proj
      spatRas1SmallAll <- projectTo(terra::rast(ras1SmallAll), t20)
      expect_true( #  these are off b/c of projection probably
        abs(sum(!is.na(values2(spatRas1SmallAll))) -
          sum(!is.na(values2(t20MaskedByRas)))) <= 0
      )

      if (FALSE) {
        dev.off()
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
      w3 <- fixErrorsIn(w2)
      expect_true(!all(terra::is.valid(w2)))
      expect_true(all(terra::is.valid(w3)))
    }
  }
})

