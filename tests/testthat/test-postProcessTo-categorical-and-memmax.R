test_that("postProcessTo preserves categorical datatype and restores terraOptions(memmax)", {
  testInit("terra",
    needGoogleDriveAuth = FALSE,
    opts = list(
      reproducible.useMemoise = FALSE,
      reproducible.cacheSaveFormat = .qs2Format
    )
  )
  withr::local_options(reproducible.cachePath = tmpCache)
  skip_if_not_installed("terra")

  # ---- build a small categorical INT1U raster on disk -----------------------
  # Use a coarse grid in a projected CRS so re-projection is meaningful.
  src <- terra::rast(
    nrows = 100, ncols = 100,
    xmin = 0, xmax = 1e5, ymin = 0, ymax = 1e5,
    crs = "EPSG:3978"
  )
  cls <- data.frame(
    value = c(20, 31, 32, 33, 40, 50, 80, 100, 210, 220, 230, 240),
    label = c("water", "snow_ice", "rock_rubble", "exposed_barren_land",
              "bryoids", "shrubs", "wetland", "herbs",
              "coniferous", "broadleaf", "mixedwood", "disturbed")
  )
  set.seed(7)
  terra::values(src) <- sample(cls$value, terra::ncell(src), replace = TRUE)
  levels(src) <- cls
  srcFile <- tempfile(fileext = ".tif")
  terra::writeRaster(src, srcFile, overwrite = TRUE, datatype = "INT1U")
  from <- terra::rast(srcFile)

  # sanity: input really is INT1U + factor
  expect_identical(terra::datatype(from)[1], "INT1U")
  expect_true(terra::is.factor(from)[1])

  # ---- a target template at a different CRS -------------------------------
  tcrs <- "EPSG:3979"
  templ <- terra::project(from, tcrs, method = "near")

  # ---- A: postProcessTo without explicit method/datatype -------------------
  # With our fix, factor input should trigger method="near" + datatype="INT1U"
  # automatically.
  outFile <- tempfile(fileext = ".tif")
  out <- reproducible::postProcessTo(
    from = from,
    cropTo = templ,
    maskTo = templ,
    projectTo = templ,
    writeTo = outFile,
    verbose = FALSE
  )

  # (a) factor table survives
  expect_true(terra::is.factor(out)[1])

  # (b) on-disk datatype preserved (no INT1U -> FLT4S promotion)
  expect_identical(terra::datatype(terra::rast(outFile))[1], "INT1U")

  # (c) no invented categories from bilinear interpolation: every category
  #     present in the output must come from the source's scheme. terra::unique
  #     on a factor SpatRaster returns the active-category labels, so check
  #     against cls$label (semantically equivalent to checking integer codes
  #     against cls$value when the RAT is intact).
  vals <- terra::unique(out)[, 1]
  vals <- vals[!is.na(vals)]
  expect_true(all(vals %in% cls$label),
    info = paste("Unexpected values in output:",
                 paste(setdiff(vals, cls$label), collapse = ",")))

  # ---- B: terraOptions(memmax) is restored after the call ------------------
  withr::with_options(list(reproducible.terraMemmax = 1), {
    pre <- terra::terraOptions(print = FALSE)$memmax
    out2 <- reproducible::postProcessTo(
      from = from, projectTo = templ, maskTo = templ, cropTo = templ,
      verbose = FALSE
    )
    post <- terra::terraOptions(print = FALSE)$memmax
    # Equal up to coercion; NA == NA via identical()
    expect_identical(post, pre)
  })

  # ---- B2: if the user has set terraOptions(memmax) explicitly, leave it ----
  # alone (don't let reproducible.terraMemmax override an intentional setting).
  origMemmax <- terra::terraOptions(print = FALSE)$memmax
  on.exit(terra::terraOptions(memmax = origMemmax), add = TRUE)
  terra::terraOptions(memmax = 7) # arbitrary positive value, clearly user-set
  withr::with_options(list(reproducible.terraMemmax = 1), {
    reproducible::postProcessTo(
      from = from, projectTo = templ, maskTo = templ, cropTo = templ,
      verbose = FALSE
    )
    # The user's 7 must survive the call (not be replaced by 1).
    expect_equal(terra::terraOptions(print = FALSE)$memmax, 7)
  })
  terra::terraOptions(memmax = origMemmax) # restore for the rest of the file

  # ---- C: caller-supplied method/datatype is respected (not overridden) ----
  outFile3 <- tempfile(fileext = ".tif")
  out3 <- reproducible::postProcessTo(
    from = from,
    cropTo = templ, maskTo = templ, projectTo = templ,
    writeTo = outFile3,
    datatype = "INT2U", # user override; ours would have picked INT1U
    verbose = FALSE
  )
  expect_identical(terra::datatype(terra::rast(outFile3))[1], "INT2U")
})
