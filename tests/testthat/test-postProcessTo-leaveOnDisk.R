## reproducible.leaveOnDisk used to set terraOptions(memfrac = 0) when memfrac was at
## terra's default 0.5. memfrac = 0 makes terra::project ~15x slower (34 min on a
## SCANFI study area). The option now sets todisk = TRUE and leaves memfrac alone.

test_that("leaveOnDisk sets todisk, not memfrac, and restores it", {
  testInit("terra", needGoogleDriveAuth = FALSE)

  from <- terra::rast(nrows = 50, ncols = 50, xmin = 0, xmax = 5e4, ymin = 0, ymax = 5e4,
                      crs = "EPSG:3978", vals = 1)
  templ <- terra::rast(nrows = 20, ncols = 20, xmin = 1e4, xmax = 3e4, ymin = 1e4, ymax = 3e4,
                       crs = "EPSG:3978")

  orig <- terra::terraOptions(print = FALSE)[c("memfrac", "todisk")]
  on.exit(terra::terraOptions(memfrac = orig$memfrac, todisk = orig$todisk), add = TRUE)
  terra::terraOptions(memfrac = 0.5, todisk = FALSE)

  ## what terra's options are while postProcessTo is working
  during <- NULL
  local_mocked_bindings(cropTo = function(from, ...) {
    during <<- terra::terraOptions(print = FALSE)[c("memfrac", "todisk")]
    from
  })

  withr::with_options(list(reproducible.leaveOnDisk = TRUE), {
    out <- postProcessTo(from, cropTo = templ, verbose = FALSE)
  })
  expect_equal(during$memfrac, 0.5)
  expect_true(during$todisk)
  expect_false(terra::terraOptions(print = FALSE)$todisk) # restored

  withr::with_options(list(reproducible.leaveOnDisk = FALSE), {
    out <- postProcessTo(from, cropTo = templ, verbose = FALSE)
  })
  expect_false(during$todisk)
})

test_that("memfrac below 0.1 is raised to 0.1 for the call, with a warning", {
  testInit("terra", needGoogleDriveAuth = FALSE)

  from <- terra::rast(nrows = 50, ncols = 50, xmin = 0, xmax = 5e4, ymin = 0, ymax = 5e4,
                      crs = "EPSG:3978", vals = 1)
  templ <- terra::rast(nrows = 20, ncols = 20, xmin = 1e4, xmax = 3e4, ymin = 1e4, ymax = 3e4,
                       crs = "EPSG:3978")

  orig <- terra::terraOptions(print = FALSE)$memfrac
  on.exit(terra::terraOptions(memfrac = orig), add = TRUE)

  during <- NULL
  local_mocked_bindings(cropTo = function(from, ...) {
    during <<- terra::terraOptions(print = FALSE)$memfrac
    from
  })

  for (mf in c(0, 0.01)) {
    terra::terraOptions(memfrac = mf)
    expect_warning(postProcessTo(from, cropTo = templ, verbose = FALSE),
                   .message$memfracTooLowTxt, fixed = TRUE)
    expect_equal(during, 0.1)
    expect_equal(terra::terraOptions(print = FALSE)$memfrac, mf) # restored
  }

  terra::terraOptions(memfrac = 0.1)
  expect_no_warning(postProcessTo(from, cropTo = templ, verbose = FALSE))
  expect_equal(during, 0.1)
})
