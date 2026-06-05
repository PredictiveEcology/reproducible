# Tests for the opt-in `reproducible.digestV4` platform-stable digest of
# sf / SpatVector objects (R/robustDigest.R). The previous algorithm could
# digest the same vector differently across operating systems, which broke
# shared/cloud caching of these objects. digestV4 normalizes geometry (WKT with
# rounded coordinates) and attributes so the cacheId is identical across OSes.

test_that("reproducible.digestV4 defaults to FALSE (opt-in)", {
  expect_false(isTRUE(reproducibleOptions()[["reproducible.digestV4"]]))
})

test_that("digestV4 switches the SpatVector algorithm (FALSE = legacy, TRUE = new)", {
  skip_if_not_installed("terra")
  v <- terra::vect("POLYGON ((0 0, 0 1, 1 1, 1 0, 0 0))")
  v$id <- "a"
  d_off <- withr::with_options(list(reproducible.digestV4 = FALSE),
                               reproducible:::.robustDigest(v))
  d_on  <- withr::with_options(list(reproducible.digestV4 = TRUE),
                               reproducible:::.robustDigest(v))
  # Distinct algorithms -> distinct cacheId. (FALSE keeps the legacy
  # wrapSpatVector path, so existing caches are not invalidated by default.)
  expect_false(identical(d_off, d_on))
})

test_that("digestV4 = TRUE: sf and the equivalent SpatVector digest identically", {
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")
  wkt <- "POLYGON ((0 0, 0 1.123456789, 1 1, 1 0, 0 0))"
  v <- terra::vect(wkt)
  v$id <- "a"
  v$n <- 3L
  sfobj <- sf::st_as_sf(data.frame(id = "a", n = 3L, g = wkt), wkt = "g")
  withr::local_options(reproducible.digestV4 = TRUE)
  expect_identical(reproducible:::.robustDigest(v), reproducible:::.robustDigest(sfobj))
})

test_that("digestV4 = TRUE normalizes sub-precision coordinate differences", {
  skip_if_not_installed("terra")
  # Differ only in the 7th decimal -> rounded to 6 -> identical digest.
  v1 <- terra::vect("POLYGON ((0 0, 0 1.1234561, 1 1, 1 0, 0 0))")
  v2 <- terra::vect("POLYGON ((0 0, 0 1.1234562, 1 1, 1 0, 0 0))")
  withr::local_options(reproducible.digestV4 = TRUE)
  expect_identical(reproducible:::.robustDigest(v1), reproducible:::.robustDigest(v2))
  # Genuinely different geometry must still differ.
  v3 <- terra::vect("POLYGON ((0 0, 0 2, 1 1, 1 0, 0 0))")
  expect_false(identical(reproducible:::.robustDigest(v1), reproducible:::.robustDigest(v3)))
})

test_that("digestV4 = FALSE leaves sf on the generic (unchanged) path", {
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")
  sfobj <- sf::st_as_sf(data.frame(id = "a", g = "POLYGON ((0 0, 0 1, 1 1, 1 0, 0 0))"),
                        wkt = "g")
  d_off <- withr::with_options(list(reproducible.digestV4 = FALSE),
                               reproducible:::.robustDigest(sfobj))
  d_on  <- withr::with_options(list(reproducible.digestV4 = TRUE),
                               reproducible:::.robustDigest(sfobj))
  # The opt-in path produces a different digest than the default generic path.
  expect_false(identical(d_off, d_on))
})

test_that("digestSpatVector is robust to no-attribute and single-row vectors", {
  skip_if_not_installed("terra")
  v0 <- terra::vect("POLYGON ((0 0, 0 1, 1 1, 1 0, 0 0))") # no attributes
  expect_silent(reproducible:::digestSpatVector(v0))
  v1 <- terra::vect("POLYGON ((0 0, 0 1, 1 1, 1 0, 0 0))") # single row
  v1$id <- "a"
  expect_silent(reproducible:::digestSpatVector(v1))
})
