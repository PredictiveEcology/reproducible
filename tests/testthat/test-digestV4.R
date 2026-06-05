# Tests for `reproducible.digestVersion` 4 -- the platform-stable digest of
# sf / SpatVector objects (R/robustDigest.R) which is now the default. Earlier
# versions could digest the same vector differently across operating systems,
# which broke shared/cloud caching of these objects. Version 4 normalizes
# geometry (rounded numeric vertex matrix) and attributes so the cacheId is
# identical across OSes. The superseded booleans digestV3/digestV4 still map to
# versions (see the back-compat test).

test_that("the digest version defaults to 4", {
  withr::local_options(reproducible.digestVersion = NULL,
                       reproducible.digestV3 = TRUE, reproducible.digestV4 = FALSE)
  expect_identical(reproducible:::.digestVersion(), 4L)
})

test_that("digestVersion supersedes, but still honours, digestV3/digestV4", {
  # explicit digestVersion wins
  expect_identical(withr::with_options(list(reproducible.digestVersion = 3),
                                       reproducible:::.digestVersion()), 3L)
  expect_identical(withr::with_options(list(reproducible.digestVersion = 2),
                                       reproducible:::.digestVersion()), 2L)
  # back-compat: the old booleans set the version when digestVersion is unset
  expect_identical(withr::with_options(
    list(reproducible.digestVersion = NULL, reproducible.digestV4 = TRUE),
    reproducible:::.digestVersion()), 4L)
  expect_identical(withr::with_options(
    list(reproducible.digestVersion = NULL, reproducible.digestV3 = FALSE),
    reproducible:::.digestVersion()), 2L)
  # new option overrides the old boolean
  expect_identical(withr::with_options(
    list(reproducible.digestVersion = 4, reproducible.digestV4 = FALSE),
    reproducible:::.digestVersion()), 4L)
})

test_that("digestVersion 4 cacheId is identical across operating systems (golden)", {
  # The cross-OS guarantee, checked automatically by the multi-OS CI matrix
  # (ubuntu / windows / macOS): a fixed input must hash to a fixed value on every
  # OS, so no human has to run it on each OS and compare.
  #   * fails on ONE OS but passes on others -> digestVersion 4 is NOT platform-stable
  #     (a real bug to fix);
  #   * fails on ALL OSes -> the algorithm or terra's geom() output changed;
  #     recompute `golden` below (it is not OS-specific).
  # The accented PolyID (e-acute) stresses the attribute path; the \u escape keeps
  # the source bytes identical on every OS regardless of file encoding.
  skip_if_not_installed("terra")
  withr::local_options(reproducible.digestVersion = 4)
  golden <- "fee95802937421d3"
  wkt <- "POLYGON ((0 0, 0 1.1234565, 1 1, 1 0, 0 0))"
  v <- terra::vect(wkt)
  v$PolyID <- "Qu\u00e9bec"
  v$n <- 3L
  expect_identical(reproducible:::.robustDigest(v), golden,
                   info = paste("terra", as.character(packageVersion("terra"))))
  skip_if_not_installed("sf")
  s <- sf::st_as_sf(data.frame(PolyID = "Qu\u00e9bec", n = 3L, g = wkt), wkt = "g")
  expect_identical(reproducible:::.robustDigest(s), golden) # sf == SpatVector, every OS
})

test_that("digestVersion switches the SpatVector algorithm (3 = legacy, 4 = new)", {
  skip_if_not_installed("terra")
  v <- terra::vect("POLYGON ((0 0, 0 1, 1 1, 1 0, 0 0))")
  v$id <- "a"
  d_off <- withr::with_options(list(reproducible.digestVersion = 3),
                               reproducible:::.robustDigest(v))
  d_on  <- withr::with_options(list(reproducible.digestVersion = 4),
                               reproducible:::.robustDigest(v))
  # Distinct algorithms -> distinct cacheId. (Version 3 keeps the legacy
  # wrapSpatVector path; version 4 uses the platform-stable one.)
  expect_false(identical(d_off, d_on))
})

test_that("digestVersion = 4: sf and the equivalent SpatVector digest identically", {
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")
  wkt <- "POLYGON ((0 0, 0 1.123456789, 1 1, 1 0, 0 0))"
  v <- terra::vect(wkt)
  v$id <- "a"
  v$n <- 3L
  sfobj <- sf::st_as_sf(data.frame(id = "a", n = 3L, g = wkt), wkt = "g")
  withr::local_options(reproducible.digestVersion = 4)
  expect_identical(reproducible:::.robustDigest(v), reproducible:::.robustDigest(sfobj))
})

test_that("digestVersion = 4 normalizes sub-precision coordinate differences", {
  skip_if_not_installed("terra")
  # Differ only in the 7th decimal -> rounded to 6 -> identical digest.
  v1 <- terra::vect("POLYGON ((0 0, 0 1.1234561, 1 1, 1 0, 0 0))")
  v2 <- terra::vect("POLYGON ((0 0, 0 1.1234562, 1 1, 1 0, 0 0))")
  withr::local_options(reproducible.digestVersion = 4)
  expect_identical(reproducible:::.robustDigest(v1), reproducible:::.robustDigest(v2))
  # Genuinely different geometry must still differ.
  v3 <- terra::vect("POLYGON ((0 0, 0 2, 1 1, 1 0, 0 0))")
  expect_false(identical(reproducible:::.robustDigest(v1), reproducible:::.robustDigest(v3)))
})

test_that("digestVersion = 3 leaves sf on the generic (unchanged) path", {
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")
  sfobj <- sf::st_as_sf(data.frame(id = "a", g = "POLYGON ((0 0, 0 1, 1 1, 1 0, 0 0))"),
                        wkt = "g")
  d_off <- withr::with_options(list(reproducible.digestVersion = 3),
                               reproducible:::.robustDigest(sfobj))
  d_on  <- withr::with_options(list(reproducible.digestVersion = 4),
                               reproducible:::.robustDigest(sfobj))
  # Version 3 leaves sf on the generic path; version 4 digests it via the
  # SpatVector path -> different digest.
  expect_false(identical(d_off, d_on))
})

test_that("digestVersion = 4 keeps attributes bound to their geometry", {
  # Property check: swapping which feature carries which attribute changes the
  # digest (attributes are not sorted independently of the geometry).
  skip_if_not_installed("terra")
  withr::local_options(reproducible.digestVersion = 4)
  gA <- "POLYGON ((0 0, 0 1, 1 1, 0 0))"
  gB <- "POLYGON ((2 2, 2 3, 3 3, 2 2))"
  v1 <- terra::vect(c(gA, gB)); v1$id <- c("x", "y")
  v2 <- terra::vect(c(gA, gB)); v2$id <- c("y", "x") # same geoms + id set, swapped pairing
  expect_false(identical(reproducible:::.robustDigest(v1),
                         reproducible:::.robustDigest(v2)))
})

test_that("digestVersion 4 attribute digest is invariant to LC_COLLATE (cross-OS proxy)", {
  # The earlier algorithm sorted attribute rows by order(do.call(paste, attrs)),
  # which uses LC_COLLATE -- so accented attribute text ordered (and therefore
  # digested) differently across OS locales: a cross-OS difference. version 4
  # leaves rows in feature order, so the digest is collation-invariant. This
  # reproduces the cross-OS condition on a single machine by switching
  # LC_COLLATE (and fails under the old row-sort). It skips where two locales
  # that reorder accented text are not available (e.g. some CI images).
  skip_if_not_installed("terra")
  withr::local_options(reproducible.digestVersion = 4)
  orig <- Sys.getlocale("LC_COLLATE")
  withr::defer(suppressWarnings(Sys.setlocale("LC_COLLATE", orig)))
  ids <- c("z", "\u00e9", "a") # accented (e-acute) -> collation-sensitive; \u keeps bytes portable
  cand <- c("C", "en_US.UTF-8", "C.UTF-8", "POSIX", "en_CA.UTF-8")
  usable <- Filter(function(l) nzchar(suppressWarnings(Sys.setlocale("LC_COLLATE", l))), cand)
  orders <- unique(vapply(usable, function(l) {
    suppressWarnings(Sys.setlocale("LC_COLLATE", l)); paste(order(ids), collapse = "")
  }, character(1)))
  skip_if(length(orders) < 2,
          "need two LC_COLLATE locales that order accented text differently")
  mk <- function() {
    v <- terra::vect(c("POINT (0 0)", "POINT (1 1)", "POINT (2 2)"))
    v$id <- ids
    v
  }
  digs <- vapply(usable, function(l) {
    suppressWarnings(Sys.setlocale("LC_COLLATE", l))
    reproducible:::.robustDigest(mk())
  }, character(1))
  expect_equal(length(unique(digs)), 1L) # identical under every locale
})

test_that("digestVersion = 4 distinguishes geometry type (polygon vs line) with same vertices", {
  skip_if_not_installed("terra")
  poly <- terra::vect("POLYGON ((0 0, 0 1, 1 1, 0 0))")
  line <- terra::vect("LINESTRING (0 0, 0 1, 1 1, 0 0)")
  withr::local_options(reproducible.digestVersion = 4)
  expect_false(identical(reproducible:::.robustDigest(poly),
                         reproducible:::.robustDigest(line)))
})

test_that("digestSpatVector is robust to no-attribute and single-row vectors", {
  skip_if_not_installed("terra")
  v0 <- terra::vect("POLYGON ((0 0, 0 1, 1 1, 1 0, 0 0))") # no attributes
  expect_silent(reproducible:::digestSpatVector(v0))
  v1 <- terra::vect("POLYGON ((0 0, 0 1, 1 1, 1 0, 0 0))") # single row
  v1$id <- "a"
  expect_silent(reproducible:::digestSpatVector(v1))
})
