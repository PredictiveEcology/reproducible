# sf and SpatVector share a cacheId (digestVersion >= 4), so a cache hit can come
# from the other class. Cache() must return a spatial-vector result in the class
# of the input (R/cache-helpers.R returnInClassOfInput, used in R/cache.R).

skipIfNoSpatial <- function() {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")
}

makeVect <- function() {
  terra::vect(sf::st_sf(id = 1:2, name = c("a", "b"),
                        geometry = sf::st_sfc(
                          sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 0)))),
                          sf::st_polygon(list(rbind(c(2, 2), c(3, 2), c(3, 3), c(2, 2)))),
                          crs = 4326)))
}

test_that("a cache hit is returned in the class of the input (sf <-> SpatVector)", {
  skipIfNoSpatial()
  testInit(opts = list(reproducible.digestVersion = 4, reproducible.useMemoise = FALSE))
  f <- function(x) x
  v <- makeVect()
  s <- sf::st_as_sf(v)

  r1 <- Cache(f, v, cachePath = tmpCache)
  expect_s4_class(r1, "SpatVector")
  msgs <- capture_messages(r2 <- Cache(f, s, cachePath = tmpCache))
  expect_true(any(grepl("Loaded! Cached result", msgs)))
  expect_true(any(grepl("Cached result was a SpatVector; returned as sf", msgs)))
  expect_s3_class(r2, "sf")
  expect_identical(r2$name, c("a", "b"))
  expect_identical(r2$id, 1:2)

  # reverse: sf first, SpatVector second
  clearCache(tmpCache, ask = FALSE, verbose = -1)
  r3 <- Cache(f, s, cachePath = tmpCache)
  expect_s3_class(r3, "sf")
  msgs <- capture_messages(r4 <- Cache(f, v, cachePath = tmpCache))
  expect_true(any(grepl("Loaded! Cached result", msgs)))
  expect_s4_class(r4, "SpatVector")
  expect_identical(terra::values(r4)$name, c("a", "b"))
})

test_that("the memoised hit is also returned in the class of the input", {
  skipIfNoSpatial()
  testInit(opts = list(reproducible.digestVersion = 4, reproducible.useMemoise = TRUE))
  f <- function(x) x
  v <- makeVect()
  s <- sf::st_as_sf(v)
  Cache(f, v, cachePath = tmpCache)
  msgs <- capture_messages(r2 <- Cache(f, s, cachePath = tmpCache))
  expect_true(any(grepl("Loaded! Memoised result", msgs)))
  expect_s3_class(r2, "sf")
})

test_that("a call without sf/SpatVector arguments is unchanged", {
  skipIfNoSpatial()
  testInit(opts = list(reproducible.digestVersion = 4, reproducible.useMemoise = FALSE))
  g <- function(n) makeVect()
  Cache(g, 1, cachePath = tmpCache)
  msgs <- capture_messages(r2 <- Cache(g, 1, cachePath = tmpCache))
  expect_true(any(grepl("Loaded! Cached result", msgs)))
  expect_false(any(grepl("returned as", msgs)))
  expect_s4_class(r2, "SpatVector")
})
