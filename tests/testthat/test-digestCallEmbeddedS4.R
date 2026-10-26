# A call can carry evaluated objects: Cache() replaces names by their values inside an
# unevaluated pipe (`{ f(.area) |> g() } |> Cache()`), so the slow inner step is not run
# before the digest. The digest of such a call must depend on those objects. It used to be
# format(call), and a live terra object formats as a pointer, so different areas collided.

skipIfNoTerra <- function() skip_if_not_installed("terra")

areas <- function() list(
  terra::vect(terra::ext(0, 1000, 0, 1000), crs = "EPSG:3005"),
  terra::vect(terra::ext(5000, 6000, 0, 1000), crs = "EPSG:3005")
)
callWith <- function(obj) as.call(list(as.name("f"), area = obj))

test_that("a call with a different SpatVector or SpatRaster inside has a different digest", {
  skipIfNoTerra()
  a <- areas()
  r <- lapply(a, function(v) terra::rast(terra::ext(v), resolution = 100, crs = "EPSG:3005", vals = 1))
  expect_false(identical(.robustDigest(callWith(a[[1]])), .robustDigest(callWith(a[[2]]))))
  expect_false(identical(.robustDigest(callWith(r[[1]])), .robustDigest(callWith(r[[2]]))))
  ## and the same object gives the same digest
  expect_identical(.robustDigest(callWith(a[[1]])), .robustDigest(callWith(terra::deepcopy(a[[1]]))))
})

test_that("a call with no S4 object inside keeps its digest", {
  cl <- quote(f(x = 1, y = "a", z = g(2)))
  expect_identical(.robustDigest(cl), .doDigest(format(cl), algo = formals(.robustDigest)$algo))
})

test_that("a braced pipe in Cache() keys on the terra object it uses", {
  skipIfNoTerra()
  testInit(opts = list(reproducible.useMemoise = FALSE))
  mk <- function(area) terra::rast(terra::ext(area), resolution = 100, crs = "EPSG:3005", vals = 1)
  a <- areas()
  .area <- a[[1]]
  r1 <- { mk(.area) |> terra::aggregate(fact = 2) } |> Cache(cachePath = tmpCache)
  .area <- a[[2]]
  r2 <- { mk(.area) |> terra::aggregate(fact = 2) } |> Cache(cachePath = tmpCache)
  expect_equal(terra::xmin(r1), 0)
  expect_equal(terra::xmin(r2), 5000)
})

test_that("a call with an empty argument digests as before", {
  cl <- quote(f(x[, 1], y = g(z[1, ])))
  expect_identical(.robustDigest(cl), .doDigest(format(cl), algo = formals(.robustDigest)$algo))
})
