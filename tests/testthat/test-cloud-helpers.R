test_that("isID accepts 32 and 33 character strings", {
  expect_true(reproducible:::isID(strrep("a", 32L)))
  expect_true(reproducible:::isID(strrep("a", 33L)))
})

test_that("isID rejects strings outside the 32-33 length window", {
  expect_false(reproducible:::isID(strrep("a", 31L)))
  expect_false(reproducible:::isID(strrep("a", 34L)))
  expect_false(reproducible:::isID(""))
})

test_that("isOrHasRaster identifies a SpatRaster", {
  skip_if_not_installed("terra")
  r <- terra::rast(terra::ext(0, 1, 0, 1), vals = 1)
  expect_true(isTRUE(reproducible:::isOrHasRaster(r)))
})

test_that("isOrHasRaster returns FALSE for non-raster atomic objects", {
  expect_false(isTRUE(reproducible:::isOrHasRaster(1L)))
  expect_false(isTRUE(reproducible:::isOrHasRaster("foo")))
  expect_false(isTRUE(reproducible:::isOrHasRaster(data.frame(a = 1))))
})

test_that("isOrHasRaster recurses into lists and environments", {
  skip_if_not_installed("terra")
  r <- terra::rast(terra::ext(0, 1, 0, 1), vals = 1)
  ll <- list(notRaster = 1, alsoNot = "x", inner = list(ras = r))
  out <- reproducible:::isOrHasRaster(ll)
  expect_true(any(unlist(out)))

  env <- new.env(parent = emptyenv())
  env$x <- 1
  env$r <- r
  outE <- reproducible:::isOrHasRaster(env)
  expect_true(any(unlist(outE)))
})

test_that("mergeShownCacheCloud is a no-op when there is no cloud metadata", {
  local <- data.table::data.table(
    cacheId = "aaa", tagKey = c("function", "x"),
    tagValue = c("rnorm", "1"), createdDate = "2026-01-01"
  )
  expect_identical(reproducible:::mergeShownCacheCloud(local, NULL, "rnorm"), local)
  empty <- reproducible:::.emptyCacheTable
  expect_identical(reproducible:::mergeShownCacheCloud(local, empty, "rnorm"), local)
})

test_that("mergeShownCacheCloud restricts cloud rows to the requested function", {
  local <- data.table::data.table(
    cacheId = "aaa", tagKey = c("function", "x"),
    tagValue = c("rnorm", "1"), createdDate = "2026-01-01"
  )
  cloud <- data.table::data.table(
    cacheId = c("bbb", "bbb", "ccc", "ccc"),
    tagKey = c("function", "x", "function", "x"),
    tagValue = c("rnorm", "2", "runif", "3"),
    createdDate = "2026-01-02"
  )
  out <- reproducible:::mergeShownCacheCloud(local, cloud, "rnorm")
  ## ccc was a different function and is dropped; aaa (local) + bbb (cloud) remain
  expect_setequal(unique(out$cacheId), c("aaa", "bbb"))
  expect_false("ccc" %in% out$cacheId)
})

test_that("mergeShownCacheCloud de-duplicates overlapping cacheId/tagKey/tagValue rows", {
  local <- data.table::data.table(
    cacheId = "aaa", tagKey = c("function", "x"),
    tagValue = c("rnorm", "1"), createdDate = "2026-01-01"
  )
  ## same rows as local, plus one genuinely new row -> only the new row is added
  cloud <- data.table::data.table(
    cacheId = "aaa", tagKey = c("function", "x", "y"),
    tagValue = c("rnorm", "1", "9"), createdDate = "2026-01-01"
  )
  out <- reproducible:::mergeShownCacheCloud(local, cloud, "rnorm")
  expect_equal(NROW(out), 3L)
  expect_true(all(c("x", "y", "function") %in% out$tagKey))
})

test_that("mergeShownCacheCloud keeps all cloud rows when .functionName is NULL", {
  local <- reproducible:::.emptyCacheTable
  cloud <- data.table::data.table(
    cacheId = c("bbb", "ccc"), tagKey = "function",
    tagValue = c("rnorm", "runif"), createdDate = "2026-01-02"
  )
  out <- reproducible:::mergeShownCacheCloud(local, cloud, NULL)
  expect_setequal(unique(out$cacheId), c("bbb", "ccc"))
})
