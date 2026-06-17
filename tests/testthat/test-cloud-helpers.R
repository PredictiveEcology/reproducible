test_that("isID accepts 32 and 33 character strings", {
  expect_true(reproducible:::isID(strrep("a", 32L)))
  expect_true(reproducible:::isID(strrep("a", 33L)))
})

test_that("isID rejects strings outside the 32-33 length window", {
  expect_false(reproducible:::isID(strrep("a", 31L)))
  expect_false(reproducible:::isID(strrep("a", 34L)))
  expect_false(reproducible:::isID(""))
})

test_that(".cloudNamePrefixQuery pushes a single cacheId to the server, NULL otherwise", {
  # A plain cacheId (the per-Cache lookup) is a name PREFIX -> a `name contains`
  # query so Drive returns only that cacheId's files, not the whole folder.
  expect_identical(reproducible:::.cloudNamePrefixQuery("bedb839348d3b36e"),
                   "name contains 'bedb839348d3b36e'")
  # Not prefix-safe -> NULL (fall back to full listing + local filter):
  expect_null(reproducible:::.cloudNamePrefixQuery(".dbFile."))          # a suffix / has metachars
  expect_null(reproducible:::.cloudNamePrefixQuery("tagA|tagB"))         # regex alternation
  expect_null(reproducible:::.cloudNamePrefixQuery(c("a", "b")))         # more than one token
  expect_null(reproducible:::.cloudNamePrefixQuery(NULL))
  expect_null(reproducible:::.cloudNamePrefixQuery(NA_character_))
  expect_null(reproducible:::.cloudNamePrefixQuery(""))
})

test_that("driveLs scopes a cacheId lookup server-side (q), and not a regex pattern", {
  skip_if_not_installed("googledrive")
  # Capture the args drive_ls() is called with, without hitting the network.
  seenQ <- new.env()
  fakeLs <- function(path, pattern, q, ...) {
    seenQ$q <- q
    seenQ$pattern <- pattern
    googledrive::as_dribble()        # empty dribble (0 rows)
  }
  testthat::local_mocked_bindings(drive_ls = fakeLs, .package = "googledrive")

  folder <- googledrive::as_dribble() # an (empty) tbl, so checkAndMakeCloudFolderID is skipped

  # cacheId lookup -> server-side `name contains` query
  reproducible:::driveLs(folder, pattern = "bedb839348d3b36e", verbose = -1)
  expect_identical(seenQ$q, "name contains 'bedb839348d3b36e'")

  # dbFile-suffix listing -> no server query (NULL); full listing + local filter
  reproducible:::driveLs(folder, pattern = reproducible:::suffixMultipleDBFiles(), verbose = -1)
  expect_null(seenQ$q)
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

test_that(".downloadCloudDBFile keys on a non-reserved arg (not cacheId)", {
  # showCacheCloud() wraps .downloadCloudDBFile() in Cache() to memoise each
  # remote .dbFile by its content hash. If the keying arg were named `cacheId`
  # (which is in .defaultCacheOmitArgs) Cache() would drop it from the digest
  # and collapse every file onto one entry -- listing only one similar item.
  fmls <- names(formals(reproducible:::.downloadCloudDBFile))
  expect_true("hash" %in% fmls)
  expect_false("cacheId" %in% fmls)
  expect_false("hash" %in% reproducible:::.defaultCacheOmitArgs)
})

test_that("Cache() keyed on a non-reserved arg yields distinct entries", {
  # Guards the assumption behind showCacheCloud's memo: varying the (non-reserved)
  # key arg must produce distinct cache entries. (A regression here is what made
  # showSimilar list only one cloud item.)
  skip_on_cran()
  skip_if_not_installed("terra") # Cache()'s .wrap path needs terra even for a data.table
  tmp <- file.path(tempdir(), paste0("ccloud-key-", Sys.getpid()))
  oldOpts <- options(reproducible.cachePath = tmp, reproducible.useDBI = FALSE,
                     reproducible.showSimilar = FALSE, reproducible.verbose = -1)
  on.exit({options(oldOpts); unlink(tmp, recursive = TRUE)}, add = TRUE)
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  f <- function(id, hash) data.table::data.table(v = hash)
  got <- vapply(c("h1", "h2", "h3"), function(h)
    (f(id = "same", hash = h) |>
       Cache(cachePath = tmp, omitArgs = "id", .functionName = "m", verbose = -1))$v,
    character(1))
  expect_identical(unname(got), c("h1", "h2", "h3"))
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
