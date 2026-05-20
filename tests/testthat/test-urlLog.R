test_that("urlLog: off by default", {
  testInit()
  withr::local_options(reproducible.urlLog = NULL)
  clearUrlLog()
  reproducible:::.logUrlAccess("prepInputs", "https://example.com/x.tif")
  expect_length(getUrlLog(), 0L)
})

test_that("urlLog: TRUE sink + idempotency + clear", {
  testInit()
  withr::local_options(reproducible.urlLog = TRUE)
  clearUrlLog()

  reproducible:::.logUrlAccess("prepInputs", "https://example.com/a.tif",
                               destinationPath = "/tmp", cacheId = "abc")
  reproducible:::.logUrlAccess("prepInputs", "https://example.com/a.tif",
                               destinationPath = "/tmp", cacheId = "abc")
  reproducible:::.logUrlAccess("prepInputs", "https://example.com/b.tif",
                               destinationPath = "/tmp", cacheId = "abc")

  log <- getUrlLog()
  expect_length(log, 2L)
  expect_equal(log[[1]]$url, "https://example.com/a.tif")
  expect_equal(log[[2]]$url, "https://example.com/b.tif")

  clearUrlLog()
  expect_length(getUrlLog(), 0L)
})

test_that("urlLog: environment sink populates env$records and env$seen", {
  testInit()
  e <- new.env(parent = emptyenv())
  withr::local_options(reproducible.urlLog = e)

  reproducible:::.logUrlAccess("prepInputs", "https://example.com/a.tif",
                               cacheId = "abc", cacheHit = FALSE,
                               via = "prepInputs")
  reproducible:::.logUrlAccess("prepInputs", "https://example.com/a.tif",
                               cacheId = "abc", cacheHit = TRUE,
                               via = "Cache")  # dedup: same (fn,url,cacheId)

  expect_length(e$records, 1L)
  expect_length(e$seen, 1L)
  expect_equal(e$records[[1]]$url, "https://example.com/a.tif")
  expect_equal(e$records[[1]]$fn,  "prepInputs")
  expect_equal(e$records[[1]]$cacheId, "abc")

  reproducible:::.logUrlAccess("prepInputs", "https://example.com/a.tif",
                               cacheId = "DIFFERENT")
  expect_length(e$records, 2L)
})

test_that("urlLog: preProcess head labels caller as 'prepInputs' when .tempPath supplied", {
  testInit()
  withr::local_options(reproducible.urlLog = TRUE)
  clearUrlLog()

  ## Direct preProcess() call -> .tempPath missing -> labelled "preProcess"
  ## (we exercise this without network by stubbing as a minimal recorder
  ## that mirrors what the real preProcess head does.)
  fakePreProcess <- function(url, destinationPath = ".", .tempPath) {
    reproducible:::.logUrlAccess(
      if (missing(.tempPath)) "preProcess" else "prepInputs",
      url, destinationPath = destinationPath)
  }

  fakePreProcess(url = "https://example.com/a.tif")
  fakePreProcess(url = "https://example.com/b.tif", .tempPath = tempdir())

  log <- getUrlLog()
  expect_length(log, 2L)
  expect_equal(log[[1]]$fn, "preProcess")
  expect_equal(log[[2]]$fn, "prepInputs")
})

test_that("urlLog: function callback sink invoked with each record", {
  testInit()
  collected <- list()
  cb <- function(rec) collected[[length(collected) + 1L]] <<- rec
  withr::local_options(reproducible.urlLog = cb)
  reproducible:::.logUrlAccess("prepInputs", "https://example.com/a.tif")
  reproducible:::.logUrlAccess("prepInputs", "https://example.com/b.tif")
  expect_length(collected, 2L)
  expect_equal(collected[[1]]$url, "https://example.com/a.tif")
})

test_that("urlLog: NULL url is ignored", {
  testInit()
  withr::local_options(reproducible.urlLog = TRUE)
  clearUrlLog()
  reproducible:::.logUrlAccess("prepInputs", NULL)
  reproducible:::.logUrlAccess("prepInputs", character(0))
  expect_length(getUrlLog(), 0L)
})

test_that("urlLog: idempotency key handles NA cacheId", {
  testInit()
  e <- new.env(parent = emptyenv())
  withr::local_options(reproducible.urlLog = e)
  reproducible:::.logUrlAccess("prepInputs", "https://example.com/a.tif",
                               cacheId = NA_character_)
  reproducible:::.logUrlAccess("prepInputs", "https://example.com/a.tif",
                               cacheId = NA_character_)
  expect_length(e$records, 1L)
})

test_that("urlLog: Cache(Map(...prepInputs(url=url))) attaches urls via frame on miss + replays on hit", {
  testInit("terra")
  withr::local_options(reproducible.cachePath = tmpdir)
  e <- new.env(parent = emptyenv())
  withr::local_options(reproducible.urlLog = e)

  urls <- c("https://example.com/a.tif", "https://example.com/b.tif")

  ## Stub prepInputs that calls the real function-head hook, so the frame
  ## mechanism gets exercised without touching the network.
  fakePrepInputs <- function(url, destinationPath = ".") {
    reproducible:::.logUrlAccess("prepInputs", url,
                                 destinationPath = destinationPath)
    url
  }
  prepInputs <- fakePrepInputs

  ## First call: cache miss. Inner stub fires hook -> urls pushed to Cache
  ## frame -> Cache attaches tags + emits session records on save.
  r1 <- Cache(Map(url = urls, function(url) prepInputs(url = url)))

  expect_length(e$records, 2L)
  cids <- unique(vapply(e$records, function(r) r$cacheId, character(1)))
  expect_length(cids, 1L)
  expect_true(all(vapply(e$records, function(r) r$cacheHit, logical(1)) == FALSE))
  expect_setequal(vapply(e$records, function(r) r$url, character(1)), urls)

  sc <- showCache(tmpdir, cacheId = cids)
  expect_true("reproducible.url"          %in% sc$tagKey)
  expect_true("reproducible.urlHitCount"  %in% sc$tagKey)

  ## Second call: cache hit. Fresh env -> replay from DB tags.
  e2 <- new.env(parent = emptyenv())
  withr::local_options(reproducible.urlLog = e2)
  r2 <- Cache(Map(url = urls, function(url) prepInputs(url = url)))

  expect_length(e2$records, 2L)
  expect_true(all(vapply(e2$records, function(r) r$cacheHit, logical(1)) == TRUE))
  expect_setequal(vapply(e2$records, function(r) r$url, character(1)), urls)

  sc2 <- showCache(tmpdir, cacheId = cids)
  hc <- as.integer(sc2$tagValue[sc2$tagKey == "reproducible.urlHitCount"])
  expect_true(any(hc >= 1L))
})

test_that("urlLog: Cache(Map(..., function(url) prepInputs(url=url))) does not error", {
  testInit("terra")
  withr::local_options(
    reproducible.urlLog   = TRUE,
    reproducible.cachePath = tmpdir
  )
  clearUrlLog()

  ## Mask prepInputs locally with a stub that invokes the real hook
  ## (so the frame mechanism gets exercised).
  fakePrepInputs <- function(url, destinationPath = ".") {
    reproducible:::.logUrlAccess("prepInputs", url,
                                 destinationPath = destinationPath)
    url
  }
  prepInputs <- fakePrepInputs
  urls <- c("https://example.com/a.tif", "https://example.com/b.tif")

  expect_no_error({
    Cache(Map(url = urls, function(url) prepInputs(url = url)))
  })
})

test_that("urlLog: Cache hooks tag cacheId with reproducible.url* tags", {
  testInit("terra", needInternet = FALSE)
  withr::local_options(
    reproducible.urlLog   = TRUE,
    reproducible.cachePath = tmpdir
  )
  clearUrlLog()

  ## Drive the Cache hooks via a synthetic call that does NOT touch the network:
  ## the stub invokes the real function-head hook so the frame mechanism
  ## carries the URL into the Cache tag-write path.
  fakePrepInputs <- function(url, destinationPath = ".") {
    reproducible:::.logUrlAccess("prepInputs", url,
                                 destinationPath = destinationPath)
    "ok"
  }
  prepInputs <- fakePrepInputs

  out1 <- Cache(prepInputs(url = "https://example.com/a.tif"))
  out2 <- Cache(prepInputs(url = "https://example.com/a.tif"))  # hit
  expect_equal(as.character(out1), "ok")
  expect_equal(as.character(out2), "ok")

  sc <- showCache(tmpdir, userTags = "reproducible.url")
  expect_true(NROW(sc) > 0L)
  expect_true("reproducible.url"          %in% sc$tagKey)
  expect_true("reproducible.urlFn"        %in% sc$tagKey)
  expect_true("reproducible.urlFirstSeen" %in% sc$tagKey)
  expect_true("reproducible.urlLastSeen"  %in% sc$tagKey)
  expect_true("reproducible.urlHitCount"  %in% sc$tagKey)

  ## hitCount should be >= 1 after the second (hit) call
  hc <- as.integer(sc$tagValue[sc$tagKey == "reproducible.urlHitCount"])
  expect_true(any(hc >= 1L))

  ## Session log saw both accesses but dedup keeps it to one record per cacheId
  expect_length(getUrlLog(), 1L)
})
