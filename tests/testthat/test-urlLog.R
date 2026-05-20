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

test_that("urlLog: preProcess record suppressed when prepInputs is on stack", {
  testInit()
  e <- new.env(parent = emptyenv())
  withr::local_options(reproducible.urlLog = e)

  ## Simulate prepInputs -> preProcess
  prepInputs <- function() {
    reproducible:::.logUrlAccess("prepInputs", "https://example.com/x.tif",
                                 cacheId = NA_character_)
    reproducible:::.logUrlAccess("preProcess", "https://example.com/x.tif",
                                 cacheId = NA_character_)
  }
  prepInputs()
  expect_length(e$records, 1L)
  expect_equal(e$records[[1]]$fn, "prepInputs")
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

test_that("urlLog: Cache(Map(..., function(url) prepInputs(url=url))) does not error", {
  testInit("terra")
  withr::local_options(
    reproducible.urlLog   = TRUE,
    reproducible.cachePath = tmpdir
  )
  clearUrlLog()

  fakePrepInputs <- function(url, destinationPath = ".") url
  prepInputs <- fakePrepInputs                      # local masking
  urls <- c("https://example.com/a.tif", "https://example.com/b.tif")

  ## Regression: walker must not descend into the anonymous function body and
  ## try to resolve the `url` formal arg against the calling env (where it
  ## would hit base::url, a closure).
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
  ## Cache a trivial function whose call is shaped like prepInputs(url=...).
  fakePrepInputs <- function(url, destinationPath = ".") "ok"
  ## Make it visible as "prepInputs" to the Cache call inspection:
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
