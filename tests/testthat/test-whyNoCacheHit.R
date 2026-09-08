## The question this answers -- "why did my run not reuse the cache, and what
## changed?" -- is normally answered by staring at two calls. Every cache entry
## already records the hash of each element it digested, addressed by its path,
## so the answer is a comparison, not an investigation.

test_that("it names the element that changed, however deeply nested", {
  cachePath <- withr::local_tempdir()
  f <- function(x, settings) paste(x, length(settings))
  Cache(f, x = 1, settings = list(a = 1, b = list(name = "12.4")),
        cachePath = cachePath, verbose = -2)
  Cache(f, x = 1, settings = list(a = 1, b = list(name = 12.4)),
        cachePath = cachePath, verbose = -2)   # character -> numeric, three levels down

  out <- whyNoCacheHit(cachePath = cachePath, verbose = -2)
  expect_s3_class(out, "whyNoCacheHit")
  expect_equal(nrow(out), 1L)
  expect_equal(out$element, "settings.b.name")
  expect_equal(out$status, "differs")
  expect_true(nzchar(out$thisCall) && nzchar(out$otherCall))
  expect_false(identical(out$thisCall, out$otherCall))
})

test_that("an added or removed argument is reported as such, not as a change", {
  cachePath <- withr::local_tempdir()
  f <- function(x, ...) x
  Cache(f, x = 1, cachePath = cachePath, verbose = -2)
  Cache(f, x = 1, extra = "new", cachePath = cachePath, verbose = -2)

  out <- whyNoCacheHit(cachePath = cachePath, verbose = -2)
  expect_true("extra" %in% out$element)
  expect_equal(out$status[out$element == "extra"], "only in this call")
})

test_that("the closest previous call comes first, so the report is about the right one", {
  cachePath <- withr::local_tempdir()
  f <- function(a, b, c) a + b + c
  Cache(f, a = 1, b = 1, c = 1, cachePath = cachePath, verbose = -2)  # differs in 2
  Cache(f, a = 1, b = 2, c = 2, cachePath = cachePath, verbose = -2)  # differs in 1
  Cache(f, a = 1, b = 2, c = 3, cachePath = cachePath, verbose = -2)  # the call in question

  out <- whyNoCacheHit(cachePath = cachePath, n = 3, verbose = -2)
  first <- out[out$candidate == out$candidate[[1L]], ]
  expect_equal(nrow(first), 1L)
  expect_equal(first$element, "c")
})

test_that("a specific pair can be compared, and identical inputs yield no differences", {
  cachePath <- withr::local_tempdir()
  f <- function(a) a
  Cache(f, a = 1, cachePath = cachePath, verbose = -2)
  Cache(f, a = 2, cachePath = cachePath, verbose = -2)
  ids <- unique(showCache(cachePath, verbose = -2)$cacheId)
  expect_length(ids, 2L)

  out <- whyNoCacheHit(cacheId = ids[[1L]], other = ids[[2L]], cachePath = cachePath, verbose = -2)
  expect_equal(out$candidate, ids[[2L]])
  expect_equal(out$element, "a")

  same <- whyNoCacheHit(cacheId = ids[[1L]], other = ids[[1L]], cachePath = cachePath, verbose = -2)
  expect_equal(nrow(same), 0L)
})

test_that("it says so plainly when there is nothing to compare against", {
  cachePath <- withr::local_tempdir()
  f <- function(a) a
  Cache(f, a = 1, cachePath = cachePath, verbose = -2)
  out <- whyNoCacheHit(cachePath = cachePath, verbose = -2)
  expect_equal(nrow(out), 0L)
  expect_output(print(out), "Nothing to compare")
})

test_that("a cacheId that is not in the repository is an error, not an empty answer", {
  cachePath <- withr::local_tempdir()
  f <- function(a) a
  Cache(f, a = 1, cachePath = cachePath, verbose = -2)
  expect_error(whyNoCacheHit(cacheId = "notacacheid", cachePath = cachePath, verbose = -2),
               "No cache entry")
})

test_that("printing gives the one-line answer", {
  cachePath <- withr::local_tempdir()
  f <- function(x, settings) paste(x, length(settings))
  Cache(f, x = 1, settings = list(b = list(name = "12.4")), cachePath = cachePath, verbose = -2)
  Cache(f, x = 1, settings = list(b = list(name = 12.4)), cachePath = cachePath, verbose = -2)
  expect_output(print(whyNoCacheHit(cachePath = cachePath, verbose = -2)), "settings.b.name")
})
