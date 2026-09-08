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

test_that("a dry run answers the question before the expensive call is made", {
  ## The point: you should not have to run the miss to learn why it will miss.
  cachePath <- withr::local_tempdir()
  f <- function(x, settings) { Sys.sleep(0); paste(x, length(settings)) }
  Cache(f, x = 1, settings = list(b = list(name = "12.4")), cachePath = cachePath, verbose = -2)
  before <- length(unique(showCache(cachePath, verbose = -2)$cacheId))

  dr <- Cache(f, x = 1, settings = list(b = list(name = 12.4)),
              cachePath = cachePath, dryRun = TRUE, verbose = -2)
  expect_s3_class(dr, "cacheDryRun")
  expect_true(length(dr$preDigest) > 0)
  expect_output(print(dr), "whyNoCacheHit")

  ## Nothing was run and nothing was written.
  expect_equal(length(unique(showCache(cachePath, verbose = -2)$cacheId)), before)

  out <- whyNoCacheHit(dr, verbose = -2)
  expect_equal(out$element, "settings.b.name")
  expect_equal(out$status, "differs")
})

test_that("a whole run can be explained, which is the form a pipeline needs", {
  ## In a SpaDES pipeline the Cache() calls belong to the modules, so the user
  ## has no cacheId to hand and no call of their own to put dryRun on. They ask
  ## about the run: which entries did it write that it might have reused?
  cachePath <- withr::local_tempdir()
  f <- function(x, settings) paste(x, length(settings))
  g <- function(y) y
  Cache(f, x = 1, settings = list(b = list(name = "12.4")), cachePath = cachePath, verbose = -2)
  Cache(g, y = "12.4", cachePath = cachePath, verbose = -2)
  Sys.sleep(1.1)
  runStart <- Sys.time()
  Sys.sleep(1.1)
  ## The "run": the same two calls, with one shared value changed in both.
  Cache(f, x = 1, settings = list(b = list(name = 12.4)), cachePath = cachePath, verbose = -2)
  Cache(g, y = 12.4, cachePath = cachePath, verbose = -2)

  out <- whyNoCacheHit(cachePath = cachePath, since = runStart, verbose = -2)
  expect_equal(length(unique(out$entry)), 2L)
  expect_setequal(unique(out$fn), c("f", "g"))
  expect_setequal(unique(out$element), c("settings.b.name", "y"))
  ## Candidates must exclude the run's own entries, or two misses explain each other.
  expect_false(any(out$candidate %in% out$entry))
  expect_output(print(out), "Entries written after")
})

test_that("a run that reused everything says so", {
  cachePath <- withr::local_tempdir()
  f <- function(x) x
  Cache(f, x = 1, cachePath = cachePath, verbose = -2)
  Sys.sleep(1.1)
  runStart <- Sys.time()
  Cache(f, x = 1, cachePath = cachePath, verbose = -2)   # a hit: writes nothing new
  out <- whyNoCacheHit(cachePath = cachePath, since = runStart, verbose = -2)
  expect_equal(nrow(out), 0L)
})

test_that("cacheId and since are alternatives, not a combination", {
  cachePath <- withr::local_tempdir()
  f <- function(x) x
  Cache(f, x = 1, cachePath = cachePath, verbose = -2)
  ids <- unique(showCache(cachePath, verbose = -2)$cacheId)
  expect_error(whyNoCacheHit(cacheId = ids[[1]], since = Sys.time(), cachePath = cachePath, verbose = -2),
               "not both")
})

test_that("stopOnCacheMiss stops at the first accidental miss and names what changed", {
  ## The pipeline case: the user cannot reach the Cache() calls, so they set an
  ## option before the run and are told at the moment it matters.
  cachePath <- withr::local_tempdir()
  f <- function(x, settings) paste(x, length(settings))
  Cache(f, x = 1, settings = list(b = list(name = "12.4")), cachePath = cachePath, verbose = -2)

  withr::local_options(reproducible.stopOnCacheMiss = TRUE)
  expect_error(
    Cache(f, x = 1, settings = list(b = list(name = 12.4)), cachePath = cachePath, verbose = -2),
    "settings.b.name")
})

test_that("a miss with nothing to compare against is new work, and does not stop the run", {
  cachePath <- withr::local_tempdir()
  f <- function(x) x
  g <- function(y) y
  Cache(f, x = 1, cachePath = cachePath, verbose = -2)

  withr::local_options(reproducible.stopOnCacheMiss = TRUE)
  ## Cache() attaches its own attributes to the result; the value is what matters.
  expect_equal(as.numeric(Cache(g, y = 99, cachePath = cachePath, verbose = -2)), 99)
})

test_that("a numeric setting fires only when the miss looks like a slip", {
  cachePath <- withr::local_tempdir()
  f <- function(a, b, c) paste(a, b, c)
  Cache(f, a = 1, b = 1, c = 1, cachePath = cachePath, verbose = -2)

  ## Three elements differ; with a threshold of 1 that is a different job, not a slip.
  withr::local_options(reproducible.stopOnCacheMiss = 1)
  expect_equal(as.character(Cache(f, a = 2, b = 2, c = 2, cachePath = cachePath, verbose = -2)), "2 2 2")

  ## One element differs: that is the slip the switch is for.
  expect_error(Cache(f, a = 2, b = 2, c = 3, cachePath = cachePath, verbose = -2), "\\bc\\b")
})
