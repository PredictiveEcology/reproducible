test_that("omitArgs = TRUE drops every captured arg from the digest", {
  ## Two Cache() calls with different inputs and `omitArgs = TRUE` plus the
  ## same `.cacheExtra` should resolve to the same cache key, so the second
  ## call hits the cache populated by the first.
  skip_on_cran()

  tmpCache <- file.path(tempdir(), basename(tempfile("rcache_omitTRUE_")))
  dir.create(tmpCache, showWarnings = FALSE, recursive = TRUE)
  withr::local_options(reproducible.cachePath  = tmpCache,
                       reproducible.useMemoise = FALSE,
                       reproducible.verbose    = 0)

  ## Sentinel counter to detect re-evaluation
  e <- new.env()
  e$count <- 0L
  f <- function(x) { e$count <- e$count + 1L; x * 2 }

  out1 <- Cache(f, x = 1, cachePath = tmpCache,
                omitArgs    = TRUE,
                .cacheExtra = "vTag1",
                useCloud    = FALSE)
  out2 <- Cache(f, x = 999, cachePath = tmpCache,
                omitArgs    = TRUE,
                .cacheExtra = "vTag1",
                useCloud    = FALSE)

  expect_equal(e$count, 1L,
               info = "omitArgs = TRUE should make x irrelevant -> 2nd call is a cache hit")
  ## Compare values only; the .Cache attribute differs between miss/hit
  expect_equal(as.numeric(out1), as.numeric(out2),
               info = "Cache hit returns the value computed on the 1st call (x = 1)")

  ## Different .cacheExtra is the only thing that should drive a miss now
  out3 <- Cache(f, x = 1, cachePath = tmpCache,
                omitArgs    = TRUE,
                .cacheExtra = "vTag2",
                useCloud    = FALSE)
  expect_equal(e$count, 2L,
               info = "Changing .cacheExtra invalidates the entry under omitArgs = TRUE")
})

test_that("omitArgs = TRUE still busts the cache when FUN's body changes", {
  ## .FUN is the function VALUE (not just its name), so editing the body
  ## should produce a new digest even though omitArgs = TRUE drops all the
  ## captured args. This is the safety net that prevents stale results when
  ## a developer changes their function under a fixed .cacheExtra.
  skip_on_cran()

  tmpCache <- file.path(tempdir(), basename(tempfile("rcache_omitTRUE_body_")))
  dir.create(tmpCache, showWarnings = FALSE, recursive = TRUE)
  withr::local_options(reproducible.cachePath  = tmpCache,
                       reproducible.useMemoise = FALSE,
                       reproducible.verbose    = 0)

  f <- function(x) x * 2
  v1 <- as.numeric(Cache(f, x = 1, cachePath = tmpCache,
                         omitArgs = TRUE, .cacheExtra = "vTag",
                         useCloud = FALSE))

  ## Edit the function body; same .cacheExtra; expect a new value (cache miss)
  f <- function(x) x * 100
  v2 <- as.numeric(Cache(f, x = 1, cachePath = tmpCache,
                         omitArgs = TRUE, .cacheExtra = "vTag",
                         useCloud = FALSE))
  expect_false(isTRUE(all.equal(v1, v2)),
               info = "Editing FUN's body should bust the cache even under omitArgs = TRUE")
})

test_that("doDigestPrepare(omitArgs = TRUE) keeps only .FUN (and .cacheExtra if set)", {
  ## Direct unit test of the digest-prep helper. With omitArgs = TRUE we
  ## should retain only the special .FUN slot; .cacheExtra is appended after
  ## the omit pass.
  fakeCall <- structure(
    quote(f(a = 1, b = 2)),
    .Cache = list(
      args_w_defaults = list(a = 1, b = 2),
      method          = "f"
    )
  )

  d1 <- reproducible:::doDigestPrepare(fakeCall, omitArgs = TRUE,
                                       .cacheExtra = NULL)
  expect_named(d1, ".FUN")
  expect_equal(d1$.FUN, "f")

  d2 <- reproducible:::doDigestPrepare(fakeCall, omitArgs = TRUE,
                                       .cacheExtra = "v1")
  expect_setequal(names(d2), c(".FUN", ".cacheExtra"))
  expect_equal(d2$.cacheExtra, "v1")

  ## Back-compat: character vector still works
  d3 <- reproducible:::doDigestPrepare(fakeCall, omitArgs = "a",
                                       .cacheExtra = NULL)
  expect_named(d3, c("b", ".FUN"))
})
