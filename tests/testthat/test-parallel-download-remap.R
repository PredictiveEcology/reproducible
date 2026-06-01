# Tests for parallel ranged downloads (Feature A) and the URL remap hook
# (Feature B). The gating/policy and remap logic are tested fully offline via
# mocking; the actual byte-level reassembly is verified against a real
# Range-capable host (Arbutus) under skip_on_cran() + skip_if_offline().

# ---------------------------------------------------------------------------
# Feature B: URL remap hook (.applyUrlRemap) and makeUrlRemap() -- all offline
# ---------------------------------------------------------------------------

test_that("makeUrlRemap matches on basename and returns NULL otherwise", {
  manifest <- data.frame(
    filename = c("a.tif", "b.tif"),
    url = c("https://mirror/x/a.tif", "https://mirror/y/b.tif"),
    stringsAsFactors = FALSE
  )
  remap <- reproducible::makeUrlRemap(manifest)

  expect_identical(remap("gd://id1", "a.tif"), "https://mirror/x/a.tif")
  # matching is by basename, so a full path resolves the same
  expect_identical(remap("gd://id2", "/some/dir/b.tif"), "https://mirror/y/b.tif")
  # no match -> NULL (caller keeps original)
  expect_null(remap("gd://id3", "not_in_manifest.tif"))
})

test_that("makeUrlRemap is robust to a length != 1 filename (e.g. a Drive directory)", {
  manifest <- data.frame(filename = "a.tif", url = "https://mirror/a.tif",
                         stringsAsFactors = FALSE)
  remap <- reproducible::makeUrlRemap(manifest)
  # A directory resolves to multiple filenames -> no single mirror; must not error
  expect_silent(out <- remap("gd://dir", c("a.tif", "b.tif")))
  expect_null(out)
  expect_null(remap("gd://dir", character(0)))
})

test_that(".remapUrlEarly leaves a Drive directory (multi-file) URL unchanged", {
  skip_if_not_installed("googledrive")
  called <- FALSE
  withr::local_options(
    reproducible.urlRemap = function(url, filename) {
      called <<- TRUE
      NULL
    }
  )
  # Mock a directory resolving to several files
  testthat::local_mocked_bindings(assessGoogle = function(url, ...) c("a.tif", "b.tif"))
  out <- reproducible:::.remapUrlEarly(
    "https://drive.google.com/drive/folders/ABCDEF", verbose = 0)
  expect_identical(out, "https://drive.google.com/drive/folders/ABCDEF")
  expect_false(called) # directory short-circuits before the hook is consulted
})

test_that("makeUrlRemap validates its manifest argument", {
  expect_error(reproducible::makeUrlRemap(list(a = 1)), "data.frame")
  expect_error(reproducible::makeUrlRemap(data.frame(foo = 1, bar = 2)),
               "filename.*url|columns")
})

test_that(".applyUrlRemap returns the original url when the option is unset", {
  withr::local_options(reproducible.urlRemap = NULL)
  expect_identical(
    reproducible:::.applyUrlRemap("http://x/y.tif", "y.tif", verbose = 0),
    "http://x/y.tif"
  )
})

test_that(".applyUrlRemap uses a new url returned by the hook", {
  withr::local_options(
    reproducible.urlRemap = function(url, filename) "http://mirror/y.tif"
  )
  expect_identical(
    reproducible:::.applyUrlRemap("http://x/y.tif", "y.tif", verbose = 0),
    "http://mirror/y.tif"
  )
})

test_that(".applyUrlRemap keeps the original url when the hook returns NULL", {
  withr::local_options(reproducible.urlRemap = function(url, filename) NULL)
  expect_identical(
    reproducible:::.applyUrlRemap("http://x/y.tif", "y.tif", verbose = 0),
    "http://x/y.tif"
  )
})

test_that(".applyUrlRemap survives a broken hook: original url + warning", {
  withr::local_options(
    reproducible.urlRemap = function(url, filename) stop("boom")
  )
  expect_warning(
    res <- reproducible:::.applyUrlRemap("http://x/y.tif", "y.tif", verbose = 0),
    "urlRemap function failed"
  )
  expect_identical(res, "http://x/y.tif")
})

test_that(".applyUrlRemap ignores invalid (non-scalar-character) hook returns", {
  withr::local_options(reproducible.urlRemap = function(url, filename) c("a", "b"))
  expect_identical(
    reproducible:::.applyUrlRemap("http://x/y.tif", "y.tif", verbose = 0),
    "http://x/y.tif"
  )
})

# ---------------------------------------------------------------------------
# Feature B: early remap (.remapUrlEarly) feeding the COG fast-path -- offline
# ---------------------------------------------------------------------------

test_that(".remapUrlEarly is a no-op when urlRemap is unset or url is NULL", {
  withr::local_options(reproducible.urlRemap = NULL)
  expect_identical(reproducible:::.remapUrlEarly("https://x/y.tif", verbose = 0), "https://x/y.tif")
  expect_null(reproducible:::.remapUrlEarly(NULL, verbose = 0))
})

test_that(".remapUrlEarly remaps a plain HTTPS url by basename", {
  withr::local_options(
    reproducible.urlRemap = function(url, filename) {
      if (identical(filename, "y.tif")) "https://mirror/y.tif" else NULL
    }
  )
  expect_identical(reproducible:::.remapUrlEarly("https://orig/y.tif", verbose = 0),
                   "https://mirror/y.tif")
  # no manifest match -> original url unchanged
  expect_identical(reproducible:::.remapUrlEarly("https://orig/other.tif", verbose = 0),
                   "https://orig/other.tif")
})

test_that(".remapUrlEarly resolves a Drive URL's filename then remaps to a mirror", {
  skip_if_not_installed("googledrive")
  withr::local_options(
    reproducible.urlRemap = function(url, filename) {
      if (identical(filename, "biomass.tif")) "https://mirror/biomass.tif" else NULL
    }
  )
  # Pretend drive_get resolved the Drive ID to this filename (no network).
  testthat::local_mocked_bindings(assessGoogle = function(url, ...) "biomass.tif")
  out <- reproducible:::.remapUrlEarly(
    "https://drive.google.com/file/d/ABCDEF/view", verbose = 0)
  expect_identical(out, "https://mirror/biomass.tif")
})

# ---------------------------------------------------------------------------
# Feature A: opt-in gating policy (.useParallelDownload) -- all offline
# ---------------------------------------------------------------------------

# A remap function that observes nothing -- just marks the opt-in as "set" so
# the parallel gate is reachable. (Returning NULL means "don't actually redirect".)
optedIn <- function(url, filename) NULL

test_that("parallel path is GATED OFF when urlRemap is unset, even at streams = 48", {
  withr::local_options(
    reproducible.urlRemap = NULL, # the opt-in is NOT set
    reproducible.parallel.streams = 48L,
    reproducible.parallel.threshold = 1L
  )
  probed <- FALSE
  testthat::local_mocked_bindings(
    .probeRange = function(...) {
      probed <<- TRUE
      list(size = 1e9, acceptRanges = TRUE)
    }
  )
  out <- reproducible:::.useParallelDownload("http://x/big.tif", verbose = 0)
  expect_false(out$use)
  expect_false(probed) # hard gate short-circuits before any HEAD request
})

test_that("parallel path is disabled when streams = 1L and does not probe", {
  withr::local_options(
    reproducible.urlRemap = optedIn,
    reproducible.parallel.streams = 1L
  )
  probed <- FALSE
  testthat::local_mocked_bindings(
    .probeRange = function(...) {
      probed <<- TRUE
      list(size = 1e9, acceptRanges = TRUE)
    }
  )
  out <- reproducible:::.useParallelDownload("http://x/big.tif", verbose = 0)
  expect_false(out$use)
  expect_false(probed) # streams gate short-circuits before any HEAD request
})

test_that("parallel path engages when opted in, ranges supported, above threshold", {
  skip_if_not_installed("curl")
  skip_if_not_installed("httr2")
  withr::local_options(
    reproducible.urlRemap = optedIn,
    reproducible.parallel.streams = 16L,
    reproducible.parallel.threshold = 100 * 1024^2
  )
  testthat::local_mocked_bindings(
    .probeRange = function(...) list(size = 500 * 1024^2, acceptRanges = TRUE)
  )
  out <- reproducible:::.useParallelDownload("http://x/big.tif", verbose = 0)
  expect_true(out$use)
  expect_identical(out$info$size, 500 * 1024^2)
})

test_that("parallel path does NOT engage below the size threshold", {
  skip_if_not_installed("curl")
  skip_if_not_installed("httr2")
  withr::local_options(
    reproducible.urlRemap = optedIn,
    reproducible.parallel.streams = 16L,
    reproducible.parallel.threshold = 100 * 1024^2
  )
  testthat::local_mocked_bindings(
    .probeRange = function(...) list(size = 1 * 1024^2, acceptRanges = TRUE) # 1 MiB < 100 MiB
  )
  expect_false(reproducible:::.useParallelDownload("http://x/small.tif", verbose = 0)$use)
})

test_that("parallel path does NOT engage when the server lacks Accept-Ranges", {
  skip_if_not_installed("curl")
  skip_if_not_installed("httr2")
  withr::local_options(
    reproducible.urlRemap = optedIn,
    reproducible.parallel.streams = 16L,
    reproducible.parallel.threshold = 100 * 1024^2
  )
  testthat::local_mocked_bindings(
    .probeRange = function(...) list(size = 500 * 1024^2, acceptRanges = FALSE)
  )
  expect_false(reproducible:::.useParallelDownload("http://x/noranges.tif", verbose = 0)$use)
})

# ---------------------------------------------------------------------------
# Feature A: dlGeneric dispatch -- offline via mocking of the mechanism
# ---------------------------------------------------------------------------

test_that("dlGeneric takes the parallel path and returns its file when opted in", {
  skip_if_not_installed("curl")
  skip_if_not_installed("httr2")
  withr::local_options(
    reproducible.parallel.streams = 8L,
    reproducible.parallel.threshold = 1L,
    reproducible.urlRemap = optedIn # opt-in set (no actual redirect)
  )
  dp <- withr::local_tempdir()
  called <- FALSE
  testthat::local_mocked_bindings(
    .probeRange = function(...) list(size = 1e6, acceptRanges = TRUE),
    .parallelRangedDownload = function(url, destFile, size, n, verbose) {
      called <<- TRUE
      writeBin(as.raw(rep(0L, 10)), destFile) # pretend success
      TRUE
    }
  )
  res <- reproducible:::dlGeneric("http://x/big.tif", destinationPath = dp, verbose = 0)
  expect_true(called)
  expect_true(file.exists(res$destFile))
  expect_identical(basename(res$destFile), "big.tif")
})

test_that("dlGeneric falls back to single-stream when the parallel attempt fails", {
  skip_if_not_installed("curl")
  skip_if_not_installed("httr2")
  withr::local_options(
    reproducible.parallel.streams = 8L,
    reproducible.parallel.threshold = 1L,
    reproducible.urlRemap = optedIn # opt-in set (no actual redirect)
  )
  dp <- withr::local_tempdir()
  called <- FALSE
  testthat::local_mocked_bindings(
    .probeRange = function(...) list(size = 1e6, acceptRanges = TRUE),
    .parallelRangedDownload = function(url, destFile, size, n, verbose) {
      called <<- TRUE
      FALSE # simulate a partial failure -> caller must fall back
    }
  )
  # The fallback single-stream then targets a non-routable address and errors;
  # the point is that control proceeded PAST the parallel block (no early return).
  expect_error(
    suppressWarnings(
      reproducible:::dlGeneric("http://127.0.0.1:1/big.tif", destinationPath = dp, verbose = 0)
    )
  )
  expect_true(called)
})

test_that("dlGeneric applies the remap hook before downloading (generic url)", {
  seen <- NULL
  withr::local_options(
    reproducible.parallel.streams = 1L, # keep single-stream
    reproducible.urlRemap = function(url, filename) {
      seen <<- list(url = url, filename = filename)
      NULL # don't actually redirect; just observe
    }
  )
  dp <- withr::local_tempdir()
  # single-stream will fail on the non-routable host; we only assert the hook ran
  try(suppressWarnings(
    reproducible:::dlGeneric("http://127.0.0.1:1/data.tif", destinationPath = dp, verbose = 0)
  ), silent = TRUE)
  expect_identical(seen$filename, "data.tif")
  expect_identical(seen$url, "http://127.0.0.1:1/data.tif")
})

# ---------------------------------------------------------------------------
# Feature A: real reassembly correctness against a Range-capable host
# ---------------------------------------------------------------------------

test_that("parallel ranged download is byte-identical to single-stream (network)", {
  skip_on_cran()
  skip_if_offline()
  skip_if_not_installed("curl")
  skip_if_not_installed("httr2")

  url <- paste0(
    "https://object-arbutus.cloud.computecanada.ca/predictiveecology/",
    "SCANFI_v2/1990/SCANFI_spsCC_BETU_ALL_1990_v2_20260119.tif.ovr"
  )

  info <- reproducible:::.probeRange(url, verbose = 0)
  skip_if_not(isTRUE(info$acceptRanges) && is.finite(info$size) && info$size > 0,
              "host did not advertise Range support")

  baseFile <- withr::local_tempfile()
  httr2::req_perform(httr2::request(url), path = baseFile)

  parFile <- withr::local_tempfile()
  ok <- reproducible:::.parallelRangedDownload(url, parFile, info$size, n = 8L, verbose = 0)

  expect_true(isTRUE(ok))
  expect_identical(file.size(parFile), file.size(baseFile))
  expect_identical(unname(tools::md5sum(parFile)), unname(tools::md5sum(baseFile)))
})
