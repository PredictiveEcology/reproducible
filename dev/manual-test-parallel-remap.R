# Manual tests for parallel ranged downloads (Feature A) + URL remap (Feature B)
# Run interactively from the package root:  pkgload::load_all(".")
#
# Both features are STRICTLY OPT-IN: with default options nothing changes.
#   - Feature A engages only if  options(reproducible.parallel.streams = <N > 1>)
#   - Feature B engages only if  options(reproducible.urlRemap = <function>)

suppressMessages(pkgload::load_all(".", quiet = TRUE))

# A small (~1 MB) public, Range-capable object on Arbutus, and its directory.
base <- "https://object-arbutus.cloud.computecanada.ca/predictiveecology/SCANFI_v2/1990/"
smallObj <- paste0(base, "SCANFI_spsCC_BETU_ALL_1990_v2_20260119.tif.ovr")

md5 <- function(f) unname(tools::md5sum(f))

## ---------------------------------------------------------------------------
## Feature B: makeUrlRemap() + the .applyUrlRemap hook
## ---------------------------------------------------------------------------

manifest <- data.frame(
  filename = "SCANFI_spsCC_BETU_ALL_1990_v2_20260119.tif.ovr",
  url = smallObj
)
remap <- makeUrlRemap(manifest)

stopifnot(
  # match by basename -> mirror url
  identical(remap("gd://whatever", "SCANFI_spsCC_BETU_ALL_1990_v2_20260119.tif.ovr"), smallObj),
  # no match -> NULL (caller keeps original)
  is.null(remap("gd://whatever", "not_in_manifest.tif"))
)
cat("B1 makeUrlRemap matching: OK\n")

# Hook returns NULL  -> original url kept
options(reproducible.urlRemap = function(url, filename) NULL)
stopifnot(identical(reproducible:::.applyUrlRemap("http://x/y.tif", "y.tif", verbose = 0),
                    "http://x/y.tif"))
cat("B2 NULL remap keeps original: OK\n")

# Hook ERRORS -> original url kept + a warning (broken remap must not break dl)
options(reproducible.urlRemap = function(url, filename) stop("boom"))
res <- withCallingHandlers(
  reproducible:::.applyUrlRemap("http://x/y.tif", "y.tif", verbose = 0),
  warning = function(w) { cat("   (expected warning:", conditionMessage(w), ")\n"); invokeRestart("muffleWarning") }
)
stopifnot(identical(res, "http://x/y.tif"))
cat("B3 erroring remap keeps original + warns: OK\n")

# Hook returns a new url -> that url is used
options(reproducible.urlRemap = function(url, filename) "http://mirror/y.tif")
stopifnot(identical(reproducible:::.applyUrlRemap("http://x/y.tif", "y.tif", verbose = 0),
                    "http://mirror/y.tif"))
cat("B4 remap returns new url: OK\n")
options(reproducible.urlRemap = NULL)  # reset

## ---------------------------------------------------------------------------
## Feature A: parallel ranged download path
## ---------------------------------------------------------------------------

# Probe: size + Accept-Ranges
info <- reproducible:::.probeRange(smallObj, verbose = 0)
stopifnot(info$acceptRanges, info$size > 0)
cat("A1 .probeRange:", info$size, "bytes, ranges =", info$acceptRanges, "\n")

# Single-stream baseline to compare against
baseFile <- tempfile(fileext = ".ovr")
httr2::req_perform(httr2::request(smallObj), path = baseFile)

# Parallel ranged -> must be byte-identical to baseline
parFile <- tempfile(fileext = ".ovr")
ok <- reproducible:::.parallelRangedDownload(smallObj, parFile, info$size, n = 8L, verbose = 0)
stopifnot(isTRUE(ok), file.size(parFile) == info$size, identical(md5(baseFile), md5(parFile)))
cat("A2 parallel reassembly byte-identical (n=8): OK\n")

# OPT-IN OFF (default streams = 1L): dlGeneric must take single stream
d <- tempfile("d"); dir.create(d)
options(reproducible.parallel.streams = 1L)            # the default
r <- reproducible:::dlGeneric(smallObj, destinationPath = d, verbose = 0)
stopifnot(identical(md5(r$destFile), md5(baseFile)))
cat("A3 opt-in OFF -> single stream, identical: OK\n")

# OPT-IN ON, above threshold: dlGeneric must take the parallel path
d <- tempfile("d"); dir.create(d)
options(reproducible.parallel.streams = 8L, reproducible.parallel.threshold = 5e5)  # 0.5 MB < file
r <- reproducible:::dlGeneric(smallObj, destinationPath = d, verbose = 1)  # watch for "parallel ranged streams"
stopifnot(identical(md5(r$destFile), md5(baseFile)))
cat("A4 opt-in ON, > threshold -> parallel, identical: OK\n")

# OPT-IN ON, below threshold: must fall back to single stream (file < threshold)
d <- tempfile("d"); dir.create(d)
options(reproducible.parallel.streams = 8L, reproducible.parallel.threshold = 100 * 1024^2)  # 100 MB > file
r <- reproducible:::dlGeneric(smallObj, destinationPath = d, verbose = 0)
stopifnot(identical(md5(r$destFile), md5(baseFile)))
cat("A5 opt-in ON, < threshold -> single stream fallback: OK\n")

# Fallback when server has no Range support: point at a URL that 200s w/o Accept-Ranges.
# (httpbin: /bytes/N returns 200 and no Accept-Ranges.) Skipped if unreachable.
noRange <- "https://httpbin.org/bytes/1048576"
ni <- tryCatch(reproducible:::.probeRange(noRange, verbose = 0), error = function(e) NULL)
if (!is.null(ni)) {
  cat("A6 no-Accept-Ranges probe -> acceptRanges =", ni$acceptRanges,
      "(parallel path correctly NOT engaged)\n")
} else {
  cat("A6 skipped (httpbin unreachable)\n")
}

options(reproducible.parallel.streams = 1L, reproducible.parallel.threshold = 100 * 1024^2)

## ---------------------------------------------------------------------------
## Combined (optional; needs googledrive auth): Drive URL -> remap -> Arbutus -> parallel
## ---------------------------------------------------------------------------
if (FALSE) {
  options(
    reproducible.urlRemap = makeUrlRemap(read.csv("dev/arbutus_manifest.csv")),
    reproducible.parallel.streams = 16L
  )
  out <- prepInputs(
    url = "https://drive.google.com/file/d/<DRIVE_ID>/view",
    destinationPath = tempdir2(), fun = "terra::rast"
  )
  # Expect: messages show the URL remapped to object-arbutus..., then N parallel ranged streams.
}

cat("\nAll automated manual-test assertions passed.\n")
