## Unit tests for helpers in R/download.R that need no network.
##
## download.R is 53.9% covered and holds the largest block of uncovered lines in
## the package. Most of that is genuinely network-bound (dlGoogle, assessGoogle,
## download_resumable_httr2), but a handful of helpers are ordinary functions --
## duration formatting, HTML form parsing, interstitial detection, checksum-file
## editing -- that were untested only because they live in a file whose main
## paths need the internet.

test_that(".formatDuration renders h/m/s and rejects bad input", {
  testInit()

  expect_identical(.formatDuration(0), "0s")
  expect_identical(.formatDuration(45), "45s")
  ## Minutes and seconds are zero-padded so successive readings line up.
  expect_identical(.formatDuration(61), "1m01s")
  expect_identical(.formatDuration(599), "9m59s")
  expect_identical(.formatDuration(3661), "1h01m01s")
  expect_identical(.formatDuration(36000), "10h00m00s")
  ## Fractional seconds round rather than truncate.
  expect_identical(.formatDuration(59.6), "1m00s")

  ## Anything not a single finite non-negative number is "--" rather than an error,
  ## because this only ever decorates a progress message.
  expect_identical(.formatDuration(-1), "--")
  expect_identical(.formatDuration(NA_real_), "--")
  expect_identical(.formatDuration(Inf), "--")
  expect_identical(.formatDuration(numeric(0)), "--")
  expect_identical(.formatDuration(c(1, 2)), "--")
})

test_that(".parseGoogleConfirm extracts hidden form inputs", {
  testInit()

  f <- file.path(tmpdir, "interstitial.html")
  writeLines(c(
    '<html><body><form action="https://drive.usercontent.google.com/download">',
    '<input type="hidden" name="id" value="1AbC-dEf">',
    '<input type="hidden" name="export" value="download">',
    '<input type="hidden" name="confirm" value="t">',
    '<input type="hidden" name="uuid" value="abcd-1234">',
    '<input type="submit" value="Download anyway">',
    '</form></body></html>'
  ), f)

  out <- .parseGoogleConfirm(f)
  expect_type(out, "list")
  ## These four are what the download has to resubmit.
  expect_identical(out$id, "1AbC-dEf")
  expect_identical(out$export, "download")
  expect_identical(out$confirm, "t")
  expect_identical(out$uuid, "abcd-1234")
  ## An input with a value but no name contributes nothing.
  expect_false("" %in% names(out))

  ## No inputs -> empty list, not an error.
  f2 <- file.path(tmpdir, "plain.html")
  writeLines("<html><body>nothing here</body></html>", f2)
  expect_length(.parseGoogleConfirm(f2), 0L)

  ## Unreadable file is caught and treated as "no inputs".
  expect_length(suppressWarnings(.parseGoogleConfirm(file.path(tmpdir, "missing.html"))), 0L)
})

test_that(".looksLikeGoogleInterstitial distinguishes the HTML gate from a real payload", {
  testInit()

  gate <- file.path(tmpdir, "gate.html")
  writeLines(paste('<!DOCTYPE html><html><body>',
                   '<form action="https://drive.usercontent.google.com/download">',
                   'Google Drive can\'t perform a virus scan.</form></body></html>'), gate)
  expect_true(.looksLikeGoogleInterstitial(gate))

  ## HTML, but not Drive's gate -- must not be mistaken for one.
  other <- file.path(tmpdir, "other.html")
  writeLines("<html><body>some unrelated page</body></html>", other)
  expect_false(.looksLikeGoogleInterstitial(other))

  ## A real (binary) payload. Embedded NUL bytes are why the function reads raw
  ## and matches useBytes -- a plain grepl() here can trip locale translation.
  bin <- file.path(tmpdir, "payload.zip")
  writeBin(as.raw(c(0x50, 0x4b, 0x03, 0x04, 0x00, 0x00, 0xff, 0xfe, 0x00, 0x01)), bin)
  expect_false(.looksLikeGoogleInterstitial(bin))

  ## Absent and empty files are both "not an interstitial".
  expect_false(.looksLikeGoogleInterstitial(file.path(tmpdir, "nope.html")))
  empty <- file.path(tmpdir, "empty.html")
  file.create(empty)
  expect_false(.looksLikeGoogleInterstitial(empty))
})

test_that("purgeChecksums removes only the named files", {
  skip_if_not_installed("data.table")
  testInit()

  cf <- file.path(tmpdir, "CHECKSUMS.txt")
  data.table::fwrite(data.table::data.table(
    file     = c("a.tif", "b.tif", "c.tif"),
    checksum = c("aaa", "bbb", "ccc")
  ), cf)

  purgeChecksums(cf, "b.tif")

  out <- data.table::fread(cf)
  expect_identical(sort(out$file), c("a.tif", "c.tif"))
  expect_false("bbb" %in% out$checksum)

  ## Purging something absent leaves the file untouched rather than erroring.
  purgeChecksums(cf, "not-there.tif")
  expect_identical(nrow(data.table::fread(cf)), 2L)
})

test_that(".gdriveHasToken reports token state without erroring", {
  testInit()

  res <- .gdriveHasToken()
  expect_type(res, "logical")
  expect_length(res, 1L)
  expect_false(is.na(res))

  ## After an explicit deauth it must be FALSE -- this is the predicate the
  ## download path uses to decide between authenticated and anonymous access.
  if (requireNamespace("googledrive", quietly = TRUE)) {
    withr::defer(try(googledrive::drive_deauth(), silent = TRUE))
    try(googledrive::drive_deauth(), silent = TRUE)
    expect_false(.gdriveHasToken())
  }
})

test_that(".isRstudioServer is FALSE outside RStudio Server", {
  testInit()
  skip_if("tools:rstudio" %in% search(), "running inside RStudio")

  ## Plain R session (which is what CI and R CMD check are): no rstudio on the
  ## search path, so the answer is FALSE without consulting the API.
  expect_false(.isRstudioServer())
})

## ---------------------------------------------------------------------------
## .dirListingUrls() -- parsing an HTML directory index
##
## `prepInputs(url = <a directory>)` has to work out which files a remote
## directory holds. Servers render an index in whatever markup they like, so
## these fixtures are the real shapes seen in the wild, trimmed: Apache
## `mod_autoindex` (a table, with sort links and a parent link), nginx
## `autoindex` (markup starting at column 1), and a CDN that emits
## root-absolute hrefs. All are offline: the parser is a pure function.
## ---------------------------------------------------------------------------

test_that(".dirListingUrls reads an Apache mod_autoindex table", {
  html <- c(
    '<html><head><title>Index of /rasterDir</title></head><body>',
    '<h1>Index of /rasterDir</h1><table>',
    '<tr><th><a href="?C=N;O=D">Name</a></th><th><a href="?C=S;O=A">Size</a></th>',
    '<th><a href="?C=D;O=A">Description</a></th></tr>',
    '<tr><td><img alt="[PARENTDIR]"></td><td><a href="/">Parent Directory</a></td></tr>',
    '<tr><td><img alt="[   ]"></td><td><a href="elev.tif">elev.tif</a></td><td>12K</td></tr>',
    '<tr><td><img alt="[   ]"></td><td><a href="elev.tif.aux.xml">elev.tif.aux.xml</a></td><td>1K</td></tr>',
    '<tr><td><img alt="[DIR]"></td><td><a href="sub/">sub/</a></td></tr>',
    '</table></body></html>')
  out <- .dirListingUrls(html, "https://example.org/rasterDir/")

  ## the two files, and nothing else: not the parent link, not the `?C=` sort
  ## links (which the old regex returned as a file called "?C=D;O=A"), not the
  ## subdirectory
  expect_identical(names(out), c("elev.tif", "elev.tif.aux.xml"))
  expect_identical(unname(out[["elev.tif"]]), "https://example.org/rasterDir/elev.tif")
})

test_that(".dirListingUrls reads an nginx autoindex", {
  ## nginx puts the anchor at the start of the line. The previous parser
  ## required at least one character before `<a`, so it returned nothing at all
  ## for every nginx server.
  html <- c("<html><head><title>Index of /download/</title></head><body>",
            "<h1>Index of /download/</h1><hr><pre><a href=\"../\">../</a>",
            "<a href=\"nginx-1.0.0.tar.gz\">nginx-1.0.0.tar.gz</a>   01-Jan-2026 00:00   1000000",
            "<a href=\"nginx-1.0.1.tar.gz\">nginx-1.0.1.tar.gz</a>   02-Jan-2026 00:00   1000001",
            "</pre><hr></body></html>")
  out <- .dirListingUrls(html, "http://nginx.example/download/")

  expect_identical(names(out), c("nginx-1.0.0.tar.gz", "nginx-1.0.1.tar.gz"))
  expect_identical(unname(out[[1]]), "http://nginx.example/download/nginx-1.0.0.tar.gz")
})

test_that(".dirListingUrls resolves root-absolute and protocol-relative hrefs", {
  ## A CDN (jsDelivr is the case that matters here) links each file by an
  ## absolute path, not a bare name, so the urls cannot be built by pasting the
  ## name onto the directory.
  html <- c('<a href="https://www.example.net/about">About</a>',
            '<a href="/gh/o/r@abc/ex/dir/">../</a>',
            '<a href="/gh/o/r@abc/ex/dir/a.tif">a.tif</a>',
            '<a href="//cdn.example.net/gh/o/r@abc/ex/dir/b.tif">b.tif</a>',
            '<a href="/gh/o/r@abc/ex/other/c.tif">c.tif</a>')
  out <- .dirListingUrls(html, "https://cdn.example.net/gh/o/r@abc/ex/dir/")

  ## a.tif by origin + path; b.tif by scheme + //host; the off-site link and
  ## `c.tif` (a different directory) are both dropped, as is the self link
  expect_identical(names(out), c("a.tif", "b.tif"))
  expect_identical(unname(out[["a.tif"]]), "https://cdn.example.net/gh/o/r@abc/ex/dir/a.tif")
  expect_identical(unname(out[["b.tif"]]), "https://cdn.example.net/gh/o/r@abc/ex/dir/b.tif")
})

test_that(".dirListingUrls tolerates quoting, case, spacing and a missing slash", {
  html <- c("<A HREF = 'one.tif'>one.tif</A>",
            '<a  href="two.tif">two.tif</a>',
            '<a href="#top">top</a>',
            '<a href="">empty</a>',
            '<a href="one.tif">one.tif again</a>')
  ## no trailing slash on the directory: it should still be treated as one
  out <- .dirListingUrls(html, "https://example.org/d")

  expect_identical(names(out), c("one.tif", "two.tif")) # deduplicated, anchors dropped
  expect_identical(unname(out[["two.tif"]]), "https://example.org/d/two.tif")
})

test_that(".dirListingUrls returns an empty result for a listing of only directories", {
  html <- c('<a href="../">Parent Directory</a>',
            '<a href="alpha/">alpha/</a>',
            '<a href="beta/">beta/</a>')
  out <- .dirListingUrls(html, "https://example.org/pub/")

  expect_length(out, 0L)
})
