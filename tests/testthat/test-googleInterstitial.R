## Coverage for the Google Drive "confirm download" interstitial handling in
## R/download.R: .looksLikeGoogleInterstitial() and .parseGoogleConfirm().
##
## When a public Drive file is too big for a virus scan, Drive serves an HTML
## confirmation page INSTEAD of the file, with HTTP 200. Nothing errors -- the
## download "succeeds" and writes a small HTML file where a dataset was
## expected. These two functions are what notice that and re-issue the request
## with the form's parameters, so their failure mode is silent corruption of a
## downloaded input.
##
## Both are pure functions over a local file, so this needs no network and no
## Drive credentials, and runs on CRAN.

## A stand-in for Drive's interstitial page.
interstitialHtml <- paste0(
  '<!DOCTYPE html><html><head><title>Google Drive - Virus scan warning</title>',
  '</head><body><form id="download-form" action="https://drive.usercontent.google.com/download">',
  '<input type="hidden" name="id" value="ABC123">',
  '<input type="hidden" name="export" value="download">',
  '<input type="hidden" name="confirm" value="t">',
  '<input type="hidden" name="uuid" value="dead-beef">',
  '</form></body></html>')

test_that(".looksLikeGoogleInterstitial spots an HTML page served instead of a file", {
  testInit()

  f <- file.path(tmpdir, "page.html")
  writeLines(interstitialHtml, f)
  expect_true(.looksLikeGoogleInterstitial(f))

  ## Needs BOTH signals: html-ish AND a Drive marker. Ordinary HTML is not an
  ## interstitial -- a caller may legitimately be downloading an HTML file.
  plain <- file.path(tmpdir, "plain.html")
  writeLines("<html><body>an ordinary page</body></html>", plain)
  expect_false(.looksLikeGoogleInterstitial(plain))
})

test_that(".looksLikeGoogleInterstitial is FALSE for real payloads and missing files", {
  testInit()

  ## A real (binary) download must never be mistaken for the interstitial.
  z <- file.path(tmpdir, "real.zip")
  owd <- setwd(tmpdir); writeLines("payload", "p.txt")
  suppressWarnings(utils::zip(z, "p.txt", flags = "-q")); setwd(owd)
  expect_false(.looksLikeGoogleInterstitial(z))

  ## Absent and empty files are "not an interstitial", not an error: this is
  ## called on whatever the download left behind, including nothing.
  expect_false(.looksLikeGoogleInterstitial(file.path(tmpdir, "nope.bin")))
  empty <- file.path(tmpdir, "empty.bin"); file.create(empty)
  expect_false(.looksLikeGoogleInterstitial(empty))
})

test_that(".looksLikeGoogleInterstitial handles non-UTF-8 bytes without erroring", {
  testInit()

  ## The check reads raw bytes precisely so a binary payload cannot trip
  ## locale translation inside grepl(). Bytes that are invalid UTF-8 are the
  ## case that used to be able to error rather than return FALSE.
  f <- file.path(tmpdir, "binary.bin")
  writeBin(as.raw(c(0xff, 0xfe, 0x00, 0x01, 0x80, 0x90, 0xa0)), f)
  expect_false(.looksLikeGoogleInterstitial(f))
})

test_that(".parseGoogleConfirm extracts the form parameters", {
  testInit()

  f <- file.path(tmpdir, "page.html")
  writeLines(interstitialHtml, f)

  params <- .parseGoogleConfirm(f)

  ## These are exactly what gets pasted into the retry URL, so both names and
  ## values matter.
  expect_type(params, "list")
  expect_identical(params$id, "ABC123")
  expect_identical(params$confirm, "t")
  expect_identical(params$uuid, "dead-beef")
  expect_true(all(c("id", "export", "confirm", "uuid") %in% names(params)))
})

test_that(".parseGoogleConfirm returns empty for pages with no usable inputs", {
  testInit()

  ## No <input> at all -> nothing to retry with; the caller checks
  ## length(params) before building a second URL.
  none <- file.path(tmpdir, "none.html")
  writeLines("<html><body>no form here</body></html>", none)
  expect_length(.parseGoogleConfirm(none), 0)

  ## Inputs missing a name or a value are skipped rather than producing
  ## malformed query parameters.
  partial <- file.path(tmpdir, "partial.html")
  writeLines(paste0('<html><input type="hidden" value="novalue">',
                    '<input type="hidden" name="">',
                    '<input type="hidden" name="ok" value="yes"></html>'), partial)
  p <- .parseGoogleConfirm(partial)
  expect_identical(names(p), "ok")
  expect_identical(p$ok, "yes")
})
