test_that("googledriveIDtoDownloadURL builds the v3 media URL", {
  expect_identical(
    reproducible:::googledriveIDtoDownloadURL("abc123"),
    "https://www.googleapis.com/drive/v3/files/abc123?alt=media"
  )
})

test_that("googledriveIDtoHumanURL builds the human-readable URL", {
  expect_identical(
    reproducible:::googledriveIDtoHumanURL("abc123"),
    "https://drive.google.com/file/d/abc123"
  )
})

test_that("messageAboutFilesize emits a single-file message at verbose=1", {
  expect_message(
    reproducible:::messageAboutFilesize(1024, verbose = 1),
    "File on Google Drive is"
  )
})

test_that("messageAboutFilesize sums multi-file sizes and pluralizes", {
  expect_message(
    reproducible:::messageAboutFilesize(c(512, 512), verbose = 1),
    "Files on Google Drive are"
  )
})

test_that("messageAboutFilesize honors msgMiddle override", {
  expect_message(
    reproducible:::messageAboutFilesize(2048, verbose = 1, msgMiddle = " at URL "),
    "File at URL is"
  )
})

test_that(".humanBytes formats byte counts and guards non-finite input", {
  expect_match(reproducible:::.humanBytes(1), "1 bytes|^1 ")
  expect_match(reproducible:::.humanBytes(1536), "1.5 Kb")
  expect_match(reproducible:::.humanBytes(5 * 1024^2), "5 Mb")
  expect_identical(reproducible:::.humanBytes(NA_real_), "?")
  expect_identical(reproducible:::.humanBytes(Inf), "?")
  expect_identical(reproducible:::.humanBytes(c(1, 2)), "?") # not length-1
})

test_that(".dlRejectionProbeFile reads only the ASCII head; binary bodies are clean", {
  # an HTML 'Request Rejected' page (some servers return it with HTTP 200)
  rej <- tempfile(fileext = ".html")
  writeLines("<html><body>Request Rejected: bad bot</body></html>", rej)
  expect_true(grepl("Request Rejected", reproducible:::.dlRejectionProbeFile(rej)))

  # a binary body must NOT match and must NOT warn about invalid multibyte chars
  bin <- tempfile(fileext = ".bin")
  writeBin(as.raw(c(0L, 255L, 200L, 1L, 130L, 0L, 127L)), bin)
  probe <- expect_silent(reproducible:::.dlRejectionProbeFile(bin))
  expect_false(grepl("Request Rejected", probe))

  expect_identical(reproducible:::.dlRejectionProbeFile(tempfile()), "") # missing file
})
