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
