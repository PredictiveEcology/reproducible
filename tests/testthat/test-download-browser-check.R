# A server behind a Cloudflare browser check (header `cf-mitigated: challenge`,
# HTTP 403) refuses every scripted request. prepInputs() should say so, once,
# and point at a manual download, rather than retry and blame permissions.
test_that("Cloudflare browser check gives a clear, non-retried failure", {
  skip_if_not_installed("httr2")
  withr::local_envvar(NOT_CRAN = "true") # error, not the on-CRAN skip()
  withr::local_options(reproducible.interactiveOnDownloadFail = FALSE)
  td <- withr::local_tempdir()

  nCalls <- 0
  httr2::local_mocked_responses(function(req) {
    nCalls <<- nCalls + 1
    httr2::response(403, headers = list(`cf-mitigated` = "challenge",
                                        `content-type` = "text/html"),
                    body = charToRaw("<html>Just a moment...</html>"))
  })

  err <- expect_error(suppressMessages(
    prepInputs(url = "https://example.com/files/thing.zip",
               destinationPath = td, verbose = 0)
  ))
  msg <- conditionMessage(err)
  expect_match(msg, .txtBrowserChallenge, fixed = TRUE)
  expect_false(grepl("permissions issue", msg))
  expect_match(msg, "download it manually")
  expect_identical(nCalls, 1)
  expect_false(file.exists(file.path(td, "thing.zip")))
})
