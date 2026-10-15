## .gdriveRetry(): a googledrive call is run again after a brief failure, never after a refusal.

waits <- NULL
noWait <- function(attempt) waits <<- c(waits, 2L * attempt)

test_that(".gdriveRetry() returns the value of a call that fails twice with a timeout", {
  waits <<- NULL
  local_mocked_bindings(.gdriveRetryWait = noWait)
  n <- 0L
  flaky <- function() {
    n <<- n + 1L
    if (n <= 2L) stop("Timeout was reached: [www.googleapis.com] Operation timed out")
    "ok"
  }
  expect_identical(reproducible:::.gdriveRetry(flaky(), verbose = -1), "ok")
  expect_identical(n, 3L)
  expect_identical(waits, c(2L, 4L))
})

test_that(".gdriveRetry() retries 5xx, DNS and token-refresh failures, and gives up after `retries`", {
  local_mocked_bindings(.gdriveRetryWait = noWait)
  for (msg in c("Server error (503) Service Unavailable", "Could not resolve host: www.googleapis.com",
                "Failed to refresh token against oauth2.googleapis.com", "Connection reset by peer")) {
    n <- 0L
    expect_error(reproducible:::.gdriveRetry({ n <- n + 1L; stop(msg) }, retries = 3, verbose = -1), msg, fixed = TRUE)
    expect_identical(n, 4L, label = msg)
  }
})

test_that(".gdriveRetry() raises a 404 or a 403 at once, and any other error too", {
  local_mocked_bindings(.gdriveRetryWait = function(attempt) stop("must not wait"))
  for (msg in c("Client error: (404) Not Found", "Client error: (403) Forbidden", "File not found: abc",
                "object 'x' not found")) {
    n <- 0L
    expect_error(reproducible:::.gdriveRetry({ n <- n + 1L; stop(msg) }, verbose = -1), msg, fixed = TRUE)
    expect_identical(n, 1L, label = msg)
  }
})

test_that(".gdriveRetry(): retries = 0 turns retrying off; the default is the option", {
  local_mocked_bindings(.gdriveRetryWait = noWait)
  n <- 0L
  expect_error(reproducible:::.gdriveRetry({ n <- n + 1L; stop("operation timed out") }, retries = 0, verbose = -1))
  expect_identical(n, 1L)
  expect_identical(reproducible:::.gdriveRetries(), 6L)
  withr::local_options(reproducible.gdriveAuthRetries = 1L)
  n <- 0L
  expect_error(reproducible:::.gdriveRetry({ n <- n + 1L; stop("operation timed out") }, verbose = -1))
  expect_identical(n, 2L)
})

test_that("a Drive listing that times out twice is retried in driveLs()", {
  skip_if_not_installed("googledrive")
  local_mocked_bindings(.gdriveRetryWait = noWait)
  n <- 0L
  local_mocked_bindings(
    drive_ls = function(...) {
      n <<- n + 1L
      if (n <= 2L) stop("Timeout was reached")
      data.frame(name = "a", id = "a")
    },
    .package = "googledrive")
  folder <- structure(data.frame(name = "f", id = "f"), class = c("tbl_df", "tbl", "data.frame"))
  out <- reproducible:::driveLs(cloudFolderID = folder, verbose = -1)
  expect_identical(n, 3L)
  expect_identical(out$name, "a")
})
