## .gdriveRetry(): a googledrive call is run again after a brief failure, never after a refusal.

waits <- NULL
noWait <- function(attempt) waits <<- c(waits, reproducible:::.gdriveRetryDelay(attempt))

## The condition gargle raises for a 403 from the Drive API, with Google's `reason` (as seen from
## CacheGeoWrite() on the macOS runner: "User rate limit exceeded", reason userRateLimitExceeded).
gargle403 <- function(reason, detail) {
  structure(class = c("gargle_error_request_failed", "http_error_403", "rlang_error", "error", "condition"),
            list(message = paste(c("Client error: (403) Forbidden", detail, paste0("• message: ", detail),
                                   "• domain: usageLimits", paste0("• reason: ", reason)), collapse = "\n"),
                 call = NULL))
}

test_that("a 403 rate limit is retried (Google: retriable with backoff); a 403 denial is not", {
  waits <<- NULL
  local_mocked_bindings(.gdriveRetryWait = noWait)
  n <- 0L
  limited <- function() {
    n <<- n + 1L
    if (n <= 2L) stop(gargle403("userRateLimitExceeded", "User rate limit exceeded."))
    "ok"
  }
  expect_identical(reproducible:::.gdriveRetry(limited(), verbose = -1), "ok")
  expect_identical(n, 3L)
  expect_identical(waits, c(2L, 4L))
  n <- 0L
  expect_identical(reproducible:::.gdriveRetry({
    n <- n + 1L
    if (n == 1L) stop(gargle403("rateLimitExceeded", "Rate Limit Exceeded"))
    "ok"
  }, verbose = -1), "ok")
  expect_identical(n, 2L)

  denied <- gargle403("insufficientFilePermissions", "The user does not have sufficient permissions for this file.")
  expect_true(reproducible:::.gdriveIsDenial(denied))
  expect_false(reproducible:::.gdriveIsDenial(gargle403("userRateLimitExceeded", "User rate limit exceeded.")))
  local_mocked_bindings(.gdriveRetryWait = function(attempt) stop("must not wait"))
  n <- 0L
  expect_error(reproducible:::.gdriveRetry({ n <- n + 1L; stop(denied) }, verbose = -1), "403")
  expect_identical(n, 1L)
})

test_that("the wait before a retry doubles from 2 s and is capped at 32 s", {
  expect_identical(vapply(1:7, reproducible:::.gdriveRetryDelay, integer(1)), c(2L, 4L, 8L, 16L, 32L, 32L, 32L))
})

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
