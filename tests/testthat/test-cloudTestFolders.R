## The Drive test-folder helpers in helper-allEqual.R, with Drive mocked: they run
## on every Drive-backed session, and a mistake in the sweep would trash folders.

test_that(".sweepOldCloudTestRuns trashes only old run-* folders this account owns", {
  skip_if_not_installed("googledrive")
  seen <- new.env()
  trashed <- character()
  testthat::local_mocked_bindings(
    drive_ls = function(path, ..., recursive = FALSE) {
      seen$path <- path
      seen$args <- list(...)
      data.frame(name = c("run-1-R-CMD-check-abcdef", "urlForTiles_x", "run-2-test-coverage-ghijkl"),
                 id = c("a", "b", "c"))
    },
    drive_trash = function(file, ...) {
      if (file$id == "c") stop("Insufficient permissions")  # must not stop the sweep
      trashed <<- c(trashed, file$id)
      invisible(file)
    },
    .package = "googledrive"
  )
  n <- .sweepOldCloudTestRuns(list(id = "parentId"), olderThanDays = 3, max = 7)

  expect_identical(as.character(seen$path), "parentId")
  q <- seen$args$q
  expect_true("name contains 'run-'" %in% q)
  expect_true("'me' in owners" %in% q)
  cutoff <- sub("^createdTime < '(.*)'$", "\\1", grep("^createdTime < ", q, value = TRUE))
  age <- difftime(Sys.time(), as.POSIXct(cutoff, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
                  units = "days")
  expect_equal(as.numeric(age), 3, tolerance = 0.01)
  expect_identical(seen$args$n_max, 7)

  expect_identical(n, 2L)                  # the non-run- folder is never touched
  expect_identical(trashed, "a")           # "c" failed, quietly
})

test_that(".sweepOldCloudTestRuns never errors when listing fails", {
  skip_if_not_installed("googledrive")
  testthat::local_mocked_bindings(
    drive_ls = function(...) stop("HTTP 500"),
    drive_trash = function(...) stop("must not be called"),
    .package = "googledrive"
  )
  expect_identical(.sweepOldCloudTestRuns(list(id = "p")), 0L)
})

test_that(".cloudTestRoot(peek = TRUE) creates nothing", {
  skip_if_not_installed("googledrive")
  testthat::local_mocked_bindings(
    drive_mkdir = function(...) stop("must not be called"),
    drive_get = function(...) stop("must not be called"),
    .package = "googledrive"
  )
  ## NULL unless this session already made its folder (only in a Drive-backed run)
  expect_true(is.null(.cloudTestRoot(peek = TRUE)) || is.list(.cloudTestRoot(peek = TRUE)))
})
