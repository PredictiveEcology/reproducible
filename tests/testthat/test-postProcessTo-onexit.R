## `terraOptions()` is process-wide, so postProcessTo() restores what it changes with
## on.exit(). It changes up to two options -- memfrac and memmax -- and registers a
## restore for each, so every one of those on.exit() calls must pass `add = TRUE`:
## without it, a later registration replaces an earlier one and that option is left
## changed for the rest of the session. Whether the bug bites depends only on which
## branches happen to run, which is not something to rely on.

test_that("every terra-option restore in postProcessTo is registered with add = TRUE", {
  f <- testthat::test_path("..", "..", "R", "postProcessTo.R")
  skip_if_not(file.exists(f), "source not available (installed package)")

  code <- grep("^\\s*#", readLines(f, warn = FALSE), invert = TRUE, value = TRUE)
  restores <- grep("on\\.exit\\(.*terraOptions", code, value = TRUE)
  expect_gt(length(restores), 0L)          # the restores still exist at all
  expect_true(all(grepl("add\\s*=\\s*TRUE", restores)),
              info = paste("restore(s) without add = TRUE:",
                           paste(grep("add\\s*=\\s*TRUE", restores, invert = TRUE, value = TRUE),
                                 collapse = " | ")))
})
