## Run exactly the "test useDBI TRUE <--> FALSE" test_that block in isolation,
## under testthat::test_that, to see if the 22-minute slowdown reproduces
## without the rest of test-cache.R having run first.
suppressPackageStartupMessages({
  devtools::load_all(quiet = TRUE)
  library(testthat)
})

## Mimic helper-allEqual.R::testInit's tmpdir + cachePath setup minimally.
testInit_min <- function() {
  td <- withr::local_tempdir(.local_envir = parent.frame())
  withr::local_options(reproducible.cachePath = td, .local_envir = parent.frame())
  withr::local_options(reproducible.ask = FALSE, .local_envir = parent.frame())
  td
}

t0 <- Sys.time()
test_that("test useDBI TRUE <--> FALSE (isolated)", {
  tmpdir <- testInit_min()
  orig <- useDBI()
  on.exit(useDBI(orig), add = TRUE)
  useDBI(TRUE)
  d <- b <- a <- list()
  b[[1]] <- Cache(rnorm(1))
  b[[2]] <- Cache(rnorm(2))
  b[[3]] <- Cache(runif(3))
  useDBI(FALSE)
  a[[1]] <- Cache(rnorm(1))
  a[[2]] <- Cache(rnorm(2))
  a[[3]] <- Cache(runif(3))
  useDBI(TRUE)
  d[[1]] <- Cache(rnorm(1))
  d[[2]] <- Cache(rnorm(2))
  d[[3]] <- Cache(runif(3))
  lapply(a, function(aa) expect_false(attr(aa, ".Cache")$newCache))
  lapply(b, function(aa) expect_true(attr(aa, ".Cache")$newCache))
  lapply(d, function(aa) expect_false(attr(aa, ".Cache")$newCache))
})
cat(sprintf("\nELAPSED (isolated test_that): %.2fs\n",
            as.numeric(difftime(Sys.time(), t0, units = "secs"))))
