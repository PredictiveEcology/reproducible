## Bisect the useDBI block: time every single statement.
suppressPackageStartupMessages({
  devtools::load_all(quiet = TRUE)
  library(testthat)
})

tt <- function(label, expr) {
  t0 <- Sys.time()
  res <- force(expr)
  el <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  cat(sprintf("[%7.2fs]  %s\n", el, label))
  res
}

td <- withr::local_tempdir()
options(reproducible.cachePath = td, reproducible.ask = FALSE)

orig <- useDBI()
on.exit(useDBI(orig), add = TRUE)

tt("useDBI(TRUE)",        useDBI(TRUE))
tt("Cache(rnorm(1)) #b1", { b1 <- Cache(rnorm(1)) })
tt("Cache(rnorm(2)) #b2", { b2 <- Cache(rnorm(2)) })
tt("Cache(runif(3)) #b3", { b3 <- Cache(runif(3)) })
tt("useDBI(FALSE)",       useDBI(FALSE))
tt("Cache(rnorm(1)) #a1", { a1 <- Cache(rnorm(1)) })
tt("Cache(rnorm(2)) #a2", { a2 <- Cache(rnorm(2)) })
tt("Cache(runif(3)) #a3", { a3 <- Cache(runif(3)) })
tt("useDBI(TRUE)#2",      useDBI(TRUE))
tt("Cache(rnorm(1)) #d1", { d1 <- Cache(rnorm(1)) })
tt("Cache(rnorm(2)) #d2", { d2 <- Cache(rnorm(2)) })
tt("Cache(runif(3)) #d3", { d3 <- Cache(runif(3)) })
