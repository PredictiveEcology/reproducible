## One-shot script: time test_that blocks in test-prepInputs.R with tracing.
## Optional 1st arg = filter (regex on test_that description).
##   Rscript dev/run-time-tests.R                     # time all blocks
##   Rscript dev/run-time-tests.R "preProcess"        # only blocks matching
options(warn = 1)
setwd("/home/emcintir/GitHub/reproducible")
Sys.setenv(NOT_CRAN = "true")
options(testthat.progress.max_fails = Inf)
source("dev/time-tests.R")

args <- commandArgs(trailingOnly = TRUE)
flt  <- if (length(args)) args[[1L]] else NULL

res <- time_tests("tests/testthat/test-prepInputs.R", trace_downloads = FALSE,
                  force_interactive = TRUE, filter = flt)
saveRDS(res, "dev/time-tests-summary.rds")
