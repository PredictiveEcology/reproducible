library(testthat)
# devtools::check(args = c('--as-cran','--run-dontrun','--run-donttest'),
#                 env_vars = c("R_REPRODUCIBLE_RUN_ALL_EXAMPLES" = "true"))
# opts <- options(reproducible.verbose = -2)
# on.exit(options(opts))
test_check("reproducible")
# options(opts)
on.exit()


