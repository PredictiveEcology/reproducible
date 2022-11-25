library(testthat)
# devtools::check(args = c('--as-cran','--run-dontrun','--run-donttest'),
#                 env_vars = c("R_REPRODUCIBLE_RUN_ALL_EXAMPLES" = "true"))
test_check("reproducible")
