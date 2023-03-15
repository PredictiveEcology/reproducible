library(testthat)
library(data.table)
origDTthreads <- getDTthreads()
setDTthreads(2)
# devtools::check(args = c('--as-cran','--run-dontrun','--run-donttest'),
#                 env_vars = c("R_REPRODUCIBLE_RUN_ALL_EXAMPLES" = "true"))
# opts <- options(reproducible.verbose = -2)
# on.exit(options(opts))
#  ff <- list()
#  runTestsWithTimings("ff", authorizeGoogle = TRUE)
#
opts <- options(reproducible.useTerra = TRUE,
                reproducible.rasterRead = "terra::rast")
test_check("reproducible")
# options(opts)
setDTthreads(origDTthreads)
