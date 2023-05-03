library(testthat)
# library(data.table)
# origDTthreads <- getDTthreads()
# setDTthreads(2)
# options(reproducible.rasterRead = "terra::rast")
# options(reproducible.runLargeFileTests = FALSE) # Set to TRUE to run the 2 long tests -- 20 minutes
# print(getOption("reproducible.rasterRead"))
# cat(getOption("reproducible.rasterRead"))
# devtools::check(args = c('--as-cran','--run-dontrun','--run-donttest'),
#                 env_vars = c("R_REPRODUCIBLE_RUN_ALL_EXAMPLES" = "true"))
# opts <- options(reproducible.verbose = -2)
# on.exit(options(opts))
#  ff <- list()
#  runTestsWithTimings("ff", authorizeGoogle = TRUE)
#
test_check("reproducible")
# options(opts)
# setDTthreads(origDTthreads)
