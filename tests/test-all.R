library(testthat)

# NOTE: ALL OPTIONS ARE BEING SET IN tests/testthat/setup.R

## run all tests using different combinations of env vars
if (nzchar(Sys.getenv("NOT_CRAN")) && as.logical(Sys.getenv("NOT_CRAN"))) {
  Sys.setenv(R_REPRODUCIBLE_RASTER_READ = "raster::raster")
  Sys.setenv(R_REPRODUCIBLE_USE_DBI = "false")
  test_check("reproducible")

  Sys.setenv(R_REPRODUCIBLE_RASTER_READ = "raster::raster")
  Sys.setenv(R_REPRODUCIBLE_USE_DBI = "true")
  test_check("reproducible")

  Sys.setenv(R_REPRODUCIBLE_RASTER_READ = "terra::rast") ## default
  Sys.setenv(R_REPRODUCIBLE_USE_DBI = "false") ## default
  test_check("reproducible")

  Sys.setenv(R_REPRODUCIBLE_RASTER_READ = "terra::rast")
  Sys.setenv(R_REPRODUCIBLE_USE_DBI = "true")
  test_check("reproducible")
} else {
  test_check("reproducible")
}
