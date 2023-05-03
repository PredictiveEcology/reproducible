library(data.table)
origDTthreads <- getDTthreads()
wantMoreTests <- isInteractive() || Sys.info()["user"] %in% "emcintir"
if (wantMoreTests) # this is for covr::package_coverage
  Sys.setenv(NOT_CRAN="true")
opts <- options(# reproducible.rasterRead = "raster::raster",
                reproducible.runLargeFileTests = FALSE) # Set to TRUE to run the 2 long tests -- 20 minutes
setDTthreads(2)
withr::defer({
  if (wantMoreTests) {
    print(paste0("getOption('reproducible.rasterRead') = ", getOption("reproducible.rasterRead")))
    print(paste0("getOption('reproducible.runLargeFileTests') = ", getOption('reproducible.runLargeFileTests')))
    Sys.setenv(NOT_CRAN="")
  }
  options(opts)
  setDTthreads(origDTthreads)

}, teardown_env())
if (wantMoreTests) {
  print(paste0("getOption('reproducible.rasterRead') = ", getOption("reproducible.rasterRead")))
  print(paste0("getOption('reproducible.runLargeFileTests') = ", getOption('reproducible.runLargeFileTests')))
}

