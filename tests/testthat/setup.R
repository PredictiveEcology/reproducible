library(data.table)
origDTthreads <- getDTthreads()
opts <- options(reproducible.rasterRead = "raster::raster",
                reproducible.runLargeFileTests = TRUE) # Set to TRUE to run the 2 long tests -- 20 minutes
setDTthreads(2)
withr::defer({
  if (isInteractive()) {
    print(paste0("getOption('reproducible.rasterRead') = ", getOption("reproducible.rasterRead")))
    print(paste0("getOption('reproducible.runLargeFileTests') = ", getOption('reproducible.runLargeFileTests')))
  }
  options(opts)
  setDTthreads(origDTthreads)
}, teardown_env())
if (isInteractive()) {
  print(paste0("getOption('reproducible.rasterRead') = ", getOption("reproducible.rasterRead")))
  print(paste0("getOption('reproducible.runLargeFileTests') = ", getOption('reproducible.runLargeFileTests')))
}

