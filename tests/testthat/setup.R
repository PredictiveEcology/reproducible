library(data.table)
origDTthreads <- getDTthreads()
wantMoreTests <- isInteractive() || Sys.info()["user"] %in% "emcintir"
if (wantMoreTests) { # this is for covr::package_coverage
  Sys.setenv(NOT_CRAN = "true")
  # Sys.setenv(SKIP_GAUTH = "true")
}
opts <- options(
  reproducible.runLargeFileTests = FALSE, # Set to TRUE to run the 2 long tests -- 20 minutes
  warnPartialMatchArgs = TRUE, # This gives false positives for `raster::stack`
  warnPartialMatchAttr = TRUE,
  warnPartialMatchDollar = TRUE
)
setDTthreads(2)
withr::defer(
  {
    if (wantMoreTests) {
      print(paste0("getOption('reproducible.rasterRead') = ", getOption("reproducible.rasterRead")))
      print(paste0("getOption('reproducible.runLargeFileTests') = ", getOption("reproducible.runLargeFileTests")))
      print(paste0("getOption('reproducible.useDBI') = ", getOption("reproducible.useDBI")))
      Sys.setenv(NOT_CRAN = "")
      Sys.setenv(SKIP_GAUTH = "")
    }
    options(opts)
    data.table::setDTthreads(origDTthreads)
    try(unlink("CHECKSUMS.txt"), silent = TRUE) # comes from an unknown place
  },
  teardown_env()
)
if (wantMoreTests) {
  print(paste0("getOption('reproducible.rasterRead') = ", getOption("reproducible.rasterRead")))
  print(paste0("getOption('reproducible.runLargeFileTests') = ", getOption("reproducible.runLargeFileTests")))
  print(paste0("getOption('reproducible.useDBI') = ", getOption("reproducible.useDBI")))
}
