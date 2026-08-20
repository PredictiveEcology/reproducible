library(testthat)

# NOTE: ALL OPTIONS ARE BEING SET IN tests/testthat/setup.R

## Test matrix: 5 combinations of {raster, terra} × {USE_DBI, no DBI} +
## qs cache format. Running them sequentially in one R session was the
## documented behavior, but on ubuntu CI it triggered SIGTERM mass-kills
## (cumulative memory pressure from 5 test_check passes loading namespaces
## + raster/SF objects without releasing between iterations). Bisect on
## branch `bisect-ubuntu-fail` confirmed the SINGLE-pass full suite
## passes; only the 5-pass version wedges. Default to single pass; opt
## back into the full matrix via R_REPRO_TEST_FULL_MATRIX=true (e.g.
## locally or on a separate workflow) when you want the full coverage.
if (nzchar(Sys.getenv("R_REPRO_TEST_FULL_MATRIX")) &&
    as.logical(Sys.getenv("R_REPRO_TEST_FULL_MATRIX"))) {
  ## Full 5-combo matrix (opt-in)
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

  Sys.setenv(R_REPRODUCIBLE_CACHESAVEFORMAT = "qs") ## default
  Sys.setenv(R_REPRODUCIBLE_USE_DBI = "false") ## default
  test_check("reproducible")

} else {
  ## Single pass at defaults (terra + USE_DBI=FALSE + qs format).
  ## Covers ~95% of the test surface; alternate raster/DBI paths are
  ## still exercised by individual test files that explicitly opt in.
  test_check("reproducible")

  ## On CI only (NOT CRAN -- keeps CRAN's check within its time budget), add a
  ## focused pass under the SQLite (useDBI) backend of every test file that
  ## touches the cache metadata store. This is deliberately wider than
  ## test-cache*.R: urlLog tags cacheIds, the cloud code round-trips metadata
  ## between the two backends, and showCache/clearCache read it. Narrowing this
  ## to "^cache" is what let urlLog sit silently disabled under DBI -- nothing
  ## in the DBI pass covered it. Anything with a `useDBI()` branch behind it
  ## belongs here; prepInputs()/postProcess()/gis tests do not.
  ## `CI` is set by GitHub Actions but not by CRAN; RSQLite + DBI are required
  ## for the backend (skipped if a nosuggests run lacks them). The option is set
  ## directly (the R_REPRODUCIBLE_USE_DBI env var is only read at package load).
  if (isTRUE(as.logical(Sys.getenv("CI", "false"))) &&
      requireNamespace("RSQLite", quietly = TRUE) &&
      requireNamespace("DBI", quietly = TRUE)) {
    options(reproducible.useDBI = TRUE)
    Sys.setenv(R_REPRODUCIBLE_USE_DBI = "true")
    dbiBackedTests <- c(
      "cache",       # cache, cacheGeo, cacheHelpers, cachePath-lazy
      "cloud",       # cloud, cloud-helpers, cloudFolderID-offline, cloudUploadNonFatal
      "useCloud",    # useCloudPullPush
      "showCache",   # showCacheAsyncInstall, showCacheCorruptFile
      "urlLog",
      "multipleCacheRepo",
      "preDigestDump",
      "lazyAsyncSpawn",
      "cluster",
      "misc"
    )
    test_check("reproducible",
               filter = paste(paste0("^", dbiBackedTests), collapse = "|"))
  }
}
