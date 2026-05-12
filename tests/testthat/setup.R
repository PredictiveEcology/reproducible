library(data.table)

origDTthreads <- setDTthreads(2)

## Detect "this is a local dev run, not CI/CRAN" so we can flip a few
## opt-in switches on by default. CRAN/CI already skip via skip_on_cran() /
## skip_on_ci(); the runLargeFileTests gate is in addition to those for
## tests that need the 20–30 minute integration path.
onCI   <- isTRUE(as.logical(Sys.getenv("CI", "false")))
onCRAN <- !nzchar(Sys.getenv("NOT_CRAN")) &&
          !isTRUE(as.logical(Sys.getenv("NOT_CRAN", "false")))
wantMoreTests <- isInteractive() || Sys.info()[["user"]] %in% c("emcintir")

if (wantMoreTests) { # this is for covr::package_coverage
  Sys.setenv(NOT_CRAN = "true")
  # Sys.setenv(SKIP_GAUTH = "true")
}

opts <- options(
  ## Default: run the multi-minute large-file tests when this is a local
  ## (non-CI, non-CRAN) dev run. User can override either way:
  ##   options(reproducible.runLargeFileTests = TRUE  / FALSE)
  reproducible.runLargeFileTests = isTRUE(
    getOption("reproducible.runLargeFileTests", wantMoreTests && !onCI && !onCRAN)
  ),
  warnPartialMatchArgs = TRUE, # This gives false positives for `raster::stack`
  warnPartialMatchAttr = TRUE,
  warnPartialMatchDollar = TRUE,
  reproducible.useCacheV3 = !isFALSE(getOption("reproducible.useCacheV3"))#,
  #reproducible.useDBI = FALSE  # done TF  = c(691, 764.1), TT c(793,874), FF = c(875.5s, 876s), FT = c(1024.8,934)
)

# if (Sys.info()["user"] %in% "emcintir") {
#   opts2 <- options(gargle_oauth_email = "predictiveecology@gmail.com")
#   secretDir <- if (isWindows()) "C:/Eliot/.secret" else "~/.secret"
#   opts2 <- append(options(gargle_oauth_cache = secretDir), opts2)
#   if (requireNamespace("googledrive"))
#     googledrive::drive_auth()
#   opts <- append(opts, opts2)
# }


# uses eliot-githubauthentication@genial-cycling-408722.iam.gserviceaccount.com
# Whatever files are on googledrive must be shared with this google service account
if (isNamespaceLoaded("googledrive"))
  if ((!googledrive::drive_has_token())) {
    gauthEnv <- Sys.getenv("GOOGLEDRIVE_AUTH")
    if (nzchar(gauthEnv)) {
      if (file.exists(gauthEnv))
        ## see helper-allEqual.R: tolerate revoked / rotated service-account keys
        tryCatch(googledrive::drive_auth(path = gauthEnv),
                 error = function(e) invisible(NULL))
    }
  }


withr::defer(
  {
    if (wantMoreTests) {
      print(paste0("getOption('reproducible.cacheSaveFormat') = '", getOption("reproducible.cacheSaveFormat"), "'"))
      print(paste0("getOption('reproducible.useCacheV3') = ", getOption("reproducible.useCacheV3")))
      print(paste0("getOption('reproducible.rasterRead') = ", getOption("reproducible.rasterRead")))
      print(paste0("getOption('reproducible.runLargeFileTests') = ", getOption("reproducible.runLargeFileTests")))
      print(paste0("getOption('reproducible.useDBI') = ", getOption("reproducible.useDBI")))
      print(paste0("getOption('reproducible.useMemoise') = ", getOption("reproducible.useMemoise")))
      Sys.setenv(NOT_CRAN = "")
      Sys.setenv(SKIP_GAUTH = "")
    }
    options(opts)
    data.table::setDTthreads(origDTthreads)
    try(reproducible::clearCache(ask = FALSE, verbose = -1))
    try(unlink("CHECKSUMS.txt"), silent = TRUE) # comes from an unknown place
  },
  teardown_env()
)

if (wantMoreTests) {
  print(paste0("getOption('reproducible.cacheSaveFormat') = '", getOption("reproducible.cacheSaveFormat"), "'"))
  print(paste0("getOption('reproducible.useCacheV3') = ", getOption("reproducible.useCacheV3")))
  print(paste0("getOption('reproducible.rasterRead') = ", getOption("reproducible.rasterRead")))
  print(paste0("getOption('reproducible.runLargeFileTests') = ", getOption("reproducible.runLargeFileTests")))
  print(paste0("getOption('reproducible.useDBI') = ", getOption("reproducible.useDBI")))
  print(paste0("getOption('reproducible.useMemoise') = ", getOption("reproducible.useMemoise")))
}
