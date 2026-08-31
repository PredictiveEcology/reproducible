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
  reproducible.useCacheV3 = !isFALSE(getOption("reproducible.useCacheV3")),
  ## Under covr (covr::package_coverage sets R_COVR=true; the workflow also sets
  ## USING_COVR), disable the automatic showCache pre-warm fork. covr runs the
  ## whole suite in ONE process that touches many distinct cachePaths, so the
  ## per-path forks accumulate to ~38 / ~23 GB and OOM-kill the 16 GB runner
  ## (the months-long test-coverage exit-143). Plain R CMD check still exercises
  ## the fork (it stays on there). This is the documented
  ## `reproducible.showCachePreWarm` advanced option.
  reproducible.showCachePreWarm = !(
    isTRUE(as.logical(Sys.getenv("R_COVR", "false"))) ||
      isTRUE(as.logical(Sys.getenv("USING_COVR", "false")))
  )
)

# if (Sys.info()["user"] %in% "emcintir") {
#   opts2 <- options(gargle_oauth_email = "predictiveecology@gmail.com")
#   secretDir <- if (isWindows()) "C:/Eliot/.secret" else "~/.secret"
#   opts2 <- append(options(gargle_oauth_cache = secretDir), opts2)
#   if (requireNamespace("googledrive"))
#     googledrive::drive_auth()
#   opts <- append(opts, opts2)
# }


# Drive auth for the test suite: a user OAuth token only (see helper-allEqual.R).
if (isNamespaceLoaded("googledrive"))
  if ((!googledrive::drive_has_token())) {
    ## A user OAuth token only, as testInit() does. Service accounts are not
    ## supported: one authenticates but has no Drive quota on user-owned
    ## folders, so it cannot complete an upload round-trip. GDRIVE_OAUTH_TOKEN
    ## is a path to a serialized token, staged by the reusable workflow from
    ## the org-level GOOGLEDRIVE_AUTH secret. See helper-allEqual.R.
    oauthTokenFile <- Sys.getenv("GDRIVE_OAUTH_TOKEN")
    if (nzchar(oauthTokenFile) && file.exists(oauthTokenFile)) {
      tok <- tryCatch(readRDS(oauthTokenFile), error = function(e) {
        message("GDRIVE_OAUTH_TOKEN could not be read: ", conditionMessage(e)); NULL
      })
      if (!is.null(tok)) {
        ## Drop the token's own cache_path before using it. drive_auth()
        ## writes the refreshed token back to that path, which is wherever
        ## the token was MINTED (e.g. ~/.secret on a dev machine). On a CI
        ## runner that directory does not exist, the write fails, and gargle
        ## surfaces it as the maximally unhelpful "Can't get Google
        ## credentials" -- indistinguishable from having no credential at
        ## all. A runner should not be persisting a credential to disk
        ## anyway, so this is the right behaviour regardless.
        tok$cache_path <- NULL
        tryCatch(googledrive::drive_auth(token = tok),
                 error = function(e)
                   message("GDRIVE_OAUTH_TOKEN was not usable: ", conditionMessage(e)))
      }
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
