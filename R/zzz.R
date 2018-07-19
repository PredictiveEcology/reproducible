## be sure to update the 'Package Options' section of the package help file
##   in R/reproducible-package.R
##
.onLoad <- function(libname, pkgname) {
  ## set options using the approach used by devtools
  opts <- options()
  checkPath(.reproducibleTempDir, create = TRUE)
  opts.reproducible <- list( # nolint
    reproducible.ask = TRUE,
    reproducible.cachePath = file.path(.reproducibleTempDir),
    reproducible.futurePlan = FALSE, #future::plan("multiprocess"), #memoise
    reproducible.quick = FALSE,
    reproducible.useCache = TRUE, # override Cache function
    reproducible.useMemoise = TRUE, #memoise
    reproducible.useragent = "http://github.com/PredictiveEcology/reproducible",
    reproducible.verbose = FALSE
  )
  toset <- !(names(opts.reproducible) %in% names(opts))
  if (any(toset)) options(opts.reproducible[toset])

  backports::import(pkgname, obj = "isFALSE")
  invisible()
}

#' @importFrom utils packageVersion
.onAttach <- function(libname, pkgname) {
  if (interactive()) {
    packageStartupMessage("Using reproducible version ", utils::packageVersion("reproducible"), ".")
  }
}

.onUnload <- function(libpath) {
  ## if temp session dir is being used, ensure it gets reset each session
  if (getOption("reproducible.cachePath") == file.path(.reproducibleTempDir, "cache")) {
    options(reproducible.cachePath = NULL)
  }
}
.reproducibleTempDir <- file.path(tempdir(), "reproducibleCache")
