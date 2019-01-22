## be sure to update the 'Package Options' section of the package help file
##   in R/reproducible-package.R
##
.onLoad <- function(libname, pkgname) {
  ## set options using the approach used by devtools
  opts <- options()
  checkPath(.reproducibleTempCacheDir, create = TRUE)
  checkPath(.reproducibleTempInputDir, create = TRUE)
  opts.reproducible <- reproducibleOptions()
  toset <- !(names(opts.reproducible) %in% names(opts))
  if (any(toset)) options(opts.reproducible[toset])

  backports::import(pkgname, obj = "isFALSE")
  invisible()
}

#' @importFrom utils packageVersion
.onAttach <- function(libname, pkgname) {
  if (isInteractive()) {
    packageStartupMessage("Using reproducible version ",
                          utils::packageVersion("reproducible"), ".",
                          "\n  'reproducible' has changed the default digest algorithm.",
                          "\n  See ?reproducibleOptions for details. To revert to the old:",
                          "\n  options('reproducible.useNewDigestAlgorithm' = FALSE)")
  }
}

.onUnload <- function(libpath) {
  ## unset reproducible options on unload
  o <- options()
  o[startsWith(names(o), prefix = "reproducible.")] <- NULL
  options(o)
  # if (getOption("reproducible.cachePath") == file.path(.reproducibleTempCacheDir)) {
  #   options(reproducible.cachePath = NULL)
  # }
  # if (getOption("reproducible.inputPaths") == file.path(.reproducibleTempInputDir)) {
  #   options(reproducible.inputPaths = NULL)
  # }
}

.reproducibleTempCacheDir <- file.path(tempdir(), "reproducible", "cache")
.reproducibleTempInputDir <- file.path(tempdir(), "reproducible", "inputs")

.argsToRemove <- argsToRemove <- unique(c(names(formals(prepInputs)),
                                          names(formals(cropInputs)),
                                          names(formals(fixErrors)),
                                          names(formals(raster::writeRaster)),
                                          names(formals(raster::projectRaster)),
                                          names(formals(determineFilename)),
                                          names(formals(writeOutputs)),
                                          unlist(lapply(methods("postProcess"), function(x) names(formals(x))))))

