.onLoad <- function(libname, pkgname) {
  Require::checkPath(.reproducibleTempCacheDir(), create = TRUE)
  Require::checkPath(.reproducibleTempInputDir(), create = TRUE)

  ## set options using the approach used by devtools
  opts <- options()
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
                          "\n  'reproducible' has changed the default database backend.",
                          "\n  See ?reproducibleOptions for details.")
  }
}

.onUnload <- function(libpath) {
  ## unset reproducible options on unload
  o <- options()
  o[startsWith(names(o), prefix = "reproducible.")] <- NULL
  options(o)
}

.reproducibleTempPath <- function() Require::tempdir2()
.reproducibleTempCacheDir <- function() Require::tempdir2("cache")
.reproducibleTempInputDir <- function() Require::tempdir2("inputs")

.argsToRemove <- argsToRemove <- unique(c(names(formals(prepInputs)),
                                          names(formals(cropInputs)),
                                          names(formals(fixErrors)),
                                          names(formals(raster::writeRaster)),
                                          names(formals(raster::projectRaster)),
                                          names(formals(determineFilename)),
                                          names(formals(writeOutputs)),
                                          unlist(lapply(methods("postProcess"),
                                                        function(x) names(formals(x))))))

#' The \code{reproducible} package environment
#'
#' Environment used internally to store internal package objects and methods.
#'
#' @keywords internal
#' @rdname pkgEnv
.pkgEnv <- new.env(parent = emptyenv())
