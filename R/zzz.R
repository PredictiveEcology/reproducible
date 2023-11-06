.onLoad <- function(libname, pkgname) {
  ## set options using the approach used by devtools
  opts <- options()
  opts.reproducible <- reproducibleOptions()
  toset <- !(names(opts.reproducible) %in% names(opts))
  if (any(toset)) options(opts.reproducible[toset])
  SysInfo <<- Sys.info() # update with system at time of loading; all we need is username
  invisible()
}

#' @importFrom utils packageVersion
.onAttach <- function(libname, pkgname) {
  if (isInteractive()) {
    packageStartupMessage(
      "Using reproducible version ",
      utils::packageVersion("reproducible"), ".",
      # "\n  'reproducible' has changed the default database backend.", # Not true yet
      "\n  See ?reproducibleOptions for details."
    )
  }
}

.onUnload <- function(libpath) {
  ## unset reproducible options on unload
  o <- options()
  o[startsWith(names(o), prefix = "reproducible.")] <- NULL
  options(o)
}

.reproducibleTempPath <- function() getOption("reproducible.tempPath") # file.path(tempdir(), "reproducible")
.reproducibleTempCacheDir <- function() file.path(getOption("reproducible.tempPath"), "cache")
.reproducibleTempInputDir <- function() file.path(tempdir(), "reproducible", "inputs")

.argsToRemove <- unique(c(
  names(formals(prepInputs)),
  names(formals(cropInputs)),
  names(formals(cropTo)),
  names(formals(maskTo)),
  names(formals(projectTo)),
  names(formals(postProcessTo)),
  names(formals(fixErrors)),
  names(formals(fixErrorsIn)),
  # names(formals(raster::writeRaster)),
  # names(formals(raster::projectRaster)),
  names(formals(determineFilename)),
  names(formals(writeOutputs)),
  names(formals(writeTo)),
  unlist(lapply(
    methods("postProcess"),
    function(x) names(formals(x))
  ))
))

#' The `reproducible` package environment
#'
#' Environment used internally to store internal package objects and methods.
#'
#' @keywords internal
#' @rdname pkgEnv
.pkgEnv <- new.env(parent = emptyenv())
.pkgEnv$testCacheCounter <- 1L
