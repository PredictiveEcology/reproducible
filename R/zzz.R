#' The `reproducible` package environments
#'
#' Environment used internally to store internal package objects and methods.
#'
#' - `.message` is specifically for messages and message-generating functions;
#' - `.pkgEnv` is for general use within the package;
#' - `.reproEnv` is used for `Cache`-related objects;
#'
#' @keywords internal
#' @rdname pkgEnv
.pkgEnv <- new.env(parent = emptyenv())
.pkgEnv$testCacheCounter <- 1L

.onLoad <- function(libname, pkgname) {
  ## set options using the approach used by devtools
  opts <- options()
  opts.reproducible <- reproducibleOptions()
  toset <- !(names(opts.reproducible) %in% names(opts))
  if (any(toset)) options(opts.reproducible[toset])
  .pkgEnv$SysInfo <- Sys.info() # record once at loading; repeatedly calling Sys.info is a waste
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
