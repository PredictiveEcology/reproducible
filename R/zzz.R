.onLoad <- function(libname, pkgname) {
  Require::checkPath(.reproducibleTempCacheDir(), create = TRUE)
  Require::checkPath(.reproducibleTempInputDir(), create = TRUE)

  ## set options using the approach used by devtools
  opts <- options()
  opts.reproducible <- reproducibleOptions()
  toset <- !(names(opts.reproducible) %in% names(opts))
  if (any(toset)) options(opts.reproducible[toset])
  td <- tempdir()
  if (!dir.exists(td)) dir.create(td)
  invisible()
}

#' @importFrom utils packageVersion
.onAttach <- function(libname, pkgname) {
  if (isInteractive()) {
    packageStartupMessage(
      "Using reproducible version ",
      utils::packageVersion("reproducible"), ".",
      # "\n  'reproducible' has changed the default database backend.",
      "\n  See ?reproducibleOptions for many available options to customize reproducible.",
      "\n  During transition to GDAL>3 and PROJ>6, many warnings will be suppressed until",
      " simple solutions are available; if these GDAL and PROJ changes",
      " are important to your project you will have to manually update",
      " proj and crs in spatial objects.")
  }
}

.onDetach <- function(libpath) {
  connsToDisconnect <- grep("_conn", ls(.pkgEnv), value = TRUE)
  if (length(connsToDisconnect))
    disConnected <- lapply(mget(connsToDisconnect, envir = .pkgEnv), dbDisconnectAll,
                           shutdown = TRUE)
  return(invisible(NULL))
}

.onUnload <- function(libpath) {
  ## unset reproducible options on unload
  o <- options()
  o[startsWith(names(o), prefix = "reproducible.")] <- NULL
  options(o)
}

.reproducibleTempPath <- function(sub = "")
  Require::tempdir2(sub = sub, tempdir = file.path(tempdir(), "reproducible"))
.reproducibleTempCacheDir <- function() .reproducibleTempPath("cache")
.reproducibleTempInputDir <- function() Require::tempdir2("inputs")


#' @importFrom utils isS3stdGeneric
formalsAllMethods <- function(fn, envir = parent.frame()) {
  fnChar <- deparse(substitute(fn))
  if (is(fn, "character")) {
    fnChar <- fn
    fn <- eval(parse(text = fn))
  }
  if (isS4(fn)) {
    meths <- showMethods(fn, printTo = FALSE)[-1]
    meths <- meths[nzchar(meths)]
    meths2 <- strsplit(meths, split = ",")
    names(meths2) <- meths
    nams <-
      lapply(meths2, function(x) {
        sig <- gsub("\"(.+)\"", "\\1" , x)
        sig <- gsub(" ", "", sig)
        nams <- strsplit(sig, "=")
        nams1 <- sapply(nams, function(yy) yy[[1]])
        sigs <- sapply(nams, function(yy) yy[[2]])
        names(sigs) <- nams1
        methodFormals(fn, signature = sigs)
      })
  } else if (isS3stdGeneric(fnChar)) {
    meths <- methods(fnChar)
    names(meths) <- meths
    nams <- lapply(meths, function(y) formals(y))
    nams <- append(list("generic" = formals(fn)), nams)
  } else {
    nams <- list(formals(fn))
    names(nams) <- "function"
  }
  nams

}

formalArgsAllMethods <- function(fn, unique = TRUE, envir = parent.frame()) {
  fnChar <- if (is.character(fn))
    fn
  else
    deparse(substitute(fn))
  fam <- formalsAllMethods(fnChar)
  if (isTRUE(unique)) {
    fam1 <- unname(fam)
    fam <- unique(unlist(lapply(fam1, names)))
  }

  fam
}

.argsToRemove <- function() unique(c(formalArgsAllMethods(prepInputs),
                                     formalArgsAllMethods(cropInputs),
                                     formalArgsAllMethods(fixErrors),
                                     formalArgsAllMethods(terra::writeRaster),
                                     formalArgsAllMethods(terra::project),
                                     formalArgsAllMethods(determineFilename),
                                     formalArgsAllMethods(writeOutputs),
                                     formalArgsAllMethods(postProcess)))

#' The \code{reproducible} package environment
#'
#' Environment used internally to store internal package objects and methods.
#'
#' @keywords internal
#' @rdname pkgEnv
.pkgEnv <- new.env(parent = emptyenv())
