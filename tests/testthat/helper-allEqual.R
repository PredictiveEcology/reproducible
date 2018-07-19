all.equalWONewCache <- function(a, b) {
  attr(a, ".Cache")$newCache <- NULL
  attr(b, ".Cache")$newCache <- NULL
  all.equal(a,b)
}

options(reproducible.verbose = FALSE)
# puts tmpdir, tmpCache, tmpfile (can be vectorized with length >1 tmpFileExt),
#   optsAsk in this environment,
# loads and libraries indicated plus testthat,
# sets options("reproducible.ask" = FALSE) if ask = FALSE
testInit <- function(libraries, ask = FALSE, verbose = FALSE, tmpFileExt = "") {
  optsAsk <- if (!ask)
    options("reproducible.ask" = ask)
  else
    list()
  optsVerbose <- if (verbose)
    options(reproducible.verbose = verbose)
  else
    list()

  if (missing(libraries)) libraries <- list()
  unlist(lapply(libraries, require, character.only = TRUE))
  require("testthat")
  tmpdir <- normPath(file.path(tempdir(), rndstr(1,6)))
  checkPath(tmpdir, create = TRUE)
  origDir <- setwd(tmpdir)
  tmpCache <- normPath(file.path(tmpdir, "testCache"))
  checkPath(tmpCache, create = TRUE)

  if (!is.null(tmpFileExt)) {
    ranfiles <- unlist(lapply(tmpFileExt, function(x) paste0(rndstr(1,7), ".", x)))
    tmpfile <- file.path(tmpdir, ranfiles)
    tmpfile <- gsub(pattern = "\\.\\.", tmpfile, replacement = "\\.")
    file.create(tmpfile)
    tmpfile <- normPath(tmpfile)
  }

  try(clearCache(tmpdir, ask = FALSE), silent = TRUE)
  try(clearCache(tmpCache, ask = FALSE), silent = TRUE)

  outList <- list(tmpdir = tmpdir, origDir = origDir, libs = libraries,
                  tmpCache = tmpCache, optsAsk = optsAsk,
                  optsVerbose = optsVerbose, tmpfile = tmpfile)
  list2env(outList, envir = parent.frame())
  return(outList)
}

testOnExit <- function(testInitOut) {
  if (length(testInitOut$optsVerbose))
    options("reproducible.verbose" = testInitOut$optsVerbose[[1]])
  if (length(testInitOut$optsAsk))
    options("reproducible.ask" = testInitOut$optsAsk[[1]])
  setwd(testInitOut$origDir)
  unlink(testInitOut$tmpdir, recursive = TRUE)
  lapply(testInitOut$libs, function(lib) {
    detach(paste0("package:", lib), character.only = TRUE)}
  )

}
