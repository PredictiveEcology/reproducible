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
testInit <- function(libraries, ask = FALSE, verbose = FALSE, tmpFileExt = "",
                     opts = NULL, needGoogle = FALSE) {
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

  if (interactive() && isTRUE(needGoogle)) {
    if (file.exists("~/.httr-oauth")) {
      linkOrCopy("~/.httr-oauth", to = file.path(tmpdir, ".httr-oauth"))
    } else {
      googledrive::drive_auth()
      file.copy(".httr-oauth", "~/.httr-oauth")
    }
  }
  checkPath(tmpdir, create = TRUE)
  origDir <- setwd(tmpdir)
  tmpCache <- normPath(file.path(tmpdir, "testCache"))
  checkPath(tmpCache, create = TRUE)

  if (!is.null(opts)) {
    opts <- options(opts)
  }
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
                  optsVerbose = optsVerbose, tmpfile = tmpfile,
                  opts = opts)
  list2env(outList, envir = parent.frame())
  return(outList)
}

testOnExit <- function(testInitOut) {
  if (length(testInitOut$optsVerbose))
    options("reproducible.verbose" = testInitOut$optsVerbose[[1]])
  if (length(testInitOut$optsAsk))
    options("reproducible.ask" = testInitOut$optsAsk[[1]])
  if (length(testInitOut$opts))
    options(testInitOut$opts)
  setwd(testInitOut$origDir)
  unlink(testInitOut$tmpdir, recursive = TRUE)
  lapply(testInitOut$libs, function(lib) {
    detach(paste0("package:", lib), character.only = TRUE)}
  )

}

runTest <- function(prod, class, numFiles, mess, expectedMess, filePattern, tmpdir,
                    test) {
  files <- dir(tmpdir, pattern = filePattern, full.names = TRUE)
  expect_true(length(files) == numFiles)
  expect_is(test, class)
  message(mess)
  print(hasMessageNum <-
          paste(collapse = "_", which(unlist(
            lapply(strsplit(expectedMess, "\\|")[[1]], function(m)
              any(grepl(m, mess)))
          ))))

  isOK <- hasMessageNum == prod
  if (!isOK) {
    expe <- as.numeric(strsplit(prod, split = "_")[[1]])
    getting <- as.numeric(strsplit(hasMessageNum, split = "_")[[1]])

    expectedMessVec <- strsplit(expectedMess, split = "\\|")[[1]]
    message("expecting, but didn't get ", paste(collapse = ", ", expectedMessVec[setdiff(expe, getting)]))
    message("got, but didn't expect ", paste(collapse = ", ", expectedMessVec[setdiff(getting, expe)]))
  }
  expect_true(isOK) #
}


expectedMessageRaw <- c("Running preP", "Preparing:", "File downloaded",
                        "From:Shapefile", "Checking local", "Finished checking",
                        "Downloading", "Skipping download", "Skipping extractFrom",
                        "targetFile was not.*Trying raster",
                        "Writing checksums.*you can specify targetFile",
                        "No targetFile supplied. Extracting", "Appending checksums")
expectedMessage <- paste0(collapse = "|", expectedMessageRaw)

expectedMessagePostProcessRaw <- c("cropping", "Checking for errors", "Found no errors",
                                   "intersecting", "masking")
expectedMessagePostProcess <- paste0(collapse = "|", expectedMessagePostProcessRaw)

urlTif1 <- "https://raw.githubusercontent.com/PredictiveEcology/quickPlot/master/inst/maps/DEM.tif"
urlShapefiles1Zip <- "https://drive.google.com/file/d/1Bk4SPz8rx8zziIlg2Yp9ELZmdNZytLqb/view?usp=sharing"
urlShapefilesZip <- "https://drive.google.com/file/d/1z1x0oI5jUDJQosOXacI8xbzbR15HFi0W/view?usp=sharing"

