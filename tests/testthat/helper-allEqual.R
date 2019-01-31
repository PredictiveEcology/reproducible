all.equalWONewCache <- function(a, b) {
  attr(a, ".Cache")$newCache <- NULL
  attr(b, ".Cache")$newCache <- NULL
  all.equal(a,b)
}

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
  tmpdir <- normPath(file.path(tempdir(), rndstr(1, 6)))

  if (isTRUE(needGoogle)) {
    googledrive::drive_auth_config(active = TRUE)

    if (interactive()) {
      if (file.exists("~/.httr-oauth")) {
        linkOrCopy("~/.httr-oauth", to = file.path(tmpdir, ".httr-oauth"))
      } else {
        googledrive::drive_auth()
        file.copy(".httr-oauth", "~/.httr-oauth")
      }
    }
    if (!file.exists("~/.httr-oauth")) message("Please put an .httr-oauth file in your ~ directory")

  }
  checkPath(tmpdir, create = TRUE)
  origDir <- setwd(tmpdir)
  tmpCache <- normPath(file.path(tmpdir, "testCache"))
  checkPath(tmpCache, create = TRUE)

  opts <- append(list(reproducible.overwrite = TRUE,
                      reproducible.useNewDigestAlgorithm = TRUE,
                      reproducible.cachePath = tmpCache), opts)

  if (!is.null(opts)) {
    if (needGoogle) {
      optsGoogle <- list(httr_oob_default = FALSE, httr_oauth_cache = "~/.httr-oauth")
      opts <- append(opts, optsGoogle)
    }
    opts <- options(opts)
  }

  if (!is.null(tmpFileExt)) {
    ranfiles <- unlist(lapply(tmpFileExt, function(x) paste0(rndstr(1, 7), ".", x)))
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
                  opts = opts, needGoogle = needGoogle)
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
  if (isTRUE(testInitOut$needGoogle))
    googledrive::drive_auth_config(active = FALSE)
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
                        "targetFile was not.*ry",
                        "Writing checksums.*you can specify targetFile",
                        "No targetFile supplied. Extracting", "Appending checksums")
expectedMessage <- paste0(collapse = "|", expectedMessageRaw)

expectedMessagePostProcessRaw <- c("cropping", "Checking for errors", "Found no errors",
                                   "intersecting", "masking")
expectedMessagePostProcess <- paste0(collapse = "|", expectedMessagePostProcessRaw)

urlTif1 <- "https://raw.githubusercontent.com/PredictiveEcology/quickPlot/master/inst/maps/DEM.tif"
urlShapefiles1Zip <- "https://drive.google.com/file/d/1Bk4SPz8rx8zziIlg2Yp9ELZmdNZytLqb/view?usp=sharing"
urlShapefilesZip <- "https://drive.google.com/file/d/1z1x0oI5jUDJQosOXacI8xbzbR15HFi0W/view?usp=sharing"

### Raster package function getData is failing for GADM objects because that site seems to have changed its url
targetFileLuxRDS <- "GADM_3.6_LUX_adm0.rds"

.GADMtmp <- function(country, level, download, path, version) {
  country <- raster:::.getCountry(country)
  if (missing(level)) {
    stop("provide a \"level=\" argument; levels can be 0, 1, or 2 for most countries, and higher for some")
  }
  filename <- paste(path, "GADM_", version, "_", country, "_adm",
                    level, ".rds", sep = "")
  if (!file.exists(filename)) {
    if (download) {
      baseurl <- paste0("https://biogeo.ucdavis.edu/data/gadm",version,"/Rsp/gadm36")
      #baseurl <- paste0("http://biogeo.ucdavis.edu/data/gadm",
      #    version)
      if (version == 2) {
        theurl <- paste(baseurl, "/R/", country, "_adm",
                        level, ".RData", sep = "")
      } else if (version == 3.6) {
        # https://biogeo.ucdavis.edu/data/gadm3.6/Rsp/gadm36_LUX_0_sp.rds
        theurl <- paste(baseurl, "_", country, "_",
                        level, "_sp.rds", sep = "")
      } else {
        theurl <- paste(baseurl, "/rds/", country, "_adm",
                        level, ".rds", sep = "")
      }
      raster:::.download(theurl, filename)
      if (!file.exists(filename)) {
        message("\nCould not download file -- perhaps it does not exist")
      }
    }
    else {
      message("File not available locally. Use 'download = TRUE'")
    }
  }
  if (file.exists(filename)) {
    if (version == 2) {
      thisenvir <- new.env()
      data <- get(load(filename, thisenvir), thisenvir)
    }
    else {
      data <- readRDS(filename)
    }
    return(data)
  }
  else {
    return(NULL)
  }
}

getDatatmp <- function(name = "GADM", download = TRUE, path = "", ...) {
  path <- raster:::.getDataPath(path)
  if (name == "GADM") {
    .GADMtmp(..., download = download, path = path, version = 3.6)
  }
  else if (name == "SRTM") {
    raster:::.SRTM(..., download = download, path = path)
  }
  else if (name == "alt") {
    raster:::.raster(..., name = name, download = download, path = path)
  }
  else if (name == "worldclim") {
    raster:::.worldclim(..., download = download, path = path)
  }
  else if (name == "CMIP5") {
    raster:::.cmip5(..., download = download, path = path)
  }
  else if (name == "ISO3") {
    raster:::ccodes()[, c(2, 1)]
  }
  else if (name == "countries") {
    raster:::.countries(download = download, path = path, ...)
  }
  else {
    stop(name, " not recognized as a valid name.")
  }
}

if (utils::packageVersion("raster") <= "2.6.7") {
  getDataFn <- getDatatmp
} else {
  getDataFn <- raster::getData
}
