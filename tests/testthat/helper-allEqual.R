all.equalWONewCache <- function(a, b) {
  attr(a, ".Cache")$newCache <- NULL
  attr(b, ".Cache")$newCache <- NULL
  all.equal(a,b)
}

skip_if_no_token <- function() {
  testthat::skip_if_not(googledrive::drive_has_token(), "No Drive token")
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

  assign(value = "testsForPkgs",  "testsForPkgs", envir = .pkgEnv)

  optsVerbose <- if (verbose)
    options(reproducible.verbose = verbose)
  else
    list()

  if (missing(libraries)) libraries <- list()
  unlist(lapply(libraries, require, character.only = TRUE, quietly = TRUE))
  require("testthat", quietly = TRUE)
  tmpdir <- .reproducibleTempPath(rndstr(1, 6))

  if (isTRUE(needGoogle)) {
    if (!requireNamespace("googledrive"))
      stop(requireNamespaceMsg("googledrive", "to use google drive files"))

    if (!utils::packageVersion("googledrive") >= "1.0.0")
      # googledrive::drive_deauth()
    #else
      googledrive::drive_auth_config(active = TRUE)

    if (.isRstudioServer()) {
      .requireNamespace("httr", stopOnFALSE = TRUE)
      options(httr_oob_default = TRUE)
    }

    ## #119 changed use of .httr-oauth (i.e., no longer used)
    ## instead, uses ~/.R/gargle/gargle-oauth/long_random_token_name_with_email
    if (interactive()) {
      options(gargle_oauth_cache = ".secret")
      if (utils::packageVersion("googledrive") >= "1.0.0") {
        # googledrive::drive_deauth()
      } else {
        if (file.exists("~/.httr-oauth")) {
          linkOrCopy("~/.httr-oauth", to = file.path(tmpdir, ".httr-oauth"))
        } else {
          googledrive::drive_auth()
          messagePrepInputs("copying .httr-oauth to ~/.httr-oauth")
          file.copy(".httr-oauth", "~/.httr-oauth", overwrite = TRUE)
        }

        if (!file.exists("~/.httr-oauth"))
          messagePrepInputs("Please put an .httr-oauth file in your ~ directory")
      }
    }
  }
  checkPath(tmpdir, create = TRUE)
  origDir <- setwd(tmpdir)
  tmpCache <- normPath(file.path(tmpdir, "testCache"))
  checkPath(tmpCache, create = TRUE)

  defaultOpts <- list(
    reproducible.cachePath = .reproducibleTempCacheDir(), ## TODO: deal with cachePath issues in non-interactive tests
    reproducible.showSimilar = FALSE,
    reproducible.overwrite = TRUE,
    reproducible.useNewDigestAlgorithm = 2,
    reproducible.cacheSpeed = "slow"
  )
  if (length(opts) > 0)
    defaultOpts[names(opts)] <- opts
  opts <- defaultOpts

  if (!is.null(opts)) {
    if (needGoogle) {
      optsGoogle <- if (utils::packageVersion("googledrive") >= "1.0.0") {
      } else {
        list(httr_oob_default = .isRstudioServer())
      }
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
  rm(list = "testsForPkgs", envir = .pkgEnv)
  if (length(testInitOut$optsVerbose))
    options("reproducible.verbose" = testInitOut$optsVerbose[[1]])
  if (length(testInitOut$optsAsk))
    options("reproducible.ask" = testInitOut$optsAsk[[1]])
  if (length(testInitOut$opts))
    options(testInitOut$opts)
  setwd(testInitOut$origDir)
  unlink(testInitOut$tmpdir, recursive = TRUE)
  if (isTRUE(testInitOut$needGoogle)) {
    if (!requireNamespace("googledrive")) stop(requireNamespaceMsg("googledrive", "to use google drive files"))
    if (utils::packageVersion("googledrive") < "1.0.0")
      googledrive::drive_auth_config(active = FALSE)
  }
  unlink(testInitOut$tmpCache, recursive = TRUE, force = TRUE)
  unlink(testInitOut$tmpdir, recursive = TRUE, force = TRUE)

  if (grepl("Pq", class(getOption("reproducible.conn", NULL)))) {
    tabs <- DBI::dbListTables(conn = getOption("reproducible.conn", NULL))
    tab1 <- grep(value = TRUE, tabs, pattern =
                   paste(collapse = "_", c(basename2(dirname(testInitOut$tmpCache)),
                                           basename2(testInitOut$tmpCache))))
    tab2 <- grep(value = TRUE, tabs, pattern =
                   paste(collapse = "_", c(basename2(dirname(testInitOut$tmpdir)),
                                           basename2(testInitOut$tmpdir))))
    if (length(tab1))
      try(DBI::dbRemoveTable(conn = getOption("reproducible.conn", NULL), tab1))
    if (length(tab2))
      try(DBI::dbRemoveTable(conn = getOption("reproducible.conn", NULL), tab2))
  }

  lapply(testInitOut$libs, function(lib) {
    try(detach(paste0("package:", lib), character.only = TRUE), silent = TRUE)}
  )
}

runTest <- function(prod, class, numFiles, mess, expectedMess, filePattern, tmpdir, test) {
  files <- dir(tmpdir, pattern = filePattern, full.names = TRUE)
  expect_true(length(files) == numFiles)
  expect_true(inherits(test, class))
  messagePrepInputs(mess)
  hasMessageNum <- paste(collapse = "_", which(unlist(
    lapply(strsplit(expectedMess, "\\|")[[1]], function(m)
      any(grepl(m, mess)))
  )))

  isOK <- hasMessageNum == prod
  if (!isOK) {
    expe <- as.numeric(strsplit(prod, split = "_")[[1]])
    getting <- as.numeric(strsplit(hasMessageNum, split = "_")[[1]])

    expectedMessVec <- strsplit(expectedMess, split = "\\|")[[1]]
    messagePrepInputs("expecting, but didn't get ", paste(collapse = ", ", expectedMessVec[setdiff(expe, getting)]))
    messagePrepInputs("got, but didn't expect ", paste(collapse = ", ", expectedMessVec[setdiff(getting, expe)]))
  }
  expect_true(isOK) #
}

expectedMessageRaw <- c("Running preP", "Preparing:", "File downloaded",
                        "From:Shapefile", "Checking local", "Finished checking",
                        "Downloading", "Skipping download", "Skipping extractFrom",
                        "targetFile was not.*ry",
                        "Writing checksums.*you can specify targetFile",
                        "No targetFile supplied. Extracting", "Appending checksums", "although coordinates are longitude")
expectedMessage <- paste0(collapse = "|", expectedMessageRaw)

expectedMessagePostProcessRaw <- c("cropping", "Checking for errors", "Found no errors",
                                   "intersecting", "masking", "although coordinates are longitude")
expectedMessagePostProcess <- paste0(collapse = "|", expectedMessagePostProcessRaw)

urlTif1 <- "https://raw.githubusercontent.com/PredictiveEcology/quickPlot/master/inst/maps/DEM.tif"
urlShapefiles1Zip <- "https://drive.google.com/file/d/1Bk4SPz8rx8zziIlg2Yp9ELZmdNZytLqb/view?usp=sharing"
urlShapefilesZip <- "https://drive.google.com/file/d/1z1x0oI5jUDJQosOXacI8xbzbR15HFi0W/view?usp=sharing"

### Raster package function getData is failing for GADM objects because that site seems to have changed its url
#targetFileLuxRDS <- "GADM_3.6_LUX_adm0.rds"
targetFileLuxRDS <- "gadm36_LUX_0_sp.rds"

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
        messagePrepInputs("\nCould not download file -- perhaps it does not exist")
      }
    }
    else {
      messagePrepInputs("File not available locally. Use 'download = TRUE'")
    }
  }
  if (file.exists(filename)) {
    if (version == 2) {
      thisenvir <- new.env(parent = emptyenv())
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

testRasterInCloud <- function(fileext, cloudFolderID, numRasterFiles, tmpdir, type = c("Raster", "Stack", "Brick")) {
  if (!requireNamespace("googledrive")) stop(requireNamespaceMsg("googledrive", "to use google drive files"))

  # Second test .grd which has two files
  ####################################################
  # neither cloud or local exist -- should create local and upload to cloud
  ####################################################
  fn <- function(raster) {
    return(raster)
  }

  mc <- match.call()
  r1Orig <- raster(extent(0,200, 0, 200), vals = 1, res = 1)
  r1Orig <- writeRaster(r1Orig, filename = tempfile(tmpdir = tmpdir, fileext = fileext), overwrite = TRUE)
  r2Orig <- raster(extent(0,200, 0, 200), vals = 1, res = 1)
  r2Orig <- writeRaster(r2Orig, filename = tempfile(tmpdir = tmpdir, fileext = fileext), overwrite = TRUE)
  names(r1Orig) <- "layer1"
  names(r2Orig) <- "layer1"


  if (mc$type == "Stack") {
    r1Orig2 <- writeRaster(r1Orig, filename = tempfile(tmpdir = tmpdir, fileext = fileext), overwrite = TRUE)
    r1Orig <- stack(r1Orig, r1Orig2)
  } else if (mc$type == "Brick") {
    r1Orig2 <- r1Orig
    r1Orig <- brick(r1Orig, r1Orig2)
    r1Orig <- writeRaster(r1Orig, filename = tempfile(tmpdir = tmpdir, fileext = fileext), overwrite = TRUE)
  }

  # ._clearCache_3 <<- ._cloudUpload_1 <<- ._cloudDownloadRasterBackend_1 <<- 1
  r1End <- Cache(fn, r1Orig, useCloud = TRUE, cloudFolderID = cloudFolderID)
  cloudFolderID1 <- cloudFolderID
  on.exit({
    clearCache(useCloud = TRUE, cloudFolderID = cloudFolderID1)
  })

  r1EndData <- r1End[]
  r1EndFilename <- Filenames(r1End)
  r1EndCacheAttr <- attr(r1End, ".Cache")$newCache

  # Clear local copy
  rm(r1End)
  clearCache()
  ####################################################
  # cloud copy exists only -- should download to local copy
  ####################################################
  if (mc$type == "Stack") {
    r2Orig2 <- writeRaster(r2Orig, filename = tempfile(tmpdir = tmpdir, fileext = fileext), overwrite = TRUE)
    r2Orig <- stack(r2Orig, r2Orig2)
  } else if (mc$type == "Brick") {
    r2Orig2 <- r2Orig
    r2Orig <- brick(r2Orig, r2Orig2)
    r2Orig <- writeRaster(r2Orig, filename = tempfile(tmpdir = tmpdir, fileext = fileext), overwrite = TRUE)
  }
  # ._clearCache_3 <<- ._cloudUpload_1 <<- ._cloudDownloadRasterBackend_1 <<- 1
  r2End <- Cache(fn, r2Orig, useCloud = TRUE, cloudFolderID = cloudFolderID)
  cloudFolderID2 <- cloudFolderID
  on.exit({
    clearCache(useCloud = TRUE, cloudFolderID = cloudFolderID2)
  })

  expect_true(identical(unname(r1EndData), unname(r2End[])))
  expect_true(identical(r1EndFilename, Filenames(r2End))) # this now has correct: only 1 downloaded copy exists
  expect_false(identical(Filenames(r2Orig), Filenames(r1Orig)))
  expect_true(r1EndCacheAttr == TRUE)
  expect_true(attr(r2End, ".Cache")$newCache == FALSE)
  filnames2End <- unique(dir(dirname(Filenames(r2End)), pattern = paste(collapse = "|", basename(filePathSansExt(Filenames(r2End))))))
  filnames1End <- unique(dir(dirname(r1EndFilename), pattern = paste(collapse = "|", basename(filePathSansExt(r1EndFilename)))))
  expect_true(NROW(filnames1End) == numRasterFiles) # both sets because of the _1 -- a bit of an artifact due to same folder
  expect_true(NROW(filnames2End) == numRasterFiles) # both sets because of the _1


  ####################################################
  # only local exists -- upload to cloud
  ####################################################
  clearCache(useCloud = TRUE, cloudFolderID = cloudFolderID)
  r1Orig <- raster(extent(0,200, 0, 200), vals = 5, res = 1)
  r1Orig <- writeRaster(r1Orig, filename = tempfile(tmpdir = tmpdir, fileext = fileext), overwrite = TRUE)
  if (mc$type == "Stack") {
    r1Orig2 <- writeRaster(r1Orig, filename = tempfile(tmpdir = tmpdir, fileext = fileext), overwrite = TRUE)
    r1Orig <- stack(r1Orig, r1Orig2)
  } else if (mc$type == "Brick") {
    r1Orig2 <- r1Orig
    r1Orig <- brick(r1Orig, r1Orig2)
    r1Orig <- writeRaster(r1Orig, filename = tempfile(tmpdir = tmpdir, fileext = fileext), overwrite = TRUE)
  }
  r1End <- Cache(fn, r1Orig, useCloud = FALSE, cloudFolderID = cloudFolderID)

  expect_true(attr(r1End, ".Cache")$newCache == TRUE) # new to local cache

  r4End <- Cache(fn, r1Orig, useCloud = TRUE, cloudFolderID = cloudFolderID)
  cloudFolderID3 <- cloudFolderID
  on.exit({
    clearCache(useCloud = TRUE, cloudFolderID = cloudFolderID3)
  })

  expect_true(attr(r4End, ".Cache")$newCache == FALSE) # new to local cache
  driveLs <- drive_ls(cloudFolderID)
  data.table::setDT(driveLs)
  expect_true(all(basename(Filenames(r4End)) %in% driveLs$name))
  # should have 2 files in cloud b/c of grd and gri
  expect_true(sum(filePathSansExt(driveLs$name) %in% filePathSansExt(basename(Filenames(r4End)))) == numRasterFiles)
  # should have 1 file that matches in local and in cloud, based on cacheId
  suppressMessages(expect_true(NROW(unique(showCache(userTags = filePathSansExt(driveLs[endsWith(name, "rda")]$name)),
                                           by = .cacheTableHashColName()))==1))

  ####################################################
  # both cloud and local exist -- take local only -- no change to cloud
  ####################################################
  clearCache(useCloud = TRUE, cloudFolderID = cloudFolderID)
  r1Orig <- raster(extent(0,200, 0, 200), vals = 5, res = 1)
  r1Orig <- writeRaster(r1Orig, filename = tempfile(tmpdir = tmpdir, fileext = fileext), overwrite = TRUE)
  if (mc$type == "Stack") {
    r1Orig2 <- writeRaster(r1Orig, filename = tempfile(tmpdir = tmpdir, fileext = fileext), overwrite = TRUE)
    r1Orig <- stack(r1Orig, r1Orig2)
  } else if (mc$type == "Brick") {
    r1Orig2 <- r1Orig
    r1Orig <- brick(r1Orig, r1Orig2)
    r1Orig <- writeRaster(r1Orig, filename = tempfile(tmpdir = tmpdir, fileext = fileext), overwrite = TRUE)
  }
  r1End <- Cache(fn, r1Orig, useCloud = TRUE, cloudFolderID = cloudFolderID)
  on.exit({
    clearCache(useCloud = TRUE, cloudFolderID = cloudFolderID)
  })

  expect_true(attr(r1End, ".Cache")$newCache == TRUE) # new to local cache


  driveLsBefore <- googledrive::drive_ls(cloudFolderID)
  r5Orig <- raster(extent(0,200, 0, 200), vals = 5, res = 1)
  r5Orig <- writeRaster(r5Orig, filename = tempfile(tmpdir = tmpdir, fileext = fileext), overwrite = TRUE)
  if (mc$type == "Stack") {
    r5Orig2 <- writeRaster(r5Orig, filename = tempfile(tmpdir = tmpdir, fileext = fileext), overwrite = TRUE)
    r5Orig <- stack(r5Orig, r5Orig2)
  } else if (mc$type == "Brick") {
    r5Orig2 <- r5Orig
    r5Orig <- brick(r5Orig, r5Orig2)
    r5Orig <- writeRaster(r5Orig, filename = tempfile(tmpdir = tmpdir, fileext = fileext), overwrite = TRUE)
  }
  r5End <- Cache(fn, r5Orig, useCloud = TRUE, cloudFolderID = cloudFolderID)
  on.exit({
    clearCache(useCloud = TRUE, cloudFolderID = cloudFolderID)
  })
  expect_true(attr(r5End, ".Cache")$newCache == FALSE) # new to local cache
  driveLsAfter <- googledrive::drive_ls(cloudFolderID)
  expect_true(identical(driveLsAfter, driveLsBefore))
  clearCache(useCloud = TRUE, cloudFolderID = cloudFolderID)
  driveLsEnd <- googledrive::drive_ls(cloudFolderID)
  expect_true(NROW(driveLsEnd) == 0)
}

fnCacheHelper1 <- function() {
  1
}

fnCacheHelper <- function(a, cacheRepo2) {
  Cache(fnCacheHelper1, cacheRepo = cacheRepo2, verbose = 2)
}

crsToUse <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84"

messageNoCacheRepo <- "No cacheRepo supplied and getOption\\('reproducible.cachePath'\\) is inside"


.writeRaster <- function(...) {
  suppressWarningsSpecific(falseWarnings = "NOT UPDATED FOR PROJ",
                           writeRaster(...))
}

theRasterTests <- "https://github.com/tati-micheletti/host/raw/master/data/"
theRasterTestFilename <- function(pre = "", suff = "") {
  paste0(pre, "rasterTest.", suff)
}
theRasterTestZip <- theRasterTestFilename(theRasterTests, "zip") # "https://github.com/tati-micheletti/host/raw/master/data/rasterTest.zip"
theRasterTestRar <- theRasterTestFilename(theRasterTests, "rar") # "https://github.com/tati-micheletti/host/raw/master/data/rasterTest.rar"
theRasterTestTar <- theRasterTestFilename(theRasterTests, "tar")


shapefileClassDefault <- function() {
  shpfl <- if(is.character(getOption("reproducible.shapefileRead")))
    eval(parse(text = getOption("reproducible.shapefileRead")))
  else
    getOption("reproducible.shapefileRead")

  if (identical(shpfl, raster::shapefile)) "SpatialPolygons" else "sf"
}
