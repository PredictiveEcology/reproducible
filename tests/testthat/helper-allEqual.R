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

  optsVerbose <- if (verbose)
    options(reproducible.verbose = verbose)
  else
    list()

  if (missing(libraries)) libraries <- list()
  unlist(lapply(libraries, require, character.only = TRUE, quietly = TRUE))
  require("testthat", quietly = TRUE)

  .pkgEnv <- getFromNamespace(".pkgEnv", "reproducible")
  tmpdir <- tempdir2(sprintf("%s_%03d", rndstr(1, 6), .pkgEnv$testCacheCounter))
  tmpCache <- checkPath(file.path(tmpdir, "testCache"), create = TRUE)
  .pkgEnv$testCacheCounter <- .pkgEnv$testCacheCounter + 1L


  if (isTRUE(needGoogle)) {
    .requireNamespace("googledrive", stopOnFALSE = TRUE, messageStart = "to use google drive files")

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

  origDir <- setwd(tmpdir)

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

  try(clearCache(tmpCache, ask = FALSE), silent = TRUE)
  try(clearCache(tmpdir, ask = FALSE), silent = TRUE)

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
  if (isTRUE(testInitOut$needGoogle)) {
    .requireNamespace("googledrive", stopOnFALSE = TRUE, messageStart = "to use google drive files")
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

  # lapply(testInitOut$libs, function(lib) {
  #   try(detach(paste0("package:", lib), character.only = TRUE), silent = TRUE)}
  # )
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


## TODO: switch to `geodata` package (raster::getData() is deprecated) (#256)
getDataFn <- function(...) {
  suppressWarningsSpecific({
    raster::getData(...)
  }, falseWarnings = "getData will be removed in a future version of raster")
}

testRasterInCloud <- function(fileext, cloudFolderID, numRasterFiles, tmpdir,
                              type = c("Raster", "Stack", "Brick")) {
  .requireNamespace("googledrive", stopOnFALSE = TRUE, messageStart = "to use google drive files")

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
  r2Orig <- raster(extent(0,200, 0, 200), vals = 1, res = 1)
  r2Orig <- writeRaster(r2Orig, filename = tempfile(tmpdir = tmpdir, fileext = fileext), overwrite = TRUE)
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
  Cache(fnCacheHelper1, cachePath = cacheRepo2, verbose = 2)
}

crsToUse <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84"

messageNoCacheRepo <- "No cachePath supplied and getOption\\('reproducible.cachePath'\\) is inside"


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
theRasterTestTar <- theRasterTestFilename(theRasterTests, "tar") # "https://github.com/tati-micheletti/host/raw/master/data/rasterTest.tar"


shapefileClassDefault <- function() {
  shpfl <- if (is.character(getOption("reproducible.shapefileRead"))) {
    eval(parse(text = getOption("reproducible.shapefileRead")))
  } else {
    getOption("reproducible.shapefileRead")
  }
  if (identical(shpfl, raster::shapefile)) "SpatialPolygons" else "sf"
}

rasterType <- function(nlayers = 1) {
  rasterRead <- getOption("reproducible.rasterRead")
  if (identical(rasterRead, "terra::rast"))
    "SpatRaster"
  else
    if (nlayers == 1) "RasterLayer" else "RasterStack"
}

runTestsWithTimings <- function(nameOfOuterList = "ff", envir = parent.frame(), authorizeGoogle = FALSE) {
  if (isTRUE(authorizeGoogle))
    if (Sys.info()[["user"]] == "emcintir")
      googledrive::drive_auth(cache = "~/.secret", email = "predictiveecology@gmail.com")
  prepend <- "/home/emcintir/GitHub/reproducible/tests/testthat"
  testFiles <- dir(prepend, pattern = "^test-", full.names = TRUE)
  testFiles <- grep("large", testFiles, value = TRUE, invert = TRUE)
  rrrr <- get(nameOfOuterList, envir = envir)
  testFiles <- setdiff(testFiles, file.path(prepend, names(rrrr)))
  for (tf in testFiles) {
    messageDF(colour = "blue", basename(tf))
    a <- parse(tf, keep.source = TRUE)
    labels <- unlist(lapply(a, function(x) x[[2]]))
    # Sys.setenv("NOT_CRAN" = "false") # doesn't work
    dd <- Map(testLabel = labels, parsed = a, function(parsed, testLabel) {
      message(testLabel)
      skipOnCran <- any(grepl("skip_on_cran", parsed[[3]]))
      start <- Sys.time()
      try(eval(parsed))
      end <- Sys.time()
      b <- difftime(end, start)
      print(format(b))
      data.table(elapsed = as.numeric(b), skipOnCRAN = skipOnCran)
    })
    ee <- data.table::rbindlist(dd, idcol = "Label")
    ee <- setNames(list(ee), basename(tf))
    rrrr <- append(rrrr, ee)
    assign(nameOfOuterList, rrrr, envir = envir)

    testFiles <- testFiles[-1]
  }

  gg <- data.table::rbindlist(get(nameOfOuterList, envir = envir),
                              idcol = "TestFile")
  gg[, TestFile := basename(TestFile)]
  gg
}

