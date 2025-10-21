all.equalWONewCache <- function(a, b) {
  attr(a, ".Cache")$newCache <- NULL
  attr(b, ".Cache")$newCache <- NULL
  all.equal(a, b)
}

skip_if_no_token <- function() {
  testthat::skip_if_not(googledrive::drive_has_token(), "No Drive token")
}

## NOTE: needs to be called after testInit("googledrive", needGoogleDriveAuth = TRUE)
skip_if_service_account <- function() {
  ## service accounts cannot upload to standard drive folders (no quota)
  testthat::skip_if_not(!grepl("gserviceaccount", googledrive::drive_user()$emailAddress),
                        message =  "Using service account")
}

## puts tmpdir, tmpCache, tmpfile (can be vectorized with length >1 tmpFileExt),
##   optsAsk in this environment,
## loads and libraries indicated plus testthat,
## sets options("reproducible.ask" = FALSE) if ask = FALSE
## if `needInternet = TRUE`, it will only re-try every 30 seconds
testInit <- function(libraries = character(), ask = FALSE, verbose, tmpFileExt = "",
                     opts = NULL, needGoogleDriveAuth = FALSE, needInternet = FALSE,
                     envir = parent.frame(1)) {
  set.randomseed()

  pf <- parent.frame()

  if (isTRUE(needGoogleDriveAuth)) {
    libraries <- c(libraries, "googledrive")
    needInternet <- TRUE
  }

  if (isTRUE(needInternet)) {
    if (!is.null(.pkgEnv$.internetExists)) {
      if (difftime(Sys.time(), .pkgEnv$.internetExistsLastCheck) > 30) {
        .pkgEnv$.internetExists <- NULL
        .pkgEnv$.internetExistsLastCheck <- NULL
      }
    }
    if (is.null(.pkgEnv$.internetExists)) {
      for (i in 1:2) {
        .pkgEnv$.internetExists <- internetExists()
        if (isTRUE(.pkgEnv$.internetExists))
          break
        SSL_REVOKE_BEST_EFFORT(envir)
      }
      .pkgEnv$.internetExistsLastCheck <- Sys.time()
    }
    intExists <- .pkgEnv$.internetExists
    if (!intExists) skip("Need internet")
  }

  if (length(libraries)) {
    libraries <- unique(libraries)
    loadedAlready <- vapply(libraries, function(pkg) {
      any(grepl(paste0("package:", pkg), search()))
    }, FUN.VALUE = logical(1))
    libraries <- libraries[!loadedAlready]

    if (length(libraries)) {
      pkgsLoaded <- unlist(lapply(libraries, requireNamespace, quietly = TRUE))
      if (!all(pkgsLoaded)) {
        lapply(libraries[!pkgsLoaded], skip_if_not_installed)
      }
      suppressWarnings(lapply(libraries, withr::local_package, .local_envir = pf))
    }
  }

  skip_gauth <- identical(Sys.getenv("SKIP_GAUTH"), "true") # only set in setup.R for covr
  if (isTRUE(needGoogleDriveAuth)) {
    if (isNamespaceLoaded("googledrive"))
      if ((!googledrive::drive_has_token())) {
        if (!nzchar(Sys.getenv("GOOGLEDRIVE_AUTH"))) {
          Sys.setenv("GOOGLEDRIVE_AUTH" = "~/genial-cycling-408722-788552a3ecac.json")
        }
        googledrive::drive_auth(path = Sys.getenv("GOOGLEDRIVE_AUTH"))
      }


    # if (!skip_gauth) {
    #   if (interactive()) {
    #     if (!googledrive::drive_has_token()) {
    #       getAuth <- FALSE
    #       if (is.null(getOption("gargle_oauth_email"))) {
    #         possLocalCache <- "c:/Eliot/.secret"
    #         cache <- if (file.exists(possLocalCache)) {
    #           possLocalCache
    #         } else {
    #           TRUE
    #         }
    #         switch(Sys.info()["user"],
    #                emcintir = {
    #                  options(gargle_oauth_email = "predictiveecology@gmail.com")
    #                }, # ,
    #                # gargle_oauth_cache = cache)},
    #                NULL
    #         )
    #       }
    #       if (is.null(getOption("gargle_oauth_email"))) {
    #         if (.isRstudioServer()) {
    #           .requireNamespace("httr", stopOnFALSE = TRUE)
    #           options(httr_oob_default = TRUE)
    #         }
    #       }
    #       getAuth <- TRUE
    #       if (isTRUE(getAuth)) {
    #         googledrive::drive_auth()
    #       }
    #     }
    #   }
    # }
    skip_if_no_token()
  }

  out <- list()

  if (isFALSE(getOption("reproducible.useCacheV3"))) {
    testthat::local_mocked_bindings(Cache = reproducible:::CacheV2, .env = pf)
    # withr::local_options("reproducible.useDBI" = FALSE, .local_envir = pf)
  }

  withr::local_options("reproducible.ask" = ask, .local_envir = pf)
  if (!missing(verbose)) {
    withr::local_options("reproducible.verbose" = verbose, .local_envir = pf)
  }
  if (!is.null(opts)) {
    withr::local_options(opts, .local_envir = pf)
  }
  tmpdir <- normPath(withr::local_tempdir(tmpdir = tempdir2(), .local_envir = pf))
  tmpCache <- normPath(withr::local_tempdir(tmpdir = tmpdir, .local_envir = pf))
  if (isTRUE(any(nzchar(tmpFileExt)))) {
    dotStart <- startsWith(tmpFileExt, ".")
    if (any(!dotStart)) {
      tmpFileExt[!dotStart] <- paste0(".", tmpFileExt)
    }
    out$tmpfile <- normPath(withr::local_tempfile(fileext = tmpFileExt))
  }
  withr::local_dir(tmpdir, .local_envir = pf)
  withr::defer({
    try(reproducible::clearCache(cachePath = tmpCache, ask = FALSE, verbose = -1))
    try(reproducible::clearCache(ask = FALSE, verbose = -1), silent = TRUE)
    try(unlink(tmpCache, recursive = TRUE))
  }, envir = pf)

  out <- append(out, list(tmpdir = tmpdir, tmpCache = tmpCache))
  list2env(out, envir = pf)
  return(out)

  #
  #   ################ BELOW HERE IS OLDER CODE THAT DOES NOT USE withr
  #
  #   tmpdir <- tempdir2(sprintf("%s_%03d", rndstr(1, 6), .pkgEnv$testCacheCounter))
  #   tmpCache <- checkPath(file.path(tmpdir, "testCache"), create = TRUE)
  #   .pkgEnv$testCacheCounter <- .pkgEnv$testCacheCounter + 1L
  #
  #   optsAsk <- if (!ask)
  #     options("reproducible.ask" = ask)
  #   else
  #     list()
  #
  #   optsVerbose <- if (verbose)
  #     options(reproducible.verbose = verbose)
  #   else
  #     list()
  #
  #   if (missing(libraries)) libraries <- list()
  #   if (length(libraries)) {
  #     pkgsLoaded <- unlist(lapply(libraries, requireNamespace, quietly = TRUE))
  #     if (!all(pkgsLoaded)) {
  #       lapply(libraries[!pkgsLoaded], skip_if_not_installed)
  #     }
  #     pf <- parent.frame()
  #     lapply(libraries, withr::local_package, .local_envir = pf)
  #   }
  #
  #   require("testthat", quietly = TRUE)
  #
  #   .pkgEnv <- getFromNamespace(".pkgEnv", "reproducible")
  #
  #   # Set a new seed each time
  #   if (isTRUE(needGoogleDriveAuth))
  #     skip_if_not_installed("googledrive")
  #
  #   skip_gauth <- identical(Sys.getenv("SKIP_GAUTH"), "true") # only set in setup.R for covr
  #   if (isTRUE(needGoogleDriveAuth) && !skip_gauth) {
  #     if (interactive()) {
  #       if (!googledrive::drive_has_token()) {
  #         getAuth <- FALSE
  #         if (is.null(getOption("gargle_oauth_email"))) {
  #           possLocalCache <- "c:/Eliot/.secret"
  #           cache <- if (file.exists(possLocalCache))
  #             possLocalCache else TRUE
  #           switch(Sys.info()["user"],
  #                  emcintir = {options(gargle_oauth_email = "eliotmcintire@gmail.com",
  #                                      gargle_oauth_cache = cache)},
  #                  NULL)
  #         }
  #         if (is.null(getOption("gargle_oauth_email"))) {
  #           if (.isRstudioServer()) {
  #             .requireNamespace("httr", stopOnFALSE = TRUE)
  #             options(httr_oob_default = TRUE)
  #           }
  #         }
  #         getAuth <- TRUE
  #         if (isTRUE(getAuth))
  #           googledrive::drive_auth()
  #       }
  #     }
  #     skip_if_no_token()
  #   }
  #
  #   origDir <- setwd(tmpdir)
  #
  #   defaultOpts <- list(
  #     reproducible.cachePath = .reproducibleTempCacheDir(), ## TODO: deal with cachePath issues in non-interactive tests
  #     reproducible.showSimilar = FALSE,
  #     reproducible.overwrite = TRUE,
  #     reproducible.cacheSpeed = "slow"
  #   )
  #   if (length(opts) > 0)
  #     defaultOpts[names(opts)] <- opts
  #   opts <- defaultOpts
  #
  #   if (!is.null(opts)) {
  #     if (needGoogleDriveAuth) {
  #       optsGoogle <- # if (utils::packageVersion("googledrive") >= "1.0.0") {
  #       # } else {
  #         list(httr_oob_default = .isRstudioServer())
  #       # }
  #       opts <- append(opts, optsGoogle)
  #     }
  #     opts <- lapply(opts, function(o) if (is.name(o)) eval(o, envir = environment()) else o)
  #     opts <- options(opts)
  #   }
  #
  #   if (!is.null(tmpFileExt) && any(nzchar(tmpFileExt))) {
  #     ranfiles <- unlist(lapply(tmpFileExt, function(x) paste0(rndstr(1, 7), ".", x)))
  #     tmpfile <- file.path(tmpdir, ranfiles)
  #     tmpfile <- gsub(pattern = "\\.\\.", tmpfile, replacement = "\\.")
  #     file.create(tmpfile)
  #     tmpfile <- normPath(tmpfile)
  #   } else {
  #     tmpfile <- NULL
  #   }
  #
  #   try(suppressMessages(clearCache(tmpCache, ask = FALSE)), silent = TRUE)
  #   try(suppressMessages(clearCache(tmpdir, ask = FALSE)), silent = TRUE)
  #
  #   outList <- list(tmpdir = tmpdir, origDir = origDir, libs = libraries,
  #                   tmpCache = tmpCache, optsAsk = optsAsk,
  #                   optsVerbose = optsVerbose, tmpfile = tmpfile,
  #                   opts = opts, needGoogleDriveAuth = needGoogleDriveAuth)
  #   list2env(outList, envir = pf)
  #   return(outList)
}

testOnExit <- function(testInitOut) {
  return()

  # if (length(testInitOut$optsVerbose))
  #   options("reproducible.verbose" = testInitOut$optsVerbose[[1]])
  # if (length(testInitOut$optsAsk))
  #   options("reproducible.ask" = testInitOut$optsAsk[[1]])
  # if (length(testInitOut$opts))
  #   options(testInitOut$opts)
  # setwd(testInitOut$origDir)
  # unlink(testInitOut$tmpdir, recursive = TRUE)
  # if (isTRUE(testInitOut$needGoogleDriveAuth)) {
  #   .requireNamespace("googledrive", stopOnFALSE = TRUE, messageStart = "to use google drive files")
  #   if (utils::packageVersion("googledrive") < "1.0.0")
  #     googledrive::drive_auth_config(active = FALSE)
  # }
  # unlink(testInitOut$tmpCache, recursive = TRUE, force = TRUE)
  # unlink(testInitOut$tmpdir, recursive = TRUE, force = TRUE)
  #
  # if (grepl("Pq", class(getOption("reproducible.conn", NULL)))) {
  #   tabs <- DBI::dbListTables(conn = getOption("reproducible.conn", NULL))
  #   tab1 <- grep(value = TRUE, tabs, pattern =
  #                  paste(collapse = "_", c(basename2(dirname(testInitOut$tmpCache)),
  #                                          basename2(testInitOut$tmpCache))))
  #   tab2 <- grep(value = TRUE, tabs, pattern =
  #                  paste(collapse = "_", c(basename2(dirname(testInitOut$tmpdir)),
  #                                          basename2(testInitOut$tmpdir))))
  #   if (length(tab1))
  #     try(DBI::dbRemoveTable(conn = getOption("reproducible.conn", NULL), tab1))
  #   if (length(tab2))
  #     try(DBI::dbRemoveTable(conn = getOption("reproducible.conn", NULL), tab2))
  # }
  #
}

runTest <- function(prod, class, numFiles, mess, expectedMess, filePattern, tmpdir, test) {
  files <- dir(tmpdir, pattern = filePattern, full.names = TRUE)
  expect_true(length(files) == numFiles)
  expect_true(inherits(test, class))
  # messagePrepInputs(mess)
  hasMessageNum <- paste(collapse = "_", which(unlist(
    lapply(strsplit(expectedMess, "\\|")[[1]], function(m) {
      any(grepl(m, mess))
    })
  )))

  isOK <- hasMessageNum == prod
  if (!isOK) {
    if (interactive()) {
      expe <- as.numeric(strsplit(prod, split = "_")[[1]])
      getting <- as.numeric(strsplit(hasMessageNum, split = "_")[[1]])

      expectedMessVec <- strsplit(expectedMess, split = "\\|")[[1]]
      expecting <- paste(collapse = ", ", expectedMessVec[setdiff(expe, getting)])
      if (length(expecting)) {
        cat("\nexpecting, but didn't get: ", expecting)
      }
      got <- paste(collapse = ", ", expectedMessVec[setdiff(getting, expe)])
      if (length(got)) {
        cat("\ngot, but didn't expect ", got, "\n")
      }
    }
  }
  expect_true(isOK) #
}

expectedMessageRaw <- c(
  "Running `preP", "Preparing:", "File downloaded",
  "From:.*Shapefile", "Checking local", "Finished checking",
  "Downloading", "Skipping download", "Skipping extractFrom",
  "targetFile was not.*ry",
  "Writing checksums.*you can specify targetFile",
  "No targetFile supplied. Checksumming", "Appending checksums", "although coordinates are longitude"
)
expectedMessage <- paste0(collapse = "|", expectedMessageRaw)

expectedMessagePostProcessRaw <- c(
  "cropping", "Checking for errors", "Found no errors",
  "intersecting", "masking", "although coordinates are longitude"
)
expectedMessagePostProcess <- paste0(collapse = "|", expectedMessagePostProcessRaw)

urlTif1 <- "https://raw.githubusercontent.com/PredictiveEcology/quickPlot/master/inst/maps/DEM.tif"
urlShapefiles1Zip <- "https://drive.google.com/file/d/1Bk4SPz8rx8zziIlg2Yp9ELZmdNZytLqb/view?usp=sharing"
urlShapefilesZip <- "https://drive.google.com/file/d/1z1x0oI5jUDJQosOXacI8xbzbR15HFi0W/view?usp=sharing"

targetFileLuxRDS <- "gadm36_LUX_0_sp.rds"

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

  tempFile <- replicate(14, tempfile(tmpdir = tmpdir, fileext = fileext))

  mc <- match.call()
  r1Orig <- terra::rast(terra::ext(0, 200, 0, 200), vals = 1, resolution = 1)
  r1Orig <- terra::writeRaster(r1Orig, filename = tempFile[1], overwrite = TRUE)

  if (mc$type == "Stack") {
    r1Orig2 <- terra::writeRaster(r1Orig, filename = tempFile[2], overwrite = TRUE)
    r1Orig <- c(r1Orig, r1Orig2)
  } else if (mc$type == "Brick") {
    message("Brick is deprecated; not tested any more")
  }

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
  r2Orig <- terra::rast(terra::ext(0, 200, 0, 200), vals = 1, resolution = 1)
  r2Orig <- terra::writeRaster(r2Orig, filename = tempFile[3], overwrite = TRUE)
  if (mc$type == "Stack") {
    r2Orig2 <- terra::writeRaster(r2Orig, filename = tempFile[4], overwrite = TRUE)
    r2Orig <- c(r2Orig, r2Orig2)
  } else if (mc$type == "Brick") {
    r1Orig2 <- r1Orig
    r1Orig <- c(r1Orig, r1Orig2)
    r1Orig <- terra::writeRaster(r1Orig, filename = tempFile[4], overwrite = TRUE)
  }

  # TODO for SpatRaster -- this returns the Path not SpatRaster
  r2End <- Cache(fn, r2Orig, useCloud = TRUE, cloudFolderID = cloudFolderID)
  cloudFolderID2 <- cloudFolderID
  on.exit({
    clearCache(useCloud = TRUE, cloudFolderID = cloudFolderID2)
  })

  expect_true(identical(unname(r1EndData), unname(r2End[])))
  expect_true(all.equal(r1EndFilename, as.character(Filenames(r2End)))) # this now has correct: only 1 downloaded copy exists
  expect_false(identical(Filenames(r2Orig), Filenames(r1Orig)))
  expect_true(r1EndCacheAttr == TRUE)
  expect_true(attr(r2End, ".Cache")$newCache == FALSE)
  filnames2End <- unique(
    dir(dirname(Filenames(r2End)),
        pattern = paste(collapse = "|", basename(filePathSansExt(Filenames(r2End))))
    )
  )
  filnames1End <- unique(
    dir(dirname(r1EndFilename),
        pattern = paste(collapse = "|", basename(filePathSansExt(r1EndFilename)))
    )
  )
  expect_true(NROW(filnames1End) == numRasterFiles) # both sets because of the _1 -- a bit of an artifact due to same folder
  expect_true(NROW(filnames2End) == numRasterFiles) # both sets because of the _1


  ####################################################
  # only local exists -- upload to cloud
  ####################################################
  clearCache(useCloud = TRUE, cloudFolderID = cloudFolderID)
  r1Orig <- terra::rast(terra::ext(0, 200, 0, 200), vals = 5, resolution = 1)
  r1Orig <- terra::writeRaster(r1Orig, filename = tempFile[5], overwrite = TRUE)
  if (mc$type == "Stack") {
    r1Orig2 <- terra::writeRaster(r1Orig, filename = tempFile[12], overwrite = TRUE)
    r1Orig <- c(r1Orig, r1Orig2)
  } else if (mc$type == "Brick") {
    r1Orig2 <- r1Orig
    r1Orig <- c(r1Orig, r1Orig2)
    r1Orig <- terra::writeRaster(r1Orig, filename = tempFile[12], overwrite = TRUE)
  }
  r1End <- Cache(fn, r1Orig, useCloud = FALSE, cloudFolderID = cloudFolderID)

  expect_true(attr(r1End, ".Cache")$newCache == TRUE) # new to local cache

  r4End <- Cache(fn, r1Orig, useCloud = TRUE, cloudFolderID = cloudFolderID)
  cloudFolderID3 <- cloudFolderID
  on.exit({
    clearCache(useCloud = TRUE, cloudFolderID = cloudFolderID3)
  })

  expect_true(attr(r4End, ".Cache")$newCache == FALSE) # new to local cache
  driveLs <- googledrive::drive_ls(cloudFolderID)
  data.table::setDT(driveLs)
  # expect_true(all(basename(Filenames(r4End)) %in% driveLs$name))
  # should have 2 files in cloud b/c of grd and gri
  # expect_true(sum(filePathSansExt(driveLs$name) %in% filePathSansExt(basename(Filenames(r4End)))) == numRasterFiles)
  # should have 1 file that matches in local and in cloud, based on cacheId
  suppressMessages(expect_true(NROW(unique(showCache(userTags = filePathSansExt(driveLs[endsWith(name, "rda")]$name)),
                                           by = .cacheTableHashColName()
  )) == 1))

  ####################################################
  # both cloud and local exist -- take local only -- no change to cloud
  ####################################################
  clearCache(useCloud = TRUE, cloudFolderID = cloudFolderID)
  r1Orig <- terra::rast(terra::ext(0, 200, 0, 200), vals = 5, resolution = 1)
  r1Orig <- terra::writeRaster(r1Orig, filename = tempFile[6], overwrite = TRUE)
  if (mc$type == "Stack") {
    r1Orig2 <- terra::writeRaster(r1Orig, filename = tempFile[13], overwrite = TRUE)
    r1Orig <- c(r1Orig, r1Orig2)
  } else if (mc$type == "Brick") {
    r1Orig2 <- r1Orig
    r1Orig <- c(r1Orig, r1Orig2)
    r1Orig <- terra::writeRaster(r1Orig, filename = tempFile[13], overwrite = TRUE)
  }
  r1End <- Cache(fn, r1Orig, useCloud = TRUE, cloudFolderID = cloudFolderID)
  on.exit({
    clearCache(useCloud = TRUE, cloudFolderID = cloudFolderID)
  })

  expect_true(attr(r1End, ".Cache")$newCache == TRUE) # new to local cache


  driveLsBefore <- googledrive::drive_ls(cloudFolderID)
  r5Orig <- terra::rast(terra::ext(0, 200, 0, 200), vals = 5, resolution = 1)
  r5Orig <- terra::writeRaster(r5Orig, filename = tempFile[9], overwrite = TRUE)
  if (mc$type == "Stack") {
    r5Orig2 <- terra::writeRaster(r5Orig, filename = tempFile[14], overwrite = TRUE)
    r5Orig <- c(r5Orig, r5Orig2)
  } else if (mc$type == "Brick") {
    r5Orig2 <- r5Orig
    r5Orig <- c(r5Orig, r5Orig2)
    r5Orig <- terra::writeRaster(r5Orig, filename = tempFile[14], overwrite = TRUE)
  }
  r5End <- Cache(fn, r5Orig, useCloud = TRUE, cloudFolderID = cloudFolderID)
  on.exit({
    clearCache(useCloud = TRUE, cloudFolderID = cloudFolderID)
  })
  expect_true(attr(r5End, ".Cache")$newCache == FALSE) # new to local cache
  driveLsAfter <- googledrive::drive_ls(cloudFolderID)
  expect_true(all.equal(driveLsAfter[, 1:2], driveLsBefore[, 1:2])) # There are differences deep in the drive_resources
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

.writeRaster <- function(...) {
  .requireNamespace("terra", stopOnFALSE = TRUE)
  suppressWarningsSpecific(
    falseWarnings = "NOT UPDATED FOR PROJ",
    terra::writeRaster(...)
  )
}

theRasterTests <- "https://github.com/tati-micheletti/host/raw/master/data/"
theRasterTestFilename <- function(pre = "", suff = "") {
  paste0(pre, "rasterTest.", suff)
}
theRasterTestZip <- theRasterTestFilename(theRasterTests, "zip") # "https://github.com/tati-micheletti/host/raw/master/data/rasterTest.zip"
theRasterTestRar <- theRasterTestFilename(theRasterTests, "rar") # "https://github.com/tati-micheletti/host/raw/master/data/rasterTest.rar"
theRasterTestTar <- theRasterTestFilename(theRasterTests, "tar") # "https://github.com/tati-micheletti/host/raw/master/data/rasterTest.tar"


runTestsWithTimings <- function(nameOfOuterList = "ff", envir = parent.frame(), authorizeGoogle = FALSE) {
  if (isTRUE(authorizeGoogle)) {
    testInit(needGoogleDriveAuth = TRUE)
  }
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
                              idcol = "TestFile"
  )
  gg[, TestFile := basename(TestFile)]
  gg
}

expect_match_noSlashN <- function(object, regexp, ...) {
  object <- gsub("  ", " ", gsub("\\n", "", messageStripColor(object)))
  expect_match(object, regexp, ...)

}

googleSetupForUseCloud <- function(cloudFolderID, tmpdir, tmpCache) {
  testsForPkgs <- "testsForPkgs"
  if (isTRUE(tryCatch(googledrive::drive_ls(testsForPkgs), error = function(e) TRUE))) {
    testsForPkgsDir <- retry(quote(googledrive::drive_mkdir(name = testsForPkgs)))
    on.exit2(googledrive::drive_rm(testsForPkgsDir))
  }
  on.exit2({
    try(googledrive::drive_rm(testsForPkgsDir), silent = TRUE)
    try(googledrive::drive_rm(cloudFolderID), silent = TRUE)
    try(googledrive::drive_rm(cloudFolderFromCacheRepo(tmpdir)), silent = TRUE)
    try(googledrive::drive_rm(cloudFolderFromCacheRepo(tmpCache)), silent = TRUE)
  })
}
