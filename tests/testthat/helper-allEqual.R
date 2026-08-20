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

## Run `expr` and convert transient upstream HTTP/network errors into a skip
## so flaky CDN behaviour (e.g. GitHub raw returning 5xx during a request)
## does not surface as a test FAIL. Non-transient errors propagate unchanged.
## Evaluated in the caller's frame so any assignments inside `expr` are
## visible to the rest of the test.
.transientNetworkPattern <- paste(
  "HTTP 5\\d\\d", "Bad Gateway", "Service Unavailable", "Gateway Timeout",
  "Could(?:n't| not) resolve host", "Empty reply from server",
  "Timeout was reached", "Operation timed out",
  "Connection reset by peer", "Connection refused",
  "TLS connect error", "SSL connect error",
  "Recv failure", "Resolving timed out",
  # httr2::req_perform_connection() (the streaming download path, .dlHttr2Stream)
  #   surfaces a transient connection failure with these messages, which the
  #   curl-style phrases above don't cover.
  "Failed to perform HTTP request", "cannot open the connection",
  sep = "|"
)

skip_on_transient_http <- function(expr) {
  expr <- substitute(expr)
  pf <- parent.frame()
  result <- tryCatch(eval(expr, envir = pf), error = identity)
  if (!inherits(result, "error")) return(invisible(result))
  msg <- paste(conditionMessage(result), collapse = "\n")
  if (grepl(.transientNetworkPattern, msg, perl = TRUE)) {
    testthat::skip(paste0("transient upstream HTTP/network error: ",
                          substring(msg, 1L, 240L)))
  }
  stop(result)
}

## Run `expr` and skip if any GDAL streaming-failure warning is emitted
## (truncated TIFF tile reads, IReadBlock failures, etc., or HTTP 5xx mid-
## stream). Returns the result of `expr` so the caller can chain assertions
## on it. Non-transient warnings are re-emitted so they remain visible in
## test output instead of being silently muffled.
.transientStreamPattern <- paste(
  "TIFFFillTile", "TIFFReadEncodedTile", "IReadBlock failed",
  "GDAL error", "HTTP 5\\d\\d", "Bad Gateway",
  "Service Unavailable", "Gateway Timeout",
  sep = "|"
)

## Warnings we know are harmless installation/config noise from GDAL/PROJ —
## emitted by terra/sf on systems where the PROJ database isn't on the
## default search path, but don't affect the result of operations that
## don't actually need CRS transforms. Muffled silently (no test skip,
## no re-emit) so they stop polluting CI output.
.benignGDALPattern <- paste(
  "PROJ: file is not a database",
  sep = "|"
)

skip_if_transient_stream_warnings <- function(expr) {
  expr <- substitute(expr)
  pf <- parent.frame()
  warns <- character()
  result <- withCallingHandlers(
    eval(expr, envir = pf),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  if (any(grepl(.transientStreamPattern, warns, perl = TRUE))) {
    testthat::skip(paste0("transient upstream streaming failure: ",
                          substring(paste(warns, collapse = "; "), 1L, 240L)))
  }
  warns <- warns[!grepl(.benignGDALPattern, warns, perl = TRUE)]
  for (w in warns) warning(w, call. = FALSE)
  invisible(result)
}

skip_if_service_account_releaseVer_NotLinux <- function() {
  ## Service accounts (e.g. eliot-githubauthentication@...gserviceaccount.com)
  ## have no Drive quota on user-owned folders, so they cannot complete the
  ## upload-and-roundtrip path these tests exercise. We gate those tests on:
  ##   (a) we're actually running unattended (CI or R CMD check non-interactive)
  ##       AND the token currently in use IS a service account, AND
  ##   (b) the runner is not the supported Linux/release combination.
  ##
  ## R_VERSION_LABEL is a CI-only signal the team's runners set to declare
  ## "I am a release R job". A local R CMD check won't have it set, and
  ## absence MUST NOT trigger a skip — only an explicit non-"release" value
  ## should. Without this guard, every local `R CMD check` against a stored
  ## service-account token skipped these tests for no good reason.

  on_ci    <- isTRUE(as.logical(Sys.getenv("CI", "false")))
  isAuto   <- !interactive() || on_ci
  isSA     <- isAuto && grepl(
                "gserviceaccount",
                googledrive::drive_user()$emailAddress)
  notLinux <- Sys.info()[["sysname"]] != "Linux"
  verLabel <- Sys.getenv("R_VERSION_LABEL")
  notRelease <- nzchar(verLabel) && verLabel != "release"

  skip <- isSA && (notLinux || notRelease)

  if (requireNamespace("covr", quietly = TRUE) && covr::in_covr()) {
    skip <- FALSE
  }
  testthat::skip_if(
    skip,
    "Service-account token + non-Linux or non-release R: skipping Drive upload tests"
  )
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

  if (!requireNamespace("withr"))
    skip("Need withr")

  if (length(libraries)) {
    libraries <- unique(libraries)
    if (identical(getOption("reproducible.rasterRead"), "raster::raster")) {
      libraries <- unique(c(libraries, "raster"))
    }
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
    if (isTRUE(skip_gauth))
      skip("SKIP_GAUTH=true; skipping Google Drive tests")
    if (isNamespaceLoaded("googledrive"))
      if ((!googledrive::drive_has_token())) {
        ## Prefer a real user OAuth token when one is available.
        ##
        ## A service account has no Drive quota on user-owned folders: it
        ## authenticates fine but cannot complete an upload round-trip, so the
        ## Drive tests start and then stop part-way. Nothing fails loudly --
        ## covr does not fail on test errors and prints no skip summary -- so
        ## those code paths just silently show 0 coverage (CacheGeo() sat at
        ## 0/176 lines this way). A user token carries the user's own quota.
        ##
        ## GDRIVE_OAUTH_TOKEN is a path to a serialized token, set by the
        ## reusable workflow when the GOOGLEDRIVE_AUTH secret holds one (see
        ## PredictiveEcology/actions); locally, point it at your own saveRDS'd
        ## token. Falls through to the service-account path below when unset or
        ## unreadable, so this is a no-op until the secret is switched over.
        oauthTokenFile <- Sys.getenv("GDRIVE_OAUTH_TOKEN")
        if (nzchar(oauthTokenFile) && file.exists(oauthTokenFile)) {
          tok <- tryCatch(readRDS(oauthTokenFile), error = function(e) {
            message("GDRIVE_OAUTH_TOKEN could not be read: ", conditionMessage(e))
            NULL
          })
          if (!is.null(tok)) {
            ## Drop the token's own cache_path before using it. drive_auth()
            ## writes the refreshed token back to that path, which is wherever
            ## the token was MINTED (e.g. ~/.secret on a dev machine). On a CI
            ## runner that directory does not exist, the write fails, and gargle
            ## surfaces it as the maximally unhelpful "Can't get Google
            ## credentials" -- indistinguishable from having no credential at
            ## all. A runner should not be persisting a credential to disk
            ## anyway, so this is the right behaviour regardless.
            tok$cache_path <- NULL
            tryCatch(googledrive::drive_auth(token = tok),
                     error = function(e)
                       message("GDRIVE_OAUTH_TOKEN was not usable: ", conditionMessage(e)))
          }
        }
      }
    if (isNamespaceLoaded("googledrive"))
      if ((!googledrive::drive_has_token())) {
        if (!nzchar(Sys.getenv("GOOGLEDRIVE_AUTH"))) {
          Sys.setenv("GOOGLEDRIVE_AUTH" = "~/genial-cycling-408722-788552a3ecac.json")
        }
        gauthEnv <- Sys.getenv("GOOGLEDRIVE_AUTH")
        if (nzchar(gauthEnv)) {
          ## drive_auth() -> gargle::credentials_service_account() accepts
          ## either a JSON file path or the JSON contents themselves
          ## (anything jsonlite::fromJSON accepts as `txt`), so pass the
          ## env var through as-is. On GHA the secret holds JSON content;
          ## locally it's typically a path.
          ##
          ## Service-account credentials can be revoked at Google's end
          ## (key rotated, SA disabled, etc.). drive_auth() converts the
          ## underlying HTTP 400 / invalid_grant into a generic "Can't get
          ## Google credentials" abort, which turns every Drive-using test
          ## into an ERROR instead of a SKIP. Swallow it so the
          ## skip_if_no_token() below cleanly skips instead.
          ## Report the reason rather than discarding it: a revoked or
          ## malformed credential is otherwise indistinguishable from "no
          ## credential configured", and the tests just skip in silence.
          tryCatch(googledrive::drive_auth(path = gauthEnv),
                   error = function(e)
                     message("GOOGLEDRIVE_AUTH was not usable: ", conditionMessage(e)))
        }
      }

    skip_if_no_token()
  }

  out <- list()

  withr::local_options("reproducible.ask" = ask, .local_envir = pf)
  ## Never block test runs waiting on `Type y if you have attempted a manual
  ## download...` — downloadFile() prompts when isInteractive() && this option
  ## is TRUE (its default). Override here for all tests that go through
  ## testInit(); individual tests can still re-enable via `opts`.
  withr::local_options(
    "reproducible.interactiveOnDownloadFail" = FALSE,
    .local_envir = pf
  )
  if (!missing(verbose)) {
    withr::local_options("reproducible.verbose" = verbose, .local_envir = pf)
  }
  if (!is.null(opts)) {
    withr::local_options(opts, .local_envir = pf)
  }
  tmpdir <- normPath(withr::local_tempdir(tmpdir = tempdir2(), .local_envir = pf))
  tmpCache <- normPath(withr::local_tempdir(tmpdir = tmpdir, .local_envir = pf))

  # can't figure out how to build and delete a temporary working directory with withr
  #  The test is `test-cluster.R` that fails if using
  # withr::local_dir(withr::local_tempdir(.local_envir = pf), .local_envir = pf)

  # BUT CRAN does not let you change the setwd during testing ... so, neither works
  # wd <- tempfile2() |> checkPath(create = TRUE)
  # od <- getwd()
  # withr::defer({setwd(od); unlink(wd, recursive = TRUE)}, envir = pf)
  # withr::local_dir(wd, .local_envir = pf)
  ###

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
  # withr::defer({
  #   browser()
  # })

  return(out)
}


runTest <- function(prod, class, numFiles, mess, expectedMess, filePattern, tmpdir, test) {
  files <- dir(tmpdir, pattern = filePattern, full.names = TRUE)
  if (length(files) != numFiles) {
    ## Use message() so the dump survives capture.output({...}) wrappers in the
    ## test body (capture.output redirects stdout, not stderr).
    message(sprintf(
      "[runTest] pattern=%s expected=%d got=%d tmpdir=%s",
      filePattern, numFiles, length(files), tmpdir
    ))
    message("  matching:    ", paste(basename(files), collapse = ", "))
    others <- setdiff(dir(tmpdir), basename(files))
    if (length(others))
      message("  also in dir: ", paste(others, collapse = ", "))
  }
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

theRasterTests <- "https://github.com/PredictiveEcology/reproducible/releases/download/v3.1.1/"
theRasterTestFilename <- function(pre = "", suff = "") {
  paste0(pre, "rasterTest.", suff)
}
theRasterTestZip <- theRasterTestFilename(theRasterTests, "zip") # "https://github.com/PredictiveEcology/reproducible/releases/download/v3.1.1/rasterTest.zip"
theRasterTestRar <- theRasterTestFilename(theRasterTests, "rar") # "https://github.com/PredictiveEcology/reproducible/releases/download/v3.1.1/rasterTest.rar"
theRasterTestTar <- theRasterTestFilename(theRasterTests, "tar") # "https://github.com/PredictiveEcology/reproducible/releases/download/v3.1.1/rasterTest.tar"


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

## Drive layout for the test suite:
##
##   testsForPkgs/                        <- stable parent, NEVER deleted
##     run-<GHA run>-<job>-<rnd>/         <- one per test SESSION, removed on teardown
##       <per-test working folders>
##
## The suite originally used "testsForPkgs" itself as the working root and
## deleted it wholesale on teardown. Local cachePaths are random, and so are the
## per-test working folders -- but their shared *parent* was not, so the first
## job to finish destroyed every concurrent job's workspace mid-run. With 8
## R-CMD-check matrix legs plus the coverage job all starting together, that
## surfaced as drive_rm() 404s ("File not found") and as uploads being skipped
## because another job had already put the object there.
##
## Session folders are nested rather than sitting at the Drive root, so the root
## does not accumulate one folder per run. Their names carry the GHA run and job
## when available, so anything left behind by a cancelled run (which cannot run
## its own teardown) is traceable; the random suffix keeps concurrent local runs
## apart too.

## The stable parent. Fetched if it exists, created once if not, and never
## removed -- deleting it is precisely the bug described above.
.cloudTestParent <- local({
  dir <- NULL
  function() {
    stillThere <- !is.null(dir) && isTRUE(tryCatch({
      googledrive::drive_get(id = dir$id); TRUE
    }, error = function(e) FALSE))
    if (!stillThere) {
      found <- tryCatch(googledrive::drive_get(path = "testsForPkgs/"),
                        error = function(e) NULL)
      dir <<- if (!is.null(found) && NROW(found) > 0) {
        ## Concurrent jobs can each create one before either sees the other;
        ## settling on the first keeps every job in the same place.
        found[1, ]
      } else {
        retry(quote(googledrive::drive_mkdir(name = "testsForPkgs")))
      }
    }
    dir
  }
})

.cloudTestRootName <- local({
  nm <- NULL
  function() {
    if (is.null(nm)) {
      tag <- paste(Filter(nzchar, c(Sys.getenv("GITHUB_RUN_ID"), Sys.getenv("GITHUB_JOB"))),
                   collapse = "-")
      nm <<- paste0("run-", if (nzchar(tag)) paste0(tag, "-") else "", rndstr(1, 6))
    }
    nm
  }
})

## This session's working folder, inside the stable parent.
##
## Self-healing on purpose: googleSetupForUseCloud() removes it in its teardown,
## so a later test_that() in the same session finds it gone. Without the
## existence check, drive_mkdir(path = <deleted folder>) fails, retries five
## times and reports "Failed after 5 attempts". (The old fixed-name code got
## this right by accident, re-creating "testsForPkgs" whenever it was missing.)
.cloudTestRoot <- local({
  dir <- NULL
  function() {
    stillThere <- !is.null(dir) && isTRUE(tryCatch({
      googledrive::drive_get(id = dir$id); TRUE
    }, error = function(e) FALSE))
    if (!stillThere) {
      dir <<- retry(quote(googledrive::drive_mkdir(name = .cloudTestRootName(),
                                                   path = .cloudTestParent())))
    }
    dir
  }
})

googleSetupForUseCloud <- function(cloudFolderID, tmpdir, tmpCache) {
  testsForPkgsDir <- .cloudTestRoot()
  on.exit2({
    ## Teardown is best-effort: removing this session's own root takes its
    ## subfolders with it, and a 404 here means something already went. Never
    ## fail a test on cleanup.
    try(googledrive::drive_rm(testsForPkgsDir), silent = TRUE)
    try(googledrive::drive_rm(cloudFolderID), silent = TRUE)
    try(googledrive::drive_rm(cloudFolderFromCacheRepo(tmpdir)), silent = TRUE)
    try(googledrive::drive_rm(cloudFolderFromCacheRepo(tmpCache)), silent = TRUE)
  })
}
