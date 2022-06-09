test_that("test Cache(useCloud=TRUE, ...)", {
  if (!requireNamespace("googledrive", quietly = TRUE))
    stop(requireNamespaceMsg("googledrive", "to use google drive files"))

  skip_if_no_token()
  if (interactive()) {
    testInitOut <- testInit(
      c("googledrive", "raster"), tmpFileExt = c(".tif", ".grd"),
      #needGoogle = TRUE,
      opts = list("reproducible.cachePath" = file.path(tempdir(), rndstr(1, 7)),
                  "reproducible.ask" = FALSE,
                  "reproducible.cloudFolderID" = "tmp_CacheForTesting")
    )
    clearCache(x = tmpCache, useCloud = TRUE)
    # if (packageVersion("googledrive") < "2.0.0") {
    #   df <- googledrive::drive_find(pattern = testsForPkgs, team_drive = NULL)
    # } else {
    #   df <- googledrive::drive_find(pattern = testsForPkgs, shared_drive = NULL)
    # }
    #if (NROW(df) == 0)
    testsForPkgsDir <- try(googledrive::drive_mkdir(name = .pkgEnv$testsForPkgs, overwrite = TRUE), silent = TRUE)
    on.exit({
      retry(quote(googledrive::drive_trash(.pkgEnv$testsForPkgs)))
      testOnExit(testInitOut)
    }, add = TRUE)

    newDir <- retry(quote(googledrive::drive_mkdir(name = rndstr(1, 6), path = .pkgEnv$testsForPkgs)))
    cloudFolderID = newDir

    messLoadedCached <- "loaded cached"
    messUploaded <- "Uploading"
    messDownload <- "Download"
    #######################################
    # local absent, cloud absent
    #######################################
    mess1 <- capture_messages({
      a1 <- Cache(rnorm, 1, cloudFolderID = cloudFolderID, cacheRepo = tmpCache, useCloud = TRUE)
    })
    expect_true(any(grepl(messUploaded, mess1)))

    #######################################
    # local present, cloud present
    #######################################
    mess2 <- capture_messages({
      a1 <- Cache(rnorm, 1, cloudFolderID = cloudFolderID, cacheRepo = tmpCache, useCloud = TRUE)
    })
    expect_true(any(grepl(messLoadedCached, mess2)))
    expect_false(all(grepl(messUploaded, mess2)))
    expect_false(all(grepl(messDownload, mess2)))

    #######################################
    # local absent, cloud present
    #######################################
    #kkkk <<- 1

    clearCache(userTags = .robustDigest(1), x = tmpCache, useCloud = FALSE)
    mess3 <- capture_messages({
      a1 <- Cache(rnorm, 1, cloudFolderID = cloudFolderID, cacheRepo = tmpCache, useCloud = TRUE)
    })
    expect_false(any(grepl(messLoadedCached, mess3)))
    expect_false(any(grepl(messUploaded, mess3)))
    expect_true(any(grepl(messDownload, mess3)))

    #######################################
    # local present, cloud absent
    #######################################
    clearCache(x = tmpCache, useCloud = TRUE, cloudFolderID = cloudFolderID)
    a1 <- Cache(rnorm, 2, cloudFolderID = cloudFolderID, cacheRepo = tmpCache)
    mess4 <- capture_messages({
      a2 <- Cache(rnorm, 2, cloudFolderID = cloudFolderID, cacheRepo = tmpCache, useCloud = TRUE)
    })

    expect_true(any(grepl(messLoadedCached, mess4)))
    expect_true(any(grepl(messUploaded, mess4)))
    expect_false(any(grepl(messDownload, mess4)))

    #######################################
    # cloudFolderID missing
    #######################################
    reproducible::clearCache(x = tmpCache, useCloud = TRUE, cloudFolderID = cloudFolderID)

    opts <- options("reproducible.cloudFolderID" = NULL)

    warn5 <- capture_warnings({
      mess5 <- capture_messages({
        a2 <- Cache(rnorm, 3, cacheRepo = tmpCache, useCloud = TRUE)
      })
    })

    expect_true(any(grepl(messUploaded, mess5)))
    expect_false(any(grepl(messDownload, mess5)))

    warn6 <- capture_warnings({
      mess6 <- capture_messages({
        a2 <- Cache(rnorm, 3, cacheRepo = tmpCache, useCloud = TRUE)
      })
    })

    expect_false(any(grepl("Folder created", mess6)))
    expect_false(any(grepl(messUploaded, mess6)))
    expect_false(any(grepl(messDownload, mess6)))
    expect_true(any(grepl(messLoadedCached, mess6)))
    expect_true(isTRUE(all.equal(length(warn6), 0)))

    ########
    retry(quote(googledrive::drive_trash(newDir))) # clear the original one
    cloudFolderID <- getOption("reproducible.cloudFolderID")
    clearCache(x = tmpCache, useCloud = TRUE)#, cloudFolderID = cloudFolderID)
    # Add 3 things to cloud and local -- then clear them all
    for (i in 1:3)
      a1 <- Cache(rnorm, i, cloudFolderID = cloudFolderID, cacheRepo = tmpCache, useCloud = TRUE)
    expect_silent({
      mess1 <- capture_messages(
        clearCache(x = tmpCache, useCloud = TRUE, cloudFolderID = cloudFolderID)
      )
    })
    expect_true(NROW(googledrive::drive_ls(path = cloudFolderFromCacheRepo(tmpCache))) == 0)

    # Add 3 things to local, only 2 to cloud -- clear them all, without an error
    for (i in 1:2)
      a1 <- Cache(rnorm, i, cloudFolderID = cloudFolderID, cacheRepo = tmpCache, useCloud = TRUE)
    a1 <- Cache(rnorm, 3, cloudFolderID = cloudFolderID, cacheRepo = tmpCache, useCloud = FALSE)
    expect_silent({
      mess2 <- capture_messages(
        clearCache(x = tmpCache, useCloud = TRUE, cloudFolderID = cloudFolderID)
      )
    })
    expect_true(NROW(googledrive::drive_ls(path = cloudFolderFromCacheRepo(tmpCache))) == 0)

    # Add 2 things to local and cloud -- clear only 1 of them, without an error
    Cache(rnorm, 1, cloudFolderID = cloudFolderID, cacheRepo = tmpCache, useCloud = TRUE)
    Cache(rnorm, 2, cloudFolderID = cloudFolderID, cacheRepo = tmpCache, useCloud = TRUE)
    expect_silent({
      mess2 <- capture_messages(
        clearCache(x = tmpCache, userTags = .robustDigest(1), useCloud = TRUE, cloudFolderID = cloudFolderID)
      )
    })

    gdriveLs <- googledrive::drive_ls(path = cloudFolderFromCacheRepo(tmpCache))
    expect_true(NROW(gdriveLs) == 1)
    expect_true(grepl(unique(showCache(tmpCache)[[.cacheTableHashColName()]]), gdriveLs$name))
    retry(quote(googledrive::drive_trash(getOption("reproducible.cloudFolderID"))))
  }
})

test_that("test Cache(useCloud=TRUE, ...) with raster-backed objs -- tif and grd", {
  skip_if_no_token()
  if (interactive()) {
    testInitOut <- testInit(c("googledrive", "raster"), tmpFileExt = c(".tif", ".grd"),
                            opts = list("reproducible.ask" = FALSE))

    opts <- options("reproducible.cachePath" = tmpdir)
    suppressWarnings(rm(list = "aaa", envir = .GlobalEnv))
    testsForPkgsDir <- try(googledrive::drive_mkdir(name = .pkgEnv$testsForPkgs, overwrite = FALSE), silent = TRUE)
    clearCache(x = tmpCache)
    clearCache(x = tmpdir)
    newDir <- retry(quote(googledrive::drive_mkdir(name = rndstr(1, 6), path = .pkgEnv$testsForPkgs)))
    on.exit({
      retry(quote(googledrive::drive_trash(googledrive::as_id(newDir$id))))
      options(opts)
      retry(quote(googledrive::drive_trash(.pkgEnv$testsForPkgs)))
      testOnExit(testInitOut)
    }, add = TRUE)

    # googledrive::drive_auth("predictiveecology@gmail.com")

    cloudFolderID = newDir

    testRasterInCloud(".tif", cloudFolderID = cloudFolderID, numRasterFiles = 1, tmpdir = tmpdir,
                      type = "Raster")

    retry(quote(googledrive::drive_trash(googledrive::as_id(newDir$id))))
    clearCache(x = tmpdir)
    newDir <- retry(quote(googledrive::drive_mkdir(rndstr(1,6), path = .pkgEnv$testsForPkgs)))
    cloudFolderID = newDir

    testRasterInCloud(".grd", cloudFolderID = cloudFolderID, numRasterFiles = 2, tmpdir = tmpdir,
                      type = "Raster")
  }
})

test_that("test Cache(useCloud=TRUE, ...) with raster-backed objs -- stack", {
  skip_if_no_token()
  if (interactive()) {
    testInitOut <- testInit(c("googledrive", "raster"), tmpFileExt = c(".tif", ".grd"),
                            opts = list("reproducible.ask" = FALSE))

    on.exit({
      try(googledrive::drive_trash(googledrive::as_id(newDir$id)), silent = TRUE)
      try(googledrive::drive_trash(testsForPkgsDir), silent = TRUE)
      options(opts)
      testOnExit(testInitOut)
    }, add = TRUE)
    clearCache(x = tmpCache)
    clearCache(x = tmpdir)
    testsForPkgsDir <- try(googledrive::drive_mkdir(name = .pkgEnv$testsForPkgs, overwrite = FALSE), silent = TRUE)
    newDir <- retry(quote(googledrive::drive_mkdir(name = rndstr(1, 6), path = .pkgEnv$testsForPkgs)))
    cloudFolderID = newDir
    testRasterInCloud(".tif", cloudFolderID = cloudFolderID, numRasterFiles = 2, tmpdir = tmpdir,
                      type = "Stack")
    try(googledrive::drive_trash(googledrive::as_id(newDir$id)), silent = TRUE)

    clearCache(x = tmpdir)
    newDir <- retry(quote(googledrive::drive_mkdir(name = rndstr(1, 6), path = .pkgEnv$testsForPkgs)))
    cloudFolderID = newDir
    testRasterInCloud(".grd", cloudFolderID = cloudFolderID, numRasterFiles = 4, tmpdir = tmpdir,
                      type = "Stack")
  }
})

test_that("test Cache(useCloud=TRUE, ...) with raster-backed objs -- brick", {
  skip_if_no_token()
  if (interactive()) {
    testInitOut <- testInit(c("googledrive", "raster"), tmpFileExt = c(".tif", ".grd"),
                            opts = list("reproducible.ask" = FALSE))

    opts <- options("reproducible.cachePath" = tmpdir)
    on.exit({
      try(googledrive::drive_trash(googledrive::as_id(newDir$id)), silent = TRUE)
      try(googledrive::drive_trash(testsForPkgsDir), silent = TRUE)
      options(opts)
      testOnExit(testInitOut)
    }, add = TRUE)
    clearCache(x = tmpCache)
    clearCache(x = tmpdir)
    testsForPkgsDir <- try(googledrive::drive_mkdir(name = .pkgEnv$testsForPkgs, overwrite = FALSE), silent = TRUE)

    newDir <- retry(quote(googledrive::drive_mkdir(name = rndstr(1, 6), path = .pkgEnv$testsForPkgs)))
    cloudFolderID = newDir
    testRasterInCloud(".tif", cloudFolderID = cloudFolderID, numRasterFiles = 1, tmpdir = tmpdir,
                      type = "Brick")
    try(googledrive::drive_trash(googledrive::as_id(newDir$id)), silent = TRUE)

    newDir <- retry(quote(googledrive::drive_mkdir(name = rndstr(1, 6), path = .pkgEnv$testsForPkgs)))
    cloudFolderID = newDir
    testRasterInCloud(".grd", cloudFolderID = cloudFolderID, numRasterFiles = 2, tmpdir = tmpdir,
                      type = "Brick")
    try(googledrive::drive_trash(googledrive::as_id(newDir$id)), silent = TRUE)

  }
})

test_that("prepInputs works with team drives", {
  if (!requireNamespace("googledrive", quietly = TRUE))
    stop(requireNamespaceMsg("googledrive", "to use google drive files"))

  skip_if_no_token()
  if (interactive()) {
    testInitOut <- testInit(
      "googledrive",
      opts = list("reproducible.cachePath" = file.path(tempdir(), rndstr(1, 7)),
                  "reproducible.ask" = FALSE)
    )
    # googledrive::drive_auth("predictiveecology@gmail.com")
    on.exit({
      testOnExit(testInitOut)
    }, add = TRUE)

    zipUrl <- "https://drive.google.com/file/d/1zRX2c55ebJbQtjijCErEfGxhsa7Ieph2"
    if (packageVersion("googledrive") < "2.0.0") {
      wb <- prepInputs(targetFile = "WB_BCR.shp", destinationPath = tmpdir, url = zipUrl,
                       alsoExtract = "similar", fun = "shapefile", team_drive = TRUE)
    } else {
      wb <- prepInputs(targetFile = "WB_BCR.shp", destinationPath = tmpdir, url = zipUrl,
                       alsoExtract = "similar", fun = "shapefile", shared_drive = TRUE)
    }
    expect_true(is(wb, "Spatial"))

  }
})

test_that("test Cache(useCloud=TRUE, ...) with shared drive", {
  skip_if_no_token()
  if (interactive()) {
    testInitOut <- testInit(c("googledrive", "raster"), tmpFileExt = c(".tif", ".grd"),
                            opts = list("reproducible.ask" = FALSE))

    clearCache(x = tmpdir)
    sharedDriveFolder <- googledrive::as_id("1vuXsTRma-vySEAvkofP8aTUqZhUVsxd0")
    testsForPkgsDir <- try(googledrive::drive_mkdir(sharedDriveFolder, overwrite = FALSE), silent = TRUE)

    # sharedDriveFolder <- as_id("1egHoGCBEV137aQroaz1yPFmYQ24enKdJ")
    newDir <- retry(quote(googledrive::drive_mkdir(name = rndstr(1, 6), path = sharedDriveFolder)))

    cloudFolderID = newDir

    opts <- options("reproducible.cachePath" = tmpdir, "reproducible.cloudFolderID" = cloudFolderID)
    on.exit({
      try(googledrive::drive_trash(googledrive::as_id(newDir$id)), silent = TRUE)
      # try(googledrive::drive_trash(testsForPkgsDir), silent = TRUE)
      options(opts)
      testOnExit(testInitOut)
    }, add = TRUE)

    a <- Cache(rnorm, 17, useCloud = TRUE, cloudFolderID = cloudFolderID)
    gdriveLs1 <- googledrive::drive_ls(cloudFolderID)

    b <- Cache(rnorm, 16, useCloud = TRUE, cloudFolderID = cloudFolderID)
    gdriveLs2 <- googledrive::drive_ls(cloudFolderID)

    d <- Cache(runif, 16, useCloud = TRUE, cloudFolderID = cloudFolderID)
    gdriveLs3 <- googledrive::drive_ls(cloudFolderID)

    expect_true(NROW(gdriveLs1) == 1)
    expect_true(NROW(gdriveLs2) == 2)
    expect_true(NROW(gdriveLs3) == 3)
    clearCache(useCloud = TRUE, cloudFolderID = cloudFolderID, userTags = "rnorm")
    gdriveLs4 <- googledrive::drive_ls(cloudFolderID)
    expect_true(NROW(gdriveLs4) == 1)

    clearCache(useCloud = TRUE, cloudFolderID = cloudFolderID)
    gdriveLs5 <- googledrive::drive_ls(cloudFolderID)
    expect_true(NROW(gdriveLs5) == 0)

    # Now try a raster

    fn <- function(seed) {
      ras <- raster::raster(extent(c(0, 10, 0, 10)), vals = sample(100))
      ras <- .writeRaster(ras, "hi.grd", overwrite = TRUE)
      return(ras)
    }

    rasOut <- Cache(fn, seed = 123, useCloud = TRUE, cloudFolderID = cloudFolderID)
    expect_true(attr(rasOut, ".Cache")$newCache)

    if (FALSE) {
      # 1. make sure reproducible is up to date
      # 2. run this here to get a character string to put in next line
      dput(as.character(cloudFolderID$id)) # copy this to next line ...
      # On a different machine -- run this:
      Require::Require("PredictiveEcology/reproducible@fst (> 1.2.9.9000)")
      cfid <- "1gPa1aeRF8e6nxZ-fIqY6qj3SJ_4Q2YxK"
      clearCache(useCloud = FALSE)
      fn <- function(seed) {
        ras <- raster::raster(extent(c(0, 10, 0, 10)), vals = sample(100))
        ras <- .writeRaster(ras, "hi.grd", overwrite = TRUE)
        return(ras)
      }
      rasOut <- Cache(fn, seed = 123, useCloud = TRUE, cloudFolderID = cfid)
      raster::plot(rasOut) # should show a raster with random colours
      Filenames(rasOut)    # should be length 2

    }


    # now move to a new folder so it isn't "getting it right" for the wrong reasons (i.e., files already in place)
    setwd(tempdir())
    clearCache(useCloud = FALSE)
    rasOut <- Cache(fn, seed = 123, useCloud = TRUE, cloudFolderID = cloudFolderID)
    expect_true(length(Filenames(rasOut)) == 2)
    expect_false(attr(rasOut, ".Cache")$newCache)

    try(googledrive::drive_trash(cloudFolderID))
  }


})


test_that("test individual useCloud family", {
  if (!requireNamespace("googledrive"))
    stop(requireNamespaceMsg("googledrive", "to use google drive files"))
  skip_if_no_token()
  testInitOut <- testInit("raster", tmpFileExt = c(".tif", ".grd"))
  on.exit({
    testOnExit(testInitOut)
    googledrive::drive_trash(googledrive::as_id(cloudFolderID))
    googledrive::drive_trash(googledrive::as_id(tmpCloudFolderID))
  }, add = TRUE)

  ras <- raster(extent(0,1,0,1), res  = 1, vals = 1)
  ras <- writeRaster(ras, file = tmpfile[1], overwrite = TRUE)

  gdriveLs1 <- data.frame(name = "GADM", id = "sdfsd", drive_resource = list(sdfsd = 1))
  expect_warning({
    tmpCloudFolderID <- cloudFolderID(create = TRUE)
  }, "No cloudFolderID supplied")
  gdriveLs <- cloudDriveLs(cloudFolderID = NULL, "sdfsdf")
  expect_true(NROW(gdriveLs) == 0)
  expect_is(cloudFolderID("testy"), "character")
  cloudFolderID <- cloudFolderID("testy", create = TRUE)
  # testthat::with_mock(
  #   "reproducible::retry" = function(..., retries = 1) TRUE,
  #   {
  #       mess1 <- capture_messages(expect_error(
  #         cloudUploadFromCache( isInRepo = data.frame(artifact = "sdfsdf"), outputHash = "sdfsiodfja",
  #                     gdriveLs = gdriveLs1, cacheRepo = tmpCache)))
  #   })
  # expect_true(grepl("Uploading local copy of", mess1))
  # expect_true(grepl("cacheId\\: sdfsiodfja to cloud folder", mess1))

  a <- cloudUploadRasterBackends(ras, cloudFolderID = cloudFolderID)
  mess1 <- capture_messages(expect_error(expect_warning({
    a <- cloudDownload(outputHash = "sdfsd", newFileName = "test.tif",
                       gdriveLs = gdriveLs1, cloudFolderID = "testy")
  })))
  expect_true(grepl("Downloading cloud copy of test\\.tif", mess1))
  testthat::with_mock(
    "reproducible::retry" = function(..., retries = 1) TRUE,
    {
      # cloudFolderID can't be meaningless "character", but retry is TRUE
      warns <- capture_warnings({
        err <- capture_error({
          cloudDownloadRasterBackend(output = ras, cacheRepo = tmpCache, cloudFolderID = "character")
        })
      })
      expect_true(is.null(err))
    })

  testthat::with_mock(
    "reproducible::retry" = function(..., retries = 1) TRUE,
    {
      mess1 <- capture_messages({
        err <- capture_error({
          cloudUpload(isInCloud = FALSE, outputHash = "sdsdfs", saved = "life",
                               cacheRepo = tmpCache)
        })
      })
      expect_true(all(grepl("cloudFolderID.*is missing, with no default", err)))
    })
  expect_true(grepl("Uploading new cached object|with cacheId", mess1))

  a <- new.env(parent = emptyenv())
  a$a = list(ras, ras)
  expect_true(all(unlist(isOrHasRaster(a))))
})
