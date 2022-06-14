options(reproducible.drv = "fst")
# options(reproducible.drv = RSQLite::SQLite())

test_that("test Cache(useCloud=TRUE, ...)", {
  if (!requireNamespace("googledrive", quietly = TRUE))
    stop(requireNamespaceMsg("googledrive", "to use google drive files"))

  skip_if_no_token()

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
    a1 <- Cache(rnorm(1), cloudFolderID = cloudFolderID, cacheRepo = tmpCache, useCloud = TRUE)
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
    a2 <- Cache(rnorm(2), cloudFolderID = cloudFolderID, cacheRepo = tmpCache, useCloud = TRUE)
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
      a2 <- Cache(rnorm(3), cacheRepo = tmpCache, useCloud = TRUE)
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
  expect_true(NROW(googledrive::drive_ls(path = cloudFolderID)) == 1) # cache.db

  # Add 3 things to local, only 2 to cloud -- clear them all, without an error
  for (i in 1:2)
    a1 <- Cache(rnorm(i), cloudFolderID = cloudFolderID, cacheRepo = tmpCache, useCloud = TRUE)
  a1 <- Cache(rnorm, 3, cloudFolderID = cloudFolderID, cacheRepo = tmpCache, useCloud = FALSE)
  expect_silent({
    mess2 <- capture_messages(
      clearCache(x = tmpCache, useCloud = TRUE, cloudFolderID = cloudFolderID)
    )
  })
  expect_true(NROW(googledrive::drive_ls(path = cloudFolderID)) == 1) # cache.db

  # Add 2 things to local and cloud -- clear only 1 of them, without an error
  Cache(rnorm, 1, cloudFolderID = cloudFolderID, cacheRepo = tmpCache, useCloud = TRUE)
  Cache(rnorm(2), cloudFolderID = cloudFolderID, cacheRepo = tmpCache, useCloud = TRUE)
  expect_silent({
    mess2 <- capture_messages(
      clearCache(x = tmpCache, userTags = .robustDigest(1), useCloud = TRUE, cloudFolderID = cloudFolderID)
    )
  })

  gdriveLs <- googledrive::drive_ls(path = cloudFolderID)
  expect_true(NROW(gdriveLs) == 2)
  expect_true(sum(grepl(unique(showCache(tmpCache)[[.cacheTableHashColName()]]), gdriveLs$name)) == 1)
  try(googledrive::drive_trash(getOption("reproducible.cloudFolderID")))

})

test_that("test Cache(useCloud=TRUE, ...) with raster-backed objs -- tif and grd", {
  skip_if_no_token()
    testInitOut <- testInit(c("googledrive", "raster"), tmpFileExt = c(".tif", ".grd"),
                            opts = list("reproducible.ask" = FALSE,
                                        "reproducible.inputPaths" = NULL))

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
})

test_that("test Cache(useCloud=TRUE, ...) with raster-backed objs -- stack", {
  skip_if_no_token()
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
})

test_that("test Cache(useCloud=TRUE, ...) with raster-backed objs -- brick", {
  skip_if_no_token()
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

})

test_that("prepInputs works with team drives", {
  if (!requireNamespace("googledrive", quietly = TRUE))
    stop(requireNamespaceMsg("googledrive", "to use google drive files"))

  skip_if_no_token()
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
      wb <- suppressWarningsSpecific(falseWarnings = "Discarded datum", prepInputs(targetFile = "WB_BCR.shp", destinationPath = tmpdir, url = zipUrl,
                       alsoExtract = "similar", fun = "shapefile", team_drive = TRUE)
      )
    } else {
      wb <- wb <- suppressWarningsSpecific(falseWarnings = "Discarded datum",
                                           prepInputs(targetFile = "WB_BCR.shp", destinationPath = tmpdir, url = zipUrl,
                       alsoExtract = "similar", fun = "shapefile", shared_drive = TRUE)
      )
    }
    expect_true(is(wb, "Spatial"))


})

test_that("test Cache(useCloud=TRUE, ...) with shared drive", {
  skip_if_no_token()
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

    expect_true(NROW(gdriveLs1) == 1 + 1)
    expect_true(NROW(gdriveLs2) == 2 + 1)
    expect_true(NROW(gdriveLs3) == 3 + 1)
    clearCache(useCloud = TRUE, cloudFolderID = cloudFolderID, userTags = "rnorm")
    gdriveLs4 <- googledrive::drive_ls(cloudFolderID)
    expect_true(NROW(gdriveLs4) == 1 + 1)

    clearCache(useCloud = TRUE, cloudFolderID = cloudFolderID)
    gdriveLs5 <- googledrive::drive_ls(cloudFolderID)
    expect_true(NROW(gdriveLs5) == 0 + 1)

    # Now try a raster

    fn <- function(seed) {
      ras <- raster::raster(extent(c(0, 10, 0, 10)), vals = sample(100))
      ras <- .writeRaster(ras, "hi.grd", overwrite = TRUE)
      return(ras)
    }

    rasOut <- Cache(fn, seed = 123, useCloud = TRUE, cloudFolderID = cloudFolderID)
    expect_true(attr(rasOut, ".Cache")$newCache)

    # now move to a new folder so it isn't "getting it right" for the wrong reasons (i.e., files already in place)
    setwd(tempdir())
    clearCache(useCloud = FALSE)
    rasOut <- Cache(fn, seed = 123, useCloud = TRUE, cloudFolderID = cloudFolderID)
    expect_true(length(Filenames(rasOut)) == 2)
    expect_false(attr(rasOut, ".Cache")$newCache)

    try(googledrive::drive_trash(cloudFolderID))


})



test_that("test keepCache(useCloud=TRUE, ...)", {
  skip_if_no_token()
  testInitOut <- testInit(c("googledrive", "raster"), tmpFileExt = c(".tif", ".grd"),
                          opts = list("reproducible.ask" = FALSE,
                                      "reproducible.cloudFolderID" = NULL))

  opts <- options("reproducible.cachePath" = tmpdir)
  on.exit({
    try(googledrive::drive_trash(cloudFolderID()), silent = TRUE)
    options(opts)
    testOnExit(testInitOut)
  }, add = TRUE)
  suppressWarnings(rm(aaa))
  clearCache(x = tmpCache)
  clearCache(x = tmpdir)

  cfid <- cloudFolderID(cacheRepo = tmpdir, create = TRUE) # sets option
  cloudClearCache(cfid)

  out1 <- Cache(rnorm, 2, useCloud = TRUE, cloudFolderID = cfid, cacheRepo = tmpdir)
  out2 <- Cache(rnorm, 3, useCloud = TRUE, cloudFolderID = cfid, cacheRepo = tmpdir)
  out3 <- Cache(runif, 4, useCloud = TRUE, cloudFolderID = cfid, cacheRepo = tmpdir)
  out4 <- Cache(runif, 5, useCloud = TRUE, cloudFolderID = cfid, cacheRepo = tmpCache)
  out5 <- Cache(rnorm, 6, useCloud = TRUE, cloudFolderID = cfid, cacheRepo = tmpCache)

  # The cloud used 2 different local caches; the cloud db file has all info
  scAll <- cloudShowCache(cfid)
  sctmpdir <- showCache(tmpdir)
  sctmpCache <- showCache(tmpCache)
  scLocal <- setkeyv(data.table::rbindlist(list(sctmpdir, sctmpCache)), colnames(scAll))
  expect_true(all.equal(data.table::setkey(scAll[, -"createdDate"]),
                        data.table::setkey(scLocal[, -"createdDate"])))


  # This will only remove the objects that are *in* tmpdir
  out6 <- keepCache(userTags = "runif", x = tmpdir, cloudFolderID = cfid, useCloud = TRUE)
  expect_true(length(unique(out6$cacheId)) == 1)
  sc <- cloudShowCache(cfid)
  expect_true(NROW(unique(sc$cacheId)) == 3) # the 1 runif from tmpdir + 2 from tmpCache

  out7 <- keepCache(userTags = "runif", x = tmpCache, cloudFolderID = cfid, useCloud = TRUE)
  expect_true(length(unique(out7$cacheId)) == 1)
  sc1 <- cloudShowCache(cfid)
  expect_true(NROW(unique(sc1$cacheId)) == 2) # the 1 runif from tmpdir + 1 unif from tmpCache

  out8 <- clearCache(userTags = "runif", x = tmpCache, cloudFolderID = cfid, useCloud = TRUE)
  expect_true(length(unique(out8$cacheId)) == 1)
  sc2 <- cloudShowCache(cfid)
  expect_true(NROW(unique(sc2$cacheId)) == 1) # the 1 runif from tmpdir


  out9 <- Cache(rnorm, 7, useCloud = TRUE, cloudFolderID = cfid, cacheRepo = tmpdir)
  sc3 <- cloudShowCache(cfid)
  expect_true(NROW(unique(sc3$cacheId)) == 2) # the 1 runif from tmpdir + rnorm in tmpdir

  # Delete only cloud --> this will cause local and cloud to be different
  # aaa <<- 1
  ss <- cloudClearCache(userTags = "rnorm")
  expect_true(NROW(unique(ss$cacheId)) == 1) # the 1 runif from tmpdir + rnorm in tmpdir

  # Put it back from local cache to cloud cache
  out10 <- Cache(rnorm, 7, useCloud = TRUE, cloudFolderID = cfid, cacheRepo = tmpdir)
  sc4 <- cloudShowCache(cfid)
  scLoc <- showCache(tmpdir)
  # expect_true(identical(sc4[,-"createdDate"], scLoc[,-"createdDate"]))
  expect_true(all.equal(sc4[, -"createdDate"],
                        scLoc[, -"createdDate"]))


})
