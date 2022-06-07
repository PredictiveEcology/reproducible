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
    # googledrive::drive_auth("predictiveecology@gmail.com")
    on.exit({
      testOnExit(testInitOut)
    }, add = TRUE)
    clearCache(x = tmpCache, useCloud = TRUE)
    # if (packageVersion("googledrive") < "2.0.0") {
    #   df <- googledrive::drive_find(pattern = testsForPkgs, team_drive = NULL)
    # } else {
    #   df <- googledrive::drive_find(pattern = testsForPkgs, shared_drive = NULL)
    # }
    #if (NROW(df) == 0)
    testsForPkgsDir <- retry(quote(googledrive::drive_mkdir(name = .pkgEnv$testsForPkgs, overwrite = TRUE)))
    on.exit({
      retry(quote(googledrive::drive_rm(.pkgEnv$testsForPkgs)))
    }, add = TRUE)

    newDir <- retry(quote(googledrive::drive_mkdir(name = rndstr(1, 6), path = .pkgEnv$testsForPkgs)))
    cloudFolderID = newDir
    # on.exit({
    #   retry(quote(googledrive::drive_rm(cloudFolderID)))
    # }, add = TRUE)

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

    # on.exit({
    #   retry(quote(googledrive::drive_rm(googledrive::as_id(cloudFolderID))))
    # }, add = TRUE)
    expect_true(any(grepl(messUploaded, mess5)))
    expect_false(any(grepl(messDownload, mess5)))
    # expect_true(any(grepl("No cloudFolderID", warn5)))

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
    retry(quote(googledrive::drive_rm(newDir))) # clear the original one
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
    retry(quote(googledrive::drive_rm(getOption("reproducible.cloudFolderID"))))
  }
})

test_that("test Cache(useCloud=TRUE, ...) with raster-backed objs -- tif and grd", {
  skip_if_no_token()
  if (interactive()) {
    testInitOut <- testInit(c("googledrive", "raster"), tmpFileExt = c(".tif", ".grd"),
                            opts = list("reproducible.ask" = FALSE))

    opts <- options("reproducible.cachePath" = tmpdir)
    suppressWarnings(rm(list = "aaa", envir = .GlobalEnv))
    testsForPkgsDir <- retry(quote(googledrive::drive_mkdir(name = .pkgEnv$testsForPkgs, overwrite = TRUE)))
    clearCache(x = tmpCache)
    clearCache(x = tmpdir)
    newDir <- retry(quote(googledrive::drive_mkdir(name = rndstr(1, 6), path = .pkgEnv$testsForPkgs)))
    on.exit({
      retry(quote(googledrive::drive_rm(googledrive::as_id(newDir$id))))
      options(opts)
      retry(quote(googledrive::drive_rm(.pkgEnv$testsForPkgs)))
      testOnExit(testInitOut)
    }, add = TRUE)

    # googledrive::drive_auth("predictiveecology@gmail.com")

    cloudFolderID = newDir

    testRasterInCloud(".tif", cloudFolderID = cloudFolderID, numRasterFiles = 1, tmpdir = tmpdir,
                      type = "Raster")

    retry(quote(googledrive::drive_rm(googledrive::as_id(newDir$id))))
    clearCache(x = tmpdir)
    newDir <- retry(quote(googledrive::drive_mkdir(rndstr(1,6))))
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

    googledrive::drive_auth("predictiveecology@gmail.com")
    on.exit({
      testOnExit(testInitOut)
      retry(quote(googledrive::drive_rm(googledrive::as_id(newDir$id))))
      options(opts)
    }, add = TRUE)
    clearCache(x = tmpCache)
    clearCache(x = tmpdir)
    newDir <- retry(quote(googledrive::drive_mkdir(name = rndstr(1, 6), path = .pkgEnv$testsForPkgs)))
    cloudFolderID = newDir

    testRasterInCloud(".tif", cloudFolderID = cloudFolderID, numRasterFiles = 2, tmpdir = tmpdir,
                      type = "Stack")

  }
})

test_that("test Cache(useCloud=TRUE, ...) with raster-backed objs -- brick", {
  skip_if_no_token()
  if (interactive()) {
    testInitOut <- testInit(c("googledrive", "raster"), tmpFileExt = c(".tif", ".grd"),
                            opts = list("reproducible.ask" = FALSE))

    opts <- options("reproducible.cachePath" = tmpdir)
    googledrive::drive_auth("predictiveecology@gmail.com")
    on.exit({
      testOnExit(testInitOut)
      retry(quote(googledrive::drive_rm(googledrive::as_id(newDir$id))))
      options(opts)
    }, add = TRUE)
    clearCache(x = tmpCache)
    clearCache(x = tmpdir)
    newDir <- #if (Sys.info()[["user"]] == "emcintir") {
      #  list(id = "1vKImpt2FQLmdDzA7atwhz9B-6Er26rka")
      #} else { # this is slow for emcintir because googledrive is large
      retry(quote(googledrive::drive_mkdir(name = rndstr(1, 6), path = .pkgEnv$testsForPkgs)))
    #}
    cloudFolderID = newDir

    testRasterInCloud(".tif", cloudFolderID = cloudFolderID, numRasterFiles = 1, tmpdir = tmpdir,
                      type = "Brick")
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
  }
})
