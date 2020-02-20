test_that("test Cache(useCloud=TRUE, ...)", {
  skip_if_no_token()
  if (interactive()) {
    testInitOut <- testInit(
      c("googledrive", "raster"), tmpFileExt = c(".tif", ".grd"),
      #needGoogle = TRUE,
      opts = list("reproducible.cachePath" = file.path(tempdir(), rndstr(1, 7)),
                  "reproducible.ask" = FALSE)
    )
    # drive_auth("predictiveecology@gmail.com")
    on.exit({
      testOnExit(testInitOut)
    }, add = TRUE)
    clearCache(x = tmpCache)
    testsForPkgs <- "testsForPkgs"
    df <- drive_find(pattern = testsForPkgs)
    if (NROW(df) == 0)
      testsForPkgsDir <- retry(quote(drive_mkdir(name = testsForPkgs)))
    newDir <- retry(quote(drive_mkdir(name = rndstr(1, 6), path = testsForPkgs)))
    cloudFolderID = newDir
    on.exit({
      try(retry(quote(drive_rm(cloudFolderID))), silent = TRUE)
    }, add = TRUE)

    #######################################
    # local absent, cloud absent
    #######################################
    mess1 <- capture_messages({
      a1 <- Cache(rnorm, 1, cloudFolderID = cloudFolderID, cacheRepo = tmpCache, useCloud = TRUE)
    })
    expect_true(any(grepl("uploaded", mess1)))

    #######################################
    # local present, cloud present
    #######################################
    mess2 <- capture_messages({
      a1 <- Cache(rnorm, 1, cloudFolderID = cloudFolderID, cacheRepo = tmpCache, useCloud = TRUE)
    })
    expect_true(any(grepl("loading cached", mess2)))
    expect_false(all(grepl("uploaded", mess2)))
    expect_false(all(grepl("download", mess2)))

    #######################################
    # local absent, cloud present
    #######################################
    #kkkk <<- 1

    clearCache(userTags = .robustDigest(1), x = tmpCache, useCloud = FALSE)
    mess3 <- capture_messages({
      a1 <- Cache(rnorm, 1, cloudFolderID = cloudFolderID, cacheRepo = tmpCache, useCloud = TRUE)
    })
    expect_false(any(grepl("loading cached", mess3)))
    expect_false(any(grepl("uploaded", mess3)))
    expect_true(any(grepl("download", mess3)))

    #######################################
    # local present, cloud absent
    #######################################
    clearCache(x = tmpCache, useCloud = TRUE, cloudFolderID = cloudFolderID)
    a1 <- Cache(rnorm, 2, cloudFolderID = cloudFolderID, cacheRepo = tmpCache)
    mess4 <- capture_messages({
      a2 <- Cache(rnorm, 2, cloudFolderID = cloudFolderID, cacheRepo = tmpCache, useCloud = TRUE)
    })

    expect_true(any(grepl("loading cached", mess4)))
    expect_true(any(grepl("uploaded", mess4)))
    expect_false(any(grepl("download", mess4)))

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


    on.exit({
      try(retry(quote(drive_rm(cloudFolderFromCacheRepo(tmpCache)))), silent = TRUE)
    }, add = TRUE)
    expect_true(any(grepl("Created Drive file", mess5)))
    expect_true(any(grepl("Uploading", mess5)))
    expect_false(any(grepl("download", mess5)))
    # expect_true(any(grepl("No cloudFolderID", warn5)))

    warn6 <- capture_warnings({
      mess6 <- capture_messages({
        a2 <- Cache(rnorm, 3, cacheRepo = tmpCache, useCloud = TRUE)
      })
    })

    expect_false(any(grepl("Folder created", mess6)))
    expect_false(any(grepl("Uploading", mess6)))
    expect_false(any(grepl("download", mess6)))
    expect_true(any(grepl("loading cached", mess6)))
    expect_true(isTRUE(all.equal(length(warn6), 0)))

    ########
    retry(quote(drive_rm(newDir))) # clear the original one
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
    expect_true(NROW(drive_ls(path = cloudFolderFromCacheRepo(tmpCache))) == 0)

    # Add 3 things to local, only 2 to cloud -- clear them all, without an error
    for (i in 1:2)
      a1 <- Cache(rnorm, i, cloudFolderID = cloudFolderID, cacheRepo = tmpCache, useCloud = TRUE)
    a1 <- Cache(rnorm, 3, cloudFolderID = cloudFolderID, cacheRepo = tmpCache, useCloud = FALSE)
    expect_silent({
      mess2 <- capture_messages(
        clearCache(x = tmpCache, useCloud = TRUE, cloudFolderID = cloudFolderID)
      )
    })
    expect_true(NROW(drive_ls(path = cloudFolderFromCacheRepo(tmpCache))) == 0)

    # Add 2 things to local and cloud -- clear only 1 of them, without an error
    Cache(rnorm, 1, cloudFolderID = cloudFolderID, cacheRepo = tmpCache, useCloud = TRUE)
    Cache(rnorm, 2, cloudFolderID = cloudFolderID, cacheRepo = tmpCache, useCloud = TRUE)
    on.exit({
      try(drive_rm(getOption("reproducible.cloudFolderID")), silent = TRUE)
      options("reproducible.cloudFolderID" = NULL)
    }, add = TRUE)
    mess2 <- capture_messages(
      clearCache(x = tmpCache, userTags = .robustDigest(1), useCloud = TRUE, cloudFolderID = cloudFolderID)
    )
    mess3 <- capture_messages(
      clearCache(x = tmpCache, userTags = .robustDigest(1), useCloud = TRUE, cloudFolderID = cloudFolderID)
    )
    expect_true(sum(grepl("Cloud", mess2))==1)
    expect_true(sum(grepl("Cloud", mess3))==0)

    # cloudFolderID <- getOption("reproducible.cloudFolderID")
    gdriveLs <- drive_ls(path = cloudFolderID)
    expect_true(NROW(gdriveLs) == 1)
    mess <- capture_messages(
      expect_true(grepl(unique(showCache(tmpCache)[[.cacheTableHashColName()]]),
                        gdriveLs$name)))
  }
})

test_that("test Cache(useCloud=TRUE, ...) with raster-backed objs -- tif and grd", {
  skip_if_no_token()
  if (interactive()) {
    testInitOut <- testInit(c("googledrive", "raster"), tmpFileExt = c(".tif", ".grd"),
                            opts = list("reproducible.ask" = FALSE))

    opts <- options("reproducible.cachePath" = tmpdir)
    suppressWarnings(rm(list = "aaa", envir = .GlobalEnv))

    # drive_auth("predictiveecology@gmail.com")
    on.exit({
      testOnExit(testInitOut)
      retry(quote(drive_rm(as_id(newDir$id))))
      options(opts)
    }, add = TRUE)
    clearCache(x = tmpCache)
    clearCache(x = tmpdir)
    newDir <- retry(quote(drive_mkdir(name = rndstr(1, 6), path = "testsForPkgs")))
    cloudFolderID = newDir

    on.exit({
    }, add = TRUE)

    testRasterInCloud(".tif", cloudFolderID = cloudFolderID, numRasterFiles = 1, tmpdir = tmpdir,
                      type = "Raster")

    retry(quote(drive_rm(as_id(newDir$id))))
    clearCache(x = tmpdir)
    newDir <- retry(quote(drive_mkdir(rndstr(1,6))))
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
      testOnExit(testInitOut)
      try(retry(quote(drive_rm(newDir))), silent = TRUE)
      options(opts)
    }, add = TRUE)
    clearCache(x = tmpCache)
    clearCache(x = tmpdir)
    newDir <- retry(quote(drive_mkdir(name = rndstr(1, 6), path = "testsForPkgs")))
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
    on.exit({
      testOnExit(testInitOut)
      try(retry(quote(drive_rm(newDir))), silent = TRUE)
      options(opts)
    }, add = TRUE)
    clearCache(x = tmpCache)
    clearCache(x = tmpdir)
    newDir <- #if (Sys.info()[["user"]] == "emcintir") {
      #  list(id = "1vKImpt2FQLmdDzA7atwhz9B-6Er26rka")
      #} else { # this is slow for emcintir because googledrive is large
      retry(quote(drive_mkdir(name = rndstr(1, 6), path = "testsForPkgs")))
    #}
    cloudFolderID = newDir

    testRasterInCloud(".tif", cloudFolderID = cloudFolderID, numRasterFiles = 1, tmpdir = tmpdir,
                      type = "Brick")

  }
})

test_that("Cache(useCloud=TRUE, ...) with useCache = 'overwrite'", {
  skip_if_no_token()
  if (interactive()) {
    rm(list = ls(all.names = TRUE)[startsWith(ls(all.names = TRUE), "._")])
    testInitOut <- testInit(c("googledrive"), # tmpFileExt = c(".tif", ".grd"),
                            opts = list("reproducible.ask" = FALSE))

    opts <- options("reproducible.cachePath" = tmpdir)
    on.exit({
      try(retry(quote(drive_rm(cloudFolderFromCacheRepo(tmpCache)))), silent = TRUE)
      testOnExit(testInitOut)
      options(opts)
    }, add = TRUE)
    clearCache(x = tmpCache)
    clearCache(x = tmpdir)

    mess2 <- capture_messages({
      a1 <- Cache(rnorm, 1, cloudFolderID = NULL, cacheRepo = tmpCache, useCloud = TRUE,
                  omitArgs = "n")
    })
    mess3 <- capture_messages({
      a2 <- Cache(rnorm, 2, cloudFolderID = NULL, cacheRepo = tmpCache, useCloud = TRUE,
                  omitArgs = "n", useCache = "overwrite")
    })
    expect_true(sum(grepl("Overwriting", mess3)) == 1)
    expect_true(sum(grepl("Uploading new cached", mess3)) == 1)
    expect_true(sum(grepl("Uploading new cached", mess2)) == 1)
    expect_true(identical(CacheDigest(list(rnorm))$outputHash,
                          gsub("cacheId:", "", attr(a2, "tags"))))
    expect_true(length(a2) == 2)
    ## Remove local copy -- should NOT get cloud version that is still there
    clearCache(x = tmpCache)
    # ._clearCache_3 <<- ._clearCache_1 <<- ._Cache_16 <<- 1
    mess3 <- capture_messages({
      a2 <- Cache(rnorm, 2, cloudFolderID = NULL, cacheRepo = tmpCache, useCloud = TRUE,
                  omitArgs = "n", useCache = "overwrite")
    })
    expect_true(sum(grepl("Overwriting", mess3)) == 1)
    expect_true(sum(grepl("Uploading new cached", mess3)) == 1)

    # Now try with a true overwrite ... results in a different object
    clearCache(x = tmpCache)
    mess3 <- capture_messages({
      a2 <- Cache(rnorm, 3, cloudFolderID = NULL, cacheRepo = tmpCache, useCloud = TRUE,
                  omitArgs = "n", useCache = "overwrite")
    })
    expect_true(sum(grepl("Overwriting", mess3)) == 1)
    expect_true(sum(grepl("Uploading new cached", mess3)) == 1)
    expect_true(length(a2) == 3)

    # Now remove local and should retrieve from cloud with length 3
    clearCache(x = tmpCache)
    mess3 <- capture_messages({
      a2 <- Cache(rnorm, 3, cloudFolderID = NULL, cacheRepo = tmpCache, useCloud = TRUE,
                  omitArgs = "n")
    })
    expect_false(sum(grepl("Overwriting", mess3)) == 1)
    expect_false(sum(grepl("Uploading new cached", mess3)) == 1)
    expect_true(sum(grepl("Downloading cloud", mess3)) == 1)
    expect_true(length(a2) == 3)


    # replace with a different object, same CacheDigest, using "overwrite"
    mess4 <- capture_messages({
      a3 <- Cache(rnorm, 6, cloudFolderID = NULL, cacheRepo = tmpCache, useCloud = TRUE,
                  omitArgs = "n", useCache = "overwrite")
    })
    expect_true(sum(grepl("Overwriting", mess4)) == 1)
    expect_true(sum(grepl("Uploading new cached", mess4)) == 1)
    expect_true(length(a3) == 6)
    expect_true(NROW(driveLs(cloudFolderID = cloudFolderFromCacheRepo(tmpCache),
                             pattern = "*")) == 1) # still only one object

    # ._Cache_15 <<- 1
    mess5 <- capture_messages({
      a5 <- Cache(rnorm, 1, cloudFolderID = NULL, cacheRepo = tmpCache)
    })

    mess6 <- capture_messages({
      a6 <- Cache(rnorm, 1, cloudFolderID = NULL, cacheRepo = tmpCache, useCloud = TRUE)
    })

    expect_true(length(a6) == 1)
    expect_true(length(a5) == 1)
    expect_true(isFALSE(attr(a6, ".Cache")$newCache))
    expect_true(sum(grepl("Uploading", mess6)) == 1)

  }

})

test_that("Cache(useCloud=TRUE, ...) more", {
  skip_if_no_token()
  if (interactive()) {
    rm(list = ls(all.names = TRUE)[startsWith(ls(all.names = TRUE), "._")])
    testInitOut <- testInit(c("googledrive"), # tmpFileExt = c(".tif", ".grd"),
                            opts = list("reproducible.ask" = FALSE))
    on.exit({
      try(drive_auth("predictiveecology@gmail.com"))
      try(retry(quote(drive_rm(cloudFolderID))), silent = TRUE)
      testOnExit(testInitOut)
      options(opts)
    }, add = TRUE)
    mess1 <- capture_messages({
      a1 <- Cache(rnorm, 10, useCloud = TRUE, cloudFolderID = NULL)
    })
    cloudFolderID <- getOption("reproducible.cloudFolderID")
    # remove local -- then pass cloudFolderID as a character string
    clearCache(tmpCache)
    mess6 <- capture_messages({
      a1 <- Cache(rnorm, 10, useCloud = TRUE, cloudFolderID = cloudFolderID$id)
    })
    drive_share(cloudFolderID, type = "anyone")
    drive_deauth()
    drive_auth()
    clearCache(tmpCache)
    mess7 <- capture_messages({
      a2 <- Cache(rnorm, 10, useCloud = TRUE, cloudFolderID = cloudFolderID$id)
    })
    expect_true(length(a2) == 10)
    expect_true(sum(grepl("Downloading cloud", mess7)) == 1)
  }

})
