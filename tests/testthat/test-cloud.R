test_that("test Cache(useCloud=TRUE, ...)", {
  skip_if_no_token()
  if (interactive()) {
    testInitOut <- testInit(
      c("googledrive", "raster"), tmpFileExt = c(".tif", ".grd"),
      #needGoogle = TRUE,
      opts = list("reproducible.cachePath" = file.path(tempdir(), rndstr(1, 7)),
                  "reproducible.ask" = FALSE)
    )
    drive_auth("predictiveecology@gmail.com")
    on.exit({
      testOnExit(testInitOut)
      retry(quote(drive_rm(as_id(newDir$id))))
    }, add = TRUE)
    clearCache(x = tmpCache)
    newDir <- #if (Sys.info()[["user"]] == "emcintir") {
    #  list(id = "1vKImpt2FQLmdDzA7atwhz9B-6Er26rka")
    #} else { # this is slow for emcintir because googledrive is large
      retry(quote(drive_mkdir(name = rndstr(1, 6), path = "testsForPkgs")))
    #}
    cloudFolderID = newDir$id
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

    # on.exit({
    #   retry(quote(drive_rm(as_id(cloudFolderID))))
    # }, add = TRUE)
    expect_true(any(grepl("Created Drive file", mess5)))
    expect_true(any(grepl("Uploading", mess5)))
    expect_false(any(grepl("download", mess5)))
    expect_true(any(grepl("No cloudFolderID", warn5)))

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
    cloudFolderID <- getOption("reproducible.cloudFolderID")
    clearCache(x = tmpCache, useCloud = TRUE, cloudFolderID = cloudFolderID)
    # Add 3 things to cloud and local -- then clear them all
    for (i in 1:3)
      a1 <- Cache(rnorm, i, cloudFolderID = cloudFolderID, cacheRepo = tmpCache, useCloud = TRUE)
    expect_silent({
      mess1 <- capture_messages(
        clearCache(x = tmpCache, useCloud = TRUE, cloudFolderID = cloudFolderID)
      )
    })
    expect_true(NROW(drive_ls(path = as_id(cloudFolderID))) == 0)

    # Add 3 things to local, only 2 to cloud -- clear them all, without an error
    for (i in 1:2)
      a1 <- Cache(rnorm, i, cloudFolderID = cloudFolderID, cacheRepo = tmpCache, useCloud = TRUE)
    a1 <- Cache(rnorm, 3, cloudFolderID = cloudFolderID, cacheRepo = tmpCache, useCloud = FALSE)
    expect_silent({
      mess2 <- capture_messages(
        clearCache(x = tmpCache, useCloud = TRUE, cloudFolderID = cloudFolderID)
      )
    })
    expect_true(NROW(drive_ls(path = as_id(cloudFolderID))) == 0)

    # Add 2 things to local and cloud -- clear only 1 of them, without an error
    Cache(rnorm, 1, cloudFolderID = cloudFolderID, cacheRepo = tmpCache, useCloud = TRUE)
    Cache(rnorm, 2, cloudFolderID = cloudFolderID, cacheRepo = tmpCache, useCloud = TRUE)
    expect_silent({
      mess2 <- capture_messages(
        clearCache(x = tmpCache, userTags = .robustDigest(1), useCloud = TRUE, cloudFolderID = cloudFolderID)
      )
    })

    gdriveLs <- drive_ls(path = as_id(cloudFolderID))
    expect_true(NROW(gdriveLs) == 1)
    expect_true(grepl(unique(showCache(tmpCache)[[.cacheTableHashColName()]]), gdriveLs$name))
  }
})

test_that("test Cache(useCloud=TRUE, ...) with raster-backed objs -- tif and grd", {
  skip_if_no_token()
  if (interactive()) {
    testInitOut <- testInit(c("googledrive", "raster"), tmpFileExt = c(".tif", ".grd"),
                            opts = list("reproducible.ask" = FALSE))

    opts <- options("reproducible.cachePath" = tmpdir)
    suppressWarnings(rm(list = "aaa", envir = .GlobalEnv))

    drive_auth("predictiveecology@gmail.com")
    on.exit({
      testOnExit(testInitOut)
      retry(quote(drive_rm(as_id(newDir$id))))
      options(opts)
    }, add = TRUE)
    clearCache(x = tmpCache)
    clearCache(x = tmpdir)
    newDir <- #if (Sys.info()[["user"]] == "emcintir") {
      #  list(id = "1vKImpt2FQLmdDzA7atwhz9B-6Er26rka")
      #} else { # this is slow for emcintir because googledrive is large
      retry(quote(drive_mkdir(name = rndstr(1, 6), path = "testsForPkgs")))
    #}
    cloudFolderID = newDir$id

    on.exit({
    }, add = TRUE)

    testRasterInCloud(".tif", cloudFolderID = cloudFolderID, numRasterFiles = 1, tmpdir = tmpdir,
                      type = "Raster")

    retry(quote(drive_rm(as_id(newDir$id))))
    clearCache(x = tmpdir)
    newDir <- retry(quote(drive_mkdir(rndstr(1,6))))
    cloudFolderID = newDir$id

    testRasterInCloud(".grd", cloudFolderID = cloudFolderID, numRasterFiles = 2, tmpdir = tmpdir,
                      type = "Raster")
  }
})

test_that("test Cache(useCloud=TRUE, ...) with raster-backed objs -- stack", {
  skip_if_no_token()
  if (interactive()) {
    testInitOut <- testInit(c("googledrive", "raster"), tmpFileExt = c(".tif", ".grd"),
                            opts = list("reproducible.ask" = FALSE))

    drive_auth("predictiveecology@gmail.com")
    on.exit({
      testOnExit(testInitOut)
      retry(quote(drive_rm(as_id(newDir$id))))
      options(opts)
    }, add = TRUE)
    clearCache(x = tmpCache)
    clearCache(x = tmpdir)
    newDir <- #if (Sys.info()[["user"]] == "emcintir") {
      #  list(id = "1vKImpt2FQLmdDzA7atwhz9B-6Er26rka")
      #} else { # this is slow for emcintir because googledrive is large
      retry(quote(drive_mkdir(name = rndstr(1, 6), path = "testsForPkgs")))
    #}
    cloudFolderID = newDir$id

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
    drive_auth("predictiveecology@gmail.com")
    on.exit({
      testOnExit(testInitOut)
      retry(quote(drive_rm(as_id(newDir$id))))
      options(opts)
    }, add = TRUE)
    clearCache(x = tmpCache)
    clearCache(x = tmpdir)
    newDir <- #if (Sys.info()[["user"]] == "emcintir") {
      #  list(id = "1vKImpt2FQLmdDzA7atwhz9B-6Er26rka")
      #} else { # this is slow for emcintir because googledrive is large
      retry(quote(drive_mkdir(name = rndstr(1, 6), path = "testsForPkgs")))
    #}
    cloudFolderID = newDir$id

    testRasterInCloud(".tif", cloudFolderID = cloudFolderID, numRasterFiles = 1, tmpdir = tmpdir,
                      type = "Brick")

  }
})

