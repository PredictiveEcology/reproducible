test_that("test Cache(useCloud=TRUE, ...)", {
  skip_on_cran()
  skip_on_ci()

  testInit(
    c("googledrive", "terra"),
    tmpFileExt = c(".tif", ".grd"),
    needGoogleDriveAuth = TRUE,
  )

  ## service accounts cannot upload to standard drive folders (no quota)
  skip_if_service_account()

  withr::local_options(
    reproducible.cachePath = file.path(tempdir(), rndstr(1, 7)),
    reproducible.ask = FALSE,
    reproducible.useMemoise = FALSE
  )

  clearCache(x = tmpCache)
  googleSetupForUseCloud(cloudFolderID, tmpdir, tmpCache)

  testsForPkgs <- "testsForPkgs"
  tryCatch(googledrive::drive_ls(testsForPkgs), error = function(x)
    googledrive::drive_mkdir(name = testsForPkgs))
  newDir <- retry(quote(googledrive::drive_mkdir(name = rndstr(1, 6), path = testsForPkgs)))
  cloudFolderID <- newDir

  # local absent, cloud absent
  mess1 <- capture_messages({
    a1 <- Cache(rnorm, 1, cloudFolderID = cloudFolderID, cachePath = tmpCache, useCloud = TRUE)
  })
  expect_true(any(grepl("Uploading", mess1)))

  # local present, cloud present
  mess2 <- capture_messages({
    a1 <- Cache(rnorm, 1, cloudFolderID = cloudFolderID, cachePath = tmpCache, useCloud = TRUE)
  })
  expect_true(any(grepl(.message$LoadedCacheResult(), mess2)))
  .message$LoadedCacheResult
  expect_false(all(grepl("uploaded", ignore.case = TRUE, mess2)))
  expect_false(all(grepl("download", mess2)))

  # local absent, cloud present
  clearCache(userTags = .robustDigest(1), x = tmpCache, useCloud = FALSE)
  mess3 <- capture_messages({
    a1 <- Cache(rnorm, 1, cloudFolderID = cloudFolderID, cachePath = tmpCache, useCloud = TRUE)
  })

  # it is actually both loaded & saved locally; so should be loadedCachedResult and saved
  if (!useDBI()) {
    expect_true(any(grepl(.message$LoadedCacheResult(), mess3)))
  } else {
    expect_false(any(grepl(.message$LoadedCacheResult(), mess3)))
  }
  expect_false(any(grepl("Uploaded", mess3)))
  expect_true(any(grepl("Downloading", mess3)))

  # local present, cloud absent
  clearCache(x = tmpCache, useCloud = TRUE, cloudFolderID = cloudFolderID)
  a1 <- Cache(rnorm, 2, cloudFolderID = cloudFolderID, cachePath = tmpCache)
  mess4 <- capture_messages({
    a2 <- Cache(rnorm, 2, cloudFolderID = cloudFolderID, cachePath = tmpCache, useCloud = TRUE)
  })

  expect_true(any(grepl(.message$LoadedCacheResult(), mess4)))
  expect_true(any(grepl("Uploading", mess4)))
  expect_false(any(grepl("Download", mess4)))

  # cloudFolderID missing
  reproducible::clearCache(x = tmpCache, useCloud = TRUE, cloudFolderID = cloudFolderID)

  withr::local_options("reproducible.cloudFolderID" = NULL)
  # on.exit(try(googledrive::drive_rm(cloudFolderFromCacheRepo(tmpdir))), add = TRUE)
  # Try two different cloud folders -- based on tmpdir and tmpCache
  warn5 <- capture_warnings({
    mess5 <- capture_messages({
      a2 <- Cache(rnorm, 3, cachePath = tmpdir, useCloud = TRUE)
    })
  })
  options("reproducible.cloudFolderID" = NULL)

  expect_true(any(grepl("Uploading", mess5)))
  expect_false(any(grepl("Download", mess5)))


  # on.exit(try(googledrive::drive_rm(cloudFolderFromCacheRepo(tmpCache))), add = TRUE)
  warn6 <- capture_warnings({
    mess6 <- capture_messages({
      a2 <- Cache(rnorm, 3, cachePath = tmpCache, useCloud = TRUE)
    })
  })

  expect_true(any(grepl("Uploading", mess6)))
  expect_false(any(grepl("Download", mess6)))
  expect_false(any(grepl(.message$LoadedCacheResult(), mess6)))
  expect_true(isTRUE(all.equal(length(warn6), 0)))

  # Clear all
  try(drive_rm(drive_ls(cloudFolderFromCacheRepo(tmpCache))), silent = TRUE)

  # will use getOption("reproducible.cloudFolderID") b/c not specified, which is not cloudFolderFromCacheRepo(tmpCache)
  # clearCache(x = tmpCache, useCloud = TRUE)
  # Switch to tmpCache only
  withr::local_options(reproducible.cloudFolderID = tmpCache)
  cloudFolderID <- getOption("reproducible.cloudFolderID") # currently based on tmpCache

  # Add 3 things to cloud and local -- then clear them all
  for (i in 1:3) {
    a1 <- Cache(rnorm, i, cloudFolderID = cloudFolderID, cachePath = tmpCache, useCloud = TRUE)
  }
  expect_silent({
    mess1 <- capture_messages(
      clearCache(x = tmpCache, useCloud = TRUE, cloudFolderID = cloudFolderID)
    )
  })

  expect_true(NROW(googledrive::drive_ls(path = cloudFolderFromCacheRepo(tmpCache))) == 0)

  # Add 3 things to local, only 2 to cloud -- clear them all, without an error
  for (i in 1:2) {
    a1 <- Cache(rnorm, i, cloudFolderID = cloudFolderID, cachePath = tmpCache, useCloud = TRUE)
  }
  a1 <- Cache(rnorm, 3, cloudFolderID = cloudFolderID, cachePath = tmpCache, useCloud = FALSE)
  expect_silent({
    mess2 <- capture_messages(
      clearCache(x = tmpCache, useCloud = TRUE, cloudFolderID = cloudFolderID)
    )
  })
  expect_true(NROW(googledrive::drive_ls(path = cloudFolderFromCacheRepo(tmpCache))) == 0)

  # Add 2 things to local and cloud -- clear only 1 of them, without an error
  Cache(rnorm, 1, cloudFolderID = cloudFolderID, cachePath = tmpCache, useCloud = TRUE)
  Cache(rnorm, 2, cloudFolderID = cloudFolderID, cachePath = tmpCache, useCloud = TRUE)
  expect_silent({
    mess2 <- capture_messages(
      clearCache(x = tmpCache, userTags = CacheDigest(rnorm(1))$outputHash, useCloud = TRUE, cloudFolderID = cloudFolderID)
    )
  })

  gdriveLs <- googledrive::drive_ls(path = cloudFolderFromCacheRepo(tmpCache))
  expect_true(NROW(gdriveLs) == 2)
  expect_true(all(grepl(unique(showCache(tmpCache)[[.cacheTableHashColName()]]), gdriveLs$name)))
})

test_that("test Cache(useCloud=TRUE, ...) with raster-backed objs -- tif and grd", {
  skip_on_cran()
  skip_on_ci()
  testInit(c("googledrive", "terra"),
    needGoogleDriveAuth = TRUE,
    opts = list(reproducible.ask = FALSE)
  )

  ## service accounts cannot upload to standard drive folders (no quota)
  skip_if_service_account()

  googleSetupForUseCloud(cloudFolderID, tmpdir, tmpCache)

  withr::local_options(reproducible.cachePath = tmpdir)

  # on.exit(
  #   {
  #     retry(quote(googledrive::drive_rm(googledrive::as_id(newDir$id))))
  #   },
  #   add = TRUE
  # )
  clearCache(x = tmpCache)
  clearCache(x = tmpdir)
  newDir <- retry(quote(googledrive::drive_mkdir(name = basename2(tmpdir), path = "testsForPkgs")))
  cloudFolderID <- newDir

  testRasterInCloud(".tif",
    cloudFolderID = cloudFolderID, numRasterFiles = 1, tmpdir = tmpdir,
    type = "Raster"
  )

  retry(quote(googledrive::drive_rm(googledrive::as_id(newDir$id))))
  clearCache(x = tmpdir)
  newDir <- retry(quote(googledrive::drive_mkdir(name = rndstr(1, 6), path = "testsForPkgs")))
  cloudFolderID <- newDir

  ## the 3 raster files include the .grd, .gri, and .grd.aux.xml
  testRasterInCloud(".grd",
    cloudFolderID = cloudFolderID, numRasterFiles = 3, tmpdir = tmpdir,
    type = "Raster"
  )
})

test_that("test Cache(useCloud=TRUE, ...) with raster-backed objs -- stack", {
  skip_on_cran()
  skip_on_ci()
  testInit(c("googledrive", "terra"),
    needGoogleDriveAuth = TRUE,
    opts = list(reproducible.ask = FALSE)
  )

  ## service accounts cannot upload to standard drive folders (no quota)
  skip_if_service_account()

  googleSetupForUseCloud(cloudFolderID, tmpdir, tmpCache)
  withr::local_options(reproducible.cachePath = tmpdir)
  clearCache(x = tmpCache)
  clearCache(x = tmpdir)
  newDir <- retry(quote(googledrive::drive_mkdir(name = basename2(tmpdir), path = "testsForPkgs")))
  cloudFolderID <- newDir

  testRasterInCloud(".tif",
    cloudFolderID = cloudFolderID, numRasterFiles = 2, tmpdir = tmpdir,
    type = "Stack"
  )
})

test_that("test Cache(useCloud=TRUE, ...) with raster-backed objs -- brick", {
  skip_on_cran()
  skip_on_ci()
  testInit(c("googledrive", "terra"),
    needGoogleDriveAuth = TRUE,
    opts = list(reproducible.ask = FALSE)
  )

  ## service accounts cannot upload to standard drive folders (no quota)
  skip_if_service_account()

  googleSetupForUseCloud(cloudFolderID, tmpdir, tmpCache)
  withr::local_options(reproducible.cachePath = tmpdir)
  clearCache(x = tmpCache)
  clearCache(x = tmpdir)
  newDir <- retry(quote(googledrive::drive_mkdir(name = tempdir2(), path = "testsForPkgs")))
  cloudFolderID <- newDir

  testRasterInCloud(".tif",
    cloudFolderID = cloudFolderID, numRasterFiles = 1, tmpdir = tmpdir,
    type = "Brick"
  )
})

test_that("prepInputs works with team drives", {
  skip_on_cran()
  skip_on_ci()

  testInit(needGoogleDriveAuth = TRUE, "googledrive")
  withr::local_options(
    reproducible.cachePath = file.path(tempdir(), rndstr(1, 7)),
    reproducible.ask = FALSE
  )

  googleSetupForUseCloud(cloudFolderID, tmpdir, tmpCache)

  # zipUrl <- "https://drive.google.com/file/d/1zRX2c55ebJbQtjijCErEfGxhsa7Ieph2" # Orig
  zipUrl <- "https://drive.google.com/file/d/1JpdvM8QiyCyNyQAvSaFU0rAY-1I3mcbp"

  # This will fail if it is hit too many times -- we don't want the test to report
  #  fail because of this
  withr::local_options("reproducible.interactiveOnDownloadFail" = FALSE)
  if (packageVersion("googledrive") < "2.0.0") {
    wb <- prepInputs(
      targetFile = "WB_BCR.shp", destinationPath = tmpdir, url = zipUrl,
      alsoExtract = "similar",
      team_drive = TRUE
    )
  } else {
    err <- capture_error(
      noisy <- capture.output(
        wb <- prepInputs(
          targetFile = "WB_BCR.shp", destinationPath = tmpdir, url = zipUrl,
          alsoExtract = "similar",
          shared_drive = TRUE
        )
      )
    )
    if (is.null(err)) {
      if (.requireNamespace("sf")) {
        expect_true(is(wb, "sf"))
      }
    }
  }
})
