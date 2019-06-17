# test_that("test Cache(useCache=TRUE, ...)", {
#   if (interactive()) {
#     testInitOut <- testInit(c("googledrive", "raster"), tmpFileExt = c(".tif", ".grd"),
#                             opts = list("reproducible.cachePath" = file.path(tempdir(), rndstr(1, 7)),
#                                         "reproducible.ask" = FALSE))
#     on.exit({
#       testOnExit(testInitOut)
#       drive_rm(as_id(newDir$id))
#     }, add = TRUE)
#     clearCache(x = tmpCache)
#     newDir <- drive_mkdir("testFolder")
#     cloudFolderID = newDir$id
#     #######################################
#     # local absent, cloud absent
#     #######################################
#     mess1 <- capture_messages(a1 <- Cache(rnorm, 1, cloudFolderID = cloudFolderID,
#                                          cacheRepo = tmpCache, useCloud = TRUE))
#     expect_true(any(grepl("uploaded", mess1)))
#
#     #######################################
#     # local present, cloud present
#     #######################################
#     mess2 <- capture_messages(a1 <- Cache(rnorm, 1, cloudFolderID = cloudFolderID,
#                                          cacheRepo = tmpCache, useCloud = TRUE))
#     expect_true(grepl("loading cached", mess2))
#     expect_false(grepl("uploaded", mess2))
#     expect_false(grepl("download", mess2))
#
#     #######################################
#     # local absent, cloud present
#     #######################################
#     clearCache(userTags = .robustDigest(1), x = tmpCache)
#     mess3 <- capture_messages(a1 <- Cache(rnorm, 1, cloudFolderID = cloudFolderID,
#                                           cacheRepo = tmpCache, useCloud = TRUE))
#     expect_false(any(grepl("loading cached", mess3)))
#     expect_false(any(grepl("uploaded", mess3)))
#     expect_true(any(grepl("download", mess3)))
#
#     #######################################
#     # local present, cloud absent
#     #######################################
#     clearCache(x = tmpCache, useCloud = TRUE, cloudFolderID = cloudFolderID)
#     a1 <- Cache(rnorm, 2, cloudFolderID = cloudFolderID, cacheRepo = tmpCache)
#     mess4 <- capture_messages(a2 <- Cache(rnorm, 2, cloudFolderID = cloudFolderID,
#                                           cacheRepo = tmpCache, useCloud = TRUE))
#
#     expect_true(any(grepl("loading cached", mess4)))
#     expect_true(any(grepl("uploaded", mess4)))
#     expect_false(any(grepl("download", mess4)))
#
#     #######################################
#     # cloudFolderID missing
#     #######################################
#     clearCache(x = tmpCache, useCloud = TRUE, cloudFolderID = cloudFolderID)
#
#     opts <- options("reproducible.cloudFolderID" = NULL)
#     warn5 <- capture_warnings(
#       mess5 <- capture_messages(
#         a2 <- Cache(rnorm, 3, cacheRepo = tmpCache, useCloud = TRUE)))
#
#     expect_true(any(grepl("Folder created", mess5)))
#     expect_true(any(grepl("Uploading", mess5)))
#     expect_false(any(grepl("download", mess5)))
#     expect_true(any(grepl("No cloudFolderID", warn5)))
#
#     warn6 <- capture_warnings(
#       mess6 <- capture_messages(
#         a2 <- Cache(rnorm, 3, cacheRepo = tmpCache, useCloud = TRUE)))
#
#     expect_false(any(grepl("Folder created", mess6)))
#     expect_false(any(grepl("Uploading", mess6)))
#     expect_false(any(grepl("download", mess6)))
#     expect_true(any(grepl("loading cached", mess6)))
#     expect_true(isTRUE(all.equal(length(warn6), 0)))
#
#     ########
#     clearCache(x = tmpCache, useCloud = TRUE, cloudFolderID = cloudFolderID)
#     # Add 3 things to cloud and local -- then clear them all
#     for (i in 1:3)
#       a1 <- Cache(rnorm, i, cloudFolderID = cloudFolderID, cacheRepo = tmpCache, useCloud = TRUE)
#     expect_silent(mess1 <- capture_messages(clearCache(x = tmpCache, useCloud = TRUE, cloudFolderID = cloudFolderID)))
#     expect_true(NROW(drive_ls(path = as_id(cloudFolderID)))==0)
#
#     # Add 3 things to local, only 2 to cloud -- clear them all, without an error
#     for (i in 1:2)
#       a1 <- Cache(rnorm, i, cloudFolderID = cloudFolderID, cacheRepo = tmpCache, useCloud = TRUE)
#     a1 <- Cache(rnorm, 3, cloudFolderID = cloudFolderID, cacheRepo = tmpCache, useCloud = FALSE)
#     expect_silent(mess2 <- capture_messages(clearCache(x = tmpCache, useCloud = TRUE, cloudFolderID = cloudFolderID)))
#     expect_true(NROW(drive_ls(path = as_id(cloudFolderID)))==0)
#
#     # Add 2 things to local, only 1 to cloud -- clear them all, without an error
#     Cache(rnorm, 1, cloudFolderID = cloudFolderID, cacheRepo = tmpCache, useCloud = TRUE)
#     Cache(rnorm, 2, cloudFolderID = cloudFolderID, cacheRepo = tmpCache, useCloud = TRUE)
#     expect_silent(mess2 <- capture_messages(clearCache(x = tmpCache, userTags = .robustDigest(1), useCloud = TRUE, cloudFolderID = cloudFolderID)))
#
#     expect_true(NROW(drive_ls(path = as_id(cloudFolderID)))==1)
#
#   }
# })


test_that("test Cache(useCache=TRUE, ...) with raster-backed objs", {
  if (interactive()) {
    testInitOut <- testInit(c("googledrive", "raster"), tmpFileExt = c(".tif", ".grd"),
                            opts = list("reproducible.cachePath" = file.path(tempdir(), rndstr(1, 7)),
                                        "reproducible.ask" = FALSE))

    on.exit({
      testOnExit(testInitOut)
      drive_rm(as_id(newDir$id))
      raster::rasterOptions(maxmemory = opts$maxmemory)
    }, add = TRUE)
    clearCache(x = tmpCache)
    newDir <- drive_mkdir("testFolder")
    cloudFolderID = newDir$id

    fn <- function(raster) {
      return(raster)
    }
    r <- raster(extent(0,200, 0, 200), vals = 1, res = 1)
    r <- writeRaster(r, filename = tmpfile[1], overwrite = TRUE)
    r <- Cache(fn, r, useCloud = TRUE, cloudFolderID = cloudFolderID)
    driveLs <- drive_ls(as_id(cloudFolderID))
    expect_true(NROW(driveLs) == 2)


    r <- raster(extent(0,200, 0, 200), vals = 1, res = 1)
    r <- writeRaster(r, filename = tempfile(tmpdir = tmpdir, fileext = ".tif"), overwrite = TRUE)
    r1 <- Cache(fn, r, useCloud = TRUE, cloudFolderID = cloudFolderID)

    # Clear local copy
    clearCache()
    r <- raster(extent(0,200, 0, 200), vals = 1, res = 1)
    r <- writeRaster(r, filename = tempfile(tmpdir = tmpdir, fileext = ".tif"), overwrite = TRUE)
    r1 <- Cache(fn, r, useCloud = TRUE, cloudFolderID = cloudFolderID)

  }
})
