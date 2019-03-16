test_that("test cloudSyncCache", {
  if (interactive()) {
    testInitOut <- testInit("raster", tmpFileExt = c(".tif", ".grd"),
                            opts = list("reproducible.cachePath" = file.path(tempdir(), rndstr(1, 7)),
                                        "reproducible.ask" = FALSE))
    on.exit({
      testOnExit(testInitOut)
    }, add = TRUE)
    #   Can use >1 cacheRepo
    cachePaths <- getOption("reproducible.cachePath")
    library(googledrive)
    newDir <- drive_mkdir("testFolder")
    # To remove whole folder:
    on.exit({
      drive_rm(as_id(newDir$id))
    }, add = TRUE)
    #a <- Cache(rnorm, 1, cacheRepo = getOption("reproducible.cachePath")[3])
    a <- Cache(rnorm, 1)
    b <- Cache(rnorm, 2)

    # Will copy the 2 to the cloud
    a <- cloudSyncCache(cloudFolderID = newDir$id)
    expect_true(NROW(a) == 2)
    #'
    # remove a local one
    clearCache(userTags = CacheDigest(list(rnorm, 2))$outputHash)
    sc1 <- suppressMessages(unique(showCache()$artifact))
    expect_true(NROW(sc1) == 1)

    # Now will delete the object in the cloud that was just deleted locally
    a <- cloudSyncCache(cloudFolderID = newDir$id)
    expect_true(NROW(a) == 1)
    #'
    # clean up
    lapply(cachePaths, clearCache, ask = FALSE)
    # clearCache(ask = FALSE) # if there were only 1 cacheRepo
    a <- cloudSyncCache(cloudFolderID = newDir$id)
    expect_true(NROW(a) == 0)
    #'
    #######################################################################
    # use showCache args to have control ... on upload & delete NOTE difference!
    #######################################################################
    # a <- Cache(rnorm, 1, cacheRepo = getOption("reproducible.cachePath")[3]) # multiple cacheRepos!
    a <- Cache(rnorm, 1)
    b <- Cache(rnorm, 2)
    # only sync the one with rnorm, 2 as arguments
    #   This CacheDigest is the same algorithm used by Cache
    tag <- CacheDigest(list(rnorm, 2))$outputHash
    a <- cloudSyncCache(cloudFolderID = newDir$id, userTags = tag) # only syncs the one
    expect_true(NROW(a) == 1)
    # that is identified
    # with userTags
    #'
    a <- cloudSyncCache(cloudFolderID = newDir$id) # sync any other ones
    expect_true(NROW(a) == 2)

    # Now clear an object locally -- next how to propagate this deletion to cloud
    clearCache(userTags = tag)
    sc1 <- suppressMessages(unique(showCache()$artifact))
    expect_true(NROW(sc1) == 1)

    # Add one more to local, so now local has 2 (a and d), cloud has 2 (a and b)
    d <- Cache(rnorm, 4)
    #'
    # DELETING IS DIFFERENT
    # Doesn't quite work same way for deleting -- this tag is not in local Cache,
    # so can't find it this way.
    # This next line DOES THE WRONG THING -- IT DELETES EVERYTHING because there is
    #         no entry in the local cache -- use cacheId arg instead -- see below
    #    showCache(userTags = tags) shows empty
    #    cloudSyncCache(cloudFolderID = newDir$id, userTags = tag)
    #'
    # Only delete the one that was removed from local cache, set upload = FALSE,
    #    leaving only 1 in cloud: a  -- this is still a sync, so, it will only
    #    delete 1 file because local has 1 fewer files -- see next for just deleting 1 artifact
    a <- cloudSyncCache(cloudFolderID = newDir$id, upload = FALSE)
    expect_true(NROW(a) == 1)
    sc1 <- suppressMessages(unique(showCache()$artifact))
    expect_true(NROW(sc1) == 2)

    # Upload the d, because it is the only one in the localCache not in the cloudCache
    a <- cloudSyncCache(cloudFolderID = newDir$id)
    expect_true(NROW(a) == 2)

    f <- Cache(rnorm, 5)
    g <- Cache(rnorm, 6)
    # upload both -- a "sync"
    a <- cloudSyncCache(cloudFolderID = newDir$id)
    expect_true(NROW(a) == 4)

    # now start deleting
    tag5 <- CacheDigest(list(rnorm, 5))$outputHash # this is the same algorithm used by Cache
    tag6 <- CacheDigest(list(rnorm, 6))$outputHash
    clearCache(userTags = tag5) # delete one locally by tag
    clearCache(userTags = tag6) # delete one locally by tag
    # delete only one by tag
    a <- cloudSyncCache(cloudFolderID = newDir$id, cacheIds = tag5) # will delete only this obj in cloud
    expect_true(NROW(a) == 3)
    # delete another one by tag
    a <- cloudSyncCache(cloudFolderID = newDir$id, cacheIds = tag6)
    expect_true(NROW(a) == 2)


    ################################################
    # Now, with 1 to delete and 1 to upload, but using delete = FALSE or TRUE for control
    f <- Cache(rnorm, 5)
    g <- Cache(rnorm, 6)
    i <- Cache(rnorm, 7)
    # upload all 3 -- a "sync"
    a <- cloudSyncCache(cloudFolderID = newDir$id)
    expect_true(NROW(a) == 5)

    # Now, with 2 missing locally, 1 missing in cloud
    h <- Cache(rnorm, 8)
    sc1 <- suppressMessages(unique(showCache()$artifact))
    expect_true(NROW(sc1) == 6)

    clearCache(userTags = tag5) # delete one locally by tag
    clearCache(userTags = tag6) # delete another locally by tag

    sc1 <- suppressMessages(unique(showCache()$artifact))
    expect_true(NROW(sc1) == 4)
    # With a sync -- there is ambiguity if there is something missing --
    #  If a cloud exists which is not local,
    #   should it delete the cloud (sync up) or download to local (sync down)
    tryDelete = TRUE # can try both TRUE or FALSE and get the two behaviours
    # If delete is FALSE
    #   will cause download to local Cache of 2 objs, 5 and 6
    # Alternatively, if delete is TRUE, then the 2 objects would be deleted in the cloud
    a <- cloudSyncCache(cloudFolderID = newDir$id, delete = tryDelete, upload = FALSE)
    expect_true(NROW(a) == 3) # 2 less due to delete
    # Check this worked --> yes! Only one to upload still
    sc1 <- suppressMessages(unique(showCache()$artifact))
    expect_true(NROW(sc1) == 4)
    # Do it again; ensure nothing changes
    a <- cloudSyncCache(cloudFolderID = newDir$id, delete = tryDelete, upload = FALSE)
    expect_true(NROW(a) == 3) # 2 less due to delete
    sc1 <- suppressMessages(unique(showCache()$artifact))
    expect_true(NROW(sc1) == 4)

    # Do it again with tryDelete = FALSE
    f <- Cache(rnorm, 5)
    g <- Cache(rnorm, 6)
    i <- Cache(rnorm, 7)
    # upload all 3 -- a "sync"
    a <- cloudSyncCache(cloudFolderID = newDir$id)
    expect_true(NROW(a) == 6) # 2 less due to delete

    j <- Cache(rnorm, 9)
    clearCache(userTags = tag5) # delete one locally by tag
    clearCache(userTags = tag6) # delete another locally by tag
    sc1 <- suppressMessages(unique(showCache()$artifact))
    expect_true(NROW(sc1) == 4)
    tryDelete = FALSE # can try both TRUE or FALSE and get the two behaviours
    # If delete is FALSE
    #   will cause download to local Cache of 2 objs, 5 and 6
    # Alternatively, if delete is TRUE, then the 2 objects would be deleted in the cloud
    a <- cloudSyncCache(cloudFolderID = newDir$id, delete = tryDelete, upload = FALSE)
    expect_true(NROW(a) == 6)
    sc1 <- suppressMessages(unique(showCache()$artifact))
    expect_true(NROW(sc1) == 7)

    # Now let the upload happen
    a <- cloudSyncCache(cloudFolderID = newDir$id, delete = FALSE)
    expect_true(NROW(a) == 7) # plus 1 more -- j

    # Nothing left to sync
    a <- cloudSyncCache(cloudFolderID = newDir$id)
    expect_true(NROW(a) == 7) # plus 1 more -- j

    # clean up
    # clearCache(ask = FALSE) # if only one cacheRepo
    lapply(cachePaths, clearCache, ask = FALSE)
    a <- cloudSyncCache(cloudFolderID = newDir$id)

    expect_true(NROW(a) == 0)
  }
})
