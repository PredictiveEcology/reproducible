##########################
test_that("test miscellaneous unit tests cache-helpers", {
  testInitOut <- testInit(libraries = "raster", tmpFileExt = c(".tif", ".grd"),
                          opts = list(reproducible.useMemoise = TRUE, reproducible.useDBI = FALSE))
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  # this is just a short selection of Cache for code coverage
  expect_silent(a1 <- Cache(rnorm, 1, cacheRepo = tmpdir))
  expect_message(a2 <- Cache(rnorm, 1, cacheRepo = tmpdir))
  expect_message(a3 <- Cache(rnorm, 1, cacheRepo = tmpdir))
  a4 <- Cache(rnorm, 2, cacheRepo = tmpdir, userTags = "test")

  sc1 <- showCache(tmpdir)
  expect_true(length(unique(sc1$artifact)) == 2)
  sc2 <- showCache(tmpdir, userTags = "test")
  expect_true(length(unique(sc2$artifact)) == 1)

  sc2 <- keepCache(tmpdir, userTags = "test")
  sc1 <- showCache(tmpdir)
  expect_true(length(unique(sc1$artifact)) == 1)

  sc3 <- clearCache(tmpdir)
  expect_true(length(unique(sc3$artifact)) == 0)

  expect_silent(r1 <- Cache(raster, extent(0,1,0,1), res = 1, vals = 1))
  expect_message(r1 <- Cache(raster, extent(0,1,0,1), res = 1, vals = 1))

  expect_silent(r2 <- Cache(writeRaster, r1, file = tmpfile[1], overwrite = TRUE, quick = TRUE,
                            cacheRepo = tmpdir, userTags = "raster1"))
  expect_message(r2 <- Cache(writeRaster, r1, file = tmpfile[1], overwrite = TRUE, quick = TRUE,
                             cacheRepo = tmpdir, userTags = "raster1"))

  expect_silent(r2 <- Cache(writeRaster, r1, file = tmpfile[2], overwrite = TRUE, quick = TRUE, cacheRepo = tmpdir))
  expect_message(r2 <- Cache(writeRaster, r1, file = tmpfile[2], overwrite = TRUE, quick = TRUE, cacheRepo = tmpdir))

  r2 <- Cache(writeRaster, r1, file = tmpfile[2], overwrite = TRUE, quick = TRUE)
  expect_true(all(file.exists(tmpfile)))
  expect_true(all(file.exists(file.path(tmpdir, "rasters", basename2(tmpfile)))))

  sc1 <- showCache(tmpdir)
  expect_true(length(unique(sc1$artifact)) == 2)
  sc1 <- clearCache(tmpdir, userTags = "raster1")
  expect_true(length(unique(sc1$artifact)) == 1)
  expect_false(all(file.exists(file.path(tmpdir, "rasters", basename2(tmpfile[1])))))
  sc1 <- clearCache(tmpdir)
  expect_false(all(file.exists(file.path(tmpdir, "rasters", basename2(tmpfile)))))


})
