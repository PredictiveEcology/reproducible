test_that("test cached downloads", {
  skip_on_cran()

  testInitOut <- testInit("raster", tmpFileExt = c(".tif", ".grd"))
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  outdir <- tmpdir
  ## https://open.canada.ca/data/en/dataset/9e1efe92-e5a3-4f70-b313-68fb1283eadf
  ## (Record ID: 9e1efe92-e5a3-4f70-b313-68fb1283eadf)
  #url <- "http://www.agr.gc.ca/atlas/data_donnees/lcv/aafcLand_Use/tif/2010/IMG_AAFC_LANDUSE_Z07_2010.zip" # nolint

   # ensure the file does not exist before testing
  if (file.exists(file.path(outdir, basename(urlTif1)))) {
    file.remove(file.path(outdir, basename(urlTif1)))
  }
  expect_false(file.exists(file.path(outdir, basename(urlTif1))))

  out <- createCache(outdir)
  # sideE <<- gggg <<- 1
  storageDir <- CacheStorageDir(outdir)
  # Cache download first run. File is downloaded. checksum is logged in backpack.
  out <- Cache(utils::download.file, url = urlTif1,
               destfile = asPath(file.path(outdir, basename(urlTif1))),
               method = "auto", quiet = TRUE, mode = "wb", cacheOK = TRUE,
               cacheRepo = outdir, sideEffect = TRUE, makeCopy = FALSE, quick = TRUE)

  # check if download occured
  expect_true(file.exists(file.path(outdir, basename(urlTif1))))

  # compare checksum from file with checksum stored in backpack
  urlfileSize <- list(basename(urlTif1), file.size(file.path(outdir, basename(urlTif1))))
  urlfileChcksum <- digest::digest(urlfileSize, algo = "xxhash64")

  cachedChcksum <- attributes(out)$chcksumFiles
  expect_equal(paste0(file.path(basename(outdir), basename(urlTif1)), ":", urlfileChcksum),
               cachedChcksum)

  # rerun download. Shouldn't run. # TODO: this shouldn't be rerunning
  out <- Cache(utils::download.file, url = urlTif1,
               destfile = asPath(file.path(outdir, basename(urlTif1))),
               method = "auto", quiet = TRUE, mode = "wb", cacheOK = TRUE,
               cacheRepo = outdir, sideEffect = TRUE, makeCopy = FALSE, quick = TRUE)

  # Make sur the file do not exists before testing
  toRemove <- list(basename(CacheSQLiteFile(".")), basename(urlTif1))
  lapply(toRemove, function(x) {
    if (file.exists(file.path(outdir, x))) file.remove(file.path(outdir, x))
  })
  expect_false(file.exists(file.path(outdir, basename(urlTif1))))
  expect_false(file.exists(CacheSQLiteFile(outdir)))

  # Test MakeCopy = TRUE
  out <- Cache(utils::download.file, url = urlTif1,
               destfile = asPath(file.path(outdir, basename(urlTif1))),
               method = "auto", quiet = TRUE, mode = "wb", cacheOK = TRUE,
               cacheRepo = outdir, sideEffect = TRUE, makeCopy = TRUE, quick = TRUE)

  # check if copy was created
  copyFolder <- storageDir
  expect_true(file.exists(file.path(copyFolder, basename(urlTif1))))

  # Remove downloaded file and check if it is brought back using the copy
  # see message 'loading cached result from previous FUN call'
  file.remove(file.path(outdir, basename(urlTif1)))
  expect_false(file.exists(file.path(outdir, basename(urlTif1))))
  out <- Cache(utils::download.file, url = urlTif1,
               destfile = asPath(file.path(outdir, basename(urlTif1))),
               method = "auto", quiet = TRUE, mode = "wb", cacheOK = TRUE,
               cacheRepo = outdir, sideEffect = TRUE, makeCopy = TRUE, quick = TRUE)

  expect_true(file.exists(file.path(outdir, basename(urlTif1))))
})
