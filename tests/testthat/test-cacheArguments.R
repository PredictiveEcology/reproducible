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
  storageDir <- CacheStorageDir(outdir)
  destFile <- asPath(file.path(outdir, basename(urlTif1)))
  # Cache download first run. File is downloaded. checksum is logged in cache db
  out <- Cache(utils::download.file, url = urlTif1,
               destfile = destFile,
               method = "auto", quiet = TRUE, mode = "wb", cacheOK = TRUE,
               cacheRepo = outdir, sideEffect = "destfile",
               quick = TRUE,
               omitArgs = "destfile")

  # check if download occured
  outputHash <- gsub("cacheId:", "", attr(out, "tags"))
  expect_true(file.exists(destFile)) # in actual destination
  stashedFile <- sideEffectStashedFiles(outdir, outputHash, basename(destFile))
  expect_true(file.exists(stashedFile)) # in Cache

  # compare checksum from file with checksum stored in cache db
  urlfileSize <- list(basename(urlTif1), file.size(file.path(outdir, basename(urlTif1))))
  urlfileChcksum <- digest::digest(urlfileSize, algo = "xxhash64")

  # cachedChcksum <- attributes(out)$chcksumFiles
  # expect_equal(paste0(file.path(basename(outdir), basename(urlTif1)), ":", urlfileChcksum),
  #              cachedChcksum)

  # rerun download. Shouldn't run. # TODO: this shouldn't be rerunning
  # sideE <<- aaaa <<- gggg <<- eeee <<- ffff <<- nnnn <<- 1
  tf <- tempfile()
  out <- Cache(utils::download.file, url = urlTif1,
               destfile = tf,
               method = "auto", quiet = TRUE, mode = "wb", cacheOK = TRUE,
               cacheRepo = outdir, sideEffect = "destfile",
               quick = TRUE,
               omitArgs = "destfile")
  expect_true(file.exists(tf)) # recovered, with
  expect_true(file.exists(stashedFile)) # in Cache

  # Make sur the file do not exists before testing
  file.remove(destFile)
  clearCache(outdir, userTags = outputHash)
  expect_false(file.exists(file.path(outdir, basename(urlTif1))))
  expect_false(file.exists(file.path(CacheStorageDir(outdir), basename(urlTif1))))


  # Remove downloaded file and check if it is brought back using the copy
  # see message 'loading cached result from previous FUN call'
  file.remove(stashedFile)
  expect_false(file.exists(stashedFile))
  tf1 <- file.path(.reproducibleTempPath("allo"), "myFile")
  out <- Cache(utils::download.file, url = urlTif1,
               destfile = tf1,
               method = "auto", quiet = TRUE, mode = "wb", cacheOK = TRUE,
               cacheRepo = outdir, sideEffect = "destfile", quick = TRUE,
               omitArgs = "destfile")

  expect_true(file.exists(tf1))
})
