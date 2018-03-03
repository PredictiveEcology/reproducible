test_that("test cached downloads", {
  outdir <- file.path(tempdir(), "test-cached-downloads")
  expect_true(dir.create(outdir))

  on.exit({
    unlink(outdir, recursive = TRUE)
  })
  setwd(outdir)
  url <- "http://ftp.geogratis.gc.ca/pub/nrcan_rncan/archive/vector/cli_itc_50k/land_use/L040J03.zip" # nolint

   # ensure the file does not exist before testing
  if (file.exists(file.path(outdir, basename(url)))) {
    file.remove(file.path(outdir, basename(url)))
  }
  expect_false(file.exists(file.path(outdir, basename(url))))

  # Cache download first run. File is downloaded. checksum is logged in backpack.
  out <- Cache(utils::download.file, url = url,
               destfile = asPath(file.path(outdir, basename(url))),
               method = "auto", quiet = TRUE, mode = "wb", cacheOK = TRUE,
               cacheRepo = outdir, sideEffect = TRUE, makeCopy = FALSE, quick = TRUE)

  # check if download occured
  expect_true(file.exists(file.path(outdir, basename(url))))

  # compare checksum from file with checksum stored in backpack
  urlfileSize <- list(basename(url), file.size(file.path(outdir, basename(url))))
  urlfileChcksum <- digest::digest(urlfileSize, algo = "xxhash64")

  cachedChcksum <- attributes(out)$chcksumFiles
  expect_equal(paste0(file.path(basename(outdir), basename(url)), ":", urlfileChcksum),
               cachedChcksum)

  # rerun download. Shouldn't run. # TODO: this shouldn't be rerunning
  out <- Cache(utils::download.file, url = url,
               destfile = asPath(file.path(outdir, basename(url))),
               method = "auto", quiet = TRUE, mode = "wb", cacheOK = TRUE,
               cacheRepo = outdir, sideEffect = TRUE, makeCopy = FALSE, quick = TRUE)

  # Make sur the file do not exists before testing
  toRemove <- list("backpack.db", basename(url))
  lapply(toRemove, function(x) {
    if (file.exists(file.path(outdir, x))) file.remove(file.path(outdir, x))
  })
  expect_false(file.exists(file.path(outdir, basename(url))))
  expect_false(file.exists(file.path(outdir, "backpack.db")))

  # Test MakeCopy = TRUE
  out <- Cache(utils::download.file, url = url,
               destfile = asPath(file.path(outdir, basename(url))),
               method = "auto", quiet = TRUE, mode = "wb", cacheOK = TRUE,
               cacheRepo = outdir, sideEffect = TRUE, makeCopy = TRUE, quick = TRUE)

  # check if copy was created
  copyFolder <- file.path(outdir, "gallery")
  expect_true(file.exists(file.path(copyFolder, basename(url))))

  # Remove downloaded file and check if it is brought back using the copy
  # see message 'loading cached result from previous FUN call'
  file.remove(file.path(outdir, basename(url)))
  expect_false(file.exists(file.path(outdir, basename(url))))
  out <- Cache(utils::download.file, url = url,
               destfile = asPath(file.path(outdir, basename(url))),
               method = "auto", quiet = TRUE, mode = "wb", cacheOK = TRUE,
               cacheRepo = outdir, sideEffect = TRUE, makeCopy = TRUE, quick = TRUE)

  expect_true(file.exists(file.path(outdir, basename(url))))
})
