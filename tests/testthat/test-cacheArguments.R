test_that("test sideEffects makeCopy args", {

  outdir<-tempdir()
  setwd(outdir)
  url <- "http://ftp.geogratis.gc.ca/pub/nrcan_rncan/archive/vector/cli_itc_50k/land_use/L040J03.zip"

   # Make sur the file do not exists before testing
  if(file.exists(file.path(outdir, basename(url)))){
    file.remove(file.path(outdir, basename(url)))
  }
  expect_false(file.exists(file.path(outdir, basename(url))))

  # Cache download first run. File is downloaded. checksum is logged in backpack.
  out <- Cache(download.file, url = url, destfile = asPath(file.path(outdir, basename(url))),
                       method = "auto",
                       quiet = TRUE,
                       mode = "wb",
                       cacheOK = TRUE,
                       cacheRepo= outdir,
                       sideEffect = TRUE, makeCopy = FALSE, quick = TRUE)

  #check if download occured
  expect_true(file.exists(file.path(outdir, basename(url))))

  #Compare checksum from file with checksum stored in backpack
  urlfile_size <- list(basename(url), file.size(file.path(outdir, basename(url))))
  urlfile_chcksum <- digest::digest(urlfile_size, algo = "xxhash64")

  cachedchcksum <- attributes(out)$chcksumFiles
  expect_equal(paste0(file.path(basename(outdir), basename(url)), ":",urlfile_chcksum),
               cachedchcksum)

  # rerun download. Shouldn't run --ERROR IT RE-RUN
  out <- Cache(download.file, url = url, destfile = asPath(file.path(outdir, basename(url))),
                       method = "auto",
                       quiet = TRUE,
                       mode = "wb",
                       cacheOK = TRUE,
                       cacheRepo= outdir,
                       sideEffect = TRUE, makeCopy = FALSE, quick = TRUE)


  # Make sur the file do not exists before testing
  toRemove <- list("backpack.db", basename(url))
  lapply(toRemove, function (x) if(file.exists(file.path(outdir, x))){
    file.remove(file.path(outdir, x))
  })
  expect_false(file.exists(file.path(outdir, basename(url))))
  expect_false(file.exists(file.path(outdir, "backpack.db")))

  # Test MakeCopy = TRUE
  out <- Cache(download.file, url = url, destfile = asPath(file.path(outdir, basename(url))),
                       method = "auto",
                       quiet = TRUE,
                       mode = "wb",
                       cacheOK = TRUE,
                       cacheRepo= outdir,
                       sideEffect = TRUE, makeCopy = TRUE, quick = TRUE)

  # check if copy was created
  copyFolder <- file.path(outdir,"gallery")
  expect_true(file.exists(file.path(copyFolder, basename(url))))

  # Remove downloaded file and check if it is brought back using the copy
  # see message 'loading cached result from previous FUN call'
  file.remove(file.path(outdir, basename(url)))
  expect_false(file.exists(file.path(outdir, basename(url))))
  out <- Cache(download.file, url = url, destfile = asPath(file.path(outdir, basename(url))),
                       method = "auto",
                       quiet = TRUE,
                       mode = "wb",
                       cacheOK = TRUE,
                       cacheRepo= outdir,
                       sideEffect = TRUE, makeCopy = TRUE, quick = TRUE)

  expect_true(file.exists(file.path(outdir, basename(url))))
})
