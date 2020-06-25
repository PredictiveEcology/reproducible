test_that("test parallel collisions", {
  skip_on_cran() # testing multi-threaded things on CRAN
  skip_on_os("mac")

  testInitOut <- testInit("raster", tmpFileExt = c(".tif", ".grd", ".txt"))
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  if (require(parallel, quietly = TRUE)) {
    # make cluster -- note this works if cluster is FORK also, but for simplicity, using default
    #   which works on Linux, Mac, Windows
    N <- min(2, detectCores())

    if (!file.exists(CacheDBFile(tmpdir))) {
      if (useDBI())
        createCache(tmpdir)
    }

    # make function that will write to cache repository from with clusters
    fun <- function(x, cacheRepo) {
      #print(x)
      Cache(rnorm, 10, sd = x, cacheRepo = cacheRepo)
    }
    # Run something that will write many times
    # This will produce "database is locked" on Windows or Linux *most* of the time without the fix
    if (interactive()) {
      of <- tmpfile[3]
      cl <- makeCluster(N, outfile = of)
      print(paste("log file is", of))
    } else {
      cl <- makeCluster(N)
    }
    on.exit(stopCluster(cl), add = TRUE)

    clusterSetRNGStream(cl)
    parallel::clusterEvalQ(cl, {library(reproducible)})
    numToRun <- 40

    # There is a 'creating Cache at the same time' problem -- haven't resolved
    #  Just make cache first and it seems fine
    Cache(rnorm, 1, cacheRepo = tmpdir)
    a <- try(clusterMap(cl = cl, fun, seq(numToRun), cacheRepo = tmpdir, .scheduling = "dynamic"),
             silent = FALSE)
    if (!is(a, "try-error")) {
      expect_true(is.list(a))
      expect_true(length(a) == numToRun)
    }
  }
})
