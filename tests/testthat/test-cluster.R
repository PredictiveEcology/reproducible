test_that("test parallel collisions", {
  skip_on_cran() # testing multi-threaded things on CRAN
  testInitOut <- testInit("raster", tmpFileExt = c(".tif", ".grd", ".txt"))
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  if (require(parallel, quietly = TRUE)) {
    # make cluster -- note this works if cluster is FORK also, but for simplicity, using default
    #   which works on Linux, Mac, Windows
    N <- min(2, detectCores())

    # make archivist repository
    if (!file.exists(CacheDBFile(tmpdir))) {
      if (useDBI())
        createCache(tmpdir)
      else
        archivist::createLocalRepo(tmpdir)
    }

    # make function that will write to archivist repository from with clusters
    fun <- function(x, cacheRepo) {
      #print(x)
      Cache(rnorm, 10, sd = x, cacheRepo = cacheRepo)
    }
    # Run something that will write many times
    # This will produce "database is locked" on Windows or Linux *most* of the time without the fix
    if (interactive()) {
      of <- tmpfile[3]
      cl <- makeCluster(N, outfile = of)
    } else {
      cl <- makeCluster(N)
    }
    on.exit(stopCluster(cl), add = TRUE)

    clusterSetRNGStream(cl)
    parallel::clusterEvalQ(cl, {library(reproducible)})
    # clusterEvalQ(cl = cl, {
    #   devtools::load_all()
    # })
    numToRun <- 40
    skip_on_os("mac")
    a <- try(clusterMap(cl = cl, fun, seq(numToRun), cacheRepo = tmpdir, .scheduling = "dynamic"),
             silent = FALSE)
    expect_false(is(a, "try-error"))
    expect_true(is.list(a))
    expect_true(length(a) == numToRun)
  }
})
