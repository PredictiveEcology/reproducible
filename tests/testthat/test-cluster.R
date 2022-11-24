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
    N <- 10

    if (!file.exists(CacheDBFile(tmpdir))) {
      if (useDBI())
        createCache(tmpdir)
    }

    # make function that will write to cache repository from with clusters
    fun <- function(x, cacheRepo) {
      #print(x)
      reproducible::Cache(rnorm, 10, sd = x, cacheRepo = cacheRepo, verbose = 0)
    }
    # Run something that will write many times
    # This will produce "database is locked" on Windows or Linux *most* of the time without the fix
    # if (interactive()) {
    #   of <- tmpfile[3]
    #   cl <- makeCluster(N, outfile = of)
    #   message(paste("log file is", of))
    # } else {
    cl <- makeCluster(N)
    # }
    # parallel::clusterEvalQ(cl, library(reproducible))
    on.exit(try(stopCluster(cl), silent = TRUE), add = TRUE)

    clusterSetRNGStream(cl)
    #    parallel::clusterEvalQ(cl, {library(reproducible)})
    numToRun <- 400
    x <- sample(1:10, size = numToRun, replace = TRUE)

    # There is a 'creating Cache at the same time' problem -- haven't resolved
    #  Just make cache first and it seems fine
    # lapply(1:10, fun, cacheRepo = tmpdir)
    st1 <- system.time(
      a <- try(clusterMap(cl = cl, fun, x, cacheRepo = tmpdir, .scheduling = "dynamic"),
               silent = FALSE))

    st2 <- system.time(b <- try(lapply(x, fun, cacheRepo = tmpdir),
                                silent = FALSE))
    # Now do again with read only so that we can compare actual outputs
    st1 <- system.time(
      d <- try(clusterMap(cl = cl, fun, x, cacheRepo = tmpdir, .scheduling = "dynamic"),
               silent = FALSE))
    if (!is(a, "try-error")) {
      expect_true(is.list(a))
      expect_true(length(a) == numToRun)
      expect_true(all.equal(d, b))
      # expect_true( 2*(st1[["elapsed"]]) < st2[["elapsed"]])

    }
    stopCluster(cl)
  }
})
