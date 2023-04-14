test_that("test parallel collisions", {
  skip_on_cran() # testing multi-threaded things on CRAN
  skip_on_os("mac")
  startTime <- Sys.time()

  testInitOut <- testInit("parallel", tmpFileExt = c(".tif", ".grd", ".txt"))
  on.exit({
    try(testOnExit(testInitOut), silent = TRUE)
  }, add = TRUE)

  if (require(parallel, quietly = TRUE)) {
    # make cluster -- note this works if cluster is FORK also, but for simplicity, using default
    #   which works on Linux, Mac, Windows
    N <- min(4, detectCores())

    if (useDBI())
      if (!file.exists(CacheDBFile(tmpdir))) {
        createCache(tmpdir)
      }

    # make function that will write to cache repository from with clusters
    fun <- function(x, cachePath) {
      #print(x)
      Cache(rnorm, 10, sd = x %% 2 + 1, cachePath = cachePath,
            # useCache = "overwrite",
            verbose = 4) # The "overwrite" means delete, rewrite, delete, rewrite over and over
    }
    # Run something that will write many times
    # This will produce "database is locked" on Windows or Linux *most* of the time without the fix
    if (interactive()) {
      of <- tmpfile[3]
      of <- "c:/Eliot/tmpCache/log.txt"
      cl <- makeCluster(N, outfile = of)
      print(paste("log file is", of))
    } else {
      cl <- makeCluster(N)
    }
    on.exit(stopCluster(cl), add = TRUE)

    tmpdir <- 'c:/Eliot/tmpCache/'
    clusterSetRNGStream(cl)
    parallel::clusterEvalQ(cl, {
      # options(reproducible.useMultipleDBFiles = TRUE)
      library(reproducible)
      })
    numToRun <- 140
    if (interactive())
      print(tmpdir)

    # There is a 'creating Cache at the same time' problem -- haven't resolved
    #  Just make cache first and it seems fine
    Cache(rnorm, 1, cachePath = tmpdir)
    a <- try(clusterMap(cl = cl, fun, seq(numToRun), cachePath = tmpdir,
                        .scheduling = "dynamic"),
             silent = FALSE)
    if (!is(a, "try-error")) {
      expect_true(is.list(a))
      expect_true(length(a) == numToRun)
    }
  }
  endTime <- Sys.time()
  if (interactive())
    print(endTime - startTime)
})
