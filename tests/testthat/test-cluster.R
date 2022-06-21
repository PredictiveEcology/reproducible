test_that("test parallel collisions", {
  skip_on_cran() # testing multi-threaded things on CRAN
  skip_on_os("mac")

  setwd("~/GitHub/reproducible")
  workingDir <- getwd()
  testInitOut <- testInit("parallel", tmpFileExt = c(".log", ".log", ".log"))
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  tmpfile <- file.path(paste0("~/tmp/out", 1:3, ".txt"))
  cat(" ", file = tmpfile[3])

  if (!require(parallel, quietly = TRUE)) skip("Need parallel")
  # make cluster -- note this works if cluster is FORK also, but for simplicity, using default
  #   which works on Linux, Mac, Windows
  N <- min(2, detectCores())

  if (!file.exists(CacheDBFile(tmpdir))) {
    createCache(tmpdir)
  }

  # make function that will write to cache repository from with clusters
  fun <- function(x, cacheRepo) {
    # options(reproducible.verbose = 3)
    #out <- try(
      Cache(rnorm, 10, sd = x, cacheRepo = cacheRepo)#, verbose = 3)
#      )
    # if (is(out, "try-error")) {
    #   cat(capture.output(warnings()), sep = "\n", file = tmpfile[3])
    #   cat(out, file = tmpfile[3], append = TRUE, sep = "\n")
    # } else {
    #   cat(x, file = tmpfile[3], append = TRUE, sep = "\n")
    # }
    # out

  }
  # Run something that will write many times
  # This will produce "database is locked" on Windows or Linux *most* of the time without the fix
  if (interactive()) {
    of <- tmpfile[3]
    cl <- makeCluster(N, outfile = of)
    message(paste("log file is", of))
  } else {
    cl <- makeCluster(N)
  }
  on.exit(try(stopCluster(cl), silent = TRUE), add = TRUE)

  clusterSetRNGStream(cl)
  numToRun <- if (interactive()) 4000 else 40

  # There is a 'creating Cache at the same time' problem -- haven't resolved
  #  Just make cache first and it seems fine
  Cache(rnorm, 1, cacheRepo = tmpdir)
  parallel::clusterExport(cl, varlist = c("workingDir", "tmpfile"), envir = environment())
  outs <- parallel::clusterEvalQ(cl, {#options("reproducible.drv" = 'csv');
    library(reproducible)})

  a <- clusterMap(cl = cl, fun, seq(numToRun), cacheRepo = tmpdir, .scheduling = "dynamic")
  expect_true(is.list(a))
  expect_true(length(a) == numToRun)
  expect_true(all(sapply(a, function(aa) attr(aa, ".Cache")$newCache == TRUE)))

})
