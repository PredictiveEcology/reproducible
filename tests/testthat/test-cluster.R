test_that("test parallel collisions", {
  skip_on_cran() # testing multi-threaded things on CRAN
  # skip_on_os("mac")
  skip_if_not_installed("parallel")

  startTime <- Sys.time()

  testInit("parallel", tmpFileExt = c(".tif", ".grd", ".txt"))

  # make cluster -- note this works if cluster is FORK also, but for simplicity, using default
  #   which works on Linux, Mac, Windows
  N <- min(2L, parallel::detectCores())

  if (useDBI()) {
    if (!file.exists(CacheDBFile(tmpdir))) {
      createCache(tmpdir)
    }
  }

  # make function that will write to cache repository from with clusters
  numNew <- 20
  fun <- function(x, cachePath, numNew, keepLog) {
    require("reproducible")
    if (keepLog) {
      fn <- file.path("c:/Eliot/tmpCache/log", Sys.getpid())
      checkPath(dirname(fn), create = TRUE)
    }
    withCallingHandlers(
      Cache(rnorm, 10,
        sd = x %% numNew + 1, cachePath = cachePath,
        verbose = 4
      ), # The "overwrite" means delete, rewrite, delete, rewrite over and over
      message = function(m) {
        if (keepLog) {
          cat(m$message, file = fn, append = TRUE)
        }
      }
    )
  }
  cl <- makeCluster(N)
  on.exit(parallel::stopCluster(cl), add = TRUE)

  keepLog <- FALSE
  if (isWindows() && Sys.info()["user"] == "emcintir") {
    # tmpdir <- 'c:/Eliot/tmpCache/'
    keepLog <- TRUE
  }

  clusterSetRNGStream(cl)
  parallel::clusterEvalQ(cl, {
    require("reproducible")
  })
  numToRun <- 140
  # if (interactive()) {
  #   print(tmpdir)
  # }

  # There is a 'creating Cache at the same time' problem -- haven't resolved
  #  Just make cache first and it seems fine
  clearCache(tmpdir) # If in c:/Eliot, then it may be reused
  Cache(rnorm, 1, cachePath = tmpdir)
  a <- try(
    clusterMap(
      cl = cl, fun, seq(numToRun), numNew = numNew, keepLog = keepLog,
      cachePath = tmpdir, .scheduling = "dynamic"
    ),
    silent = FALSE
  )
  if (!is(a, "try-error")) {
    expect_true(is.list(a))
    expect_true(length(a) == numToRun)
    news <- vapply(a, function(x) attr(x, ".Cache")$newCache, FUN.VALUE = logical(1))
    expect_equal(sum(news), numNew)
  }
  endTime <- Sys.time()
  # if (interactive()) {
  #   print(endTime - startTime)
  # }
})
