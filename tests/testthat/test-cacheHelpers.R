##########################
test_that("test miscellaneous unit tests cache-helpers", {
  testInitOut <- testInit()
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  a <- 1
  mess <- capture_message(.cacheMessage(a, "test", TRUE))
  expect_true(any(grepl("loading memoised", mess)))

  mess <- capture_message(.cacheMessage(a, "test", FALSE))
  expect_true(any(grepl("loading cached.*adding", mess)))

  mess <- capture_message(.cacheMessage(a, "test", NA))
  expect_true(any(grepl("loading cached", mess)))
  expect_false(all(grepl("adding", mess)))

  # .checkCacheRepo
  options(reproducible.cachePath = dirname(tmpdir))
  mess <- capture_message(.checkCacheRepo(a))
  expect_true(any(grepl("No cacheRepo supplied and getOption\\('reproducible.cachePath'\\) is the temporary", mess)))

  opt <- options("reproducible.cachePath" = NULL)
  on.exit({
    options(opt)
  }, add = TRUE)
  mess <- capture_message(.checkCacheRepo(a))
  expect_true(any(grepl("No cacheRepo supplied. Using tempdir()", mess)))

  # getFunctionName
  fn <- function(FUN) {
    getFunctionName(fn, isPipe = FALSE, overrideCall = "fn")
  }
  expect_true(fn(1)$functionName == "FUN")

  fn <- function(FUN) {
    getFunctionName(fn, isPipe = FALSE, overrideCall = "fn")
  }
  expect_true(fn(2)$functionName == "FUN")

  fn <- function(FUN) {
    getFunctionName(1, isPipe = FALSE, overrideCall = "fn")
  }
  expect_true(fn(2)$functionName == "FUN")
  expect_true(is.null(fn(2)$.FUN))

  fn <- function(FUN) {
    getFunctionName(1, isPipe = FALSE, overrideCall = "fn")
  }
  expect_true(fn(log(1))$functionName== "FUN")

  ## nextNumericName
  b <- nextNumericName("test.pdf")
  b1 <- nextNumericName(b)
  expect_true(grepl("_2.pdf", b1))
  aMess <- capture_messages(a <- Cache(rnorm, 1, useCache = FALSE, cacheRepo = tmpCache))
  bMess <- capture_messages(b <- Cache(rnorm, 1, useCache = FALSE, cacheRepo = tmpCache))
  expect_false(identical(a,b))
  expect_true(grepl("skipping Cache", aMess))
  expect_true(grepl("skipping Cache", bMess))

  ## getOption("reproducible.useMemoise" = FALSE)
  opt <- options("reproducible.useMemoise" = FALSE)
  aMess <- capture_messages(a <- Cache(rnorm, 1, cacheRepo = tmpCache))
  bMess <- capture_messages(a <- Cache(rnorm, 1, cacheRepo = tmpCache))
  options(opt)
  cMess <- capture_messages(a <- Cache(rnorm, 1, cacheRepo = tmpCache))
  dMess <- capture_messages(a <- Cache(rnorm, 1, cacheRepo = tmpCache))
  #expect_true(identical(aMess, bMess[1]))
  expect_false(any(grepl("memoise", bMess)))
  expect_true(any(grepl("memoise", dMess)))

  ## showSimilar
  try(clearCache(ask = FALSE, x = tmpCache), silent = TRUE)
  aMess <- capture_messages(a <- Cache(rnorm, 1, cacheRepo = tmpCache))
  bMess <- capture_messages(b <- Cache(rnorm, 2, showSimilar = TRUE, cacheRepo = tmpCache))
  expect_true(any(grepl("different n", bMess)))

  ## debugCache -- "complete"
  thing <- 1
  aa <- Cache(rnorm, thing, debugCache = "complete", cacheRepo = tmpCache)
  expect_true(identical(thing, attr(aa, "debugCache1")[[1]]))
  expect_true(identical(.robustDigest(thing), attr(aa, "debugCache2")$n))
  expect_true(is.numeric(aa))

  ## debugCache -- "quick"
  aa <- Cache(rnorm, thing, debugCache = "quick", cacheRepo = tmpCache)
  expect_true(identical(.robustDigest(thing), aa$hash$n))
  expect_true(identical(thing, aa$content[[1]]))
  ## cache -- deprecated
  # aMess <- capture_warnings(a <- reproducible::cache(cacheRepo = tmpCache, rnorm, 1))
  # expect_true(grepl("deprecated", aMess))

  ## .unlistToCharacter
  expect_true(grepl("not list", unlist(.unlistToCharacter(1, 1))))
  expect_true(grepl("other", unlist(.unlistToCharacter(1, 0))))

  ## writeFuture
  expect_true(identical("dda1fbb70d256e6b3b696ef0176c63de",
                        writeFuture(1, "sdf", cacheRepo = tmpCache, userTags = "")))
  expect_error(writeFuture(1, "sdf", cacheRepo = "sdfd", userTags = ""))

  ## verbose -- need 2 nested levels to run all lin
  fn <- function(a) {
    Cache(fn1, cacheRepo = tmpCache, verbose = 2)
  }
  fn1 <- function() {
    2
  }

  try(silent = TRUE, clearCache(tmpCache, ask = FALSE))
  bMess <- capture_output(aMess <- capture_messages(aa <- Cache(fn, 1, verbose = 2, cacheRepo = tmpCache)))
  expect_true(any(grepl("fn1", bMess))) # TODO: fix this;
  expect_true(any(grepl("The hashing details", aMess)))
})

