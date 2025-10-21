test_that("test data.table caching", {
  testInit("data.table")

  # make sure that .robustDigest does not include the cache-added attributes
  c1 <- Cache(rnorm, 11, cachePath = tmpCache)
  c1a <- .robustDigest(c1)

  c1 <- Cache(rnorm, 11, cachePath = tmpCache)
  c1b <- .robustDigest(c1)
  expect_true(identical(c1a, c1b)) # failed pre reproducible 0.2.4.9000

  # This was wrong on with .robustDigest with no data.frame method -- this fails previous version
  a <- list(data.table(a = LETTERS, b = letters, c = letters, d = letters, e = letters))
  b <- list(data.table(a = rep(LETTERS, 2), b = rep(letters, 2), c = rep(letters, 2), d = rep(letters, 2), e = rep(letters, 2)))
  aC <- CacheDigest(a)
  bC <- CacheDigest(b)
  expect_false(identical(aC, bC))
})

test_that("test ALTREP integers", {
  testInit(.qsFormat, opts = list(reproducible.cacheSaveFormat = .qsFormat,
                             reproducible.cacheSpeed = "fast"))

  for (i in c(.rdsFormat, .qsFormat)) {
    for (s in c("slow", "fast")) {
      withr::local_options(reproducible.cacheSaveFormat = i,
                           reproducible.cacheSpeed = s)

      a <- 1991:20200
      aDig <- .robustDigest(a)
      tf <- tempfile(fileext = i);
      if (identical(i, .rdsFormat)) {
        saveRDS(a, file = tf);
        b <- readRDS(tf)
      } else {
        qs::qsave(a, file = tf);
        b <- qs::qread(tf)
      }
      bDig <- .robustDigest(b)
      expect_true(identical(aDig, bDig))
      withr::deferred_run()
    }}

})
