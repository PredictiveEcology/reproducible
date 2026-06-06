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
  testInit(.qs2Format, opts = list(reproducible.cacheSaveFormat = .qs2Format,
                             reproducible.cacheSpeed = "fast"))

  for (i in .cacheSaveFormats) {
    if (.requireNamespace(i)) {
      for (s in c("slow", "fast")) {
      withr::local_options(reproducible.cacheSaveFormat = i,
                           reproducible.cacheSpeed = s)
      a <- 1991:20200
      aDig <- .robustDigest(a)
      tf <- tempfile(fileext = i);
      if (identical(i, .rdsFormat)) {
        saveRDS(a, file = tf);
        b <- readRDS(tf)
      } else if (i %in% c(.qs2Format, .qsFormat)) {
        fek <- .fileExtsKnown()
        funSave <- fek$saveFun[fek$extension == i]
        funSave <- eval(parse(text = funSave))
        funRead <- fek$fun[fek$extension == i]
        funRead <- eval(parse(text = funRead))
        funSave(a, file = tf);
        b <- funRead(tf)
      } else {

      }
      bDig <- .robustDigest(b)
      expect_true(identical(aDig, bDig))
      withr::deferred_run()
      }
    }

  }
})

test_that(".robustDigest realizes deferred-string ALTREP for a stable cross-platform digest (v4)", {
  withr::local_options(reproducible.digestVersion = 4L)
  ## A character vector must digest identically whether it is a plain STRSXP or a
  ## deferred-string ALTREP. A deferred string serializes differently from its
  ## realized form -- and differently across R versions/platforms -- which is what
  ## split .inputObjects cacheIds between Linux and Windows. The fix realizes it via
  ## `object[]`; it is a no-op for an already-materialized vector (so existing
  ## cacheIds are unchanged). On platforms that don't create the ALTREP these are
  ## already identical; the invariant must hold everywhere.
  x <- c("alpha", ".useCache", "beta", "gamma")
  expect_identical(.robustDigest(x), .robustDigest(x[]))
  ## same content nested in a data.frame column (the parameter-table path)
  df1 <- data.frame(p = x, stringsAsFactors = FALSE)
  df2 <- df1; df2$p <- df2$p[]
  expect_identical(.robustDigest(df1), .robustDigest(df2))
})
