##########################
test_that("test multiple cachePath", {
  testInit()

  withr::local_options(
    "reproducible.cachePath" = c(tmpdir, tmpCache),
    reproducible.useMemoise = TRUE)

  for (memoise in c(TRUE, FALSE)) {
    options(reproducible.useMemoise = memoise)
    a <- Cache(rnorm, 1, cachePath = tmpCache)
    suppressMessages(aCache <- showCache(tmpCache))
    expect_true(length(unique(aCache[[.cacheTableHashColName()]])) == 1)

    b <- Cache(rnorm, 2, cachePath = tmpdir)
    suppressMessages(bCache <- showCache(tmpdir))
    expect_true(length(unique(bCache[[.cacheTableHashColName()]])) == 1)

    d <- Cache(rnorm, 1, cachePath = c(tmpdir, tmpCache))
    suppressMessages(dCache <- showCache(tmpCache))
    expect_true(length(unique(dCache[[.cacheTableHashColName()]])) == 1)

    f <- Cache(rnorm, 2, cachePath = c(tmpdir, tmpCache))
    suppressMessages(fCache <- showCache(tmpdir))
    expect_true(length(unique(fCache[[.cacheTableHashColName()]])) == 1)

    d <- Cache(rnorm, 1)
    suppressMessages(dCache <- showCache())
    expect_true(length(unique(dCache[[.cacheTableHashColName()]])) == 1)

    f <- Cache(rnorm, 2)
    suppressMessages({
      fCache <- showCache()
    })
    expect_true(length(unique(fCache[[.cacheTableHashColName()]])) == 1)

    clearCache(tmpdir)
    clearCache(tmpCache)
  }

})

