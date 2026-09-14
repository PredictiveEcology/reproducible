## With useCache = FALSE, Cache() hands the work to skipCache(). That function decided how to run
## the work from `usesDots` alone: TRUE meant "Cache(fn, args...)", so it called FUN(...). But
## `usesDots` is a heuristic on the call's arguments, and any argument Cache() does not have --
## a typo such as `.omitArgs = ` for `omitArgs = ` -- turns it on for the Cache(fn(args)) and
## fn(args) |> Cache() forms too, where `FUN` is already the call's value. The bypass then
## called that value as a function: 'could not find function "FUN"'. With caching on the same
## call worked (the stray argument was ignored), which is why this only surfaced once
## module-internal caching was switched off for a campaign (spades.useCache = "eventsOnly").

test_that("useCache = FALSE with an argument Cache() does not have still evaluates the call", {
  testInit(opts = list(reproducible.useCache = FALSE, reproducible.useMemoise = FALSE))
  f <- function(x, k = 1) x * k

  ## the call forms modules use, each with a stray argument
  expect_identical(Cache(f(2), .omitArgs = "x"), 2)
  expect_identical(f(2) |> Cache(.omitArgs = "x"), 2)
  expect_identical(Map(f = f, 1:3) |> Cache(.omitArgs = "x", .functionName = "m"), Map(f = f, 1:3))
  expect_identical(Cache(Map(f = f, x = 1:2, MoreArgs = list(k = 2)), .omitArgs = "x"),
                   Map(f = f, x = 1:2, MoreArgs = list(k = 2)))

  ## the function form, which really does take the dots, is unchanged
  expect_identical(Cache(f, 2, k = 3), 6)
  expect_identical(Cache(f, 2), 2)

  ## and nothing was written
  expect_identical(NROW(showCache(tmpCache, verbose = -2)), 0L)
})

test_that("useCache = TRUE is unchanged by the bypass fix", {
  testInit(opts = list(reproducible.useMemoise = FALSE))
  f <- function(x, k = 1) x * k
  ## cached values carry the cache attributes; c() drops them (keeping names)
  expect_equal(c(Cache(f(2), .omitArgs = "x", cachePath = tmpCache)), 2)
  expect_equal(c(Map(f = f, 1:3) |> Cache(.omitArgs = "x", cachePath = tmpCache)), Map(f = f, 1:3))
  expect_equal(c(Cache(f, 2, k = 3, cachePath = tmpCache)), 6)
  expect_gt(NROW(showCache(tmpCache, verbose = -2)), 0L)
})
