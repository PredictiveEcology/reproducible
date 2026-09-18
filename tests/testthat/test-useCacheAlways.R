## useCache = "always": the call is cached even when options(reproducible.useCache = FALSE) --
## which is what SpaDES.core's spades.useCache = "eventsOnly" sets for a run -- and even when it
## is nested inside a Cache() call whose useCache is FALSE. It is this call's own setting: a
## Cache() nested inside an "always" call keeps its own behaviour.

test_that("useCache = 'always' caches despite the option and any outer Cache(useCache = FALSE)", {
  testInit(verbose = -1, opts = list(reproducible.useCache = FALSE))
  withr::local_options(reproducible.cachePath = tmpCache)

  n <- 0
  inner <- function(x) {
    n <<- n + 1
    x
  }
  innerAlways <- function(x) Cache(inner(x), useCache = "always")
  outer <- function(y, x) innerAlways(x)
  outerTagged <- function(y, x) Cache(outer(y, x), userTags = c("outerTagged", y))

  ## each case calls the "always" function twice with the same argument: it must run once
  runs <- function(expr) {
    n <<- 0
    for (y in 1:2) eval(expr, list(y = y))
    n
  }
  ## top level
  expect_equal(runs(quote(innerAlways(1))), 1)
  ## inside a Cache() that takes the option (FALSE), without and with userTags
  expect_equal(runs(quote(Cache(outer(y, 2)))), 1)
  expect_equal(runs(quote(Cache(outer(y, 3), userTags = c("outer", y)))), 1)
  ## inside an explicit Cache(useCache = FALSE), with userTags
  expect_equal(runs(quote(Cache(outer(y, 4), useCache = FALSE, userTags = c("outer", y)))), 1)
  ## inside an event-level Cache(useCache = TRUE) > option-FALSE Cache with userTags
  expect_equal(runs(quote(Cache(outerTagged(y, 5), useCache = TRUE, userTags = "event"))), 1)
})

test_that("useCache = 'always' otherwise behaves as TRUE: same key", {
  testInit(verbose = -1)
  withr::local_options(reproducible.cachePath = tmpCache)

  n <- 0
  f <- function(x) {
    n <<- n + 1
    x
  }
  Cache(f(1), useCache = TRUE)
  Cache(f(1), useCache = "always")
  expect_equal(n, 1) # the TRUE call's entry is hit: useCache is not part of the key
  Cache(f(2), useCache = "always")
  expect_equal(n, 2) # and a new argument is a miss
})

test_that("useCache = 'always' does not transfer to Cache() calls nested inside it", {
  testInit(verbose = -1, opts = list(reproducible.useCache = FALSE))
  withr::local_options(reproducible.cachePath = tmpCache)

  m <- 0
  inner <- function(x) {
    m <<- m + 1
    x
  }
  ## an inner Cache() that takes the option (FALSE) is still skipped
  outerDefault <- function(y) Cache(inner(1), userTags = "inner")
  for (y in 1:2) Cache(outerDefault(y), useCache = "always", userTags = c("outer", y))
  expect_equal(m, 2)

  ## an inner explicit TRUE is cached, as TRUE: what nested calls inherit from an "always"
  ## call is TRUE, not "always"
  inherited <- list()
  outerTRUE <- function(y) {
    inherited[[y]] <<- reproducible:::.pkgEnv$.reproEnv2$useCache
    Cache(inner(2), useCache = TRUE, userTags = "inner")
  }
  m <- 0
  for (y in 1:2) Cache(outerTRUE(y), useCache = "always", userTags = c("outerTRUE", y))
  expect_equal(m, 1)
  expect_identical(inherited, list(TRUE, TRUE))
})
