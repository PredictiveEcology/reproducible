## Coverage for the argument-marshalling helpers in R/GPT2.R that Cache() uses
## to build a stable, digestible argument list before hashing.
##
## Their correctness matters more than it looks: reorder_arguments() decides
## what goes into the cacheId, so if it produced a different ordering for the
## same logical call, identical calls would miss the cache.
##
## No network, no Drive.

test_that("reorder_arguments normalises argument order and fills defaults", {
  testInit()

  f <- function(a, b, c = 3) NULL

  ## Supplied in order.
  expect_identical(
    reorder_arguments(formals(f), list(a = 1, b = 2)),
    list(a = 1, b = 2, c = 3)
  )

  ## Supplied out of order -> SAME result. This is the property that makes the
  ## cacheId stable: f(a = 1, b = 2) and f(b = 2, a = 1) are the same call and
  ## must hash the same.
  expect_identical(
    reorder_arguments(formals(f), list(b = 2, a = 1)),
    reorder_arguments(formals(f), list(a = 1, b = 2))
  )

  ## Defaults are materialised rather than left missing, so a call that relies
  ## on a default hashes the same as one that passes it explicitly.
  expect_identical(
    reorder_arguments(formals(f), list(a = 1, b = 2, c = 3)),
    reorder_arguments(formals(f), list(a = 1, b = 2))
  )

  ## An explicit value overrides the default.
  expect_identical(reorder_arguments(formals(f), list(a = 1, b = 2, c = 99))$c, 99)
})

test_that("reorder_arguments flattens dots into the argument list", {
  testInit()

  g <- function(a, ...) NULL

  ## Arguments arriving via `...` are spliced in alongside the named ones, so
  ## they are digested individually rather than as one opaque list.
  out <- reorder_arguments(formals(g), list(a = 1, `...` = list(z = 9)))

  expect_identical(out$a, 1)
  expect_identical(out$z, 9)
  expect_false("..." %in% names(out))
})

test_that("filter_objects narrows a list argument to the named objects", {
  testInit()

  ## Cache(.objects = ) means "hash only these elements of the list/environment
  ## argument", so a change to an unlisted element does not invalidate the cache.
  evaluated <- list(dat = list(a = 1, b = 2, c = 3), other = 5)
  out <- filter_objects(evaluated, c("a", "c"))

  expect_named(out$dat, c("a", "c"))
  expect_identical(out$dat$a, 1)
  expect_identical(out$dat$c, 3)

  ## Only the first list-like argument is narrowed; everything else passes
  ## through untouched.
  expect_identical(out$other, 5)
})

test_that("filter_objects passes through when no list argument is present", {
  testInit()

  ## Nothing list-like to narrow -> the arguments are returned unchanged rather
  ## than erroring, so .objects is harmless on a call it does not apply to.
  evaluated <- list(x = 1, y = "two")
  expect_identical(filter_objects(evaluated, c("a", "b")), evaluated)
})
