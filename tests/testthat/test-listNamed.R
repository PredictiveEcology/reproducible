test_that("listNamed captures object names from caller", {
  a <- 1
  b <- "two"
  d <- list(3)
  out <- listNamed(a, b, d)
  expect_named(out, c("a", "b", "d"))
  expect_identical(out$a, 1)
  expect_identical(out$b, "two")
  expect_identical(out$d, list(3))
})

test_that("listNamed keeps manually-supplied names", {
  a <- 1
  d <- 3
  out <- listNamed(a, dManual = d)
  expect_named(out, c("a", "dManual"))
  expect_identical(out$dManual, 3)
})

test_that("listNamed mixes positional and named args", {
  x <- 10
  y <- 20
  out <- listNamed(x, custom = y, z = 30)
  expect_named(out, c("x", "custom", "z"))
  expect_identical(out$x, 10)
  expect_identical(out$custom, 20)
  expect_identical(out$z, 30)
})

test_that("listNamed with no args returns an empty named list", {
  out <- listNamed()
  expect_type(out, "list")
  expect_length(out, 0)
})

test_that("listNamed deparses expressions as names", {
  out <- listNamed(1 + 1)
  expect_named(out, "1 + 1")
  expect_identical(out[[1]], 2)
})
