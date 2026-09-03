## padYears(): both years must come back at the *same* width, taken from the
## end year, so that the resulting filenames sort correctly.

test_that("padYears pads both years to the end year's width", {
  py <- padYears(c(2011, 2100))

  expect_identical(py$padL, 4)
  expect_identical(py$start, "2011")
  expect_identical(py$end, "2100")
})

test_that("padYears widens the start year to match a wider end year", {
  py <- padYears(c(1, 100))

  expect_identical(py$padL, 3)
  expect_identical(py$start, "001")
  expect_identical(py$end, "100")
  expect_identical(nchar(py$start), nchar(py$end))
})

test_that("padYears agrees with paddedFloatToChar's own default for the end year", {
  ## the padL formula is paddedFloatToChar's default when x is the end year
  for (yrs in list(c(1, 100), c(2011, 2100), c(0, 9), c(1991, 2020))) {
    expect_identical(padYears(yrs)$end, paddedFloatToChar(yrs[2]))
  }
})

test_that("padYears does not pad right of the decimal for whole years", {
  expect_false(grepl("\\.", padYears(c(2011, 2100))$start))
})
