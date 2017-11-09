test_that("package-related functions work", {
  skip_on_cran()

  # These tests don't work on Rstudio, but they do work in R
  ## don't bother running checks if they'll all fail due to .gitconfig SSH issue
  # .libPaths(c(tempdir(), .libPaths()))
  # .libPaths(tempdir())
  #newLibPaths("C:/Eliot/R/win-library/3.4")
  expect_output(Require("crayon", libPath = tempdir()), "successfully")
  expect_message(Require("achubaty/amc@development", libPath = tempdir(), install_githubArgs = list(force = TRUE)), "Installing")
  detach("package:amc", unload = TRUE)
  expect_message(Require("achubaty/amc@development", libPath = tempdir(), install_githubArgs = list(force = TRUE)), "Installing")
})
