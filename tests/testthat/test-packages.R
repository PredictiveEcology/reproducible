test_that("package-related functions work", {
  skip_on_cran()

  ## don't bother running checks if they'll all fail due to .gitconfig SSH issue
  .libPaths(tempdir())
  #newLibPaths("C:/Eliot/R/win-library/3.4")
  library(reproducible)
  Require("crayon")
})
