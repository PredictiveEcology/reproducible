test_that("all exported functions have examples", {
  fns <- ls("package:reproducible")
  omit <- which(fns == "cache") ## cache is deprecated, so omit it
  if (grepl("VIC-", Sys.info()["nodename"]))  # for debugging only
    cat("###############\n", file = "C:/Eliot/tmp/examples.txt", append = FALSE)
  sapply(fns[-omit], function(x) {
    warn <- capture_warning(example(x, package = "reproducible", character.only = TRUE,
                           echo = FALSE))
    if (grepl("VIC-", Sys.info()["nodename"])) { # for debugging only
      cat(paste(x, " -- ", warn, "\n"), file = "C:/Eliot/tmp/examples.txt", append = TRUE)
    }
    #expect_true(length(warn)>0)
  })
})

test_that("check all examples", {
  test_examples(path = "../../man")
})
