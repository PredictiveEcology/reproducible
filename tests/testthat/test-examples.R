test_that("all exported functions have examples", {
  fns <- ls("package:reproducible")
  omit <- which(fns == "cache") ## cache is deprecated, so omit it
  if (grepl("VIC-", Sys.info()["nodename"]))  { # for debugging only
    cat("#START##############\n", file = "C:/Eliot/tmp/examples.txt", append = FALSE)
    cat(fns[-omit], sep = "\n", file = "C:/Eliot/tmp/examples.txt", append = TRUE)
    cat("#END##############\n", file = "C:/Eliot/tmp/examples.txt", append = TRUE)
  }
  sapply(fns[-omit], function(x) {
    if (grepl("VIC-", Sys.info()["nodename"])) { # for debugging only
      cat(paste(x, " -- ", "\n"), file = "C:/Eliot/tmp/examples.txt", append = TRUE)
    }
    warn <- capture_warning(warn1 <- try(example(x, package = "reproducible", character.only = TRUE,
                           echo = FALSE)))
    if (grepl("VIC-", Sys.info()["nodename"])) { # for debugging only
      cat(paste(x, " -- ", warn, "\n", "  ", warn1, "\n"), file = "C:/Eliot/tmp/examples.txt", append = TRUE)
    }
    #expect_true(length(warn)>0)
  })
})

test_that("check all examples", {
  test_examples(path = "../../man")
})
