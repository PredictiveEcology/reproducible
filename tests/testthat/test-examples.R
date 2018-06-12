test_that("all exported functions have examples", {
  fns <- ls("package:reproducible")
  omit <- which(fns == "cache") ## cache is deprecated, so omit it

  if (grepl("VIC-", Sys.info()["nodename"]))  {
    # for debugging only
    cat("#START##############\n", file = "C:/Eliot/tmp/examples.txt", append = FALSE)
    cat(fns[-omit], sep = "\n", file = "C:/Eliot/tmp/examples.txt", append = TRUE)
    cat("#END##############\n", file = "C:/Eliot/tmp/examples.txt", append = TRUE)
  }

  nohelp <- vapply(fns[-omit], function(x) {
    # for debugging only
    if (grepl("VIC-", Sys.info()["nodename"])) {
      cat(paste(x, " -- ", "\n"), file = "C:/Eliot/tmp/examples.txt", append = TRUE)
    }
    warn <- capture_warning(warn1 <- try(example(x, package = "reproducible", character.only = TRUE,
                                                 echo = FALSE)))

    # for debugging only
    if (grepl("VIC-", Sys.info()["nodename"])) {
      cat(paste(x, " -- ", warn, "\n", "  ", warn1, "\n"), file = "C:/Eliot/tmp/examples.txt", append = TRUE)
    }

    #expect_gt(length(warn), 0) ## TODO: what was this for? remove?

    grepl("no help found", warn$message)
  }, logical(1))
  names(nohelp) <- fns[-omit]

  #expect_identical(sum(nohelp), 0) ## TODO: implement this check/test
})

test_that("check all examples", {
  test_examples(path = "../../man")
})
