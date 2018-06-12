test_that("all exported functions have examples", {
  fns <- ls("package:reproducible")
  omit <- which(fns == "cache") ## cache is deprecated, so omit it
  tmpExFile <- "C:/Eliot/tmp/examples.txt"
  if (grepl("VIC-", Sys.info()["nodename"]))  { # for debugging only
     cat("#START##############\n", file = tmpExFile, append = FALSE)
  #   cat(fns[-omit], sep = "\n", file = tmpExFile, append = TRUE)
  #   cat("#END##############\n", file = tmpExFile, append = TRUE)
  }

  exFiles <- normalizePath(dir("../../man", full.names = TRUE))
  # use for loop as it keeps control at top level
  owd <- getwd()
  tmpdir <- file.path(tempdir(), "test_Examples") %>% checkPath(create = TRUE)
  setwd(tmpdir)
  on.exit({
    unlink(tmpdir, recursive = TRUE)
    setwd(owd)}
    , add = TRUE)
  if (grepl("VIC-", Sys.info()["nodename"])) { # for debugging only
    cat(paste("All files exist: ", isTRUE(all(file.exists(exFiles))), "\n"), file = tmpExFile, append = TRUE)

  }

  for (file in exFiles) {
    if (grepl("VIC-", Sys.info()["nodename"])) { # for debugging only
      cat(paste(file, " -- ", "\n"), file = tmpExFile, append = TRUE)
    }
    # for debugging only
    print(file)
    test_example(file)
  }

  # sapply(fns[-omit], function(x) {
  #   if (grepl("VIC-", Sys.info()["nodename"])) { # for debugging only
  #     cat(paste(x, " -- ", "\n"), file = tmpExFile, append = TRUE)
  #   }
  #   browser()
  #   warn <- capture_warning(
  #     warn1 <- try(example(x, package = "reproducible", character.only = TRUE,
  #                          echo = FALSE)))
  #   if (grepl("VIC-", Sys.info()["nodename"])) { # for debugging only
  #     cat(paste(x, " -- ", warn, "\n", "  ", warn1, "\n"), file = tmpExFile, append = TRUE)
  #   }
  #   warn <- capture_warning(
  #     warn1 <- try(test_example(x, package = "reproducible", character.only = TRUE,
  #                          echo = FALSE)))
  #
  #   #expect_true(length(warn)>0)
  # })
})

# test_that("check all examples", {
#   browser()
#   test_examples(path = "../../man")
# })
