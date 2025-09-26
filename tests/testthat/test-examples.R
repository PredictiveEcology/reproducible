test_that("all exported functions have examples", {
  testInit()
  fns <- ls("package:reproducible")
  omit <- which(fns == "cache") ## cache is deprecated, so omit it

  ## for debugging only:
  tmpDir <- if (grepl("VIC-", Sys.info()["nodename"])) {
    checkPath("~/tmp/reproducible-examples", create = TRUE)
  } else {
    checkPath(file.path(tempdir(), "reproducible-examples"), create = TRUE)
  }
  tmpExFile <- file.path(tmpDir, "test-examples-out.txt")

  if (grepl("VIC-", Sys.info()["nodename"])) {
    cat("#START##############\n", file = tmpExFile, append = FALSE)
    # cat(fns[-omit], sep = "\n", file = tmpExFile, append = TRUE)
    # cat("#END##############\n", file = tmpExFile, append = TRUE)
  }

  manDir <- if (dir.exists("../../man")) {
    "../../man" ## if called during devtools::check()
  } else if (dir.exists("./man")) {
    "./man" ## if called during devtools::test()
  } else {
    system.file("man", package = "reproducible")
  }
  exFiles <- normalizePath(list.files(manDir, full.names = TRUE, pattern = "[.]Rd$"))


  # use for loop as it keeps control at top level
  # owd <- getwd()
  # tmpdir <- file.path(tmpDir, "test_Examples") |> checkPath(create = TRUE)
  # setwd(tmpdir)
  # on.exit({
  #   unlink(tmpdir, recursive = TRUE)
  #   setwd(owd)}
  #   , add = TRUE)
  if (grepl("VIC-", Sys.info()["nodename"])) { # for debugging only
    cat(paste("All files exist: ", isTRUE(all(file.exists(exFiles))), "\n"), file = tmpExFile, append = TRUE)
  }

  for (file in exFiles) {
    if (grepl("VIC-", Sys.info()["nodename"])) { # for debugging only
      cat(paste(file, " -- ", "\n"), file = tmpExFile, append = TRUE)
    }
    # for debugging only
    #if (isTRUE(grepl("emcintir|achubaty", Sys.info()[["user"]])))
    #  print(file)
    co3 <- capture.output(type = "output",
                          co2 <- capture_messages(
                            co <- capture.output(type = "message",
                                                 test_example(file)
                            ))
    )
  }
})
