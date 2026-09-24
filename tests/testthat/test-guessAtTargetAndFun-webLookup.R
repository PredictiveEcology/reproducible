## When the caller supplies both the file and `fun`, there is nothing to guess, but
## .guessAtTargetAndFun() still fetched the sf "guessing a driver" web page (with rvest) for any
## extension it did not know. fireSenseUtils' lightning files (.txt, read by an explicit `fun`)
## made every ignition-data run need rvest and the network for nothing.

test_that("a file with an unknown extension and an explicit fun does not look anything up", {
  testInit()
  tmpd <- withr::local_tempdir()
  writeLines(c("a b", "1 2"), file.path(tmpd, "x.txt"))
  local_mocked_bindings(checkSFWebPage = function(...) stop("looked up the sf web page"))

  out <- prepInputs(targetFile = "x.txt", destinationPath = tmpd,
                    fun = quote(utils::read.table(targetFile, header = TRUE)), useCache = FALSE)
  expect_identical(out, data.frame(a = 1L, b = 2L))
})

test_that("without fun, an unknown extension still consults the sf list", {
  testInit()
  tmpd <- withr::local_tempdir()
  writeLines(c("a b", "1 2"), file.path(tmpd, "x.txt"))
  looked <- FALSE
  local_mocked_bindings(checkSFWebPage = function(funPoss, ...) {
    looked <<- TRUE
    funPoss
  })

  .guessAtTargetAndFun(file.path(tmpd, "x.txt"), destinationPath = tmpd,
                       filesExtracted = character(), fun = NULL)
  expect_true(looked)
})
