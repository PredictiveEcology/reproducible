## A 3.4 GB SCANFI download on the GPSC (2026-10-27) ended as a 524 MB file that prepInputs()
## reported as complete, wrote a .hash sidecar for, and handed to every later caller. The
## connection dropped once (retried), then the bytes written to disk fell short of the bytes
## received: a write that fails (full disk, quota) only warns, and nothing compared the file on
## disk with the server's Content-Length.

test_that("a download that cannot be written in full is an error, not a short file", {
  skip_on_cran()
  skip_on_os("windows") # ulimit and the XFSZ trap need a POSIX shell
  testInit()

  body <- as.raw(sample(0:255, 3e6, TRUE))
  srv <- startTestHttpServer(body, dropFirst = 1L)
  on.exit(srv$stop(), add = TRUE)

  dest <- checkPath(file.path(tmpdir, "dest"), create = TRUE)
  log <- file.path(tmpdir, "consumer.log")
  script <- file.path(tmpdir, "consumer.R")
  writeLines(c(
    childProcessPreamble(),
    'options(reproducible.verbose = 1)',
    sprintf('out <- try(prepInputs(url = "%s", destinationPath = "%s", fun = NA, useCache = FALSE))',
            srv$url, dest),
    'cat("\\nDONE", if (is(out, "try-error")) "ERROR" else "OK", "\\n")'
  ), script)
  ## A file-size limit below the body size makes every write past it fail the way a full disk
  ## does (EFBIG instead of ENOSPC). XFSZ is ignored so the write fails instead of the process.
  cmd <- sprintf("trap '' XFSZ; ulimit -f 1000; exec %s %s > %s 2>&1",
                 shQuote(file.path(R.home("bin"), "Rscript")), shQuote(script), shQuote(log))
  system2("bash", c("-c", shQuote(cmd)))
  out <- readLines(log, warn = FALSE)

  expect_true(any(grepl("DONE ERROR", out)), info = paste(out, collapse = "\n"))
  got <- file.path(dest, "file.bin")
  if (file.exists(got)) expect_equal(file.size(got), length(body))
  expect_length(dir(dest, pattern = "\\.hash$", all.files = TRUE), 0L)
})

test_that("a dropped connection leaves nothing at the destination; the retry gets every byte", {
  skip_on_cran()
  skip_if_not_installed("httr2")
  skip_if_not_installed("curl")
  testInit()

  body <- as.raw(sample(0:255, 3e6, TRUE))
  srv <- startTestHttpServer(body, dropFirst = 2L)
  on.exit(srv$stop(), add = TRUE)

  dp <- checkPath(file.path(tmpdir, "dl"), create = TRUE)
  ## streamed (progress lines) and plain httr2 paths both
  for (v in c(0, 1)) {
    unlink(dir(dp, full.names = TRUE, all.files = TRUE, no.. = TRUE))
    expect_error(dlGeneric(srv$url, destinationPath = dp, verbose = v))
    ## attempt 1 died part-way: nothing may sit at the final name, or anywhere in dp
    expect_length(dir(dp, all.files = TRUE, no.. = TRUE), 0L)
  }
  ## the server now serves in full
  res <- dlGeneric(srv$url, destinationPath = dp, verbose = 0)
  expect_identical(readBin(res$destFile, "raw", length(body) + 1), body)
  expect_identical(dir(dp, all.files = TRUE, no.. = TRUE), "file.bin")
})

test_that("prepInputs retries a dropped download and keeps only the complete file", {
  skip_on_cran()
  skip_if_not_installed("httr2")
  skip_if_not_installed("curl")
  testInit()

  body <- as.raw(sample(0:255, 3e6, TRUE))
  srv <- startTestHttpServer(body, dropFirst = 1L)
  on.exit(srv$stop(), add = TRUE)

  dest <- checkPath(file.path(tmpdir, "dest"), create = TRUE)
  f <- prepInputs(url = srv$url, destinationPath = dest, fun = NA, useCache = FALSE, verbose = -1)
  expect_identical(readBin(f, "raw", length(body) + 1), body)
  ## the sidecar records the size, so a later shortened file is never trusted on its say-so
  sc <- dir(dest, pattern = "\\.hash$", all.files = TRUE, full.names = TRUE)
  expect_length(sc, 1L)
  expect_true(any(readLines(sc) == paste0("size:", length(body))))
})

test_that("an ETag sidecar does not vouch for a local file of the wrong size", {
  skip_on_cran()
  skip_if_not_installed("httr2")
  skip_if_not_installed("curl")
  testInit()

  body <- as.raw(sample(0:255, 3e6, TRUE))
  srv <- startTestHttpServer(body)
  on.exit(srv$stop(), add = TRUE)

  dest <- checkPath(file.path(tmpdir, "dest"), create = TRUE)
  f <- prepInputs(url = srv$url, destinationPath = dest, fun = NA, useCache = FALSE, verbose = -1)
  sc <- dir(dest, pattern = "\\.hash$", all.files = TRUE, full.names = TRUE)
  expect_length(sc, 1L)
  sidecarLines <- readLines(sc)

  shorten <- function() {
    ## what the GPSC run left: a short file, its row in CHECKSUMS.txt, and the sidecar
    unlink(f)
    writeBin(body[seq_len(524288)], f)
    Checksums(dest, write = TRUE, files = f)
  }

  ## sidecar records the size
  shorten()
  f2 <- prepInputs(url = srv$url, destinationPath = dest, fun = NA, useCache = FALSE, verbose = -1)
  expect_equal(file.size(f2), length(body))
  expect_identical(readBin(f2, "raw", length(body) + 1), body)

  ## a sidecar written before sizes were recorded: the server's size, when its ETag still
  ## matches, is the reference
  shorten()
  writeLines(grep("^size:", sidecarLines, value = TRUE, invert = TRUE), sc)
  f3 <- prepInputs(url = srv$url, destinationPath = dest, fun = NA, useCache = FALSE, verbose = -1)
  expect_equal(file.size(f3), length(body))
  expect_identical(readBin(f3, "raw", length(body) + 1), body)

  ## and a complete file with a sidecar is still trusted without a download
  expect_message(
    prepInputs(url = srv$url, destinationPath = dest, fun = NA, useCache = FALSE),
    "local file matches remote version"
  )
})
