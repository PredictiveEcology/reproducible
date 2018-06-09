test_that("Checksums read and written correctly", {
  library(magrittr)

  sampleDir <- system.file("maps", package = "quickPlot")
  sampleFiles <- list.files(sampleDir, pattern = "[.]tif", full.names = TRUE)
  tmpdir <- file.path(tempdir(), "test_checksums", "data") %>%
    checkPath(create = TRUE)
  on.exit(unlink(dirname(tmpdir), recursive = TRUE), add = TRUE)

  expect_true(all(file.copy(sampleFiles, tmpdir)))

  csf <- file.path(tmpdir, "CHECKSUMS.txt")
  cnamesR <- c("result", "expectedFile", "actualFile", "checksum.x", "checksum.y",
               "algorithm.x", "algorithm.y")
  cnamesW <- c("file", "checksum", "filesize", "algorithm")
  csums <- c("77c56d42fecac5b1", "8affcdf311555fd6", "e2dd8734d6ed3d05",
             "f21251dcdf23dde0", "86e342cfc6876b7d")

  # 1. read Checksums without CHECKSUMS.txt file
  browser()
  expect_error(Checksums(file.path(dirname(dirname(tmpdir))), "test_checksums"))

  # 2. read Checksums with empty CHECKSUMS.txt file
  expect_true(file.create(csf))
  txt <- Checksums("test_checksums", dirname(dirname(tmpdir)))
  expect_true(all(colnames(txt) == cnamesR))
  expect_equal(nrow(txt), 0)

  # 3. write Checksums without CHECKSUMS.txt
  expect_true(file.remove(csf))
  txt <- Checksums("test_checksums", dirname(dirname(tmpdir)), write = TRUE)
  expect_true(all(colnames(txt) == cnamesW))
  expect_equal(nrow(txt), 5)
  expect_true(all(txt$file == basename(sampleFiles)))
  expect_true(all(txt$checksum == csums))

  # 4. read Checksums with non-empty CHECKSUMS.txt file
  out <- data.frame(file = basename(sampleFiles[-1]),
                    checksum = csums[-1],
                    algorithm = c("xxhash64", "xxhash64", "xxhash64", "xxhash64"),
                    stringsAsFactors = FALSE)
  utils::write.table(out, csf, eol = "\n", col.names = TRUE, row.names = FALSE)

  txt <- Checksums("test_checksums", dirname(dirname(tmpdir)), write = TRUE)
  expect_true(all(colnames(txt) == cnamesW))
  expect_equal(nrow(txt), 5)
  expect_true(all(txt$file == basename(sampleFiles)))
  expect_true(all(txt$checksum == csums))
})
