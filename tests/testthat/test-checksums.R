test_that("Checksums read and written correctly", {
  # library(magrittr)

  testInitOut <- testInit()
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  sampleDir <- system.file("examples", package = "reproducible")
  sampleFiles <- list.files(sampleDir, pattern = "[.R]", full.names = TRUE)
  on.exit(unlink(dirname(tmpdir), recursive = TRUE), add = TRUE)

  expect_true(all(file.copy(sampleFiles, tmpdir)))

  csf <- file.path(tmpdir, "CHECKSUMS.txt")
  cnamesR <- c("result", "expectedFile", "actualFile", "checksum.x", "checksum.y",
               "algorithm.x", "algorithm.y", "filesize.x", "filesize.y")
  cnamesW <- c("file", "checksum", "filesize", "algorithm")
  csums <- c("e765e999bf75d95f", "67bb602a320e6ab0", "95cf3d316655454d",
             "1781d29114c37e4b", "ed47937f707440af")

  # 1. read Checksums without CHECKSUMS.txt file
  expect_true(NROW(Checksums(tmpdir))==0)

  # 2. read Checksums with empty CHECKSUMS.txt file
  expect_true(file.create(csf))
  txt <- Checksums(tmpdir)
  expect_true(all(colnames(txt) == cnamesR))
  expect_equal(nrow(txt), 0)

  # 3. write Checksums without CHECKSUMS.txt
  expect_true(file.remove(csf))
  txt <- Checksums(dirname(csf), write = TRUE)
  txt <- txt[grepl("R", expectedFile)]
  expect_true(all(colnames(txt) == cnamesR))
  expect_equal(nrow(txt), NROW(dir(tmpdir, pattern = "R")))
  expect_true(all(txt$expectedFile == basename(sampleFiles)))
  expect_true(all(sort(txt$checksum.y) == sort(csums)))

  # 4. read Checksums with non-empty CHECKSUMS.txt file
  out <- data.frame(file = basename(sampleFiles[-1]),
                    checksum = csums[-1],
                    algorithm = c("xxhash64", "xxhash64", "xxhash64", "xxhash64"),
                    stringsAsFactors = FALSE)
  utils::write.table(out, csf, eol = "\n", col.names = TRUE, row.names = FALSE)

  txt <- Checksums(tmpdir, write = FALSE)
  expect_equal(nrow(txt), NROW(out))

  txt <- Checksums(tmpdir, write = TRUE)
  txt <- txt[grepl("R", expectedFile)]
  expect_true(all(colnames(txt) == cnamesR))
  expect_equal(nrow(txt), NROW(dir(tmpdir, pattern = "R")))
  expect_true(all(txt$expectedFile == basename(sampleFiles)))
  expect_true(all(sort(txt$checksum.y) == sort(csums)))
})
