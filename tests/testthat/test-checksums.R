test_that("Checksums read and written correctly", {
  testInit()
  sampleDir <- system.file("examples", package = "reproducible")
  sampleFiles <- list.files(sampleDir, pattern = "[.R]", full.names = TRUE)
  on.exit(unlink(dirname(tmpdir), recursive = TRUE), add = TRUE)

  expect_true(all(file.copy(sampleFiles, tmpdir)))

  csf <- file.path(tmpdir, "CHECKSUMS.txt")
  cnamesR <- c(
    "result", "expectedFile", "actualFile", "checksum.x", "checksum.y",
    "algorithm.x", "algorithm.y", "filesize.x", "filesize.y"
  )
  cnamesW <- c("file", "checksum", "filesize", "algorithm")

  # 1. read Checksums without CHECKSUMS.txt file
  expect_true(NROW(Checksums(tmpdir)) == 0)

  Checksums(dirname(csf), write = TRUE)
  txt <- Checksums(dirname(csf))
  txt <- txt[grepl("R", expectedFile)]
  data.table::setorderv(txt, "expectedFile")

  csums <- txt$checksum.x

  # 2. read Checksums with empty CHECKSUMS.txt file
  expect_true(file.create(csf))
  txt <- Checksums(tmpdir)
  expect_true(all(colnames(txt) == cnamesR))
  expect_equal(nrow(txt), 0)

  # 3. write Checksums without CHECKSUMS.txt
  expect_true(file.remove(csf))
  txt <- Checksums(dirname(csf), write = TRUE)
  txt <- txt[grepl("[.]R$", basename(expectedFile))]
  data.table::setorderv(txt, "expectedFile")
  expect_true(all(colnames(txt) == cnamesR))
  expect_equal(nrow(txt), NROW(dir(tmpdir, pattern = "[.]R$")))

  # expect_identical(sort(txt$expectedFile), sort(basename(sampleFiles)))
  # expect_true(all(sort(txt$expectedFile) == sort(basename(sampleFiles))))
  a <- basename2(sort(grep("\\.R$", txt[result == "OK", ]$expectedFile, value = TRUE))) # the [result == "OK",] is because some unknown reason on GA
  b <- sort(basename2(sampleFiles))
  expect_identical(a, b)
  # expect_identical(a[1], b[1])
  # expect_identical(a[2], b[2])
  # expect_identical(a[3], b[3])
  # expect_identical(a[4], b[4])
  # expect_identical(a[5], b[5])
  # expect_identical(a[6], b[6])

  # 4. read Checksums with non-empty, but incomplete CHECKSUMS.txt file
  out <- txt[, `:=`(
    file = expectedFile,
    checksum = checksum.x,
    algorithm = algorithm.x
  )][1:4]
  # out <- data.table::data.table(file = basename(sampleFiles[-1]),
  #                  checksum = csums[-1],
  #                  algorithm = c("xxhash64", "xxhash64", "xxhash64", "xxhash64"),
  #                  stringsAsFactors = FALSE)
  utils::write.table(out, csf, eol = "\n", col.names = TRUE, row.names = FALSE)

  txt <- Checksums(tmpdir, write = FALSE)
  txt <- txt[!is.na(result)]
  data.table::setorderv(txt, "expectedFile")
  expect_equal(nrow(txt), NROW(out))
  expect_true(all(txt$result == "OK"))

  # 5. make Checksums complete -- just append one line
  expect_true(all(colnames(txt) == cnamesR))
  txt <- Checksums(tmpdir, write = TRUE)
  txt <- Checksums(tmpdir)
  txt <- txt[grepl("[.]R$", expectedFile)][result == "OK", ]
  expect_equal(nrow(txt), NROW(dir(tmpdir, pattern = "[.]R$")))
  # expect_true(all(sort(txt$expectedFile) == sort(basename(sampleFiles))))
  a <- basename2(sort(grep("\\.R$", txt$expectedFile, value = TRUE)))
  # print(txt$expectedFile)
  expect_identical(a, b)

  # Now add a leading dot (./) to filenames; this must be removed
  csf <- dir(tmpdir, full.names = TRUE, pattern = "CHECKSUMS.txt")
  txt <- read.table(csf, header = TRUE) |> as.data.table()
  txt[, file := paste0("./", file)]
  write.table(txt, file = csf)
  doCS <- Checksums(tmpdir)
  doCS <- doCS[grepl("[.]R$", expectedFile)][result == "OK", ]
  expect_equal(nrow(doCS), NROW(dir(tmpdir, pattern = "[.]R$")))

})
