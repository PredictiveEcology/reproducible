test_that("preProcess works for .tar files", {
  skip_on_cran()
  testInit("terra", needInternet = TRUE)
  url <- theRasterTestTar
  noisyOutput <- capture.output(
    ras <- reproducible::preProcess(url = url, destinationPath = tmpdir)
  )
  testthat::expect_is(object = ras, class = "list")
  testthat::expect_true(file.exists(ras$targetFilePath))
})

test_that("preProcess works for .zip when provided only url and destinationPath", {
  skip_on_cran()
  testInit("terra", needInternet = TRUE)
  url <- theRasterTestZip
  noisyOutput <- capture.output(
    ras <- reproducible::preProcess(url = url, destinationPath = tmpdir)
  )
  testthat::expect_is(object = ras, class = "list")
  testthat::expect_true(file.exists(ras$targetFilePath))
})

test_that("preProcess works with only url", {
  skip_on_cran()
  testInit("terra", needInternet = TRUE)
  url <- theRasterTestZip
  noisyOutput <- capture.output(
    ras <- reproducible::preProcess(url = url)
  )
  testthat::expect_is(object = ras, class = "list")
  testthat::expect_true(file.exists(ras$targetFilePath))
})

test_that("preProcess works when provides only archive", {
  skip_on_cran()
  testInit("terra", needInternet = TRUE)
  url <- theRasterTestZip
  noisyOutput <- capture.output(
    pre <- reproducible::preProcess(url = url, destinationPath = tmpdir)
  )
  testthat::expect_is(object = pre, class = "list")
  noisyOutput <- capture.output(
    ras <- reproducible::preProcess(archive = file.path(
      pre$destinationPath,
      list.files(pre$destinationPath)[
        grepl(
          x = list.files(pre$destinationPath),
          pattern = ".zip|.tar"
        )
      ]
    ))
  )
  testthat::expect_is(object = ras, class = "list")
})

test_that("preProcess works when provides archive and destinationPath", {
  skip_on_cran()
  testInit("terra", needInternet = TRUE)
  url <- theRasterTestZip
  noisyOutput <- capture.output(
    pre <- reproducible::preProcess(url = url, destinationPath = tmpdir)
  )
  testthat::expect_is(object = pre, class = "list")
  noisyOutput <- capture.output(
    ras <- reproducible::preProcess(
      archive = file.path(
        pre$destinationPath,
        list.files(pre$destinationPath)[
          grepl(
            x = list.files(pre$destinationPath),
            pattern = ".zip|.tar"
          )
        ]
      ),
      destinationPath = tmpdir
    )
  )
  testthat::expect_is(object = ras, class = "list")
})

test_that("preProcess works when provides only targetFile", {
  skip_on_cran()
  testInit("terra", needInternet = TRUE)
  url <- theRasterTestZip
  noisyOutput <- capture.output(
    pre <- reproducible::preProcess(url = url, destinationPath = tmpdir)
  )
  testthat::expect_is(object = pre, class = "list")
  noisyOutput <- capture.output(
    ras <- reproducible::preProcess(targetFile = pre$targetFilePath)
  )
  testthat::expect_is(object = ras, class = "list")
})

test_that("preProcess works when provides targetfile and destinationPath", {
  skip_on_cran()
  testInit("terra", needInternet = TRUE)
  url <- theRasterTestZip
  noisyOutput <- capture.output(
    pre <- reproducible::preProcess(url = url, destinationPath = tmpdir)
  )
  testthat::expect_is(object = pre, class = "list")
  noisyOutput <- capture.output(
    ras <- reproducible::preProcess(
      targetFile = pre$targetFilePath,
      destinationPath = tmpdir
    )
  )
  testthat::expect_is(object = ras, class = "list")
})

test_that("preProcess works when provides url, archive, targetfile and destinationPath", {
  skip_on_cran()
  testInit("terra", needInternet = TRUE)
  url <- theRasterTestZip
  noisyOutput <- capture.output(
    ras <- reproducible::preProcess(
      url = url, targetFile = theRasterTestFilename(suff = "tif"),
      archive = theRasterTestFilename(suff = "zip"),
      destinationPath = tmpdir
    )
  )
  testthat::expect_is(object = ras, class = "list")
  testthat::expect_true(file.exists(ras$targetFilePath))
})

test_that("preProcess works when provides url, archive, and destinationPath", {
  skip_on_cran()
  testInit("terra", needInternet = TRUE)
  url <- theRasterTestZip
  noisyOutput <- capture.output(
    ras <- reproducible::preProcess(
      url = url,
      archive = theRasterTestFilename(suff = "zip"),
      destinationPath = tmpdir
    )
  )
  testthat::expect_is(object = ras, class = "list")
  testthat::expect_true(file.exists(ras$targetFilePath))
})

test_that("preProcess works when provides url, archive, and destinationPath and reproducible.inputPaths", {
  skip_on_cran()
  testInit("terra", needInternet = TRUE)
  withr::local_options("reproducible.inputPaths" = tmpdir)
  url <- theRasterTestZip
  noisyOutput <- capture.output(
    ras <- reproducible::preProcess(
      url = url,
      archive = theRasterTestFilename(suff = "zip"),
      destinationPath = tmpCache
    )
  )
  testthat::expect_is(object = ras, class = "list")
  testthat::expect_true(file.exists(ras$targetFilePath))
})

test_that("preProcess works when provides url, targetfile and destinationPath", {
  skip_on_cran()
  testInit("terra", needInternet = TRUE)
  url <- theRasterTestZip
  noisyOutput <- capture.output(
    ras <- reproducible::preProcess(
      url = url, targetFile = theRasterTestFilename(suff = "tif"),
      destinationPath = tmpdir
    )
  )
  testthat::expect_is(object = ras, class = "list")
  testthat::expect_true(file.exists(ras$targetFilePath))
})

test_that("preProcess works when provides url and destinationPath for a .rar file", {
  skip_on_cran()
  testInit("terra", needInternet = TRUE)
  # extractSystemCallPath <- try(.testForArchiveExtract(), silent = TRUE)
  url <- theRasterTestRar

  # if (!is(extractSystemCallPath, "try-error")) {
  # if (is.null(extractSystemCallPath)) {
  #   noisyOutput <- capture.output(
  #     expect_error({
  #       ras <- reproducible::preProcess(url = url, destinationPath = tmpdir)
  #     })
  #   )
  # } else {
  if (isWindows() && getRversion() < "4.3")
    skip("archive pkg on Windows 4.2.3 fails on rar")

  noisyOutput <- capture.output(
    ras <- reproducible::preProcess(url = url, destinationPath = tmpdir)
  )
  testthat::expect_is(object = ras, class = "list")
  testthat::expect_true(file.exists(ras$targetFilePath))
  # }
  # }
})

test_that("preProcess works when provides url, targetfile and destinationPath for a .rar file", {
  skip_on_cran()
  testInit("terra", needInternet = TRUE)
  ## extractSystemCallPath <- try(.testForArchiveExtract(), silent = TRUE)
  url <- theRasterTestRar

  # if (!is(extractSystemCallPath, "try-error")) {
  # if (is.null(extractSystemCallPath)) {
  #   noisyOutput <- capture.output(
  #     expect_error({
  #       ras <- reproducible::preProcess(
  #         url = url, targetFile = theRasterTestFilename(suff = "tif"),
  #         destinationPath = tmpdir
  #       )
  #     })
  #   )
  # } else {
  if (isWindows() && getRversion() < "4.3")
    skip("archive pkg on Windows 4.2.3 fails on rar")

  wd <- getwd()
  noisyOutput <- capture.output(
    ras <- reproducible::preProcess(
      url = url, targetFile = theRasterTestFilename(suff = "tif"),
      destinationPath = tmpdir
    )
  )
  testthat::expect_is(object = ras, class = "list")
  testthat::expect_true(file.exists(ras$targetFilePath))
  expect_equal(wd, getwd()) # Test that working directory is restored after unrar call
  # }
  # }
})

test_that("preProcess works when provides url, archive and destinationPath for a .rar file", {
  skip_on_cran()
  testInit("terra", needInternet = TRUE)
  # extractSystemCallPath <- try(.testForArchiveExtract(), silent = TRUE)
  url <- theRasterTestRar
  rasterTestRarFilename <- theRasterTestFilename(suff = "rar")

  # if (!is(extractSystemCallPath, "try-error")) {
  #   if (is.null(extractSystemCallPath)) {
  #   noisyOutput <- capture.output(
  #     expect_error({
  #       ras <- reproducible::preProcess(url = url, archive = rasterTestRarFilename, destinationPath = tmpdir)
  #     })
  #   )
  # } else {
  if (isWindows() && getRversion() < "4.3")
    skip("archive pkg on Windows 4.2.3 fails on rar")
  noisyOutput <- capture.output(
    ras <- reproducible::preProcess(url = url, archive = rasterTestRarFilename, destinationPath = tmpdir)
  )
  testthat::expect_is(object = ras, class = "list")
  testthat::expect_true(file.exists(ras$targetFilePath))
  #   }
  # }
})

test_that("preProcess works, but gives a warning when supplying cacheTags", {
  skip_on_cran()
  testInit("terra", needInternet = TRUE)
  url <- theRasterTestZip
  noisyOutput <- capture.output(
    testthat::expect_message({
      ras <- reproducible::preProcess(url = url, destinationPath = tmpdir, cacheTags = "objectName::ras")
    })
  )
  testthat::expect_is(object = ras, class = "list")
  testthat::expect_true(file.exists(ras$targetFilePath))
})

test_that("preProcess works, but gives a warning when supplying postProcessedFilename", {
  skip_on_cran()
  testInit("terra", needInternet = TRUE)
  url <- theRasterTestZip
  noisyOutput <- capture.output(
    testthat::expect_message({
      ras <- reproducible::preProcess(
        url = url, destinationPath = tmpdir,
        postProcessedFilename = "ras.tif"
      )
    })
  )
  testthat::expect_is(object = ras, class = "list")
  testthat::expect_true(file.exists(ras$targetFilePath))
})

test_that("preProcess works, but gives a warning when supplying rasterInterpMethod", {
  skip_on_cran()
  testInit("terra", needInternet = TRUE)
  url <- theRasterTestZip
  noisyOutput <- capture.output(
    testthat::expect_message({
      ras <- reproducible::preProcess(
        url = url, destinationPath = tmpdir,
        rasterInterpMethod = "near"
      )
    })
  )
  testthat::expect_is(object = ras, class = "list")
  testthat::expect_true(file.exists(ras$targetFilePath))
})

test_that("preProcess works, but gives a warning when supplying rasterDatatype", {
  skip_on_cran()
  testInit("terra", needInternet = TRUE)
  url <- theRasterTestZip
  noisyOutput <- capture.output(
    testthat::expect_message({
      ras <- reproducible::preProcess(url = url, destinationPath = tmpdir, rasterDatatype = "INT1U")
    })
  )
  testthat::expect_is(object = ras, class = "list")
  testthat::expect_true(file.exists(ras$targetFilePath))
})

test_that("preProcess works, but gives a warning when supplying pkg", {
  skip_on_cran()
  testInit("terra", needInternet = TRUE)
  url <- theRasterTestZip
  noisyOutput <- capture.output(
    testthat::expect_message({
      ras <- reproducible::preProcess(url = url, destinationPath = tmpdir, pkg = "utils", fun = "unzip")
    })
  )
  testthat::expect_is(object = ras, class = "list")
  testthat::expect_true(file.exists(ras$targetFilePath))
})

test_that("message when files from archive are already present", {
  skip_on_cran()
  testInit("terra", needInternet = TRUE)
  url <- "https://github.com/tati-micheletti/host/raw/master/data/rasterTest.zip"
  noisyOutput <- capture.output(
    ccc <- testthat::capture_output(
      ras <- reproducible::preProcess(
        url = url,
        targetFile = "rasterTest.tif",
        destinationPath = tmpdir
      )
    )
  )
  noisyOutput <- capture.output(
    testthat::expect_message({
      ras <- reproducible::preProcess(archive = "rasterTest.zip", destinationPath = tmpdir)
    })
  )
})

test_that("message when file is a shapefile", {
  skip_on_cran()
  testInit("terra", needInternet = TRUE)
  url <- "https://github.com/tati-micheletti/host/raw/master/data/shapefileTest.zip"
  noisyOutput <- capture.output(
    ccc <- testthat::capture_output(
      testthat::expect_message({
        ras <- reproducible::preProcess(url = url, destinationPath = tmpdir)
      })
    )
  )
})

test_that("message when doesn't know the targetFile extension", {
  skip_on_cran()
  testInit("terra", needInternet = TRUE)
  url <- "https://github.com/tati-micheletti/host/raw/master/data/unknownTargetFile.zip"
  noisyOutput <- capture.output(
    ccc <- testthat::capture_output(
      testthat::expect_error(regexp = "guess at which function to use to read", {
        ras <- reproducible::preProcess(url = url, destinationPath = tmpdir)
      })
    )
  )
})

test_that("When supplying two files without archive, when archive and files have different names", {
  skip_on_cran()
  testInit("terra", needInternet = TRUE)
  url <- "https://github.com/tati-micheletti/host/raw/master/data/twoKnownFiles.zip"
  noisyOutput <- capture.output(
    ccc <- testthat::capture_output(
      testthat::expect_error({
        ras <- reproducible::preProcess(
          url = url,
          targetFile = c("rasterTest.tif", "shapefileTest.shp"),
          destinationPath = tmpdir
        )
      })
    )
  )
})

test_that("message when archive has two known files (raster and shapefile)", {
  skip_on_cran()
  testInit("terra", needInternet = TRUE)
  url <- "https://github.com/tati-micheletti/host/raw/master/data/knownFiles.zip"
  noisyOutput <- capture.output(
    ccc <- testthat::capture_output(
      testthat::expect_error({
        ras <- reproducible::preProcess(
          url = url,
          archive = "knownFiles.zip",
          targetFile = c("knownFiles.tif", "knownFiles.shp"),
          destinationPath = tmpdir
        )
      })
    )
  )
})

test_that("message when extracting a file that is already present", {
  skip_on_cran()
  testInit("terra", needInternet = TRUE)
  url1 <- "https://github.com/tati-micheletti/host/raw/master/data/rasterTest.zip"
  noisyOutput <- capture.output({
    ccc <- testthat::capture_output({
      ras <- reproducible::preProcess(
        url = url1,
        targetFile = "rasterTest.tif",
        destinationPath = tmpdir
      )
    })
  })
  url2 <- "https://github.com/tati-micheletti/host/raw/master/data/shapefileTest.zip"
  noisyOutput <- capture.output({
    ccc <- testthat::capture_output({
      shp <- reproducible::preProcess(url = url2, destinationPath = tmpdir)
    })
  })
  url3 <- "https://github.com/tati-micheletti/host/raw/master/data/knownFiles.zip"
  noisyOutput <- capture.output({
    ccc <- testthat::capture_output({
      testthat::expect_message({
        fl <- reproducible::preProcess(url = url3, destinationPath = tmpdir)
      })
    })
  })
})

test_that("masking with larger extent obj", {
  skip_on_cran()
  skip_on_ci()

  testInit("terra", needGoogleDriveAuth = TRUE, needInternet = TRUE)
  smallRT <- prepInputs(url = "https://drive.google.com/open?id=1WhL-DxrByCbzAj8A7eRx3Y1FVujtGmtN")
  sam <- sample(terra::ncell(smallRT), size = terra::ncell(smallRT) / 2)
  smallRT[] <- NA
  smallRT[sam] <- 1L
  if (is(smallRT, "Raster")) {
    a <- raster::extent(smallRT)
  } else {
    a <- terra::ext(smallRT)
  }
  a <- terra::extend(a, -3e5) # make it small
  test <- rasterRead(a, resolution = 250, vals = 2)
  terra::crs(test) <- terra::crs(smallRT)
  b <- postProcess(x = test, rasterToMatch = smallRT, maskWithRTM = TRUE)
  expect_true(is(b, rasterType()))
})

test_that("just google id not url", {
  skip_on_cran()
  skip_on_ci()

  testInit("terra", needGoogleDriveAuth = TRUE, needInternet = TRUE)
  co <- capture.output(
    smallObj <- prepInputs(url = "1Bk4SPz8rx8zziIlg2Yp9ELZmdNZytLqb")
  )
  expect_is(smallObj, "sf")
})

test_that("Test of using future and progress indicator for lrg files on Google Drive", {
  skip_if_not_installed("future")
  skip_if_not_installed("googledrive")

  if (interactive()) {
    testInit(c("terra", "future"),
      needGoogleDriveAuth = TRUE, needInternet = TRUE,
      opts = list("reproducible.futurePlan" = "multisession")
    )
    noisyOutput <- capture.output({
      ccc <- testthat::capture_output({
        smallRT <- preProcess(url = "https://drive.google.com/open?id=1WhL-DxrByCbzAj8A7eRx3Y1FVujtGmtN")
      })
    })
    expect_true(is(smallRT, "list"))
  }
})

test_that("lightweight tests for preProcess code coverage", {
  skip_on_cran()
  testInit(c("sf", "terra"))
  expect_true(is.data.frame(preProcessParams()))
  expect_true(is.data.frame(preProcessParams(1)))
  expect_true(is.data.frame(preProcessParams(2)))
  expect_true(is.data.frame(preProcessParams(3)))
  expect_true(is.data.frame(preProcessParams(4)))

  # test purge
  localFileLuxSm <- system.file("ex/luxSmall.shp", package = "reproducible")
  capture.output(
    la <- prepInputs(targetFile = localFileLuxSm, destinationPath = tmpdir)
  )
  csf <- dir(pattern = "CHECKSUMS", path = tmpdir, full.names = TRUE)
  a <- file.info(csf)
  Sys.sleep(0.1)
  capture.output(
    la2 <- prepInputs(targetFile = localFileLuxSm, destinationPath = tmpdir)
  )
  b <- file.info(csf)
  expect_true(milliseconds(b$mtime) == milliseconds(a$mtime))
  if (isWindows() && isInteractive()) { # apparently atime is not write on *nix-alikes
    expect_false(milliseconds(b$atime) == milliseconds(a$atime))
  }

  # purge will delete CHECKSUMS 7 -- written, read
  capture.output(
    la3 <- prepInputs(targetFile = localFileLuxSm, destinationPath = tmpdir, purge = 7)
  )
  d <- file.info(csf)
  if (isWindows()) { # linux doesn't do ctime
    expect_true(milliseconds(d$ctime) == milliseconds(a$ctime))
    expect_false(milliseconds(d$atime) == milliseconds(a$atime))
  }
  expect_false(milliseconds(d$mtime) == milliseconds(a$mtime))

  # purge will delete CHECKSUMS 1 -- deleted, written, read
  Sys.sleep(0.1)
  capture.output(
    la4 <- prepInputs(targetFile = localFileLuxSm, destinationPath = tmpdir, purge = 1)
  )
  e <- file.info(csf)
  # if (isWindows()) # windows doesn't release a file's ctime even when removed
  #   expect_false(milliseconds(e$ctime) == milliseconds(a$ctime))
  expect_false(milliseconds(e$mtime) == milliseconds(a$mtime))
  expect_false(milliseconds(e$atime) == milliseconds(a$atime))

  expect_null(.decodeMagicNumber("sdfddsffdfs.tetes"))
  expect_true(is.character(.decodeMagicNumber("Shapefile")))
  expect_true(is.character(.decodeMagicNumber("RAR")))
  expect_true(is.character(.decodeMagicNumber("tar")))
  expect_true(is.character(.decodeMagicNumber("TIFF")))
  expect_true(is.character(.decodeMagicNumber("Zip")))
})

test_that("large test for nested file structures in zips", {
  skip_on_cran()
  skip_on_ci()
  testInit(c("sf", "googledrive", "terra"), needInternet = TRUE, needGoogleDriveAuth = TRUE)
  climateDataURL <- "https://drive.google.com/file/d/1we9GqEVAORWLbHi3it66VnCcvLu85QIk"

  ## extracts flat files, overwriting and keeping only the last subdir's files
  files <- list(
    paste0("Alberta/Year_", 2015:2019, "M/Eref01.asc"),
    paste0("Alberta/Year_", 1991:2019, "M/Eref01.asc"),
    c(
      paste0("Alberta/Year_", 1991:2019, "M/CMD01.asc"),
      paste0("Alberta/Year_", 1991:2019, "M/Eref01.asc")
    )
  )
  lapply(files, function(fis) {
    res1 <- preProcess(
      url = climateDataURL, destinationPath = tmpdir,
      targetFile = "Alberta/Year_2020M/CMD01.asc",
      alsoExtract = fis
    )
    testLength <- length(fis) + 2
    expect_equal(NROW(res1$checkSums[checksum.x != "dir"]), testLength)
  })
})

test_that("more nested file structures in zip; alsoExtract NA", {
  skip_on_cran()
  testInit("terra")
  tmpdir <- withr::local_tempdir()
  withr::local_dir(tmpdir)
  zipName <- "ex.zip"
  system.time({
    ras <- lapply(1:2, function(x) {
      td <- tempdir2()
      terra::rast(terra::ext(0, 4, 0, 4), vals = sample(1:16), resolution = 1) |>
        terra::writeRaster(filename = file.path(td, basename(tempfile(fileext = ".tif"))))
    })
    setwd(dirname(dirname(Filenames(ras[[1]]))))
    fns1 <- Filenames(ras)
    # zip(zipName, files = file.path(basename(dirname(fns)), basename(fns)))
    ras <- lapply(1:2, function(x) {
      terra::rast(terra::ext(0, 4, 0, 4), vals = sample(1:16), resolution = 1) |>
        terra::writeRaster(filename = file.path(basename(tempfile(fileext = ".tif"))))
    })
    fns2 <- Filenames(ras)
    zip(zipName, files = c(file.path(basename(dirname(fns1)), basename(fns1)), basename(fns2)), flags = "-q")
  })
  zipName2 <- file.path(tmpdir, zipName)

  # don't specify targetFile --> fail because many options
  linkOrCopy(zipName, zipName2)
  a <- prepInputs(archive = zipName, destinationPath = tmpdir)
  knownOtherFiles <- c("cache.db", "CHECKSUMS.txt", "ex.zip", "testCache")
  files <- dir(tmpdir, recursive = TRUE) # %in% dirname(.listFilesInArchive(zipName2))
  expect_true(all(.listFilesInArchive(zipName2) %in% files))
  unlink(file.path(tmpdir, grep("\\.", invert = TRUE, dirname(files), value = TRUE)), recursive = TRUE)
  files2 <- dir(pattern = ".tif", tmpdir, full.names = TRUE)
  unlink(files2)

  # alsoExtract = NULL --> the default --> extract all
  checkPath(dirname(zipName2), create = TRUE)
  linkOrCopy(zipName, zipName2)
  a <- prepInputs(
    archive = zipName,
    targetFile = grep("\\.tif", basename(files), value = TRUE)[1],
    destinationPath = tmpdir
  )
  files <- dir(tmpdir, recursive = TRUE) # %in% dirname(.listFilesInArchive(zipName2))
  expect_true(all(.listFilesInArchive(zipName2) %in% files))
  unlink(file.path(tmpdir, grep("\\.", invert = TRUE, dirname(files), value = TRUE)), recursive = TRUE)
  files2 <- dir(pattern = ".tif", tmpdir, full.names = TRUE)
  unlink(files2)


  # alsoExtract = NA --> extract only targetFile -- with wrong filename (missing subdir)
  checkPath(dirname(zipName2), create = TRUE)
  linkOrCopy(zipName, zipName2)
  filesPre <- dir(tmpdir, recursive = TRUE) # %in% dirname(.listFilesInArchive(zipName2))
  a <- prepInputs(
    archive = zipName, alsoExtract = NA,
    targetFile = grep("\\.tif", basename(files), value = TRUE)[1],
    destinationPath = tmpdir
  )
  files <- dir(tmpdir, recursive = TRUE) # %in% dirname(.listFilesInArchive(zipName2))
  expect_false(all(.listFilesInArchive(zipName2) %in% files))
  expect_true(sum(.listFilesInArchive(zipName2) %in% files) == 1)
  unlink(file.path(tmpdir, grep("\\.", invert = TRUE, dirname(files), value = TRUE)), recursive = TRUE)
  files2 <- dir(pattern = ".tif", tmpdir, full.names = TRUE)
  unlink(files2)



  # alsoExtract = a filename --> extract targetFile & 1 other -- with correct filename (with subdir)
  checkPath(dirname(zipName2), create = TRUE)
  linkOrCopy(zipName, zipName2)
  possFiles <- .listFilesInArchive(zipName2)
  filesWithSubDir <- grep("\\/", possFiles, value = TRUE, invert = FALSE)
  filesPre <- dir(tmpdir, recursive = TRUE) # %in% dirname(.listFilesInArchive(zipName2))
  a <- prepInputs(
    archive = zipName, alsoExtract = filesWithSubDir[2],
    targetFile = filesWithSubDir[1],
    destinationPath = tmpdir
  )
  files <- dir(tmpdir, recursive = TRUE) # no subdir on this file, so non-recursive should do it
  expect_false(all(.listFilesInArchive(zipName2) %in% files))
  expect_true(sum(.listFilesInArchive(zipName2) %in% files) == 2) #
  unlink(file.path(tmpdir, grep("\\.", invert = TRUE, dirname(files), value = TRUE)), recursive = TRUE)
  files2 <- dir(pattern = ".tif", tmpdir, full.names = TRUE)
  unlink(files2)


  # alsoExtract = NA --> extract only targetFile -- with correct filename (with subdir)
  checkPath(dirname(zipName2), create = TRUE)
  linkOrCopy(zipName, zipName2)
  possFiles <- .listFilesInArchive(zipName2)
  filesNoSubDir <- grep("\\/", possFiles, value = TRUE, invert = TRUE)
  filesPre <- dir(tmpdir, recursive = TRUE) # %in% dirname(.listFilesInArchive(zipName2))
  a <- prepInputs(
    archive = zipName, alsoExtract = NA,
    targetFile = filesNoSubDir[1],
    destinationPath = tmpdir
  )
  files <- dir(tmpdir, recursive = FALSE) # no subdir on this file, so non-recursive should do it
  expect_false(all(.listFilesInArchive(zipName2) %in% files))
  expect_true(sum(.listFilesInArchive(zipName2) %in% files) == 1)
})

test_that("PR#358 if dwnld already exists, was missing nested paths", {
  skip_on_cran()
  testInit("terra", needInternet = TRUE, needGoogleDriveAuth = TRUE)
  url <- "https://drive.google.com/file/d/1S-4itShMXtwzGxjKPgsznpdTD2ydE9qn/"
  noisyOutput <- capture.output(
    ras <- preProcess(targetFile = "all_gp_site_info.csv",
               url = url,
               destinationPath = getOption("reproducible.destinationPath", file.path(tmpdir, "dPath")),
               overwrite = TRUE,
               fun = "data.table::fread",
               useCache = FALSE)  )
  testthat::expect_true(file.exists(ras$targetFilePath))
  noisyOutput <- capture.output(
    ras <- preProcess(targetFile = "all_gp_site_info.csv",
               url = url,
               destinationPath = getOption("reproducible.destinationPath", file.path(tmpdir, "dPath")),
               overwrite = TRUE,
               fun = "data.table::fread",
               useCache = FALSE)  )
  testthat::expect_true(file.exists(ras$targetFilePath))

})


