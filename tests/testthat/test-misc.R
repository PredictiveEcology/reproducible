test_that("test miscellaneous fns (part 1)", {
  # ONLY RELEVANT FOR RASTERS
  testInit("raster", tmpFileExt = c(".tif", ".grd"))

  expect_type(searchFullEx(), "list")
  expect_true(length(searchFullEx()) > length(search()))
  expect_true(length(searchFullEx()) == (3 + length(search())))

  expect_true(all(unlist(lapply(searchFull(simplify = FALSE), is.environment))))
  test <- lapply(searchFull(simplify = FALSE), attributes)
  test <- grep("withr_handler", value = TRUE, test, invert = TRUE)
  expect_true(all(is.character(unlist(test))))

  # NO LONGER RELIABLE TEST BECAUSE OF NEW REMOVAL OF PACKAGES fEB 24 2021
  # expect_true(sum(unlist(d1)) < sum(unlist(d)))

  # convertRasterPaths
  filenames <- normalizePath(c("/home/user1/Documents/file.txt", "/Users/user1/Documents/file.txt"),
    winslash = "/", mustWork = FALSE
  )
  oldPaths <- dirname(filenames)
  newPaths <- normalizePath(c("/home/user2/Desktop", "/Users/user2/Desktop"),
    winslash = "/", mustWork = FALSE
  )
  expect_true(grepl(newPaths[1], convertPaths(filenames, oldPaths, newPaths)[1]))

  r1 <- raster::raster(system.file("external/test.grd", package = "raster"))
  r2 <- raster::raster(system.file("external/rlogo.grd", package = "raster"))
  rasters <- list(r1, r2)
  oldPaths <- system.file("external", package = "raster")
  newPaths <- file.path("~/rasters")
  rasters <- convertRasterPaths(rasters, oldPaths, newPaths)

  ## spurious failures non-interactively when not sorting
  expect_true(identical(
    sort(unlist(lapply(rasters, raster::filename))),
    sort(normPath(file.path(newPaths, basename(unlist(lapply(list(r1, r2), raster::filename))))))
  ))

  r3 <- suppressWarnings(writeRaster(r1, tmpfile[1], overwrite = TRUE)) ## TODO: raster needs updating for crs stuff
  r4 <- suppressWarnings(convertRasterPaths(tmpfile[1], dirname(tmpfile[1]), newPaths)) ## TODO: raster needs updating for crs stuff

  expect_identical(
    normPath(file.path(newPaths, basename(filename(r4)))),
    normPath(Filenames(r4))
  )

  expect_silent({
    b <- retry(quote(rnorm(1)), retries = 1, silent = TRUE)
  })
  expect_error({
    b <- retry(quote(stop()), retries = 1, silent = TRUE)
  })

  expect_true(identical(NULL, basename2(NULL)))
  a <- .formalsNotInCurrentDots(rnorm, n = 1, b = 2)
  b <- .formalsNotInCurrentDots(rnorm, dots = list(n = 1, b = 2))
  expect_identical(a, b)
})

test_that("objSize and objSizeSession", {
  skip_on_cran()
  # objectSize
  a <- 1
  b <- tempfile()
  saveRDS(a, b)
  expect_true(is.numeric(objSize(asPath(b))))
  expect_true(is(objSize(asPath(b)), "lobstr_bytes"))
})

test_that("setting options works correctly", {
  testInit(verbose = 1, ask = TRUE)

  a <- reproducibleOptions()

  # The keep is during terra-migration
  keep <- setdiff(names(a), c(
    "reproducible.rasterRead",
    "reproducible.cachePath",
    "reproducible.overwrite", # This is a bug # TODO... something prior to this test is changing it
    "reproducible.useDBI",
    "reproducible.cacheSaveFormat",
    "reproducible.shapefileRead"
  ))
  a <- a[keep]

  a1 <- a[sapply(a, function(x) !is.null(x))]
  b <- options()
  # b$reproducible.verbose <- as.numeric(b$reproducible.verbose)
  bbb <- match(names(b), names(a1))
  # expect_true(identical(sort(names(a1)), sort(names(a1[na.omit(bbb)]))))
  expect_true(identical(sort(names(a1)), sort(names(a1[bbb[!is.na(bbb)]]))))
  # omit <- c(names(testInitOut$opts), names(testInitOut$optsAsk),
  #          "reproducible.inputPath", "reproducible.tempPath")
  b1 <- b[names(a1)]
  # b1 <- b1[!names(b1) %in% omit]
  a2 <- a1 # [!names(a1) %in% omit]
  #a1[names(a1) %in% "reproducible.useMemoise"] <- NULL # useMemoise may be TRUE during some tests
  expect_identical(b1, a2)
})

test_that("guessAtTargetAndFun works correctly", {
  testInit("terra")

  # expect_error(.guessAtTargetAndFun(fun = rnorm), "fun must be a")
  expect_message(
    .guessAtTargetAndFun(targetFilePath = NULL, filesExtracted = "", fun = "load"),
    "Don't know which file to load"
  )
  expect_message(
    .guessAtTargetAndFun(targetFilePath = NULL, filesExtracted = "hi.rds", fun = "readRDS"),
    "targetFile was not specified."
  )
  expect_message(
    .guessAtTargetAndFun(targetFilePath = NULL, filesExtracted = c("hi.rds", "hello.rds"), fun = "readRDS"),
    "More than one possible files to load"
  )
})

test_that("unrar is working as expected", {
  testInit("terra", tmpFileExt = c(".tif", ".grd"))

  cat("hi", file = tmpfile[1])
  cat("hi", file = tmpfile[2])
  rarPath <- file.path(tmpdir, "tmp.rar")
  file.create(tmpfile)
  out <- try(utils::zip(zipfile = rarPath, files = tmpfile, flags = "-q")) # this should only be relevant if system can unrar
  if (!is(out, "try-error")) {
    unrar <- .whichExtractFn(archive = rarPath, args = "")
    if (.requireNamespace("archive"))
      expect_true(identical(unrar$fun, archive::archive_extract))
    suppressWarnings(
      expect_error(.callArchiveExtractFn(unrar$fun, files = "", args = list(exdir = tmpCache)))
    )
  }
})

test_that("test miscellaneous fns (part 2)", {
  testInit("terra",
    tmpFileExt = c(".tif", ".grd"),
    needGoogleDriveAuth = TRUE,
    opts = list("reproducible.cloudFolderID" = NULL)
  )
  on.exit(
    {
      try(googledrive::drive_rm(googledrive::as_id(cloudFolderID)), silent = TRUE)
      try(googledrive::drive_rm(googledrive::as_id(tmpCloudFolderID)), silent = TRUE)
    },
    add = TRUE
  )

  ras <- terra::rast(terra::ext(0, 1, 0, 1), resolution = 1, vals = 1)
  ras <- terra::writeRaster(ras, filename = tmpfile[1], overwrite = TRUE)

  gdriveLs1 <- data.frame(name = "GADM", id = "sdfsd", drive_resource = list(sdfsd = 1))
  tmpCloudFolderID <- checkAndMakeCloudFolderID(create = TRUE)
  gdriveLs <- driveLs(cloudFolderID = NULL, "sdfsdf")
  expect_true(NROW(gdriveLs) == 0)
  expect_true(is(checkAndMakeCloudFolderID("testy"), "dribble") ||
    is(checkAndMakeCloudFolderID("testy"), "character"))
  cloudFolderID <- checkAndMakeCloudFolderID("testy", create = TRUE)
  testthat::with_mocked_bindings(
    retry = function(..., retries = 1) TRUE,
    {
      if (useDBI()) {
        # Need to convert to cloudUpload from Cache
        mess1 <- capture_messages(
          capture_warnings( # about cache repo -- not the point here
            expect_error(
              cloudUploadFromCache( # outputToSave = ,
                isInCloud = FALSE, outputHash = "sdfsiodfja",
                # gdriveLs = gdriveLs1,
                cloudFolderID = cloudFolderID,
                cachePath = tmpCache
              )
            )
          )
        )
      }
    }
  )

  a <- cloudUploadRasterBackends(ras, cloudFolderID = cloudFolderID)
  mess1 <- capture_messages(expect_error(expect_warning({
    a <- cloudDownload(
      outputHash = "sdfsd", newFileName = "test.tif",
      gdriveLs = gdriveLs1, cloudFolderID = "testy", cachePath = tmpCache
    )
  })))
  expect_true(sum(grepl("Downloading cloud copy of test\\.tif", mess1)) == 1)
  testthat::with_mocked_bindings(
    retry = function(..., retries = 1) TRUE,
    {
      # cloudFolderID can't be meaningless "character", but retry is TRUE
      warns <- capture_warnings({
        err <- capture_error({
          cloudDownloadRasterBackend(output = ras, cachePath = tmpCache, cloudFolderID = "character")
        })
      })
      expect_true(is.null(err))
    }
  )

  a <- new.env(parent = emptyenv())
  a$a <- list(ras, ras)
  expect_true(all(unlist(isOrHasRaster(a))))
})

test_that("Filenames for environment", {
  testInit(c("terra"),
    tmpFileExt = c(".tif", ".grd", ".tif", ".tif", ".grd"),
    opts = list("reproducible.ask" = FALSE)
  )

  s <- new.env(parent = emptyenv())
  s$r <- terra::rast(terra::ext(0, 10, 0, 10), vals = 1, resolution = 1)
  s$r2 <- terra::rast(terra::ext(0, 10, 0, 10), vals = 1, resolution = 1)
  s$r <- suppressWarningsSpecific(
    terra::writeRaster(s$r, filename = tmpfile[1], overwrite = TRUE),
    "NOT UPDATED FOR PROJ >= 6"
  )
  s$r2 <- suppressWarningsSpecific(
    terra::writeRaster(s$r2, filename = tmpfile[3], overwrite = TRUE),
    "NOT UPDATED FOR PROJ >= 6"
  )
  s$s <- c(s$r, s$r2)
  s$b <- terra::writeRaster(s$s, filename = tmpfile[5], overwrite = TRUE)

  Fns <- Filenames(s)

  fnsGrd <- unlist(normPath(Filenames(s$b)))
  expect_true(identical(c(Fns[["b1"]], Fns[["b2"]]), fnsGrd))
  expect_true(identical(Fns[["r"]], normPath(Filenames(s$r))))
  expect_true(identical(Fns[["r2"]], normPath(Filenames(s$r2))))
  expect_true(identical(
    c(Fns[["s1"]], Fns[["s2"]]),
    sapply(seq_len(nlayers2(s$s)), function(rInd) normPath(Filenames(s$s[[rInd]])))
  ))

  FnsR <- Filenames(s$r)
  expect_true(identical(FnsR, normPath(Filenames(s$r))))

  FnsS <- Filenames(s$s)
  expect_true(identical(FnsS, sapply(
    seq_len(nlayers2(s$s)),
    function(rInd) normPath(Filenames(s$s[[rInd]]))
  )))

  FnsB <- Filenames(s$b)
  expect_true(identical(FnsB, fnsGrd))

  # Another stack with identical files
  rlogoFiles <- system.file("ex/test.grd", package = "terra")
  rlogoFiles <- c(rlogoFiles, gsub("grd$", "gri", rlogoFiles))
  secondSet <- file.path(tmpdir, c("one.grd", "one.gri"))
  res <- suppressWarnings(file.link(rlogoFiles, secondSet))
  if (all(res)) {
    b <- c(terra::rast(rlogoFiles[1]), terra::rast(secondSet[1]))
    expect_true(identical(sort(normPath(c(rlogoFiles, secondSet))), sort(Filenames(b))))
  }

  # Test duplicated filenames in same Stack
  b <- c(terra::rast(rlogoFiles[1]), terra::rast(rlogoFiles[1]))
  expect_true(identical(sort(normPath(c(rlogoFiles))), unique(sort(Filenames(b, allowMultiple = TRUE)))))

  rlogoFiles <- system.file("ex/test.grd", package = "terra")
  rlogoDir <- dirname(rlogoFiles)
  b <- terra::rast(rlogoFiles)
  rlogoFiles <- c(rlogoFiles, gsub("grd$", "gri", rlogoFiles))
  expect_true(identical(
    sort(normPath(dir(pattern = "test", rlogoDir, full.names = TRUE))),
    sort(Filenames(b))
  ))
})

test_that("test miscellaneous fns (part 3)", {
  testInit(opts = list(datatable.print.class = FALSE))

  x1 <- append(as.list(c(0, 1, -1, 10^(-(1:10)))), as.list(c(0L, 1L)))
  a <- lapply(x1, roundTo6Dec)

  # Keeps class
  expect_true(all(unlist(lapply(seq_along(x1), function(y) identical(class(x1[[y]]), class(a[[y]]))))))
  whBig <- which(x1 >= 10e-7)
  expect_true(identical(x1[whBig], a[whBig]))
  whSmall <- which(abs(unlist(x1)) < 10e-7 & unlist(x1) != 0)
  expect_false(all(unlist(lapply(whSmall, function(ws) identical(x1[[ws]], a[[ws]])))))
  whWhole <- which(unlist(x1) %% 1 != unlist(x1))
  expect_true(all(unlist(lapply(whWhole, function(ws) identical(x1[[ws]], a[[ws]])))))
  whZero <- which(unlist(x1) == 0)
  expect_true(all(unlist(lapply(whZero, function(ws) identical(x1[[ws]], a[[ws]])))))

  out <- capture_messages(messageDF(cbind(a = 1.1232), round = 2))
  out <- strsplit(out, "\n")[[1]] # the prev line is all on one line now (Dec 2023), with \n separating
  expect_true(is.character(out))
  expect_identical(length(out), 2L) ## TODO: only passes when run line by line interactively
  expect_true(is.numeric(as.numeric(gsub("\033.*", "", gsub(".*: ", "", out)[2]))))

  out <- capture_messages(messageDF(cbind(a = 1.1232), round = 2, colnames = FALSE))
  expect_true(is.character(out))
  expect_identical(length(out), 1L) ## TODO: only passes when run line by line interactively

  expect_true(is.numeric(as.numeric(gsub("\033.*", "", gsub(".*: ", "", out)))))


  out <- capture_messages(messageDF(1.1232, round = 2, colnames = TRUE))
  out <- strsplit(out, "\n")[[1]] # the prev line is all on one line now (Dec 2023), with \n separating
  expect_true(is.character(out))
  expect_identical(length(out), 2L) ## TODO: only passes when run line by line interactively
  expect_true(is.numeric(as.numeric(gsub("\033.*", "", gsub(".*: ", "", out)[2]))))
})

test_that("test set.randomseed", {
  skip_if(getRversion() < "4.2") # Can't figure out why this doesn't wok
  testInit()

  N <- 1e4
  a <- integer(N)
  for (i in 1:N) {
    a[i] <- set.randomseed()
  }
  expect_false(any(duplicated(a)))
})
