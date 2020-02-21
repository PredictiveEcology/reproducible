test_that("test miscellaneous fns", {
  testInitOut <- testInit("raster", tmpFileExt = c(".tif", ".grd"))
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  expect_is(searchFullEx(), "list")
  expect_true(length(searchFullEx()) > length(search()))
  expect_true(length(searchFullEx()) == (3 + length(search())))

  expect_true(all(unlist(lapply(searchFull(simplify = FALSE), is.environment))))
  expect_true(all(is.character(unlist(lapply(searchFull(simplify = FALSE), attributes)))))

  # objectSize
  a <- 1
  b <- tempfile()
  saveRDS(a, b)
  expect_is(objSize(asPath(b)), "numeric")
  expect_is(objSize(asPath(b), quick = TRUE), "object_size")

  # objSizeSession
  mess <- capture.output(d <- objSizeSession())
  expect_true(is.list(d))
  g <- unlist(d)
  expect_true(is.numeric(g))
  expect_true(any(grepl("package", names(g))))

  mess <- capture.output(d <- objSizeSession(1))
  expect_true(is.list(d))
  g <- unlist(d)
  expect_true(is.numeric(g))
  expect_true(any(grepl("package", names(g))))
  expect_true(all(names(g) %in% search() ))

  mess <- capture.output(d1 <- objSizeSession(enclosingEnvs = FALSE))
  expect_true(is.list(d1))
  g <- unlist(d1)
  expect_true(is.numeric(g))
  expect_true(any(grepl("package", names(g))))
  expect_true(sum(unlist(d1)) < sum(unlist(d)))

  mess <- capture.output(d <- objSizeSession(0))
  expect_true(!is.list(d))
  expect_true(is.numeric(d))

  # convertRasterPaths
  filenames <- normalizePath(c("/home/user1/Documents/file.txt", "/Users/user1/Documents/file.txt"),
                              winslash = "/", mustWork = FALSE)
  oldPaths <- dirname(filenames)
  newPaths <- normalizePath(c("/home/user2/Desktop", "/Users/user2/Desktop"),
                            winslash = "/", mustWork = FALSE)
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

  r3 <- writeRaster(r1, tmpfile[1], overwrite = TRUE)
  r4 <- convertRasterPaths(tmpfile[1], dirname(tmpfile[1]), newPaths)

  expect_true(identical(
    normPath(file.path(newPaths, basename(filename(r4)))),
    normPath(filename(r4))
  ))

  expect_silent(b <- retry(quote(rnorm(1)), retries = 1, silent = TRUE))
  expect_error(b <- retry(quote(stop()), retries = 1, silent = TRUE))

  expect_true(identical(NULL, basename2(NULL)))
  a <- .formalsNotInCurrentDots(rnorm, n = 1, b = 2)
  b <- .formalsNotInCurrentDots(rnorm, dots = list(n = 1, b = 2))
  expect_identical(a,b)

  a <- reproducibleOptions()
  a1 <- a[sapply(a, function(x) !is.null(x))]
  b <- options()
  expect_true(identical(sort(names(a1)), sort(names(a1[na.omit(match(names(b),names(a1)))]))))
  omit <- c("reproducible.ask", "reproducible.overwrite", "reproducible.cachePath")
  b1 <- b[names(a1)]
  b1 <- b1[!names(b1) %in% omit]
  a2 <- a1[!names(a1) %in% omit]
  expect_true(identical(b1, a2))

  expect_error(.guessAtTargetAndFun(fun = rnorm), "fun must be a")
  expect_message(.guessAtTargetAndFun(targetFilePath = NULL, filesExtracted = "", fun = "load"),
                 "Don't know which file to load")
  expect_message(.guessAtTargetAndFun(targetFilePath = NULL, filesExtracted = "hi.rds", fun = "readRDS"),
                 "targetFile was not specified.  Trying readRDS")
  expect_message(.guessAtTargetAndFun(targetFilePath = NULL, filesExtracted = c("hi.rds", "hello.rds"), fun = "readRDS"),
                 "More than one possible files to load")

  # unrar
  rarPath <- file.path(tmpdir, "tmp.rar")
  zip(zipfile = rarPath, files = tmpfile)
  unrar <- .whichExtractFn(archive = rarPath, args = "")
  expect_true(identical(unrar$fun, "unrar"))
  suppressWarnings(
    expect_error( .callArchiveExtractFn(unrar$fun, files = "", args = list(exdir = tmpCache)))
  )


  testthat::with_mock(
    "reproducible::getGDALVersion" = function() NA,
    {
      expect_false(checkGDALVersion("3.0"))
    }
  )

  skip_on_appveyor() # can't tell what the CRAN repo is
  # helpers.R
  a <- getCRANrepos(NULL)
  expect_true(is.character(a))

  a <- getCRANrepos("")
  expect_true(grepl("https://cloud.r-project.org", a))

  testthat::with_mock(
    "reproducible::isInteractive" = function() TRUE,
    "reproducible::chooseCRANmirror2" = function() {
      repos <- NULL
      repos2 <- "https://cloud.r-project.org"
      repos["CRAN"] <- repos2
      options("repos" = repos)},
    {
      out <- getCRANrepos()
      expect_true(identical("https://cloud.r-project.org", unname(out)))
    }
  )

})


test_that("test miscellaneous fns", {
  skip_if_no_token()
  testInitOut <- testInit("raster", tmpFileExt = c(".tif", ".grd"))
  on.exit({
    testOnExit(testInitOut)
    drive_rm(as_id(cloudFolderID))
    drive_rm(as_id(tmpCloudFolderID))
  }, add = TRUE)

  ras <- raster(extent(0,1,0,1), res  = 1, vals = 1)
  ras <- writeRaster(ras, file = tmpfile[1], overwrite = TRUE)

  gdriveLs1 <- data.frame(name = "GADM", id = "sdfsd", drive_resource = list(sdfsd = 1))
  expect_warning(
    tmpCloudFolderID <- checkAndMakeCloudFolderID(create = TRUE),
    "No cloudFolderID supplied")
  gdriveLs <- driveLs(cloudFolderID = NULL, "sdfsdf")
  expect_true(NROW(gdriveLs) == 0)
  expect_is(checkAndMakeCloudFolderID("testy"), "character")
  cloudFolderID <- checkAndMakeCloudFolderID("testy", create = TRUE)
  testthat::with_mock(
    "reproducible::retry" = function(..., retries = 1) TRUE,
    {
      if (useDBI()) {

        mess1 <- capture_messages(expect_error(
          cloudUpload(isInRepo = data.frame(artifact = "sdfsdf"), outputHash = "sdfsiodfja",
                      gdriveLs = gdriveLs1, cacheRepo = tmpCache)))
      } else {
        mess1 <- capture_messages(expect_error(
          cloudUpload(isInRepo = data.frame(artifact = "sdfsdf"), outputHash = "sdfsiodfja",
                      gdriveLs = gdriveLs1, cacheRepo = tmpCache)))
      }
    })
  expect_true(grepl("Uploading local copy of", mess1))
  expect_true(grepl("cacheId\\: sdfsiodfja to cloud folder", mess1))

  a <- cloudUploadRasterBackends(ras, cloudFolderID = cloudFolderID)
  mess1 <- capture_messages(expect_error(expect_warning({
    a <- cloudDownload(outputHash = "sdfsd", newFileName = "test.tif",
                       gdriveLs = gdriveLs1, cloudFolderID = "testy")
  })))
  expect_true(grepl("Downloading cloud copy of test\\.tif", mess1))
  testthat::with_mock(
    "reproducible::retry" = function(..., retries = 1) TRUE,
    {
      # cloudFolderID can't be meaningless "character", but retry is TRUE
      warns <- capture_warnings(err <- capture_error(
        cloudDownloadRasterBackend(output = ras, cacheRepo = tmpCache, cloudFolderID = "character")
      ))
      expect_true(is.null(err))
    })

  testthat::with_mock(
    "reproducible::retry" = function(..., retries = 1) TRUE,
    {
      mess1 <- capture_messages(err <- capture_error(
        cloudUploadFromCache(isInCloud = FALSE, outputHash = "sdsdfs", saved = "life",
                             cacheRepo = tmpCache)
      ))
      expect_true(all(grepl("cloudFolderID.*is missing, with no default", err)))
    })
  expect_true(grepl("Uploading new cached object|with cacheId", mess1))

  a <- new.env(parent = emptyenv())
  a$a = list(ras, ras)
  expect_true(all(isOrHasRaster(a)))

})

test_that("Filenames for environment", {
  testInitOut <- testInit(c("raster"), tmpFileExt = c(".tif", ".grd", ".tif", ".tif", ".grd"),
                          opts = list("reproducible.ask" = FALSE))

  on.exit({
    testOnExit(testInitOut)
    options(opts)
    rm(s)
  }, add = TRUE)

  s <- new.env(parent = emptyenv())
  s$r <- raster(extent(0,10,0,10), vals = 1, res = 1)
  s$r2 <- raster(extent(0,10,0,10), vals = 1, res = 1)
  s$r <- writeRaster(s$r, filename = tmpfile[1], overwrite = TRUE)
  s$r2 <- writeRaster(s$r2, filename = tmpfile[3], overwrite = TRUE)
  s$s <- stack(s$r, s$r2)
  s$b <- writeRaster(s$s, filename = tmpfile[5], overwrite = TRUE)

  Fns <- Filenames(s)

  fnsGrd <- normPath(c(filename(s$b), gsub("grd$", "gri", filename(s$b))))
  expect_true(identical(Fns$b, fnsGrd))
  expect_true(identical(Fns$r, normPath(filename(s$r))))
  expect_true(identical(Fns$r2, normPath(filename(s$r2))))
  expect_true(identical(Fns$s, sapply(seq_len(nlayers(s$s)), function(rInd) normPath(filename(s$s[[rInd]])))))

  FnsR <- Filenames(s$r)
  expect_true(identical(FnsR, normPath(filename(s$r))))

  FnsS <- Filenames(s$s)
  expect_true(identical(FnsS, sapply(seq_len(nlayers(s$s)), function(rInd) normPath(filename(s$s[[rInd]])))))

  FnsB <- Filenames(s$b)
  expect_true(identical(FnsB, fnsGrd))

  # Another stack with identical files
  rlogoFiles <- system.file("external/rlogo.grd", package="raster")
  rlogoFiles <- c(rlogoFiles, gsub("grd$", "gri", rlogoFiles))
  secondSet <- file.path(tmpdir, c("one.grd", "one.gri"))
  file.link(rlogoFiles, secondSet)
  b <- raster::stack(rlogoFiles[1], secondSet[1])
  expect_true(identical(
    sort(normPath(c(rlogoFiles, secondSet))),
    sort(Filenames(b))))

  # Test duplicated filenames in same Stack
  b <- raster::stack(rlogoFiles[1], rlogoFiles[1])
  expect_true(identical(
    sort(normPath(c(rlogoFiles))),
    sort(Filenames(b))))

  browser()

  rlogoFiles <- system.file("external/rlogo.grd", package="raster")
  b <- raster::brick(rlogoFiles)
  rlogoFiles <- c(rlogoFiles <- gsub("grd$", "gri", rlogoFiles))
  expect_true(identical(
    sort(normPath(dir(pattern = "rlogo", dirname(rlogoFiles), full.names = TRUE))),
    sort(Filenames(b))))

})
