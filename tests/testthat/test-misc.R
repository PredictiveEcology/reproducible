test_that("test miscellaneous fns (part 1)", {
  testInitOut <- testInit("terra", tmpFileExt = c(".tif", ".grd"))
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
  expect_true(is.numeric(objSize(asPath(b))))
  expect_true(is(objSize(asPath(b)), "lobstr_bytes"))

  # objSizeSession
  mess <- capture.output({d <- objSizeSession()})
  expect_true(is.list(d))
  g <- unlist(d)
  expect_true(is.numeric(g))
  expect_true(any(grepl("package", names(g))))

  mess <- capture.output({d <- objSizeSession(1)})
  expect_true(is.list(d))
  g <- unlist(d)
  expect_true(is.numeric(g))
  expect_true(any(grepl("package", names(g))))
  expect_true(all(names(g) %in% search() ))

  mess <- capture.output({d1 <- objSizeSession(enclosingEnvs = FALSE)})
  expect_true(is.list(d1))
  g2 <- unlist(d1)
  expect_true(is.numeric(g2))
  expect_true(any(grepl("package", names(g2))))

  # NO LONGER RELIABLE TEST BECAUSE OF NEW REMOVAL OF PACKAGES fEB 24 2021
  # expect_true(sum(unlist(d1)) < sum(unlist(d)))

  mess <- capture.output({d <- objSizeSession(0)})
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

  r3 <- suppressWarnings(.writeRaster(r1, tmpfile[2], overwrite = TRUE)) ## TODO: raster needs updating for crs stuff
  r4 <- suppressWarnings(convertRasterPaths(tmpfile[2], dirname(tmpfile[1]), newPaths))  ## TODO: raster needs updating for crs stuff

  expect_true(identical(
    normPath(file.path(newPaths, basename(Filenames(r4)))),
    normPath(Filenames(r4))
  ))

  expect_silent({b <- retry(quote(rnorm(1)), retries = 1, silent = TRUE)})
  expect_error({b <- retry(quote(stop()), retries = 1, silent = TRUE)})

  expect_true(identical(NULL, basename2(NULL)))
  a <- .formalsNotInCurrentDots(rnorm, n = 1, b = 2)
  b <- .formalsNotInCurrentDots(rnorm, dots = list(n = 1, b = 2))
  expect_identical(a,b)
})

test_that("setting options works correctly", {
  testInitOut <- testInit()
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  a <- reproducibleOptions()
  a1 <- a[sapply(a, function(x) !is.null(x))]
  b <- options()
  bbb <- match(names(b), names(a1))
  # expect_true(identical(sort(names(a1)), sort(names(a1[na.omit(bbb)]))))
  expect_true(identical(sort(names(a1)), sort(names(a1[bbb[!is.na(bbb)]]))))
  omit <- c(names(testInitOut$opts), names(testInitOut$optsAsk))
  b1 <- b[names(a1)]
  b1 <- b1[!names(b1) %in% omit]
  a2 <- a1[!names(a1) %in% omit]

  # tempPath will be different if it was already set in a different test_that; so omit here
  b1$reproducible.tempPath <- NULL
  a2$reproducible.tempPath <- NULL
  expect_identical(b1, a2)
})

test_that("guessAtTargetAndFun works correctly", {
  testInitOut <- testInit()
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  # expect_error(.guessAtTargetAndFun(fun = rnorm), "fun must be a")
  expect_message(.guessAtTargetAndFun(targetFilePath = NULL, filesExtracted = "", fun = "load"),
                 "Don't know which file to load")
  expect_message(.guessAtTargetAndFun(targetFilePath = NULL, filesExtracted = "hi.rds", fun = "readRDS"),
                 "targetFile was not specified.")
  expect_message(.guessAtTargetAndFun(targetFilePath = NULL, filesExtracted = c("hi.rds", "hello.rds"), fun = "readRDS"),
                 "More than one possible files to load")
})

test_that("unrar is working as expected", {
  testInitOut <- testInit("raster", tmpFileExt = c(".tif", ".grd"))
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  rarPath <- file.path(tmpdir, "tmp.rar")
  utils::zip(zipfile = rarPath, files = tmpfile)
  unrar <- .whichExtractFn(archive = rarPath, args = "")
  expect_true(identical(unrar$fun, "unrar"))
  suppressWarnings(
    expect_error(.callArchiveExtractFn(unrar$fun, files = "", args = list(exdir = tmpCache)))
  )
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
  s$r <- raster(extent(0, 10, 0, 10), vals = 1, res = 1)
  s$r2 <- raster(extent(0, 10, 0, 10), vals = 1, res = 1)
  s$r <- suppressWarningsSpecific(.writeRaster(s$r, filename = tmpfile[1], overwrite = TRUE),
                                  "NOT UPDATED FOR PROJ >= 6")
  s$r2 <- suppressWarningsSpecific(.writeRaster(s$r2, filename = tmpfile[3], overwrite = TRUE),
                                   "NOT UPDATED FOR PROJ >= 6")
  s$s <- stack(s$r, s$r2)
  s$b <- .writeRaster(s$s, filename = tmpfile[5], overwrite = TRUE)

  Fns <- Filenames(s)

  fnsGrd <- unlist(normPath(c(filename(s$b), gsub("grd$", "gri", filename(s$b)))))
  expect_true(identical(c(Fns[["b1"]], Fns[["b2"]]), fnsGrd))
  expect_true(identical(Fns[["r"]], normPath(filename(s$r))))
  expect_true(identical(Fns[["r2"]], normPath(filename(s$r2))))
  expect_true(identical(c(Fns[["s1"]], Fns[["s2"]]),
                        sapply(seq_len(nlayers(s$s)), function(rInd) normPath(filename(s$s[[rInd]])))))

  FnsR <- Filenames(s$r)
  expect_true(identical(FnsR, normPath(filename(s$r))))

  FnsS <- Filenames(s$s)
  expect_true(identical(FnsS, sapply(seq_len(nlayers(s$s)),
                                     function(rInd) normPath(filename(s$s[[rInd]])))))

  FnsB <- Filenames(s$b)
  expect_true(identical(FnsB, fnsGrd))

  # Another stack with identical files
  rlogoFiles <- system.file("external/rlogo.grd", package = "raster")
  rlogoFiles <- c(rlogoFiles, gsub("grd$", "gri", rlogoFiles))
  secondSet <- file.path(tmpdir, c("one.grd", "one.gri"))
  res <- suppressWarnings(file.link(rlogoFiles, secondSet))
  if (all(res)) {
    b <- raster::stack(rlogoFiles[1], secondSet[1])
    expect_true(identical(sort(normPath(c(rlogoFiles, secondSet))), sort(Filenames(b))))
  }

  # Test duplicated filenames in same Stack
  b <- raster::stack(rlogoFiles[1], rlogoFiles[1])
  expect_true(identical(sort(normPath(c(rlogoFiles))), sort(Filenames(b))))

  rlogoFiles <- system.file("external/rlogo.grd", package = "raster")
  rlogoDir <- dirname(rlogoFiles)
  b <- raster::brick(rlogoFiles)
  rlogoFiles <- c(rlogoFiles, gsub("grd$", "gri", rlogoFiles))
  expect_true(identical(
    sort(normPath(dir(pattern = "rlogo", rlogoDir, full.names = TRUE))),
    sort(Filenames(b))))
})

test_that("test miscellaneous fns", {
  testInitOut <- testInit()
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

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
  whZero <- which(unlist(x1) == 0 )
  expect_true(all(unlist(lapply(whZero, function(ws) identical(x1[[ws]], a[[ws]])))))

  if (FALSE) { ## TODO: fix empty messageDF outputs when run during non-interactive tests
    out <- utils::capture.output(type = "message", messageDF(cbind(a = 1.1232), round = 2))
    expect_true(is.character(out))
    expect_identical(length(out), 2L) ## TODO: only passes when run line by line interactively
    expect_true(is.numeric(as.numeric(gsub(".*: ", "", out)[2])))

    out <- utils::capture.output(type = "message", messageDF(cbind(a = 1.1232), round = 2, colnames = FALSE))
    expect_true(is.character(out))
    expect_identical(length(out), 1L) ## TODO: only passes when run line by line interactively
    expect_true(is.numeric(as.numeric(gsub(".*: ", "", out)[2])))

    out <- utils::capture.output(type = "message", messageDF(1.1232, round = 2, colnames = TRUE))
    expect_true(is.character(out))
    expect_identical(length(out), 2L) ## TODO: only passes when run line by line interactively
    expect_true(is.numeric(as.numeric(gsub(".*: ", "", out)[2])))
  }
})
