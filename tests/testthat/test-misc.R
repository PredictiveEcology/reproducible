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

  # helpers.R
  a <- getCRANrepos(NULL)
  is.character(a)

  # opt <- getOption("repos")
  # on.exit(options("repos" = opt),
  #         add = TRUE)
  # namedSite <- c(CRAN = "https://cloud.R-project.org")
  #
  # with_mock(
  #   `isInteractive` = function() TRUE,
  #   `chooseCRANmirror` = function () options("repos" = namedSite),
  #   a <- getCRANrepos(""),
  #   print(a),
  #   expect_true(identical(tolower(unname(a)) , tolower(unname(getOption("repos")["CRAN"])))),
  #   a <- getCRANrepos("@CRAN@"),
  #   expect_true(identical(tolower(unname(a)) , tolower(unname(getOption("repos")["CRAN"]))))
  # )


})
