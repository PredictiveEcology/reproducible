test_that("symlinks work with cache, input, output paths", {
  skip_on_cran()
  # skip_on_os("windows")
  skip_if_not_installed("withr")

  testInit(c("sf", "terra"), opts = list(
    "rasterTmpDir" = tempdir2(rndstr(1, 6)),
    "reproducible.inputPaths" = NULL,
    "reproducible.overwrite" = TRUE,
    reproducible.showSimilar = TRUE
  ), tmpFileExt = (".tif"), needInternet = FALSE)

  currentDir <- tmpdir


  # This will only test "different drive" if /mnt/scratch or tempdir() are on different drives.
  #  For Windows, this "different drive" must be on the a local volume, not a network drive
  #  file.symlink, in theory, will work on Windows, but user needs to be an Administrator; so this is
  #  not testable here.
  linkedDir <- ifelse(
    dir.exists("/mnt/scratch"),
    file.path("/mnt/scratch", Sys.info()[["user"]], basename(tempfile())),
    tempfile()
  ) |>
    file.path("reproducible_test_symlinks") |>
    checkPath(create = TRUE)

  linkedCacheDir <- file.path(linkedDir, "cache") |>
    checkPath(create = TRUE)
  linkedInputDir <- file.path(linkedDir, "inputs") |>
    checkPath(create = TRUE)
  linkedOutputDir <- file.path(linkedDir, "outputs") |>
    checkPath(create = TRUE)

  localDirs <- list(
    cacheDir = file.path(currentDir, "cache"),
    inputDir = file.path(currentDir, "inputs"),
    outputDir = file.path(currentDir, "outputs")
  )

  # test initial state
  expect_identical(character(), unlist(lapply(localDirs, dir)))

  linkFn <- if (isWindows()) Sys.junction else file.symlink
  expect_true(linkFn(linkedCacheDir, localDirs$cacheDir))

  # On Windows on Github actions, one of these paths has Windows `shortPathName`
  #   for c:/Users/RUNNER~1, but second has long name; need normalizePath in addition to fs::
  expect_identical(normalizePath(mustWork = FALSE, winslash = "/", fs::as_fs_path(linkedCacheDir)),
                   normalizePath(mustWork = FALSE, winslash = "/", fs::link_path(localDirs$cacheDir)))

  expect_true(linkFn(linkedInputDir, localDirs$inputDir))
  expect_identical(normalizePath(mustWork = FALSE, winslash = "/", fs::as_fs_path(linkedInputDir)),
                   normalizePath(mustWork = FALSE, winslash = "/", fs::link_path(localDirs$inputDir)))

  expect_true(linkFn(linkedOutputDir, localDirs$outputDir))
  expect_identical(normalizePath(mustWork = FALSE, winslash = "/", fs::as_fs_path(linkedOutputDir)),
                   normalizePath(mustWork = FALSE, winslash = "/", fs::link_path(localDirs$outputDir)))

  withr::local_options("reproducible.cachePath" = asPath(localDirs$cacheDir))
  on.exit({
    unlink(linkedDir, recursive = TRUE)
  }, add = TRUE)

  ## CRS
  targetCRS <- paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
                     "+x_0=0 +y_0=0 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

  luxFile <- system.file("ex/lux.shp", package = "terra")
  luxVect <- terra::vect(luxFile)
  luxVectSmall <- luxVect[1, ]
  r <- terra::rast(luxVect, ncols=75, nrows=100)
  luxRast <- terra::rasterize(luxVect, r, "NAME_2")
  luxRastFile <- writeRaster(filename = tmpfile, luxRast)

  silence <- capture.output(
    studyArea <- prepInputs(
      targetFile = luxFile,
      destinationPath = asPath(localDirs$inputDir),
      useCache = FALSE
    ) |> Cache()
  )

  # Test useCache = FALSE -- this shouldn't need to be tested here, but it was failing here
  #   when first detected
  filesInCache <- dir(CacheStorageDir())
  for (i in 1:2)
    filesInCache <- gsub("^(.+)\\..+$", "\\1", filesInCache)
  filesInCache <- unique(filesInCache)
  expect_identical(length(filesInCache), 1L)

  rass <- prepInputs(targetFile = Filenames(luxRastFile),
             to = luxVectSmall,
             writeTo = asPath(file.path(localDirs$outputDir, basename(tmpfile))),
             useCache = FALSE
  ) |> Cache()

  testTif <- prepInputs(url = urlTif1, destinationPath = asPath(localDirs$inputDir),
                        useCache = FALSE) |>
    Cache()







  # These are VERY large for automated testing; reworked all the functionality to
  #   use small files
  ## get studyArea (Ontario forest management units)
  # studyArea <- {
  #   prepInputs(
  #     url = "https://www.gisapplication.lrc.gov.on.ca/fmedatadownload/Packages/FORMGMT.zip",
  #     destinationPath = asPath(inputDir),
  #     targetFile = "FOREST_MANAGEMENT_UNIT.shp",
  #     alsoExtract = "similar",
  #     fun = "sf::st_read"
  #   ) |>
  #     sf::st_transform(crs = targetCRS) |>
  #     sf::st_union()
  # } |>
  #   Cache()
  #
  # ## prep LCC for studyArea and write to outputDir
  # browser()
  # LCC <- Cache(
  #   prepInputs,
  #   url = paste0(
  #     "https://ftp.maps.canada.ca/pub/nrcan_rncan/",
  #     "Land-cover_Couverture-du-sol/canada-landcover_canada-couverture-du-sol/",
  #     "CanadaLandcover2010.zip"
  #   ),
  #   destinationPath = asPath(inputDir),
  #   # to = studyArea,
  #   cropTo = studyArea,
  #   # method = "near",
  #   datatype = "INT2U",
  #   fun = "terra::rast",
  #   targetFile = asPath("CAN_LC_2010_CAL.tif"),
  #   writeTo = asPath(file.path(outputDir, "LCC_ON_FMU.tif"))
  # )

  ## check files exist in the correct places
  ### no files at top level
  expect_true(all(list.files(linkedDir) %in% c("cache", "inputs", "outputs")))

  ### cache files in 'cache/'
  expect_true(length(list.files(linkedCacheDir)) > 0)
  expect_true(length(list.files(linkedCacheDir)) < 16) # Test for `useCache = FALSE`; should be only 3 unique cacheIds
  expect_equal(length(unique(showCache()$cacheId)), 3L)

  ### downloaded files etc. in 'inputs/'
  # expect_true(file.exists(file.path(linkedInputDir, "CanadaLandcover2010.zip")))
  expect_true(file.exists(file.path(linkedInputDir, "CHECKSUMS.txt")))
  expect_true(file.exists(file.path(linkedInputDir, "DEM.tif")))

  ### output files in 'outputs/'
  expect_true(file.exists(file.path(linkedOutputDir, basename(tmpfile))))
  # expect_identical(terra::sources(LCC), file.path(linkedOutputDir, "LCC_ON_FMU.tif"))

  # test end state
  expect_false(identical(character(), unlist(lapply(localDirs, dir))))
  ## cleanup
  try(unlink(linkedDir, recursive = TRUE))
  expect_identical(character(), unlist(lapply(localDirs, dir)))

  # withr::deferred_run()
})
