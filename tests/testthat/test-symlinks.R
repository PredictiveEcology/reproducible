test_that("symlinks work with cache, input, output paths", {
  skip_on_cran()
  skip_on_os("windows")
  skip_if_not_installed("withr")

  testInit(c("sf", "terra"), opts = list(
    "rasterTmpDir" = tempdir2(rndstr(1, 6)),
    "reproducible.inputPaths" = NULL,
    "reproducible.overwrite" = TRUE,
    reproducible.showSimilar = TRUE
  ), needInternet = TRUE)

  currentDir <- tmpdir

  linkedDir <- ifelse(dir.exists("/mnt/scratch"), file.path("/mnt/scratch", Sys.info()[["user"]]),
                      dirname(tempdir())) |>
    file.path("reproducible_test_symlinks") |>
    checkPath(create = TRUE)
  linkedCacheDir <- file.path(linkedDir, "cache") |>
    checkPath(create = TRUE)
  linkedInputDir <- file.path(linkedDir, "inputs") |>
    checkPath(create = TRUE)
  linkedOutputDir <- file.path(linkedDir, "outputs") |>
    checkPath(create = TRUE)

  cacheDir <- file.path(currentDir, "cache")
  inputDir <- file.path(currentDir, "inputs")
  outputDir <- file.path(currentDir, "outputs")

  expect_true(file.symlink(linkedCacheDir, cacheDir))
  expect_identical(fs::as_fs_path(linkedCacheDir), fs::link_path(cacheDir))

  expect_true(file.symlink(linkedInputDir, inputDir))
  expect_identical(fs::as_fs_path(linkedInputDir), fs::link_path(inputDir))

  expect_true(file.symlink(linkedOutputDir, outputDir))
  expect_identical(fs::as_fs_path(linkedOutputDir), fs::link_path(outputDir))

  withr::local_options("reproducible.cachePath" = cacheDir)

  ## CRS
  targetCRS <- paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
                     "+x_0=0 +y_0=0 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

  ## get studyArea (Ontario forest management units)
  studyArea <- {
    prepInputs(
      url = "https://www.gisapplication.lrc.gov.on.ca/fmedatadownload/Packages/FORMGMT.zip",
      destinationPath = asPath(inputDir),
      targetFile = "FOREST_MANAGEMENT_UNIT.shp",
      alsoExtract = "similar",
      fun = "sf::st_read"
    ) |>
      sf::st_transform(crs = targetCRS) |>
      sf::st_union()
  } |>
    Cache()

  ## prep LCC for studyArea and write to outputDir
  LCC <- Cache(
    prepInputs,
    url = paste0("https://ftp.maps.canada.ca/pub/nrcan_rncan/",
                 "Land-cover_Couverture-du-sol/canada-landcover_canada-couverture-du-sol/",
                 "CanadaLandcover2010.zip"),
    destinationPath = asPath(inputDir),
    studyArea = studyArea, ## NOTE: this doesn't reproject the raster?
    method = "near",
    datatype = "INT2U",
    fun = "terra::rast",
    targetFile = asPath("CAN_LC_2010_CAL.tif"),
    writeTo = file.path(outputDir, "LCC_ON_FMU.tif")
  )

  ## check files exist in the correct places
  ### no files at top level
  expect_true(all(list.files(linkedDir) %in% c("cache", "inputs", "outputs")))

  ### cache files in 'cache/'
  expect_true(length(list.files(linkedCacheDir)) > 0)

  ### downloaded files etc. in 'inputs/'
  expect_true(file.exists(file.path(linkedInputDir, "CanadaLandcover2010.zip")))
  expect_true(file.exists(file.path(linkedInputDir, "CHECKSUMS.txt")))
  expect_true(file.exists(file.path(linkedInputDir, "FORMGMT.zip")))

  ### output files in 'outputs/'
  expect_true(file.exists(file.path(linkedOutputDir, "LCC_ON_FMU.tif")))
  expect_identical(terra::sources(LCC), file.path(linkedOutputDir, "LCC_ON_FMU.tif"))

  ## cleanup
  unlink(linkedDir, recursive = TRUE)
  withr::deferred_run()
})
