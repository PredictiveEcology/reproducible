test_that("prepInputs correctly unzips large files", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not(isInteractive(), "tests extracting large files should be run manually devtools::test()")
  skip_if_not(getOption("reproducible.runLargeFileTests"))
  ## based on #145. extracted file is ~30 GB so this takes a long time to test!
  testInit("terra")
  # tmpdir <- "/mnt/d/temp" # need a drive that is large enough
  # if (!"emcintir" %in% Sys.info()["user"] || (!dir.exists(tmpdir)))
  #   skip("This requires a lot of drive space")
  targFile <- "CA_harvest_year_1985_2015.tif"
  on.exit(
    {
      unlink(file.path(tmpdir, targFile), recursive = TRUE)
    },
    add = TRUE
  )

  url <- "https://opendata.nfis.org/downloads/forest_change/CA_forest_harvest_mask_year_1985_2015.zip"
  withr::local_options(reproducible.tempPath = file.path(tmpdir, "ttt"))
  ff <- prepInputs(
    url = url,
    targetFile = targFile,
    destinationPath = asPath(tmpdir),
    fun = "terra::rast",
    userTags = c("objectName:forestHarvestMask", "goal:posthocGIS")
  )
  fout <- file.path(tmpdir, "CA_harvest_year_1985_2015.tif")
  expect_true(identical(normPath(Filenames(ff)), fout))
  expect_true(file.info(fout)[["size"]] > 28 * 1024^3)
})

test_that("Issue 181 geodatabase file", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not(isInteractive(), "test #2: extracting large files should be run manually devtools::test()")
  skip_if_not(getOption("reproducible.runLargeFileTests"))

  ## based on #145. extracted file is ~30 GB so this takes a long time to test!
  testInit(c("terra", "googledrive"), needGoogleDriveAuth = TRUE)
  rstLCC <- Cache(prepInputs,
    targetFile = "EOSD_Mosaic.gdb",
    archive = "EOSD_2000_2007_combined.zip",
    alsoExtract = "similar",
    url = "https://drive.google.com/file/d/1p66_P6dNdlrvAF3Mp99Xz9Bdz2lvfaQ7",
    destinationPath = tmpdir,
    filename2 = NULL,
    fun = NA,
    userTags = c(
      "outFun:Cache",
      "step:prepEOSD"
    )
  )
  expect_true(is(sf::st_read(rstLCC$targetFilePath, layer = "EOSD_Mosaic_BWC_range_clip", quiet = TRUE), "sf"))
})

test_that("Issue 242 masking fail", {
  skip_on_cran()
  skip_on_ci()
  testInit("terra", needInternet = TRUE, verbose = FALSE)
  # skip_if_not(isInteractive(), "test #3: extracting large files should be run manually with devtools::test()")
  testInit(c("terra", "googledrive"), needGoogleDriveAuth = FALSE)
  studyArea <- vect(structure(
    c(
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, -1418427,
      -1415931, -1398140, -1393331, -1382857, -1371358, -1365393, -1350548,
      -1325832, -1323489, -1322689, -1321448, -1319605, -1318493, -1309216,
      -1307387, -1305766, -1305267, -1303130, -1295515, -1296061, -1296486,
      -1298156, -1299017, -1300951, -1300981, -1301682, -1302736, -1317683,
      -1320782, -1322070, -1322341, -1324137, -1335839, -1336905, -1346232,
      -1401188, -1422500, -1423243, -1423419, -1418427, 6990085, 6999544,
      7008667, 7021910, 7028787, 7029407, 7029780, 7035153, 7036928,
      7039810, 7050226, 7044431, 7035561, 7027977, 7027582, 7026342,
      7021796, 7006494, 7006125, 6994606, 6994507, 6984394, 6975963,
      6975848, 6968927, 6961588, 6957530, 6933733, 6933100, 6926385,
      6925727, 6929217, 6932967, 6947379, 6948395, 6954427, 6980171,
      6980300, 6987758, 6989296, 6990085, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    ),
    dim = c(41L, 5L),
    dimnames = list(NULL, c("geom", "part", "x", "y", "hole"))
  ))
  crs(studyArea) <- "PROJCRS[\"unknown\",\nBASEGEOGCRS[\"unknown\",\nDATUM[\"North American Datum 1983\",\nELLIPSOID[\"GRS 1980\",6378137,298.257222101,\nLENGTHUNIT[\"metre\",1]],\nID[\"EPSG\",6269]],\nPRIMEM[\"Greenwich\",0,\nANGLEUNIT[\"degree\",0.0174532925199433],\nID[\"EPSG\",8901]]],\nCONVERSION[\"unknown\",\nMETHOD[\"Lambert Conic Conformal (2SP)\",\nID[\"EPSG\",9802]],\nPARAMETER[\"Latitude of false origin\",0,\nANGLEUNIT[\"degree\",0.0174532925199433],\nID[\"EPSG\",8821]],\nPARAMETER[\"Longitude of false origin\",-95,\nANGLEUNIT[\"degree\",0.0174532925199433],\nID[\"EPSG\",8822]],\nPARAMETER[\"Latitude of 1st standard parallel\",49,\nANGLEUNIT[\"degree\",0.0174532925199433],\nID[\"EPSG\",8823]],\nPARAMETER[\"Latitude of 2nd standard parallel\",77,\nANGLEUNIT[\"degree\",0.0174532925199433],\nID[\"EPSG\",8824]],\nPARAMETER[\"Easting at false origin\",0,\nLENGTHUNIT[\"metre\",1],\nID[\"EPSG\",8826]],\nPARAMETER[\"Northing at false origin\",0,\nLENGTHUNIT[\"metre\",1],\nID[\"EPSG\",8827]]],\nCS[Cartesian,2],\nAXIS[\"(E)\",east,\nORDER[1],\nLENGTHUNIT[\"metre\",1,\nID[\"EPSG\",9001]]],\nAXIS[\"(N)\",north,\nORDER[2],\nLENGTHUNIT[\"metre\",1,\nID[\"EPSG\",9001]]]]"
  studyAreaRas <- rasterize(
    studyArea,
    rast(extent = ext(studyArea), crs = crs(studyArea), resolution = 250)
  )
  sppAbundURL <- "https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/canada-forests-attributes_attributs-forests-canada/2001-attributes_attributs-2001/NFI_MODIS250m_2001_kNN_Species_Pinu_Alb_v1.tif"
  a <- capture.output(
    sppAbundance <- prepInputs(
      targetFile = "NFI_MODIS250m_2001_kNN_Species_Pinu_Alb_v1.tif",
      url = sppAbundURL,
      destinationPath = tempdir2(),
      fun = "terra::rast",
      to = studyAreaRas,
      overwrite = TRUE
    )
  )
  expect_true(is(sppAbundance, "SpatRaster"))
  expect_true(all.equal(ext(studyAreaRas), ext(sppAbundance)))
})
