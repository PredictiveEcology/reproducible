templateURL <- "https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/SCANFI/v1/SCANFI_sps_douglasFir_SW_2020_v1.2.tif"

# Small polygon in BC in geographic coordinates
studyAreaBC <- function() {
  terra::vect(
    matrix(c(-122, 50.95, -121.95, 50.95, -121.95, 51, -122, 51, -122, 50.95), ncol = 2, byrow = TRUE),
    type = "polygons", crs = "EPSG:4326"
  )
}

test_that("prepInputsCOG returns NULL for invalid pre-conditions", {
  skip_if_not_installed("terra")

  expect_identical(prepInputsCOG(NULL,        cropTo = studyAreaBC()), "NULL")
  expect_identical(prepInputsCOG("not-a-url", cropTo = studyAreaBC()), "NULL")
  expect_identical(prepInputsCOG(templateURL),                         "NULL") # no to/cropTo/maskTo

  # Non-GeoTiff URLs (archives etc.) must short-circuit before any /vsicurl/ read.
  expect_identical(
    prepInputsCOG("https://geodata.ucdavis.edu/geodata/elv/CAN_elv_msk.zip",
                  cropTo = studyAreaBC()),
    "NULL"
  )
  expect_identical(
    prepInputsCOG("https://example.com/data.tar.gz", cropTo = studyAreaBC()),
    "NULL"
  )
  # Query/fragment after a .tif must still be recognized as a tif.
  expect_identical(
    prepInputsCOG("https://example.com/x.zip?foo=bar", cropTo = studyAreaBC()),
    "NULL"
  )
})

test_that("prepInputsCOG returns a windowed SpatRaster for the SCANFI tif", {
  skip_on_cran()
  testInit("terra", needInternet = TRUE, verbose = FALSE)

  result <- skip_if_transient_stream_warnings(
    prepInputsCOG(templateURL, cropTo = studyAreaBC())
  )

  expect_true(.isSpatRaster(result))
  expect_gt(terra::ncell(result), 0L)
})

test_that("prepInputs COG fast-path works with cropTo/maskTo (stays in native CRS)", {
  skip_on_cran()
  testInit("terra", needInternet = TRUE, verbose = FALSE)

  result <- skip_if_transient_stream_warnings(
    prepInputs(url = templateURL, cropTo = studyAreaBC(), maskTo = studyAreaBC(),
               destinationPath = tmpdir)
  )

  expect_true(.isSpatRaster(result))
  expect_gt(terra::ncell(result), 0L)
})
