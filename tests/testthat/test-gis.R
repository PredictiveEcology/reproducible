test_that("testing prepInputs with deauthorized googledrive", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("googledrive")

  testInit("terra", needGoogleDriveAuth = TRUE)
  withr::local_dir(tmpdir)

  testthat::with_mocked_bindings(
    isInteractive = function() {
      FALSE
    },
    {
      noisyOutput <- capture.output({
        warn <- capture_warnings({
          BCR6_VT <- skip_on_transient_http(prepInputs(
            alsoExtract = "similar",
            url = "https://drive.google.com/open?id=1sEiXKnAOCi-f1BF7b4kTg-6zFlGr0YOH",
            targetFile = "BCR6.shp",
            overwrite = TRUE
          ))
        })
      })
    }
  )
  expect_true(is(BCR6_VT, vectorType()))

})

test_that("prepInputs with an sf point archive, and a `fun` that reads its calling env", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("sf")

  testInit("sf", needGoogleDriveAuth = FALSE, needInternet = TRUE)
  withr::local_dir(tmpdir)

  ## `fun` is an unquoted call whose argument (`theQuietVar`) lives in this
  ## frame, not in prepInputs() -- resolving it is the point of this test.
  theQuietVar <- TRUE
  co <- capture.output(NFDB_PT <-
    skip_on_transient_http(prepInputs(
      url = theNFDBpointUrl,
      overwrite = TRUE,
      fun = sf::st_read(targetFile, quiet = theQuietVar)
    ))
  )
  expect_is(NFDB_PT, "sf")

  ## the shapefile sidecars all come out of the archive, not just the .shp
  expect_true(all(c("zip", "shp", "shx", "dbf", "prj") %in%
    fileExt(dir(pattern = "NFDB_point"))))

  ## sf-only: this test declares sf, and does not need terra for a bbox
  studyArea <- sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(NFDB_PT)))
  noisyOutput <- capture.output({
    warn <- capture_warnings({
      NFDB_PT_sm <- Cache(postProcess, NFDB_PT, studyArea = studyArea)
    })
  })
  expect_is(NFDB_PT_sm, "sf")
  if (!all(grepl("attribute variables are assumed to be spatially constant", warn))) {
    warnings(warn)
  }
})

test_that("testing rebuildColors", {
  # ONLY RELEVANT FOR RASTER
  testInit(needGoogleDriveAuth = FALSE, "raster")

  x <- raster::raster(extent(0, 10, 0, 10), vals = runif(100, 0, 197))
  origColors <- list(origColors = character(0), origMinValue = 0, origMaxValue = 197.100006103516)
  expect_is(rebuildColors(x, origColors), "Raster")
})
