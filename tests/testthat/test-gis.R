test_that("testing prepInputs with deauthorized googledrive", {
  skip_on_cran()
  skip_on_ci()

  if (interactive()) {
    testInit("terra", needGoogleDriveAuth = TRUE)
    withr::local_dir(tmpdir)

    # if (Sys.info()["user"] == "emcintir") {
    #   googledrive::drive_deauth()
    #   googledrive::drive_auth("eliotmcintire@gmail.com", cache = "C:/Eliot/.secret")
    #   on.exit(googledrive::drive_deauth())
    testthat::with_mocked_bindings(
      isInteractive = function() {
        FALSE
      },
      {
        noisyOutput <- capture.output({
          warn <- capture_warnings({
            BCR6_VT <- prepInputs(
              alsoExtract = "similar",
              url = "https://drive.google.com/open?id=1sEiXKnAOCi-f1BF7b4kTg-6zFlGr0YOH",
              targetFile = "BCR6.shp",
              overwrite = TRUE
            )
          })
        })
      }
    )
    expect_true(is(BCR6_VT, vectorType()))
    # }

    if (.requireNamespace("sf", stopOnFALSE = FALSE)) {
      theQuietVar <- TRUE # test for finding this
      co <- capture.output(NFDB_PT <- # Cache(
        prepInputs(
          url = "http://cwfis.cfs.nrcan.gc.ca/downloads/nfdb/fire_pnt/current_version/NFDB_point.zip",
          overwrite = TRUE,
          fun = sf::st_read(targetFile, quiet = theQuietVar)
        )
      )
      expect_is(NFDB_PT, "sf")
      expect_true(all(c("zip", "sbx", "shp", "xml", "shx", "sbn") %in%
        fileExt(dir(pattern = "NFDB_point"))))

      noisyOutput <- capture.output({
        warn <- capture_warnings({
          NFDB_PT_BCR6 <- Cache(postProcess, NFDB_PT, studyArea = BCR6_VT)
        })
      })
      if (!all(grepl("attribute variables are assumed to be spatially constant", warn))) {
        warnings(warn)
      }
    }
  }
})

test_that("testing rebuildColors", {
  # ONLY RELEVANT FOR RASTER
  testInit(needGoogleDriveAuth = FALSE, "raster")

  x <- raster::raster(extent(0, 10, 0, 10), vals = runif(100, 0, 197))
  origColors <- list(origColors = character(0), origMinValue = 0, origMaxValue = 197.100006103516)
  expect_is(rebuildColors(x, origColors), "Raster")
})
