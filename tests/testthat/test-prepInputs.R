test_that("prepInputs doesn't work (part 1)", {
  skip_on_cran()
  skip_on_ci()

  testInit(
    "terra",
    opts = list(
      rasterTmpDir = tempdir2(rndstr(1, 6)),
      reproducible.inputPaths = NULL,
      reproducible.overwrite = TRUE,
      reproducible.useMemoise = FALSE,
      reproducible.showSimilar = TRUE
    ),
    needInternet = TRUE
  )

  withr::local_options(reproducible.cachePath = tmpdir)

  globalNoisy <- capture.output({
    ## Add a study area to Crop and Mask to
    coords <- structure(
      c(-122.98, -116.1, -99.2, -106, -122.98, 59.9, 65.73, 63.58, 54.79, 59.9),
      .Dim = c(5L, 2L)
    )
    StudyArea <- terra::vect(coords, "polygons")
    terra::crs(StudyArea) <- crsToUse

    dPath <- file.path(tmpdir, "ecozones")

    url <- "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip"

    mess <- capture_messages({
      shpEcozone <- prepInputs(destinationPath = dPath, url = url)
    })
    expect_true(any(grepl(mess, pattern = "ecozone_shp[.]zip")))
    expect_true(any(grepl(mess, pattern = "Appending")))
    expect_true(is(shpEcozone, vectorType()))

    # test sf::st_read vs "sf::st_read" -- sf::st_read didn't work before Oc 29, 2024
    if (.requireNamespace("sf")) {
      out <- prepInputs(
        targetFile = "Ecozones/ecozones.shp",
        destinationPath = dPath,
        fun = sf::st_read
      )
      expect_is(out, "sf")
    }

    # Robust to partial file deletions:
    unlink(dir(dPath, full.names = TRUE)[1:3])
    expect_error(terra::vect(file.path(dPath, "ecozone_shp.zip")))
    rm(shpEcozone)
    shpEcozone1 <- prepInputs(destinationPath = dPath, url = url)
    expect_true(is(shpEcozone1, vectorType()))
    unlink(dPath, recursive = TRUE)

    ### url, targetFile, alsoExtract # # #g
    # Once this is done, can be more precise in operational code:
    #  specify targetFile, alsoExtract, and fun, wrap with Cache
    ecozoneFilename <- file.path(dPath, "Ecozones/ecozones.shp")
    ## fmt: skip
    ecozoneFiles <- c(
      "ecozones.dbf", "ecozones.prj", "ecozones.sbn",
      "ecozones.sbx", "ecozones.shp", "ecozones.shx"
    )
    shpEcozone2 <- prepInputs(
      targetFile = ecozoneFilename,
      url = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip",
      alsoExtract = ecozoneFiles,
      destinationPath = dPath
    )

    if (.requireNamespace("sf")) {
      expect_true(is(shpEcozone2, "sf"))
      testObj <- if (!is(shpEcozone1, "sf")) as(shpEcozone1, "sf") else shpEcozone1
    }

    # As of Jan 2022 -- these objects are very different; character encoding of accents, numbers interpreted as character
    # expect_equivalent(testObj, shpEcozone2) # different attribute newCache

    ### url, targetFile, alsoExtract -- with Cache
    # specify targetFile, alsoExtract, and fun, wrap with Cache -- it is wrong b/c no subfolder
    ecozoneFilename <- file.path(dPath, "ecozones.shp")
    # Note, you don't need to "alsoExtract" the archive... if the archive is not there, but the
    #   targetFile is there, it will not redownload the archive.

    unlink(dirname(ecozoneFilename), recursive = TRUE)
    # Test useCache = FALSE -- doesn't error and has no "loading from cache" or "loading from memoised"
    warn <- suppressWarningsSpecific(falseWarnings = "attribute variables are assumed to be spatially constant", {
      mess <- capture_messages({
        shpEcozoneSm <- Cache(
          prepInputs(
            url = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip",
            targetFile = reproducible::asPath(ecozoneFilename),
            alsoExtract = reproducible::asPath(ecozoneFiles),
            studyArea = StudyArea,
            destinationPath = dPath,
            writeTo = "EcozoneFile.shp",
            useCache = FALSE
          ),
          quick = "destinationPath"
        )
      })
    })
    expect_false(all(grepl("loading", mess)))

    # Test useCache -- doesn't error and loads from cache
    mess <- capture_messages({
      warn <- suppressWarningsSpecific(falseWarnings = "attribute variables are assumed to be spatially constant", {
        shpEcozoneSm <- Cache(
          prepInputs(
            url = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip",
            targetFile = reproducible::asPath(ecozoneFilename),
            alsoExtract = reproducible::asPath(ecozoneFiles),
            studyArea = StudyArea,
            destinationPath = dPath,
            writeTo = "EcozoneFile.shp",
            useCache = TRUE # with useTerra = TRUE, this is only for loading, not postProcess
          ),
          quick = "destinationPath"
        )
      })
    })

    expect_true(any(grepl(.message$LoadedCacheResult(), mess)))

    ##  archive
    ## don't pass url -- use local copy of archive only
    ## use purge = TRUE to rm checksums file, rewrite it here
    shpEcozone <- prepInputs(
      destinationPath = dPath,
      archive = file.path(dPath, "ecozone_shp.zip"),
      purge = TRUE
    )
    expect_true(is(shpEcozone, vectorType()))

    ### archive, alsoExtract char
    shpEcozone <- prepInputs(
      destinationPath = dPath,
      archive = file.path(dPath, "ecozone_shp.zip"),
      ## fmt: skip
      alsoExtract = c(
        "ecozones.dbf", "ecozones.prj", "ecozones.sbn",
        "ecozones.sbx", "ecozones.shp", "ecozones.shx"
      )
    )
    expect_true(is(shpEcozone, vectorType()))

    rm(shpEcozone)
    expect_false(exists("shpEcozone", inherits = FALSE))

    ### url, alsoExtract, archive
    ## try again with url - should *not* download, even though checksums came from the
    ##   prepInputs that had locally generated -- confirming that checksums with a manually copied file will work
    ##   instead of forcing prepInputs to get the file.
    shpEcozone <- prepInputs(
      destinationPath = dPath,
      url = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip",
      archive = file.path(dPath, "ecozone_shp.zip"),
      ## fmt: skip
      alsoExtract = c(
        "ecozones.dbf", "ecozones.prj", "ecozones.sbn",
        "ecozones.sbx", "ecozones.shp", "ecozones.shx"
      )
    )
    expect_true(is(shpEcozone, vectorType()))

    ## stops if deprecated arguments used
    expect_error(prepInputs(
      destinationPath = dPath,
      url = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip",
      archive = file.path(dPath, "ecozone_shp.zip"),
      studyArea = StudyArea,
      filename2 = "use_writeTo_instead.shp"
    ))
  })
})

test_that("interactive prepInputs", {
  skip_on_cran()
  skip_on_ci()

  testInit(
    "terra",
    opts = list(
      rasterTmpDir = tempdir2(rndstr(1, 6)),
      reproducible.overwrite = TRUE,
      reproducible.inputPaths = NULL
    ),
    needGoogleDriveAuth = TRUE
  )

  globalNoisy <- capture.output({
    warns <- capture_warnings({
      test <- prepInputs(
        url = "https://drive.google.com/file/d/1BNsUiMqENJa0I8gzhO68K307ySPHbdGk/view?usp=sharing",
        destinationPath = tmpdir
      )
    })

    files <- dir(tmpdir, pattern = "FMA_Boundary")
    expect_true(length(files) == 9)
    expect_true(inherits(test, vectorType()))

    ### url, targetFile
    ## need authentication for this
    warns <- capture_warnings({
      test <- prepInputs(
        targetFile = "FMA_Boundary_Updated.shp",
        url = "https://drive.google.com/file/d/1BNsUiMqENJa0I8gzhO68K307ySPHbdGk",
        destinationPath = tmpdir
      )
    })

    ## There is a meaningless warning for this unit test -- ignore it :
    ## In rgdal::readOGR(dirname(x), fn, stringsAsFactors = stringsAsFactors,  :
    ##                  Z-dimension discarded
    expect_true(inherits(test, vectorType()))

    ## From Bird/Tati project
    testInit(
      "terra",
      opts = list(reproducible.inputPaths = NULL, reproducible.overwrite = TRUE),
      needGoogleDriveAuth = TRUE
    )
    birdSpecies <- c("BBWA", "YRWA")
    urls <- c(
      "https://drive.google.com/open?id=1CmzYNpxwWr82PoRSbHWG8yg2cC3hncfb",
      "https://drive.google.com/open?id=11Hxk0CcwJsoAnUgfrwbJhXBJNM5Xbd9e"
    )

    ### url, targetFile, archive
    outsideModule <- Map(
      x = birdSpecies,
      url = urls,
      MoreArgs = list(tmpdir = tmpdir),
      function(x, url, tmpdir) {
        ras <- prepInputs(
          targetFile = paste0(x, "_currmean.asc"),
          archive = paste0(x, "_current.zip"),
          # fun = "raster::raster",
          url = url,
          destinationPath = tmpdir,
          overwrite = TRUE
        )
      }
    )
    expect_true(inherits(outsideModule[[1]], rasterType()))
    expect_true(inherits(outsideModule[[2]], rasterType()))
    # expect_true(inherits(terra::crs(outsideModule[[2]]), "CRS"))
    if (requireNamespace("sf")) {
      expect_true(inherits(sf::st_crs(outsideModule[[1]]), "crs"))
    }
    expect_false(identical(outsideModule[[1]], outsideModule[[2]]))

    ## remove the .prj files -- test "similar"
    ### url, targetFile, archive, alsoExtract similar
    file.remove(grep(
      pattern = "asc|zip|CHECK",
      invert = TRUE,
      value = TRUE,
      dir(tmpdir, full.names = TRUE)[!isDirectory(dir(tmpdir))]
    ))

    outsideModule <- Map(
      x = birdSpecies,
      url = urls,
      MoreArgs = list(tmpdir = tmpdir),
      function(x, url, tmpdir, purge) {
        ras <- prepInputs(
          targetFile = paste0(x, "_currmean.asc"),
          archive = paste0(x, "_current.zip"),
          url = url,
          # fun = "raster::raster",
          alsoExtract = "similar",
          destinationPath = tmpdir,
          overwrite = TRUE
        )
      }
    )
    expect_true(inherits(outsideModule[[1]], rasterType()))
    expect_true(inherits(outsideModule[[2]], rasterType()))
    # expect_true(inherits(crs(outsideModule[[2]]), "CRS"))
    # expect_true(inherits(crs(outsideModule[[1]]), "CRS"))
    expect_true(!is.na(crs(outsideModule[[1]])))
    expect_false(identical(outsideModule[[1]], outsideModule[[2]]))

    ## remove the .prj files -- test "similar"
    file.remove(grep(
      pattern = "asc|zip|CHECK",
      invert = TRUE,
      value = TRUE,
      dir(tmpdir, full.names = TRUE)[!isDirectory(dir(tmpdir))]
    ))

    ### url, targetFile, archive, alsoExtract NA
    ## because alsoExtract is NA ... no other files are unzipped, so no .prj and so no CRS
    outsideModule <- Map(
      x = birdSpecies,
      url = urls,
      MoreArgs = list(tmpdir = tmpdir),
      function(x, url, tmpdir, purge) {
        ras <- prepInputs(
          targetFile = paste0(x, "_currmean.asc"),
          archive = paste0(x, "_current.zip"),
          url = url,
          alsoExtract = NULL,
          destinationPath = tmpdir,
          overwrite = TRUE
        )
      }
    )
    expect_true(inherits(outsideModule[[1]], rasterType()))
    expect_true(inherits(outsideModule[[2]], rasterType()))
    expect_false(identical(terra::crs(outsideModule[[1]]), "")) # now with subfolders & all files, has crs
    expect_false(identical(outsideModule[[1]], outsideModule[[2]]))
  })
})

test_that("preProcess doesn't work", {
  skip_on_cran()
  skip_on_ci()

  testInit(
    "terra",
    opts = list(reproducible.inputPaths = NULL, reproducible.overwrite = TRUE),
    needGoogleDriveAuth = TRUE
  )

  cls <- rasterType()
  # cls <- .fileExtsKnown()[.fileExtsKnown()[, "extension"] == "tif", "type"]

  # Note urlShapefiles1Zip, urlShapefilesZip, and urlTif1 are in helper-allEqual.R
  globalNoisy <- capture.output({
    # # # # # Comment
    # # # url
    # # # # # Comment
    mess <- capture_messages({
      warns <- capture_warnings({
        test <- prepInputs(url = urlTif1, destinationPath = tmpdir)
      })
    })
    runTest("1_2_7_10_13", cls, 1, mess,
            expectedMess = expectedMessage,
            filePattern = "DEM", tmpdir = tmpdir, test = test
    )

    # 2nd time # no targetFile, but since url is simple, can guess correctly
    mess <- capture_messages({
      warns <- capture_warnings({
        test <- prepInputs(url = urlTif1, destinationPath = tmpdir)
      })
    })

    runTest("1_2_8_10", cls, 1, mess,
            expectedMess = expectedMessage,
            filePattern = "DEM", tmpdir = tmpdir, test = test
    )
    unlink(dir(tmpdir, full.names = TRUE))

    # url is an archive on googledrive -- can get file.info from remote -- so can do checksums
    mess <- capture_messages({
      warns <- capture_warnings({
        test <- prepInputs(url = urlShapefiles1Zip, destinationPath = tmpdir)
      })
    })

    runTest("1_4_7_10_12_13", vectorType(), 5, mess,
            expectedMess = expectedMessage,
            filePattern = "Shapefile", # the file name is actually Shapefile1...
            tmpdir = tmpdir, test = test
    )

    # 2nd time # can checksums
    mess <- capture_messages({
      warns <- capture_warning({
        test <- prepInputs(url = urlShapefiles1Zip, destinationPath = tmpdir)
      })
    })
    runTest("1_8_9_10", vectorType(), 5, mess,
            expectedMess = expectedMessage,
            filePattern = "Shapefile", tmpdir = tmpdir, test = test
    )
    unlink(dir(tmpdir, full.names = TRUE))

    # # # # # Comment
    # # # url, targetFile
    # # # # # Comment
    mess <- capture_messages({
      warns <- capture_warnings({
        test <- prepInputs(url = urlTif1, targetFile = basename(urlTif1), destinationPath = tmpdir)
      })
    })
    runTest("1_2_7_13", cls, 1, mess,
            expectedMess = expectedMessage,
            filePattern = "DEM", tmpdir = tmpdir, test = test
    )

    # 2nd time # can checksums
    mess <- capture_messages({
      warns <- capture_warnings({
        test <- prepInputs(url = urlTif1, targetFile = basename(urlTif1), destinationPath = tmpdir)
      })
    })
    runTest("1_2_8", cls, 1, mess,
            expectedMess = expectedMessage,
            filePattern = "DEM", tmpdir = tmpdir, test = test
    )
    unlink(dir(tmpdir, full.names = TRUE))

    # url is an archive on googledrive --
    mess <- capture_messages({
      warns <- capture_warnings({
        test <- prepInputs(
          url = urlShapefiles1Zip,
          targetFile = "Shapefile1.shp",
          destinationPath = tmpdir
        )
      })
    })
    runTest("1_2_4_7_13", vectorType(), 5, mess,
            expectedMess = expectedMessage,
            filePattern = "Shapefile", tmpdir = tmpdir, test = test
    )

    ## 2nd time; can checksums
    mess <- capture_messages({
      warns <- capture_warnings({
        test <- prepInputs(
          url = urlShapefiles1Zip,
          targetFile = "Shapefile1.shp",
          destinationPath = tmpdir
        )
      })
    })
    runTest("1_2_8_9", vectorType(), 5, mess,
            expectedMess = expectedMessage,
            filePattern = "Shapefile", tmpdir = tmpdir, test = test
    )
    unlink(dir(tmpdir, full.names = TRUE))

    # # # # # Comment
    # # # url, alsoExtract
    # # # # # Comment
    mess <- capture_messages({
      warns <- capture_warnings({
        test <- prepInputs(url = urlTif1, alsoExtract = "DEM.tif", destinationPath = tmpdir)
      })
    })
    runTest("1_2_7_10_13", cls, 1, mess,
            expectedMess = expectedMessage,
            filePattern = "DEM", tmpdir = tmpdir, test = test
    )

    # 2nd time # can use checksums, even though don't have targetFile, b/c simple url
    mess <- capture_messages({
      warns <- capture_warnings({
        test <- prepInputs(url = urlTif1, alsoExtract = "DEM.tif", destinationPath = tmpdir)
      })
    })
    runTest("1_2_8_10", cls, 1, mess,
            expectedMess = expectedMessage,
            filePattern = "DEM", tmpdir = tmpdir, test = test
    )
    unlink(dir(tmpdir, full.names = TRUE))

    # url is an archive on googledrive --
    mess <- capture_messages({
      warns <- capture_warnings({
        test <- prepInputs(
          url = urlShapefiles1Zip,
          alsoExtract = c("Shapefile1.dbf", "Shapefile1.prj", "Shapefile1.shp", "Shapefile1.shx"),
          destinationPath = tmpdir
        )
      })
    })
    runTest("1_4_7_10_13", vectorType(), 5, mess,
            expectedMess = expectedMessage,
            filePattern = "Shapefile", tmpdir = tmpdir, test = test
    )

    # 2nd time # can't checksums because no targetfile
    mess <- capture_messages({
      warns <- capture_warnings({
        test <- prepInputs(
          url = urlShapefiles1Zip,
          alsoExtract = c("Shapefile1.dbf", "Shapefile1.prj", "Shapefile1.shp", "Shapefile1.shx"),
          destinationPath = tmpdir
        )
      })
    })
    runTest("1_8_9_10", vectorType(), 5, mess,
            expectedMess = expectedMessage,
            filePattern = "Shapefile", tmpdir = tmpdir, test = test
    )
    unlink(dir(tmpdir, full.names = TRUE), recursive = TRUE)

    # # # # # Comment
    # # # url, archive
    # # # # # Comment
    # url is an archive on googledrive -- here, zip has 2 Shapefile filesets -- Shapefile1* and Shapefile2*
    #   should extract all
    mess <- capture_messages({
      warns <- capture_warnings({
        test <- prepInputs(
          url = urlShapefilesZip,
          archive = "Shapefiles.zip",
          destinationPath = tmpdir
        )
      })
    })
    runTest("1_4_7_10_12_13", vectorType(), 9, mess,
            expectedMess = expectedMessage,
            filePattern = "Shapefile", tmpdir = tmpdir, test = test
    )

    # 2nd time # can checksums
    mess <- capture_messages({
      warns <- capture_warnings({
        test <- prepInputs(
          url = urlShapefilesZip,
          archive = "Shapefiles.zip",
          destinationPath = tmpdir
        )
      })
    })
    runTest("1_8_9_10", vectorType(), 9, mess,
            expectedMess = expectedMessage,
            filePattern = "Shapefile", tmpdir = tmpdir, test = test
    )
    unlink(dir(tmpdir, full.names = TRUE))

    # # # # # Comment
    # # # url, archive, targetFile
    # # # # # Comment
    # url is an archive on googledrive --
    mess <- capture_messages({
      warns <- capture_warnings({
        test <- prepInputs(
          url = urlShapefiles1Zip,
          archive = "Shapefiles1.zip",
          targetFile = "Shapefile1.shp",
          destinationPath = tmpdir
        )
      })
    })
    runTest("1_2_4_7_13", vectorType(), 5, mess,
            expectedMess = expectedMessage,
            filePattern = "Shapefile", tmpdir = tmpdir, test = test
    )

    # 2nd time # can checksums
    mess <- capture_messages({
      warns <- capture_warnings({
        test <- prepInputs(
          url = urlShapefiles1Zip,
          archive = "Shapefiles1.zip",
          targetFile = "Shapefile1.shp",
          destinationPath = tmpdir
        )
      })
    })
    runTest("1_2_8_9", vectorType(), 5, mess,
            expectedMess = expectedMessage,
            filePattern = "Shapefile", tmpdir = tmpdir, test = test
    )
    unlink(dir(tmpdir, full.names = TRUE))

    # # # # # Comment
    # # # url, targetFile, alsoExtract                        # # # # #
    # # # # # Comment
    # url is an archive on googledrive --
    mess <- capture_messages({
      warns <- capture_warnings({
        test <- prepInputs(
          url = urlShapefilesZip,
          targetFile = "Shapefile1.shp",
          alsoExtract = c("Shapefile1.dbf", "Shapefile1.prj", "Shapefile1.shx"),
          destinationPath = tmpdir
        )
      })
    })
    runTest("1_2_4_7_13", vectorType(), 5, mess,
            expectedMess = expectedMessage,
            filePattern = "Shapefile", tmpdir = tmpdir, test = test
    )

    # 2nd time # can checksums
    mess <- capture_messages({
      warns <- capture_warnings({
        test <- prepInputs(
          url = urlShapefilesZip,
          targetFile = "Shapefile1.shp",
          alsoExtract = c("Shapefile1.dbf", "Shapefile1.prj", "Shapefile1.shx"),
          destinationPath = tmpdir
        )
      })
    })
    runTest("1_2_8_9", vectorType(), 5, mess,
            expectedMess = expectedMessage,
            filePattern = "Shapefile", tmpdir = tmpdir, test = test
    )
    unlink(dir(tmpdir, full.names = TRUE))

    mess <- capture_messages({
      warns <- capture_warnings({
        test <- prepInputs(
          url = urlShapefilesZip,
          targetFile = "Shapefile1.shp",
          alsoExtract = c("similar"),
          destinationPath = tmpdir
        )
      })
    })
    runTest("1_2_4_7_13", vectorType(), 5, mess,
            expectedMess = expectedMessage,
            filePattern = "Shapefile", tmpdir = tmpdir, test = test
    )
    mess <- capture_messages({
      warns <- capture_warnings({
        test <- prepInputs(
          url = urlTif1,
          targetFile = "DEM.tif",
          alsoExtract = c("DEM.tif"),
          destinationPath = tmpdir
        )
      })
    })
    runTest("1_2_7_13", cls, 1, mess,
            expectedMess = expectedMessage,
            filePattern = "DEM", tmpdir = tmpdir, test = test
    )
    unlink(dir(tmpdir, full.names = TRUE))

    # # # # # Comment
    # # # url, archive, alsoExtract               # # #
    # # # # # Comment
    # url is an archive on googledrive --
    mess <- capture_messages({
      warns <- capture_warnings({
        test <- prepInputs(
          url = urlShapefilesZip,
          archive = "Shapefiles.zip",
          alsoExtract = "similar",
          destinationPath = tmpdir
        )
      })
    })
    runTest("1_4_7_10_12_13", vectorType(), 9, mess,
            expectedMess = expectedMessage,
            filePattern = "Shapefile", tmpdir = tmpdir, test = test
    )

    # 2nd time # can checksums
    mess <- capture_messages({
      warns <- capture_warnings({
        test <- prepInputs(
          url = urlShapefilesZip,
          archive = "Shapefiles.zip",
          alsoExtract = "similar",
          destinationPath = tmpdir
        )
      })
    })

    runTest("1_8_9_10_12", vectorType(), 9, mess,
            expectedMess = expectedMessage,
            filePattern = "Shapefile", tmpdir = tmpdir, test = test
    )

    unlink(dir(tmpdir, full.names = TRUE))
    ## Without a `targetFile` and with an incomplete `alsoExtract` (no .shp),
    ## `fun` cannot be guessed. preProcess() (the download/extract layer) is
    ## fine with a NULL fun — but prepInputs() is the loader and must refuse
    ## a call it cannot fulfil. (Commit 67edb6e8 dropped the stop() from the
    ## preProcess layer; prepInputs reinstates it just before process().)
    expect_error({
      mess <- capture_messages({
        warns <- capture_warnings({
          test <- prepInputs(
            url = urlShapefilesZip,
            archive = "Shapefiles.zip",
            alsoExtract = c("Shapefile1.dbf", "Shapefile1.prj", "Shapefile1.shx"),
            destinationPath = tmpdir
          )
        })
      })
    })

    unlink(dir(tmpdir, full.names = TRUE))

    # # # # # # Comment
    # # # url, targetFile, alsoExtract               # # #
    # # # # # Comment
    # url is an archive on googledrive --
    mess <- capture_messages({
      warns <- capture_warnings({
        test <- prepInputs(
          url = urlShapefilesZip,
          alsoExtract = "similar",
          targetFile = "Shapefile1.shp",
          destinationPath = tmpdir
        )
      })
    })
    runTest("1_2_4_7_13", vectorType(), 5, mess,
            expectedMess = expectedMessage,
            filePattern = "Shapefile", tmpdir = tmpdir, test = test
    )
    mess <- capture_messages({
      warns <- capture_warnings({
        test <- prepInputs(
          url = urlShapefilesZip,
          alsoExtract = "similar",
          targetFile = "Shapefile1.shp",
          destinationPath = tmpdir
        )
      })
    })
    runTest("1_2_8_9", vectorType(), 5, mess,
            expectedMess = expectedMessage,
            filePattern = "Shapefile", tmpdir = tmpdir, test = test
    )
    unlink(dir(tmpdir, full.names = TRUE))

    # 2nd time # can checksums
    mess <- capture_messages({
      warns <- capture_warnings({
        test <- prepInputs(
          url = urlShapefilesZip,
          alsoExtract = c("Shapefile1.dbf", "Shapefile1.prj", "Shapefile1.shx"),
          targetFile = "Shapefile1.shp",
          destinationPath = tmpdir
        )
      })
    })
    runTest("1_2_4_7_13", vectorType(), 5, mess,
            expectedMess = expectedMessage,
            filePattern = "Shapefile", tmpdir = tmpdir, test = test
    )

    # 2nd time # can checksums
    mess <- capture_messages({
      warns <- capture_warnings({
        test <- prepInputs(
          url = urlShapefilesZip,
          alsoExtract = c("Shapefile1.dbf", "Shapefile1.prj", "Shapefile1.shx"),
          targetFile = "Shapefile1.shp",
          destinationPath = tmpdir
        )
      })
    })
    runTest("1_2_8_9", vectorType(), 5, mess,
            expectedMess = expectedMessage,
            filePattern = "Shapefile", tmpdir = tmpdir, test = test
    )
    unlink(dir(tmpdir, full.names = TRUE))

    # # # # # Comment
    # # # url, archive, targetFile, alsoExtract               # # #
    # # # # # Comment
    # url is an archive on googledrive --
    mess <- capture_messages({
      warns <- capture_warnings({
        test <- prepInputs(
          url = urlShapefilesZip,
          archive = "Shapefiles.zip",
          alsoExtract = "similar",
          targetFile = "Shapefile1.shp",
          destinationPath = tmpdir
        )
      })
    })
    runTest("1_2_4_7_13", vectorType(), 5, mess,
            expectedMess = expectedMessage,
            filePattern = "Shapefile", tmpdir = tmpdir, test = test
    )

    # 2nd time # can checksums
    mess <- capture_messages({
      warns <- capture_warnings({
        test <- prepInputs(
          url = urlShapefilesZip,
          archive = "Shapefiles.zip",
          alsoExtract = "similar",
          targetFile = "Shapefile1.shp",
          destinationPath = tmpdir
        )
      })
    })
    runTest("1_2_8_9", vectorType(), 5, mess,
            expectedMess = expectedMessage,
            filePattern = "Shapefile", tmpdir = tmpdir, test = test
    )

    # # # # # Comment
    # # # archive
    # # # # # Comment
    # archive exists locally
    # remove all non archive files
    file.remove(grep(
      dir(tmpdir, full.names = TRUE)[!isDirectory(dir(tmpdir))],
      pattern = "\\.zip",
      invert = TRUE,
      value = TRUE
    ))

    mess <- capture_messages({
      warns <- capture_warnings({
        test <- prepInputs(archive = "Shapefiles.zip", destinationPath = tmpdir)
      })
    })

    runTest("1_4_9_10_13", vectorType(), 9, mess,
            expectedMess = expectedMessage,
            filePattern = "Shapefile", tmpdir = tmpdir, test = test
    )

    # 2nd time # can checksums
    mess <- capture_messages({
      warns <- capture_warnings({
        test <- prepInputs(archive = "Shapefiles.zip", destinationPath = tmpdir)
      })
    })
    runTest("1_9_10", vectorType(), 9, mess,
            expectedMess = expectedMessage,
            filePattern = "Shapefile", tmpdir = tmpdir, test = test
    )

    # # # # # Comment
    # # # archive, targetFile
    # # # # # Comment
    # archive exists locally
    # remove all non archive files
    file.remove(grep(
      dir(tmpdir, full.names = TRUE)[!isDirectory(dir(tmpdir))],
      pattern = "\\.zip",
      invert = TRUE,
      value = TRUE
    ))

    mess <- capture_messages({
      warns <- capture_warnings({
        test <- prepInputs(
          archive = "Shapefiles.zip",
          targetFile = "Shapefile1.shp",
          destinationPath = tmpdir
        )
      })
    })
    runTest("1_2_4_9_13", vectorType(), 9, mess,
            expectedMess = expectedMessage,
            filePattern = "Shapefile", tmpdir = tmpdir, test = test
    )

    # 2nd time # can checksums
    mess <- capture_messages({
      warns <- capture_warnings({
        test <- prepInputs(
          archive = "Shapefiles.zip",
          targetFile = "Shapefile1.shp",
          destinationPath = tmpdir
        )
      })
    })
    runTest("1_2_9", vectorType(), 9, mess,
            expectedMess = expectedMessage,
            filePattern = "Shapefile", tmpdir = tmpdir, test = test
    )

    # # # # # Comment
    # # # archive, targetFile, alsoExtract                    # # #
    # # # # # Comment
    # archive exists locally
    # remove all non archive files
    file.remove(grep(
      dir(tmpdir, full.names = TRUE)[!isDirectory(dir(tmpdir))],
      pattern = "\\.zip",
      invert = TRUE,
      value = TRUE
    ))
    mess <- capture_messages({
      warns <- capture_warnings({
        test <- prepInputs(
          archive = "Shapefiles.zip",
          targetFile = "Shapefile1.shp",
          alsoExtract = c("Shapefile1.dbf", "Shapefile1.prj", "Shapefile1.shp", "Shapefile1.shx"),
          destinationPath = tmpdir
        )
      })
    })
    runTest("1_2_4_9_13", vectorType(), 5, mess,
            expectedMess = expectedMessage,
            filePattern = "Shapefile", tmpdir = tmpdir, test = test
    )

    # 2nd time # can checksums
    mess <- capture_messages({
      warns <- capture_warnings({
        test <- prepInputs(
          archive = "Shapefiles.zip",
          targetFile = "Shapefile1.shp",
          alsoExtract = c("Shapefile1.dbf", "Shapefile1.prj", "Shapefile1.shp", "Shapefile1.shx"),
          destinationPath = tmpdir
        )
      })
    })
    runTest("1_2_9", vectorType(), 5, mess,
            expectedMess = expectedMessage,
            filePattern = "Shapefile", tmpdir = tmpdir, test = test
    )

    file.remove(grep(
      dir(tmpdir, full.names = TRUE)[!isDirectory(dir(tmpdir))],
      pattern = "\\.zip",
      invert = TRUE,
      value = TRUE
    ))
    file.remove(grep(
      dir(tmpdir, full.names = TRUE)[!isDirectory(dir(tmpdir))],
      pattern = "CHECKSUMS.txt",
      value = TRUE
    ))
    mess <- capture_messages({
      warns <- capture_warnings({
        test <- prepInputs(
          archive = "Shapefiles.zip",
          targetFile = "Shapefile1.shp",
          alsoExtract = "similar",
          destinationPath = tmpdir
        )
      })
    })
    runTest("1_2_4_9_13", vectorType(), 5, mess,
            expectedMess = expectedMessage,
            filePattern = "Shapefile", tmpdir = tmpdir, test = test
    )

    # 2nd time # can checksums
    mess <- capture_messages({
      warns <- capture_warnings({
        test <- prepInputs(
          archive = "Shapefiles.zip",
          targetFile = "Shapefile1.shp",
          alsoExtract = c("similar"),
          destinationPath = tmpdir
        )
      })
    })
    runTest("1_2_9", vectorType(), 5, mess,
            expectedMess = expectedMessage,
            filePattern = "Shapefile", tmpdir = tmpdir, test = test
    )

    # # # # # Comment
    # # # targetFile
    # # # # # Comment
    file.remove(grep(dir(tmpdir, full.names = TRUE), pattern = "CHECKSUMS.txt", value = TRUE))
    mess <- capture_messages({
      warns <- capture_warnings({
        test <- prepInputs(targetFile = "Shapefile1.shp", destinationPath = tmpdir)
      })
    })
    runTest("1_2", vectorType(), 5, mess,
            expectedMess = expectedMessage,
            filePattern = "Shapefile", tmpdir = tmpdir, test = test
    )
    mess <- capture_messages({
      warns <- capture_warnings({
        test <- prepInputs(targetFile = "Shapefile1.shp", destinationPath = tmpdir)
      })
    })
    runTest("1_2", vectorType(), 5, mess,
            expectedMess = expectedMessage,
            filePattern = "Shapefile", tmpdir = tmpdir, test = test
    )

    # # # # # Comment
    # # # targetFile, alsoExtract
    # # # # # Comment
    file.remove(grep(
      dir(tmpdir, full.names = TRUE)[!isDirectory(dir(tmpdir))],
      pattern = "CHECKSUMS.txt",
      value = TRUE
    ))
    mess <- capture_messages({
      warns <- capture_warnings({
        test <- prepInputs(
          targetFile = "Shapefile1.shp",
          alsoExtract = c("Shapefile1.dbf", "Shapefile1.prj", "Shapefile1.shp", "Shapefile1.shx"),
          destinationPath = tmpdir
        )
      })
    })
    runTest("1_2", vectorType(), 5, mess,
            expectedMess = expectedMessage,
            filePattern = "Shapefile", tmpdir = tmpdir, test = test
    )
    mess <- capture_messages({
      warns <- capture_warnings({
        test <- prepInputs(
          targetFile = "Shapefile1.shp",
          alsoExtract = c("Shapefile1.dbf", "Shapefile1.prj", "Shapefile1.shp", "Shapefile1.shx"),
          destinationPath = tmpdir
        )
      })
    })
    runTest("1_2", vectorType(), 5, mess,
            expectedMess = expectedMessage,
            filePattern = "Shapefile", tmpdir = tmpdir, test = test
    )

    # # # # # Comment
    # # # alsoExtract -- previously failed b/c no information; now ok-- .guessAtTargetAndFun # # #
    # # # # # Comment
    file.remove(grep(
      dir(tmpdir, full.names = TRUE)[!isDirectory(dir(tmpdir))],
      pattern = "CHECKSUMS.txt",
      value = TRUE
    ))
    mess <- capture_messages({
      warns <- capture_warnings({
        test <- prepInputs(
          alsoExtract = c("Shapefile1.dbf", "Shapefile1.prj", "Shapefile1.shp", "Shapefile1.shx"),
          destinationPath = tmpdir
        )
      })
    })

    # # # # # Comment
    # # # archive, alsoExtract
    # # # # # Comment
    # archive exists locally
    # remove all non archive files
    file.remove(grep(
      dir(tmpdir, full.names = TRUE)[!isDirectory(dir(tmpdir))],
      pattern = "\\.zip",
      invert = TRUE,
      value = TRUE
    ))
    file.remove(grep(
      dir(tmpdir, full.names = TRUE)[!isDirectory(dir(tmpdir))],
      pattern = "CHECKSUMS.txt",
      value = TRUE
    ))
    mess <- capture_messages({
      warns <- capture_warnings({
        test <- prepInputs(
          archive = "Shapefiles.zip",
          alsoExtract = c("Shapefile1.dbf", "Shapefile1.prj", "Shapefile1.shp", "Shapefile1.shx"),
          destinationPath = tmpdir
        )
      })
    })
    runTest("1_4_9_10_13", vectorType(), 5, mess,
            expectedMess = expectedMessage,
            filePattern = "Shapefile", tmpdir = tmpdir, test = test
    )

    # 2nd time # can checksums
    mess <- capture_messages({
      warns <- capture_warnings({
        test <- prepInputs(
          archive = "Shapefiles.zip",
          alsoExtract = c("Shapefile1.dbf", "Shapefile1.prj", "Shapefile1.shp", "Shapefile1.shx"),
          destinationPath = tmpdir
        )
      })
    })
    runTest("1_9_10", vectorType(), 5, mess,
            expectedMess = expectedMessage,
            filePattern = "Shapefile", tmpdir = tmpdir, test = test
    )

    # Try without .shp -- fail
    file.remove(grep(
      dir(tmpdir, full.names = TRUE)[!isDirectory(dir(tmpdir))],
      pattern = "\\.zip",
      invert = TRUE,
      value = TRUE
    ))
    expect_error({
      mess <- capture_messages({
        warns <- capture_warnings({
          test <- prepInputs(
            archive = "Shapefiles1.zip",
            alsoExtract = c("Shapefile1.dbf", "Shapefile1.prj", "Shapefile1.shx"),
            destinationPath = tmpdir
          )
        })
      })
    })

    file.remove(grep(
      dir(tmpdir, full.names = TRUE)[!isDirectory(dir(tmpdir))],
      pattern = "\\.zip",
      invert = TRUE,
      value = TRUE
    ))
    file.remove(grep(
      dir(tmpdir, full.names = TRUE)[!isDirectory(dir(tmpdir))],
      pattern = "CHECKSUMS.txt",
      value = TRUE
    ))
    mess <- capture_messages({
      warns <- capture_warnings({
        test <- prepInputs(
          archive = "Shapefiles.zip",
          targetFile = "Shapefile1.shp",
          alsoExtract = "similar",
          destinationPath = tmpdir
        )
      })
    })
    runTest("1_2_4_9_13", vectorType(), 5, mess,
            expectedMess = expectedMessage,
            filePattern = "Shapefile", tmpdir = tmpdir, test = test
    )

    # 2nd time # can checksums
    mess <- capture_messages({
      warns <- capture_warnings({
        test <- prepInputs(
          archive = "Shapefiles.zip",
          targetFile = "Shapefile1.shp",
          alsoExtract = c("similar"),
          destinationPath = tmpdir
        )
      })
    })
    runTest("1_2_9", vectorType(), 5, mess,
            expectedMess = expectedMessage,
            filePattern = "Shapefile", tmpdir = tmpdir, test = test
    )
  })
})

test_that(".resolveDlFunCaptured handles all dlFun forms without eager eval", {
  cap <- function(dlFun) reproducible:::.resolveDlFunCaptured(substitute(dlFun), dlFun)

  # Symbol -> resolves to the function value
  myFn <- function() 42
  expect_true(is.function(cap(myFn)))

  # NULL literal -> NULL
  expect_null(cap(NULL))

  # quote(fn(...)) -> unwraps to inner call
  q <- cap(quote(myFn(x = 1)))
  expect_true(is.call(q))
  expect_identical(deparse(q), "myFn(x = 1)")

  # pkg::fn(args) -> kept as a deferred call; side effect must NOT fire
  ran <- FALSE
  fakePkg <- list(fakeFn = function(...) { ran <<- TRUE; "value" })
  attach(fakePkg, name = "fakePkg", warn.conflicts = FALSE)
  on.exit(detach("fakePkg"), add = TRUE)
  d <- cap(fakePkg::fakeFn(country = "LUX"))
  expect_false(ran)
  expect_true(is.call(d))

  # bare fn(args) -> kept as a deferred call; side effect must NOT fire
  ran2 <- FALSE
  side <- function() { ran2 <<- TRUE; 7 }
  d2 <- cap(side())
  expect_false(ran2)
  expect_true(is.call(d2))

  # Control-flow expression -> evaluated (e.g., if/else)
  useFn <- TRUE
  expect_true(is.function(cap(if (useFn) myFn else NULL)))
})

test_that("prepInputs(dlFun = ...) ignores pre-existing files in destinationPath subdirs", {
  # Regression test for a bug where downloadRemote()'s noTargetFile branch
  # took the "before dlFun" snapshot of destinationPath with
  # `dir(destinationPath, full.names = TRUE)` (non-recursive) and the "after
  # dlFun" snapshot with `dir(..., recursive = TRUE, full.names = TRUE)`.
  # The setdiff() of those mismatched listings classified files that were
  # already present in subdirectories of destinationPath as "newly created
  # by dlFun". They then propagated as `downloadResults$destFile` and tripped
  # a spurious "already exists at ..." stop later in the function.
  #
  # Real-world trigger: a user sets `reproducible.inputPaths` to a shared
  # data stash, then calls `prepInputs(url = some_archive.zip, ...)` whose
  # archive payload extracts into a subdirectory of the stash. A subsequent
  # `prepInputs(dlFun = some_function(...))` for an unrelated dataset would
  # fail with an error mentioning the previous archive's stashed files.
  testInit(
    opts = list(
      reproducible.interactiveOnDownloadFail = FALSE,
      reproducible.inputPaths = NULL,
      reproducible.overwrite = FALSE
    )
  )

  destPath <- file.path(tmpdir, "destSubdir")
  dir.create(destPath, recursive = TRUE)
  preExistingDir <- file.path(destPath, "preExisting")
  dir.create(preExistingDir)
  # Two files whose basenames collide between the subdir and top-level.
  # The collision is what makes the bug surface as an "already exists"
  # stop; without it, the buggy code still mis-identifies the subdir file
  # as new but the desiredPath check happens to pass.
  writeLines("subdir-content",   file.path(preExistingDir, "junk.txt"))
  writeLines("toplevel-content", file.path(destPath, "junk.txt"))

  # dlFun returns a small in-memory object; no network, no side effects on
  # destinationPath. The bug manifests purely from the snapshot logic.
  res <- expect_no_error(
    prepInputs(
      destinationPath = destPath,
      dlFun = function() data.frame(x = 1:3)
    )
  )

  # Sanity: prepInputs returned the dlFun's value, and the pre-existing
  # files are untouched.
  expect_s3_class(res, "data.frame")
  expect_identical(res$x, 1:3)
  expect_identical(readLines(file.path(preExistingDir, "junk.txt")), "subdir-content")
  expect_identical(readLines(file.path(destPath, "junk.txt")), "toplevel-content")
})

test_that("prepInputs when fun = NA", {
  skip_on_cran()

  ## Probe the GADM host with a short timeout. The test below calls
  ## geodata::gadm(), which talks to geodata.ucdavis.edu — when that endpoint
  ## is slow/unreachable (SSL hiccups, DNS, packet drops) geodata's internal
  ## retry loop can stall the test for minutes. Bail out cleanly instead.
  skip_if_not(.requireNamespace("httr2"), "httr2 not available for probe")
  gadmReachable <- tryCatch({
    httr2::request("https://geodata.ucdavis.edu/") |>
      httr2::req_method("HEAD") |>
      httr2::req_timeout(5) |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform()
    TRUE
  }, error = function(e) FALSE)
  skip_if_not(isTRUE(gadmReachable), "geodata.ucdavis.edu unreachable")

  testInit(
    c("sf", "terra"),
    opts = list(
      rasterTmpDir = tempdir2(rndstr(1, 6)),
      reproducible.interactiveOnDownloadFail = FALSE,
      reproducible.inputPaths = NULL,
      reproducible.overwrite = TRUE
    ),
    needGoogleDriveAuth = TRUE
  )

  globalNoisy <- capture.output({
    coords <- structure(c(6, 6.1, 6.2, 6.15, 6, 49.5, 49.7, 49.8, 49.6, 49.5), .Dim = c(5L, 2L))
    StudyArea <- terra::vect(coords, "polygons")
    terra::crs(StudyArea) <- crsToUse

    mess1 <- capture_messages({
      test1 <- try(silent = TRUE, {
        prepInputs(
          fun = NA,
          dlFun = getDataFn,
          name = "GADM",
          country = "LUX",
          level = 0,
          path = tmpdir,
          quiet = TRUE
        )
      })
    })
    if (!is(test1, "try-error") && !any(grepl("out of service", mess1))) {
      expect_true(is(test1, "SpatVector"))
      # test quoted version of `dlFun`
      mess3 <- capture_messages({
        test3 <- prepInputs(
          fun = NA,
          dlFun = quote(getDataFn(name = "GADM", country = "LUX", level = 0, path = tmpdir)),
          destinationPath = tmpdir
        )
      })
      expect_true(is(test3, "SpatVector"))

      if (.requireNamespace("sf")) {
        mess6 <- capture_messages({
          test6 <- prepInputs(
            # targetFile = targetFileLuxRDS,
            dlFun = quote({
              out <- getDataFn(name = "GADM", country = "LUX", level = 0, path = tmpdir)
              sf::st_as_sf(out)
            }),
            tmpdir = tmpdir
          )
        })

        expect_is(test6, "sf")
      }
    }
  })
})

test_that("load rdata in prepInputs", {
  testInit(
    "terra",
    tmpFileExt = "rda",
    opts = list(reproducible.inputPaths = NULL, reproducible.overwrite = TRUE),
    needGoogleDriveAuth = TRUE
  )
  a <- 1
  b <- 2
  save(a, b, file = tmpfile)
  aa <- prepInputs(tmpfile, fun = "base::load")
  expect_true(identical(aa, list(a = a, b = b)))

  d <- new.env(parent = emptyenv())
  aa <- prepInputs(tmpfile, fun = "base::load", envir = d)
  expect_false(identical(aa, list(a = a, b = b))) # not in aa, because loaded to d
  expect_true(identical(as.list(d), list(a = a, b = b)))
})

test_that("assessDataType doesn't work", {
  testInit(
    "terra",
    opts = list(reproducible.inputPaths = NULL, reproducible.overwrite = TRUE),
    needGoogleDriveAuth = TRUE
  )

  ## LOG1S
  ras <- terra::rast(ncols = 10, nrows = 10)
  ras[] <- c(0, NaN, rep(c(0, 1), 49))
  expect_true(assessDataType(ras) == "LOG1S")

  ras <- terra::rast(ncols = 10, nrows = 10)
  ras[] <- rep(c(0, 1), 50)
  expect_true(assessDataType(ras) == "LOG1S")

  ras[] <- rep(c(TRUE, FALSE), 50)
  expect_true(assessDataType(ras) == "LOG1S")

  ras[] <- c(NA, NA, rep(c(0, 1), 49))
  expect_true(assessDataType(ras) == "LOG1S")

  ## INT1S
  ras[] <- -1:98
  expect_true(assessDataType(ras) == "INT1S")

  ras[] <- c(NA, -1:97)
  expect_true(assessDataType(ras) == "INT1S")

  ## INT1U
  ras <- terra::rast(ncols = 10, nrows = 10)
  ras[] <- 1:100
  expect_true(assessDataType(ras) == "INT1U")

  ras[] <- c(NA, 2:100)
  expect_true(assessDataType(ras) == "INT1U")

  ## INT2U
  ras <- terra::rast(ncols = 10, nrows = 10)
  ras[] <- round(runif(100, min = 64000, max = 65000))
  expect_true(assessDataType(ras) == "INT2U")

  ## INT2S
  ras <- terra::rast(ncols = 10, nrows = 10)
  ras[] <- round(runif(100, min = -32767, max = 32767))
  expect_true(assessDataType(ras) == "INT2S")

  ras[54] <- NA
  expect_true(assessDataType(ras) == "INT2S")

  ## INT4U
  ras <- terra::rast(ncols = 10, nrows = 10)
  ras[] <- round(runif(100, min = 0, max = 500000000))
  expect_true(assessDataType(ras) == "INT4U")

  ras[14] <- NA
  expect_true(assessDataType(ras) == "INT4U")

  ## INT4S
  ras <- terra::rast(ncols = 10, nrows = 10)
  ras[] <- round(runif(100, min = -200000000, max = 200000000))
  expect_true(assessDataType(ras) == "INT4S")

  ras[14] <- NA
  expect_true(assessDataType(ras) == "INT4S")

  ## FLT4S
  ras <- terra::rast(ncols = 10, nrows = 10)
  ras[] <- runif(100, min = -10, max = 87)
  expect_true(assessDataType(ras) == "FLT4S")

  ras <- terra::rast(ncols = 10, nrows = 10)
  ras[] <- round(runif(100, min = -3.4e+26, max = 3.4e+28))
  expect_true(assessDataType(ras) == "FLT4S")

  ras <- terra::rast(ncols = 10, nrows = 10)
  ras[] <- round(runif(100, min = 3.4e+26, max = 3.4e+28))
  expect_true(assessDataType(ras) == "FLT4S")

  ras <- terra::rast(ncols = 10, nrows = 10)
  ras[] <- round(runif(100, min = -3.4e+26, max = -1))
  expect_true(assessDataType(ras) == "FLT4S")

  ## FLT8S
  ras <- terra::rast(ncols = 10, nrows = 10)
  ras[] <- round(runif(100, min = -1.7e+30, max = 1.7e+308))
  expect_true(assessDataType(ras) == "FLT8S")

  ras <- terra::rast(ncols = 10, nrows = 10)
  ras[] <- round(runif(100, min = 1.7e+30, max = 1.7e+308))
  expect_true(assessDataType(ras) == "FLT8S")

  ras <- terra::rast(ncols = 10, nrows = 10)
  ras[] <- round(runif(100, min = -1.7e+308, max = -1))
  expect_true(assessDataType(ras) == "FLT8S")

  ras <- terra::rast(ncols = 10, nrows = 10)
  ras[] <- c(-Inf, 1, rep(c(0, 1), 49))
  expect_true(assessDataType(ras) == "FLT8S")

  ras <- terra::rast(ncols = 10, nrows = 10)
  ras[] <- c(Inf, 1, rep(c(0, 1), 49))
  expect_true(assessDataType(ras) == "FLT8S")
})

test_that("assessDataType for categorical rasters", {
  testInit(c("terra", "raster"))

  r <- terra::rast(terra::ext(c(0, 2, 0, 2)), vals = 1:4, resolution = 1)
  levels(r) <- data.frame(ID = 1:4, Lett = LETTERS[1:4])
  expect_identical(assessDataType(r), "INT1U")

  r <- raster::raster(raster::extent(c(0, 2, 0, 2)), vals = 1:4, resolution = 1)
  levels(r) <- data.frame(ID = 1:4, Lett = LETTERS[1:4])
  expect_identical(assessDataType(r), "INT1U")
})

test_that("lightweight tests for code coverage", {
  skip_on_cran()

  testInit(
    c("sf", "terra"),
    opts = list(reproducible.inputPaths = NULL, reproducible.overwrite = TRUE),
    needGoogleDriveAuth = TRUE
  )

  url <- "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip"
  globalOutput <- capture.output({
    checkPath(tmpdir, create = TRUE)
    checkSums <- .emptyChecksumsResult
    checkSumFilePath <- file.path(tmpdir, "CHECKSUMS.txt")

    downloadFile(
      url = url,
      neededFiles = "ecozones.shp",
      checkSums = checkSums,
      archive = "ecozone_shp.zip",
      needChecksums = TRUE,
      quick = FALSE,
      destinationPath = tmpdir,
      checksumFile = checkSumFilePath
    )
    expect_true(file.exists(dir(tmpdir, pattern = "ecozone", full.names = TRUE)))

    # have local copy
    unzip("ecozone_shp.zip", exdir = tmpdir)
    expect_true(all(file.copy(dir(file.path(tmpdir, "Ecozones"), full.names = TRUE), tmpdir)))
    checkSums <- Checksums(path = tmpdir, write = TRUE)

    aMess <- capture_messages(downloadFile(
      url = url,
      neededFiles = "ecozones.shp",
      checkSums = checkSums,
      targetFile = "ecozones.shp",
      archive = NULL,
      needChecksums = TRUE,
      quick = FALSE,
      destinationPath = file.path(tmpdir, "Ecozones"),
      checksumFile = file.path(tmpdir, "CHECKSUMS.txt")
    ))

    if (!isMac()) {
      expect_true(any(grepl("Skipping download", aMess))) ## 2023-05-08: fails on macOS
    }

    filesForShp <- dir(file.path(tmpdir), pattern = "ecozones", full.names = TRUE)
    expect_true(all(file.copy(filesForShp, tmpCache)))
    # Need these in a test further down -- mostly just need the CRS
    filesForShp2 <- dir(file.path(tmpCache), pattern = "ecozones", full.names = TRUE)
    if (.requireNamespace("sf")) {
      shpFile <- sf::st_read(grep(filesForShp2, pattern = "\\.shp", value = TRUE))
    }
    # Test when wrong archive exists, wrong checkSums
    expect_true(file.remove(file.path(tmpdir, "ecozone_shp.zip")))
    expect_true(all(file.remove(filesForShp)))
    expect_true(file.create(file.path(tmpdir, "ecozone_shp.zip")))
    checkSums <- Checksums(path = tmpdir, write = TRUE)
    expect_true(file.remove(file.path(tmpdir, "ecozone_shp.zip")))
    checkSums <- Checksums(path = tmpdir)

    out <- try(
      silent = TRUE,
      downloadFile(
        url = url,
        ## fmt: skip
        neededFiles = c(
          "ecozones.dbf", "ecozones.prj", "ecozones.sbn",
          "ecozones.sbx", "ecozones.shp", "ecozones.shx"
        ),
        checkSums = checkSums,
        targetFile = "ecozones.shp",
        archive = "ecozone_shp.zip",
        needChecksums = TRUE,
        quick = FALSE,
        destinationPath = tmpdir,
        checksumFile = checkSumFilePath
      )
    )

    ## try to purge from the CHECKSUMS.txt
    toPurgeCode <- grep(
      "purgeChecksums|fileToRemove",
      capture.output(attr(out, "condition")),
      value = TRUE
    )
    toPurgeCode <- parse(text = gsub(">", "", toPurgeCode))
    checksumsFile <- dir(tmpdir, pattern = "CHECKSUMS.txt", full.names = TRUE)
    dtBefore <- data.table::fread(checksumsFile)
    eval(toPurgeCode)
    dtAfter <- data.table::fread(checksumsFile)
    expect_equivalent(NROW(dtBefore[!dtAfter, on = "file"]), 1L)

    ## 2023-05-08: does not error on macOS
    isErr <- is(out, "try-error")
    # if (isMac()) expect_false(isErr) else
    expect_true(isErr)

    ## postProcess.default
    b <- 1
    expect_no_error(postProcess(b))

    ## postProcess.list
    b <- list(1, 1)
    expect_no_error(postProcess(b))

    ras <- terra::rast(terra::ext(0, 10, 0, 10), resolution = 1, vals = 1:100)
    terra::crs(ras) <- crsToUse

    expect_error(postProcess(ras, studyArea = 1), .message$Greps$anySpatialClass)
    expect_error(postProcess(ras, rasterToMatch = 1), .message$Greps$anySpatialClass)

    ## cropInputs.default
    b <- 1
    a <- cropInputs(b)
    expect_true(identical(a, b))

    ras2 <- terra::rast(terra::ext(0, 5, 0, 5), resolution = 1, vals = 1:25)
    terra::crs(ras2) <- crsToUse
    a <- cropInputs(ras, extentToMatch = terra::ext(ras2), extentCRS = terra::crs(ras2))
    expect_true(.isSpatRaster(a))

    ras4 <- terra::rast(terra::ext(7, 11, 7, 11), resolution = 1, vals = 1:16)
    sp4 <- terra::vect(terra::ext(ras4))
    terra::crs(sp4) <- crsToUse
    # sp4 <- sf::st_as_sfc(sf::st_bbox(ras4))
    # sf::st_crs(sp4) <- crsToUse

    grepMessHere <- "extents do not overlap"
    expect_error(cropInputs(ras2, studyArea = sp4), grepMessHere)

    ras3 <- terra::rast(terra::ext(0, 5, 0, 5), resolution = 1, vals = 1:25)
    terra::crs(ras3) <- crsToUse

    ## Different crs
    ## Because studyArea is a Raster, then it doesn't work correctly
    a <- cropInputs(ras2, studyArea = ras3)
    expect_true(.isSpatRaster(a))
    expect_true(identical(terra::crs(a), terra::crs(ras2)))

    # Now rasterToMatch used -- internally reprojects it to x
    a <- cropInputs(ras2, rasterToMatch = ras3)
    expect_true(.isSpatRaster(a))
    expect_true(identical(terra::crs(a), terra::crs(ras2)))

    ## fixErrors.default
    b <- 1
    a <- fixErrors(b)
    expect_true(identical(a, b))

    ## projectInputs.Raster
    a <- projectInputs(ras2, rasterToMatch = ras3, method = "near")
    expect_true(.isSpatRaster(a))
    expect_true(identical(terra::crs(a), terra::crs(ras3)))

    a <- projectInputs(ras2, targetCRS = terra::crs(ras3), rasterToMatch = ras3, method = "near")
    expect_true(.isSpatRaster(a))
    expect_true(identical(terra::crs(a), terra::crs(ras3)))

    ## warns if bilinear is passed for reprojecting integer
    if (.requireNamespace("sf")) {
      expect_warning(projectInputs(ras2, targetCRS = terra::crs(shpFile), method = "bilinear"))
    }

    ## Works with no rasterToMatch
    a <- projectInputs(ras2, targetCRS = crs(ras3), method = "near")
    expect_true(identical(crs(a), crs(ras3)))

    # }
  })
  # sp::CRS("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"))
})

test_that("lightweight tests 2 for code coverage", {
  skip_on_cran()

  testInit(
    "terra",
    opts = list(reproducible.inputPaths = NULL, reproducible.overwrite = TRUE),
    needGoogleDriveAuth = TRUE
  )

  theZipFile <- tempfile(tmpdir = tmpdir, fileext = ".zip")
  theZipFile2 <- tempfile(tmpdir = tmpdir, fileext = ".zip")
  theZipFile3 <- tempfile(tmpdir = tmpdir, fileext = ".zip")
  theZipName <- file.path(tmpdir, "hi.zip")
  theZapFile <- tempfile(tmpdir = tmpdir, fileext = ".zap")
  theRDSFile <- tempfile(tmpdir = tmpdir, fileext = ".rds")
  a <- 1
  saveRDS(a, file = theRDSFile)

  withr::with_dir(dirname(theRDSFile), {
    utils::zip(zipfile = theZipFile, files = basename(theRDSFile), flags = "-q")
    utils::zip(zipfile = theZipFile2, files = basename(theZipFile), flags = "-q")
    utils::zip(zipfile = theZipFile3, files = basename(theZipFile2), flags = "-q")
  })

  expect_error(extractFromArchive(theZapFile), "Archives of type zap are not currently supported")
  oo <- capture.output(
    type = "message",
    expect_error(extractFromArchive(theZipName), "No archive exists with filename")
  )

  extractFromArchive(theZipFile, neededFiles = character())

  csfp <- file.path(tmpdir, "CHECKSUMS.txt")
  data.table::fwrite(.emptyChecksumsFileContent, file = csfp, sep = "\t")

  # check Checksums fn
  a <- extractFromArchive(
    theZipFile,
    neededFiles = character(),
    checkSumFilePath = csfp,
    destinationPath = tmpdir
  )
  expect_true(file.exists(a$filesExtracted))
  # check Checksums fn

  expect_error(
    oo <- capture.output(type = "message",
    suppressWarnings(extractFromArchive(
      theZipFile,
      neededFiles = character(),
      checkSumFilePath = theRDSFile,
      destinationPath = tmpdir
    )),
    "checkSumFilePath is not a CHECKSUMS.txt"
  ))

  # Doubley nested zips -- extract inner, inner
  a <- extractFromArchive(
    c(theZipFile2, theZipFile),
    neededFiles = character(),
    checkSumFilePath = csfp,
    destinationPath = tmpdir
  )
  expect_true(isTRUE(all(file.exists(a$filesExtracted))))

  # triply
  a <- extractFromArchive(
    theZipFile3,
    neededFiles = theRDSFile,
    checkSumFilePath = csfp,
    destinationPath = tmpdir,
    .tempPath = tempdir2()
  )
  expect_true(length(a$extractedArchives) == 3)
  expect_true(length(a$filesExtracted) == 3)
  expect_true(all(basename(a$filesExtracted) %in% basename(c(theZipFile, theZipFile2, theRDSFile))))
  expect_true(all(
    basename(a$extractedArchives) %in% basename(c(theZipFile, theZipFile2, theZipFile3))
  ))

  allZipsAndRDS <- c(theZipFile, theZipFile2, theZipFile3, theRDSFile)
  Checksums(tmpdir, write = TRUE, files = allZipsAndRDS, overwrite = TRUE)
  a <- extractFromArchive(
    theZipFile3,
    neededFiles = theRDSFile,
    checkSumFilePath = csfp,
    destinationPath = tmpdir,
    checkSums = Checksums(tmpdir, files = allZipsAndRDS)
  )
})

test_that("options inputPaths", {
  skip_on_cran()
  skip_if_not_installed("geodata")

  testInit(
    c("terra", "geodata"),
    opts = list(reproducible.inputPaths = NULL, reproducible.inputPathsRecursive = FALSE),
    needInternet = TRUE
  )
  # SSL_REVOKE_BEST_EFFORT() # uses withr::defer to remove it after this test
  f <- formals3(prepInputs)
  getDataFn <- getDataFn # not exported from reproducible; can access here, not in the dlFun

  withr::local_options("reproducible.inputPaths" = NULL)
  withr::local_options("reproducible.inputPathsRecursive" = FALSE)

  mess1 <- capture_messages({
    test0 <- try(getDataFn(path = tmpdir, country = "LUX", quiet = TRUE), silent = TRUE)
  })

  useGADM <- !is(test0, "try-error") && any(grepl("server seems|server is", mess1)) %in% FALSE # NROW(dir(tmpdir, recursive = TRUE)) > 0

  if (useGADM) {
    # noisyOutput <- capture.output(type = "message", {
    mess1 <- capture_messages({
      test1 <- try(prepInputs(
        destinationPath = tmpdir,
        # url = if (!useGADM) url2 else f$url,
        # targetFile = if (useGADM) theFile else f$targetFile,
        dlFun = getDataFn,
        name = "GADM",
        country = "LUX",
        level = 0,
        path = tmpdir,
        quiet = TRUE
      ))
    })
    # })
  }

  theFile <- if (useGADM) {
    targetFileLuxRDS
  } else {
    "rasterTest.tif"
  }
  url2 <- "https://github.com/tati-micheletti/host/raw/master/data/rasterTest.tif"

  mess1 <- capture_messages({
    test1 <- try(prepInputs(
      destinationPath = tmpdir,
      url = if (!useGADM) url2 else f$url,
      targetFile = if (useGADM) theFile else f$targetFile,
      dlFun = if (useGADM) getDataFn else NULL,
      name = if (useGADM) "GADM" else NULL,
      country = if (useGADM) "LUX" else NULL,
      level = if (useGADM) 0 else NULL,
      path = if (useGADM) tmpdir else NULL
    ))
  })

  # Use inputPaths -- should do a link to tmpCache (the destinationPath)
  withr::local_options("reproducible.inputPaths" = tmpdir)
  withr::local_options("reproducible.inputPathsRecursive" = FALSE)
  dlFun1 <- if (useGADM) getDataFn else NULL

  mess1 <- capture_messages({
    test1 <- prepInputs(
      url = if (!useGADM) url2 else f$url,
      targetFile = if (useGADM) theFile else f$targetFile,
      dlFun = dlFun1,
      name = if (useGADM) "GADM" else NULL,
      country = if (useGADM) "LUX" else NULL,
      level = if (useGADM) 0 else NULL,
      path = if (useGADM) tmpdir else NULL,
      destinationPath = tmpCache,
      getDataFn = dlFun1,
      verbose = 2
    )
  })
  expect_true(sum(grepl(paste0("Hardlinked", ".*:"), mess1)) == 1)

  ## Now two folders - file not in destinationPath, not in 1st inputPaths, but yes 2nd
  ##   should hardlink from 2nd IP to destinationPath, make sure CHECKSUMS.txt is correct in both
  withr::local_options("reproducible.inputPaths" = c(tmpdir, tmpCache))
  file.remove(file.path(tmpdir, theFile))
  tmpdir3 <- file.path(tmpCache, "test")
  mess1 <- capture_messages({
    test1 <- prepInputs(
      url = if (!useGADM) url2 else f$url,
      targetFile = if (useGADM) theFile else f$targetFile,
      dlFun = if (useGADM) getDataFn else NULL,
      name = if (useGADM) "GADM" else NULL,
      country = if (useGADM) "LUX" else NULL,
      level = if (useGADM) 0 else NULL,
      path = if (useGADM) tmpdir else NULL,
      destinationPath = tmpdir3,
      verbose = 2
    )
  })
  expect_true(sum(grepl(paste0(hardlinkOrSymlinkMessagePrefixForGrep), mess1)) == 1)
  ## because the targetFile could be absent or present, this may create a spurious message
  ##  that we don't need to test for
  mess1b <- grep("targetFile was not supplied", mess1, invert = TRUE, value = TRUE)
  expect_true(sum(grepl(paste0(tmpdir3), mess1b)) == 2)

  ## THIS NEXT ONE DOESN"T PASS ON GitHub Actions on WINDOWS, skip it;
  ##  should copy from 2nd directory (tmpCache) because it is removed in the lower
  ##  tmpdir directory & has a CHECKSUMS.txt

  skip_on_ci()
  url_2 <- if (!useGADM) url2 else f$url
  targetFile_2 <- if (useGADM) theFile else f$targetFile
  dlFun_2 <- if (useGADM) getDataFn else NULL
  name_2 <- if (useGADM) "GADM" else NULL
  country_2 <- if (useGADM) "LUX" else NULL
  level_2 <- if (useGADM) 0 else NULL
  path_2 <- if (useGADM) tmpdir else NULL

  withr::local_options("reproducible.inputPaths" = tmpdir)
  withr::local_options("reproducible.inputPathsRecursive" = TRUE)
  file.remove(file.path(tmpCache, theFile))
  tmpdir1 <- file.path(tmpCache, "test1")
  warns <- capture_warnings(
    mess1 <- capture_messages({
      test1 <- prepInputs(
        url = url_2,
        targetFile = targetFile_2,
        dlFun = dlFun_2,
        name = name_2,
        country = country_2,
        level = level_2,
        path = path_2,
        destinationPath = tmpdir1,
        verbose = 3
      )
    })
  )

  mess1 <- gsub("\n    ", " ", mess1) ## remove misc new lines
  expect_true(sum(grepl(paste0(hardlinkOrSymlinkMessagePrefixForGrep), mess1)) == 1)
  expect_true(sum(grepl(whPointsToMessForGrep, mess1)) == 1)
  expect_true(sum(grepl(paste0(file.path(tmpdir1, theFile), ".+which point.+"), mess1)) == 1)
  expect_true(sum(basename(dir(file.path(tmpdir), recursive = TRUE)) %in% theFile) == 3)

  ## Try download to inputPath, intercepting the destination, creating a link
  testInit(
    "terra",
    opts = list(reproducible.inputPaths = NULL, reproducible.inputPathsRecursive = FALSE)
  )

  withr::local_options("reproducible.inputPaths" = tmpdir)
  tmpdir2 <- file.path(tmpdir, rndstr(1, 5))
  url_2 = if (!useGADM) url2 else f$url
  targetFile_2 = if (useGADM) theFile else f$targetFile
  dlFun_2 = if (useGADM) getDataFn else NULL
  name_2 = if (useGADM) "GADM" else NULL
  country_2 = if (useGADM) "LUX" else NULL
  level_2 = if (useGADM) 0 else NULL
  path_2 = if (useGADM) tmpdir else NULL

  # noisyOutput <- capture.output(type = "message", {
  mess1 <- capture_messages({
    test1 <- prepInputs(
      url = url_2,
      targetFile = targetFile_2,
      dlFun = dlFun_2,
      name = name_2,
      country = country_2,
      level = level_2,
      path = path_2,
      destinationPath = tmpdir2,
      quiet = TRUE
    )
  })
  # })

  ## Must remove the link that happens during downloading to a .tempPath
  test10 <- grep(hardlinkOrSymlinkMessagePrefixForGrep, mess1, value = TRUE)
  test10 <- grep(tmpdir2, test10, invert = TRUE, value = TRUE)
  expect_true(length(test10) == (1)) #

  ## Have file in inputPath, not in destinationPath
  unlink(file.path(tmpdir2, theFile))
  expect_false(file.exists(file.path(tmpdir2, theFile))) # FALSE -- confirm previous line
  expect_true(file.exists(file.path(tmpdir, theFile))) # TRUE b/c is in getOption('reproducible.inputPaths')
  tmpdir2 <- file.path(tmpdir, rndstr(1, 5))
  url_2 = if (!useGADM) url2 else f$url
  targetFile_2 = if (useGADM) theFile else f$targetFile
  dlFun_2 = if (useGADM) getDataFn else NULL
  name_2 = if (useGADM) "GADM" else NULL
  country_2 = if (useGADM) "LUX" else NULL
  level_2 = if (useGADM) 0 else NULL
  path_2 = if (useGADM) tmpdir else NULL

  mess1 <- capture_messages({
    test1 <- prepInputs(
      url = url_2,
      targetFile = targetFile_2,
      dlFun = dlFun_2,
      name = name_2,
      country = country_2,
      level = level_2,
      path = path_2,
      destinationPath = tmpdir2,
      verbose = 3
    )
  })
  expect_true(sum(grepl(hardlinkOrSymlinkMessagePrefixForGrep, mess1)) == 1) # used a linked version
  expect_true(sum(grepl(paste0("Hardlinked.*"), mess1)) == 1) # it is now in tmpdir2, i.e., the destinationPath
  expect_true(sum(grepl(paste0(basename(tmpdir2)), mess1)) %in% 2:3) # it is now in tmpdir2, i.e., the destinationPath

  ## Have file in destinationPath, not in inputPath
  unlink(file.path(tmpdir, theFile))
  expect_false(file.exists(file.path(tmpdir, theFile))) # FALSE -- confirm previous line
  expect_true(file.exists(file.path(tmpdir2, theFile))) # TRUE b/c is in getOption('reproducible.inputPaths')
  url_2 = if (!useGADM) url2 else f$url
  targetFile_2 = if (useGADM) theFile else f$targetFile
  dlFun_2 = if (useGADM) getDataFn else NULL
  name_2 = if (useGADM) "GADM" else NULL
  country_2 = if (useGADM) "LUX" else NULL
  level_2 = if (useGADM) 0 else NULL
  path_2 = if (useGADM) tmpdir else NULL
  mess1 <- capture_messages({
    test1 <- prepInputs(
      url = url_2,
      targetFile = targetFile_2,
      dlFun = dlFun_2,
      name = name_2,
      country = country_2,
      level = level_2,
      path = path_2,
      overwrite = TRUE,
      destinationPath = tmpdir2,
      verbose = 2
    )
  })
  # expect_true(sum(grepl(hardlinkOrSymlinkMessagePrefixForGrep, mess1)) == 1) # used a linked version
  # expect_true(sum(grepl(paste0("Hardlinked.*",basename(tmpdir2)), mess1)) == 1) # it is now in tmpdir2, i.e., the destinationPath

  ## Try with inputPaths == destinationPath
  unlink(file.path(tmpdir, theFile))
  unlink(file.path(tmpdir2, theFile))
  expect_false(file.exists(file.path(tmpdir, theFile))) # FALSE -- confirm previous line
  expect_false(file.exists(file.path(tmpdir2, theFile))) # TRUE b/c is in getOption('reproducible.inputPaths')
  withr::local_options("reproducible.inputPaths" = tmpdir)
  url_2 = if (!useGADM) url2 else f$url
  targetFile_2 = if (useGADM) theFile else f$targetFile
  dlFun_2 = if (useGADM) getDataFn else NULL
  name_2 = if (useGADM) "GADM" else NULL
  country_2 = if (useGADM) "LUX" else NULL
  level_2 = if (useGADM) 0 else NULL
  path_2 = if (useGADM) tmpdir else NULL
  # noisyOutput <- capture.output(type = "message", {
  mess1 <- capture_messages({
    test1 <- prepInputs(
      url = url_2,
      targetFile = targetFile_2,
      dlFun = dlFun_2,
      name = name_2,
      country = country_2,
      level = level_2,
      path = path_2,
      destinationPath = tmpdir,
      verbose = 2
    )
  })
  # })
  objType <- if (useGADM) vectorType() else rasterType()
  expect_true(is(test1, objType) || is(test1, "SpatVector"))
  test11 <- grep(hardlinkOrSymlinkMessagePrefixForGrep, mess1, value = TRUE)
  test11 <- grep(tmpdir, test11, invert = TRUE)
  expect_true(length(test11) == 0) # no link made b/c identical dir
  expect_true(sum(grepl(paste0("Hardlinked.*", basename(tmpdir2)), mess1)) == 0) # no link made b/c identical dir
})

test_that("writeOutputs saves factor rasters with .grd class to preserve levels", {
  skip_on_cran()

  testInit(
    "terra",
    opts = list(reproducible.inputPaths = NULL, reproducible.overwrite = TRUE),
    needGoogleDriveAuth = TRUE
  )
  a <- terra::rast(terra::ext(0, 2, 0, 2), resolution = 1, vals = c(1, 1, 2, 2))
  levels(a) <- data.frame(ID = 1:2, Factor = c("This", "That"))
  tifTmp <- tempfile(tmpdir = tmpdir, fileext = ".tif")
  file.create(tifTmp)
  tifTmp <- normPath(tifTmp)

  b1 <- suppressWarnings(terra::writeRaster(a, filename = tifTmp, overwrite = TRUE)) # the GDAL>6 issue
  b1a <- writeOutputs(a, writeTo = tifTmp)
  expect_equivalent(b1, b1a)
  expect_equivalent(b1[], b1a[])

  expect_true(identical(normPath(Filenames(b1)), normPath(tifTmp)))
})

test_that("rasters aren't properly resampled", {
  skip_on_cran()

  testInit(
    "terra",
    opts = list(reproducible.inputPaths = NULL, reproducible.overwrite = TRUE),
    needGoogleDriveAuth = TRUE
  )
  a <- terra::rast(terra::ext(0, 20, 0, 20), resolution = 2, vals = as.integer(1:100 * 4))
  b <- terra::rast(terra::ext(0, 30, 0, 30), resolution = c(3, 3), vals = 1L:100L)
  crs(a) <- crsToUse
  crs(b) <- crsToUse

  tiftemp1 <- normPath(tempfile(tmpdir = tmpdir, fileext = ".tif"))
  tiftemp2 <- normPath(tempfile(tmpdir = tmpdir, fileext = ".tif"))

  suppressWarnings({
    a <- terra::writeRaster(a, filename = tiftemp1, datatype = "INT2U")
    b <- terra::writeRaster(b, filename = tiftemp2, datatype = "INT2U")
  }) ## TODO: temporary GDAL>6

  # Test bilinear --> but keeps integer if it is integer
  suppressWarnings({
    out2 <- prepInputs(
      targetFile = tiftemp1,
      rasterToMatch = terra::rast(tiftemp2),
      destinationPath = dirname(tiftemp1),
      method = "bilinear",
      datatype = "INT2S",
      writeTo = tempfile(tmpdir = tmpdir, fileext = ".tif")
    )
  }) # about "raster layer has integer values"

  if (!isWindows()) {
    expect_true(dataType2(out2) %in% c("INT2S")) # because of "bilinear", it can become negative

    rrr1 <- terra::rast(terra::ext(0, 20, 0, 20), resolution = 1, vals = runif(400, 0, 1))
    terra::crs(rrr1) <- crsToUse
    tiftemp3 <- tempfile(tmpdir = tmpdir, fileext = ".tif")
    tiftemp4 <- tempfile(tmpdir = tmpdir, fileext = ".tif")
    suppressWarningsSpecific(terra::writeRaster(rrr1, filename = tiftemp3), proj6Warn)

    out3 <- prepInputs(
      targetFile = tiftemp3,
      rasterToMatch = terra::rast(tiftemp2),
      destinationPath = dirname(tiftemp3),
      writeTo = tempfile(tmpdir = tmpdir, fileext = ".tif")
    )
    expect_true(dataType2(out3) == "FLT4S")

    # Test for raster::stack
    rasStack <- c(terra::rast(tiftemp3), terra::rast(tiftemp3))
    rasStack[] <- rasStack[]
    rasStack[131][1] <- 1.5
    tiftemp4 <- tempfile(tmpdir = tmpdir, fileext = ".tif")

    rasStack <- terra::writeRaster(rasStack, filename = tiftemp4)
    rm(rasStack)
    warns <- capture_warnings(
      # rasters aren't properly resampled ─────
      # partial argument match of 'ncol' to 'ncols'
      out3 <- prepInputs(
        targetFile = tiftemp4,
        rasterToMatch = terra::rast(tiftemp2),
        destinationPath = dirname(tiftemp3),
        writeTo = tempfile(tmpdir = tmpdir, fileext = ".tif")
      )
    )
    expect_true(is(out3, rasterType()))
    expect_true(identical(length(Filenames(out3)), 1L))

    if (.requireNamespace("raster")) {
      rasterStackFn <- "raster::stack"
      suppressWarningsSpecific(falseWarnings = "partial argument match", {
        out4 <- prepInputs(
          targetFile = tiftemp4,
          rasterToMatch = terra::rast(tiftemp2),
          destinationPath = dirname(tiftemp3),
          fun = rasterStackFn,
          writeTo = c(
            tempfile(tmpdir = tmpdir, fileext = ".grd"),
            tempfile(tmpdir = tmpdir, fileext = ".grd")
          )
        )
      })
      expect_true(is(out4, rasterType(nlayers = nlayers2(out4), rasterRead = rasterStackFn)))
      expect_true(identical(length(Filenames(out4, allowMultiple = TRUE)), 4L))

      # Test for raster::stack with 3 layers, different types of writeRaster file ext
      rasStack <- c(terra::rast(tiftemp3), terra::rast(tiftemp3), terra::rast(tiftemp3))
      rasStack[] <- rasStack[]
      rasStack[131][1] <- 1.5
      rasStack[131][2] <- 2.5
      tiftemp5 <- tempfile(tmpdir = tmpdir, fileext = ".tif")

      rasStack <- writeRaster(rasStack, filename = tiftemp5)
      rm(rasStack)
      suppressWarningsSpecific(falseWarnings = "partial argument match", {
        out5 <- prepInputs(
          targetFile = tiftemp5,
          rasterToMatch = terra::rast(tiftemp2),
          destinationPath = dirname(tiftemp3),
          fun = rasterStackFn,
          writeTo = c(
            tempfile(tmpdir = tmpdir, fileext = ".grd"),
            tempfile(tmpdir = tmpdir, fileext = ".grd"),
            tempfile(tmpdir = tmpdir, fileext = ".tif")
          )
        )
      })
      expect_true(is(out5, "RasterStack"))
      expect_true(identical(length(Filenames(out5, allowMultiple = TRUE)), 5L))

      suppressWarningsSpecific(falseWarnings = "partial argument match", {
        out4 <- prepInputs(
          targetFile = tiftemp4,
          rasterToMatch = terra::rast(tiftemp2),
          destinationPath = dirname(tiftemp3),
          fun = rasterStackFn,
          writeTo = c(
            tempfile(tmpdir = tmpdir, fileext = ".grd"),
            tempfile(tmpdir = tmpdir, fileext = ".grd")
          )
        )
      })
      expect_true(is(out4, rasterType(nlayers2(out4), rasterStackFn)))
      expect_true(identical(length(Filenames(out4)), 4L))
    }
  }
})

test_that("test prepInputs url when a directory", {
  skip_on_cran()
  skip_if_not_installed("httr")
  skip_if_not_installed("curl")

  testInit("terra", opts = list(reproducible.inputPaths = NULL, reproducible.overwrite = TRUE))
  withr::local_options(destinationPath = tmpdir)

  globalOutput <- capture.output({
    url <- "http://forestales.ujed.mx/incendios2/cartografia/tematicos/combustibles_y_vegetacion/tipo_combustibles_serie_VI/"

    if (!urlExists(url)) {
      skip("Mexico url doesn't exist; skipping")
    }
    # Nothing specified
    a <- prepInputs(url = url, fun = "terra::rast")
    expect_is(a, "SpatRaster")
    files <- dir(getwd(), pattern = "comb")
    expect_true(length(files) == 8)

    unlink(dir(getwd(), recursive = TRUE, full.names = TRUE))
    a <- prepInputs(url = url, targetFile = "comb_290719.tif", fun = "terra::rast")
    expect_is(a, "SpatRaster")
    files <- dir(getwd(), pattern = "comb")
    expect_true(length(files) == 8)

    unlink(dir(getwd(), recursive = TRUE, full.names = TRUE))
    a <- prepInputs(
      url = url,
      targetFile = "comb_290719.tif",
      alsoExtract = FALSE,
      fun = "terra::rast"
    )
    expect_is(a, "SpatRaster")
    files <- dir(getwd(), pattern = "comb")
    expect_true(length(files) == 1)

    unlink(dir(getwd(), recursive = TRUE, full.names = TRUE))
    a <- prepInputs(url = url, fun = "terra::rast")
    expect_is(a, "SpatRaster")
    files <- dir(getwd(), pattern = "comb_290719")
    expect_true(length(files) == 7)
  })
})

test_that("test prepInputs url when a gdrive directory", {
  skip_on_cran()

  testInit(
    c("terra", "googledrive"),
    opts = list(reproducible.inputPaths = NULL, reproducible.overwrite = TRUE)
  )
  withr::local_options(destinationPath = tmpdir)
  skip_if_no_token()

  globalOutput <- capture.output({
    withr::local_dir(tmpdir)
    dPath <- "."
    url <- "https://drive.google.com/drive/u/3/folders/1q3aosWJ_THpgEaDzchvCWLMwT91pD9Fs"
    a <- prepInputs(
      url = url,
      fun = quote({
        tfp <- sort(targetFilePath)
        b <- terra::rast(tfp)
        names(b) <- basename(tfp)
        b
      }),
      destinationPath = dPath
    ) |>
      Cache()

    expect_is(a, "SpatRaster")
    expect_true(terra::nlyr(a) > 1)
  })
})

test_that("test prepInputs with zip file with hidden files", {
  testInit()
  skip_on_os("mac")
  withr::local_dir(tmpdir)
  a <- 1
  theFile <- "__MACOSX/._theFile.txt"
  checkPath(dirname(theFile), create = TRUE)
  cat(a, file = theFile)
  zipFilename <- "test.zip"
  zip(files = theFile, zipfile = zipFilename, flags = "-q")
  unlink(theFile, recursive = TRUE)
  expect_false(file.exists(file = theFile))
  b <- prepInputs(targetFile = theFile, archive = zipFilename, fun = NA)
  expect_true(file.exists(file = theFile))
})

test_that(".guessAtTargetAndFun ignores OS archive metadata when auto-picking", {
  testInit()
  # When no targetFile is specified, OS-injected metadata files must not be
  # auto-selected: macOS __MACOSX/*, ._ AppleDouble, .DS_Store; Windows Thumbs.db,
  # desktop.ini.
  filesExtracted <- c(
    "data/clean_NMC_20kmBuff.shp",
    "data/__MACOSX/._clean_NMC_20kmBuff.shp",
    "data/._clean_NMC_20kmBuff.shp",
    "data/.DS_Store",
    "data/Thumbs.db",
    "data/desktop.ini"
  )
  out <- .guessAtTargetAndFun(
    targetFilePath = NULL,
    filesExtracted = filesExtracted,
    fun = NULL,
    verbose = 0
  )
  expect_identical(out$targetFilePath, "data/clean_NMC_20kmBuff.shp")

  # If the user explicitly asks for an otherwise-filtered path, honor it.
  out2 <- .guessAtTargetAndFun(
    targetFilePath = "data/__MACOSX/._clean_NMC_20kmBuff.shp",
    filesExtracted = filesExtracted,
    fun = "readLines",
    verbose = 0
  )
  expect_identical(out2$targetFilePath, "data/__MACOSX/._clean_NMC_20kmBuff.shp")
})
