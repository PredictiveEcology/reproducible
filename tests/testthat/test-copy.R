test_that("test Copy", {
  skip_if_not_installed("terra")
  testInit(c("terra", "data.table"), tmpFileExt = ".tif")

  ras <- terra::rast(terra::ext(0, 10, 0, 10), vals = 1)
  ras <- suppressWarningsSpecific(
    falseWarnings = proj6Warn,
    writeRaster(ras, filename = tmpfile, overwrite = TRUE)
  )
  # This will make hardlink
  ras2 <- suppressWarningsSpecific(Copy(ras, tmpdir), "NOT UPDATED FOR PROJ >= 6")

  expect_true(all.equal(ras2[], ras[]))
  expect_false(Filenames(ras2) == Filenames(ras))

  dt <- data.table(a = 1:2, b = rev(LETTERS[1:2]))
  tmpdir <- normPath(tempdir2("ras2"))
  checkPath(tmpdir, create = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
  li <- list(dt = dt, ras = ras, ras2 = ras2)
  li2 <- Copy(li, tmpdir)

  # same content
  expect_true(all(unlist(lapply(seq_along(li), function(i) {
    if (.isSpatRaster(li[[i]])) {
      all.equal(values2(li[[i]]), values2(li2[[i]]))
    } else {
      all.equal(li[[i]], li2[[i]])
    }
  }))))

  # different filenames for Rasters
  expect_false(all(unlist(lapply(seq_along(li)[-1], function(i) {
    isTRUE(all.equal(Filenames(li[[i]]), Filenames(li2[[i]])))
  }))))

  # data.table
  setkeyv(li[[1]], "b")
  expect_false(isTRUE(all.equal(li[[1]], li2[[1]])))

  ### environments
  dt <- data.table(a = 1:2, b = rev(LETTERS[1:2]))
  li <- list(dt = dt, ras = ras, ras2 = ras2)
  li <- list2env(li, envir = new.env(parent = emptyenv()))

  tmpdir <- tempdir2("ras3")
  li2 <- Copy(li, tmpdir)

  expect_true(all(unlist(lapply(names(li), function(i) {
    if (.isSpatRaster(li[[i]])) {
      all.equal(li[[i]][], li2[[i]][])
    } else {
      all.equal(li[[i]], li2[[i]])
    }
  }))))

  # different filenames for Rasters
  expect_false(all(unlist(lapply(names(li)[-1], function(i) {
    isTRUE(all.equal(Filenames(li[[i]]), Filenames(li2[[i]])))
  }))))

  # data.table
  setkeyv(li[["dt"]], "b")
  expect_false(isTRUE(all.equal(li[["dt"]], li2[["dt"]])))

  ### Nested Environments
  dt <- data.table(a = 1:2, b = rev(LETTERS[1:2]))
  li <- list(dt = dt, ras = ras, ras2 = ras2)
  env1 <- new.env(parent = emptyenv())
  env2 <- new.env(parent = emptyenv())
  liEnv <- list2env(li, envir = env1)
  liEnv[["env"]] <- li

  tmpdir <- tempdir2("ras3")
  liEnv2 <- Copy(liEnv, tmpdir)

  expect_true(all(unlist(lapply(names(liEnv[["env"]]), function(i) {
    if (.isSpatRaster(li[[i]])) {
      all.equal(liEnv[["env"]][[i]][], liEnv2[["env"]][[i]][])
    } else {
      all.equal(liEnv[["env"]][[i]], liEnv2[["env"]][[i]])
    }
  }))))

  # different filenames for Rasters
  expect_false(all(unlist(lapply(names(liEnv[["env"]])[-1], function(i) {
    isTRUE(all.equal(Filenames(liEnv[["env"]][[i]]), Filenames(liEnv2[["env"]][[i]])))
  }))))

  # data.table
  setkeyv(liEnv[["env"]][["dt"]], "b")
  expect_false(isTRUE(all.equal(liEnv[["env"]][["dt"]], liEnv2[["env"]][["dt"]])))

  ###################
  # This is an aside on testing whether the hard link can be messed with by overwrite
  fn <- Filenames(ras)
  ras3 <- terra::rast(terra::ext(0, 10, 0, 10), vals = 2)
  ras3 <- suppressWarningsSpecific(
    writeRaster(ras3, filename = tmpfile, overwrite = TRUE),
    "NOT UPDATED FOR PROJ >= 6"
  ) # overwrite the original
  # The hardlink is not affected by "overwrite = TRUE" -- it is not by filename, but by file location
  expect_true(all(ras2[] == 1))
})
