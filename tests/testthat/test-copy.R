test_that("test Copy", {
  library(raster)
  library(data.table)

  ras <- raster(extent(0, 10, 0, 10), vals = 1)
  tmpRasFilename <- tempfile("tmpRas", fileext = ".tif")
  tmpDir <- file.path(tempdir(), rndstr(1,6))
  checkPath(tmpDir, create = TRUE); on.exit(unlink(tmpDir, recursive = TRUE), add = TRUE)
  ras <- writeRaster(ras, filename = tmpRasFilename, overwrite = TRUE)
  ras2 <- Copy(ras, tmpDir)
  expect_true(all.equal(ras2[], ras[]))
  expect_false(filename(ras2) == filename(ras))

  dt <- data.table(a = 1:2, b = rev(LETTERS[1:2]))
  tmpDir <- file.path(tempdir(), "ras2")
  li <- list(dt = dt, ras = ras, ras2 = ras2)
  li2 <- Copy(li, tmpDir)

  # same content
  expect_true(all(unlist(lapply(seq_along(li), function(i) {
    if (is(li[[i]], "Raster")) {
       all.equal(li[[i]][], li2[[i]][])
     } else {
      all.equal(li[[i]], li2[[i]])
    }
  }))))

  # different filenames for Rasters
  expect_false(all(unlist(lapply(seq_along(li)[-1], function(i) {
    isTRUE(all.equal(filename(li[[i]]), filename(li2[[i]])))
  }))))

  # data.table
  setkeyv(li[[1]], "b")
  expect_false(isTRUE(all.equal(li[[1]], li2[[1]])))

  ### environments
  dt <- data.table(a = 1:2, b = rev(LETTERS[1:2]))
  li <- list(dt = dt, ras = ras, ras2 = ras2)
  li <- list2env(li, env = new.env())

  tmpDir <- file.path(tempdir(), "ras3")
  li2 <- Copy(li, tmpDir)

  expect_true(all(unlist(lapply(names(li), function(i) {
    if (is(li[[i]], "Raster")) {
      all.equal(li[[i]][], li2[[i]][])
    } else {
      all.equal(li[[i]], li2[[i]])
    }
  }))))

  # different filenames for Rasters
  expect_false(all(unlist(lapply(names(li)[-1], function(i) {
    isTRUE(all.equal(filename(li[[i]]), filename(li2[[i]])))
  }))))

  # data.table
  setkeyv(li[["dt"]], "b")
  expect_false(isTRUE(all.equal(li[["dt"]], li2[["dt"]])))

  ### Nested Environments
  dt <- data.table(a = 1:2, b = rev(LETTERS[1:2]))
  li <- list(dt = dt, ras = ras, ras2 = ras2)
  env1 <- new.env()
  env2 <- new.env()
  liEnv <- list2env(li, env = env1)
  liEnv[["env"]] <- li

  tmpDir <- file.path(tempdir(), "ras3")
  liEnv2 <- Copy(liEnv, tmpDir)

  expect_true(all(unlist(lapply(names(liEnv[["env"]]), function(i) {
    if (is(li[[i]], "Raster")) {
      all.equal(liEnv[["env"]][[i]][], liEnv2[["env"]][[i]][])
    } else {
      all.equal(liEnv[["env"]][[i]], liEnv2[["env"]][[i]])
    }

  }))))

  # different filenames for Rasters
  expect_false(all(unlist(lapply(names(liEnv[["env"]])[-1], function(i) {
    isTRUE(all.equal(filename(liEnv[["env"]][[i]]), filename(liEnv2[["env"]][[i]])))
  }))))

  # data.table
  setkeyv(liEnv[["env"]][["dt"]], "b")
  expect_false(isTRUE(all.equal(liEnv[["env"]][["dt"]], liEnv2[["env"]][["dt"]])))
})
