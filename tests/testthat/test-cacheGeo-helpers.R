test_that("checkNameHasGeom renames geom -> geometry", {
  df <- data.frame(name = "A", geom = NA)
  out <- reproducible:::checkNameHasGeom(df)
  expect_named(out, c("name", "geometry"))
})

test_that("checkNameHasGeom is a no-op when no geom column present", {
  df <- data.frame(name = "A", geometry = NA)
  out <- reproducible:::checkNameHasGeom(df)
  expect_identical(out, df)
})

test_that("update_bbox refreshes the bbox attribute from coordinates", {
  skip_if_not_installed("sf")
  pts <- sf::st_as_sf(
    data.frame(id = 1:3, x = c(0, 10, 5), y = c(0, 0, 10)),
    coords = c("x", "y"),
    crs = 4326
  )
  attr(sf::st_geometry(pts), "bbox") <- structure(
    c(xmin = 99, ymin = 99, xmax = 99, ymax = 99),
    class = "bbox"
  )
  out <- reproducible:::update_bbox(pts)
  bb <- attr(sf::st_geometry(out), "bbox")
  expect_equal(unname(as.numeric(bb)), c(0, 0, 10, 10))
})

test_that("extractPolygonIfWithin returns matching subset when domain fits", {
  skip_if_not_installed("sf")
  lux <- system.file("ex/lux.shp", package = "terra")
  skip_if(!nzchar(lux))
  full <- sf::st_read(lux, quiet = TRUE)
  big <- full[1:6, ]
  domain <- full[3, ]
  out <- reproducible:::extractPolygonIfWithin(
    domain = domain,
    existingObjSF = big,
    bufferOK = FALSE,
    existingObj = big,
    verbose = FALSE
  )
  expect_true(out$domainExisted)
  expect_lt(NROW(out$existingObjSF), NROW(big))
  expect_gt(NROW(out$existingObjSF), 0)
})

test_that("extractPolygonIfWithin with bufferOK=TRUE attempts a buffered re-check", {
  skip_if_not_installed("sf")
  lux <- system.file("ex/lux.shp", package = "terra")
  skip_if(!nzchar(lux))
  full <- sf::st_read(lux, quiet = TRUE)
  inside <- full[1:3, ]
  outside <- full[8, ]
  expect_message(
    out <- reproducible:::extractPolygonIfWithin(
      domain = outside,
      existingObjSF = inside,
      bufferOK = TRUE,
      existingObj = inside,
      verbose = FALSE
    ),
    "trying a .+ buffer"
  )
  expect_false(out$domainExisted)
})

test_that("extractPolygonIfWithin returns NULL when domain is fully outside", {
  skip_if_not_installed("sf")
  lux <- system.file("ex/lux.shp", package = "terra")
  skip_if(!nzchar(lux))
  full <- sf::st_read(lux, quiet = TRUE)
  inside <- full[1:3, ]
  outside <- full[8, ] # picked because the existing test marks it disjoint from 1:3
  out <- reproducible:::extractPolygonIfWithin(
    domain = outside,
    existingObjSF = inside,
    bufferOK = FALSE,
    existingObj = inside,
    verbose = FALSE
  )
  expect_false(out$domainExisted)
  expect_null(out$existingObj)
  expect_null(out$existingObjSF)
})
