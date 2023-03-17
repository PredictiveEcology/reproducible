if (.requireNamespace("raster") && .requireNamespace("terra"))
  for (fn in list(raster::raster, terra::rast)) {
    ## LOG1S
    ras <- fn(ncol = 10, nrow = 10)
    ras[] <- rep(c(0,1),50)
    assessDataType(ras)

    ras <- fn(ncol = 10, nrow = 10)
    ras[] <- rep(c(0,1),50)
    assessDataType(ras)

    ras[] <- rep(c(TRUE,FALSE),50)
    assessDataType(ras)

    ras[] <- c(NA, NA, rep(c(0,1),49))
    assessDataType(ras)

    ras <- fn(ncol = 10, nrow = 10)
    ras[] <- c(0, NaN, rep(c(0,1),49))
    assessDataType(ras)


    ## INT1S
    ras[] <- -1:98
    assessDataType(ras)

    ras[] <- c(NA, -1:97)
    assessDataType(ras)

    ## INT1U
    ras <- fn(ncol = 10, nrow = 10)
    ras[] <- 1:100
    assessDataType(ras)

    ras[] <- c(NA, 2:100)
    assessDataType(ras)

    ## INT2U
    ras <- fn(ncol = 10, nrow = 10)
    ras[] <- round(runif(100, min = 64000, max = 65000))
    assessDataType(ras)

    ## INT2S
    ras <- fn(ncol = 10, nrow = 10)
    ras[] <- round(runif(100, min = -32767, max = 32767))
    assessDataType(ras)

    ras[54] <- NA
    assessDataType(ras)

    ## INT4U
    ras <- fn(ncol = 10, nrow = 10)
    ras[] <- round(runif(100, min = 0, max = 500000000))
    assessDataType(ras)

    ras[14] <- NA
    assessDataType(ras)

    ## INT4S
    ras <- fn(ncol = 10, nrow = 10)
    ras[] <- round(runif(100, min = -200000000, max = 200000000))
    assessDataType(ras)

    ras[14] <- NA
    assessDataType(ras)

    ## FLT4S
    ras <- fn(ncol = 10, nrow = 10)
    ras[] <- runif(100, min = -10, max = 87)
    assessDataType(ras)

    ras <- fn(ncol = 10, nrow = 10)
    ras[] <- round(runif(100, min = -3.4e+26, max = 3.4e+28))
    assessDataType(ras)

    ras <- fn(ncol = 10, nrow = 10)
    ras[] <- round(runif(100, min = 3.4e+26, max = 3.4e+28))
    assessDataType(ras)

    ras <- fn(ncol = 10, nrow = 10)
    ras[] <- round(runif(100, min = -3.4e+26, max = -1))
    assessDataType(ras)

    ## FLT8S
    ras <- fn(ncol = 10, nrow = 10)
    ras[] <- c(-Inf, 1, rep(c(0,1),49))
    assessDataType(ras)

    ras <- fn(ncol = 10, nrow = 10)
    ras[] <- c(Inf, 1, rep(c(0,1),49))
    assessDataType(ras)

    ras <- fn(ncol = 10, nrow = 10)
    ras[] <- round(runif(100, min = -1.7e+30, max = 1.7e+308))
    assessDataType(ras)

    ras <- fn(ncol = 10, nrow = 10)
    ras[] <- round(runif(100, min = 1.7e+30, max = 1.7e+308))
    assessDataType(ras)

    ras <- fn(ncol = 10, nrow = 10)
    ras[] <- round(runif(100, min = -1.7e+308, max = -1))
    assessDataType(ras)

    # stack LOG1S and FLT8S
    ras <- fn(ncol = 10, nrow = 10)
    ras[] <- rep(c(0,1),50)
    ras1 <- fn(ncol = 10, nrow = 10)
    ras1[] <- round(runif(100, min = -1.7e+308, max = -1))
    if (is(ras1, "Raster"))
      sta <- stack(ras, ras1)
    else
      sta <- c(ras, ras1)
    assessDataType(sta)
  }
