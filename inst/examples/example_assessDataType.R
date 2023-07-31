if (requireNamespace("terra", quietly = TRUE)) {
  ## LOG1S
  rasOrig <- terra::rast(ncol = 10, nrow = 10)
  ras <- rasOrig
  ras[] <- rep(c(0,1),50)
  assessDataType(ras)

  ras <- rasOrig
  ras[] <- rep(c(0,1),50)
  assessDataType(ras)

  ras[] <- rep(c(TRUE,FALSE),50)
  assessDataType(ras)

  ras[] <- c(NA, NA, rep(c(0,1),49))
  assessDataType(ras)

  ras <- rasOrig
  ras[] <- c(0, NaN, rep(c(0,1),49))
  assessDataType(ras)


  ## INT1S
  ras[] <- -1:98
  assessDataType(ras)

  ras[] <- c(NA, -1:97)
  assessDataType(ras)

  ## INT1U
  ras <- rasOrig
  ras[] <- 1:100
  assessDataType(ras)

  ras[] <- c(NA, 2:100)
  assessDataType(ras)

  ## INT2U
  ras <- rasOrig
  ras[] <- round(runif(100, min = 64000, max = 65000))
  assessDataType(ras)

  ## INT2S
  ras <- rasOrig
  ras[] <- round(runif(100, min = -32767, max = 32767))
  assessDataType(ras)

  ras[54] <- NA
  assessDataType(ras)

  ## INT4U
  ras <- rasOrig
  ras[] <- round(runif(100, min = 0, max = 500000000))
  assessDataType(ras)

  ras[14] <- NA
  assessDataType(ras)

  ## INT4S
  ras <- rasOrig
  ras[] <- round(runif(100, min = -200000000, max = 200000000))
  assessDataType(ras)

  ras[14] <- NA
  assessDataType(ras)

  ## FLT4S
  ras <- rasOrig
  ras[] <- runif(100, min = -10, max = 87)
  assessDataType(ras)

  ras <- rasOrig
  ras[] <- round(runif(100, min = -3.4e+26, max = 3.4e+28))
  assessDataType(ras)

  ras <- rasOrig
  ras[] <- round(runif(100, min = 3.4e+26, max = 3.4e+28))
  assessDataType(ras)

  ras <- rasOrig
  ras[] <- round(runif(100, min = -3.4e+26, max = -1))
  assessDataType(ras)

  ## FLT8S
  ras <- rasOrig
  ras[] <- c(-Inf, 1, rep(c(0,1),49))
  assessDataType(ras)

  ras <- rasOrig
  ras[] <- c(Inf, 1, rep(c(0,1),49))
  assessDataType(ras)

  ras <- rasOrig
  ras[] <- round(runif(100, min = -1.7e+30, max = 1.7e+308))
  assessDataType(ras)

  ras <- rasOrig
  ras[] <- round(runif(100, min = 1.7e+30, max = 1.7e+308))
  assessDataType(ras)

  ras <- rasOrig
  ras[] <- round(runif(100, min = -1.7e+308, max = -1))
  assessDataType(ras)

  # 2 layer with different types LOG1S and FLT8S
  ras <- rasOrig
  ras[] <- rep(c(0,1),50)
  ras1 <- rasOrig
  ras1[] <- round(runif(100, min = -1.7e+308, max = -1))
  sta <- c(ras, ras1)
  assessDataType(sta)

}
