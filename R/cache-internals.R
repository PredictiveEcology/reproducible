verboseCacheMessage <- function(preDigest, functionName,
                             startHashTime, modifiedDots, quick,
                             verbose = getOption("reproducible.verbose", 1),
                             verboseLevel = 1) {
  if (verbose >= verboseLevel) {
    preDigestUnlist <- .unlistToCharacter(preDigest, 4)
    verboseDF0(verbose, functionName, startHashTime)
    # verboseDF <- data.frame(
    #   functionName = functionName,
    #   component = "Hashing",
    #   elapsedTime = as.numeric(difftime(endHashTime, startHashTime, units = "secs")),
    #   units = "secs",
    #   stringsAsFactors = FALSE
    # )

    # hashObjectSize <- unlist(lapply(modifiedDots, objSize, recursive = FALSE, quick = TRUE))
    hashObjectSize <- if (getOption("reproducible.objSize", TRUE)) {
      unlist(lapply(modifiedDots, function(x) {
        unname(attr(objSize(x, quick = FALSE), "objSize"))
      }))
    } else {
      Map(modifiedDots, function(x) NA)
    }

    lengths <- unlist(lapply(preDigestUnlist, function(x) length(unlist(x))))
    hashDetails <- data.frame(
      objectNames = rep(names(preDigestUnlist), lengths),
      hashElements = names(unlist(preDigestUnlist)),
      hash = unname(unlist(preDigestUnlist)),
      stringsAsFactors = FALSE
    )
    preDigestUnlistNames <- unlist(lapply(
      strsplit(names(unlist(preDigestUnlist)), split = "\\."), # nolint
      function(x) paste0(tail(x, 2), collapse = ".")
    ))
    hashObjectSizeNames <- unlist(lapply(
      strsplit(names(hashObjectSize), split = "\\$"),
      function(x) paste0(tail(x, 2), collapse = ".")
    ))
    hashObjectSizeNames <- gsub("\\.y", replacement = "", hashObjectSizeNames)
    hashObjectSizeNames <- unlist(lapply(
      strsplit(hashObjectSizeNames, split = "\\."),
      function(x) paste0(tail(x, 2), collapse = ".")
    ))
    hashDetails$objSize <- NA
    hashDetails$objSize[preDigestUnlistNames %in% hashObjectSizeNames] <-
      hashObjectSize[hashObjectSizeNames %in% preDigestUnlistNames]

    if (exists("hashDetails", envir = .reproEnv, inherits = FALSE)) {
      .reproEnv$hashDetails <- rbind(.reproEnv$hashDetails, hashDetails)
    } else {
      .reproEnv$hashDetails <- hashDetails
      on.exit(
        {
          assign("hashDetailsAll", .reproEnv$hashDetails, envir = .reproEnv)
          messageDF(.reproEnv$hashDetails, colour = "blue", verbose = verbose, verboseLevel = verboseLevel)
          messageCache("The hashing details are available from .reproEnv$hashDetailsAll",
                       verbose = verbose, verboseLevel = verboseLevel
          )
          rm("hashDetails", envir = .reproEnv)
        },
        add = TRUE
      )
    }
  }
}


