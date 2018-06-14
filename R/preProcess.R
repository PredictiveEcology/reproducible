#' Download, Checksum, Extract files
#'
#' This does downloading (via \code{downloadFile}), checksumming (\code{Checksums}),
#' and extracting from archives (\code{extractFromArchive}), plus cleaning up of input
#' arguments (e.g., paths, function names).
#' This is the first stage of three used in \code{prepInputs}.
#'
#' @return
#' A list with 5 elements, \code{checkSums} (the result of a \code{Checksums}
#' after downloading), \code{dots} (cleaned up ..., including deprecated argument checks),
#' \code{fun} (the function to be used to load the preProcessed object from disk),
#' \code{targetFilePath} (the fully qualified path to the \code{targetFile}),
#' and \code{tryRasterFn} (a logical whether the the \code{targetFilePath}
#' should be loaded with \code{\link[raster]{raster}}).
#'
#' @author Eliot McIntire
#' @export
#' @inheritParams prepInputs
preProcess <- function(targetFile = NULL, url = NULL, archive = NULL, alsoExtract = NULL,
                       destinationPath = ".", fun = NULL,
                       quick = getOption("reproducible.quick"),
                       overwrite = FALSE, purge = FALSE,
                       useCache = getOption("reproducible.useCache", FALSE), ...) {
  dots <- list(...)

  if (!is.null(dots$cacheTags))  {
    message("cacheTags is being deprecated;",
            " use userTags which will pass directly to Cache.")
    dots$userTags <- dots$cacheTags
    dots$cacheTags <- NULL
  }
  if (!is.null(dots$postProcessedFilename))  {
    message("postProcessedFilename is being deprecated;",
            " use filename2, used in determineFilename.")
    dots$filename2 <- dots$postProcessedFilename
    dots$postProcessedFilename <- NULL
  }
  if (!is.null(dots$writeCropped))  {
    message("writeCropped is being deprecated;",
            " use filename2, used in determineFilename.")
    dots$filename2 <- dots$writeCropped
    dots$writeCropped <- NULL
  }
  if (!is.null(dots$rasterInterpMethod))  {
    message("rasterInterpMethod is being deprecated;",
            " use method which will pass directly to projectRaster.")
    dots$method <- dots$rasterInterpMethod
    dots$rasterInterpMethod <- NULL
  }
  if (!is.null(dots$rasterDatatype))  {
    message("rasterDatatype is being deprecated;",
            " use datatype which will pass directly to writeRaster.")
    dots$datatype <- dots$rasterDatatype
    dots$rasterDatatype <- NULL
  }
  if (!is.null(dots$pkg))  {
    message("pkg is being deprecated;",
            "name the package and function directly, if needed,\n",
            "  e.g., 'pkg::fun'.")
    fun <- paste0(dots$pkg, "::", fun)
    dots$pkg <- NULL
  }
  # remove trailing slash -- causes unzip fail if it is there
  destinationPath <- gsub("\\\\$|/$", "", destinationPath)

  if (is.null(targetFile)) {
    targetFilePath <- NULL
  } else {
    targetFile <- basename(targetFile)
    targetFilePath <- file.path(destinationPath, targetFile)
  }


  if (!is.null(alsoExtract)) {
    alsoExtract <- basename(alsoExtract)
    alsoExtract <- file.path(destinationPath, alsoExtract)
  }

  checkSumFilePath <- file.path(destinationPath, "CHECKSUMS.txt")
  if (!dir.exists(destinationPath)) {
    if (isFile(destinationPath)) {
      stop("destinationPath must be a directory")
    }
    checkPath(destinationPath, create = TRUE)
  }

  message("Preparing: ", targetFile)

  needChecksums <- 0

  if (!is.null(archive)) {
    archive <- file.path(destinationPath, basename(archive))
    filesToCheck <- c(targetFilePath, archive, alsoExtract)
  } else {
    if (!is.null(url)) {
      archive <- .isArchive(file.path(destinationPath, basename(url)))
    }
    filesToCheck <- c(targetFilePath, alsoExtract)
  }

  needEmptyChecksums <- FALSE
  if (is.logical(purge)) purge <- as.numeric(purge)
  if (purge == 1) {
    unlink(checkSumFilePath)
    needChecksums <- 1
  }

  # Need to run checksums on all files in destinationPath because we may not know what files we
  #   want if targetFile, archive, alsoExtract not specified
  checkSums <- try(Checksums(path = destinationPath, write = FALSE), silent = TRUE)

  if (is(checkSums, "try-error")) {
    needChecksums <- 1
    checkSums <- .emptyChecksumsResult
  }

  if (purge > 1)  {
    checkSums <- .purge(checkSums = checkSums, purge = purge)
    needChecksums <- 2
  }

  neededFiles <- c(targetFile, if (!is.null(alsoExtract)) basename(alsoExtract))
  if (is.null(neededFiles)) neededFiles <- if (!is.null(archive)) basename(archive)

  # Stage 1 -- Download
  downloadFileResult <- downloadFile(
    archive = archive,
    targetFile = targetFile,
    neededFiles = neededFiles,
    destinationPath = destinationPath,
    quick = quick,
    checkSums = checkSums,
    url = url,
    checksumFile = asPath(checkSumFilePath),
    needChecksums = needChecksums,
    overwrite = overwrite,
    purge = purge # may need to try purging again if no target, archive or alsoExtract were known yet
  )#, moduleName = moduleName, modulePath = modulePath)
  checkSums <- downloadFileResult$checkSums
  needChecksums <- downloadFileResult$needChecksums
  neededFiles <- downloadFileResult$neededFiles
  if (length(neededFiles) > 1) alsoExtract <- setdiff(neededFiles, targetFile)
  if (is.null(archive)) archive <- downloadFileResult$archive

  filesToChecksum <- if (is.null(archive)) downloadFileResult$downloaded else basename(archive)
  on.exit({
    if (needChecksums > 0) {
      # needChecksums 1 --> write a new checksums.txt file
      # needChecksums 2 --> append  a new checksums.txt file
      appendChecksumsTable(checkSumFilePath = checkSumFilePath, filesToChecksum = basename(filesToChecksum),
                           destinationPath = destinationPath, append = needChecksums == 2)
    }
  })

  # Stage 1 - Extract from archive
  neededFiles <- c(targetFile, if (!is.null(alsoExtract)) basename(alsoExtract))
  filesExtracted <- extractFromArchive(archive = archive, destinationPath = destinationPath,
                                       neededFiles = neededFiles,
                                       checkSums = checkSums, needChecksums = needChecksums)

  filesToChecksum <- unique(c(filesToChecksum, targetFile, alsoExtract,
                              basename(filesExtracted$filesExtracted)))
  needChecksums <- filesExtracted$needChecksums

  #targetFilePath might still be NULL, need destinationPath too
  targetParams <- .guessAtTargetAndFun(targetFilePath, destinationPath,
                                       c(unique(filesToChecksum, filesExtracted$filesExtracted)),
                                       fun) # passes through if all known
  targetFile <- basename(targetParams$targetFilePath)
  targetFilePath <- targetParams$targetFilePath
  fun <- targetParams$fun

  # Now that all files are downloaded and extracted from archive, deal with missing targetFilePath
  tryRasterFn <- if (endsWith(suffix = "raster", fun)) TRUE else FALSE

  # fun is a charcter string, convert to function
  if (grepl("::", fun)) {
    fun2 <- strsplit(fun, "::")[[1]]
    pkg <- fun2[1]
    fun <- fun2[2]
    fun <- getFromNamespace(fun, pkg)
  } else {
    fun <- get(fun)
  }


  if (needChecksums > 0) {
    # needChecksums 1 --> write a new checksums.txt file
    # needChecksums 2 --> append  a new checksums.txt file
    checkSums <-
      appendChecksumsTable(
        checkSumFilePath = checkSumFilePath,
        filesToChecksum = basename(filesToChecksum),
        destinationPath = destinationPath,
        append = needChecksums == 2
      )
    on.exit() # remove on.exit because it is done here
  }
  out <- list(checkSums = checkSums,
              dots = dots,
              fun = fun,
              targetFilePath = targetFilePath,
              tryRasterFn = tryRasterFn)
  return(out)
}

#' Purge individual line items from checksums file
#'
#' @inheritParams downloadFile
#' @keywords internal
#' @rdname purge
.purge <- function(checkSums, purge, targeFile, archive, alsoExtract, url) {
  purgeChar <- as.character(purge)
  checkSums <- tryCatch(
    switch(
      purgeChar,
      "2" = checkSums[!(checkSums$expectedFile %in% basename(targetFile)), ],
      "3" = checkSums[!(checkSums$expectedFile %in% basename(archive)), ],
      "4" = checkSums[!(checkSums$expectedFile %in% basename(alsoExtract)), ],
      "5" = checkSums[!(checkSums$expectedFile %in% basename(unique(c(targetFile, alsoExtract)))), ], #nolint
      "6" = checkSums[!(checkSums$expectedFile %in% basename(unique(c(targetFile, alsoExtract, archive)))), ], #nolint
      "7" = checkSums[!(checkSums$expectedFile %in% basename(url)), ] #nolint
    ), error = function(x) checkSums)
  checkSums
}

.emptyChecksumsResult <- data.table(expectedFile = character(), result = character())
.emptyChecksumsFileContent <- data.frame(file = character(), checksum = character(), filesize = character(),
                              algorithm = character())
