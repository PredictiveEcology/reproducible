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
#' and \code{targetFilePath} (the fully qualified path to the \code{targetFile}).
#'
#' @section Combinations of \code{targetFile}, \code{url}, \code{archive}, \code{alsoExtract}:
#'
#'   \tabular{ccccclll}{
#'  # Params \tab \code{url} \tab \code{targetFile} \tab \code{archive}\tab \code{alsoExtract} \tab Result \tab Checksum 1st time \tab Checksum 2nd time \cr
#'  ------ \tab ------ \tab ------ \tab ------ \tab ------ \tab ------ \tab ------ \tab ------  \cr
#' \bold{1} \tab      char \tab NULL \tab NULL \tab NULL             \tab Download, extract all files if an archive, guess at \code{targetFile}, load into R \tab write or append all new files \tab same as 1st -- no \code{targetFile}* \cr
#'      \tab NULL \tab char \tab NULL \tab NULL             \tab load \code{targetFile} into R \tab write or append \code{targetFile} \tab no downloading, so no checksums use \cr
#'      \tab NULL \tab NULL \tab char \tab NULL             \tab extract all files, guess at \code{targetFile}, load into R \tab write or append all new files \tab no downloading, so no checksums use \cr
#'      \tab NULL \tab NULL \tab NULL \tab char             \tab guess at \code{targetFile} from files in \code{alsoExtract}, load into R \tab write or append all new files \tab no downloading, so no checksums use \cr
#'  ------ \tab ------ \tab ------ \tab ------ \tab ------ \tab ------ \tab ------ \tab ------ \cr
#' \bold{2} \tab char \tab char \tab NULL \tab NULL             \tab Download, extract all files if an archive, load \code{targetFile} into R\tab write or append all new files \tab use Checksums, skip downloading \cr
#'      \tab char \tab NULL \tab char \tab NULL             \tab Download, extract all files, guess at \code{targetFile}, load into R\tab write or append all new files \tab same as 1st -- no \code{targetFile}* \cr
#'      \tab char \tab NULL \tab NULL \tab char             \tab Download, extract only named files in \code{alsoExtract}, guess at \code{targetFile}, load into R\tab write or append all new files \tab same as 1st -- no \code{targetFile}* \cr
#'      \tab NULL \tab char \tab NULL \tab char             \tab load \code{targetFile} into R \tab write or append all new files \tab no downloading, so no checksums use \cr
#'      \tab NULL \tab char \tab char \tab NULL             \tab Extract all files, load \code{targetFile} into R\tab write or append all new files \tab no downloading, so no checksums use \cr
#'      \tab NULL \tab NULL \tab char \tab char             \tab Extract only named files in \code{alsoExtract}, guess at \code{targetFile}, load into R\tab write or append all new files \tab no downloading, so no checksums use \cr
#'  ------ \tab ------ \tab ------ \tab ------ \tab ------ \tab ------ \tab ------ \tab ------ \cr
#' \bold{3} \tab char \tab char \tab char \tab NULL             \tab Download, extract all files, load \code{targetFile} into R\tab write or append all new files \tab use Checksums, skip downloading \cr
#'      \tab char \tab NULL \tab char \tab char             \tab Download, extract files named in \code{alsoExtract}, guess at \code{targetFile}, load into R\tab write or append all new files \tab use Checksums, skip downloading \cr
#'      \tab char \tab NULL \tab char \tab \code{"similar"} \tab Download, extract all files (can't understand "similar"), guess at \code{targetFile}, load into R\tab write or append all new files \tab same as 1st -- no \code{targetFile}* \cr
#'      \tab char \tab char \tab NULL \tab char             \tab Download, if an archive, extract files named in \code{targetFile} and \code{alsoExtract}, load \code{targetFile} into R\tab write or append all new files \tab use Checksums, skip downloading \cr
#'      \tab char \tab char \tab NULL \tab \code{"similar"} \tab Download, if an archive, extract files with same base as \code{targetFile}, load \code{targetFile} into R\tab write or append all new files \tab use Checksums, skip downloading \cr
#'      \tab char \tab char \tab char \tab NULL             \tab Download, extract all files from archive, load \code{targetFile} into R\tab write or append all new files \tab use Checksums, skip downloading \cr
#'      \tab NULL \tab char \tab char \tab char             \tab Extract  files named in \code{alsoExtract} from archive, load \code{targetFile} into R\tab write or append all new files \tab no downloading, so no checksums use \cr
#'  ------ \tab ------ \tab ------ \tab ------ \tab ------ \tab ------ \tab ------ \tab ------ \cr
#' \bold{4} \tab char \tab char \tab char \tab char             \tab Download, extract files named in \code{targetFile} and \code{alsoExtract}, load \code{targetFile} into R\tab write or append all new files \tab use Checksums, skip downloading \cr
#'      \tab char \tab char \tab char \tab \code{"similar"} \tab Download, extract all files with same base as \code{targetFile}, load \code{targetFile} into R\tab write or append all new files \tab use Checksums, skip downloading \cr
#'   }
#'  \code{*} If the \code{url} is a file on Google Drive, checksumming will work
#'  even without a \code{targetFile} specified because there is an initial attempt
#'  to get the remove file information (e.g., file name). With that, the connection
#'  between the \code{url} and the filename used in the CHECKSUMS.txt file can be made.
#'
#' @inheritParams prepInputs
#' @inheritParams downloadFile
#'
#' @author Eliot McIntire
#' @export
#' @importFrom data.table fread setDT
#' @importFrom tools file_path_sans_ext
preProcess <- function(targetFile = NULL, url = NULL, archive = NULL, alsoExtract = NULL,
                       destinationPath = getOption("reproducible.destinationPath", "."),
                       fun = NULL, dlFun = NULL,
                       quick = getOption("reproducible.quick"),
                       overwrite = getOption("reproducible.overwrite", FALSE),
                       purge = FALSE,
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
  checkSumFilePath <- file.path(destinationPath, "CHECKSUMS.txt")

  if (is.null(targetFile)) {
    fileGuess <- .guessAtFile(url = url, archive = archive,
                              targetFile = targetFile,
                              destinationPath = destinationPath)
    if (is.null(archive))
      archive <- .isArchive(fileGuess)
    if (is.null(archive) && !is.null(fileGuess)) {
      message("targetFile was not supplied; guessed and will try ", fileGuess,
              ". If this is incorrect, please supply targetFile")
      targetFile <- .basename(fileGuess)
      targetFilePath <- fileGuess
    } else {
      targetFilePath <- NULL
    }
  } else {
    targetFile <- .basename(targetFile)
    targetFilePath <- file.path(destinationPath, targetFile)
    if (is.null(alsoExtract)) {
      if (file.exists(checkSumFilePath)) {
        # if alsoExtract is not specified, then try to find all files in CHECKSUMS.txt with same base name, without extension
        checksumsTmp <- as.data.table(read.table(checkSumFilePath))
        alsoExtract <- grep(paste0(file_path_sans_ext(targetFile),"\\."), checksumsTmp$file,
                            value = TRUE)
        rm(checksumsTmp) # clean up
      }
    }
  }


  if (!is.null(alsoExtract)) {
    alsoExtract <- if (isTRUE(all(is.na(alsoExtract)))) {
      character()
    } else {
      file.path(destinationPath, .basename(alsoExtract))
    }
  }

  if (!dir.exists(destinationPath)) {
    if (isFile(destinationPath)) {
      stop("destinationPath must be a directory")
    }
    checkPath(destinationPath, create = TRUE)
  }

  message("Preparing: ", targetFile)

  needChecksums <- 0

  filesToCheck <- c(targetFilePath, alsoExtract)
  if (!is.null(archive)) {
    archive <- file.path(destinationPath, .basename(archive))
    filesToCheck <- c(filesToCheck, archive)
  }

  # Need to run checksums on all files in destinationPath because we may not know what files we
  #   want if targetFile, archive, alsoExtract not specified
  checkSums <- try(Checksums(path = destinationPath, write = FALSE,
                             files = filesToCheck), silent = TRUE)

  if (is(checkSums, "try-error")) {
    needChecksums <- 1
    checkSums <- .emptyChecksumsResult
  }

  # This will populate a NULL archive if archive is local or
  if (is.null(archive)) {
    if (!is.null(url)) {
      allOK <- .similarFilesInCheckSums(targetFile, checkSums)

      if (!allOK) { # skip identification of archive if we have all files with same basename as targetFile
        # BUT if we don't have all files with identical root name (basename sans ext), then assess for
        #   an archive, either remotely, in the case of google or from the basename of url
        fileGuess <- .guessAtFile(url = url, archive = archive,
                                  targetFile = targetFile, destinationPath = destinationPath)
        archive <- .isArchive(fileGuess)
        checkSums <- .checkSumsUpdate(destinationPath = destinationPath,
                                      newFilesToCheck = archive,
                                      checkSums = checkSums)

      }

    }
  }

  needEmptyChecksums <- FALSE
  if (is.logical(purge)) purge <- as.numeric(purge)
  if (purge == 1) {
    unlink(checkSumFilePath)
    needChecksums <- 1
  }

  if (purge > 1)  {
    checkSums <- .purge(checkSums = checkSums, purge = purge)
    needChecksums <- 2
  }

  neededFiles <- c(targetFile, if (!is.null(alsoExtract)) .basename(alsoExtract))
  if (is.null(neededFiles)) neededFiles <- if (!is.null(archive)) .basename(archive)
  neededFiles <- setdiff(neededFiles, "similar") # remove "similar" from needed files. It is for extracting.

  # Deal with "similar" in alsoExtract -- maybe this is obsolete with new feature that uses file_name_sans_ext
  if (is.null(alsoExtract)) {
    filesInsideArchive <- .listFilesInArchive(archive)
    if (isTRUE(length(filesInsideArchive)>0)) {
      checkSums <- .checkSumsUpdate(destinationPath, file.path(destinationPath, filesInsideArchive),
                                    checkSums = checkSums)
    }
    neededFiles <- unique(c(neededFiles, filesInsideArchive))
  } else {
    outFromSimilar <- .checkForSimilar(neededFiles, alsoExtract, archive, targetFile,
                                       destinationPath = destinationPath, checkSums, url)
    neededFiles <- outFromSimilar$neededFiles
    checkSums <- outFromSimilar$checkSums
  }


  filesToChecksum <- if (is.null(archive))  NULL else .basename(archive)
  isOK <- .compareChecksumsAndFiles(checkSums, c(filesToChecksum, neededFiles))
  if (isTRUE(!all(isOK))) {
    results <- .tryExtractFromArchive(archive = archive, neededFiles = neededFiles,
                                      alsoExtract = alsoExtract, destinationPath = destinationPath,
                                      checkSums = checkSums, needChecksums = needChecksums,
                                      checkSumFilePath = checkSumFilePath, filesToChecksum = filesToChecksum,
                                      targetFile = targetFile, quick = quick)
    checkSums <- results$checkSums
    needChecksums <- results$needChecksums
    neededFiles <- results$neededFiles
    filesExtr <- results$filesExtr

    if (results$needChecksums > 0) {
      checkSums <- appendChecksumsTable(
        checkSumFilePath = checkSumFilePath,
        filesToChecksum = unique(.basename(results$filesToChecksum)),
        destinationPath = destinationPath,
        append = results$needChecksums >= 2
      )
    }
  }
  # Check for local copies in all values of getOption("reproducible.inputPaths")
  # At the end of this function, the files will be present in destinationPath, if they existed
  #  in options("reproducible.inputPaths")
  localChecks <- .checkLocalSources(neededFiles, checkSums = checkSums,
                                    checkSumFilePath = checkSumFilePath,
                                    otherPaths = getOption("reproducible.inputPaths"),
                                    destinationPath, needChecksums = needChecksums)
  checkSums <- localChecks$checkSums
  needChecksums <- localChecks$needChecksums
  successfulCheckSumFilePath <- localChecks$successfulCheckSumFilePath
  successfulDir <- localChecks$successfulDir


  # Change the destinationPath to the reproducible.inputPaths temporarily, so
  #   download happens there. Later it will be linked to the user destinationPath
  if (!is.null(getOption("reproducible.inputPaths"))) {
    destinationPathUser <- destinationPath
    on.exit({
      destinationPath <- destinationPathUser
    }, add = TRUE)
    destinationPath <- getOption("reproducible.inputPaths")[1]
    if (isTRUE(any(grepl(archive, pattern = destinationPathUser)))) {
      # might have a "." as destinationPath -- messes with grepl
      patt <- if (grepl("^\\.", destinationPathUser))
        gsub("^\\.", "^\\\\.", destinationPathUser)
      else
        destinationPathUser
      archive <- gsub(archive, pattern = patt,
                      replacement = destinationPath)

    }
  }

  # Stage 1 -- Download
  downloadFileResult <- downloadFile(
    archive = archive,
    targetFile = targetFile,
    neededFiles = neededFiles,
    destinationPath = destinationPath,
    quick = quick,
    checkSums = checkSums,
    dlFun = dlFun,
    url = url,
    checksumFile = asPath(checkSumFilePath),
    needChecksums = needChecksums,
    overwrite = overwrite,
    purge = purge, # may need to try purging again if no target, archive or alsoExtract were known yet
    ...
  )#, moduleName = moduleName, modulePath = modulePath)
  checkSums <- downloadFileResult$checkSums
  needChecksums <- downloadFileResult$needChecksums
  neededFiles <- downloadFileResult$neededFiles

  # archive specified, alsoExtract is NULL --> now means will extract all
  if (is.null(archive)) archive <- downloadFileResult$archive

  # redo "similar" after download
  outFromSimilar <- .checkForSimilar(neededFiles, alsoExtract, archive, targetFile,
                                     destinationPath, checkSums, url)
  neededFiles <- outFromSimilar$neededFiles
  checkSums <- outFromSimilar$checkSums

  # don't include targetFile in neededFiles -- extractFromArchive deals with it separately
  if (length(neededFiles) > 1) alsoExtract <- setdiff(neededFiles, targetFile)

  # To this point, we only have the archive in hand -- include this in the list of filesToChecksum
  filesToChecksum <- if (is.null(archive)) downloadFileResult$downloaded else .basename(archive)
  on.exit({
    if (needChecksums > 0) {
      # needChecksums 1 --> write a new checksums.txt file
      # needChecksums 2 --> append to checksums.txt
      appendChecksumsTable(checkSumFilePath = checkSumFilePath,
                           filesToChecksum = .basename(filesToChecksum),
                           destinationPath = destinationPath,
                           append = (needChecksums == 2))
    }
  })

  # Stage 1 - Extract from archive
  isOK <- .compareChecksumsAndFiles(checkSums, c(filesToChecksum, neededFiles))
  if (isTRUE(!all(isOK))) {
    filesExtracted <- .tryExtractFromArchive(archive = archive, neededFiles = neededFiles,
                                             alsoExtract = alsoExtract, destinationPath = destinationPath,
                                             checkSums = checkSums, needChecksums = needChecksums,
                                             checkSumFilePath = checkSumFilePath, filesToChecksum = filesToChecksum,
                                             targetFile = targetFile, quick = quick)

    filesExtr <- filesExtracted$filesExtr
    filesToChecksum <- filesExtracted$filesToChecksum
    needChecksums <- filesExtracted$needChecksums
    checkSums <- filesExtracted$checkSums
  } else {
    if (!is.null(.isArchive(archive)))
      message("  Skipping extractFromArchive attempt: no files missing")

    filesExtr <- c(filesToChecksum, neededFiles)
    filesExtr <- setdiff(filesExtr, .isArchive(filesExtr))
  }

  filesExtr <- if (!is.null(filesExtr)) {
    unique(c(.basename(filesExtr), .basename(filesToChecksum)))
  } else {
    unique(c(.basename(filesToChecksum)))
  }

  # link back to destinationPath if options("reproducible.inputPaths") was used.
  #  destinationPath had been overwritten to be options("reproducible.inputPaths")
  if (!is.null(getOption("reproducible.inputPaths"))) {
    #foundRecursively <- localChecks$foundRecursively
    foundInInputPaths <- localChecks$foundInInputPaths
    copyToIP <- (!filesExtr %in% foundInInputPaths)
    # Make sure they are all in options("reproducible.inputPaths"), accounting for
    #   the fact that some may have been in sub-folders -- i.e., don't deal with these
    if (isTRUE(any(copyToIP))) {
      logicalFilesExistIP <- file.exists(file.path(destinationPath, filesExtr[copyToIP]))
      if (!isTRUE(all(logicalFilesExistIP))) {
        linkOrCopy(file.path(destinationPathUser, filesExtr[!logicalFilesExistIP]),
                   file.path(destinationPath, filesExtr[!logicalFilesExistIP]))
      }
    }

    # Now make sure all are in original destinationPath
    logicalFilesExistDP <- file.exists(file.path(destinationPathUser, filesExtr))
    if (!isTRUE(all(logicalFilesExistDP))) {
      linkOrCopy(file.path(destinationPath, filesExtr[!logicalFilesExistDP]),
                 file.path(destinationPathUser, filesExtr[!logicalFilesExistDP]))
    }
  }

  targetParams <- .guessAtTargetAndFun(targetFilePath, destinationPath,
                                       filesExtracted = filesExtr,
                                       fun) # passes through if all known
  targetFile <- .basename(targetParams$targetFilePath)
  targetFilePath <- targetParams$targetFilePath
  fun <- targetParams$fun

  ## targetFilePath might still be NULL, need destinationPath too
  if (is.null(targetFilePath)) {
    if (is.null(filesExtracted$filesExtr)) {
      if (!is.null(downloadFileResult$downloaded))
        targetFilePath <- downloadFileResult$downloaded
    } else {
      targetFilePath <- filesExtracted$filesExtr
    }
  }

  if (is.null(targetFile) && !is.null(targetFilePath)) {
    targetFile <- .basename(targetFilePath)
  }

  ## Convert the fun as character string to function class, if not already
  fun <- .extractFunction(fun)

  if (needChecksums > 0) {
    ## needChecksums 1 --> write a new CHECKSUMS.txt file
    ## needChecksums 2 --> append  to CHECKSUMS.txt file
    ## needChecksums 3 --> append  to checkSumFilePath file OR successfulCheckSumFilePath, not both
    if (needChecksums == 3) {
      # successfulCheckSumFilePath we do not need to update. Determine which one this is, and do
      #   other
      if (identical(checkSumFilePath, successfulCheckSumFilePath)) { # if it was in checkSumFilePath
        checkSumFilePath <- file.path(successfulDir, "CHECKSUMS.txt")   #   run Checksums in IP
      }
      destinationPath <- destinationPathUser
    }
    checkSums <- appendChecksumsTable(
      checkSumFilePath = checkSumFilePath,
      filesToChecksum = unique(.basename(filesToChecksum)),
      destinationPath = destinationPath,
      append = needChecksums >= 2
    )
    if (!is.null(getOption("reproducible.inputPaths")) && needChecksums != 3) {
      checkSumFilePathInputPaths <- file.path(getOption("reproducible.inputPaths")[[1]],
                                              "CHECKSUMS.txt")
      suppressMessages(checkSums <- appendChecksumsTable(
        checkSumFilePath = checkSumFilePathInputPaths,
        filesToChecksum = unique(.basename(filesToChecksum)),
        destinationPath = destinationPathUser,
        append = needChecksums == 2
      ))
      destinationPath <- destinationPathUser # reset to original argument AFTER checksums
    }


    on.exit() # remove on.exit because it is done here
  }
  failStop <- if (is.null(targetFilePath)) {
    TRUE
  } else if (!isTRUE(file.exists(targetFilePath))) {
    TRUE
  } else { FALSE }
  if (isTRUE(failStop))
    stop("targetFile appears to be misspecified. ",
         "Possibly, it does not exist in the specified archive, ",
         "or the file doesn't exist in destinationPath")

  out <- list(checkSums = checkSums,
              dots = dots,
              fun = fun,
              targetFilePath = targetFilePath,
              destinationPath = destinationPath,
              object = downloadFileResult$object)
  return(out)
}

#' Purge individual line items from checksums file
#'
#' @inheritParams downloadFile
#' @keywords internal
#' @rdname purge
.purge <- function(checkSums, purge, targetFile, archive, alsoExtract, url) {
  purgeChar <- as.character(purge)
  checkSums <- tryCatch(
    switch(
      purgeChar,
      "2" = checkSums[!(checkSums$expectedFile %in% .basename(targetFile)), ],
      "3" = checkSums[!(checkSums$expectedFile %in% .basename(archive)), ],
      "4" = checkSums[!(checkSums$expectedFile %in% .basename(alsoExtract)), ],
      "5" = checkSums[!(checkSums$expectedFile %in% .basename(unique(c(targetFile, alsoExtract)))), ], #nolint
      "6" = checkSums[!(checkSums$expectedFile %in% .basename(unique(c(targetFile, alsoExtract, archive)))), ], #nolint
      "7" = checkSums[!(checkSums$expectedFile %in% .basename(url)), ] #nolint
    ), error = function(x) checkSums)
  checkSums
}

.emptyChecksumsResult <- data.table::data.table(expectedFile = character(),
                                                actualFile = character(),
                                                result = character())
.emptyChecksumsFileContent <- data.frame(file = character(),
                                         checksum = character(),
                                         filesize = character(),
                                         algorithm = character())

.extractFunction <- function(fun) {
  if (!is.null(fun)) {
    if (!is.function(fun)) {
      if (grepl("::", fun)) {
        fun2 <- strsplit(fun, "::")[[1]]
        pkg <- fun2[1]
        fun <- fun2[2]
        fun <- getFromNamespace(fun, pkg)
      } else {
        fun <- get(fun)
      }
    }
  }
  fun
}

.guessAtFile <- function(url, archive, targetFile, destinationPath) {
  guessedFile <- if (!is.null(url)) {
    if (grepl("drive.google.com", url)) {
      if (url.exists(url)) {
        assessGoogle(url = url, archive = archive,
                     targetFile = targetFile,
                     destinationPath = destinationPath)
      } else {
        # likely offline
        file.path(destinationPath, .basename(url))
      }
    } else {
      file.path(destinationPath, .basename(url))
    }
  } else {
    NULL
  }
  guessedFile
}

.checkSumsUpdate <- function(destinationPath, newFilesToCheck, checkSums,
                             checkSumFilePath = NULL) {
  if (is.null(checkSumFilePath) || length(checkSumFilePath) == 0)
    checkSumFilePath <- file.path(destinationPath, "CHECKSUMS.txt")
  if (!file.exists(checkSumFilePath)) {
    checkSums
  } else {
    suppressMessages(checkSums2 <- try(Checksums(path = destinationPath, write = FALSE,
                                                 files = newFilesToCheck, checksumFile = checkSumFilePath), silent = TRUE))
    if (!is(checkSums2, "try-error")) {
      checkSums <- rbindlist(list(checkSums, checkSums2))
      data.table::setkey(checkSums, result)
      checkSums <- unique(checkSums, fromLast = TRUE, by = "expectedFile")
      checkSums <- rbindlist(list(checkSums[compareNA("OK", result)],
                                  checkSums[compareNA("FAIL", result)],
                                  checkSums[is.na(result)]))
    } else {
      stop("checkSumFilePath is not a CHECKSUMS.txt file")
    }
  }
  checkSums
}

.similarFilesInCheckSums <- function(file, checkSums) {
  if (NROW(checkSums)) {
    anySimilarInCS <- checkSums[grepl(paste0(file_path_sans_ext(file),"\\."),
                                      checkSums$expectedFile),]$result
    if (length(anySimilarInCS)) {
      isTRUE(all(compareNA("OK", anySimilarInCS)))
    } else {
      FALSE
    }
  } else {
    FALSE
  }
}

.checkForSimilar <- function(neededFiles, alsoExtract, archive, targetFile,
                             destinationPath, checkSums, url) {
  lookForSimilar <- if (is.null(alsoExtract)) {
    TRUE
  } else {
    if ("similar" %in% .basename(alsoExtract)) {
      TRUE
    } else {
      FALSE
    }
  }

  if (lookForSimilar) {
    allFiles <- .listFilesInArchive(archive)
    if (is.null(targetFile)) {
      message("No targetFile supplied. ",
              "Extracting all files from archive")
      neededFiles <- allFiles
    } else {
      allOK <- .similarFilesInCheckSums(targetFile, checkSums)
      if (!allOK) {
        filePatternToKeep <- gsub(.basename(targetFile),
                                  pattern = file_ext(.basename(targetFile)), replacement = "")
        filesToGet <- grep(allFiles, pattern = filePatternToKeep, value = TRUE)
        neededFiles <- unique(c(neededFiles, filesToGet))
      }
    }
    rerunChecksums <- TRUE
    if (exists("filesToGet", inherits = FALSE)) {
      if (length(filesToGet) == 0)
        rerunChecksums <- FALSE

    }
    if (!is.null(neededFiles) && rerunChecksums) {
      checkSums <- .checkSumsUpdate(destinationPath = destinationPath, newFilesToCheck = neededFiles,
                                    checkSums = checkSums)
    }
  }
  list(neededFiles = neededFiles, checkSums = checkSums)
}

.checkLocalSources <- function(neededFiles, checkSumFilePath, checkSums, otherPaths, needChecksums,
                               destinationPath) {
  #foundRecursively <- character()
  foundInInputPaths <- character()
  successfulCheckSumFilePath <- character()
  successfulDir <- character()
  if (!is.null(neededFiles)) {

    filesInHand <- checkSums[compareNA(checkSums$result, "OK"),]$expectedFile
    if (!all(neededFiles %in% filesInHand)) {
      for (op in otherPaths) {
        recursively <- if (!is.null(getOption("reproducible.inputPathsRecursive"))) {
          getOption("reproducible.inputPathsRecursive")
        } else {
          FALSE
        }
        opFiles <- dir(op, recursive = recursively, full.names = TRUE)
        if (any(neededFiles %in% .basename(opFiles))) {

          isNeeded <- .basename(opFiles) %in% neededFiles
          dirNameOPFiles <- dirname(opFiles[isNeeded])

          #foundRecursively <- dirNameOPFiles != dirname(opFiles[isNeeded])
          #foundRecursively <- .basename(opFiles[isNeeded][foundRecursively])

          uniqueDirsOPFiles <- rev(unique(dirNameOPFiles))

          checkSumFilePathTry <- checkSumFilePath
          # check CHECKSUMS.txt files, first the one in destinationPath, then ones in inputPaths
          for (dirOPFiles in c(uniqueDirsOPFiles[1], uniqueDirsOPFiles)) {
            #for (i in seq(1 + length(uniqueDirsOPFiles))) {
            suppressMessages(checkSumsInputPath <- Checksums(path = dirOPFiles, write = FALSE,
                                                             files = file.path(dirNameOPFiles, neededFiles),
                                                             checksumFile = checkSumFilePathTry))
            isOK <- checkSumsInputPath[checkSumsInputPath$expectedFile %in% neededFiles, ]$result
            if (length(isOK))
              if (all(compareNA(isOK, "OK"))) {
                needChecksums <- 3 # Basically this means that we *may* have to update
                #   checksums file in either destinationPath or
                #   options("reproducible.inputPaths")
                successfulCheckSumFilePath <- checkSumFilePathTry
                successfulDir <- dirNameOPFiles
                break
              }

            checkSumFilePathTry <- file.path(dirOPFiles, "CHECKSUMS.txt")
          }
          checkSumsIPOnlyNeeded <- checkSumsInputPath[compareNA(checkSumsInputPath$result, "OK"),]
          filesInHandIP <- checkSumsIPOnlyNeeded$expectedFile
          filesInHandIPLogical <- neededFiles %in% filesInHandIP
          if (any(filesInHandIPLogical)) {
            #message("   Copying local copy of ", paste(neededFiles, collapse = ", "), " from ",dirNameOPFiles," to ", destinationPath)
            linkOrCopy(file.path(dirNameOPFiles, filesInHandIP),
                       file.path(destinationPath, filesInHandIP))
            checkSums <- rbindlist(list(checkSumsIPOnlyNeeded, checkSums))
            checkSums <- unique(checkSums, by = "expectedFile")
            # needChecksums <- 2
          }
          foundInInputPaths <- c(foundInInputPaths, filesInHandIP)
          if (isTRUE(all(filesInHandIPLogical))) {
            break
          }
        }
      }
    }
  }
  list(checkSums = checkSums, needChecksums = needChecksums,
       #foundRecursively = foundRecursively,
       successfulCheckSumFilePath = successfulCheckSumFilePath,
       successfulDir = successfulDir,
       foundInInputPaths = foundInInputPaths)
}

#' Hardlink, symlink, or copy a file
#'
#' Attempt first to make a hardlink. If that fails, try to make
#' a symlink (on non-windows systems and \code{symlink = TRUE}).
#' If that fails, copy the file.
#'
#' @note Use caution with files-backed objects (e.g., rasters). See examples.
#'
#' @param from,to  Character vectors, containing file names or paths.
#'                 \code{to} can alternatively be the path to a single existing directory.
#' @param symlink  Logical indicating whether to use symlink (instead of hardlink).
#'                 Default \code{FALSE}.
#'
#' @seealso \code{\link{file.link}}, \code{\link{file.symlink}}, \code{\link{file.copy}}.
#'
#' @author Alex Chubaty and Eliot McIntire
#' @export
#' @examples
#' library(datasets)
#' library(magrittr)
#' library(raster)
#'
#' tmpDir <- file.path(tempdir(), "symlink-test") %>%
#'   normalizePath(winslash = '/', mustWork = FALSE)
#' dir.create(tmpDir)
#'
#' f0 <- file.path(tmpDir, "file0.csv")
#' write.csv(iris, f0)
#'
#' d1 <- file.path(tmpDir, "dir1")
#' dir.create(d1)
#' write.csv(iris, file.path(d1, "file1.csv"))
#'
#' d2 <- file.path(tmpDir, "dir2")
#' dir.create(d2)
#' f2 <- file.path(tmpDir, "file2.csv")
#'
#' ## create link to a file
#' linkOrCopy(f0, f2)
#' file.exists(f2) ## TRUE
#' identical(read.table(f0), read.table(f2)) ## TRUE
#'
#' ## deleting the link shouldn't delete the original file
#' unlink(f0)
#' file.exists(f0) ## FALSE
#' file.exists(f2) ## TRUE
#'
#' ## using rasters and other file-backed objects
#' f3a <- system.file("external/test.grd", package = "raster")
#' f3b <- system.file("external/test.gri", package = "raster")
#' r3a <- raster(f3a)
#' f4a <- file.path(tmpDir, "raster4.grd")
#' f4b <- file.path(tmpDir, "raster4.gri")
#' linkOrCopy(f3a, f4a) ## hardlink
#' linkOrCopy(f3b, f4b) ## hardlink
#' r4a <- raster(f4a)
#'
#' isTRUE(all.equal(r3a, r4a)) # TRUE
#'
#' ## cleanup
#' unlink(tmpDir, recursive = TRUE)
linkOrCopy <- function (from, to, symlink = TRUE) {
  existsLogical <- file.exists(from)
  toCollapsed <- paste(to, collapse = ", ")
  fromCollapsed <- paste(from, collapse = ", ")
  if (any(existsLogical)) {
    toDirs <- unique(dirname(to))
    dirDoesntExist <- !dir.exists(toDirs)
    if (any(dirDoesntExist)) {
      lapply(toDirs[dirDoesntExist], dir.create)
    }
    dups <- duplicated(.basename(from))

    # Try hard link first -- the only type that R deeply recognizes
    warns <- capture_warnings(result <- file.link(from[!dups], to))
    if (isTRUE(all(result))) {
      message("Hardlinked version of file created at: ", toCollapsed, ", which points to "
              , fromCollapsed, "; no copy was made.")
    }

    if (any(grepl("file already exists", warns))) {
      message("File named ", toCollapsed, " already exists; will try to use it/them")
      result <- TRUE
    }

    # On *nix types -- try symlink
    if (isFALSE(all(result)) && isTRUE(symlink)) {
      if (!identical(.Platform$OS.type, "windows")) {
        result <- suppressWarnings(file.symlink(from, to))
        if (isTRUE(all(result))) {
          message("Symlinked version of file created at: ", toCollapsed, ", which points to "
                  , fromCollapsed, "; no copy was made.")
        }
      }
    }

    if (isFALSE(all(result))) {
      result <- file.copy(from, to)
      message("Copy of file: ", fromCollapsed, ", was created at: ", toCollapsed)
    }
  } else {
    message("File ", fromCollapsed, " does not exist. Not copying.")
    result <- FALSE
  }
  return(result)
}


.tryExtractFromArchive <- function(archive,
                                   neededFiles,
                                   filesToChecksum,
                                   alsoExtract,
                                   destinationPath,
                                   checkSums,
                                   needChecksums,
                                   checkSumFilePath,
                                   targetFile,
                                   quick) {
  neededFiles <- unique(c(neededFiles, if (!is.null(alsoExtract)) .basename(alsoExtract)))
  neededFiles <- setdiff(neededFiles, "similar") # remove "similar" from needed files. It is for extracting.

  filesExtr <- NULL
  if (!is.null(archive)) {
    if (any(file.exists(archive))) {
      filesExtracted <- extractFromArchive(archive = archive, destinationPath = destinationPath,
                                           neededFiles = neededFiles,
                                           checkSums = checkSums, needChecksums = needChecksums,
                                           checkSumFilePath = checkSumFilePath, quick = quick)

      checkSums <- .checkSumsUpdate(destinationPath = destinationPath,
                                    newFilesToCheck = .basename(filesExtracted$filesExtr),
                                    checkSums = filesExtracted$checkSums)

      filesToChecksum <- unique(c(filesToChecksum, targetFile, alsoExtract,
                                  .basename(filesExtracted$filesExtr)))
      needChecksums <- filesExtracted$needChecksums
      data.table::setDT(filesExtracted$checkSums)
      dontNeedChecksums <- filesExtracted$checkSums[filesExtracted$checkSums$expectedFile %in%
                                                      filesToChecksum & compareNA(result, "OK"), expectedFile]
      filesToChecksum <- setdiff(filesToChecksum, dontNeedChecksums)

      if (needChecksums > 0) {
        checkSums <- appendChecksumsTable(
          checkSumFilePath = checkSumFilePath,
          filesToChecksum = unique(.basename(filesToChecksum)),
          destinationPath = destinationPath,
          append = needChecksums >= 2
        )
        needChecksums <- 0
      }

      ## targetFilePath might still be NULL, need destinationPath too
      filesExtr <- c(filesToChecksum,
                     if (is.null(filesExtracted$filesExtr) ||
                         length(filesExtracted$filesExtr) == 0)
                       character() #downloadFileResult$downloaded
                     else
                       filesExtracted$filesExtr)
    }
  }
  if (!is.null(filesExtr)) {
    #filesExtrTemp <- filesExtr # keep this non-uniqued version... contains full paths
    filesExtr <- unique(.basename(filesExtr))
  }
  list(filesToChecksum = filesToChecksum, filesExtr = filesExtr,
       needChecksums = needChecksums,
       neededFiles = neededFiles, checkSums = checkSums)
}

.basename <- function(x) {
  if (is.null(x)) {
    NULL
  } else {
    basename(x)
  }
}
