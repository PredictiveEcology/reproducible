#' @param n Number of non-null arguments passed to \code{preProcess}.
#' E.g., passing \code{n = 1} returns combinations with only a single non-NULL parameter.
#' If \code{NULL} (default), all parameter combinations are returned.
#'
#' @export
#' @rdname preProcess
preProcessParams <- function(n = NULL) {
  p1 <- data.frame(
    url = c("char", "NULL", "NULL", "NULL"),
    targetFile = c("NULL", "char", "NULL", "NULL"),
    archive = c("NULL", "NULL", "char", "NULL"),
    alsoExtract = c("NULL", "NULL", "NULL", "char"),
    Result = c("Download, extract all files if an archive, guess at 'targetFile', load into R.",
               "Load 'targetFile' into R.",
               "Extract all files, guess at 'targetFile', load into R.",
               "Guess at 'targetFile' from files in 'alsoExtract', load into R."),
    FirstChecksum = c("Write or append all new files.",
                      "Write or append 'targetFile'.",
                      "Write or append all new files.",
                      "Write or append all new files."),
    SecondChecksum = c("Same as first; no 'targetFile'.*",
                       "No downloading, so no checksums used.",
                       "No downloading, so no checksums used.",
                       "No downloading, so no checksums used."),
    stringsAsFactors = FALSE
  )
  p2 <- data.frame(
    url = c("char", "char", "char", "NULL", "NULL", "NULL"),
    targetFile = c("char", "NULL", "NULL", "char", "char", "NULL"),
    archive = c("NULL", "char", "NULL", "NULL", "char", "char"),
    alsoExtract = c("NULL", "NULL", "char", "char", "NULL", "char"),
    Result = c("Download, extract all files if an archive, load 'targetFile' into R.",
               "Download, extract all files, guess at 'targetFile', load into R.",
               "Download, extract only named files in 'alsoExtract', guess at 'targetFile', load into R.",
               "Load 'targetFile' into R.",
               "Extract all files, load 'targetFile' into R.",
               " Extract only named files in 'alsoExtract', guess at 'targetFile', load into R."),
    FirstChecksum = c("Write or append all new files.",
                      "Write or append all new files.",
                      "Write or append all new files.",
                      "Write or append all new files.",
                      "Write or append all new files.",
                      "Write or append all new files."),
    SecondChecksum = c("Use Checksums, skip downloading.",
                       "Same as first; no 'targetFile'.*",
                       "Same as first; no 'targetFile'.*",
                       "No downloading, so no checksums used.",
                       "No downloading, so no checksums used.",
                       "No downloading, so no checksums used."),
    stringsAsFactors = FALSE
  )
  p3 <- data.frame(
    url = c("char", "char", "char", "char", "char", "char", "NULL"),
    targetFile = c("char", "NULL", "NULL", "char", "char", "char", "char"),
    archive = c("char", "char", "char", "NULL", "NULL", "char", "char"),
    alsoExtract = c("NULL", "char", "'similar'", "char", "'similar", "NULL", "char"),
    Result = c("Download, extract all files, load 'targetFile' into R.",
               "Download, extract files named in 'alsoExtract', guess at 'targetFile', load into R.",
               "Download, extract all files (can't understand \"similar\"), guess at 'targetFile', load into R.",
               "Download, if an archive, extract files named in 'targetFile' and 'alsoExtract', load 'targetFile' into R.",
               "Download, if an archive, extract files with same base as 'targetFile', load 'targetFile' into R.",
               "Download, extract all files from archive, load 'targetFile' into R.",
               "Extract  files named in 'alsoExtract' from archive, load 'targetFile' into R."),
    FirstChecksum = c("Write or append all new files.",
                      "Write or append all new files.",
                      "Write or append all new files.",
                      "Write or append all new files.",
                      "Write or append all new files.",
                      "Write or append all new files.",
                      "Write or append all new files."),
    SecondChecksum = c("Use Checksums, skip downloading.",
                       "Use Checksums, skip downloading.",
                       "Same as first; no 'targetFile'.",
                       "Use Checksums, skip downloading.",
                       "Use Checksums, skip downloading.",
                       "Use Checksums, skip downloading.",
                       "No downloading, so no checksums used."),
    stringsAsFactors = FALSE
  )
  p4 <- data.frame(
    url = c("char", "char"),
    targetFile = c("char", "char"),
    archive = c("char", "char"),
    alsoExtract = c("char", "'similar'"),
    Result = c("Download, extract files named in 'targetFile' and 'alsoExtract', load 'targetFile' into R.",
               "Download, extract all files with same basename as 'targetFile', load 'targetFile' into R."),
    FirstChecksum = c("Write or append all new files.",
                      "Write or append all new files."),
    SecondChecksum = c("Use Checksums, skip downloading.",
                       "Use Checksums, skip downloading."),
    stringsAsFactors = FALSE
  )

  if (is.null(n)) {
    rbind(p1, p2, p3, p4)
  } else if (n == 1) {
    p1
  } else if (n == 2) {
    p2
  } else if (n == 3) {
    p3
  } else if (n == 4) {
    p4
  }
}

#' Download, Checksum, Extract files
#'
#' This does downloading (via \code{downloadFile}), checksumming (\code{Checksums}),
#' and extracting from archives (\code{extractFromArchive}), plus cleaning up of input
#' arguments (e.g., paths, function names).
#' This is the first stage of three used in \code{prepInputs}.
#'
#' @return
#' A list with 5 elements: \code{checkSums} (the result of a \code{Checksums}
#' after downloading), \code{dots} (cleaned up \code{...}, including deprecated argument checks),
#' \code{fun} (the function to be used to load the \code{preProcess}ed object from disk),
#' and \code{targetFilePath} (the fully qualified path to the \code{targetFile}).
#'
#' @section Combinations of \code{targetFile}, \code{url}, \code{archive}, \code{alsoExtract}:
#'
#'   Use \code{preProcessParams()} for a table describing various parameter combinations and their
#'   outcomes.
#'
#'  \code{*} If the \code{url} is a file on Google Drive, checksumming will work
#'  even without a \code{targetFile} specified because there is an initial attempt
#'  to get the remove file information (e.g., file name). With that, the connection
#'  between the \code{url} and the filename used in the \file{CHECKSUMS.txt} file can be made.
#'
#' @inheritParams prepInputs
#' @inheritParams downloadFile
#'
#' @author Eliot McIntire
#' @export
#' @importFrom data.table fread setDT
preProcess <- function(targetFile = NULL, url = NULL, archive = NULL, alsoExtract = NULL,
                       destinationPath = getOption("reproducible.destinationPath", "."),
                       fun = NULL, dlFun = NULL,
                       quick = getOption("reproducible.quick"),
                       overwrite = getOption("reproducible.overwrite", FALSE),
                       purge = FALSE,
                       useCache = getOption("reproducible.useCache", FALSE),
                       verbose = getOption("reproducible.verbose", 1),
                       .tempPath, ...) {
  if (missing(.tempPath)) {
    .tempPath <- tempdir2(rndstr(1, 6))
    on.exit({unlink(.tempPath, recursive = TRUE)},
            add = TRUE)
  }
  dots <- list(...)

  fun <- .checkFunInDots(fun = fun, dots = dots)
  dots <- .checkDeprecated(dots, verbose = verbose)

  # remove trailing slash -- causes unzip fail if it is there
  destinationPath <- gsub("\\\\$|/$", "", destinationPath)
  checkSumFilePath <- file.path(destinationPath, "CHECKSUMS.txt")

  if (is.null(targetFile)) {
    fileGuess <- .guessAtFile(url = url, archive = archive, targetFile = targetFile,
                              destinationPath = destinationPath, verbose = verbose,
                              team_drive = dots[["team_drive"]])
    if (is.null(archive))
      archive <- .isArchive(fileGuess)
    archive <- moveAttributes(fileGuess, archive)
    if (is.null(archive) && !is.null(fileGuess)) {
      messagePrepInputs("targetFile was not supplied; guessed and will try ", fileGuess,
              ". If this is incorrect, please supply targetFile", verbose = verbose)
      targetFile <- .basename(fileGuess)
      targetFilePath <- file.path(destinationPath, targetFile)
    } else {
      targetFilePath <- NULL
    }
  } else {
    if (length(targetFile) > 1)
      stop("targetFile should be only 1 file")
    targetFile <- .basename(targetFile)
    targetFilePath <- file.path(destinationPath, targetFile)
    if (is.null(alsoExtract)) {
      if (file.exists(checkSumFilePath)) {
        if (file.size(checkSumFilePath) > 0) {
          # if alsoExtract is not specified, then try to find all files in CHECKSUMS.txt with
          # same base name, without extension
          checksumsTmp <- as.data.table(read.table(checkSumFilePath))
          alsoExtract <- grep(paste0(filePathSansExt(targetFile),"\\."), checksumsTmp$file,
                              value = TRUE)
          rm(checksumsTmp) # clean up
        }
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
    # if (!identical(file.exists(destinationPath), isFile(destinationPath))) stop("isFile is not same as file.exists")
    if (isFile(destinationPath)) {
      stop("destinationPath must be a directory")
    }
    checkPath(destinationPath, create = TRUE)
  }

  messagePrepInputs("Preparing: ", targetFile, verbose = verbose)

  needChecksums <- 0

  filesToCheck <- c(targetFilePath, alsoExtract)
  if (!is.null(archive)) {
    tmpArchive <- archive
    archive <- file.path(destinationPath, .basename(archive))
    filesToCheck <- unique(c(filesToCheck, archive))
    archive <- moveAttributes(tmpArchive, archive)
  }

  # Need to run checksums on all files in destinationPath because we may not know what files we
  #   want if targetFile, archive, alsoExtract not specified
  reproducible.inputPaths <- getOption("reproducible.inputPaths", NULL)
  if (!is.null(reproducible.inputPaths))
    reproducible.inputPaths <- checkPath(reproducible.inputPaths, create = TRUE)
  if (!is.null(reproducible.inputPaths))
    reproducible.inputPaths <- path.expand(reproducible.inputPaths)

  for (dp in unique(c(destinationPath, reproducible.inputPaths))) {
    checkSumsTmp1 <- try(Checksums(path = dp, write = FALSE, checksumFile = checkSumFilePath,
                               files = basename2(filesToCheck),
                               verbose = verbose), silent = TRUE)
    if (!is(checkSumsTmp1, "try-error")) {
      checkSums <- checkSumsTmp1
      if (!all(is.na(checkSums$result))) { # found something
        if (identical(dp, reproducible.inputPaths)) {
          destinationPathUser <- destinationPath
          destinationPath <- dp
          on.exit({destinationPath <- destinationPathUser}, add = TRUE)
        }
        break
      }
    }
  }

  if (is(checkSums, "try-error")) {
    needChecksums <- 1
    checkSums <- .emptyChecksumsResult
  }
  # browser(expr = exists("._preProcess_5"))

  # This will populate a NULL archive if archive is local or
  if (is.null(archive)) {
    if (!is.null(url)) {
      allOK <- .similarFilesInCheckSums(targetFile, checkSums)

      if (!allOK) { # skip identification of archive if we have all files with same basename as targetFile
        # BUT if we don't have all files with identical root name (basename sans ext), then assess for
        #   an archive, either remotely, in the case of google or from the basename of url
        fileGuess <- .guessAtFile(url = url, archive = archive,
                                  targetFile = targetFile, destinationPath = destinationPath,
                                  verbose = verbose, team_drive = dots[["team_drive"]])
        archive <- .isArchive(fileGuess)
        # The fileGuess MAY have a fileSize attribute, which can be attached to "archive"
        archive <- moveAttributes(fileGuess, receiving = archive)
        sourceAttributes <- attributes(fileGuess)
        if (length(sourceAttributes) > 0 && !is.null(archive)) {
          for (i in length(sourceAttributes))
            setattr(archive, names(sourceAttributes)[i], sourceAttributes[[i]])
        }

        checkSums <- .checkSumsUpdate(destinationPath = destinationPath,
                                      newFilesToCheck = archive,
                                      checkSums = checkSums,
                                      verbose = verbose)
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
    if (isTRUE(length(filesInsideArchive) > 0)) {
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

  # browser(expr = exists("._preProcess_6"))

  filesToChecksum <- if (is.null(archive))  NULL else .basename(archive)
  isOK <- .compareChecksumsAndFiles(checkSums, c(filesToChecksum, neededFiles))
  if (isTRUE(!all(isOK))) {
    results <- .tryExtractFromArchive(archive = archive, neededFiles = neededFiles,
                                      alsoExtract = alsoExtract, destinationPath = dp,
                                      checkSums = checkSums, needChecksums = needChecksums,
                                      checkSumFilePath = checkSumFilePath,
                                      filesToChecksum = filesToChecksum,
                                      targetFile = targetFile, quick = quick,
                                      verbose = verbose, .tempPath = .tempPath)

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

  # Check for local copies in all values of reproducible.inputPaths
  # At the end of this function, the files will be present in destinationPath, if they existed
  #  in options("reproducible.inputPaths")
  localChecks <- .checkLocalSources(neededFiles, checkSums = checkSums,
                                    checkSumFilePath = checkSumFilePath,
                                    otherPaths = reproducible.inputPaths,
                                    destinationPath, needChecksums = needChecksums, verbose = verbose)
  checkSums <- localChecks$checkSums
  needChecksums <- localChecks$needChecksums
  successfulCheckSumFilePath <- localChecks$successfulCheckSumFilePath
  successfulDir <- localChecks$successfulDir

  # Change the destinationPath to the reproducible.inputPaths temporarily, so
  #   download happens there. Later it will be linked to the user destinationPath
  if (!is.null(reproducible.inputPaths)) {
    # may already have been changed above
    if (!exists("destinationPathUser", inherits = FALSE))
      destinationPathUser <- destinationPath
    on.exit({
      destinationPath <- destinationPathUser
    }, add = TRUE)
    if (!identical(destinationPath, reproducible.inputPaths)) {
      destinationPath <- reproducible.inputPaths[1]
    }

    if (isTRUE(any(grepl(archive, pattern = destinationPathUser)))) {
      # might have a "." as destinationPath -- messes with grepl
      patt <- if (grepl("^\\.", destinationPathUser))
        gsub("^\\.", "^\\\\.", destinationPathUser)
      else
        destinationPathUser
      archive <- gsub(archive, pattern = patt, replacement = destinationPath)
    }
  }

  ###############################################################
  # Download
  ###############################################################
  # browser(expr = exists("._preProcess_7"))
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
    purge = purge, # may need to try purging again if no target,
                   #    archive or alsoExtract were known yet
    verbose = verbose,
    .tempPath = .tempPath,
    ...
  )

  downloadFileResult <- .fixNoFileExtension(downloadFileResult = downloadFileResult,
                                            targetFile = targetFile, archive = archive,
                                            destinationPath = destinationPath, verbose = verbose)

  # Post downloadFile -- put objects into this environment
  if (!is.null(downloadFileResult$targetFilePath))
    targetFilePath <- file.path(normPath(destinationPath), downloadFileResult$neededFiles)
  checkSums <- downloadFileResult$checkSums
  needChecksums <- downloadFileResult$needChecksums
  neededFiles <- downloadFileResult$neededFiles
  # If the download was of an archive, then it is possible the archive path is wrong
  if (identical(.basename(downloadFileResult$downloaded),
                .basename(downloadFileResult$archive)))
    archive <- downloadFileResult$downloaded
  # archive specified, alsoExtract is NULL --> now means will extract all
  if (is.null(archive)) archive <- downloadFileResult$archive

  ###############################################################
  # redo "similar" after download
  ###############################################################
  outFromSimilar <- .checkForSimilar(neededFiles = neededFiles, alsoExtract = alsoExtract,
                                     archive = archive, targetFile = targetFile,
                                     destinationPath = destinationPath, checkSums = checkSums,
                                     url =  url)
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
    needChecksums <- 0
  }, add = TRUE)

  # Stage 1 - Extract from archive
  isOK <- .compareChecksumsAndFiles(checkSums, c(filesToChecksum, neededFiles))
  if (isTRUE(!all(isOK))) {
    filesExtracted <- .tryExtractFromArchive(archive = archive,
                                             neededFiles = neededFiles,
                                             alsoExtract = alsoExtract,
                                             destinationPath = destinationPath,
                                             checkSums = checkSums,
                                             needChecksums = needChecksums,
                                             checkSumFilePath = checkSumFilePath,
                                             filesToChecksum = filesToChecksum,
                                             targetFile = targetFile,
                                             quick = quick, verbose = verbose,
                                             .tempPath = .tempPath)

    filesExtr <- filesExtracted$filesExtr
    filesToChecksum <- filesExtracted$filesToChecksum
    needChecksums <- filesExtracted$needChecksums
    checkSums <- filesExtracted$checkSums
  } else {
    if (!is.null(.isArchive(archive)))
      messagePrepInputs("  Skipping extractFromArchive attempt: no files missing", verbose = verbose)

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
  if (!is.null(reproducible.inputPaths)) {
    #foundRecursively <- localChecks$foundRecursively
    foundInInputPaths <- localChecks$foundInInputPaths
    copyToIP <- (!filesExtr %in% foundInInputPaths)
    # Make sure they are all in options("reproducible.inputPaths"), accounting for
    #   the fact that some may have been in sub-folders -- i.e., don't deal with these
    if (isTRUE(any(copyToIP))) {
      logicalFilesExistIP <- file.exists(file.path(destinationPath, filesExtr[copyToIP]))
      if (!isTRUE(all(logicalFilesExistIP))) {
        outHLC <- hardLinkOrCopy(file.path(destinationPathUser, filesExtr[!logicalFilesExistIP]),
                       file.path(destinationPath, filesExtr[!logicalFilesExistIP]))
        # linkOrCopy(file.path(destinationPathUser, filesExtr[!logicalFilesExistIP]),
        #             file.path(destinationPath, filesExtr[!logicalFilesExistIP]), verbose = verbose)
      }
    }

    # Now make sure all are in original destinationPath
    logicalFilesExistDP <- file.exists(file.path(destinationPathUser, filesExtr))
    if (!isTRUE(all(logicalFilesExistDP))) {
      outHLC <- hardLinkOrCopy(file.path(destinationPath, filesExtr[!logicalFilesExistDP]),
                     file.path(destinationPathUser, filesExtr[!logicalFilesExistDP]))
      # linkOrCopy(file.path(destinationPath, filesExtr[!logicalFilesExistDP]),
      #            file.path(destinationPathUser, filesExtr[!logicalFilesExistDP]), verbose = verbose)
    }
  }
  # if it was a nested file
  # browser(expr = exists("._preProcess_8"))

  if (any(fileExt(neededFiles) %in% c("zip", "tar", "rar"))) {
    nestedArchives <- .basename(neededFiles[fileExt(neededFiles) %in% c("zip", "tar", "rar")])
    nestedArchives <- normPath(file.path(destinationPath, nestedArchives[1]))
    messagePrepInputs("There are still archives in the extracted files.",
            " preProcess will try to extract the files from ", .basename(nestedArchives), ".",
            " If this is incorrect, please supply archive.", verbose = verbose)
    # Guess which files inside the new nested
    nestedTargetFile <- .listFilesInArchive(archive = nestedArchives)
    outFromSimilar <- .checkForSimilar(alsoExtract = alsoExtract,
                                       archive = nestedArchives,
                                       neededFiles = nestedTargetFile,
                                       destinationPath = destinationPath,
                                       checkSums = checkSums,
                                       targetFile = targetFile)
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
      needChecksums <- 0
    }, add = TRUE)
    extractedFiles <- .tryExtractFromArchive(archive = nestedArchives,
                                             neededFiles = neededFiles,
                                             alsoExtract = alsoExtract,
                                             destinationPath = destinationPath,
                                             checkSums = checkSums,
                                             needChecksums = needChecksums,
                                             checkSumFilePath = checkSumFilePath,
                                             filesToChecksum = filesToChecksum,
                                             targetFile = targetFile,
                                             quick = quick,
                                             verbose = verbose,
                                             .tempPath = .tempPath)
    filesExtr <- c(filesExtr, extractedFiles$filesExtr)
  }
  targetParams <- .guessAtTargetAndFun(targetFilePath, destinationPath,
                                       filesExtracted = filesExtr,
                                       fun, verbose = verbose) # passes through if all known
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
  # browser(expr = exists("._preProcess_9"))

  ## Convert the fun as character string to function class, if not already
  fun <- .extractFunction(fun)

  if (!is.null(reproducible.inputPaths)) {
    destinationPath <- destinationPathUser
  }
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
    }
    checkSums <- appendChecksumsTable(
      checkSumFilePath = checkSumFilePath,
      filesToChecksum = unique(.basename(filesToChecksum)),
      destinationPath = destinationPath,
      append = needChecksums >= 2
    )
    if (!is.null(reproducible.inputPaths) && needChecksums != 3) {
      checkSumFilePathInputPaths <- file.path(reproducible.inputPaths[[1]], "CHECKSUMS.txt")
      suppressMessages({
        checkSums <- appendChecksumsTable(
          checkSumFilePath = checkSumFilePathInputPaths,
          filesToChecksum = unique(.basename(filesToChecksum)),
          destinationPath = destinationPath,
          append = needChecksums == 2
        )
      })
    }
    on.exit({
      needChecksums <- 0
    }, add = TRUE, after = FALSE) # effectively remove appendChecksums in other
                                  # on.exit because it is done here
  }

  # browser(expr = exists("._preProcess_10"))
  failStop <- if (is.null(targetFilePath)) {
    TRUE
  } else if (!isTRUE(file.exists(targetFilePath))) {
    TRUE
  } else {
    FALSE
  }
  if (isTRUE(failStop))
    stop("targetFile appears to be misspecified at: ", targetFilePath, ". ",
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

#' @keywords internal
.emptyChecksumsResult <- data.table::data.table(expectedFile = character(),
                                                actualFile = character(),
                                                result = character())

#' @keywords internal
.emptyChecksumsFileContent <- data.frame(file = character(),
                                         checksum = character(),
                                         filesize = character(),
                                         algorithm = character())

#' @keywords internal
.extractFunction <- function(fun) {
  if (!is.null(fun)) {
    suppressWarnings(isNAFun <- is.na(fun))
    if (!isNAFun) {
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

  }
  fun
}

#' @keywords internal
.guessAtFile <- function(url, archive, targetFile, destinationPath,
                         verbose = getOption("reproducible.verbose", 1), team_drive = NULL) {
  guessedFile <- if (!is.null(url)) {
    if (grepl("drive.google.com", url)) {
      ie <- isTRUE(internetExists())
      if (ie) {
        assessGoogle(url = url, archive = archive, targetFile = targetFile,
                     destinationPath = destinationPath, verbose = verbose, team_drive = NULL)
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

#' @keywords internal
.checkSumsUpdate <- function(destinationPath, newFilesToCheck, checkSums,
                             checkSumFilePath = NULL, verbose = getOption("reproducible.verbose", 1)) {
  if (!is.null(newFilesToCheck)) {
    if (is.null(checkSumFilePath) || length(checkSumFilePath) == 0)
      checkSumFilePath <- file.path(destinationPath, "CHECKSUMS.txt")
    if (!file.exists(checkSumFilePath)) {
      checkSums
    } else {
      checkSums2 <- suppressMessages(try(Checksums(path = destinationPath, write = FALSE,
                                                   files = newFilesToCheck,
                                                   checksumFile = checkSumFilePath,
                                                   verbose = verbose), silent = TRUE))
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
  }
  checkSums
}

#' @keywords internal
.similarFilesInCheckSums <- function(file, checkSums) {
  if (NROW(checkSums)) {
    anySimilarInCS <- checkSums[grepl(paste0(filePathSansExt(file),"\\."),
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

#' @keywords internal
.checkForSimilar <- function(neededFiles, alsoExtract, archive, targetFile,
                             destinationPath, checkSums, url, verbose = getOption("reproducible.verbose", 1)) {
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
      messagePrepInputs("No targetFile supplied. ",
              "Extracting all files from archive", verbose = verbose)
      neededFiles <- allFiles
    } else {
      allOK <- .similarFilesInCheckSums(targetFile, checkSums)
      if (!allOK) {
        filePatternToKeep <- gsub(.basename(targetFile),
                                  pattern = fileExt(.basename(targetFile)), replacement = "")
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
                                    checkSums = checkSums, verbose = verbose)
    }
  }
  list(neededFiles = neededFiles, checkSums = checkSums)
}

#' @keywords internal
.checkLocalSources <- function(neededFiles, checkSumFilePath, checkSums, otherPaths, needChecksums,
                               destinationPath, verbose = getOption("reproducible.verbose", 1)) {
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
            checkSumsInputPath <- suppressMessages(
              Checksums(path = dirOPFiles, write = FALSE,
                        files = file.path(dirNameOPFiles, neededFiles),
                        checksumFile = checkSumFilePathTry,
                        verbose = verbose)
            )
            isOK <- checkSumsInputPath[checkSumsInputPath$expectedFile %in% neededFiles, ]$result
            if (length(isOK))
              if (all(compareNA(isOK, "OK"))) {
                needChecksums <- 3 # Basically this means that we *may* have to update
                #   checksums file in either destinationPath or
                #   options("reproducible.inputPaths")
                successfulCheckSumFilePath <- checkSumFilePathTry
                successfulDir <- unique(dirNameOPFiles)
                break
              }

            checkSumFilePathTry <- file.path(dirOPFiles, "CHECKSUMS.txt")
          }
          checkSumsIPOnlyNeeded <- checkSumsInputPath[compareNA(checkSumsInputPath$result, "OK"), ]
          filesInHandIP <- checkSumsIPOnlyNeeded$expectedFile
          filesInHandIPLogical <- neededFiles %in% filesInHandIP
          if (any(filesInHandIPLogical)) {
            outHLC <- hardLinkOrCopy(file.path(dirNameOPFiles, filesInHandIP),
                          file.path(destinationPath, filesInHandIP))
            checkSums <- rbindlist(list(checkSumsIPOnlyNeeded, checkSums))
            checkSums <- unique(checkSums, by = "expectedFile")
          }
          foundInInputPaths <- c(foundInInputPaths, filesInHandIP)
          if (isTRUE(all(filesInHandIPLogical))) {
            break
          }
        }
      }
    }
    # do a check here that destinationPath is already the inputPaths
    #   need to emulate the above behaviour
    reproducible.inputPaths <- getOption("reproducible.inputPaths", NULL)
    if (!is.null(reproducible.inputPaths))
      reproducible.inputPaths <- path.expand(reproducible.inputPaths)

    if (identical(destinationPath, reproducible.inputPaths)) {
      foundInInputPaths <- filesInHand
      successfulDir <- destinationPath
      successfulCheckSumFilePath <- checkSumFilePath
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
#' @inheritParams prepInputs
#' @seealso \code{\link{file.link}}, \code{\link{file.symlink}}, \code{\link{file.copy}}.
#'
#' @author Alex Chubaty and Eliot McIntire
#' @export
#'
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
linkOrCopy <- function(from, to, symlink = TRUE, verbose = getOption("reproducible.verbose", 1)) {
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
    result <-  captureWarningsToAttr(
      file.link(from[!dups], to)
    )
    warns <- attr(result, "warning")
    attr(result, "warning") <- NULL

    if (isTRUE(all(result))) {
      messagePrepInputs(hardlinkMessagePrefix, ": ", toCollapsed, ", ",whPointsToMess," "
              , fromCollapsed, "; no copy was made.", verbose = verbose)
    }

    if (any(grepl("file already exists", warns))) {
      messagePrepInputs("File named ", toCollapsed, " already exists; will try to use it/them", verbose = verbose)
      result <- TRUE
    }

    # On *nix types -- try symlink
    if (.isFALSE(all(result)) && isTRUE(symlink)) {
      if (!isWindows()) {
        result <- suppressWarnings(file.symlink(from, to))
        if (isTRUE(all(result))) {
          messagePrepInputs("Symlinked version of file created at: ", toCollapsed, ", ",whPointsToMess," ",
                  fromCollapsed, "; no copy was made.", verbose = verbose)
        }
      }
    }

    if (.isFALSE(all(result))) {
      result <- file.copy(from, to)
      messagePrepInputs("Copy of file: ", fromCollapsed, ", was created at: ", toCollapsed, verbose = verbose)
    }
  } else {
    messagePrepInputs("File ", fromCollapsed, " does not exist. Not copying.", verbose = verbose)
    result <- FALSE
  }
  return(result)
}

#' @keywords internal
.tryExtractFromArchive <- function(archive,
                                   neededFiles,
                                   filesToChecksum,
                                   alsoExtract,
                                   destinationPath,
                                   checkSums,
                                   needChecksums,
                                   checkSumFilePath,
                                   targetFile,
                                   quick, verbose = getOption("reproducible.verbose", 1),
                                   .tempPath) {
  if (missing(.tempPath)) {
    .tempPath <- tempdir2(rndstr(1, 6))
    on.exit({unlink(.tempPath, recursive = TRUE)},
            add = TRUE)
  }
  neededFiles <- unique(c(neededFiles, if (!is.null(alsoExtract)) .basename(alsoExtract)))
  neededFiles <- setdiff(neededFiles, "similar") # remove "similar" from neededFiles; for extracting

  filesExtr <- NULL
  if (!is.null(archive)) {
    if (any(file.exists(archive))) {
      filesExtracted <- extractFromArchive(archive = archive, destinationPath = destinationPath,
                                           neededFiles = neededFiles,
                                           checkSums = checkSums, needChecksums = needChecksums,
                                           checkSumFilePath = checkSumFilePath, quick = quick,
                                           verbose = verbose,
                                           .tempPath = .tempPath)

      checkSums <- .checkSumsUpdate(destinationPath = destinationPath,
                                    newFilesToCheck = .basename(filesExtracted$filesExtracted),
                                    checkSums = filesExtracted$checkSums, verbose = verbose)

      filesToChecksum <- unique(c(filesToChecksum, targetFile, alsoExtract,
                                  .basename(filesExtracted$filesExtr)))
      needChecksums <- filesExtracted$needChecksums
      data.table::setDT(filesExtracted$checkSums)
      dontNeedChecksums <- filesExtracted$checkSums[filesExtracted$checkSums$expectedFile %in%
                                                      filesToChecksum & compareNA(result, "OK"),
                                                    expectedFile]
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

#' @keywords internal
.basename <- function(x) {
  if (is.null(x)) {
    NULL
  } else {
    basename(x)
  }
}

#' @keywords internal
.decodeMagicNumber <- function(magicNumberString) {
  fileExt <- if (any(grepl(pattern = "Zip", x = magicNumberString))) ".zip" else
    if (any(grepl(pattern = "RAR", x = magicNumberString))) ".rar" else
      if (any(grepl(pattern = "tar", x = magicNumberString))) ".tar" else
        if (any(grepl(pattern = "TIFF", x = magicNumberString))) ".tif" else
          if (any(grepl(pattern = "Shapefile", x = magicNumberString))) ".shp" else
            NULL
  return(fileExt)
}

#' @keywords internal
.guessFileExtension <- function(file) {
  if (isWindows()) {
    tryCatch({
      possLocs <- c("C:/cygwin/bin/file.exe",
                    "C:\\cygwin64/bin/file.exe")
      findFile <- file.exists(possLocs)
      if (any(findFile))
        fileLoc <- possLocs[findFile][1]

      magicNumber <- captureWarningsToAttr(
        system(paste(fileLoc, file), intern = TRUE)
      )
      warn <- attr(magicNumber, "warning")
      attr(magicNumber, "warning") <- NULL

      if (length(warn) > 0) {
        splitted <- unlist(strsplit(x = file, split = ":/"))
        fileAdapted <- file.path(paste0("/mnt/", tolower(splitted[1])), splitted[2])
        magicNumber <- captureWarningsToAttr(
          shell(paste0("'file ", fileAdapted, "'"), "bash", intern = TRUE,
                wait = TRUE, translate = FALSE, mustWork = TRUE)
        )
        warn <- attr(magicNumber, "warning")
        attr(magicNumber, "warning") <- NULL

      }
      fileExt <- if (length(warn) == 0) {
        .decodeMagicNumber(magicNumberString = magicNumber)
      } else {
        NULL
      }
      return(fileExt)
    }, error = function(e){
      fileExt <- NULL
      return(fileExt)
      })
  } else {
    magicNumber  <- system(paste0("file ", file), wait = TRUE, intern = TRUE)
    fileExt <- .decodeMagicNumber(magicNumberString = magicNumber)
    return(fileExt)
  }
}

#' @keywords internal
.fixNoFileExtension <- function(downloadFileResult, targetFile, archive,
                                destinationPath, verbose = getOption("reproducible.verbose", 1)) {
  if (!is.null(downloadFileResult$downloaded) &&
      identical(fileExt(normPath(.basename(downloadFileResult$downloaded))), "")) {
    if (!is.null(targetFile) && !identical(fileExt(normPath(.basename(downloadFileResult$neededFiles))), "")) {
      if (is.null(archive)) {
        messagePrepInputs(
          "Downloaded file has no extension: targetFile is provided, but archive is not.\n",
          " Downloaded file will be considered as the targetFile. If the downloaded file is an archive\n",
          " that contains the targetFile, please specify both archive and targetFile.", verbose = verbose
        )
        newFileWithExtension <- file.path(normPath(dirname(downloadFileResult$downloaded)),
                                          downloadFileResult$neededFiles)
        invisible(file.move(
          from = file.path(normPath(downloadFileResult$downloaded)),
          to = newFileWithExtension))
        downloadFileResult$downloaded <- newFileWithExtension
      } else {
        messagePrepInputs(
          "Downloaded file has no extension: both targetFile and archive are provided.\n",
          " Downloaded file will be considered as the archive.", verbose = verbose
        )
        newFileWithExtension <- normPath(file.path(dirname(downloadFileResult$downloaded),
                                                   .basename(downloadFileResult$archive)))
        invisible(file.move(
          from = file.path(normPath(downloadFileResult$downloaded)),
          to = newFileWithExtension))
        downloadFileResult$downloaded <- newFileWithExtension
      }
    } else {
      if (!is.null(archive)) {
        messagePrepInputs(
          "Downloaded file has no extension: archive is provided. \n",
          " downloaded file will be considered as the archive.", verbose = verbose)
        downloadFileResult$neededFiles <- .basename(archive)
        newFileWithExtension <- file.path(normPath(dirname(downloadFileResult$downloaded)),
                                           downloadFileResult$neededFiles)
        invisible(file.move(
          from = file.path(normPath(downloadFileResult$downloaded)),
          to = newFileWithExtension))
        downloadFileResult$downloaded <- newFileWithExtension
      } else {
        messagePrepInputs(
          "Downloaded file has no extension: neither archive nor targetFile are provided. \n",
          "prepInputs will try accessing the file type.", verbose = verbose)
        fileExt <- .guessFileExtension(file = file.path(normPath(downloadFileResult$downloaded)))
        if (is.null(fileExt)) {
          messagePrepInputs("The file was not recognized by prepInputs. ",
                  "Will assume the file is an archive and add '.zip' extension. ",
                  "If this is incorrect or return error, please supply archive or targetFile", verbose = verbose)
          fileExt <- ".zip"
        }
        downloadFileResult$archive <- file.path(normPath(destinationPath),
                                                paste0(downloadFileResult$neededFiles, fileExt))
        invisible(file.move(
          from = file.path(normPath(downloadFileResult$downloaded)),
          to = normPath(downloadFileResult$archive)))
        downloadFileResult$neededFiles <- .listFilesInArchive(downloadFileResult$archive)
        downloadFileResult$downloaded <- downloadFileResult$archive
        downloadFileResult$targetFilePath <- file.path(normPath(destinationPath), downloadFileResult$neededFiles)
      }
    }
  }
  downloadFileResult
}


moveAttributes <- function(source, receiving, attrs = NULL) {
  if (!is.null(receiving)) {
    sourceAttributes <- attributes(source)
    if (length(sourceAttributes) > 0) {
      if (!is.null(attrs)) {
        sourceAttributes <- attrs
      }

      for (i in length(sourceAttributes))
        setattr(receiving, names(sourceAttributes)[i], sourceAttributes[[i]])
    }
  }
  receiving
}

.checkDeprecated <- function(dots, verbose = getOption("reproducible.verbose", 1)) {
  if (!is.null(dots$cacheTags))  {
    messagePrepInputs("cacheTags is being deprecated;",
            " use userTags which will pass directly to Cache.", verbose = verbose)
    dots$userTags <- dots$cacheTags
    dots$cacheTags <- NULL
  }
  if (!is.null(dots$postProcessedFilename))  {
    messagePrepInputs("postProcessedFilename is being deprecated;",
            " use filename2, used in determineFilename.", verbose = verbose)
    dots$filename2 <- dots$postProcessedFilename
    dots$postProcessedFilename <- NULL
  }
  if (!is.null(dots$writeCropped))  {
    messagePrepInputs("writeCropped is being deprecated;",
            " use filename2, used in determineFilename.", verbose = verbose)
    dots$filename2 <- dots$writeCropped
    dots$writeCropped <- NULL
  }
  if (!is.null(dots$rasterInterpMethod))  {
    messagePrepInputs("rasterInterpMethod is being deprecated;",
            " use method which will pass directly to projectRaster.", verbose = verbose)
    dots$method <- dots$rasterInterpMethod
    dots$rasterInterpMethod <- NULL
  }
  if (!is.null(dots$rasterDatatype))  {
    messagePrepInputs("rasterDatatype is being deprecated;",
            " use datatype which will pass directly to writeRaster.", verbose = verbose)
    dots$datatype <- dots$rasterDatatype
    dots$rasterDatatype <- NULL
  }
  if (!is.null(dots$pkg))  {
    messagePrepInputs("pkg is being deprecated;",
            "name the package and function directly, if needed,\n",
            "  e.g., 'pkg::fun'.", verbose = verbose)
    dots$pkg <- NULL
  }

  dots

}

.checkFunInDots <- function(fun = NULL, dots) {
  if (is.null(fun)) {
    if (!is.null(dots$pkg))  {
      fun <- paste0(dots$pkg, "::", fun)
    }
  }
  fun
}

hardLinkOrCopy <- function(from, to, overwrite = FALSE, verbose = TRUE) {
  outFL <- rep(FALSE, length(from))

  if (length(from)) {
    if (isTRUE(overwrite)) {
      fe <- file.exists(to)
      if (any(fe)) {
        unlinkOut <- unlink(to[fe])
      }
    }
    # Basically -- all warnings are irrelevant; if fails, it will return FALSE, then it will try the file.copy
    outFL <- suppressWarnings(file.link(from = from, to = to))
    if (any(outFL)) {
      toCollapsed <- paste(to[outFL], collapse = ", ")
      fromCollapsed <- paste(from[outFL], collapse = ", ")
      messagePrepInputs(hardlinkMessagePrefix, ": ", toCollapsed, ", ",whPointsToMess," "
                        , fromCollapsed, "; no copy/copies made.", verbose = verbose)
    }
    if (any(!outFL)) {
      outFL <- copyFile(to = to[!outFL], from = from[!outFL], overwrite = overwrite, silent = TRUE)
    }
  }
  return(outFL)
}

escapeRegexChars <- function(str, repl = c("(", ")")) {
  for (r in repl) {
    str <- gsub(paste0("\\",r,""), paste0("\\\\",r), str)
  }
  str
}

hardlinkMessagePrefix <- "Hardlinked version of file(s) created at"
hardlinkMessagePrefixForGrep <- escapeRegexChars(hardlinkMessagePrefix)

whPointsToMess <- "which point(s) to"
whPointsToMessForGrep <- escapeRegexChars(whPointsToMess)
