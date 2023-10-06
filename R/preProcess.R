utils::globalVariables(c(
  "reproducible.inputPaths", "successfulCheckSumFilePath", "successfulDir"
))

#' @param n Number of non-null arguments passed to `preProcess`.
#' E.g., passing `n = 1` returns combinations with only a single non-NULL parameter.
#' If `NULL` (default), all parameter combinations are returned.
#'
#' @export
#' @rdname preProcess
preProcessParams <- function(n = NULL) {
  p1 <- data.frame(
    url = c("char", "NULL", "NULL", "NULL"),
    targetFile = c("NULL", "char", "NULL", "NULL"),
    archive = c("NULL", "NULL", "char", "NULL"),
    alsoExtract = c("NULL", "NULL", "NULL", "char"),
    Result = c(
      "Download, extract all files if an archive, guess at 'targetFile', load into R.",
      "Load 'targetFile' into R.",
      "Extract all files, guess at 'targetFile', load into R.",
      "Guess at 'targetFile' from files in 'alsoExtract', load into R."
    ),
    FirstChecksum = c(
      "Write or append all new files.",
      "Write or append 'targetFile'.",
      "Write or append all new files.",
      "Write or append all new files."
    ),
    SecondChecksum = c(
      "Same as first; no 'targetFile'.*",
      "No downloading, so no checksums used.",
      "No downloading, so no checksums used.",
      "No downloading, so no checksums used."
    ),
    stringsAsFactors = FALSE
  )
  p2 <- data.frame(
    url = c("char", "char", "char", "NULL", "NULL", "NULL"),
    targetFile = c("char", "NULL", "NULL", "char", "char", "NULL"),
    archive = c("NULL", "char", "NULL", "NULL", "char", "char"),
    alsoExtract = c("NULL", "NULL", "char", "char", "NULL", "char"),
    Result = c(
      "Download, extract all files if an archive, load 'targetFile' into R.",
      "Download, extract all files, guess at 'targetFile', load into R.",
      "Download, extract only named files in 'alsoExtract', guess at 'targetFile', load into R.",
      "Load 'targetFile' into R.",
      "Extract all files, load 'targetFile' into R.",
      " Extract only named files in 'alsoExtract', guess at 'targetFile', load into R."
    ),
    FirstChecksum = c(
      "Write or append all new files.",
      "Write or append all new files.",
      "Write or append all new files.",
      "Write or append all new files.",
      "Write or append all new files.",
      "Write or append all new files."
    ),
    SecondChecksum = c(
      "Use Checksums, skip downloading.",
      "Same as first; no 'targetFile'.*",
      "Same as first; no 'targetFile'.*",
      "No downloading, so no checksums used.",
      "No downloading, so no checksums used.",
      "No downloading, so no checksums used."
    ),
    stringsAsFactors = FALSE
  )
  p3 <- data.frame(
    url = c("char", "char", "char", "char", "char", "char", "NULL"),
    targetFile = c("char", "NULL", "NULL", "char", "char", "char", "char"),
    archive = c("char", "char", "char", "NULL", "NULL", "char", "char"),
    alsoExtract = c("NULL", "char", "'similar'", "char", "'similar", "NULL", "char"),
    Result = c(
      "Download, extract all files, load 'targetFile' into R.",
      "Download, extract files named in 'alsoExtract', guess at 'targetFile', load into R.",
      "Download, extract all files (can't understand \"similar\"), guess at 'targetFile', load into R.",
      "Download, if an archive, extract files named in 'targetFile' and 'alsoExtract', load 'targetFile' into R.",
      "Download, if an archive, extract files with same base as 'targetFile', load 'targetFile' into R.",
      "Download, extract all files from archive, load 'targetFile' into R.",
      "Extract  files named in 'alsoExtract' from archive, load 'targetFile' into R."
    ),
    FirstChecksum = c(
      "Write or append all new files.",
      "Write or append all new files.",
      "Write or append all new files.",
      "Write or append all new files.",
      "Write or append all new files.",
      "Write or append all new files.",
      "Write or append all new files."
    ),
    SecondChecksum = c(
      "Use Checksums, skip downloading.",
      "Use Checksums, skip downloading.",
      "Same as first; no 'targetFile'.",
      "Use Checksums, skip downloading.",
      "Use Checksums, skip downloading.",
      "Use Checksums, skip downloading.",
      "No downloading, so no checksums used."
    ),
    stringsAsFactors = FALSE
  )
  p4 <- data.frame(
    url = c("char", "char"),
    targetFile = c("char", "char"),
    archive = c("char", "char"),
    alsoExtract = c("char", "'similar'"),
    Result = c(
      "Download, extract files named in 'targetFile' and 'alsoExtract', load 'targetFile' into R.",
      "Download, extract all files with same basename as 'targetFile', load 'targetFile' into R."
    ),
    FirstChecksum = c(
      "Write or append all new files.",
      "Write or append all new files."
    ),
    SecondChecksum = c(
      "Use Checksums, skip downloading.",
      "Use Checksums, skip downloading."
    ),
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
#' This does downloading (via `downloadFile`), checksumming (`Checksums`),
#' and extracting from archives (`extractFromArchive`), plus cleaning up of input
#' arguments (e.g., paths, function names).
#' This is the first stage of three used in `prepInputs`.
#'
#' @return
#' A list with 5 elements: `checkSums` (the result of a `Checksums`
#' after downloading), `dots` (cleaned up `...`, including deprecated argument checks),
#' `fun` (the function to be used to load the `preProcess`ed object from disk),
#' and `targetFilePath` (the fully qualified path to the `targetFile`).
#'
#' @section Combinations of `targetFile`, `url`, `archive`, `alsoExtract`:
#'
#'   Use `preProcessParams()` for a table describing various parameter combinations and their
#'   outcomes.
#'
#'  `*` If the `url` is a file on Google Drive, checksumming will work
#'  even without a `targetFile` specified because there is an initial attempt
#'  to get the remove file information (e.g., file name). With that, the connection
#'  between the `url` and the filename used in the \file{CHECKSUMS.txt} file can be made.
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
                       verbose = getOption("reproducible.verbose", 1),
                       .tempPath, ...) {
  if (missing(.tempPath)) {
    .tempPath <- tempdir2(rndstr(1, 6))
    on.exit(
      {
        unlink(.tempPath, recursive = TRUE)
      },
      add = TRUE
    )
  }
  dlFunCaptured <- substitute(dlFun)
  prepInputsAssertions(environment())
  isAlreadyQuoted <- any(grepl("quote", dlFunCaptured))
  if (isAlreadyQuoted) {
    dlFunCaptured <- eval(dlFunCaptured)
  }

  dots <- list(...)

  fun <- .checkFunInDots(fun = fun, dots = dots)
  dots <- .checkDeprecated(dots, verbose = verbose)

  teamDrive <- getTeamDrive(dots)

  # remove trailing slash -- causes unzip fail if it is there
  destinationPath <- normPath(destinationPath)
  checkSumFilePath <- identifyCHECKSUMStxtFile(destinationPath)

  if (!is.null(archive)) {
    if (any(is.na(archive))) {
      if (all(!is.character(archive))) {
        archive <- as.character(archive)
      }
    }
  }
  targetFileGuess <- NULL
  if (is.null(targetFile) || is.null(archive)) {
    targetFileGuess <- .guessAtFile(
      url = url, archive = archive, targetFile = targetFile,
      destinationPath = destinationPath, verbose = verbose,
      team_drive = teamDrive
    )
    if (is.null(archive)) {
      archive <- .isArchive(targetFileGuess)
    }
  }

  if (is.logical(alsoExtract)) {
    alsoExtract <- c("none", "all")[alsoExtract + 1]
  }
  targetFilePath <- getTargetFilePath(
    targetFile, archive, targetFileGuess, verbose,
    destinationPath, alsoExtract, checkSumFilePath
  )
  targetFile <- makeRelative(targetFilePath, destinationPath)

  alsoExtract <- guessAlsoExtract(targetFile, alsoExtract, checkSumFilePath)

  if (!dir.exists(destinationPath)) {
    if (isFile(destinationPath)) {
      stop("destinationPath must be a directory")
    }
    checkPath(destinationPath, create = TRUE)
  }

  messagePrepInputs("Preparing: ", targetFile, verbose = verbose)

  needChecksums <- 0

  archive <- setupArchive(archive, destinationPath)
  filesToCheck <- na.omit(unique(c(archive, targetFilePath, alsoExtract)))

  # Need to run checksums on all files in destinationPath because we may not know what files we
  #   want if targetFile, archive, alsoExtract not specified
  inputPaths <- runChecksums(destinationPath, checkSumFilePath, filesToCheck, verbose)
  list2env(inputPaths, environment()) # reproducible.inputPaths, destinationPathUser, destinationPath, checkSums

  if (is(checkSums, "try-error")) {
    needChecksums <- 1
    checkSums <- .emptyChecksumsResult
  }

  # This will populate a NULL archive if archive is local or
  archiveOut <- dealWithArchive(
    archive, url, targetFile, checkSums, alsoExtract,
    destinationPath, teamDrive, verbose
  )
  list2env(archiveOut, envir = environment()) # checkSums, archive, fileGuess

  # NOW -- archive will exist if it didn't before
  # Double check that targetFile and alsoExtract are correct paths, given that in archive, they may be in sub-folders
  needEmptyChecksums <- FALSE
  if (is.logical(purge)) purge <- as.numeric(purge)
  if (purge == 1) {
    unlink(checkSumFilePath)
    needChecksums <- 1
  }

  if (purge > 1) {
    checkSums <- .purge(
      checkSums = checkSums, purge = purge, targetFile = targetFile,
      archive = archive, url = url, alsoExtract = alsoExtract
    )
    needChecksums <- 2
  }

  neededFiles <- c(targetFile, makeAbsolute(alsoExtract, destinationPath)) # if (!is.null(alsoExtract)) basename2(alsoExtract))
  if (is.null(neededFiles)) neededFiles <- makeAbsolute(archive)

  # remove "similar" from needed files. It is for extracting.
  neededFiles <- grep("similar$", neededFiles, value = TRUE, invert = TRUE)

  # Deal with "similar" in alsoExtract -- maybe this is obsolete with new feature that uses file_name_sans_ext
  if (is.null(alsoExtract)) {
    filesInsideArchive <- .listFilesInArchive(archive) # will be relative
    neededFiles <- checkRelative(neededFiles, destinationPath, filesInsideArchive)

    if (isTRUE(length(filesInsideArchive) > 0)) {
      checkSums <- .checkSumsUpdate(destinationPath, makeAbsolute(filesInsideArchive, destinationPath),
        checkSums = checkSums
      )
    }
    neededFiles <- unique(c(neededFiles, filesInsideArchive))
  } else {
    outFromSimilar <- .checkForSimilar(neededFiles, alsoExtract, archive, targetFile,
      destinationPath = destinationPath, checkSums,
      checkSumFilePath = checkSumFilePath, url
    )
    list2env(outFromSimilar, environment()) # neededFiles, checkSums
  }
  neededFiles <- unique(makeAbsolute(neededFiles, destinationPath))
  neededFiles <- grep("none$", neededFiles, value = TRUE, invert = TRUE)
  # alsoExtract <- grep("none$", alsoExtract, value = TRUE, invert = TRUE)

  filesToChecksum <- if (is.null(archive) || isTRUE(is.na(archive))) {
    NULL
  } else {
    archive
  }

  filesToChecksum <- unique(c(filesToChecksum, neededFiles))
  isOK <- .compareChecksumsAndFilesAddDirs(checkSums, filesToChecksum, destinationPath) # also checks dirs, so adds lines
  if (isTRUE(!all(isOK))) {
    results <- .tryExtractFromArchive(
      archive = if (isTRUE(is.na(archive))) NULL else archive,
      neededFiles = neededFiles,
      alsoExtract = alsoExtract, destinationPath = destinationPath,
      checkSums = checkSums, needChecksums = needChecksums,
      checkSumFilePath = checkSumFilePath,
      filesToChecksum = filesToChecksum,
      targetFilePath = targetFilePath, quick = quick,
      verbose = verbose, .tempPath = .tempPath
    )
    list2env(results, environment()) # neededFiles, checkSums, filesExtr, targetFilePath, filesToChecksum, needChecksums

    if (needChecksums > 0) {
      checkSums <- appendChecksumsTable(
        checkSumFilePath = checkSumFilePath,
        filesToChecksum = unique(filesToChecksum),
        destinationPath = destinationPath,
        append = results$needChecksums >= 2
      )
    }
  }

  # Check for local copies in all values of reproducible.inputPaths
  # At the end of this function, the files will be present in destinationPath, if they existed
  #  in options("reproducible.inputPaths")
  localChecks <- .checkLocalSources(neededFiles,
    checkSums = checkSums,
    checkSumFilePath = checkSumFilePath,
    otherPaths = reproducible.inputPaths,
    destinationPath, needChecksums = needChecksums, verbose = verbose
  )
  list2env(localChecks, environment()) # neededFiles, checkSums, needChecksums, successfulDir, successfulCheckSumFilePath

  # Change the destinationPath to the reproducible.inputPaths temporarily, so
  #   download happens there. Later it will be linked to the user destinationPath

  if (!is.null(reproducible.inputPaths)) {
    # may already have been changed above
    outCheck <- if (!is.null(targetFilePath)) {
      !file.exists(targetFilePath)
    } else {
      TRUE
    } ## if NULL, it doesn't exist and we want to proceed
    if (outCheck) { # skip if it already existed locally
      if (is.null(destinationPathUser)) {
        destinationPathUser <- destinationPath
      }
      on.exit(
        {
          destinationPath <- destinationPathUser
        },
        add = TRUE
      )

      if (!identical(destinationPath, reproducible.inputPaths)) {
        # CHANGE destinationPath FOR REMAINDER OF THIS FUNCTION
        neededFilesNew <- makeRelative(neededFiles, destinationPath)
        targetFilePathNew <- makeRelative(targetFilePath, destinationPath)
        destinationPathNew <- reproducible.inputPaths[1]
        archiveExistInDestDir <- if (!isNULLorNA(archive)) {
          file.exists(archive)
        } else {
          FALSE
        }
        existInDestDir <- if (!isNULLorNA(neededFiles)) {
          file.exists(neededFiles)
        } else {
          FALSE
        }
        if (any(existInDestDir)) {
          linkOrCopy(neededFiles[existInDestDir],
            makeAbsolute(neededFilesNew[existInDestDir],
              absoluteBase = destinationPathNew
            ),
            verbose = verbose - 1
          )
        }
        if (any(archiveExistInDestDir)) {
          linkOrCopy(archive[archiveExistInDestDir],
            makeAbsolute(makeRelative(archive[archiveExistInDestDir], destinationPath),
              absoluteBase = destinationPathNew
            ),
            verbose = verbose - 1
          )
        }
        targetPath <- targetFilePathNew
        destinationPath <- destinationPathNew
        neededFiles <- neededFilesNew
      }

      if (isTRUE(any(grepl(archive, pattern = destinationPathUser)))) {
        # might have a "." as destinationPath -- messes with grepl
        patt <- if (grepl("^\\.", destinationPathUser)) {
          gsub("^\\.", "^\\\\.", destinationPathUser)
        } else {
          destinationPathUser
        }
        archive <- gsub(archive, pattern = patt, replacement = destinationPath)
      }
      targetFilePath <- makeAbsolute(targetFilePath, destinationPath)
      neededFiles <- makeAbsolute(neededFiles, destinationPath)
    }
  }

  ###############################################################
  # Download
  ###############################################################
  downloadFileResult <- downloadFile(
    archive = if (isTRUE(is.na(archive))) NULL else archive,
    targetFile = targetFile, neededFiles = neededFiles, destinationPath = destinationPath,
    quick = quick, checkSums = checkSums, dlFun = dlFunCaptured, url = url,
    checksumFile = asPath(checkSumFilePath), needChecksums = needChecksums,
    overwrite = overwrite, purge = purge, # may need to try purging again if no target,
    #    archive or alsoExtract were known yet
    verbose = verbose, .tempPath = .tempPath, ...
  )

  downloadFileResult <- .fixNoFileExtension(
    downloadFileResult = downloadFileResult,
    targetFile = targetFile, archive = archive,
    destinationPath = destinationPath, verbose = verbose
  )

  # Post downloadFile -- put objects into this environment
  if (!is.null(downloadFileResult$targetFilePath)) {
    targetFilePath <- makeAbsolute(downloadFileResult$neededFiles, destinationPath)
  }
  checkSums <- downloadFileResult$checkSums
  needChecksums <- downloadFileResult$needChecksums
  neededFiles <- downloadFileResult$neededFiles
  # If the download was of an archive, then it is possible the archive path is wrong
  if (!isTRUE(is.na(archive))) {
    if (identical(downloadFileResult$downloaded, downloadFileResult$archive)) {
      archive <- downloadFileResult$downloaded
    }
    # archive specified, alsoExtract is NULL --> now means will extract all
    if (is.null(archive)) archive <- downloadFileResult$archive
  }

  ###############################################################
  # redo "similar" after download
  ###############################################################
  outFromSimilar <- .checkForSimilar(
    neededFiles = neededFiles, alsoExtract = alsoExtract,
    archive = archive, targetFile = targetFile,
    destinationPath = destinationPath, checkSums = checkSums,
    checkSumFilePath = checkSumFilePath,
    url = url
  )
  list2env(outFromSimilar, environment()) # neededFiles, checkSums

  # don't include targetFile in neededFiles -- extractFromArchive deals with it separately
  if (length(neededFiles) > 1) alsoExtract <- setdiff(neededFiles, targetFile)

  # To this point, we only have the archive in hand -- include this in the list of filesToChecksum
  filesToChecksum <- if (isTRUE(is.na(archive)) || (is.null(archive))) {
    downloadFileResult$downloaded
  } else {
    archive
  }
  on.exit(
    {
      if (needChecksums > 0) {
        appendChecksumsTable(
          checkSumFilePath = checkSumFilePath,
          filesToChecksum = filesToChecksum,
          destinationPath = destinationPath,
          append = (needChecksums == 2)
        )
      }
      needChecksums <- 0
    },
    add = TRUE
  )

  # Stage 1 - Extract from archive
  filesToChecksum <- unique(c(filesToChecksum, neededFiles))
  isOK <- .compareChecksumsAndFilesAddDirs(checkSums, filesToChecksum, destinationPath)
  if (isTRUE(!all(isOK))) {
    filesExtracted <- .tryExtractFromArchive(
      archive = archive,
      neededFiles = neededFiles,
      alsoExtract = alsoExtract,
      destinationPath = destinationPath,
      checkSums = checkSums,
      needChecksums = needChecksums,
      checkSumFilePath = checkSumFilePath,
      filesToChecksum = filesToChecksum,
      targetFilePath = targetFilePath,
      quick = quick, verbose = verbose,
      .tempPath = .tempPath
    )
    # this changes targetFilePath to have folder if the extraction included a folder
    list2env(filesExtracted, environment()) # neededFiles, checkSums, filesExtr, targetFilePath, filesToChecksum, needChecksums
  } else {
    if (!is.null(.isArchive(archive))) {
      messagePrepInputs("  Skipping extractFromArchive attempt: no files missing", verbose = verbose)
    }
    if (!is.null(targetFilePath))
      if (!makeAbsolute(targetFilePath, destinationPath) %in%
          makeAbsolute(neededFiles, destinationPath)) {
        if (!basename2(targetFilePath) %in% makeRelative(neededFiles, destinationPath)) {
          targetFilePath <- grep(basename2(targetFilePath), neededFiles, value = TRUE)
        }
      }

    filesExtr <- c(filesToChecksum, neededFiles)
    filesExtr <- setdiff(filesExtr, .isArchive(filesExtr))
  }

  filesExtr <- unique(c(filesExtr, filesToChecksum))

  # link back to destinationPath if options("reproducible.inputPaths") was used.
  #  destinationPath had been overwritten to be options("reproducible.inputPaths")
  if (!is.null(reproducible.inputPaths)) {
    if (!is.null(destinationPathUser)) { # retrieved file locally
      foundInInputPaths <- grepl(normPath(destinationPath), normPath(filesExtr))
      # Make sure they are all in options("reproducible.inputPaths"), accounting for
      #   the fact that some may have been in sub-folders -- i.e., don't deal with these
      if (isTRUE(any(foundInInputPaths))) {
        whFilesExtrInIP <- which(file.exists(filesExtr[foundInInputPaths]))
        if (length(whFilesExtrInIP)) {
          from <- filesExtr[whFilesExtrInIP]
          to <- makeAbsolute(makeRelative(from, destinationPath), destinationPathUser)
          if (!isTRUE(all(from %in% to))) {
            messagePrepInputs("...using copy in getOption('reproducible.inputPaths')...")
          }
          outHLC <- hardLinkOrCopy(from, to)
          filesExtr[foundInInputPaths] <- to
        }
      }
      # targetFilePath may be already in destinationPathUser, depending on when it was created
      if (!is.null(targetFilePath)) {
        if (!identical(to, targetFilePath)) {
          targetFilePathTmp <- to[basename(to) %in% basename(targetFilePath)]
          if (any(file.exists(targetFilePathTmp))) {
            targetFilePath <- targetFilePathTmp
          } else {
            targetFilePath <- makeAbsolute(
              makeRelative(targetFilePath, destinationPath),
              destinationPathUser
            )
          }
        }
      }
      destinationPath <- destinationPathUser
    } else {
      foundInLocalPaths <- grepl(normPath(destinationPath), normPath(filesExtr))
      # Make sure they are all in options("reproducible.inputPaths"), accounting for
      #   the fact that some may have been in sub-folders -- i.e., don't deal with these
      if (isTRUE(any(foundInLocalPaths))) {
        whFilesExtrInLP <- which(file.exists(filesExtr[foundInLocalPaths]))
        if (length(whFilesExtrInLP)) {
          from <- filesExtr[whFilesExtrInLP]
          to <- makeAbsolute(makeRelative(from, destinationPath), reproducible.inputPaths)
          if (!isTRUE(all(from %in% to))) {
            messagePrepInputs("... copying to getOption('reproducible.inputPaths')...")
          }
          outHLC <- hardLinkOrCopy(from, to)
        }
      }
    }
  }
  # if it was a nested file
  if (any(fileExt(neededFiles) %in% c("zip", "tar", "rar")) && !isTRUE(is.na(archive))) {
    nestedArchives <- neededFiles[fileExt(neededFiles) %in% c("zip", "tar", "rar")]
    nestedArchives <- makeAbsolute(nestedArchives[1], destinationPath)
    messagePrepInputs("There are still archives in the extracted files.",
      " preProcess will try to extract the files from ", basename2(nestedArchives), ".",
      " If this is incorrect, please supply archive.",
      verbose = verbose
    )
    # Guess which files inside the new nested
    nestedTargetFile <- .listFilesInArchive(archive = nestedArchives)
    outFromSimilar <- .checkForSimilar(
      alsoExtract = alsoExtract,
      archive = nestedArchives,
      neededFiles = nestedTargetFile,
      destinationPath = destinationPath,
      checkSums = checkSums,
      checkSumFilePath = checkSumFilePath,
      targetFile = targetFile
    )
    neededFiles <- outFromSimilar$neededFiles
    checkSums <- outFromSimilar$checkSums

    # don't include targetFile in neededFiles -- extractFromArchive deals with it separately
    if (length(neededFiles) > 1) alsoExtract <- setdiff(neededFiles, targetFile)

    # To this point, we only have the archive in hand -- include this in the list of filesToChecksum
    filesToChecksum <- if (is.null(archive)) downloadFileResult$downloaded else basename2(archive)
    on.exit(
      {
        if (needChecksums > 0) {
          # needChecksums 1 --> write a new checksums.txt file
          # needChecksums 2 --> append to checksums.txt
          appendChecksumsTable(
            checkSumFilePath = checkSumFilePath,
            filesToChecksum = filesToChecksum,
            destinationPath = destinationPath,
            append = (needChecksums == 2)
          )
        }
        needChecksums <- 0
      },
      add = TRUE
    )
    extractedFiles <-
      .tryExtractFromArchive(
        archive = nestedArchives, neededFiles = neededFiles,
        alsoExtract = alsoExtract, destinationPath = destinationPath,
        checkSums = checkSums, needChecksums = needChecksums,
        checkSumFilePath = checkSumFilePath, filesToChecksum = filesToChecksum,
        targetFilePath = targetFilePath, quick = quick,
        verbose = verbose, .tempPath = .tempPath
      )
    filesExtr <- c(filesExtr, extractedFiles$filesExtr)
  }
  targetParams <- .guessAtTargetAndFun(targetFilePath, destinationPath,
    filesExtracted = filesExtr,
    fun, verbose = verbose
  ) # passes through if all known
  targetFile <- makeRelative(targetParams$targetFilePath, destinationPath)
  targetFilePath <- targetParams$targetFilePath
  funChar <- targetParams$fun

  ## targetFilePath might still be NULL, need destinationPath too
  if (is.null(targetFilePath)) {
    if (is.null(filesExtr)) {
      if (!is.null(downloadFileResult$downloaded)) {
        targetFilePath <- downloadFileResult$downloaded
      }
    } else {
      targetFilePath <- filesExtr
    }
  }

  if (is.null(targetFile) && !is.null(targetFilePath)) {
    targetFile <- makeRelative(targetFilePath, destinationPath)
  }

  ## Convert the fun as character string to function class, if not already
  fun <- .extractFunction(funChar)

  if (needChecksums > 0) {
    ## needChecksums 1 --> write a new CHECKSUMS.txt file
    ## needChecksums 2 --> append  to CHECKSUMS.txt file
    ## needChecksums 3 --> append  to checkSumFilePath file OR successfulCheckSumFilePath, not both
    if (needChecksums == 3) {
      # successfulCheckSumFilePath we do not need to update. Determine which one this is, and do
      #   other
      if (identical(checkSumFilePath, successfulCheckSumFilePath)) { # if it was in checkSumFilePath
        checkSumFilePath <- identifyCHECKSUMStxtFile(successfulDir) #   run Checksums in IP
      }
    }
    checkSums <- appendChecksumsTable(
      checkSumFilePath = checkSumFilePath,
      filesToChecksum = unique(filesToChecksum),
      destinationPath = destinationPath,
      append = needChecksums >= 2
    )
    if (!is.null(reproducible.inputPaths) && needChecksums != 3) {
      checkSumFilePathInputPaths <- identifyCHECKSUMStxtFile(reproducible.inputPaths[[1]])
      suppressMessages({
        checkSums <- appendChecksumsTable(
          checkSumFilePath = checkSumFilePathInputPaths,
          filesToChecksum = unique(filesToChecksum),
          destinationPath = destinationPath,
          append = needChecksums == 2
        )
      })
    }
    on.exit(
      {
        needChecksums <- 0
      },
      add = TRUE,
      after = FALSE
    ) # effectively remove appendChecksums in other
    # on.exit because it is done here
  }

  failStop <- FALSE
  if (is.null(targetFilePath)) {
    failStop <- TRUE
  } else if (!isTRUE(file.exists(targetFilePath))) {
    failStop <- TRUE
  }
  if (isTRUE(failStop)) {
    stop(
      "targetFile appears to be misspecified at: ", targetFilePath, ". ",
      "Possibly, it does not exist in the specified archive, ",
      "or the file doesn't exist in destinationPath"
    )
  }

  archiveInChecksums <- checkSums$actualFile %in% makeRelative(archive, destinationPath) # basename2 is needed in checksums
  if (any(archiveInChecksums)) {
    checkSums[which(archiveInChecksums), result := "ArchiveOK"]
  }


  out <- list(
    checkSums = checkSums,
    dots = dots,
    fun = fun,
    funChar = funChar,
    targetFilePath = targetFilePath,
    destinationPath = destinationPath,
    object = downloadFileResult$object
  )
  return(out)
}

#' Purge individual line items from checksums file
#'
#' @inheritParams downloadFile
#' @keywords internal
#' @rdname purge
.purge <- function(checkSums, purge, targetFile, archive, alsoExtract, url, destinationPath) {
  purgeChar <- as.character(purge)
  checkSums <- tryCatch(
    switch(purgeChar,
      "2" = checkSums[!(checkSums$expectedFile %in% makeRelative(targetFile, destinationPath)), ],
      "3" = checkSums[!(checkSums$expectedFile %in% makeRelative(archive, destinationPath)), ],
      "4" = checkSums[!(checkSums$expectedFile %in% makeRelative(alsoExtract, destinationPath)), ],
      "5" = checkSums[!(checkSums$expectedFile %in% makeRelative(unique(c(targetFile, alsoExtract)), destinationPath)), ], # nolint
      "6" = checkSums[!(checkSums$expectedFile %in%
        makeRelative(unique(c(targetFile, alsoExtract, archive)), destinationPath)), ], # nolint
      "7" = checkSums[!(checkSums$expectedFile %in%
        makeRelative(unique(c(targetFile, alsoExtract, archive, url)), destinationPath)), ] # nolint
    ),
    error = function(x) checkSums
  )
  checkSums
}

#' @keywords internal
.emptyChecksumsResult <- data.table::data.table(
  expectedFile = character(),
  actualFile = character(),
  result = character()
)

#' @keywords internal
.emptyChecksumsFileContent <- data.frame(
  file = character(),
  checksum = character(),
  filesize = character(),
  algorithm = character()
)

#' @keywords internal
#' @importFrom utils getFromNamespace
.extractFunction <- function(fun, envir = parent.frame()) {
  if (!is.null(fun)) {
    if (is.call(fun)) {
      fun
    } else {
      suppressWarnings(isNAFun <- is.na(fun))
      if (!any(isNAFun)) {
        if (!is.function(fun)) {
          if (any(grepl("::", fun))) {
            fun2 <- strsplit(fun, "::")[[1]]
            pkg <- fun2[1]
            fun <- fun2[2]
            fun <- getFromNamespace(fun, pkg)
          } else {
            fun <- get(fun, envir)
          }
        }
      }
    }
  }
  fun
}

#' @keywords internal
.guessAtFile <- function(url, archive, targetFile, destinationPath,
                         verbose = getOption("reproducible.verbose", 1), team_drive = NULL) {
  # if (is.null(targetFile)) {
  guessedFile <- if (!is.null(url)) {
    gf <- file.path(destinationPath, basename2(url))
    if (grepl("drive.google.com", url)) {
      # ie <- isTRUE(internetExists())
      # if (ie) {
      gf <- assessGoogle(
        url = url, archive = archive, targetFile = targetFile,
        destinationPath = destinationPath, verbose = verbose, team_drive = NULL
      )
      gf <- makeAbsolute(gf, destinationPath)
      # }
    }
    gf
  } else {
    NULL
  }
  normPath(guessedFile)
}

# COPIED FROM REQUIRE
# urlExists <- function(url) {
#   con <- url(url)
#   on.exit(try(close(con), silent = TRUE), add = TRUE)
#   for (i in 1:5) {
#     a <- try(suppressWarnings(readLines(con, n = 1)), silent = TRUE)
#     try(close(con), silent = TRUE)
#     ret <- if (is(a, "try-error")) FALSE else TRUE
#     if (isTRUE(ret)) {
#       break
#     } else {
#       Sys.sleep(0.1)
#     }
#   }
#   ret
# }


#' @keywords internal
.checkSumsUpdate <- function(destinationPath, newFilesToCheck, checkSums,
                             checkSumFilePath = NULL, verbose = getOption("reproducible.verbose", 1)) {
  if (!is.null(newFilesToCheck)) {
    if (is.null(checkSumFilePath) || length(checkSumFilePath) == 0) {
      checkSumFilePath <- identifyCHECKSUMStxtFile(destinationPath)
    }
    if (!file.exists(checkSumFilePath)) {
      checkSums
    } else {
      checkSums2 <- suppressMessages(try(Checksums(
        path = destinationPath, write = FALSE,
        files = newFilesToCheck,
        checksumFile = checkSumFilePath,
        verbose = verbose
      ), silent = TRUE))
      if (!is(checkSums2, "try-error")) {
        checkSums <- rbindlist(list(checkSums, checkSums2))
        data.table::setkey(checkSums, result)
        checkSums <- unique(checkSums, fromLast = TRUE, by = "expectedFile")
        checkSums <- rbindlist(list(
          checkSums[compareNA("OK", result)],
          checkSums[compareNA("FAIL", result)],
          checkSums[is.na(result)]
        ))
      } else {
        stop("checkSumFilePath is not a CHECKSUMS.txt file")
      }
    }
  }
  checkSums
}

#' @keywords internal
.similarFilesInCheckSums <- function(file, checkSums, alsoExtract) {
  if (NROW(checkSums)) {
    if (!missing(alsoExtract)) {
      file <- unique(c(file, alsoExtract))
    }
    anySimilarInCS <- vapply(filePathSansExt(file), function(fi) {
      res <- checkSums[grepl(paste0(fi, "\\."), checkSums$expectedFile), ]$result
      res <- na.omit(res)
      if (length(res) == 0) res <- "NotOK"
      isTRUE(all(compareNA("OK", res)))
    }, FUN.VALUE = logical(1))

    all(anySimilarInCS)
  } else {
    FALSE
  }
}

#' @keywords internal
.checkForSimilar <- function(neededFiles, alsoExtract, archive, targetFile,
                             destinationPath, checkSums, checkSumFilePath,
                             url, verbose = getOption("reproducible.verbose", 1)) {
  lookForSimilar <- FALSE
  if (is.null(alsoExtract) || length(alsoExtract) == 0) {
    messagePrepInputs("alsoExtract is unspecified; assuming that all files must be extracted")
    lookForSimilar <- "all"
  } else {
    if (!all(is.na(alsoExtract))) {
      if ("similar" %in% basename2(alsoExtract)) {
        lookForSimilar <- TRUE
      }
    }
  }

  if (isTRUE(lookForSimilar) || ("all" %in% lookForSimilar && !is.null(archive))) {
    allFiles <- .listFilesInArchive(archive)
    neededFiles <- checkRelative(neededFiles, destinationPath, allFiles)
    if (is.null(targetFile)) {
      messagePrepInputs("No targetFile supplied. ",
        "Extracting all files from archive",
        verbose = verbose
      )
      neededFiles <- allFiles
    } else if ("all" %in% lookForSimilar) {
      messagePrepInputs("Extracting all files from archive", verbose = verbose)
      neededFiles <- allFiles
    } else {
      allOK <- .similarFilesInCheckSums(targetFile, checkSums, alsoExtract)
      if (!allOK) {
        filePatternToKeep <- gsub(basename2(targetFile),
          pattern = fileExt(basename2(targetFile)), replacement = ""
        )
        filesToGet <- grep(allFiles, pattern = filePatternToKeep, value = TRUE)
        neededFiles <- unique(c(neededFiles, filesToGet))
      }
    }
    rerunChecksums <- TRUE
    if (exists("filesToGet", inherits = FALSE)) {
      if (length(filesToGet) == 0) {
        rerunChecksums <- FALSE
      }
    }
    neededFiles <- unique(makeAbsolute(neededFiles, destinationPath)) #avoids dup. in nested folders
    if (!is.null(neededFiles) && rerunChecksums) {
      checkSums <- .checkSumsUpdate(
        destinationPath = destinationPath, newFilesToCheck = neededFiles,
        checkSums = checkSums,
        checkSumFilePath = checkSumFilePath, verbose = verbose
      )
    }
  }
  list(neededFiles = neededFiles, checkSums = checkSums)
}

#' @keywords internal
.checkLocalSources <- function(neededFiles, checkSumFilePath, checkSums, otherPaths, needChecksums,
                               destinationPath, verbose = getOption("reproducible.verbose", 1)) {
  foundInInputPaths <- character()
  successfulCheckSumFilePath <- character()
  successfulDir <- character()
  if (!is.null(neededFiles)) {
    filesInHand <- checkSums[compareNA(checkSums$result, "OK"), ]$expectedFile
    if (!all(neededFiles %in% filesInHand)) {
      for (op in otherPaths) {
        recursively <- if (!is.null(getOption("reproducible.inputPathsRecursive"))) {
          getOption("reproducible.inputPathsRecursive")
        } else {
          FALSE
        }
        opFiles <- dir(op, recursive = recursively, full.names = TRUE)
        neededFilesRel <- makeRelative(neededFiles, destinationPath)
        if (any(neededFilesRel %in% basename2(opFiles))) {
          isNeeded <- basename2(opFiles) %in% neededFilesRel
          dirNameOPFiles <- dirname(opFiles[isNeeded])

          # foundRecursively <- dirNameOPFiles != dirname(opFiles[isNeeded])
          # foundRecursively <- basename2(opFiles[isNeeded][foundRecursively])

          uniqueDirsOPFiles <- rev(unique(dirNameOPFiles))

          checkSumFilePathTry <- checkSumFilePath
          # check CHECKSUMS.txt files, first the one in destinationPath, then ones in inputPaths
          for (dirOPFiles in uniqueDirsOPFiles) {
            # for (i in seq(1 + length(uniqueDirsOPFiles))) {
            checkSumFilePathTry <- identifyCHECKSUMStxtFile(dirOPFiles)
            checkSumsInputPath <- suppressMessages(
              Checksums(
                path = dirOPFiles, write = FALSE,
                files = neededFilesRel,
                checksumFile = checkSumFilePathTry,
                verbose = verbose
              )
            )
            isOK <- checkSumsInputPath[checkSumsInputPath$expectedFile %in% neededFilesRel, ]$result
            if (length(isOK)) {
              if (all(compareNA(isOK, "OK"))) {
                needChecksums <- 3 # Basically this means that we *may* have to update
                #   checksums file in either destinationPath or
                #   options("reproducible.inputPaths")
                successfulCheckSumFilePath <- checkSumFilePathTry
                successfulDir <- unique(dirOPFiles)
                break
              }
            }

            checkSumFilePathTry <- identifyCHECKSUMStxtFile(dirOPFiles)
          }
          checkSumsIPOnlyNeeded <- checkSumsInputPath[compareNA(checkSumsInputPath$result, "OK"), ]
          filesInHandIP <- checkSumsIPOnlyNeeded$expectedFile
          filesInHandIPLogical <- neededFilesRel %in% filesInHandIP
          if (any(filesInHandIPLogical)) {
            outHLC <- hardLinkOrCopy(
              from = makeAbsolute(filesInHandIP, dirOPFiles),
              to = neededFiles[filesInHandIPLogical], verbose = verbose
            )
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
    if (!is.null(reproducible.inputPaths)) {
      reproducible.inputPaths <- normPath(reproducible.inputPaths)
    }

    if (identical(destinationPath, reproducible.inputPaths)) {
      foundInInputPaths <- filesInHand
      successfulDir <- destinationPath
      successfulCheckSumFilePath <- checkSumFilePath
    }
  }
  list(
    checkSums = checkSums, needChecksums = needChecksums,
    # foundRecursively = foundRecursively,
    neededFiles = neededFiles,
    successfulCheckSumFilePath = successfulCheckSumFilePath,
    successfulDir = successfulDir,
    foundInInputPaths = makeAbsolute(foundInInputPaths, destinationPath)
  )
}

#' Hardlink, symlink, or copy a file
#'
#' Attempt first to make a hardlink. If that fails, try to make
#' a symlink (on non-windows systems and `symlink = TRUE`).
#' If that fails, copy the file.
#'
#' @note Use caution with files-backed objects (e.g., rasters). See examples.
#'
#' @param from,to  Character vectors, containing file names or paths.
#'                 `to` can alternatively be the path to a single existing directory.
#' @param symlink  Logical indicating whether to use symlink (instead of hardlink).
#'                 Default `FALSE`.
#' @inheritParams prepInputs
#' @seealso [file.link()], [file.symlink()], [file.copy()].
#'
#' @author Alex Chubaty and Eliot McIntire
#' @export
#' @return
#' This function is called for its side effects, which will be a `file.link` is that
#' is available or `file.copy` if not (e.g., the two directories are not on the
#' same physical disk).
#'
#' @examples
#'
#' tmpDir <- file.path(tempdir(), "symlink-test")
#' tmpDir <- normalizePath(tmpDir, winslash = "/", mustWork = FALSE)
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
#' if (requireNamespace("terra", quietly = TRUE)) {
#'   ## using spatRasters and other file-backed objects
#'   f3a <- system.file("ex/test.grd", package = "terra")
#'   f3b <- system.file("ex/test.gri", package = "terra")
#'   r3a <- terra::rast(f3a)
#'   f4a <- file.path(tmpDir, "raster4.grd")
#'   f4b <- file.path(tmpDir, "raster4.gri")
#'   linkOrCopy(f3a, f4a) ## hardlink
#'   linkOrCopy(f3b, f4b) ## hardlink
#'   r4a <- terra::rast(f4a)
#'
#'   isTRUE(all.equal(r3a, r4a)) # TRUE
#'
#'   ## cleanup
#'   unlink(tmpDir, recursive = TRUE)
#' }
linkOrCopy <- function(from, to, symlink = TRUE, overwrite = TRUE,
                       verbose = getOption("reproducible.verbose", 1)) {
  existsLogical <- file.exists(from)
  existsTo <- file.exists(to)
  if (any(existsTo)) {
    toDig <- unlist(.robustDigest(asPath(to[existsTo])))
    fromDig <- unname(unlist(.robustDigest(asPath(from[existsTo]))))
    existsToSame <- toDig == fromDig
    if (any(existsToSame)) {
      to <- c(to[existsTo][!existsToSame], to[!existsTo])
      from <- c(from[existsTo][!existsToSame], from[!existsTo])
    }
  }
  toCollapsed <- paste(to, collapse = "\n")
  fromCollapsed <- paste(from, collapse = "\n")
  result <- TRUE
  if (!all(to %in% from)) {
    if (any(existsLogical)) {
      toDirs1 <- unique(dirname(to))
      dirDoesntExist1 <- !dir.exists(toDirs1)

      # some directories in from won't look like directories in the prev "to"
      #  e.g., test\folder1\folder2 --> folder 2 could be a file or a dir, and "dir.exists(to)" won't know which
      #  because test\folder1 didn't exist
      # So, identify the dirs in the `from`, and those ones will also be dirs in to
      fromDirs <- dir.exists(from)
      toDirs2 <- to[fromDirs]
      dirDoesntExist2 <- rep(TRUE, length(toDirs2))

      if (any(dirDoesntExist1) || any(dirDoesntExist2)) {
        needCreate <- unique(c(toDirs1[dirDoesntExist1], toDirs2[dirDoesntExist2]))
        if (any(is.na(needCreate))) {
          needCreate <- na.omit(needCreate)
        }
        lapply(needCreate, dir.create, recursive = TRUE)
      }
      isDir <- dir.exists(to)
      dups <- duplicated(from)

      # Try hard link first -- the only type that R deeply recognizes
      result <- captureWarningsToAttr(
        file.link(from[!dups & !isDir], to[!dups & !isDir])
      )
      warns <- attr(result, "warning")
      attr(result, "warning") <- NULL

      if (isTRUE(all(result))) {
        messagePrepInputs(hardlinkMessagePrefix, ":\n", toCollapsed, "\n",
          whPointsToMess, "\n",
          fromCollapsed, "\n... no copy/copies made.",
          verbose = verbose
        )
      }

      if (any(grepl("file already exists", warns))) {
        messagePrepInputs("File named ", toCollapsed, " already exists; will try to use it/them", verbose = verbose)
        result <- TRUE
      }

      # On *nix types -- try symlink
      if (isFALSE(all(result)) && isTRUE(symlink)) {
        if (!isWindows()) {
          result <- suppressWarnings(file.symlink(from[!result], to[!result]))
          if (isTRUE(all(result))) {
            messagePrepInputs("Symlinked version of file created at: ", toCollapsed, ", ", whPointsToMess, " ",
              fromCollapsed, "; no copy was made.",
              verbose = verbose
            )
          }
        }
      }

      if (isFALSE(all(result))) {
        result <- file.copy(from[!result], to[!result], overwrite = overwrite)
        messagePrepInputs("Copy of file: ", fromCollapsed, ", was created at: ", toCollapsed, verbose = verbose)
      }
    } else {
      messagePrepInputs("File ", fromCollapsed, " does not exist. Not copying.", verbose = verbose)
      result <- FALSE
    }
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
                                   targetFilePath,
                                   quick, verbose = getOption("reproducible.verbose", 1),
                                   .tempPath) {
  if (missing(.tempPath)) {
    .tempPath <- tempdir2(rndstr(1, 6))
    on.exit(
      {
        unlink(.tempPath, recursive = TRUE)
      },
      add = TRUE
    )
  }
  alsoExtract <- grep("none$", alsoExtract, value = TRUE, invert = TRUE) # remove "none" from neededFiles; for extracting
  neededFiles <- c(neededFiles, if (!isNULLorNA(alsoExtract)) alsoExtract)
  neededFiles <- setdiff(neededFiles, "similar") # remove "similar" from neededFiles; for extracting
  neededFiles <- unique(makeAbsolute(neededFiles, destinationPath)) # unique is b/c neededFiles was absolute and alsoExtract was rel
  alsoExtract <- makeAbsolute(alsoExtract, destinationPath)

  filesExtr <- NULL
  if (!isNULLorNA(archive)) {
    # if (!is.na(archive)) {
    if (any(file.exists(archive))) {
      filesExtracted <- extractFromArchive(
        archive = archive, destinationPath = destinationPath,
        neededFiles = neededFiles,
        checkSums = checkSums, needChecksums = needChecksums,
        checkSumFilePath = checkSumFilePath, quick = quick,
        verbose = verbose,
        .tempPath = .tempPath
      )
      neededFiles <- filesExtracted$neededFiles # will have been potentially corrected if user supplied incorrect relative paths

      targetFilePath <- checkRelative(targetFilePath, destinationPath, neededFiles, verbose = verbose - 1)
      filesToChecksum <- checkRelative(filesToChecksum, destinationPath, neededFiles, verbose = verbose - 1)

      checkSums <- .checkSumsUpdate(
        destinationPath = destinationPath,
        newFilesToCheck = filesExtracted$filesExtracted,
        checkSums = filesExtracted$checkSums, verbose = verbose
      )

      # filesToChecksum may be wrong because of relative path without subfolder
      filesToChecksum <- unique(c(
        filesToChecksum, targetFilePath, # alsoExtract, # alsoExtract will be in filesExtracted$filesExtr
        filesExtracted$filesExtr
      ))

      needChecksums <- filesExtracted$needChecksums
      data.table::setDT(filesExtracted$checkSums)
      dontNeedChecksums <- if (NROW(filesExtracted$checkSums) > 0) {
        filesExtracted$checkSums[
          filesExtracted$checkSums$expectedFile %in%
            filesToChecksum & compareNA(result, "OK"),
          expectedFile
        ]
      } else {
        dontNeedChecksums <- character()
      }
      filesToChecksum <- setdiff(filesToChecksum, dontNeedChecksums)

      if (needChecksums > 0) {
        checkSums <- appendChecksumsTable(
          checkSumFilePath = checkSumFilePath,
          filesToChecksum = unique(filesToChecksum),
          destinationPath = destinationPath,
          append = needChecksums >= 2
        )
        needChecksums <- 0
      }

      ## targetFilePath might still be NULL, need destinationPath too
      filesExtr <- unique(c(
        filesToChecksum,
        if (is.null(filesExtracted$filesExtr) ||
          length(filesExtracted$filesExtr) == 0) {
          character()
        } # downloadFileResult$downloaded
        else {
          filesExtracted$filesExtr
        }
      ))
    }
    # }
  }
  if (!is.null(filesExtr)) {
    filesExtr <- unique(filesExtr)
  }
  list(
    filesToChecksum = filesToChecksum, filesExtr = filesExtr,
    needChecksums = needChecksums,
    targetFilePath = targetFilePath,
    neededFiles = neededFiles, checkSums = checkSums
  )
}

#' @keywords internal
.decodeMagicNumber <- function(magicNumberString) {
  fileExt <- if (any(grepl(pattern = "Zip", x = magicNumberString))) {
    ".zip"
  } else if (any(grepl(pattern = "RAR", x = magicNumberString))) {
    ".rar"
  } else if (any(grepl(pattern = "tar", x = magicNumberString))) {
    ".tar"
  } else if (any(grepl(pattern = "TIFF", x = magicNumberString))) {
    ".tif"
  } else if (any(grepl(pattern = "Shapefile", x = magicNumberString))) {
    ".shp"
  } else {
    NULL
  }
  return(fileExt)
}

#' @keywords internal
.guessFileExtension <- function(file) {
  if (isWindows()) {
    tryCatch(
      {
        possLocs <- c(
          "C:/cygwin/bin/file.exe",
          "C:\\cygwin64/bin/file.exe"
        )
        findFile <- file.exists(possLocs)
        if (any(findFile)) {
          fileLoc <- possLocs[findFile][1]
        }

        magicNumber <- captureWarningsToAttr(
          system(paste(fileLoc, file), intern = TRUE)
        )
        warn <- attr(magicNumber, "warning")
        attr(magicNumber, "warning") <- NULL

        if (length(warn) > 0) {
          splitted <- unlist(strsplit(x = file, split = ":/"))
          fileAdapted <- file.path(paste0("/mnt/", tolower(splitted[1])), splitted[2])
          magicNumber <- captureWarningsToAttr(
            shell(paste0("'file ", fileAdapted, "'"), "bash",
              intern = TRUE,
              wait = TRUE, translate = FALSE, mustWork = TRUE
            )
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
      },
      error = function(e) {
        fileExt <- NULL
        return(fileExt)
      }
    )
  } else {
    magicNumber <- system(paste0("file ", file), wait = TRUE, intern = TRUE)
    fileExt <- .decodeMagicNumber(magicNumberString = magicNumber)
    return(fileExt)
  }
}

#' @keywords internal
.fixNoFileExtension <- function(downloadFileResult, targetFile, archive,
                                destinationPath, verbose = getOption("reproducible.verbose", 1)) {
  needFinalCopy <- TRUE
  if (!is.null(downloadFileResult$downloaded) &&
    identical(fileExt(downloadFileResult$downloaded), "")) {
    if (!is.null(targetFile) && !identical(fileExt(normPath(basename2(downloadFileResult$neededFiles))), "")) {
      if (is.null(archive)) {
        messagePrepInputs(
          "Downloaded file has no extension: targetFile is provided, but archive is not.\n",
          " Downloaded file will be considered as the targetFile. If the downloaded file is an archive\n",
          " that contains the targetFile, please specify both archive and targetFile.",
          verbose = verbose
        )
        newFileWithExtension <- downloadFileResult$neededFiles
      } else {
        messagePrepInputs(
          "Downloaded file has no extension: both targetFile and archive are provided.\n",
          " Downloaded file will be considered as the archive.",
          verbose = verbose
        )
        newFileWithExtension <- downloadFileResult$archive
      }
    } else {
      if (!is.null(archive)) {
        messagePrepInputs(
          "Downloaded file has no extension: archive is provided. \n",
          " downloaded file will be considered as the archive.",
          verbose = verbose
        )
        downloadFileResult$neededFiles <- archive
        newFileWithExtension <- downloadFileResult$neededFiles
      } else {
        messagePrepInputs(
          "Downloaded file has no extension: neither archive nor targetFile are provided. \n",
          "prepInputs will try accessing the file type.",
          verbose = verbose
        )
        fileExt <- .guessFileExtension(file = normPath(downloadFileResult$downloaded))
        if (is.null(fileExt)) {
          messagePrepInputs("The file was not recognized by prepInputs. ",
            "Will assume the file is an archive and add '.zip' extension. ",
            "If this is incorrect or return error, please supply archive or targetFile",
            verbose = verbose
          )
          fileExt <- ".zip"
        }
        newFileWithExtension <- paste0(downloadFileResult$neededFiles, fileExt)
        hardLinkOrCopy(downloadFileResult$neededFiles, newFileWithExtension, verbose = 0)
        needFinalCopy <- FALSE
        downloadFileResult$neededFiles <- makeAbsolute(.listFilesInArchive(newFileWithExtension), destinationPath)
        downloadFileResult$archive <- newFileWithExtension
        downloadFileResult$targetFilePath <- normPath(downloadFileResult$neededFiles)
      }
    }
    if (isTRUE(needFinalCopy)) {
      hardLinkOrCopy(
        verbose = 0,
        from = normPath(downloadFileResult$downloaded),
        to = normPath(newFileWithExtension)
      )
    }
    downloadFileResult$downloaded <- newFileWithExtension
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

      for (i in length(sourceAttributes)) {
        attr(receiving, names(sourceAttributes)[i]) <- sourceAttributes[[i]]
      }
    }
  }
  receiving
}

.checkDeprecated <- function(dots, verbose = getOption("reproducible.verbose", 1)) {
  if (!is.null(dots$cacheTags)) {
    messagePrepInputs("cacheTags is being deprecated;",
      " use userTags which will pass directly to Cache.",
      verbose = verbose
    )
    dots$userTags <- dots$cacheTags
    dots$cacheTags <- NULL
  }
  if (!is.null(dots$postProcessedFilename)) {
    messagePrepInputs("postProcessedFilename is being deprecated;",
      " use filename2, used in determineFilename.",
      verbose = verbose
    )
    dots$filename2 <- dots$postProcessedFilename
    dots$postProcessedFilename <- NULL
  }
  if (!is.null(dots$writeCropped)) {
    messagePrepInputs("writeCropped is being deprecated;",
      " use filename2, used in determineFilename.",
      verbose = verbose
    )
    dots$filename2 <- dots$writeCropped
    dots$writeCropped <- NULL
  }
  if (!is.null(dots$rasterInterpMethod)) {
    messagePrepInputs("rasterInterpMethod is being deprecated;",
      " use method which will pass directly to projectRaster.",
      verbose = verbose
    )
    dots$method <- dots$rasterInterpMethod
    dots$rasterInterpMethod <- NULL
  }
  if (!is.null(dots$rasterDatatype)) {
    messagePrepInputs("rasterDatatype is being deprecated;",
      " use datatype which will pass directly to writeRaster.",
      verbose = verbose
    )
    dots$datatype <- dots$rasterDatatype
    dots$rasterDatatype <- NULL
  }
  if (!is.null(dots$pkg)) {
    messagePrepInputs("pkg is being deprecated;",
      "name the package and function directly, if needed,\n",
      "  e.g., 'pkg::fun'.",
      verbose = verbose
    )
    dots$pkg <- NULL
  }

  dots
}

.checkFunInDots <- function(fun = NULL, dots) {
  if (is.null(fun)) {
    if (!is.null(dots$pkg)) {
      fun <- paste0(dots$pkg, "::", fun)
    }
  }
  fun
}

hardLinkOrCopy <- function(from, to, overwrite = FALSE, verbose = TRUE) {
  linkOrCopy(from, to, symlink = FALSE, verbose = verbose)
}

escapeRegexChars <- function(str, repl = c("(", ")")) {
  for (r in repl) {
    str <- gsub(paste0("\\", r, ""), paste0("\\\\", r), str)
  }
  str
}

hardlinkMessagePrefix <- "Hardlinked version of file(s) created at"
hardlinkMessagePrefixForGrep <- escapeRegexChars(hardlinkMessagePrefix)

whPointsToMess <- "which point(s) to"
whPointsToMessForGrep <- escapeRegexChars(whPointsToMess)

getTeamDrive <- function(dots) {
  if (requireNamespace("googledrive", quietly = TRUE)) {
    teamDrive <- if (packageVersion("googledrive") < "2.0.0") {
      dots[["team_drive"]]
    } else {
      dots[["shared_drive"]]
    }
  } else {
    teamDrive <- NULL
  }
}

getTargetFilePath <- function(targetFile, archive, fileGuess, verbose,
                              destinationPath, alsoExtract, checkSumFilePath) {
  if (is.null(targetFile)) {
    if ((is.null(archive) || is.na(archive)) && !is.null(fileGuess)) {
      messagePrepInputs("targetFile was not supplied; guessed and will try ", fileGuess,
        ". If this is incorrect, please supply targetFile",
        verbose = verbose
      )
      targetFile <- makeRelative(fileGuess, destinationPath)
      targetFilePath <- makeAbsolute(targetFile, destinationPath)
    } else {
      targetFilePath <- NULL
    }
  } else {
    if (length(targetFile) > 1) {
      stop("targetFile should be only 1 file")
    }

    targetFilePath <- normPath(makeAbsolute(targetFile, destinationPath))
  }
  targetFilePath
}

guessAlsoExtract <- function(targetFile, alsoExtract, checkSumFilePath) {
  if (is.null(alsoExtract)) {
    if (file.exists(checkSumFilePath)) {
      if (file.size(checkSumFilePath) > 0) {
        # if alsoExtract is not specified, then try to find all files in CHECKSUMS.txt with
        # same base name, without extension
        checksumsTmp <- as.data.table(read.table(checkSumFilePath))
        alsoExtract <- grep(paste0(filePathSansExt(targetFile), "\\."), checksumsTmp$file,
          value = TRUE
        )
        rm(checksumsTmp) # clean up
      }
    }
  }
  if (any(is.na(alsoExtract))) {
    alsoExtract <- NA
  } else if (!is.null(alsoExtract)) { # must keep relative because user may not know what path is in archive
    if (isTRUE(all(is.na(alsoExtract)))) {
      alsoExtract <- character()
    }
  }

  alsoExtract
}

updateArchiveWithGuess <- function(archive, guess) {
  if (!is.null(guess)) {
    if (is.null(archive)) {
      archive <- .isArchive(guess)
    }
    if (isTRUE(!is.na(archive))) {
      archive <- moveAttributes(guess, archive)
    }
  }
  archive
}

setupArchive <- function(archive, destinationPath) {
  if (!is.null(archive)) {
    if (!is.na(archive)) {
      tmpArchive <- archive
      archive <- makeAbsolute(archive, destinationPath)
      # archive <- file.path(destinationPath, basename2(archive))
      # filesToCheck <- unique(c(filesToCheck, archive))
      archive <- moveAttributes(tmpArchive, archive)
    }
  }
  archive
}

runChecksums <- function(destinationPath, checkSumFilePath, filesToCheck, verbose) {
  reproducible.inputPaths <- getOption("reproducible.inputPaths", NULL)
  if (!is.null(reproducible.inputPaths)) {
    reproducible.inputPaths <- checkPath(reproducible.inputPaths, create = TRUE)
  }
  if (!is.null(reproducible.inputPaths)) {
    reproducible.inputPaths <- path.expand(reproducible.inputPaths)
  }

  destinationPathUser <- NULL
  for (dp in unique(c(destinationPath, reproducible.inputPaths))) {
    csfp <- identifyCHECKSUMStxtFile(dp)
    checkSumsTmp1 <- try(Checksums(
      path = dp, write = FALSE, checksumFile = csfp,
      files = makeRelative(filesToCheck, absoluteBase = destinationPath),
      verbose = verbose
    ), silent = TRUE)
    checkSums <- NULL
    if (!is(checkSumsTmp1, "try-error")) {
      checkSums <- checkSumsTmp1
      if (!all(is.na(checkSums$result))) { # found something
        if (identical(dp, reproducible.inputPaths)) {
          destinationPathUser <- destinationPath
          destinationPath <- dp
          on.exit(
            {
              destinationPath <- destinationPathUser
            },
            add = TRUE
          )
        }
        break
      }
    }
  }
  list(
    reproducible.inputPaths = reproducible.inputPaths,
    destinationPathUser = destinationPathUser,
    destinationPath = destinationPath,
    checkSums = checkSums
  )
}

dealWithArchive <- function(archive, url, targetFile, checkSums, alsoExtract, destinationPath, teamDrive, verbose) {
  fileGuess <- NULL
  if (is.null(archive)) {
    if (!is.null(url)) {
      allOK <- .similarFilesInCheckSums(targetFile, checkSums, alsoExtract)

      if (!allOK) { # skip identification of archive if we have all files with same basename as targetFile
        # BUT if we don't have all files with identical root name (basename sans ext), then assess for
        #   an archive, either remotely, in the case of google or from the basename of url
        fileGuess <- .guessAtFile(
          url = url, archive = archive,
          targetFile = targetFile, destinationPath = destinationPath,
          verbose = verbose, team_drive = teamDrive
        )
        archive <- .isArchive(fileGuess)
        # The fileGuess MAY have a fileSize attribute, which can be attached to "archive"
        archive <- moveAttributes(fileGuess, receiving = archive)
        sourceAttributes <- attributes(fileGuess)
        if (length(sourceAttributes) > 0 && !is.null(archive)) {
          for (i in length(sourceAttributes)) {
            attr(archive, names(sourceAttributes)[i]) <- sourceAttributes[[i]]
          }
        }

        checkSums <- .checkSumsUpdate(
          destinationPath = destinationPath,
          newFilesToCheck = archive,
          checkSums = checkSums,
          verbose = verbose
        )
      }
    }
  }
  list(
    checkSums = checkSums,
    archive = archive,
    fileGuess = fileGuess
  )
}

isNULLorNA <- function(x) {
  out <- TRUE
  if (!is.null(x)) {
    if (!isTRUE(is.na(x))) {
      out <- FALSE
    }
  }
  out
}

identifyCHECKSUMStxtFile <- function(path) {
  file.path(path, "CHECKSUMS.txt")
}
