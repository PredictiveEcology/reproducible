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
#' @author Eliot McIntire
#' @export
#' @inheritParams prepInputs
#' @inheritParams downloadFile
preProcess <- function(targetFile = NULL, url = NULL, archive = NULL, alsoExtract = NULL,
                       destinationPath = ".", fun = NULL, dlFun = NULL,
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
    alsoExtract <- if (isTRUE(all(is.na(alsoExtract)))) {
      character()
    } else {
      file.path(destinationPath, basename(alsoExtract))
    }
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
  checkSums <- try(Checksums(path = destinationPath, write = FALSE,
                             files = filesToCheck), silent = TRUE)

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
  neededFiles <- setdiff(neededFiles, "similar") # remove "similar" from needed files. It is for extracting.

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
  if (!is.null(alsoExtract)) {
    if ("similar" %in% basename(alsoExtract)) {
      allFiles <- .listFilesInArchive(archive)
      neededFiles <- if (is.null(targetFile)) {
        message("No targetFile supplied, so can't use \"alsoExtract = 'similar'\". ",
                "Extracting all files from archive")
        allFiles
      } else {
        filePatternToKeep <- gsub(basename(targetFile),
                                  pattern = file_ext(basename(targetFile)), replacement = "")
        filesToGet <- grep(allFiles, pattern = filePatternToKeep, value = TRUE)
        unique(c(neededFiles, filesToGet))
      }
    }
  }

  if (is.null(alsoExtract)) neededFiles <- unique(c(neededFiles, .listFilesInArchive(archive)))

  # don't include targetFile in neededFiles -- extractFromArchive deals with it separately
  if (length(neededFiles) > 1) alsoExtract <- setdiff(neededFiles, targetFile)

  # To this point, we only have the archive in hand -- include this in the list of filesToChecksum
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
                                       checkSums = checkSums, needChecksums = needChecksums,
                                       checkSumFilePath = checkSumFilePath, quick = quick)

  filesToChecksum <- unique(c(filesToChecksum, targetFile, alsoExtract,
                              basename(filesExtracted$filesExtracted)))
  needChecksums <- filesExtracted$needChecksums

  #targetFilePath might still be NULL, need destinationPath too
  filesExtr <- c(filesToChecksum,
                                 if (is.null(filesExtracted$filesExtracted) ||
                                     length(filesExtracted$filesExtracted) == 0)
                                   downloadFileResult$downloaded
                                 else
                                   filesExtracted$filesExtracted)
  if (!is.null(filesExtr)) filesExtr <- unique(basename(filesExtr))
  targetParams <- .guessAtTargetAndFun(targetFilePath, destinationPath,
                                       filesExtracted = filesExtr,
                                       fun) # passes through if all known
  targetFile <- basename(targetParams$targetFilePath)
  targetFilePath <- targetParams$targetFilePath
  fun <- targetParams$fun

  #targetFilePath might still be NULL, need destinationPath too
  if (is.null(targetFilePath)) if (is.null(filesExtracted$filesExtracted)) {
    if (!is.null(downloadFileResult$downloaded))
      targetFilePath <- downloadFileResult$downloaded
  } else {
    targetFilePath <- filesExtracted$filesExtracted
  }

  if (is.null(targetFile)) if (!is.null(targetFilePath)) targetFile <- basename(targetFilePath)

  # Convert the fun as character string to function class, if not already
  fun <- .extractFunction(fun)

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
  if (!isTRUE(file.exists(targetFilePath))) {
    stop("targetFile appears to be misspecified. ",
         "Possibly, it does not exist in the specified archive, ",
         "or the file doesn't exist in destinationPath")
  }

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
      "2" = checkSums[!(checkSums$expectedFile %in% basename(targetFile)), ],
      "3" = checkSums[!(checkSums$expectedFile %in% basename(archive)), ],
      "4" = checkSums[!(checkSums$expectedFile %in% basename(alsoExtract)), ],
      "5" = checkSums[!(checkSums$expectedFile %in% basename(unique(c(targetFile, alsoExtract)))), ], #nolint
      "6" = checkSums[!(checkSums$expectedFile %in% basename(unique(c(targetFile, alsoExtract, archive)))), ], #nolint
      "7" = checkSums[!(checkSums$expectedFile %in% basename(url)), ] #nolint
    ), error = function(x) checkSums)
  checkSums
}

.emptyChecksumsResult <- data.table(expectedFile = character(), actualFile = character(), result = character())
.emptyChecksumsFileContent <- data.frame(file = character(), checksum = character(), filesize = character(),
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
