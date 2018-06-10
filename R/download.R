#' A wrapper around a set of downloading functions
#'
#' Currently, this only deals with \code{\link[googledrive]{drive_download}},
#' and \code{\link[utils]{download.file}}.
#'
#' @export
#' @inheritParams prepInputs
#' @include checksums.R
#' @inheritParams extractFromArchive
#' @param checksumFile A character string indicating the absolute path to the \code{CHECKSUMS.txt}
#'                     file.
#' @author Eliot McIntire
downloadFile <- function(archive, targetFile, neededFiles, destinationPath, quick,
                         checksumFile,
                         checkSums, url, needChecksums, overwrite = TRUE) { #}, moduleName, modulePath, ...) {

  if (!is.null(neededFiles)) {
    if ("shp" %in% file_ext(neededFiles)) { # if user wants .shp file, needs other anciliary files
      # but not all
      shpfileBase <- gsub(".shp$", "", neededFiles[file_ext(neededFiles) %in% "shp"])
      reqdShpFiles <- paste0(shpfileBase, ".", c("shx", "dbf", "prj", "sbx", "sbn"))
      if (length(neededFiles) > 0) {
        if (identical(FALSE, (all(reqdShpFiles %in% neededFiles)))) {
          optionalShpFiles <- paste0(shpfileBase, ".", c("cpg", "shp.xml"))
          otherShpfiles <- c(reqdShpFiles, optionalShpFiles)
          neededFiles <- unique(c(neededFiles, otherShpfiles))
        }
      }

    }
  }

  if (is.null(neededFiles)) {
    result <- unique(checkSums$result)
    if (NROW(checkSums))
      neededFiles <- checkSums$expectedFile
  } else {
    result <- checkSums[checkSums$expectedFile %in% neededFiles, ]$result
  }
  missingNeededFiles <- (!(all(compareNA(result, "OK")) && all(neededFiles %in% checkSums$expectedFile)) ||
                           is.null(targetFile) || is.null(neededFiles))
  if (missingNeededFiles) {
    if (needChecksums == 0) needChecksums <- 2 # use binary addition -- 1 is new file, 2 is append
  }

  if (missingNeededFiles) {
    fileToDownload <- if (is.null(archive[1])) {
      neededFiles
    } else {
      result <- checkSums[checkSums$expectedFile %in% basename(archive[1]), ]$result
      missingArchive <- !isTRUE(result == "OK")
      if (missingArchive) {
        archive[1]
      } else {
        NULL # means nothing to download because the archive is already in hand
      }
    }
    skipDownloadMsg <- "Skipping download of url; local copy already exists and passes checksums"

    # The download step
    downloadResults <- downloadRemote(url = url, archive = archive,
                   targetFile = targetFile, fileToDownload = fileToDownload,
                   skipDownloadMsg = skipDownloadMsg,
                   checkSums = checkSums,
                   destinationPath = destinationPath,
                   overwrite = overwrite,
                   needChecksums = needChecksums)
    if (file.exists(checksumFile)) {
      if (!is.null(fileToDownload))  {
        res <- Checksums(files = file.path(destinationPath, fileToDownload), checksumFile = checksumFile,
                       path = destinationPath, quickCheck = quick,
                       write = FALSE)
        isOK <- res[compareNA(res$expectedFile, fileToDownload) | compareNA(res$actualFile, fileToDownload),]$result
        isOK <- isOK[!is.na(isOK)] == "OK"
        if (length(isOK) > 0) {
          if (!isTRUE(all(isOK))) {
            stop("Checksums for ", fileToDownload, " from url: ", url, " failed checksum test. Please try download again, ",
                 "or if the local file(s) is/are correct, rerun checksums(write = TRUE, ...) on the local files")
          }
        }
      }
    }
  } else {
    fileAlreadyDownloaded <- if (is.null(archive[1])) {
      archivePossibly <- setdiff(checkSums$expectedFile, neededFiles)
      if (!is.null(.isArchive(archivePossibly))) {
        archivePossibly
      } else {
        neededFiles
      }

    } else {
      archive[1]
    }

    downloadResults <- list(needChecksums = needChecksums, destFile = fileAlreadyDownloaded)
    if (is.null(targetFile)) {
      message("   Skipping download because all files listed in CHECKSUMS.txt file are present.",
              " If this is not correct, rerun prepInputs with purge = TRUE")
    } else {
      message("  Skipping download: targetFile (and any alsoExtract) already present")
    }
  }
  archiveReturn <- if (is.null(archive)) {
    .isArchive(downloadResults$destFile)
  } else {
    archive
  }
  list(needChecksums = downloadResults$needChecksums, archive = archiveReturn, neededFiles = neededFiles,
       downloaded = downloadResults$destFile)
}

.getSourceURL <- function(pattern, x) {
  srcURL <- "sourceURL"
  grepIndex <- grep(srcURL, x = x)
  if (length(grepIndex) == 1) {
    .getSourceURL(pattern, x[[grepIndex]])
  } else if (length(grepIndex) > 1 | length(grepIndex) == 0) {
    y <- grep(pattern = basename(pattern), x)
    if (length(y) == 1) {
      urls <- if (length(grepIndex) > 1)
        eval(x[[y]])$sourceURL
      else
        eval(x)$sourceURL
    } else {
      stop("There is no sourceURL for an object named ", basename(pattern))
    }
  } else {
    stop("There is no sourceURL in module metadata")
  }
}

#' Download file from Google Drive
#'
#' @param url  The url (link) to the file.
#'
#' @author Eilot McIntire and Alex Chubaty
#' @keywords internal
#' @importFrom googledrive as_id drive_auth drive_get
#'
dlGoogle <- function(url, archive = NULL, targetFile = NULL,
                     checkSums, skipDownloadMsg, destinationPath,
                     overwrite, needChecksums) { ## TODO: add additional arguments per below
  googledrive::drive_auth() ## neededFiles for use on e.g., rstudio-server
  if (is.null(archive)) {
    fileAttr <- googledrive::drive_get(googledrive::as_id(url))
    archive <- .isArchive(fileAttr$name)
    archive <- file.path(destinationPath, basename(archive))
    downloadFilename <- archive
    if (is.null(archive)) {
      if (is.null(targetFile)) {
        # make the guess
        targetFile <- fileAttr$name
        downloadFilename <- targetFile # override if the targetFile is not an archive
      }
    }
  } else {
    downloadFilename <- archive
  }
  destFile <- file.path(tempdir(), basename(downloadFilename))
  if (!isTRUE(checkSums[checkSums$expectedFile ==  basename(destFile), ]$result == "OK")) {
    message("  Downloading from Google Drive.")
    googledrive::drive_download(googledrive::as_id(url), path = destFile,
                                overwrite = overwrite, verbose = TRUE)
  } else {
    message(skipDownloadMsg)
    needChecksums <- 0
  }
  return(list(needChecksums = needChecksums, destFile = destFile))
}

#' Download file from generic source url
#'
#' @param url  The url (link) to the file.
#'
#' @author Eilot McIntire and Alex Chubaty
#' @keywords internal
#' @importFrom utils download.file
#'
dlGeneric <- function(url, needChecksums) {
  destFile <- file.path(tempdir(), basename(url))
  # TODO
   # if (httr::http_error(url))
   #    stop("Can not access url ", url)
   #
   #  message("  Downloading ", filename, " ...")
   #
   #  httr::GET(
   #    url = paste0(url, filename),
   #    authenticate,
   #    httr::progress(),
   #    httr::write_disk(filepath, overwrite = overwrite)
   #  )
  download.file(url, destfile = destFile, method = "auto", mode = "wb")
  list(needChecksums = needChecksums, destFile = destFile)
}

downloadRemote <- function(url, archive, targetFile, checkSums,
                           moduleName, fileToDownload, skipDownloadMsg,
                           destinationPath, overwrite, needChecksums) {
    if (!is.null(fileToDownload) ) {#|| is.null(targetFile)) {
      if (!is.null(url)) {
        if (grepl("drive.google.com", url)) {
          downloadResults <-
            dlGoogle(
              url = url,
              archive = archive,
              targetFile = targetFile,
              checkSums = checkSums,
              skipDownloadMsg = skipDownloadMsg,
              destinationPath = destinationPath,
              overwrite = overwrite,
              needChecksums = needChecksums
            )
        } else if (grepl("dl.dropbox.com", url)) {
          stop("Dropbox downloading is currently not supported")
        } else if (grepl("onedrive.live.com", url)) {
          stop("Onedrive downloading is currently not supported")
        } else {
          downloadResults <- dlGeneric(url = url, needChecksums = needChecksums)
        }
        # if destinationPath is tempdir, then don't copy and remove
        if (!(identical(dirname(downloadResults$destFile),
                        normalizePath(destinationPath, winslash = "/", mustWork = FALSE)))) {
          suppressWarnings(file.copy(downloadResults$destFile, destinationPath))
          suppressWarnings(file.remove(downloadResults$destFile))
          downloadResults$destFile <- file.path(destinationPath, basename(downloadResults$destFile))
        }

      }
    } else {
      message(skipDownloadMsg)
      downloadResults <- list(needChecksums = 0, destFile = NULL)
    }
  downloadResults
}
