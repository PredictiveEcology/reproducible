#' A wrapper around a set of downloading functions
#'
#' Currently, this only deals with \code{\link[googledrive]{drive_download}},
#' \code{\link{downloadData}}, and \code{\link[utils]{download.file}}.
#'
#' @export
#' @inheritParams prepInputs
#' @inheritParams extractFromArchive
#' @param moduleName Character string indicating SpaDES module name from which prepInputs is
#'                    being called
#'                    being called
#' @param modulePath Character string of the path where the \code{moduleName} is located.
#' @author Eliot McIntire
downloadFile <- function(archive, targetFile, neededFiles, destinationPath, quick,
                         checkSums, url, needChecksums, overwrite = TRUE, moduleName, modulePath, ...) {

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

  if (!is.null(neededFiles)) {
    result <- checkSums[checkSums$expectedFile %in% neededFiles, ]$result
  } else {
    result <- unique(checkSums$result)
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
    if (!is.null(moduleName)) { # means it is inside a SpaDES module
      if (!is.null(fileToDownload)) {
        # downloadData(moduleName, modulePath, files = fileToDownload,
        #              checked = checkSums, quickCheck = quick, overwrite = overwrite,
        #              urls = urls)
        download.file ## TODO: use httr for download
      }
    } else {
      # The ad hoc case
      if (!is.null(fileToDownload) ) {#|| is.null(targetFile)) {
        if (!is.null(url)) {
          if (grepl("drive.google.com", url)) {
            dlGoogle(url)
          } else {
            destFile <- file.path(tempdir(), basename(url))
            download.file(url, destfile = destFile)
          }
          # if destinationPath is tempdir, then don't copy and remove
          if (!(identical(dirname(destFile),
                          normalizePath(destinationPath, winslash = "/", mustWork = FALSE)))) {
            suppressWarnings(file.copy(destFile, destinationPath))
            suppressWarnings(file.remove(destFile))
          }

        }
      } else {
        message(skipDownloadMsg)
        needChecksums <- 0
      }
    }
  } else {
    if (is.null(targetFile)) {
      message("   Skipping download because all files listed in CHECKSUMS.txt file are present.",
              " If this is not correct, rerun prepInputs with purge = TRUE")
    } else {
      message("  Skipping download: targetFile (and any alsoExtract) already present")
    }
  }
  archiveReturn <- if (is.null(archive)) archive else file.path(destinationPath, basename(archive))
  list(needChecksums = needChecksums, archive = archiveReturn, neededFiles = neededFiles)
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
dlGoogle <- function(url) { ## TODO: add additional arguments per below
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
}
