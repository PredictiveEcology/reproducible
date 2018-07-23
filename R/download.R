#' A wrapper around a set of downloading functions
#'
#' Currently, this only deals with \code{\link[googledrive]{drive_download}},
#' and \code{\link[utils]{download.file}}.
#'
#' @export
#' @inheritParams prepInputs
#' @include checksums.R
#' @inheritParams extractFromArchive
#' @param dlFun Optional "download function" name, such as \code{"raster::getData"}, which does
#'              custom downloading, in addition to loading into R. Still experimental.
#' @param ... Passed to \code{dlFun}. Still experimental.
#' @param checksumFile A character string indicating the absolute path to the \code{CHECKSUMS.txt}
#'                     file.
#' @author Eliot McIntire
downloadFile <- function(archive, targetFile, neededFiles, destinationPath, quick,
                         checksumFile, dlFun = NULL,
                         checkSums, url, needChecksums, overwrite = TRUE,
                         purge = FALSE, ...) {

  if (!is.null(url) || !is.null(dlFun)) {

    if (is.null(neededFiles)) {
      result <- unique(checkSums$result)
    } else {
      result <- checkSums[checkSums$expectedFile %in% neededFiles, ]$result
    }
    if (length(result) == 0) result <- NA

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
          NA # means nothing to download because the archive is already in hand
        }
      }
      skipDownloadMsg <- "Skipping download of url; local copy already exists and passes checksums"

      # The download step
      downloadResults <- downloadRemote(url = url, archive = archive, # both url and fileToDownload must be NULL to skip downloading
                     targetFile = targetFile, fileToDownload = fileToDownload,
                     skipDownloadMsg = skipDownloadMsg,
                     checkSums = checkSums,
                     dlFun = dlFun,
                     destinationPath = destinationPath,
                     overwrite = overwrite,
                     needChecksums = needChecksums, ...)
      if (file.exists(checksumFile)) {
        if (is.null(fileToDownload) || tryCatch(is.na(fileToDownload), warning = function(x) FALSE))  { # This is case where we didn't know what file to download, and only now
                                        # do we know
          fileToDownload <- downloadResults$destFile
        }
        if (!is.null(fileToDownload)) {
          if ((length(readLines(checksumFile)) > 0)) {
            checkSums <-
              Checksums(
                files = file.path(destinationPath, basename(fileToDownload)),
                checksumFile = checksumFile,
                path = destinationPath,
                quickCheck = quick,
                write = FALSE
              )
            isOK <-
              checkSums[checkSums$expectedFile %in% basename(fileToDownload) |
                          checkSums$actualFile %in% basename(fileToDownload),]$result
            isOK <- isOK[!is.na(isOK)] == "OK"
            if (length(isOK) > 0) {
              if (!isTRUE(all(isOK))) {
                if (purge > 0)  {
                  # This is case where we didn't know what file to download, and only now
                  # do we know
                  checkSums <-
                    .purge(checkSums = checkSums,
                           purge = purge,
                           url = fileToDownload)
                  downloadResults$needChecksums <- 2
                } else {
                  tf <-
                    tryCatch(
                      basename(targetFile) %in% fileToDownload,
                      error = function(x)
                        FALSE
                    )
                  af <-
                    tryCatch(
                      basename(archive) %in% fileToDownload,
                      error = function(x)
                        FALSE
                    )

                  sc <- sys.calls()
                  piCall <- grep("^prepInputs", sc, value = TRUE)
                  purgeTry <- if (length(piCall)) {
                    gsub(piCall,
                         pattern = ")$",
                         replacement = paste0(", purge = 7)"))
                  } else {
                    ""
                  }
                  stop(
                    "\nDownloaded version of ",
                    normPath(fileToDownload),
                    " from url: ",
                    url,
                    " did not match expected file (checksums failed). There are several options:\n",
                    " 1) This may be an intermittent internet problem -- try to rerun this ",
                    "current function call.\n",
                    " 2) The local copy of the file may have been changed or corrupted -- run:\n",
                    "      file.remove('",
                    normPath(fileToDownload),
                    "')\n",
                    "      then rerun this current function call.\n",
                    " 3) The download is correct, and the Checksums should be rewritten for this file:\n",
                    "      --> rerun this current function call, specifying 'purge = 7' possibly\n",
                    "      ",
                    purgeTry,
                    call. = FALSE
                  )


                }
              } else if (isTRUE(all(isOK))) {
                downloadResults$needChecksums <- 0
              }
            }
          }
        }
      } # checksum file doesn't exist
    } else { # not missing any files to download
      fileAlreadyDownloaded <- if (is.null(archive[1])) {
        archivePossibly <- setdiff(checkSums$expectedFile, neededFiles)
        archivePossibly <- .isArchive(archivePossibly)
        if (!is.null(archivePossibly)) {
          archivePossibly
        } else {
          neededFiles
        }

      } else {
        archive
      }

      downloadResults <- list(needChecksums = needChecksums,
                              destFile = file.path(destinationPath, basename(fileAlreadyDownloaded)))
      if (is.null(targetFile)) {
        message("   Skipping download because all files listed in CHECKSUMS.txt file are present.",
                " If this is not correct, rerun prepInputs with purge = TRUE")
      } else {
        message("  Skipping download: ", paste(neededFiles, collapse = ", ") ," already present")
      }
    }
    archiveReturn <- if (is.null(archive)) {
      .isArchive(downloadResults$destFile)
    } else {
      archive
    }
    if (!is.null(downloadResults$destFile))
      neededFiles <- basename(unique(c(downloadResults$destFile, neededFiles)))
  } else {
    downloadResults <- list(needChecksums = needChecksums, destFile = NULL)
    archiveReturn <- archive
  }
  list(needChecksums = downloadResults$needChecksums, archive = archiveReturn,
       neededFiles = neededFiles,
       downloaded = downloadResults$destFile, checkSums = checkSums, object = downloadResults$out)
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
#' @author Eliot McIntire and Alex Chubaty
#' @keywords internal
#' @importFrom googledrive as_id drive_auth drive_get
#'
dlGoogle <- function(url, archive = NULL, targetFile = NULL,
                     checkSums, skipDownloadMsg, destinationPath,
                     overwrite, needChecksums) {
  if (!is.null(googledrive::drive_token()))
    googledrive::drive_auth() ## needed for use on e.g., rstudio-server
  if (is.null(archive)) {
    fileAttr <- googledrive::drive_get(googledrive::as_id(url))
    archive <- .isArchive(fileAttr$name)
    if (is.null(archive)) {
      if (is.null(targetFile)) {
        # make the guess
        targetFile <- fileAttr$name
      }
      downloadFilename <- targetFile # override if the targetFile is not an archive
    } else {
      archive <- file.path(destinationPath, basename(archive))
      downloadFilename <- archive
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
  return(list(destFile = destFile, needChecksums = needChecksums))
}

#' Download file from generic source url
#'
#' @param url  The url (link) to the file.
#' @param needChecksums TODO
#'
#' ## TODO: add overwrite arg to the function?
#'
#' @author Eliot McIntire and Alex Chubaty
#' @keywords internal
#' @importFrom crayon magenta
#' @importFrom httr GET http_error progress stop_for_status user_agent write_disk
#'
dlGeneric <- function(url, needChecksums) {
  destFile <- file.path(tempdir(), basename(url))

  if (suppressWarnings(httr::http_error(url))) ## TODO: http_error is throwing warnings
    stop("Can not access url ", url)

  message("  Downloading ", url, " ...")

  ua <- httr::user_agent(getOption("reproducible.useragent"))
  request <- suppressWarnings(
    ## TODO: GET is throving warnings
    httr::GET(url, ua, httr::progress(),
              httr::write_disk(destFile, overwrite = TRUE)) ## TODO overwrite?
  )
  httr::stop_for_status(request)

  list(destFile = destFile, needChecksums = needChecksums)
}

downloadRemote <- function(url, archive, targetFile, checkSums, dlFun = NULL,
                           fileToDownload, skipDownloadMsg,
                           destinationPath, overwrite, needChecksums, ...) {

  if (!is.null(url) || !is.null(dlFun)) { # if no url, no download
    #if (!is.null(fileToDownload)  ) { # don't need to download because no url --- but need a case
      if (!isTRUE(tryCatch(is.na(fileToDownload), warning = function(x) FALSE)))  { # NA means archive already in hand
        if (!is.null(dlFun)) {
          dlFunName <- dlFun
          dlFun <- .extractFunction(dlFun)
          fun <- .fnCleanup(out$fun, callingFun = "downloadRemote", ...)
          forms <- .argsToRemove
          dots <- list(...)
          overlappingForms <- fun$formalArgs[fun$formalArgs %in% forms]
          overlappingForms <- grep("\\.\\.\\.", overlappingForms, invert = TRUE, value = TRUE)
          dots <- list(...)
          # remove arguments that are in .argsToRemove, i.e., the sequence
          args <- if (length(overlappingForms)) {
            append(list(...), mget(overlappingForms))
          } else {
            list(...)
          }
          args <- args[!names(args) %in% forms]
          if (is.null(targetFile)) {
            fileInfo <- file.info(dir(destinationPath))
          }
          out <- do.call(dlFun, args = args)
          needSave <- TRUE
          if (is.null(targetFile)) {
            fileInfoAfter <- file.info(dir(destinationPath))
            possibleTargetFile <- setdiff(rownames(fileInfoAfter), rownames(fileInfo))
            if (length(possibleTargetFile)) {
              destFile <- targetFile <- possibleTargetFile
              needSave <- FALSE
            } else {
              destFile <- file.path(destinationPath, tempfile(fileext = ".rds"))
            }
          } else {
            destFile <- file.path(destinationPath, targetFile)
          }
          if (needSave) {
            saveRDS(out, file = destFile)
          }
          downloadResults <- list(out = out, destFile = normPath(destFile), needChecksums = 2)
        } else if (grepl("drive.google.com", url)) {
          downloadResults <- dlGoogle(
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
        if (!(identical(dirname(normPath(downloadResults$destFile)),
                        normPath(destinationPath)))) {
          suppressWarnings(file.copy(downloadResults$destFile, destinationPath))
          suppressWarnings(file.remove(downloadResults$destFile))
          downloadResults$destFile <- file.path(destinationPath, basename(downloadResults$destFile))
        }
      #}
    } else {
      message(skipDownloadMsg)
      downloadResults <- list(needChecksums = 0, destFile = NULL)
    }
  } else {
    message("No downloading; no url")
  }
  downloadResults
}
