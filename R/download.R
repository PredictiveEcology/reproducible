utils::globalVariables(c(
  "goe", "goc"
))

#' A wrapper around a set of downloading functions
#'
#' Currently, this only deals with `googledrive::drive_download`,
#' and [utils::download.file()]. In general, this is not intended for use by a
#' user.
#'
#' @inheritParams prepInputs
#' @inheritParams preProcess
#' @inheritParams prepInputs
#' @inheritParams extractFromArchive
#' @param dlFun Optional "download function" name, such as `"raster::getData"`, which does
#'              custom downloading, in addition to loading into R. Still experimental.
#' @param ... Passed to `dlFun`. Still experimental. Can be e.g., `type` for google docs.
#' @param checksumFile A character string indicating the absolute path to the `CHECKSUMS.txt`
#'                     file.
#' @inheritParams loadFromCache
#' @inheritParams Cache
#' @author Eliot McIntire
#' @return
#' This function is called for its side effects, which will be a downloaded file
#' (`targetFile`), placed in `destinationPath`. This file will be checksummed, and
#' that checksum will be appended to the `checksumFile`.
#'
#' @export
#' @include checksums.R
downloadFile <- function(archive, targetFile, neededFiles,
                         destinationPath = getOption("reproducible.destinationPath", "."), quick,
                         checksumFile, dlFun = NULL,
                         checkSums, url, needChecksums, preDigest,
                         overwrite = getOption("reproducible.overwrite", TRUE),
                         alsoExtract = "similar",
                         verbose = getOption("reproducible.verbose", 1),
                         purge = FALSE, .tempPath, .callingEnv,
                         ...) {

  dots <- list(...)
  # if (is.null(dots$.callingEnv)) {
  #   .callingEnv <- parent.frame()
  # } else {
  #   .callingEnv <- dots$.callingEnv
  #   dots$.callingEnv <- NULL
  # }

  # browser(expr = exists("._downloadFile_1"))
  if (missing(.tempPath)) {
    .tempPath <- tempdir2(rndstr(1, 6))
    on.exit(unlink(.tempPath, recursive = TRUE), add = TRUE)
  }
  if (missing(targetFile)) {
    targetFile <- NULL
  }

  if (!is.null(url) || !is.null(dlFun)) {
    missingNeededFiles <- missingFiles(neededFiles, checkSums, destinationPath)

    if (missingNeededFiles) { # needed may be missing, but maybe can skip download b/c archive exists
      if (!is.null(archive)) {
        localArchivesExist <- file.exists(archive)
        if (any(localArchivesExist)) {
          filesInLocalArchives <- unique(unlist(lapply(archive, .listFilesInArchive)))
          neededFilesRel <- makeRelative(neededFiles, destinationPath)
          haveAll <- if (isNULLorNA(neededFiles) || length(neededFiles) == 0) FALSE else all(neededFilesRel %in% filesInLocalArchives)
          if (haveAll) { # local archive has all files needed
            extractedFromArchive <- extractFromArchive(
              archive = archive[localArchivesExist],
              destinationPath = destinationPath,
              neededFiles = neededFiles, checkSums = checkSums,
              needChecksums = needChecksums,
              checkSumFilePath = checksumFile,
              quick = quick,
              .tempPath = .tempPath
            )
            checkSums <- if (!file.exists(checksumFile) || is.null(neededFiles) || length(neededFiles) == 0) {
              needChecksums <- 1
              .emptyChecksumsResult
            } else {
              Checksums(
                files = neededFiles,
                checksumFile = checksumFile,
                path = destinationPath,
                quickCheck = quick,
                write = FALSE,
                verbose = verbose
              )
            }

            # Check again, post extract ... If FALSE now, then it got it from local, already existing archive
            missingNeededFiles <- missingFiles(neededFiles, checkSums, destinationPath)
            if (!missingNeededFiles) {
              archive <- archive[localArchivesExist]
            }
          } else {
            messagePreProcess("Have local archive, ", archive, ", but its files are not listed in the CHECKSUMS.txt file.", verbose = verbose)
            messagePreProcess("\nRedownloading to start from file at url...", verbose = verbose)
          }
        }
      }
    }

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

      # The download step
      failed <- 1
      numTries <- 2

      while (failed > 0 && failed <= numTries) {
        messOrig <- character()
        warns <- character()
        withCallingHandlers({
          downloadResults <- tryCatch(
            downloadRemote(
              url = url,
              archive = archive, # both url and fileToDownload must be NULL to skip downloading
              targetFile = targetFile,
              fileToDownload = fileToDownload,
              messSkipDownload = .message$SkipDownload,
              checkSums = checkSums,
              dlFun = dlFun,
              destinationPath = destinationPath,
              overwrite = overwrite,
              needChecksums = needChecksums,
              preDigest = preDigest,
              alsoExtract = alsoExtract,
              verbose = verbose,
              .tempPath = .tempPath,
              .callingEnv = .callingEnv,
              ...
            )
            , error = function(e) {
              .downloadErrorFn(e)
            }

          )
        },
        warnings = function(w) {
          warns <<- w$message
        },
        message = function(m) {
          messOrig <<- c(messOrig, m$message)
        })
        if (isTRUE(isDirectory(url, mustExist = FALSE))) {
          fileToDownload <- downloadResults$destFile
          neededFiles <- downloadResults$destFile
        }

        if (is(downloadResults, "try-error")) {
          if (any(grepl("is required but not yet installed", messOrig)))
            failed <- numTries + 2

          downloadResults <- dlErrorHandling(failed, downloadResults, warns, messOrig, numTries, url,
                                             fileToDownload, destinationPath, targetFile, checksumFile,
                                             verbose)

          failed <- failed + 1
        } else {
          # This is so that we essentially treat it as a file, not an object, which means
          #   the second time we try this call, we can access the file locally, without needed to download
          if (is(downloadResults$out, "Spatial")) downloadResults$out <- NULL # TODO This appears to be a bug
          failed <- 0
        }
      }

      if (file.exists(checksumFile)) {
        # This is case where we didn't know what file to download, and only now
        if (is.null(fileToDownload) ||
            tryCatch(isTRUE(is.na(fileToDownload)), warning = function(x) FALSE)) {
          # do we know
          fileToDownload <- downloadResults$destFile
        }
        if (!is.null(fileToDownload)) {
          if ((length(readLines(checksumFile)) > 0)) {
            checkSums <-
              Checksums(
                files = fileToDownload,
                checksumFile = checksumFile,
                path = destinationPath,
                quickCheck = quick,
                write = FALSE,
                verbose = verbose - 1
              )
            isOK <- checkSums[checkSums$expectedFile %in% basename(fileToDownload) |
                                checkSums$actualFile %in% basename(fileToDownload), ]$result
            isOK <- isOK[!is.na(isOK)] == "OK"
            if (length(isOK) > 0) { # This is length 0 if there are no entries in the Checksums
              if (!isTRUE(all(isOK))) {
                if (purge > 0) {
                  # This is case where we didn't know what file to download, and only now
                  # do we know
                  checkSums <- .purge(
                    checkSums = checkSums,
                    purge = purge,
                    url = fileToDownload
                  )
                  downloadResults$needChecksums <- 2
                } else {
                  tf <- tryCatch(
                    makeRelative(targetFile, destinationPath) %in% fileToDownload,
                    error = function(x) {
                      FALSE
                    }
                  )
                  af <- tryCatch(
                    basename2(archive) %in% fileToDownload,
                    error = function(x) {
                      FALSE
                    }
                  )

                  sc <- sys.calls()
                  piCall <- grep("^prepInputs", sc, value = TRUE)
                  purgeTry <- if (length(piCall)) {
                    gsub(piCall,
                         pattern = ")$",
                         replacement = paste0(", purge = 7)")
                    )
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
                    if (!is.null(getOption("reproducible.inputPaths"))) {
                      obj <- dir(getOption("reproducible.inputPaths"), full.names = TRUE, pattern = basename(fileToDownload))
                      if (length(obj)) {
                        paste0(" 2b) The copy of the file in getOption('reproducible.inputPaths')",
                               " may have been changed or corrupted -- run:\n",
                               "      file.remove(c('",
                               paste(normPath(obj), collapse = "', '"),
                               "'))\n",
                               "      then rerun this current function call.\n")
                      }

                    },
                    " 3) The download is correct, and the Checksums should be rewritten for this file:\n",
                    "      --> rerun this current function call, specifying 'purge = 7' possibly\n",
                    purgeTry,
                    " 4) manually run \nreproducible::purgeChecksums('", checksumFile, "', \n               fileToRemove = '", fileToDownload, "')",
                    "      ",
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
    } else {
      # not missing any files to download
      fileAlreadyDownloaded <- if (is.null(archive[1])) {
        expectedFile <- checkSums[compareNA(checkSums$result, "OK"), ]$expectedFile

        archivePossibly <- setdiff(expectedFile, neededFiles)
        archivePossibly <- .isArchive(archivePossibly)
        if (!is.null(archivePossibly)) {
          archivePossibly
        } else {
          neededFiles
        }
      } else {
        archive
      }

      downloadResults <- list(
        needChecksums = needChecksums,
        destFile = makeAbsolute(fileAlreadyDownloaded, destinationPath)
      )
      if (is.null(targetFile)) {
        messagePreProcess("Skipping download because all needed files are listed in ",
                          "CHECKSUMS.txt file and are present.",
                          " If this is not correct, rerun prepInputs with purge = TRUE",
                          verbose = verbose
        )
      } else {
        if (exists("extractedFromArchive", inherits = FALSE)) {
          messagePreProcess("Skipping download: All requested files extracted from local archive:\n    ",
                            archive,
                            verbose = verbose
          )
        } else {
          messagePreProcess("Skipping download. All requested files already present", verbose = verbose)
        }
      }
    }
    archiveReturn <- if (is.null(archive)) {
      .isArchive(downloadResults$destFile)
    } else {
      if (!file.exists(archive)) {
        if (length(.isArchive(downloadResults$destFile))) {
          hardLinkOrCopy(downloadResults$destFile, archive, verbose = verbose)
        }
      }
      archive
    }

    ## This was commented out because of LandWeb -- removed b/c of this case:
    ##  have local archive, but not yet have the targetFile
    # if (!is.null(downloadResults$destFile))
    #   neededFiles <- unique(basename(c(downloadResults$destFile, neededFiles)))
  } else {
    downloadResults <- list(needChecksums = needChecksums, destFile = NULL)
    archiveReturn <- archive
  }
  list(
    needChecksums = downloadResults$needChecksums, archive = archiveReturn,
    neededFiles = neededFiles,
    downloaded = downloadResults$destFile, checkSums = checkSums, object = downloadResults$out
  )
}

#' Download file from Google Drive
#'
#' @param url  The url (link) to the file.
#'
#' @author Eliot McIntire and Alex Chubaty
#' @keywords internal
#' @inheritParams preProcess
#' @param ... Not used here. Only used to allow other arguments to other fns to not fail.
#'
dlGoogle <- function(url, archive = NULL, targetFile = NULL,
                     checkSums, messSkipDownload, destinationPath, type = NULL,
                     overwrite, needChecksums, verbose = getOption("reproducible.verbose", 1),
                     team_drive = NULL, ...) {
  .requireNamespace("googledrive", stopOnFALSE = TRUE)

  if (missing(destinationPath)) {
    destinationPath <- tempdir2(rndstr(1, 6))
  }
  downloadFilename <- assessGoogle(
    url = url, archive = archive,
    targetFile = targetFile,
    destinationPath = destinationPath,
    verbose = verbose,
    team_drive = team_drive
  )

  destFile <- file.path(destinationPath, basename2(downloadFilename))
  if (!isTRUE(checkSums[checkSums$expectedFile == basename(destFile), ]$result == "OK")) {
    messagePreProcess("Downloading from Google Drive.", verbose = verbose)
    fs <- attr(archive, "fileSize")
    if (is.null(fs)) {
      fs <- attr(downloadFilename, "fileSize")
      if (is.null(fs)) {
        fs <- attr(assessGoogle(url, verbose = verbose, team_drive = team_drive), "fileSize")
      }
    }
    if (!is.null(fs)) {
      class(fs) <- "object_size"
    }
    isLargeFile <- ifelse(is.null(fs), FALSE, fs > 1e6)

    # download_with_speed(url, local_path = destFile)
    downloadCall <- quote(download_resumable_httr2(url, local_path = destFile))
    # downloadCall <- quote(drive_downloadWProgress(url, local_path = destFile))
    # downloadCall <- quote(
    #   googledrive::drive_download(
    #     googledrive::as_id(url),
    #     path = destFile,
    #     type = type,
    #     overwrite = overwrite, verbose = TRUE)
    # )

    if (!isWindows() && requireNamespace("future", quietly = TRUE) && isLargeFile &&
        !isFALSE(getOption("reproducible.futurePlan"))) {
      messagePreProcess("Downloading a large file in background using future", verbose = verbose)
      message("Make sure to set\noptions(gargle_oauth_email = 'youremail@somewhere.edu')\n, and possibly ",
              "\noptions(gargle_oauth_cache = 'localPathToCache')")
      fp <- future::plan()
      if (!is(fp, getOption("reproducible.futurePlan"))) {
        fpNew <- getOption("reproducible.futurePlan")
        future::plan(fpNew, workers = 1)
        on.exit({
          future::plan(fp)
        })
      }
      b <- future::future({
        options(gargle_oauth_cache = goc,
                gargle_oauth_email = goe)
      },
      globals = list(

      ))
      a <- future::future(
        {
          googledrive::drive_auth(email = goe,
                                  cache = goc)
          retry(retries = 2,
                downloadCall)
        },
        globals = list(
          goc = getOption("gargle_oauth_cache"),
          goe = getOption("gargle_oauth_email"),
          downloadCall = downloadCall,
          drive_download = googledrive::drive_download,
          as_id = googledrive::as_id,
          retry = retry,
          # drive_deauth = googledrive::drive_deauth,
          url = url,
          type = type,
          overwrite = overwrite,
          destFile = destFile
        )
      )
      cat("\n")
      notResolved <- TRUE
      while (notResolved) {
        Sys.sleep(0.05)
        notResolved <- !future::resolved(a)
        fsActual <- file.size(destFile)
        class(fsActual) <- "object_size"
        if (!is.na(fsActual)) {
          cat(
            format(fsActual, units = "auto"), "of", format(fs, units = "auto"),
            "downloaded         \r"
          )
        }
      }
      cat("\nDone!\n")
    } else {
      useGoogleDrive <- TRUE
      if (isTRUE(getOption("reproducible.useGdown", FALSE))) {
        messForGdownIsTRUE <- "options('reproducible.useGdown') is TRUE"
        gdown <- "gdown"
        if (nchar(Sys.which(gdown))) {
          gdownCall <- paste0(gdown, " ", googledrive::as_id(url), " -O '", destFile, "'")
          messagePreProcess("Using gdown to get files from GoogleDrive because ", messForGdownIsTRUE)

          b <- try(system(gdownCall))
          if (!is(b, "try-error")) {# likely because of authentication
            messagePreProcess(messForGdownIsTRUE, ", but the attempt failed; possibly a private url?\n",
                              url, "\nUsing googledrive package")
            useGoogleDrive <- FALSE
          }
        } else {
          messagePreProcess(messForGdownIsTRUE,
                            ", but gdown is not available at the cmd line; skipping")
        }
      }
      if (isTRUE(useGoogleDrive))
        a <- retry(downloadCall, retries = 2)

    }
  } else {
    messagePreProcess(messSkipDownload, verbose = verbose)
    needChecksums <- 0
  }

  return(list(destFile = destFile, needChecksums = needChecksums))
}

#' Download file from generic source url
#'
#' @param url  The url (link) to the file.
#'
#' @author Eliot McIntire and Alex Chubaty
#' @keywords internal
#' @importFrom utils download.file
#' @inheritParams preProcess
dlGeneric <- function(url, destinationPath, verbose = getOption("reproducible.verbose", 1)) {
  if (missing(destinationPath)) {
    destinationPath <- tempdir2(rndstr(1, 6))
  }

  bn <- basename2(url)
  bn <- gsub("\\?|\\&", "_", bn) # causes errors with ? and maybe &
  destFile <- file.path(destinationPath, bn)

  # if (suppressWarnings(httr::http_error(url))) ## TODO: http_error is throwing warnings
  #   stop("Can not access url ", url)

  messagePreProcess("Downloading ", url, " ...", verbose = verbose)

  needDwnFl <- TRUE # this will try download.file if no httr2 or httr2 fails
  # R version 4.1.3 doesn't have httr2 that can do these steps; httr2 is too old, I believe

  if (.requireNamespace("httr") && .requireNamespace("curl") && getRversion() < "4.2") {
    ua <- httr::user_agent(getOption("reproducible.useragent"))
    filesize <- as.numeric(httr::HEAD(url)$headers$`content-length`)
    for (i in 1:2) {
      request <- suppressWarnings(
        ## TODO: GET is throwing warnings
        httr::GET(
          url, ua, httr::progress(),
          httr::write_disk(destFile, overwrite = TRUE)
        ) ## TODO: overwrite?
      )
      filesizeDownloaded <- file.size(destFile)
      if ( (abs(filesizeDownloaded/filesize)) < 0.2) { # if it is <20% the size; consider it a fail
        # There is only one example where this fails -- the presence of user_agent is the cause
        #   prepInputs(url = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip")
        ua <- NULL
      } else {
        httr::stop_for_status(request)
        needDwnFl <- FALSE
        break
      }
    }
  } else {
    if (.requireNamespace("httr2") && .requireNamespace("curl") && getRversion() >= "4.2") {
      for (i in 1:2) {
        req <- httr2::request(url)
        if (i == 1) # only try on first run through, in case this is the cause of failure; which it is on some sites
          req <- req |> httr2::req_user_agent(getOption("reproducible.useragent"))
        if (verbose > 0) {
          # req_progress is not in the binary httr2 available for R version 4.1.3; fails on CRAN checks
          reqProgress <- get("req_progress", envir = asNamespace("httr2"))
          req <- req |> reqProgress()
        }

        resp <- req |> httr2::req_url_query() |>
          httr2::req_perform(path = destFile)
        a <- httr2::resp_body_string(resp)
        isRjcted <- grepl("Request Rejected", a)
        if (!isTRUE(any(isRjcted)) && !httr2::resp_is_error(resp)) {
          needDwnFl <- FALSE
          break
        }
      }
    } else {
      messagePreProcess("If downloads fail; please install httr2 and try again")
    }
  }

  if (needDwnFl) {
    out <- try(download.file(url, destfile = destFile))
    if (is(out, "try-error")) {
      unlink(destFile)
      stop(.txtDownloadFailedFn("httr2"))
      # stop("Download failed; try rerunning after installing 'httr2' package.")
    }
  }

  list(destFile = destFile)
}

#' Download a remote file
#'
#' @inheritParams prepInputs
#' @inheritParams preProcess
#' @param needChecksums Logical indicating whether to generate checksums. ## TODO: add overwrite arg to the function?
#' @param messSkipDownload The character string text to pass to messaging if download skipped
#' @param checkSums TODO
#' @param fileToDownload TODO
#' @inheritParams loadFromCache
#' @inheritParams prepInputs
#' @inheritParams preProcess
#'
downloadRemote <- function(url, archive, targetFile, checkSums, dlFun = NULL,
                           fileToDownload, messSkipDownload,
                           destinationPath, overwrite, needChecksums, .tempPath, preDigest,
                           alsoExtract = "similar",
                           verbose = getOption("reproducible.verbose", 1), .callingEnv = parent.frame(),
                           ...) {
  dots <- list(...)
  # if (is.null(dots$.callingEnv)) {
  #   .callingEnv <- parent.frame()
  # } else {
  #   .callingEnv <- dots$.callingEnv
  #   dots$.callingEnv <- NULL
  # }

  noTargetFile <- is.null(targetFile) || length(targetFile) == 0
  if (missing(.tempPath)) {
    .tempPath <- tempdir2(rndstr(1, 6))
    on.exit(
      {
        unlink(.tempPath, recursive = TRUE)
      },
      add = TRUE
    )
  }

  if (!is.null(url) || !is.null(dlFun)) { # if no url, no download
    # if (!is.null(fileToDownload)  ) { # don't need to download because no url --- but need a case
    if (!isTRUE(tryCatch(is.na(fileToDownload), warning = function(x) FALSE))) {
      messagePreProcess("...downloading...", verbose = verbose)

      ## NA means archive already in hand
      out <- NULL

      if (!is.null(dlFun)) {
        dlFunName <- dlFun
        if (exists("aaaa", envir = .GlobalEnv)) browser()
        dlFunPoss <- try(.extractFunction(dlFun, envir = list2env(list(...))), silent = TRUE)
        if (is(dlFunPoss, "try-error"))
          dlFunPoss <- get0(dlFun, envir = .callingEnv)
        dlFun <- dlFunPoss
        fun <- if (is(dlFun, "call")) {
          CacheMatchedCall <- match.call(call = dlFun)
          .fnCleanup(dlFun, callingFun = "downloadRemote", CacheMatchedCall = CacheMatchedCall)
        } else {
          NULL
        }
        forms <- .argsToRemove
        overlappingForms <- fun$formalArgs[fun$formalArgs %in% forms]
        overlappingForms <- grep("\\.\\.\\.", overlappingForms, invert = TRUE, value = TRUE)

        # remove arguments that are in .argsToRemove, i.e., the sequence
        args <- if (length(overlappingForms)) {
          append(list(...), mget(overlappingForms))
        } else {
          list(...)
        }
        args <- args[!names(args) %in% forms]
        if (noTargetFile) {
          fileInfo <- file.info(dir(destinationPath, full.names = TRUE))
        }

        if (is.call(dlFun)) {
          out <- try(eval(dlFun, envir = .callingEnv), silent = TRUE)
          if (is(out, "try-error")) {
            sfs <- sys.frames()
            for (i in seq_along(sfs)) {
              env1 <- new.env(parent = sys.frame(-i))
              list2env(args, env1)
              out <- try(eval(dlFun, envir = env1), silent = TRUE)
              if (is.function(out)) { # in the previous "call", it may have just returned an unevaluated function
                dlFun <- out
              }
              if (!is(out, "try-error")) {
                break
              }
            }
          }
        }

        if (!is.call(dlFun) && !is.null(dlFun)) {
          out <- runDlFun(args, dlFun)
          # argsOrig <- args
          # formsDlFun <- formalArgs(dlFun)
          # argsKeep <- intersect(formsDlFun, names(args))
          # args <- args[argsKeep]
          # for (iii in 1:2) {
          #   out <- try(do.call(dlFun, args = args), silent = TRUE)
          #   if (!is(out, "try-error")) {
          #     break
          #   }
          #   args <- argsOrig
          # }

        }

        needSave <- !is.null(out) # TRUE
        if (noTargetFile) {
          # recursive gets rid of directories
          fileInfoAfter <- file.info(dir(destinationPath, recursive = TRUE, full.names = TRUE))
          possibleTargetFile <- setdiff(rownames(fileInfoAfter), rownames(fileInfo))

          possibleTargetFile <- makeAbsolute(possibleTargetFile, destinationPath)

          if (length(possibleTargetFile)) {
            destFile <- targetFile <- possibleTargetFile
            needSave <- FALSE
          } else {
            destFile <- normPath(file.path(destinationPath, basename(tempfile(fileext = ".rds"))))
          }
        } else {
          destFile <- makeAbsolute(targetFile, destinationPath)
          # destFile <- normPath(file.path(destinationPath, targetFile))
        }

        # some functions will load the object, not just download them, since we may not know
        #   where the function actually downloaded the file, we save it as an RDS file
        if (needSave) {
          if (!file.exists(destFile)) {
            out2 <- .wrap(out, preDigest = preDigest)
            saveRDS(out2, file = destFile)
          }
        }
        downloadResults <- list(out = out, destFile = normPath(destFile), needChecksums = 2)
      }

      if (is.null(out)) {
        isGID <- all(isTRUE(grepl("^[A-Za-z0-9_-]{33}$", url)), # Has 33 characters as letters, numbers or - or _
                     isTRUE(!grepl("\\.[^\\.]+$", url))) # doesn't have an extension --> GDrive ID's as url
        if (any(isGID, grepl("d.+.google.com", url))) {
          if (!requireNamespace("googledrive", quietly = TRUE)) {
            stop(.message$RequireNamespaceFn("googledrive", "to use google drive files"))
          }

          teamDrive <- getTeamDrive(dots)

          if (isGoogleDriveDirectory(url)) {
            drive_files <- googledrive::drive_ls(googledrive::as_id(url))
            if (!is.null(alsoExtract) && length(alsoExtract) > 0) {
              fileIndex <- seq_len(NROW(drive_files))
              if (length(alsoExtract) > 1)
                fileIndex <- sapply(alsoExtract, function(ae) grep(pattern = ae, drive_files$name)) |>
                  as.vector()
              else {
                if (!identical("all", alsoExtract))
                  fileIndex <- grep(pattern = alsoExtract, drive_files$name)
              }
              drive_files <- drive_files[fileIndex, ]
            }

            existingFiles <- drive_files$name %in% dir(destinationPath)
            if (any(existingFiles)) {
              messagePreProcess("Local version of files exists")
              if (isFALSE(overwrite)) {
                drive_files <- drive_files[!existingFiles, ]
                messagePreProcess("Overwrite is FALSE; only getting new ones:\n",
                                  paste0(drive_files$name, collapse = "\n"))

              }
            }

            ids <- drive_files$id
            downloadResults <- lapply(ids, function(ids)
              dlGoogle(
                url = ids, archive = archive, # targetFile = targetFile,
                checkSums = checkSums, messSkipDownload = messSkipDownload, destinationPath = .tempPath,
                overwrite = overwrite, needChecksums = needChecksums, verbose = verbose,
                team_drive = teamDrive, ...
              )
            )
            if (length(downloadResults)) {
              downloadResults <- list(destFile = vapply(downloadResults, function(x) x$destFile, FUN.VALUE = character(1)),
                                      needChecksums = max(vapply(downloadResults, function(x) x$needChecksums, FUN.VALUE = numeric(1))))
            } else {
              downloadResults <- list(destFile = character(), needChecksums = 0)
            }

          } else {
            downloadResults <- dlGoogle(
              url = url, archive = archive, targetFile = targetFile,
              checkSums = checkSums, messSkipDownload = messSkipDownload, destinationPath = .tempPath,
              overwrite = overwrite, needChecksums = needChecksums, verbose = verbose,
              team_drive = teamDrive, ...
            )
          }
        } else if (isTRUE(grepl("dl.dropbox.com", url))) {
          stop("Dropbox downloading is currently not supported")
        } else if (isTRUE(grepl("onedrive.live.com", url))) {
          stop("Onedrive downloading is currently not supported")
        } else {
          if (isTRUE(isDirectory(url, mustExist = FALSE))) { # a folder
            if (.requireNamespace("httr") && .requireNamespace("curl")) {
              list_files <- curl::new_handle()
              curl::handle_setopt(list_files, ftp_use_epsv = TRUE, dirlistonly = TRUE)
              con <- curl::curl(url = url, "r", handle = list_files)
              on.exit(close(con), add = TRUE)
              filenames <- readLines(con)
              # This is from NFI example
              filenames <- grep("href", filenames, value = TRUE)
              filenames <- grep("\\[PARENTDIR\\]|\\[ICO\\]", filenames, value = TRUE, invert = TRUE)
              filenames2 <- gsub(".+<a href=\"(.+)\">.+/a>.+", "\\1", filenames)
              # This was from mexico example from Steve
              # filenames3 <- gsub(".+<a.+\">(.+)</a>.+", "\\1", filenames)
              # rm http tags, plus the two files Description and Parent Directory that are in a directory
              filenames <- grep("<|>|Description|Parent Directory", filenames2, value = TRUE, invert = TRUE)
              if (isTRUE(nzchar(alsoExtract))) {
                if (grepl("^sim", alsoExtract)) {
                  theGrep <- filePathSansExt(targetFile)
                } else if (grepl("none", alsoExtract)) {
                  theGrep <- paste0("^", targetFile, "$")
                } else {
                  theGrep <- paste(alsoExtract, collapse = "|")
                }
                filenames <- grep(theGrep, filenames, value = TRUE)
              }
              # now that we have filenames; need to checksum
              urls <- file.path(url, filenames)

              checkSums <- runChecksums(destinationPath, checkSumFilePath = destinationPath, filenames, verbose)
              checkSums <- checkSums$checkSums[expectedFile %in% filenames]
              checkSums <- checkSums[data.table(expectedFile = basename2(filenames)), on = "expectedFile"]
              missingNeededFiles <- missingFiles(filenames, checkSums, destinationPath)
              stillNeed <- !checkSums$result %in% "OK"

              downloadResults <- list(destFile = character())
              if (missingNeededFiles) {
                stillNeedFile <- match(basename2(urls), checkSums$expectedFile[stillNeed])
                messagePrepInputs("url was supplied as a directory; downloading\n",
                                         paste(urls[stillNeed], collapse = "\n"),
                                  verbose = verbose)

                downloadResults <- vapply(urls[stillNeedFile], function(url)
                  dlGeneric(url, destinationPath = .tempPath, verbose = verbose) |> unlist(),
                  FUN.VALUE = character(1))
                # named list of local filenames; named with urls
                downloadResults <- list(destFile = downloadResults)
              }
              if (any(!stillNeed)) {
                filenamesAlreadyHave <- makeAbsolute(checkSums$expectedFile[stillNeed %in% FALSE], destinationPath)
                alreadyHave <- match(checkSums$expectedFile[stillNeed %in% FALSE], basename2(urls))
                names(filenamesAlreadyHave) <-  urls[alreadyHave]
                # downloadResults$destFile <- c(downloadResults$destFile, filenamesAlreadyHave)
              }

            } else {
              stop("url is a directory; need to install.packages(c('httr', 'curl'))")
            }
          } else {

            downloadResults <- dlGeneric(url = url, destinationPath = .tempPath, verbose = verbose)
          }
          downloadResults$needChecksums <- needChecksums
        }
      }
      # if destinationPath is tempdir, then don't copy and remove

      testFTD <- length(fileToDownload) > 0
      if (isTRUE(testFTD)) testFTD <- isTRUE(all(!downloadResults$destFile %in% fileToDownload))

      # Don't use .tempPath directly because of non-google approaches too

      if (!(identical(
        unique(dirname(normPath(downloadResults$destFile))),
        normPath(as.character(destinationPath))
      )) || testFTD) {
        # basename2 is OK because the destFile will be flat; it is just archive extraction that needs to allow nesting
        desiredPath <- makeAbsolute(basename2(downloadResults$destFile), destinationPath)
        desiredPathExists <- file.exists(desiredPath)
        if (any(desiredPathExists) && !isTRUE(overwrite)) {
          stopMess <- paste(desiredPath, " already exists and overwrite = FALSE; would you like to overwrite anyway? Y or N:  ")
          if (interactive()) {
            interactiveRes <- readline(stopMess)
            if (startsWith(tolower(interactiveRes), "y")) {
              overwrite <- TRUE
            }
          }
          if (!identical(overwrite, TRUE)) {
            stop(targetFile, " already exists at ", desiredPath, ". Use overwrite = TRUE?")
          }
        }

        # Try hard link first -- the only type that R deeply recognizes
        # if that fails, fall back to copying the file.
        # NOTE: never use symlink because the original will be deleted.
        result <- hardLinkOrCopy(downloadResults$destFile, desiredPath, verbose = verbose - 3)

        # result <- suppressWarningsSpecific(
        #   file.link(downloadResults$destFile, desiredPath),
        #   falseWarnings = "already exists|Invalid cross-device")
        # # result <- suppressWarnings(
        # #   file.link(downloadResults$destFile, desiredPath)
        # # )
        #
        # if (isFALSE(result)) {
        #   result <- file.copy(downloadResults$destFile, desiredPath)
        # }

        tmpFile <- makeRelative(downloadResults$destFile, dirname(downloadResults$destFile))
        downloadResults$destFile <- makeAbsolute(tmpFile, destinationPath)
        # downloadResults$destFile <- file.path(destinationPath, basename(downloadResults$destFile))
      }
      # }
    } else {
      messagePreProcess(messSkipDownload, verbose = verbose)
      downloadResults <- list(needChecksums = 0, destFile = NULL)
    }
  } else {
    messagePreProcess("No downloading; no url", verbose = verbose)
  }
  # clean up from "directory" downloads
  if (exists("filenamesAlreadyHave", inherits = FALSE)) {
    downloadResults$destFile <- c(downloadResults$destFile, filenamesAlreadyHave)
  }
  downloadResults
}

missingFiles <- function(files, checkSums, destinationPath) {
  filesBasename <- makeRelative(files, destinationPath)
  if (is.null(files)) {
    result <- unique(checkSums$result)
  } else {
    result <- checkSums[checkSums$expectedFile %in% filesBasename, ]$result
  }
  if (length(result) == 0) result <- NA

  (!(all(compareNA(result, "OK")) && all(filesBasename %in% checkSums$expectedFile)) ||
      is.null(files))
}

assessGoogle <- function(url, archive = NULL, targetFile = NULL,
                         destinationPath = getOption("reproducible.destinationPath", "."),
                         verbose = getOption("reproducible.verbose", 1),
                         team_drive = NULL) {
  if (!requireNamespace("googledrive", quietly = TRUE)) {
    stop(.message$RequireNamespaceFn("googledrive", "to use google drive files"))
  }
  if (.isRstudioServer()) {
    .requireNamespace("httr", stopOnFALSE = TRUE)
    opts <- options(httr_oob_default = TRUE)
    on.exit(options(opts))
  }

  if (is.null(archive) || is.na(archive)) {
    if (packageVersion("googledrive") < "2.0.0") {
      fileAttr <- retry(retries = 1, quote(googledrive::drive_get(googledrive::as_id(url),
                                                                  team_drive = team_drive
      )))
    } else {
      if (isTRUE(isDirectory(url, FALSE))) {
        fileAttr <- retry(retries = 1, quote(googledrive::drive_ls(googledrive::as_id(url),
                                                                    shared_drive = team_drive
        )))
      } else {
        fileAttr <- retry(retries = 1, quote(googledrive::drive_get(googledrive::as_id(url),
                                                                    shared_drive = team_drive
        )))
      }
    }
    fileSize <- sapply(fileAttr$drive_resource, function(x) x$size) ## TODO: not returned with team drive (i.e., NULL)
    if (!is.null(fileSize)) {
      messageAboutFilesize(fileSize, verbose)
      # fileSize <- as.numeric(fileSize)
      # len <- length(fileSize)
      # if (len > 1)
      #   fileSize <- sum(fileSize)
      # class(fileSize) <- "object_size"
      # Fils <- singularPlural(c("File", "Files"), v = len)
      # isAre <- isAre(v = len)
      # messagePreProcess(Fils, " on Google Drive ", isAre, " ", format(fileSize, units = "auto"),
      #                   verbose = verbose
      # )
    }
    archive <- .isArchive(fileAttr$name)
    if (is.null(archive)) {
      if (is.null(targetFile)) {
        # make the guess
        targetFile <- fileAttr$name
      }
      downloadFilename <- targetFile # override if the targetFile is not an archive
    } else {
      archive <- file.path(destinationPath, basename2(archive))
      downloadFilename <- archive
    }
    attr(downloadFilename, "drive_resource") <- fileAttr$drive_resource
  } else {
    downloadFilename <- archive
  }
  if (exists("fileSize", inherits = FALSE)) {
    attr(downloadFilename, "fileSize") <- fileSize
  }

  return(downloadFilename)
}


.isRstudioServer <- function() {
  isRstudioServer <- FALSE

  if (isTRUE("tools:rstudio" %in% search())) { ## running in Rstudio
    rsAPIFn <- get(".rs.api.versionInfo", as.environment("tools:rstudio"))
    versionInfo <- rsAPIFn()
    if (!is.null(versionInfo)) {
      isRstudioServer <- identical("server", versionInfo$mode)
    }
  }
  isRstudioServer
}


SSL_REVOKE_BEST_EFFORT <- function(envir = parent.frame(1)) {
  # Take from https://github.com/rstudio/rstudio/issues/10163#issuecomment-1193316767 #
  prevCurlVal <- Sys.getenv("R_LIBCURL_SSL_REVOKE_BEST_EFFORT")
  Sys.setenv(R_LIBCURL_SSL_REVOKE_BEST_EFFORT=TRUE)
  on.exit2({#withr::defer({
    if (nzchar(prevCurlVal))
      Sys.setenv(R_LIBCURL_SSL_REVOKE_BEST_EFFORT = prevCurlVal)
    else
      Sys.unsetenv("R_LIBCURL_SSL_REVOKE_BEST_EFFORT")
  }, envir = envir)
}

on.exit2 <- function(expr, envir = sys.frame(-2), add = TRUE, after = TRUE) {
  funExpr <- as.call(list(function() expr))
  do.call(base::on.exit, list(funExpr, add, after), envir = envir)
}



dlErrorHandling <- function(failed, downloadResults, warns, messOrig, numTries, url,
                            fileToDownload, destinationPath, targetFile, checksumFile,
                            verbose) {
  if (isTRUE(grepl(paste("already exists", .txtDownloadFailedFn(".+"), sep = "|"), downloadResults))) {
    stop(downloadResults)
  }

  if (isTRUE(grepl("already exists", downloadResults))) {
    stop(downloadResults)
  }

  SSLwarns <- grepl(.txtUnableToAccessIndex, warns)
  SSLwarns2 <- grepl("SSL peer certificate or SSH remote key was not OK", messOrig)
  if (any(SSLwarns) || any(SSLwarns2)) {
    SSL_REVOKE_BEST_EFFORT()
  }

  if (failed >= numTries) {
    isGID <- all(grepl("^[A-Za-z0-9_-]{33}$", url), # Has 33 characters as letters, numbers or - or _
                 !grepl("\\.[^\\.]+$", url)) # doesn't have an extension
    if (isGID) {
      urlMessage <- googledriveIDtoHumanURL(id)
      # urlMessage <- paste0("https://drive.google.com/file/d/", url)
    } else {
      urlMessage <- url
    }
    messCommon <- paste0(
      "Download of ", url, " failed. This may be a permissions issue. ",
      "Please check the url and permissions are correct.\n",
      "If the url is correct, it is possible that manually downloading it will work. ",
      "To try this, with your browser, go to\n",
      urlMessage, ",\n ... then download it manually, give it this name: '", fileToDownload,
      "', and place file here: ", destinationPath
    )
    if (isInteractive() && getOption("reproducible.interactiveOnDownloadFail", TRUE)) {
      mess <- paste0(
        messCommon,
        ".\n ------- \nIf you have completed a manual download, press 'y' to continue; otherwise press any other key to stop now. ",
        "\n(To prevent this behaviour in the future, set options('reproducible.interactiveOnDownloadFail' = FALSE)  )"
      )
      if (failed == numTries + 2) {
        stop(paste(messOrig, collapse = "\n"))
      } else {
        messagePreProcess(mess, verbose = verbose + 1)
      }
      resultOfPrompt <- .readline("Type y if you have attempted a manual download and put it in the correct place: ")
      resultOfPrompt <- tolower(resultOfPrompt)
      if (!identical(resultOfPrompt, "y")) {
        stop(downloadResults, "\n", messOrig, "\nDownload failed")
      }
      downloadResults <- list(
        destFile = file.path(destinationPath, targetFile),
        needChecksums = 2
      )
    } else {
      message(downloadResults)
      stop(
        downloadResults, "\n", messOrig, "\n", messCommon, ".\n-------------------\n",
        "If manual download was successful, you will likely also need to run Checksums",
        " manually after you download the file with this command: ",
        "reproducible:::appendChecksumsTable(checkSumFilePath = '", checksumFile, "', filesToChecksum = '", targetFile,
        "', destinationPath = '", dirname(checksumFile), "', append = TRUE)"
      )
    }
  } else {
    if (failed > 1) Sys.sleep(0.5) else SSL_REVOKE_BEST_EFFORT() # uses withr::defer to remove it after this test
  }

  #
  #
  # # ELIOT removed this as httr is being deprecated --> the above chunk should work
  # # if (any(grepl("SSL peer certificate or SSH remote key was not OK", messOrig))) {
  # #   # THIS IS A MAJOR WORK AROUND FOR SSL ISSUES IN SOME WORK ENVIRONMENTS. NOT ADVERTISED.
  # #   # https://stackoverflow.com/questions/46331066/quantmod-ssl-unable-to-get-local-issuer-certificate-in-r
  # #   if (isFALSE(as.logical(Sys.getenv("REPRODUCIBLE_SSL_VERIFYPEER")))) {
  # #     .requireNamespace("httr", stopOnFALSE = TRUE)
  # #     message(
  # #       "Temporarily setting ssl_verifypeer to FALSE because ",
  # #       "'SSL peer certificate or SSH remote key was not OK'"
  # #     )
  # #     sslOrig <- httr::set_config(httr::config(ssl_verifypeer = FALSE))
  # #     on.exit(httr::set_config(sslOrig), add = TRUE)
  # #   }
  # # }
  #
  # # if (any(grepl("is required but not yet installed", messOrig))) {
  # #   failed <- numTries + 2
  # # }
  # if (failed >= numTries) {
  #   isGID <- all(grepl("^[A-Za-z0-9_-]{33}$", url), # Has 33 characters as letters, numbers or - or _
  #                !grepl("\\.[^\\.]+$", url)) # doesn't have an extension
  #   if (isGID) {
  #     urlMessage <- paste0("https://drive.google.com/file/d/", url)
  #   } else {
  #     urlMessage <- url
  #   }
  #   messCommon <- paste0(
  #     "Download of ", url, " failed. This may be a permissions issue. ",
  #     "Please check the url and permissions are correct.\n",
  #     "If the url is correct, it is possible that manually downloading it will work. ",
  #     "To try this, with your browser, go to\n",
  #     urlMessage, ",\n ... then download it manually, give it this name: '", fileToDownload,
  #     "', and place file here: ", destinationPath
  #   )
  #   if (isInteractive() && getOption("reproducible.interactiveOnDownloadFail", TRUE)) {
  #     mess <- paste0(
  #       messCommon,
  #       ".\n ------- \nIf you have completed a manual download, press 'y' to continue; otherwise press any other key to stop now. ",
  #       "\n(To prevent this behaviour in the future, set options('reproducible.interactiveOnDownloadFail' = FALSE)  )"
  #     )
  #     if (failed == numTries + 2) {
  #       stop(paste(messOrig, collapse = "\n"))
  #     } else {
  #       messagePreProcess(mess, verbose = verbose + 1)
  #     }
  #     resultOfPrompt <- .readline("Type y if you have attempted a manual download and put it in the correct place: ")
  #     resultOfPrompt <- tolower(resultOfPrompt)
  #     if (!identical(resultOfPrompt, "y")) {
  #       stop(downloadResults, "\n", messOrig, "\nDownload failed")
  #     }
  #     downloadResults <- list(
  #       destFile = file.path(destinationPath, targetFile),
  #       needChecksums = 2
  #     )
  #   } else {
  #     message(downloadResults)
  #     stop(
  #       downloadResults, "\n", messOrig, "\n", messCommon, ".\n-------------------\n",
  #       "If manual download was successful, you will likely also need to run Checksums",
  #       " manually after you download the file with this command: ",
  #       "reproducible:::appendChecksumsTable(checkSumFilePath = '", checksumFile, "', filesToChecksum = '", targetFile,
  #       "', destinationPath = '", dirname(checksumFile), "', append = TRUE)"
  #     )
  #   }
  # } else {
  #   Sys.sleep(0.5)
  # }
  downloadResults
}

.downloadErrorFn <- function(xxxx) {
  try(stop(xxxx))
}


runDlFun <- function(args, dlFun) {
  argsOrig <- args
  formsDlFun <- formalArgs(dlFun)
  argsKeep <- intersect(formsDlFun, names(args))
  args <- args[argsKeep]
  for (iii in 1:2) {
    out <- try(do.call(dlFun, args = args), silent = TRUE)
    if (!is(out, "try-error")) {
      break
    }
    args <- argsOrig
  }
  out
}


#' Purge the checksums of a single file
#'
#' This is a manual way of achieving `prepInputs(..., purge = 7)`, useful in cases
#' where `prepInputs` is not called directly by the user, so it would be difficult
#' to set `purge = 7`.
#' @inheritParams downloadFile
#' @param fileToRemove The filename to remove from the `checksumFile`
#'
#' @export
#' @return NULL. Run for its side effect, namely, and file removed from the CHECKSUMS.txt
#'   file.
purgeChecksums <- function(checksumFile, fileToRemove) {
  dt <- data.table::fread(checksumFile)
  toPurge <- dt[file %in% fileToRemove]
  dtNew <- dt[!toPurge, on = c("file", "checksum")]
  data.table::fwrite(dtNew, file = checksumFile)
}




# drive_downloadWProgress <- function(file_name, local_path) {
#   # Authenticate and get file metadata
#   # googledrive::drive_auth()
#   file <- googledrive::drive_get(file_name)
#   file_id <- file$id
#
#   # Build download URL
#   download_url <- googledriveIDtoURL(file_id)
#   # download_url <- paste0("https://www.googleapis.com/drive/v3/files/", file_id, "?alt=media")
#
#   # Get token from googledrive
#   token <- googledrive::drive_token()
#   bearer <- token$auth_token$credentials$access_token
#
#   browser()
#   # Create request with progress and perform download
#   httr2::request(download_url) |>
#     httr2::req_auth_bearer_token(bearer) |>
#     httr2::req_progress() |>
#     httr2::req_perform(path = local_path)
# }
# Example usage

download_resumable_httr2 <- function(file_name, local_path) {
  # Authenticate and get file metadata
  # googledrive::drive_auth()
  isGD <- isGoogleDriveURL(file_name) || is(file_name, "drive_id")
  if (isGD) {
    file <- googledrive::drive_get(file_name)
    file_id <- file$id
    download_url <- googledriveIDtoDownloadURL(file_id)
    token <- googledrive::drive_token()
    bearer <- token$auth_token$credentials$access_token
    total_size <- as.numeric(file$drive_resource[[1]]$size)
    req <- httr2::request(download_url)
    req <- req |> httr2::req_auth_bearer_token(bearer)

  } else {
    req <- httr2::request(file_name)
    head_req <- req |> httr2::req_method("HEAD")
    head_resp <- httr2::req_perform(head_req)
    total_size <- as.numeric(httr2::resp_header(head_resp, "content-length"))

  }

  # Build download URL
  # download_url <- paste0("https://www.googleapis.com/drive/v3/files/", file_id, "?alt=media")

  # Get token

  # Check how much has already been downloaded
  downloaded_bytes <- if (file.exists(local_path)) file.info(local_path)$size else 0

  if (total_size > downloaded_bytes) {

    # Create request with Range header
    req <- req |> # httr2::request(download_url) |>
      # httr2::req_auth_bearer_token(bearer) |>
      httr2::req_headers(Range = paste0("bytes=", downloaded_bytes, "-")) |>
      httr2::req_progress()

    # Open connection in append mode
    con <- file(local_path, open = "ab")
    on.exit(try(close(con), silent = TRUE))

    # Stream response and append to file
    resp <- httr2::req_perform(req)
    body <- httr2::resp_body_raw(resp)
    writeBin(body, con)
    close(con)
  }
}





messageAboutFilesize <- function(fileSize, verbose, msgMiddle = " on Google Drive ") {
  fileSize <- as.numeric(fileSize)
  len <- length(fileSize)
  if (len > 1)
    fileSize <- sum(fileSize)
  class(fileSize) <- "object_size"
  Fils <- singularPlural(c("File", "Files"), v = len)
  isAre <- isAre(v = len)
  messagePreProcess(Fils, msgMiddle, isAre, " ", format(fileSize, units = "auto"),
                    verbose = verbose
  )
}

googledriveIDtoDownloadURL <- function(id) {
  paste0("https://www.googleapis.com/drive/v3/files/", id, "?alt=media")
}

googledriveIDtoHumanURL <- function(id) {
  paste0("https://drive.google.com/file/d/", id)
}
