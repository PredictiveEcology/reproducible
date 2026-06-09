utils::globalVariables(c(
  "goe", "goc", "gauth_path"
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
#' @param overwrite Logical. If `TRUE` then the download will overwrite an existing file
#'   if it exists.
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
        if (isTRUE(isDirectory(url, mustExist = FALSE)) && !is(downloadResults, "try-error")) {
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
                    if (!is.null(.getDestinationPathShared())) {
                      obj <- dir(.getDestinationPathShared(), full.names = TRUE, pattern = basename(fileToDownload))
                      if (length(obj)) {
                        paste0(" 2b) The copy of the file in getOption('reproducible.destinationPathShared')",
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

  # Feature B: now that the Drive filename is resolved, consult the user remap
  # hook. If it returns an alternative (public HTTPS) URL, fetch that via the
  # generic path instead -- this bypasses Google Drive auth entirely (intended,
  # for public mirrors such as Arbutus) and lets the parallel ranged-download
  # path apply. `targetFile` keeps the on-disk name equal to the Drive filename
  # so downstream checksum/extract logic is unchanged.
  remapUrl <- .applyUrlRemap(url, basename2(downloadFilename), verbose = verbose)

  # No-auth bypass: fetch over plain HTTPS with no googledrive token when any of:
  #   * the user passes the public web-download form of the URL
  #     (`uc?export=download&id=` / `drive.usercontent.google.com/download`);
  #   * `options(reproducible.gdriveNoAuth = TRUE)` is set;
  #   * no Drive token is loaded -- in which case `assessGoogle()` above
  #     *already* read the metadata anonymously, which is only possible for a
  #     public ("Anyone with the link") file, so the public endpoint is
  #     guaranteed to work and authentication would only fail. This makes the
  #     common "no token, public file" case Just Work, silently.
  # A user remap (above) still takes precedence.
  publicNoAuth <- identical(remapUrl, url) &&
    (isGoogleDownloadURL(url) || isTRUE(getOption("reproducible.gdriveNoAuth", FALSE)) ||
       !.gdriveHasToken())

  if (!isTRUE(checkSums[checkSums$expectedFile == basename(destFile), ]$result == "OK")) {
    if (!identical(remapUrl, url)) {
      res <- dlGeneric(
        url = remapUrl, destinationPath = destinationPath,
        targetFile = basename2(downloadFilename), applyRemap = FALSE, verbose = verbose
      )
      return(list(destFile = res$destFile, needChecksums = needChecksums))
    }
    if (isTRUE(publicNoAuth)) {
      res <- .dlGooglePublic(
        url = url, destinationPath = destinationPath,
        targetFile = basename2(downloadFilename), verbose = verbose
      )
      return(list(destFile = res$destFile, needChecksums = needChecksums))
    }
    messagePreProcess("Downloading from Google Drive.", verbose = verbose)
    fs <- attr(archive, "fileSize")
    if (is.null(fs)) {
      fs <- attr(downloadFilename, "fileSize")
      if (is.null(fs)) {
        fs <- attr(assessGoogle(url, verbose = verbose, team_drive = team_drive), "fileSize")
      }
    }
    if (!is.null(fs)) {
      if (!is.numeric(fs))
        fs <- as.numeric(fs)
      class(fs) <- "object_size"
    }
    isLargeFile <- ifelse(is.null(fs), FALSE, fs > 1e6)

    # download_with_speed(url, local_path = destFile)
    if (.requireNamespace("httr2", stopOnFALSE = FALSE)) {
      downloadCall <-
        quote(
          download_resumable_httr2(
            url, local_path = destFile,
            gdriveDetails = list(id = googledrive::as_id(url),
                                 drive_resource = attr(downloadFilename, "drive_resource"))))
      # downloadCall <- quote(drive_downloadWProgress(url, local_path = destFile))
    } else {
      downloadCall <- quote(
        googledrive::drive_download(
          googledrive::as_id(url),
          path = destFile,
          type = type,
          overwrite = overwrite, verbose = TRUE)
      )
    }

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
          # Re-authenticate inside the future worker. Prefer a service-account
          # JSON when one is configured (CI / GOOGLEDRIVE_AUTH path), otherwise
          # fall back to the user-account email + cache used by the parent.
          # Interactive auth (no email, no path) would silently hang the
          # worker — pass the JSON path through globals when available.
          if (nzchar(gauth_path) && file.exists(gauth_path)) {
            try(googledrive::drive_auth(path = gauth_path), silent = TRUE)
          } else {
            googledrive::drive_auth(email = goe, cache = goc)
          }
          retry(retries = 2, downloadCall)
        },
        globals = list(
          # Guard against NULL: getOption("gargle_oauth_cache") defaults to
          # NULL when unset, and normalizePath(NULL) errors with the cryptic
          # `path.expand(path) : invalid 'path' argument` inside the future
          # worker. Pass NULL through — gargle handles an absent cache.
          goc = local({
            v <- getOption("gargle_oauth_cache")
            if (is.null(v) || !nzchar(v)) v else normalizePath(v, mustWork = FALSE)
          }),
          goe = getOption("gargle_oauth_email"),
          gauth_path = Sys.getenv("GOOGLEDRIVE_AUTH", ""),
          downloadCall = downloadCall,
          downloadFilename = downloadFilename,
          download_resumable_httr2 = download_resumable_httr2,
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

      if (isTRUE(useGoogleDrive)) {
        a <- tryCatch(
          retry(downloadCall, retries = 2),
          error = function(e) {
            # Reactive no-auth fallback: the authenticated download failed (e.g.
            # an expired/absent token). If the file is in fact public, fetch it
            # silently via the no-auth endpoint and carry on; only if that *also*
            # fails do we surface the original authentication error.
            pub <- tryCatch(
              .dlGooglePublic(url = url, destinationPath = destinationPath,
                              targetFile = basename2(downloadFilename), verbose = 0),
              error = function(e2) NULL)
            if (is.null(pub)) stop(e)
            messagePreProcess("Downloaded public Google Drive file without authentication.",
                              verbose = verbose)
            pub
          })
      }
    }
  } else {
    messagePreProcess(messSkipDownload, verbose = verbose)
    needChecksums <- 0
  }

  return(list(destFile = destFile, needChecksums = needChecksums))
}

#' Download a *public* Google Drive file without authentication
#'
#' Internal. Used by [dlGoogle()] when the user supplies the public web-download
#' form of a Drive URL (or sets `reproducible.gdriveNoAuth = TRUE`). It fetches
#' the file over plain HTTPS via [dlGeneric()] -- no googledrive token required.
#'
#' Small files stream directly. Large files (>~100 MB, or any that trip Drive's
#' "can't scan for viruses" check) first return an HTML interstitial carrying a
#' one-time `confirm` token and `uuid`; these are parsed out and the request is
#' resubmitted to `drive.usercontent.google.com/download`. If the second attempt
#' still returns HTML, the file is almost certainly not shared "Anyone with the
#' link" and we error rather than silently save the page.
#'
#' @param url The original Google Drive URL (any form).
#' @param destinationPath Directory to download into.
#' @param targetFile The resolved Drive filename (so the on-disk name and
#'   checksum bookkeeping match the authenticated path).
#' @param verbose Numeric verbosity level.
#' @return A list with `destFile`.
#' @keywords internal
#' @rdname dot-dlGooglePublic
.dlGooglePublic <- function(url, destinationPath, targetFile,
                            verbose = getOption("reproducible.verbose", 1)) {
  id <- .googleDriveId(url)
  messagePreProcess("Public Google Drive file; downloading without authentication (id: ",
                    id, ").", verbose = verbose)
  pubUrl <- paste0("https://drive.usercontent.google.com/download?id=", id,
                   "&export=download&confirm=t")
  res <- dlGeneric(url = pubUrl, destinationPath = destinationPath,
                   targetFile = targetFile, applyRemap = FALSE, verbose = verbose)

  if (.looksLikeGoogleInterstitial(res$destFile)) {
    params <- .parseGoogleConfirm(res$destFile)
    if (length(params)) {
      pubUrl2 <- paste0("https://drive.usercontent.google.com/download?",
                        paste(names(params),
                              vapply(params, utils::URLencode, character(1), reserved = TRUE),
                              sep = "=", collapse = "&"))
      res <- dlGeneric(url = pubUrl2, destinationPath = destinationPath,
                       targetFile = targetFile, applyRemap = FALSE, verbose = verbose)
    }
    if (.looksLikeGoogleInterstitial(res$destFile)) {
      unlink(res$destFile)
      stop("Public Google Drive download returned an HTML page, not the file. ",
           "Confirm the file is shared as 'Anyone with the link'. URL: ", url)
    }
  }
  res
}

# Is a googledrive OAuth token currently loaded? Returns FALSE (never prompts /
# never errors) when googledrive is absent or no token has been acquired. A
# FALSE result at the download step means `assessGoogle()`'s metadata read
# succeeded anonymously, i.e. the file is public.
.gdriveHasToken <- function() {
  if (!requireNamespace("googledrive", quietly = TRUE)) return(FALSE)
  isTRUE(tryCatch(googledrive::drive_has_token(), error = function(e) FALSE))
}

# Can googledrive authenticate WITHOUT an interactive prompt? TRUE when the user
# has configured gargle for non-interactive auth: a specific OAuth email (so a
# cached token at `gargle_oauth_cache` is loaded silently) or a service-account
# JSON. Used to decide whether a token-less session should still attempt auth
# (and silently load a cached token) rather than fall back to anonymous access.
.gdriveCanAuthNonInteractively <- function() {
  email <- tryCatch(getOption("gargle_oauth_email"), error = function(e) NULL)
  wantsEmail <- (is.character(email) && length(email) == 1L && !is.na(email) && nzchar(email)) ||
    isTRUE(email)
  hasServiceAccount <- nzchar(Sys.getenv("GOOGLEDRIVE_AUTH", "")) ||
    nzchar(Sys.getenv("GARGLE_SERVICE_ACCOUNT", ""))
  isTRUE(wantsEmail) || isTRUE(hasServiceAccount)
}

# Should a Google Drive *metadata* read (drive_get/drive_ls in assessGoogle) be
# done anonymously, i.e. without triggering interactive OAuth?
#   * `reproducible.gdriveNoAuth = TRUE`  -> always anonymous (explicit opt-in);
#   * a token is already loaded           -> no (use it -- the "cloud writer" case);
#   * no token loaded, but gargle is configured to authenticate non-interactively
#     (an OAuth email or a service account) -> no: let drive_auth() silently load
#     the cached token. Going anonymous here was a regression that prevented a
#     configured user from ever authenticating.
#   * otherwise (no token, no usable auth config) -> yes: the typical "cloud
#     reader" / public-file case, where authenticating would only mean an
#     interactive prompt that we want to avoid.
.gdriveShouldGoAnon <- function(hadToken = .gdriveHasToken()) {
  if (isTRUE(getOption("reproducible.gdriveNoAuth", FALSE))) return(TRUE)
  if (isTRUE(hadToken)) return(FALSE)
  !.gdriveCanAuthNonInteractively()
}

# Put googledrive into anonymous (deauthorized) mode so a metadata read of a
# public ("Anyone with the link") file resolves via an API key instead of
# launching an interactive OAuth flow. Without this, `googledrive::drive_get()`
# with no cached token PROMPTS ("Is it OK to cache OAuth credentials ...") even
# for a public file. Returns a small list the caller uses to restore state:
#   - deauthed: did we deauthorize?
#   - token:    the previously-loaded token (or NULL) to restore on.exit, used
#               only in the edge case of `gdriveNoAuth = TRUE` *with* a token
#               present (so a cloud writer's token is never clobbered).
# A no-token deauthorization is left in place (there was nothing to restore, and
# the session was already token-less). No-op when googledrive is unavailable.
.gdriveDeauthForPublic <- function() {
  if (!requireNamespace("googledrive", quietly = TRUE)) return(list(deauthed = FALSE))
  hadToken <- .gdriveHasToken()
  if (!.gdriveShouldGoAnon(hadToken)) return(list(deauthed = FALSE))
  tok <- if (isTRUE(hadToken)) {
    tryCatch(googledrive::drive_token(), error = function(e) NULL)
  } else {
    NULL
  }
  try(googledrive::drive_deauth(), silent = TRUE)
  list(deauthed = TRUE, token = tok)
}

# A browser-pasteable URL for a Google Drive resource, for use in error messages.
# googledrive's own errors report only the bare fileId (e.g. "File not found:
# 13-atqi_..."), which the user cannot paste into a browser to check. If the
# input is already a Drive URL, echo it; if it is a bare ID, build the viewer
# URL; otherwise return it unchanged.
.gdriveBrowserUrl <- function(url) {
  u <- tryCatch(as.character(url)[1], error = function(e) NA_character_)
  if (length(u) != 1L || is.na(u) || !nzchar(u)) return(NA_character_)
  if (isTRUE(tryCatch(isGoogleDriveURL(u), error = function(e) FALSE))) return(u)
  if (isTRUE(tryCatch(isGoogleID(u), error = function(e) FALSE))) return(googledriveIDtoHumanURL(u))
  u
}

# Re-raise a googledrive metadata-access error with the full (pasteable) URL up
# front, preserving the original error detail (404 reason/location etc.).
.stopGoogleDriveAccess <- function(url, e) {
  browserUrl <- .gdriveBrowserUrl(url)
  stop("Could not access the Google Drive resource:\n  ",
       if (!is.na(browserUrl)) browserUrl else url,
       "\n  (open it in a browser to confirm it exists and is shared ",
       "'Anyone with the link')\n  ",
       conditionMessage(e), call. = FALSE)
}

# Does the downloaded file look like Google's HTML interstitial (sign-in page or
# virus-scan-warning download form) rather than the requested bytes?
.looksLikeGoogleInterstitial <- function(file) {
  if (!file.exists(file) || file.size(file) == 0) return(FALSE)
  # Read raw bytes (the file may be binary, e.g. a zip) and match on bytes so a
  # non-UTF-8 payload can't trip locale-translation warnings/errors in grepl().
  raw <- readBin(file, what = "raw", n = 4096L)
  txt <- rawToChar(raw[raw != as.raw(0L)])
  grepl("<!doctype html|<html", txt, ignore.case = TRUE, useBytes = TRUE) &&
    grepl("drive\\.usercontent\\.google\\.com|download-form|ServiceLogin|virus scan",
          txt, ignore.case = TRUE, useBytes = TRUE)
}

# Parse the hidden form inputs (id, export, confirm, uuid) from Drive's large-file
# interstitial so the download can be resubmitted with the one-time token.
.parseGoogleConfirm <- function(file) {
  html <- tryCatch(paste(readLines(file, warn = FALSE), collapse = " "),
                   error = function(e) "")
  inputs <- regmatches(html, gregexpr("<input[^>]*>", html, perl = TRUE))[[1]]
  out <- list()
  for (inp in inputs) {
    nm <- regmatches(inp, regexpr('(?<=name=")[^"]*', inp, perl = TRUE))
    vl <- regmatches(inp, regexpr('(?<=value=")[^"]*', inp, perl = TRUE))
    if (length(nm) && length(vl) && nzchar(nm)) out[[nm]] <- vl
  }
  out
}

#' Apply the user-supplied URL remap hook
#'
#' Internal. Consults `getOption("reproducible.urlRemap")`. When that option is a
#' function, it is called as `fn(url, filename)` once the target `filename` has
#' been resolved (which, for Google Drive URLs, requires the `drive_get()` lookup
#' in `assessGoogle()`). The function may return a replacement URL to download
#' from instead. A `NULL` return, a non-character or empty return, a return
#' identical to the original `url`, or *any error* (which emits a `warning()`
#' rather than failing the download) all mean "keep the original url". This way a
#' broken remap can never break a download.
#'
#' @param url The original (resolved) URL.
#' @param filename The resolved target filename, passed to the hook so the user
#'   can remap based on filename, the original url, or both.
#' @param verbose Numeric verbosity level.
#' @return A length-one character URL to download from (possibly the original).
#' @keywords internal
#' @rdname dot-applyUrlRemap
.applyUrlRemap <- function(url, filename, verbose = getOption("reproducible.verbose", 1)) {
  fn <- .urlRemapFn()
  if (is.null(fn)) {
    return(url)
  }
  newUrl <- tryCatch(
    fn(url, filename),
    error = function(e) {
      warning("reproducible.urlRemap function failed; using original url.\n  ",
              conditionMessage(e), call. = FALSE)
      NULL
    }
  )
  if (is.null(newUrl) || !is.character(newUrl) || length(newUrl) != 1L ||
      is.na(newUrl) || !nzchar(newUrl) || identical(newUrl, url)) {
    return(url)
  }
  messagePreProcess("URL remapped via 'reproducible.urlRemap':\n  ", url, "\n  -> ", newUrl,
                    verbose = verbose)
  newUrl
}

#' Resolve the `reproducible.urlRemap` option to a remap function
#'
#' Internal. The `reproducible.urlRemap` option may be set to any of:
#'   * `NULL` -- no remapping;
#'   * a `function(url, filename)` (e.g. built with [makeUrlRemap()]);
#'   * a `data.frame`/`data.table` manifest with `filename` and `url` columns; or
#'   * a length-one character path or URL to a CSV with those columns.
#' This returns a single `function(url, filename)` (or `NULL`). For the
#' `data.frame`/character forms it builds the function once via [makeUrlRemap()]
#' and caches it (keyed on the option value), so the lookup is not rebuilt -- and
#' a CSV path/URL is not re-read -- on every download. An invalid option value is
#' ignored with a warning (returns `NULL`), so it can never break a download.
#'
#' @param opt The current value of the `reproducible.urlRemap` option.
#' @return A `function(url, filename)` or `NULL`.
#' @keywords internal
#' @rdname dot-urlRemapFn
.urlRemapFn <- function(opt = getOption("reproducible.urlRemap")) {
  if (is.null(opt)) return(NULL)
  if (is.function(opt)) return(opt)

  # data.frame manifest or CSV path/URL: build (and cache) the remap function.
  cache <- .pkgEnv[["urlRemapCache"]]
  if (!is.null(cache) && identical(cache$key, opt)) return(cache$fn)

  fn <- tryCatch({
    if (is.data.frame(opt)) {
      makeUrlRemap(opt)
    } else if (is.character(opt) && length(opt) == 1L && nzchar(opt)) {
      makeUrlRemap(utils::read.csv(opt, stringsAsFactors = FALSE))
    } else {
      stop("must be NULL, a function, a data.frame manifest, or a CSV path/URL")
    }
  }, error = function(e) {
    warning("reproducible.urlRemap is invalid; ignoring it.\n  ",
            conditionMessage(e), call. = FALSE)
    NULL
  })
  .pkgEnv[["urlRemapCache"]] <- list(key = opt, fn = fn)
  fn
}

#' Build a URL remap function from a manifest
#'
#' Convenience constructor for the `reproducible.urlRemap` option (see
#' [preProcess()]). Given a `data.frame` with (at least) columns `filename` and
#' `url`, it returns a function `function(url, filename)` suitable for
#' `options(reproducible.urlRemap = ...)`. The returned function matches on the
#' basename of the resolved `filename`: when a manifest row's `filename` matches,
#' its `url` is returned, so the download is redirected there (and, if that URL
#' supports HTTP Range requests, the parallel download path applies). When there
#' is no match it returns `NULL`, so the original URL is kept.
#'
#' An optional `id` column (also accepted as `googledriveId`, `googledrive_id`,
#' `driveId` or `gid`) holding the Google Drive file id enables a **secondary**
#' match by id: when the resolved `filename` is unavailable — e.g. an
#' unauthenticated session that cannot read a Drive file's metadata — the id is
#' parsed from the Drive `url` and matched against this column. This lets a
#' download be redirected to the (public) mirror with no authentication at all.
#'
#' The manifest itself — and the responsibility for keeping it current — lives
#' with the user (for example, a community-maintained mirror manifest);
#' `reproducible` hard-codes no mirror URLs.
#'
#' Calling `makeUrlRemap()` yourself is optional: you can also assign the manifest
#' `data.frame` (or a CSV path/URL) directly to the option, e.g.
#' `options(reproducible.urlRemap = read.csv("manifest.csv"))`, and `reproducible`
#' will build (and cache) the remap function internally. See the `urlRemap` entry
#' in [reproducibleOptions()].
#'
#' @param manifest A `data.frame` (or `data.table`) with at least the character
#'   columns `filename` and `url`. `filename` is matched against the basename of
#'   the file being downloaded. An optional `id` column (Google Drive file id) is
#'   matched secondarily, by the id parsed from the Drive `url`, when no filename
#'   is available (e.g. an unauthenticated Drive download).
#'
#' @return A function of `(url, filename)` returning a replacement URL, or `NULL`
#'   to keep the original.
#' @seealso [preProcess()] for the `reproducible.urlRemap` option.
#' @export
#' @examples
#' \donttest{
#' manifest <- data.frame(
#'   filename = "SCANFI_att_biomass_2010_v2_20260119.tif",
#'   url = paste0(
#'     "https://object-arbutus.cloud.computecanada.ca/predictiveecology/",
#'     "SCANFI_v2/2010/SCANFI_att_biomass_2010_v2_20260119.tif"
#'   )
#' )
#' options(reproducible.urlRemap = makeUrlRemap(manifest))
#' }
makeUrlRemap <- function(manifest) {
  needed <- c("filename", "url")
  if (!is.data.frame(manifest) || !all(needed %in% colnames(manifest))) {
    stop("'manifest' must be a data.frame with columns 'filename' and 'url'")
  }
  urls <- as.character(manifest[["url"]])
  byName <- urls
  names(byName) <- basename2(as.character(manifest[["filename"]]))
  # Drop rows with empty/NA key or value up front.
  keepN <- nzchar(names(byName)) & !is.na(byName) & nzchar(byName)
  byName <- byName[keepN]

  # Optional secondary lookup by Google Drive id. When present, a download can be
  # remapped even if the filename is unknown -- e.g. an unauthenticated session
  # that cannot resolve a Drive file's name -- by parsing the id out of the url.
  # Recognise a few likely column names.
  idCol <- intersect(c("id", "googledriveId", "googledrive_id", "driveId", "gid"),
                     colnames(manifest))
  byId <- character(0)
  if (length(idCol)) {
    ids <- as.character(manifest[[idCol[[1]]]])
    byId <- urls
    names(byId) <- ids
    keepI <- nzchar(names(byId)) & names(byId) != "NA" & !is.na(byId) & nzchar(byId)
    byId <- byId[keepI]
  }

  function(url, filename) {
    # Primary: match on the basename of the resolved filename (a single file). A
    # length != 1 `filename` (e.g. a Drive directory of several files) has no
    # single mirror URL, so it is skipped here.
    if (length(filename) == 1L && !is.na(filename) && nzchar(filename)) {
      hit <- byName[basename2(filename)]
      if (length(hit) == 1L && !is.na(hit)) return(unname(hit))
    }
    # Secondary: match on the Google Drive id parsed from the url. This lets an
    # unauthenticated caller (no resolved filename) still redirect to the mirror.
    if (length(byId) && length(url) == 1L && !is.na(url)) {
      id <- .extractDriveId(url)
      if (!is.na(id) && nzchar(id)) {
        hit <- byId[id]
        if (length(hit) == 1L && !is.na(hit)) return(unname(hit))
      }
    }
    NULL
  }
}

# Extract a Google Drive file/folder id from a Drive URL or a bare id. Returns
# NA_character_ when none is found. Regex-based, so it needs no googledrive token
# (the point is to match a manifest by id when the file cannot be authenticated).
.extractDriveId <- function(url) {
  u <- tryCatch(as.character(url)[1], error = function(e) NA_character_)
  if (length(u) != 1L || is.na(u) || !nzchar(u)) return(NA_character_)
  if (isTRUE(tryCatch(isGoogleID(u), error = function(e) FALSE))) return(u)
  m <- regmatches(u, regexec("(?:/d/|[?&]id=|/folders/)([A-Za-z0-9_-]{15,})", u, perl = TRUE))[[1]]
  if (length(m) >= 2L && nzchar(m[[2]])) m[[2]] else NA_character_
}

#' Resolve and remap a URL early, before the COG fast-path
#'
#' Internal. Called near the top of [prepInputs()] so that a Google Drive URL can
#' be redirected to a Range-capable mirror *before* the COG fast-path decision.
#' [prepInputsCOG()] only triggers for `https://....tif`-style URLs, so a Drive
#' URL or bare Drive ID would otherwise never benefit from partial `/vsicurl/`
#' reads. When `reproducible.urlRemap` is set and `url` is a Drive URL/ID, this
#' resolves its filename (via the cached `assessGoogle()` `drive_get()` lookup)
#' and applies the remap; for plain HTTP(S) URLs it remaps on `basename(url)`.
#' With no remap set, or any failure resolving the Drive filename, the original
#' `url` is returned unchanged.
#'
#' @param url The original URL (a Google Drive URL, a bare Drive ID, or HTTP(S)).
#' @param verbose Numeric verbosity level.
#' @param ... May carry `team_drive`/`shared_drive` for the Drive lookup.
#' @return A length-one character URL (possibly the original).
#' @keywords internal
#' @rdname dot-remapUrlEarly
.remapUrlEarly <- function(url, verbose = getOption("reproducible.verbose", 1), ...) {
  if (is.null(url) || length(url) != 1L || isNULLorNA(url)) {
    return(url)
  }
  if (is.null(.urlRemapFn())) {
    return(url)
  }

  isGID <- tryCatch(isGoogleDriveURL(url) || isGoogleID(url), error = function(e) FALSE)
  if (isTRUE(isGID)) {
    # First try an id-based remap that needs neither authentication nor a
    # resolved filename: if the manifest lists this Drive id, redirect to the
    # (public) mirror and skip the Drive metadata lookup -- and its auth --
    # entirely. Only for single files (a folder has no single mirror URL), and a
    # no-op (falls through) when there is no id match or no id column.
    isDir <- tryCatch(isTRUE(isDirectory(url, FALSE)), error = function(e) FALSE)
    if (!isDir) {
      early <- .applyUrlRemap(url, filename = NA_character_, verbose = verbose)
      if (!identical(early, url)) {
        return(early)
      }
    }
    if (!requireNamespace("googledrive", quietly = TRUE)) {
      return(url)
    }
    # Resolve the Drive ID -> filename (cached) so the manifest can match on it.
    df <- tryCatch(
      assessGoogle(url, verbose = verbose - 1, team_drive = getTeamDrive(list(...))),
      error = function(e) NULL
    )
    if (is.null(df)) {
      return(url)
    }
    filename <- basename2(df)
  } else if (grepl("^https?://", url)) {
    filename <- basename2(url)
  } else {
    return(url)
  }
  # Only remap a single resolved file. A Google Drive *directory* resolves to
  # several filenames; there is no single mirror URL for a folder, so leave it
  # to the normal per-file download path.
  if (length(filename) != 1L || !nzchar(filename)) {
    return(url)
  }
  .applyUrlRemap(url, filename, verbose = verbose)
}

#' Download file from generic source url
#'
#' @param url  The url (link) to the file.
#' @param targetFile Optional basename to give the downloaded file. When supplied
#'   (e.g. by [dlGoogle()] after a URL remap) the file is named with this rather
#'   than `basename(url)`, so the rest of the pipeline sees the expected filename.
#' @param applyRemap Logical. When `TRUE` (default) the `reproducible.urlRemap`
#'   hook is consulted here. Callers that have already applied the remap (e.g.
#'   [dlGoogle()] delegating a remapped Drive URL) pass `FALSE` to avoid a
#'   second call.
#'
#' @author Eliot McIntire and Alex Chubaty
#' @keywords internal
#' @importFrom utils download.file
#' @inheritParams preProcess
dlGeneric <- function(url, destinationPath, targetFile = NULL, applyRemap = TRUE,
                      verbose = getOption("reproducible.verbose", 1)) {
  if (missing(destinationPath)) {
    destinationPath <- tempdir2(rndstr(1, 6))
  }

  haveTarget <- !is.null(targetFile) && length(targetFile) == 1 && nzchar(targetFile)
  # `wasRemapped` records whether this URL came from the `reproducible.urlRemap`
  # hook -- either remapped here, or handed in already-remapped via
  # `applyRemap = FALSE` (the only such caller is the Google Drive path recursing
  # with its mirror URL). The parallel ranged path is used ONLY for remapped
  # (mirror) URLs, never for an arbitrary range-capable origin server: such a
  # server may cap concurrent connections per IP, in which case parallel streams
  # all stall and the download is far slower than a single stream.
  wasRemapped <- !isTRUE(applyRemap)
  if (isTRUE(applyRemap)) {
    filename <- if (haveTarget) basename2(targetFile) else basename2(url)
    remappedUrl <- .applyUrlRemap(url, filename, verbose = verbose)
    wasRemapped <- !identical(remappedUrl, url)
    url <- remappedUrl
  }

  bn <- if (haveTarget) basename2(targetFile) else basename2(url)
  bn <- gsub("\\?|\\&", "_", bn) # causes errors with ? and maybe &
  destFile <- file.path(destinationPath, bn)

  # Feature A: parallel ranged download. Used ONLY for URLs that the
  # `reproducible.urlRemap` hook redirected to a (Range-capable) mirror -- see
  # `wasRemapped` above. When eligible, it engages if the server advertises
  # `Accept-Ranges: bytes` and the file exceeds the threshold; it can be forced
  # off with `reproducible.parallel.streams = 1L`. Any failure (including a mirror
  # that turns out to cap concurrency) falls through to the single-stream path
  # below, which is byte-for-byte unchanged, so checksums are unaffected.
  pp <- if (isTRUE(wasRemapped)) .useParallelDownload(url, verbose = verbose) else list(use = FALSE, info = NULL)
  if (isTRUE(pp$use)) {
    streams <- as.integer(getOption("reproducible.parallel.streams", 48L))
    messagePreProcess("Downloading ", url, " using ", streams,
                      " ranged parts (capped at ",
                      .parallelMaxConnections(), " concurrent connections) ...", verbose = verbose)
    ok <- tryCatch(
      .parallelRangedDownload(url, destFile, pp$info$size, n = streams, verbose = verbose),
      error = function(e) {
        messagePreProcess("Parallel download failed (", conditionMessage(e),
                          "); falling back to single stream.", verbose = verbose)
        FALSE
      }
    )
    if (isTRUE(ok)) {
      return(list(destFile = destFile))
    }
    if (file.exists(destFile)) unlink(destFile) # clean partial before fallback
  }

  # if (suppressWarnings(httr::http_error(url))) ## TODO: http_error is throwing warnings
  #   stop("Can not access url ", url)

  messagePreProcess("Downloading ", url, " ...", verbose = verbose)

  needDwnFl <- TRUE # this will try download.file if no httr2 or httr2 fails
  # R version 4.1.3 doesn't have httr2 that can do these steps; httr2 is too old, I believe

  if (.requireNamespace("httr2") && .requireNamespace("curl")) {
    for (i in 1:2) {
      totalTimeout <- getOption("reproducible.timeout", 12000)
      # `connecttimeout` is the cap on *establishing* the connection (TLS
      # handshake etc.), NOT the overall download time. It must stay short so a
      # stalled/flaky handshake fails quickly and gets retried, instead of hanging
      # for the full `reproducible.timeout` (which can be hours -- a slow/failing
      # SSL connect was freezing sessions for many minutes). Mirrors the parallel
      # path's `reproducible.parallel.connecttimeout`. The overall request timeout
      # (`req_timeout`) stays at `totalTimeout`.
      req <- httr2::request(url) |>
        httr2::req_timeout(totalTimeout) |>
        httr2::req_options(
          connecttimeout = as.integer(getOption("reproducible.connecttimeout", 30L))[1]
        )
      if (i == 1) # only try on first run through, in case this is the cause of failure; which it is on some sites
        req <- req |> httr2::req_user_agent(getOption("reproducible.useragent"))
      # Progress reporting. httr2::req_progress() draws a cli progress bar, which
      # is great in a dynamic terminal but emits *nothing* in non-interactive /
      # non-dynamic sessions (logged runs, CI, a SpaDES simInit) -- so a large
      # download there is completely silent until it finishes. In those sessions,
      # stream the body ourselves and report progress through messagePreProcess()
      # (which flows through the calling app's message handler and is timestamped);
      # see .dlHttr2Stream(). Keep the native cli bar for dynamic terminals where
      # it updates nicely in place.
      streamProg <- isTRUE(verbose > 0) &&
        !isTRUE(tryCatch(cli::is_dynamic_tty(), error = function(e) FALSE)) &&
        exists("req_perform_connection", envir = asNamespace("httr2")) &&
        exists("resp_stream_raw", envir = asNamespace("httr2"))
      if (isTRUE(verbose > 0) && !streamProg) {
        # req_progress is not in the binary httr2 available for R version 4.1.3; fails on CRAN checks
        # Also wrap in tryCatch: cli's app$styles can be NULL/NA in parallel/non-interactive contexts
        req <- tryCatch({
          reqProgress <- get("req_progress", envir = asNamespace("httr2"))
          req |> reqProgress()
        }, error = function(e) req)
      }

      reqq <- req |> httr2::req_url_query()
      resp <- if (streamProg) {
        .dlHttr2Stream(reqq, destFile, verbose = verbose)
      } else {
        reqq |> httr2::req_perform(path = destFile)
      }
      # Detect an HTML "Request Rejected" page (some servers return it with HTTP
      # 200). For the streamed path read only the head of the file rather than the
      # whole (possibly multi-GB) body into a string.
      a <- if (streamProg) .dlRejectionProbeFile(destFile) else httr2::resp_body_string(resp)
      isRjcted <- grepl("Request Rejected", a)
      if (!isTRUE(any(isRjcted)) && !httr2::resp_is_error(resp)) {
        needDwnFl <- FALSE
        break
      }
    }
  } else {
    messagePreProcess("If downloads fail; please install httr2 and try again")
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

#' Stream an httr2 download to a file, reporting progress via `messagePreProcess`
#'
#' `httr2::req_progress()` draws a cli progress bar, which is silent in
#' non-interactive / non-dynamic sessions (logged runs, CI, a SpaDES `simInit`)
#' and writes straight to the terminal -- so there a large download shows no
#' output at all until it completes. This streams the response body to `destFile`
#' in chunks and emits a periodic progress line through [messagePreProcess()]
#' instead, which flows through the calling app's message handler (and is
#' timestamped by, e.g., SpaDES.core's logging). Errors (including HTTP error
#' statuses, which [httr2::req_perform_connection()] raises just like
#' [httr2::req_perform()]) propagate to the caller unchanged. Returns the httr2
#' response object (its body already consumed to `destFile`).
#'
#' @param req An [httr2::request()].
#' @param destFile Destination file path.
#' @param verbose Numeric verbosity level.
#' @return The httr2 response (invisibly used by the caller for status checks).
#' @noRd
.dlHttr2Stream <- function(req, destFile, verbose = getOption("reproducible.verbose", 1)) {
  resp <- httr2::req_perform_connection(req)
  on.exit(try(close(resp), silent = TRUE), add = TRUE)
  total <- suppressWarnings(as.numeric(httr2::resp_header(resp, "content-length")))
  if (length(total) != 1L || is.na(total) || total <= 0) total <- NA_real_
  totTxt <- if (!is.na(total)) paste0(" / ", .humanBytes(total)) else ""

  con <- file(destFile, "wb")
  conOpen <- TRUE
  on.exit(if (conOpen) try(close(con), silent = TRUE), add = TRUE)

  interval <- getOption("reproducible.downloadProgressInterval", 2)
  t0 <- Sys.time()
  lastMsg <- t0
  got <- 0
  shownAny <- FALSE
  repeat {
    chunk <- httr2::resp_stream_raw(resp, kb = 512L)
    if (length(chunk) == 0L) break
    writeBin(chunk, con)
    got <- got + length(chunk)
    now <- Sys.time()
    if (as.numeric(now - lastMsg) >= interval) {
      elapsed <- as.numeric(now - t0)
      pct <- if (!is.na(total)) paste0(" (", round(100 * got / total), "%)") else ""
      rate <- if (elapsed > 0) paste0(" | ", .humanBytes(got / elapsed), "/s") else ""
      messagePreProcess("  downloaded ", .humanBytes(got), totTxt, pct, rate, verbose = verbose)
      lastMsg <- now
      shownAny <- TRUE
    }
  }
  close(con)
  conOpen <- FALSE
  # Only emit the closing line if we already reported progress, i.e. the download
  # ran longer than one interval. Fast/instant downloads (e.g. a local `file://`
  # source, as in the test fixtures) then stay completely silent -- matching the
  # old req_perform() behaviour and keeping message snapshots deterministic --
  # while genuinely long downloads still get periodic lines plus this summary.
  if (isTRUE(shownAny)) {
    messagePreProcess("  downloaded ", .humanBytes(got), totTxt, " in ",
                      round(as.numeric(Sys.time() - t0), 1), "s", verbose = verbose)
  }
  resp
}

#' Read the head of a just-downloaded file to detect an HTML rejection page
#'
#' Some servers answer with HTTP 200 but an HTML "Request Rejected" body. Reading
#' the whole (possibly multi-GB) file into a string to check for this is wasteful;
#' the marker is at the very start, so read only the first few KB.
#' @param destFile Path to the downloaded file.
#' @param n Number of bytes to read.
#' @return A character scalar (the head of the file, NUL bytes stripped), or `""`.
#' @noRd
.dlRejectionProbeFile <- function(destFile, n = 4096L) {
  if (!file.exists(destFile)) return("")
  con <- file(destFile, "rb")
  on.exit(close(con))
  raw <- readBin(con, what = "raw", n = n)
  # Keep only printable ASCII so rawToChar() yields a valid string and the
  # downstream grepl() does not warn on a binary (e.g. zip) body. The marker we
  # look for ("Request Rejected") is ASCII, so this never masks a real rejection.
  raw <- raw[raw >= as.raw(9L) & raw <= as.raw(126L)]
  if (length(raw) == 0L) return("")
  rawToChar(raw)
}

#' Human-readable byte count (e.g. `"45.2 Mb"`)
#'
#' Thin wrapper around base R's `format.object_size` so download progress lines
#' read naturally regardless of magnitude. Non-finite input returns `"?"`.
#' @param n A number of bytes.
#' @return A character scalar.
#' @noRd
.humanBytes <- function(n) {
  if (length(n) != 1L || !is.finite(n)) return("?")
  format(structure(n, class = "object_size"), units = "auto")
}

#' Decide whether to use the parallel ranged download path
#'
#' Internal policy helper for Feature A, separated from the download mechanism so
#' it can be unit-tested without network access. The parallel path is **gated on
#' the opt-in `reproducible.urlRemap` being set**: if no remap function is set,
#' it is never used, regardless of `reproducible.parallel.streams`. When a remap
#' is set, the path is used only when `reproducible.parallel.streams > 1` (the
#' default is `48L`; set it to `1L` to disable), the \pkg{curl} and \pkg{httr2}
#' packages are available, the server advertises `Accept-Ranges: bytes`, and the
#' file is larger than `reproducible.parallel.threshold`. When that gate is not
#' met, the (potentially network-touching) [.probeRange()] call is skipped
#' entirely.
#'
#' @param url The resolved URL.
#' @param streams Number of parallel streams (`reproducible.parallel.streams`).
#' @param threshold Size threshold in bytes.
#' @param verbose Numeric verbosity level.
#' @return A list with `use` (logical) and `info` (the [.probeRange()] result, or
#'   `NULL` when the probe was skipped).
#' @keywords internal
#' @rdname dot-useParallelDownload
.useParallelDownload <- function(url,
                                 streams = getOption("reproducible.parallel.streams", 48L),
                                 threshold = getOption("reproducible.parallel.threshold", 10 * 1024^2),
                                 verbose = getOption("reproducible.verbose", 1)) {
  # Opt-in gate: the parallel path exists ONLY to accelerate downloads that the
  # user has redirected to a Range-capable mirror. If `reproducible.urlRemap` is
  # not set, there is no opt-in and the parallel path is never used, regardless
  # of `reproducible.parallel.streams`. This is also a hard short-circuit before
  # any (potentially network-touching) probe.
  if (is.null(.urlRemapFn())) {
    return(list(use = FALSE, info = NULL))
  }
  # Capability gate -- deps present and not explicitly forced to single-stream.
  if (!(is.numeric(streams) && isTRUE(streams > 1L) &&
        .requireNamespace("curl") && .requireNamespace("httr2"))) {
    return(list(use = FALSE, info = NULL))
  }
  info <- .probeRange(url, verbose = verbose)
  use <- isTRUE(info$acceptRanges) && !is.na(info$size) && info$size > threshold
  list(use = use, info = info)
}

#' Probe a URL for size and HTTP Range support
#'
#' Internal helper for the parallel ranged download path. Sends a HEAD request
#' and reads `Content-Length` and `Accept-Ranges`. Any failure (HEAD not
#' supported, timeout, httr2 unavailable) returns `size = NA` /
#' `acceptRanges = FALSE`, which the caller treats as "use single-stream".
#'
#' @param url The URL to probe.
#' @param verbose Numeric verbosity level.
#' @return A list with `size` (numeric, bytes or `NA`) and `acceptRanges`
#'   (logical).
#' @keywords internal
#' @rdname dot-probeRange
.probeRange <- function(url, verbose = getOption("reproducible.verbose", 1)) {
  out <- list(size = NA_real_, acceptRanges = FALSE)
  if (!.requireNamespace("httr2")) {
    return(out)
  }
  resp <- tryCatch(
    {
      req <- httr2::request(url) |>
        httr2::req_method("HEAD") |>
        httr2::req_timeout(getOption("reproducible.timeout", 12000)) |>
        httr2::req_user_agent(getOption("reproducible.useragent"))
      httr2::req_perform(req)
    },
    error = function(e) NULL
  )
  if (is.null(resp) || httr2::resp_is_error(resp)) {
    return(out)
  }
  cl <- httr2::resp_header(resp, "content-length")
  ar <- httr2::resp_header(resp, "accept-ranges")
  if (!is.null(cl)) {
    out$size <- suppressWarnings(as.numeric(cl))
  }
  # "Accept-Ranges: bytes" => supported; "none" or absent => not supported
  out$acceptRanges <- !is.null(ar) && isTRUE(tolower(trimws(ar)) == "bytes")
  out
}

#' Format a duration in seconds as a compact human-readable string
#'
#' Internal helper for download progress reporting: `90` -> `"1m30s"`,
#' `3725` -> `"1h02m05s"`, `5` -> `"5s"`. A non-finite or negative input
#' (e.g. an ETA before there is any signal) returns `"--"`.
#'
#' @param secs Numeric, seconds.
#' @return A length-one character string.
#' @keywords internal
#' @rdname dot-formatDuration
.formatDuration <- function(secs) {
  if (length(secs) != 1L || !is.finite(secs) || secs < 0) {
    return("--")
  }
  secs <- round(secs)
  h <- secs %/% 3600L
  m <- (secs %% 3600L) %/% 60L
  s <- secs %% 60L
  if (h > 0L) {
    sprintf("%dh%02dm%02ds", h, m, s)
  } else if (m > 0L) {
    sprintf("%dm%02ds", m, s)
  } else {
    sprintf("%ds", s)
  }
}

# Maximum number of *simultaneous* connections for the parallel ranged download.
# Controlled by `reproducible.parallel.maxConnections`; when that is unset (NULL)
# or not a valid number, the ceiling is `availableCores() - 1` -- using
# `parallelly::availableCores()` when that (Suggested) package is installed, else
# falling back to base `parallel::detectCores()`. Always at least 1. This bounds
# the burst of concurrent TLS handshakes (independent of how many parts the file
# is split into), which is what some stacks -- notably Windows -- reject when all
# `reproducible.parallel.streams` open at once.
.parallelMaxConnections <- function() {
  mc <- suppressWarnings(as.integer(getOption("reproducible.parallel.maxConnections", NULL))[1])
  if (is.na(mc)) {
    mc <- tryCatch({
      cores <- if (requireNamespace("parallelly", quietly = TRUE)) {
        parallelly::availableCores()
      } else {
        parallel::detectCores() # base package; always available
      }
      as.integer(cores)[1] - 1L
    }, error = function(e) NA_integer_)
  }
  if (is.na(mc) || mc < 1L) 1L else mc
}

#' Download a file in parallel using HTTP Range requests
#'
#' Internal helper for the opt-in parallel download path (Feature A). Splits
#' `[0, size)` into `n` contiguous byte ranges and fetches them concurrently
#' with one `curl` handle per part (each carrying its own `Range:` header) via
#' `curl`'s multi interface. On success the parts are concatenated **in order**
#' and the total size is verified to equal `size`, so the result is
#' byte-identical to a single-stream download. Returns `FALSE` (rather than
#' erroring) on any partial failure or size mismatch, so the caller can fall
#' back to single-stream.
#'
#' @param url The (already-resolved, range-capable) URL.
#' @param destFile The destination file path to assemble into.
#' @param size Total expected size in bytes (from [.probeRange()]).
#' @param n Number of parallel streams (`reproducible.parallel.streams`).
#' @param verbose Numeric verbosity level.
#' @return `TRUE` on verified success, otherwise `FALSE`.
#' @keywords internal
#' @rdname dot-parallelRangedDownload
.parallelRangedDownload <- function(url, destFile, size, n,
                                    verbose = getOption("reproducible.verbose", 1)) {
  .requireNamespace("curl", stopOnFALSE = TRUE)
  size <- as.numeric(size)
  if (!is.finite(size) || size <= 0) {
    return(FALSE)
  }
  n <- max(1L, as.integer(n))

  # Contiguous, non-overlapping byte ranges covering [0, size). Use a numeric
  # (double) sequence so files > .Machine$integer.max (2 GB) are handled.
  breaks <- unique(floor(seq(0, size, length.out = n + 1L)))
  lo <- breaks[-length(breaks)]
  hi <- c(breaks[-1] - 1)
  hi[length(hi)] <- size - 1 # last part runs to the final byte
  nParts <- length(lo)

  partFiles <- paste0(destFile, sprintf(".part%03d", seq_len(nParts)))
  on.exit(unlink(partFiles), add = TRUE)

  okPart <- logical(nParts)
  failMsg <- rep(NA_character_, nParts) # last failure reason per part (diagnostics)
  expected <- hi - lo + 1 # expected byte count for each part
  totOS <- structure(size, class = "object_size")

  # Compact, de-duplicated summary of why a set of parts failed, e.g.
  # "Couldn't connect to server (x44); Timeout was reached (x2)". Surfaced on
  # retries and on fallback so platform-specific failures (notably on Windows,
  # where opening many simultaneous connections can be refused) are visible
  # instead of silently triggering a single-stream fallback.
  reasonSummary <- function(idx) {
    msgs <- failMsg[idx]
    msgs <- msgs[!is.na(msgs) & nzchar(msgs)]
    if (!length(msgs)) return("no error detail reported")
    tab <- sort(table(msgs), decreasing = TRUE)
    paste(sprintf("%s (x%d)", names(tab), as.integer(tab)), collapse = "; ")
  }

  # Fetch a set of part indices concurrently, streaming each body straight to
  # its `partFiles[j]` (`data = <file>` => never held in memory). curl's
  # new_pool() defaults to host_con = 6 (max concurrent connections per host),
  # which would silently cap parallelism regardless of `n`; raise it to the
  # number of parts being fetched so all requested streams run at once. The
  # low-level multi pool has no built-in progress bar (unlike
  # curl::multi_download, which can't set per-part Range headers), so when
  # asked we poll in short slices and report the on-disk aggregate.
  fetchParts <- function(idx, showProgress) {
    # Cap the number of *simultaneous* connections. The file is still split into
    # many small parts (so a single drop costs only one cheap re-fetch), but
    # opening all of them at once (up to `reproducible.parallel.streams`, e.g. 48
    # TLS handshakes in a burst) is rejected by some stacks -- notably Windows --
    # failing most parts at connection time. curl's pool runs at most `maxCon`
    # concurrently and queues the rest. See .parallelMaxConnections().
    maxCon <- min(.parallelMaxConnections(), length(idx))
    pool <- curl::new_pool(total_con = maxCon, host_con = maxCon)
    for (i in idx) {
      h <- curl::new_handle(url = url)
      curl::handle_setheaders(h, Range = sprintf("bytes=%.0f-%.0f", lo[i], hi[i]))
      curl::handle_setopt(h,
        # Per-connection establishment timeout (seconds). This is NOT the overall
        # download timeout (`reproducible.timeout`, which can be hours): it is a
        # short, dedicated cap so a stalled handshake fails its own part quickly
        # and gets retried, rather than hanging.
        connecttimeout = as.integer(getOption("reproducible.parallel.connecttimeout", 30L))[1],
        useragent = getOption("reproducible.useragent")
      )
      local({
        j <- i
        curl::multi_add(
          handle = h,
          # 206 = Partial Content (ranged); 200 = server ignored Range (whole file)
          done = function(res) {
            okPart[j] <<- isTRUE(res$status_code %in% c(200L, 206L))
            if (!okPart[j]) failMsg[j] <<- paste0("HTTP status ", res$status_code)
          },
          fail = function(str) {
            okPart[j] <<- FALSE
            failMsg[j] <<- str
          },
          data = partFiles[j],
          pool = pool
        )
      })
    }
    if (isTRUE(showProgress)) {
      startTime <- Sys.time()
      repeat {
        st <- curl::multi_run(timeout = 0.5, poll = FALSE, pool = pool)
        done <- sum(file.size(partFiles), na.rm = TRUE)
        doneOS <- structure(done, class = "object_size")
        elapsed <- as.numeric(difftime(Sys.time(), startTime, units = "secs"))
        # ETA from the average rate so far (bytes/sec); blank until there is signal
        eta <- if (done > 0 && elapsed > 0) elapsed * (size - done) / done else NA_real_
        cat(sprintf("\r  %s of %s via %d concurrent ranged streams | elapsed time: %s | estimated time left: %s        ",
                    format(doneOS, units = "auto"), format(totOS, units = "auto"), maxCon,
                    .formatDuration(elapsed), .formatDuration(eta)))
        utils::flush.console()
        if (st$pending == 0) break
      }
      cat("\n")
    } else {
      curl::multi_run(pool = pool)
    }
  }

  # Parts that are missing, short, or errored -- candidates for a retry.
  badParts <- function() {
    sz <- file.size(partFiles)
    which(!okPart | is.na(sz) | sz != expected)
  }

  # Fetch all parts, then RETRY ONLY the failed/short parts a few times before
  # giving up. A single dropped connection (common with many parallel streams
  # on a long transfer) thus costs one part re-fetch (~size/n bytes), not a full
  # single-stream re-download of the whole file.
  showProgress <- verbose > 0 && interactive()
  todo <- seq_len(nParts)
  maxAttempts <- 3L
  # If the FIRST concurrent attempt completes fewer than this fraction of parts,
  # the (mirror) server is not honouring the requested concurrency -- e.g. it
  # caps connections per IP, so most parts stall with 0 bytes and burn the
  # connect timeout. Retrying is then futile and slow, so bail out immediately
  # and let the caller fall back to a single stream.
  minConcurrentFrac <- getOption("reproducible.parallel.minConcurrentFrac", 0.25)
  for (attempt in seq_len(maxAttempts)) {
    unlink(partFiles[todo]) # clear any partial bytes before (re)fetching
    okPart[todo] <- FALSE
    fetchParts(todo, showProgress = isTRUE(showProgress) && attempt == 1L)
    todo <- badParts()
    if (!length(todo)) break
    if (attempt == 1L && (nParts - length(todo)) < ceiling(nParts * minConcurrentFrac)) {
      messagePreProcess("Only ", nParts - length(todo), " of ", nParts,
                        " parts completed concurrently (reason(s): ", reasonSummary(todo),
                        "); the server appears to limit concurrent connections. ",
                        "Falling back to single stream.", verbose = verbose)
      return(FALSE)
    }
    if (attempt < maxAttempts) {
      messagePreProcess("Retrying ", length(todo), " incomplete download part(s) (attempt ",
                        attempt + 1L, " of ", maxAttempts, "); reason(s): ",
                        reasonSummary(todo), " ...", verbose = verbose)
    }
  }
  if (length(todo)) { # a part still won't complete -> let caller fall back
    messagePreProcess(length(todo), " of ", nParts, " parts still failed after ",
                      maxAttempts, " attempts; reason(s): ", reasonSummary(todo),
                      ". Falling back to single stream.", verbose = verbose)
    return(FALSE)
  }

  # Assemble in order via file.append (streams bytes, no in-R copy), deleting
  # each part as soon as it has been appended. As destFile grows the parts
  # shrink, so the transient temp-space peak stays ~= the final file size
  # (rather than ~2x it, which would matter for multi-GB files split many ways).
  if (file.exists(destFile)) unlink(destFile)
  if (!file.create(destFile)) {
    return(FALSE)
  }
  for (pf in partFiles) {
    if (!isTRUE(file.append(destFile, pf))) {
      unlink(destFile)
      return(FALSE)
    }
    unlink(pf) # free this part's bytes before appending the next
  }
  if (isTRUE(file.size(destFile) != size)) {
    unlink(destFile)
    return(FALSE)
  }
  messagePreProcess("Download of ", destFile, " complete (", nParts,
                    " parallel ranged streams)", verbose = verbose)
  TRUE
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
          # Must mirror the `recursive = TRUE` snapshot below; otherwise files
          # that were already in subdirectories of destinationPath (e.g. files
          # extracted there by an earlier prepInputs() call into the same
          # `reproducible.inputPaths` stash) are absent from the "before" set
          # and the setdiff() at the post-dlFun snapshot incorrectly classifies
          # them as newly created. They then propagate as `downloadResults$destFile`
          # and trip "already exists at <stash path>" in the desiredPath check.
          fileInfo <- file.info(dir(destinationPath, recursive = TRUE,
                                    full.names = TRUE))
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

      if (is.null(out) && !is.null(url)) { # if url is NULL and out is NULL; means dlFun did all the work
        isGID <- isGoogleDriveURL(url) || isGoogleID(url)
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
    on.exit(options(opts), add = TRUE)
  }

  # Resolve a PUBLIC file's metadata without launching interactive OAuth. With no
  # cached token, `drive_get()`/`drive_ls()` below would otherwise prompt ("Is it
  # OK to cache OAuth credentials ...") even for an "Anyone with the link" file --
  # the metadata read happens before (and so defeats) the no-auth download path.
  # Deauthorizing makes googledrive use an API key, so a public file resolves
  # anonymously. A loaded token (cloud writer) is preserved; in the edge case of
  # `gdriveNoAuth = TRUE` with a token present, it is restored on exit.
  .anonRead <- .gdriveDeauthForPublic()
  if (isTRUE(.anonRead$deauthed) && !is.null(.anonRead$token)) {
    on.exit(try(googledrive::drive_auth(token = .anonRead$token), silent = TRUE), add = TRUE)
  }

  # Cache the drive_get / drive_ls result indefinitely. The Cache key
  # includes the URL/ID, so each distinct file pays one API hit ever per
  # cachePath; subsequent calls (in-memory or on-disk) are near-instant.
  # Without this, .guessAtFile (and therefore the very first phase of
  # pp_resolve_files) hits the Google Drive API on every call, ~1-2 s of
  # latency that the sidecar fast-path further down the pipeline cannot
  # recover.
  #
  # Do NOT wrap the inner call in `quote(...)`: Cache's digest treats a
  # `quote(...)` expression as opaque text and never resolves the `url`
  # symbol against the calling frame, so every URL collides on the same
  # cache key and the first-cached fileAttr is returned for every later
  # URL. retry() captures its `expr` argument with substitute() and works
  # the same way whether or not it's wrapped in quote().
  # if (is.null(archive) || is.na(archive)) {
  fileAttr <- tryCatch({
    if (isTRUE(isDirectory(url, FALSE))) {
      Cache(
        retry(retries = 1, googledrive::drive_ls(googledrive::as_id(url),
                                                 shared_drive = team_drive)),
        verbose = FALSE
      )
    } else {
      if (packageVersion("googledrive") < "2.0.0") {
        Cache(
          retry(retries = 1, googledrive::drive_get(googledrive::as_id(url),
                                                    team_drive = team_drive)),
          verbose = FALSE
        )
      } else {
        Cache(
          retry(retries = 1, googledrive::drive_get(googledrive::as_id(url),
                                                    shared_drive = team_drive)),
          verbose = FALSE
        )
      }
    }
  }, error = function(e) .stopGoogleDriveAccess(url, e))

  fileSize <- sapply(fileAttr$drive_resource, function(x) x$size)
  if (!is.null(unlist(fileSize))) {
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
  # } else {
  #   downloadFilename <- archive
  # }
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
  Sys.setenv(R_LIBCURL_SSL_REVOKE_BEST_EFFORT = TRUE)
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
                            fileToDownload, destinationPath, targetFile, checksumFile, verbose) {
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
      urlMessage <- googledriveIDtoHumanURL(url)
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
#' @return NULL. Run for its side effect, namely, and file removed from the \file{CHECKSUMS.txt}
#'   file.
purgeChecksums <- function(checksumFile, fileToRemove) {
  dt <- data.table::fread(checksumFile)
  toPurge <- dt[file %in% fileToRemove]
  dtNew <- dt[!toPurge, on = c("file", "checksum")]
  data.table::fwrite(dtNew, file = checksumFile)
}

download_resumable_httr2 <- function(file_name, local_path, gdriveDetails, fileSize = NULL,
                                     verbose = getOption("reproducible.verbose")) {
  .requireNamespace("googledrive", stopOnFALSE = TRUE)

  ## Normalize path to avoid issues with ~
  local_path_expanded <- normalizePath(local_path, mustWork = FALSE)

  if (missing(gdriveDetails)) {
    isGD <- isGoogleDriveURL(file_name) || inherits(file_name, "drive_id")

    completed <- FALSE
    if (isGD) {
      gdriveDetails <- googledrive::drive_get(file_name)
    }
  } else {
    isGD <- TRUE
  }

  if (isGD) {
    file_id <- gdriveDetails$id
    fileSize <- as.numeric(gdriveDetails$drive_resource[[1]]$size)
    file_name <- googledriveIDtoDownloadURL(file_id)
    bearer <- .get_fresh_gd_bearer()  # refresh token via gargle before use
  } else {
    if (is.null(fileSize)) {
      fileSize <- getRemoteFileSize(isGD, url)
    }
  }

  if ( (isGD &&  (.Platform$OS.type == "windows")) || nzchar(Sys.which("curl")) %in% FALSE ||
      fileSize < 1e9) { # i.e., < 1GB can just use the simpler httr2 progress
    ## Google Drive download using httr2 (no resume support)
    ## Retry once on 401: the token may have expired mid-session; refresh and retry.
    con <- file(local_path_expanded, open = "wb")
    on.exit(try(close(con), silent = TRUE), add = TRUE)

    for (.attempt in 1:2) {
      req <- httr2::request(file_name)
      if (isGD) req <- req |> httr2::req_auth_bearer_token(bearer)
      req <- tryCatch(req |> httr2::req_progress(), error = function(e) req)

      err <- tryCatch({
        resp <- httr2::req_perform(req)
        body <- httr2::resp_body_raw(resp)
        writeBin(body, con)
        completed <- TRUE
        NULL
      }, error = function(e) e)

      if (is.null(err)) break  # success

      is401 <- inherits(err, "httr2_http_401") ||
        grepl("401", conditionMessage(err), fixed = TRUE)
      if (.attempt < 2L && is401 && isGD) {
        messagePreProcess("Google Drive token expired (HTTP 401); refreshing and retrying...",
                          verbose = verbose)
        bearer <- .get_fresh_gd_bearer(force_refresh = TRUE)
      } else {
        stop("Google Drive download failed: ", conditionMessage(err))
      }
    }

  } else {
    if (.Platform$OS.type != "windows" && nzchar(Sys.which("curl"))) {
      # Use download.file with curl on Linux/macOS
      method <- "curl"
      if (!isGD) {
        extra_args <- "-C -"
        messagePreProcess("Using 'curl' with resume support on Linux/macOS.", verbose = verbose)
      }

      for (.attempt in 1:2) {
        if (isGD)
          extra_args <- paste("-L -H", shQuote(paste("Authorization: Bearer", bearer)))
        err <- tryCatch({
          utils::download.file(
            url = file_name,
            destfile = local_path_expanded,
            method = method,
            quiet = verbose < 1,
            extra = extra_args
          )
          completed <- TRUE
          NULL
        }, error = function(e) e)

        if (is.null(err)) break

        is401 <- grepl("401", conditionMessage(err), fixed = TRUE)
        if (.attempt < 2L && is401 && isGD) {
          messagePreProcess("Google Drive token expired (HTTP 401); refreshing and retrying...",
                            verbose = verbose)
          bearer <- .get_fresh_gd_bearer(force_refresh = TRUE)
        } else {
          stop("Non-Google Drive download failed: ", conditionMessage(err))
        }
      }

    } # else {
      # # Use httr2 for non-Google Drive downloads on Windows or if curl is unavailable
      # downloaded_bytes <- if (file.exists(local_path_expanded)) file.info(local_path_expanded)$size else 0
      #
      # # Try to get total size
      # head_resp <- try(httr::HEAD(file_name), silent = TRUE)
      # total_size <- if (inherits(head_resp, "response")) {
      #   as.numeric(httr::headers(head_resp)[["content-length"]])
      # } else {
      #   NA
      # }
      #
      # req <- httr2::request(file_name)
      #
      # if (!is.na(total_size) && total_size > downloaded_bytes) {
      #   req <- req |>
      #     httr2::req_headers(Range = paste0("bytes=", downloaded_bytes, "-"))
      # }
      #
      # req <- req |> httr2::req_progress()
      #
      # con <- file(local_path_expanded, open = if (downloaded_bytes > 0) "ab" else "wb")
      # on.exit(try(close(con), silent = TRUE), add = TRUE)
      #
      # tryCatch({
      #   resp <- httr2::req_perform(req)
      #   body <- httr2::resp_body_raw(resp)
      #   writeBin(body, con)
      #   completed <- TRUE
      #   # message("Non-Google Drive download completed using httr2.")
      # }, error = function(e) {
      #   stop("Download failed: ", e$message)
      # })
    # }
  }

  if (isTRUE(completed)) {
    messagePreProcess("Download of " , local_path, " complete",  verbose = verbose)
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

## Fetch a Google Drive bearer token.  When force_refresh = TRUE, trigger
## gargle's auto-refresh by making a lightweight drive_user() API call through
## httr — this is the safe way to refresh without calling Token2.0$refresh()
## directly (which writes to the gargle cache and can fail on read-only mounts).
.get_fresh_gd_bearer <- function(force_refresh = FALSE) {
  if (!googledrive::drive_has_token())
    stop("no googledrive token discovered; run drive_auth() to authenticate.")
  if (force_refresh)
    tryCatch(googledrive::drive_user(), error = function(e) NULL)
  googledrive::drive_token()$auth_token$credentials$access_token
}

googledriveIDtoHumanURL <- function(id) {
  paste0("https://drive.google.com/file/d/", id)
}


getRemoteFileSize <- function(isGD, url) {
  if (isGD) {
    file <- googledrive::drive_get(url)
    file_id <- file$id
    download_url <- googledriveIDtoDownloadURL(file_id)
    total_size <- as.numeric(file$drive_resource[[1]]$size)
  } else {
    download_url <- url
    head_resp <- httr::HEAD(download_url)
    total_size <- as.numeric(httr::headers(head_resp)[["content-length"]])
  }
  total_size
}
