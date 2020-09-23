if (getRversion() >= "3.1.0") {
  utils::globalVariables(c("expectedFile", "objName", "V1"))
}

#' Download and optionally post-process files
#'
#' \if{html}{\figure{lifecycle-maturing.svg}{options: alt="maturing"}}
#'
#' This function can be used to prepare R objects from remote or local data sources.
#' The object of this function is to provide a reproducible version of
#' a series of commonly used steps for getting, loading, and processing data.
#' This function has two stages: Getting data (download, extracting from archives,
#' loading into R) and post-processing (for \code{Spatial*} and \code{Raster*}
#' objects, this is crop, reproject, mask/intersect).
#' To trigger the first stage, provide \code{url} or \code{archive}.
#' To trigger the second stage, provide \code{studyArea} or \code{rasterToMatch}.
#' See examples.
#'
#' @note This function is still experimental: use with caution.
#'
#' @section Stage 1 - Getting data:
#'
#' See \code{\link{preProcess}} for combinations of arguments.
#'
#'   \enumerate{
#'     \item Download from the web via either \code{\link[googledrive]{drive_download}},
#'     \code{\link[utils]{download.file}};
#'     \item Extract from archive using \code{\link{unzip}} or \code{\link{untar}};
#'     \item Load into R using \code{\link[raster]{raster}},
#'     \code{\link[raster]{shapefile}}, or any other function passed in with \code{fun};
#'     \item Checksumming of all files during this process. This is put into a
#'     \file{CHECKSUMS.txt} file in the \code{destinationPath}, appending if it is
#'     already there, overwriting the entries for same files if entries already exist.
#'  }
#'
#' @section Stage 2 - Post processing:
#'
#'   This will be triggered if either \code{rasterToMatch} or \code{studyArea}
#'   is supplied.
#'
#'   \enumerate{
#'     \item Fix errors. Currently only errors fixed are for \code{SpatialPolygons}
#'     using \code{buffer(..., width = 0)};
#'     \item Crop using \code{\link{cropInputs}};
#'     \item Project using \code{\link{projectInputs}};
#'     \item Mask using \code{\link{maskInputs}};
#'     \item Determine file name \code{\link{determineFilename}} via \code{filename2};
#'     \item Optionally, write that file name to disk via \code{\link{writeOutputs}}.
#'    }
#'
#'   NOTE: checksumming does not occur during the post-processing stage, as
#'   there are no file downloads. To achieve fast results, wrap
#'   \code{prepInputs} with \code{Cache}.
#'
#'   NOTE: \code{sf} objects are still very experimental.
#'
#' \subsection{postProcessing of \code{Raster*} and \code{Spatial*} objects:}{
#'
#'   If \code{rasterToMatch} or \code{studyArea} are used, then this will
#'   trigger several subsequent functions, specifically the sequence,
#'   \emph{Crop, reproject, mask}, which appears to be a common sequence in
#'   spatial simulation. See \code{\link{postProcess.spatialClasses}}.
#'
#'   \emph{Understanding various combinations of \code{rasterToMatch}
#'   and/or \code{studyArea}:}
#'   Please see \code{\link{postProcess.spatialClasses}}.
#'  }
#'
#' @section \code{purge}:
#'
#' In options for control of purging the \code{CHECKSUMS.txt} file are:
#'
#'   \describe{
#'     \item{\code{0}}{keep file}
#'     \item{\code{1}}{delete file}
#'     \item{\code{2}}{delete entry for \code{targetFile}}
#'     \item{\code{4}}{delete entry for \code{alsoExtract}}
#'     \item{\code{3}}{delete entry for \code{archive}}
#'     \item{\code{5}}{delete entry for \code{targetFile} & \code{alsoExtract}}
#'     \item{\code{6}}{delete entry for \code{targetFile}, \code{alsoExtract} & \code{archive}}
#'     \item{\code{7}}{delete entry that is failing (i.e., for the file downloaded by the \code{url})}
#'   }
#' will only remove entries in the \code{CHECKSUMS.txt} that are associated with
#'   \code{targetFile}, \code{alsoExtract} or \code{archive} When \code{prepInputs} is called,
#'   it will write or append to a (if already exists) \code{CHECKSUMS.txt} file.
#'   If the \code{CHECKSUMS.txt} is not correct, use this argument to remove it.
#'
#' @param targetFile Character string giving the path to the eventual file
#'   (raster, shapefile, csv, etc.) after downloading and extracting from a zip
#'   or tar archive. This is the file \emph{before} it is passed to
#'   \code{postProcess}. Currently, the internal checksumming does not checksum
#'   the file after it is \code{postProcess}ed (e.g., cropped/reprojected/masked).
#'   Using \code{Cache} around \code{prepInputs} will do a sufficient job in these cases.
#'   See table in \code{\link{preProcess}}.
#'
#' @param archive Optional character string giving the path of an archive
#'   containing \code{targetFile}, or a vector giving a set of nested archives
#'   (e.g., \code{c("xxx.tar", "inner.zip", "inner.rar")}). If there is/are (an) inner
#'   archive(s), but they are unknown, the function will try all until it finds
#'   the \code{targetFile}. See table in \code{\link{preProcess}}.
#'
#' @param url Optional character string indicating the URL to download from.
#'   If not specified, then no download will be attempted. If not entry
#'   exists in the \code{CHECKSUMS.txt} (in \code{destinationPath}), an entry
#'   will be created or appended to. This \code{CHECKSUMS.txt} entry will be used
#'   in subsequent calls to
#'   \code{prepInputs} or \code{preProcess}, comparing the file on hand with the ad hoc
#'   \code{CHECKSUMS.txt}. See table in \code{\link{preProcess}}.
#'
#' @param alsoExtract Optional character string naming files other than
#'   \code{targetFile} that must be extracted from the \code{archive}. If
#'   \code{NULL}, the default, then it will extract all files. Other options:
#'   \code{"similar"} will extract all files with the same filename without
#'   file extension as \code{targetFile}. \code{NA} will extract nothing other
#'   than \code{targetFile}. A character string of specific file names will cause
#'   only those to be extracted. See table in \code{\link{preProcess}}.
#'
#' @param destinationPath Character string of a directory in which to download
#'   and save the file that comes from \code{url} and is also where the function
#'   will look for \code{archive} or \code{targetFile}. NOTE (still experimental):
#'   To prevent repeated downloads in different locations, the user can also set
#'   \code{options("reproducible.inputPaths")} to one or more local file paths to
#'   search for the file before attempting to download. Default for that option is
#'   \code{NULL} meaning do not search locally.
#'
#' @param fun Function or character string indicating the function to use to load
#'   \code{targetFile} into an \code{R} object, e.g., in form with package name:
#'   \code{"raster::raster"}. NOTE: passing \code{NULL} will skip loading object
#'   into R.
#'
#' @param quick Logical. This is passed internally to \code{\link{Checksums}}
#'   (the quickCheck argument), and to
#'   \code{\link{Cache}} (the quick argument). This results in faster, though
#'   less robust checking of inputs. See the respective functions.
#'
#' @param purge Logical or Integer. \code{0/FALSE} (default) keeps existing
#'    \code{CHECKSUMS.txt} file and
#'    \code{prepInputs} will write or append to it. \code{1/TRUE} will deleted the entire
#'    \code{CHECKSUMS.txt} file. Other options, see details.
#'
#' @param overwrite Logical. Should downloading and all the other actions occur
#'   even if they pass the checksums or the files are all there.
#'
#' @param ... Additional arguments passed to \code{fun} (i.e,. user supplied),
#'   \code{\link{postProcess}} and \code{\link[reproducible]{Cache}}.
#'  Since \code{...} is passed to \code{\link{postProcess}}, these will
#'  \code{...} will also be passed into the inner
#'  functions, e.g., \code{\link{cropInputs}}. See details and examples.
#'
#' @param useCache Passed to \code{Cache} in various places.
#'   Defaults to \code{getOption("reproducible.useCache")}.
#'
#' @param .tempPath Optional temporary path for internal file intermediate steps.
#'   Will be cleared on.exit from this function.
#'
#' @inheritParams Cache
#' @author Eliot McIntire, Jean Marchal, and Tati Micheletti
#' @export
#' @importFrom data.table data.table
#' @importFrom digest digest
#' @importFrom methods is
#' @importFrom rlang quo
#' @importFrom utils methods
#' @include checksums.R download.R postProcess.R
#' @rdname prepInputs
#' @seealso \code{\link{downloadFile}}, \code{\link{extractFromArchive}},
#'          \code{\link{postProcess}}.
#' @examples
#' # This function works within a module; however, currently,
#' #   \cde{sourceURL} is not yet working as desired. Use \code{url}.
#' \dontrun{
#' # download a zip file from internet, unzip all files, load as shapefile, Cache the call
#' # First time: don't know all files - prepInputs will guess, if download file is an archive,
#' #   then extract all files, then if there is a .shp, it will load with raster::shapefile
#' dPath <- file.path(tempdir(), "ecozones")
#' shpEcozone <- prepInputs(destinationPath = dPath,
#'                          url = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip")
#'
#' # Robust to partial file deletions:
#' unlink(dir(dPath, full.names = TRUE)[1:3])
#' shpEcozone <- prepInputs(destinationPath = dPath,
#'                          url = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip")
#' unlink(dPath, recursive = TRUE)
#'
#' # Once this is done, can be more precise in operational code:
#' #  specify targetFile, alsoExtract, and fun, wrap with Cache
#' ecozoneFilename <- file.path(dPath, "ecozones.shp")
#' ecozoneFiles <- c("ecozones.dbf", "ecozones.prj",
#'                   "ecozones.sbn", "ecozones.sbx", "ecozones.shp", "ecozones.shx")
#' shpEcozone <- prepInputs(targetFile = ecozoneFilename,
#'                          url = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip",
#'                          alsoExtract = ecozoneFiles,
#'                          fun = "shapefile", destinationPath = dPath)
#' unlink(dPath, recursive = TRUE)
#'
#' #' # Add a study area to Crop and Mask to
#' # Create a "study area"
#' library(sp)
#' library(raster)
#' coords <- structure(c(-122.98, -116.1, -99.2, -106, -122.98, 59.9, 65.73, 63.58, 54.79, 59.9),
#'                     .Dim = c(5L, 2L))
#' Sr1 <- Polygon(coords)
#' Srs1 <- Polygons(list(Sr1), "s1")
#' StudyArea <- SpatialPolygons(list(Srs1), 1L)
#' crs(StudyArea) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
#'
#' #  specify targetFile, alsoExtract, and fun, wrap with Cache
#' ecozoneFilename <- file.path(dPath, "ecozones.shp")
#' # Note, you don't need to "alsoExtract" the archive... if the archive is not there, but the
#' #   targetFile is there, it will not redownload the archive.
#' ecozoneFiles <- c("ecozones.dbf", "ecozones.prj",
#'                   "ecozones.sbn", "ecozones.sbx", "ecozones.shp", "ecozones.shx")
#' shpEcozoneSm <- Cache(prepInputs,
#'                       url = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip",
#'                       targetFile = reproducible::asPath(ecozoneFilename),
#'                       alsoExtract = reproducible::asPath(ecozoneFiles),
#'                       studyArea = StudyArea,
#'                       fun = "shapefile", destinationPath = dPath,
#'                       filename2 = "EcozoneFile.shp") # passed to determineFilename
#'
#' plot(shpEcozone)
#' plot(shpEcozoneSm, add = TRUE, col = "red")
#' unlink(dPath)
#'
#' # Big Raster, with crop and mask to Study Area - no reprojecting (lossy) of raster,
#' #   but the StudyArea does get reprojected, need to use rasterToMatch
#' dPath <- file.path(tempdir(), "LCC")
#' lcc2005Filename <- file.path(dPath, "LCC2005_V1_4a.tif")
#' url <- file.path("ftp://ftp.ccrs.nrcan.gc.ca/ad/NLCCLandCover",
#'                  "LandcoverCanada2005_250m/LandCoverOfCanada2005_V1_4.zip")
#'
#' # messages received below may help for filling in more arguments in the subsequent call
#' LCC2005 <- prepInputs(url = url,
#'                       destinationPath = asPath(dPath),
#'                       studyArea = StudyArea)
#'
#' plot(LCC2005)
#'
#' # if wrapped with Cache, will be fast second time, very fast 3rd time (via memoised copy)
#' LCC2005 <- Cache(prepInputs, url = url,
#'                  targetFile = lcc2005Filename,
#'                  archive = asPath("LandCoverOfCanada2005_V1_4.zip"),
#'                  destinationPath = asPath(dPath),
#'                  studyArea = StudyArea)
#' # Using dlFun -- a custom download function -- passed to preProcess
#' test1 <- prepInputs(targetFile = "GADM_2.8_LUX_adm0.rds", # must specify currently
#'                     dlFun = "raster::getData", name = "GADM", country = "LUX", level = 0,
#'                     path = dPath)
#' }
#'
prepInputs <- function(targetFile = NULL, url = NULL, archive = NULL, alsoExtract = NULL,
                       destinationPath = getOption("reproducible.destinationPath", "."),
                       fun = NULL,
                       quick = getOption("reproducible.quick"),
                       overwrite = getOption("reproducible.overwrite", FALSE),
                       purge = FALSE,
                       useCache = getOption("reproducible.useCache", FALSE),
                       .tempPath,
                       verbose = getOption("reproducible.verbose", 1),
                       ...) {
  # Download, Checksum, Extract from Archive
  # browser(expr = exists("._prepInputs_1"))
  if (missing(.tempPath)) {
    .tempPath <- tempdir2(rndstr(1, 6))
    on.exit({unlink(.tempPath, recursive = TRUE)},
            add = TRUE)
  }
  messagePrepInputs("Running preProcess", verbose = verbose, verboseLevel = 0)
  out <- preProcess(
    targetFile = targetFile,
    url = url,
    archive = archive,
    alsoExtract = alsoExtract,
    destinationPath = destinationPath,
    fun = fun,
    quick = quick,
    overwrite = overwrite,
    purge = purge,
    useCache = useCache,
    .tempPath = .tempPath,
    verbose = verbose,
    ...
  )

  # Load object to R
  fun <- .fnCleanup(out$fun, callingFun = "prepInputs")

  ## dots will contain too many things for some functions
  ## -- need to remove those that are known going into prepInputs
  args <- out$dots[!(names(out$dots) %in% .argsToRemove)]

  # Only accept the ones that are the formals of the function -- above removals may be redunant
  args <- args[(names(args) %in% fun$formalArgs)]
  if (length(args) == 0) args <- NULL

  if (!is.null(out$fun)) {
    x <- if (is.null(out$object)) {
      messagePrepInputs("Loading object into R", verbose = verbose)
      if (identical(out$fun, raster::raster) |
          identical(out$fun, raster::stack) |
          identical(out$fun, raster::brick)) {
        ## Don't cache the reading of a raster
        ## -- normal reading of raster on disk is fast b/c only reads metadata
        do.call(out$fun, append(list(asPath(out$targetFilePath)), args))
      } else {
        if (identical(out$fun, base::load)) {
          if (is.null(args$envir)) {
            messagePrepInputs("  Running base::load, returning objects as a list. Pass envir = anEnvir ",
                    "if you would like it loaded to a specific environment", verbose = verbose)
            tmpEnv <- new.env(parent = emptyenv())
            returnAsList <- TRUE
          } else {
            tmpEnv <- args$envir
            args$envir <- NULL
            returnAsList <- FALSE
          }
          objs <- do.call(out$fun, append(list(file = out$targetFilePath, envir = tmpEnv), args))
          if (returnAsList)
            as.list(tmpEnv, all.names = TRUE)
        } else {
          # browser(expr = exists("._prepInputs_3"))
          err <- tryCatch(error = function(xx) xx,
            mess <- capture.output(
              type = "message",
              obj <- Cache(do.call, out$fun, append(list(asPath(out$targetFilePath)), args),
                           useCache = useCache)))
          # errOld <- capture_error(
          #  mess <- capture.output(
          #    type = "message",
          #    obj <- Cache(do.call, out$fun, append(list(asPath(out$targetFilePath)), args),
          #                 useCache = useCache)))
          if (is(err, "simpleError")) {
          #   if (!identical(errOld, err$message)) browser()
             stop(err$message)
          }
          # if (!is.null(errOld)) stop(errOld)

          mess <- grep("No cacheRepo supplied", mess, invert = TRUE, value = TRUE)
          if (length(mess) > 0)
            messagePrepInputs(mess, verbose = verbose)
          obj
        }
      }
    } else {
      out$object
    }
  } else {
    messagePrepInputs("No loading of object into R; fun = NULL", verbose = verbose)
  }

  ## postProcess -- skip if no studyArea or rasterToMatch -- Caching could be slow otherwise
  if (!(all(is.null(out$dots$studyArea),
            is.null(out$dots$rasterToMatch),
            is.null(out$dots$targetCRS)))) {
    messagePrepInputs("Running postProcess", verbose = verbose, verboseLevel = 0)

    # The do.call doesn't quote its arguments, so it doesn't work for "debugging"
    #  This rlang stuff is a way to pass through objects without evaluating them

    # argList <- append(list(x = x, filename1 = out$targetFilePath,
    #                        overwrite = overwrite,
    #                        destinationPath = out$destinationPath,
    #                        useCache = useCache), # passed into postProcess
    #                   out$dots)
    # rdal <- .robustDigest(argList)
    # browser(expr = exists("._prepInputs_2"))

    # make quosure out of all spatial objects and x
    spatials <- sapply(out$dots, function(x) is(x, "Raster") || is(x, "Spatial") || is(x, "sf"))
    out$dots[spatials] <- lapply(out$dots[spatials], function(x) rlang::quo(x))
    xquo <- rlang::quo(x)

    x <- Cache(
      do.call, postProcess, append(list(x = xquo, filename1 = out$targetFilePath,
                                        overwrite = overwrite,
                                        destinationPath = out$destinationPath,
                                        useCache = useCache,
                                        verbose = verbose), # passed into postProcess
                                   out$dots),
      useCache = useCache, verbose = verbose # used here
    )
  }

  return(x)
}

#' Extract files from archive
#'
#' Extract zip or tar archive files, possibly nested in other zip or tar archives.
#'
#' @param archive Character string giving the path of the archive
#' containing the \code{file} to be extracted. This path must exist or be \code{NULL}
#'
#' @param destinationPath Character string giving the path where \code{neededFiles} will be
#' extracted. Defaults to the archive directory.
#'
#' @param neededFiles Character string giving the name of the file(s) to be extracted.
#'
#' @param extractedArchives Used internally to track archives that have been extracted from.
#' @param filesExtracted Used internally to track files that have been extracted.
#' @param checkSums A checksums file, e.g., created by Checksums(..., write = TRUE)
#' @param needChecksums A numeric, with \code{0} indicating do not write a new checksums,
#'                      \code{1} write a new one,
#'                      \code{2} append new information to existing one.
#' @param checkSumFilePath The full path to the checksum.txt file
#' @param quick Passed to \code{Checksums}
#' @param ... Passed to \code{unzip} or \code{untar}, e.g., \code{overwrite}
#' @inheritParams prepInputs
#'
#' @return A character vector listing the paths of the extracted archives.
#'
#' @author Jean Marchal and Eliot McIntire
#'
extractFromArchive <- function(archive,
                               destinationPath = getOption("reproducible.destinationPath", dirname(archive)),
                               neededFiles = NULL, extractedArchives = NULL, checkSums = NULL,
                               needChecksums = 0, filesExtracted = character(),
                               checkSumFilePath = character(), quick = FALSE,
                               verbose = getOption("reproducible.verbose", 1),
                               .tempPath, ...) {

  # browser(expr = exists('._extractFromArchive_1'))
  if (!is.null(archive)) {
    if (!(any(c(knownInternalArchiveExtensions, knownSystemArchiveExtensions) %in% fileExt(archive)))) {
      stop("Archives of type ", fileExt(archive), " are not currently supported. ",
           "Try extracting manually then placing extracted files in ", destinationPath)
    }
  }
  if (!is.null(archive) && !is.null(neededFiles))
    neededFiles <- setdiff(basename(neededFiles), basename(archive))
  if (length(neededFiles) == 0) neededFiles <- NULL
  result <- if (!is.null(neededFiles)) {
    checkSums[checkSums$expectedFile %in% basename(neededFiles), ]$result
  } else {
    "NotOK"
  }
  extractedObjs <- list(filesExtracted = character())
  # needs to pass checkSums & have all neededFiles files
  hasAllFiles <- if (NROW(checkSums)) {
    all(neededFiles %in% checkSums$expectedFile)
  } else {
    FALSE
  }

  # browser(expr = exists('._extractFromArchive_2'))
  if (!(all(compareNA(result, "OK")) && hasAllFiles)) {
    if (!is.null(archive)) {
      if (!file.exists(archive[1]))
        stop("No archive exists with filename: ", archive[1],
             ". Please pass an archive name to a path that exists")
      args <- list(archive[1], exdir = destinationPath[1])
      funWArgs <- .whichExtractFn(archive[1], args)
      filesInArchive <- .listFilesInArchive(archive)
      if (is.null(neededFiles)) {
        neededFiles <- basename(filesInArchive)
        result <- checkSums[checkSums$expectedFile %in% neededFiles, ]$result
      }

      # need to re-Checksums because
      checkSums <- .checkSumsUpdate(destinationPath = destinationPath,
                                    newFilesToCheck = file.path(destinationPath, basename(neededFiles)),
                                    checkSums = checkSums,
                                    checkSumFilePath = checkSumFilePath)

      isOK <- if (!is.null(checkSums)) {
        .compareChecksumsAndFiles(checkSums, neededFiles)
      } else {
        FALSE
      }
      # recheck, now that we have the whole file liast
      if (!(all(isOK)) ||
          NROW(result) == 0) {
        # don't extract if we already have all files and they are fine

        # use binary addition -- 1 is new file, 2 is append
        if (needChecksums == 0) needChecksums <- 2
        if (length(archive) > 1) {
          filesExtracted <- c(filesExtracted,
                              .callArchiveExtractFn(funWArgs$fun, funWArgs$args,
                                            files = basename(archive[2]), .tempPath = .tempPath))
          # recursion, removing one archive
          extractedObjs <- extractFromArchive(
            archive[-1],
            destinationPath = destinationPath,
            neededFiles = neededFiles,
            extractedArchives = extractedArchives,
            checkSums = checkSums,
            quick = quick,
            needChecksums = needChecksums,
            checkSumFilePath = checkSumFilePath,
            filesExtracted = filesExtracted,
            verbose = verbose,
            .tempPath = .tempPath
          )
        } else if (any(neededFiles %in% basename(filesInArchive)) || is.null(neededFiles)) {
          possibleFolders <- filesInArchive[fileExt(filesInArchive) == ""]
          if (length(possibleFolders) != 0) {
            filesInArchive <- setdiff(filesInArchive, possibleFolders)
          }
          extractingTheseFiles <- paste(basename(filesInArchive[basename(filesInArchive) %in%
                                                                  neededFiles]), collapse = ", ")
          if (!nzchar(extractingTheseFiles))
            extractingTheseFiles <- paste0("all files: ", paste(basename(filesInArchive),
                                                                collapse = ", "))
          messagePrepInputs("From:", basename(archive[1]), "  \nExtracting\n ",
                  paste(collapse = "\n ", extractingTheseFiles), verbose = verbose)
          filesExtracted <- c(filesExtracted,
                              .callArchiveExtractFn(funWArgs$fun, funWArgs$args,
                                                    files = filesInArchive[basename(filesInArchive) %in%
                                                                             neededFiles],
                                                    .tempPath = .tempPath))
        } else {
          # don't have a 2nd archive, and don't have our neededFiles file
          #isArchive <- grepl(fileExt(filesInArchive), pattern = "(zip|tar|rar)", ignore.case = TRUE)
          isArchive <- grepl(fileExt(filesInArchive), pattern = paste0("(",paste(knownArchiveExtensions, collapse = "|"), ")"), ignore.case = TRUE)

          if (any(isArchive)) {
            arch <- .basename(filesInArchive[isArchive])
            filesExtracted <- c(filesExtracted,
                                .callArchiveExtractFn(funWArgs$fun, funWArgs$args, files = arch,
                                                      .tempPath = .tempPath))

            prevExtract <- lapply(file.path(destinationPath, arch), function(ap)
              extractFromArchive(archive = ap, destinationPath = destinationPath,
                                 neededFiles = neededFiles,
                                 extractedArchives = extractedArchives,
                                 filesExtracted = filesExtracted,
                                 checkSums = checkSums,
                                 needChecksums = needChecksums,
                                 checkSumFilePath = checkSumFilePath,
                                 quick = quick,
                                 .tempPath = .tempPath))

            extractedArchives <- c(prevExtract[[1]]$extractedArchives, extractedArchives)
            filesExtracted <- unique(c(prevExtract[[1]]$filesExtracted, filesExtracted))
          }
        }
      } else {
        messagePrepInputs("  Skipping extractFromArchive: all files already present", verbose = verbose)
        filesExtracted <- checkSums[checkSums$expectedFile %in%
                                      basename(filesInArchive), ]$expectedFile
      }
    }
  } else {
    if (!is.null(archive)) { # if archive is null, it means there was no archive passed
      messagePrepInputs("  Skipping extractFromArchive: ", paste(neededFiles, collapse = ", "), " already present", verbose = verbose)
    }
    filesExtracted <- setdiff(neededFiles, if (!is.null(archive)) basename(archive))
  }
  list(extractedArchives = c(extractedArchives, archive),
       filesExtracted = unique(c(filesExtracted, extractedObjs$filesExtracted)),
       needChecksums = needChecksums,
       checkSums = checkSums)
}

#' Try to pick a file to load
#'
#' @keywords internal
#' @rdname guessAtTarget
#' @name guessAtTarget
#' @importFrom utils unzip untar
#' @inheritParams postProcess
#' @param filesExtracted A character vector of all files that have been extracted (e.g.,
#'                       from an archive)
#' @param destinationPath Full path of the directory where the target file should be
#' @keywords internal
.guessAtTargetAndFun <- function(targetFilePath,
                                 destinationPath = getOption("reproducible.destinationPath", "."),
                                 filesExtracted, fun = NULL, verbose = getOption("reproducible.verbose", 1)) {
  if (!is.null(fun) && !is.character(fun)) {
    stop("fun must be a character string, not the function")
  }
  possibleFiles <- unique(.basename(c(targetFilePath, filesExtracted)))
  fileExt <- fileExt(possibleFiles)
  isShapefile <- grepl("shp", fileExt)
  isRaster <- fileExt %in% c("tif", "grd")
  isRDS <- fileExt %in% c("rds")
  if (is.null(fun)) { #i.e., the default
    fun <- if (any(isShapefile)) {
      "raster::shapefile"
    } else if (any(isRaster)) {
      "raster::raster"
    } else if (any(isRDS)) {
      "base::readRDS"
    } else {
      NULL
      #stop("Don't know what fun to use for loading targetFile. Please supply a 'fun'", call. = FALSE)
    }
  }
  if (is.null(targetFilePath)) {
    secondPartOfMess <- if (any(isShapefile)) {
      c(" Trying ",fun," on ", paste(possibleFiles[isShapefile], collapse = ", "), ".",
        " If that is not correct, please specify a different targetFile",
        " and/or fun.")
    } else {
      c(" Trying ", fun, ".\n",
        " If that is not correct, please specify a targetFile",
        " and/or different fun. The current files in the targetFilePath's",
        " directory are: \n",
        paste(possibleFiles, collapse = "\n"))
    }
    messagePrepInputs(c("  targetFile was not specified.", secondPartOfMess), verbose = verbose)

    targetFilePath <- if (is.null(fun)) {
      NULL
    } else if (length(possibleFiles[isShapefile]) > 0) {
      possibleFiles[isShapefile]
    } else {
      if (any(isRaster)) {
        possibleFiles[isRaster]
      } else if (any(isRDS)) {
        possibleFiles[isRDS]
      } else {
        messagePrepInputs("  Don't know which file to load. Please specify targetFile.", verbose = verbose)
      }
    }
    if (length(targetFilePath) > 1)  {
      messagePrepInputs("  More than one possible files to load: ", paste(targetFilePath, collapse = ", "),
              ". Picking the last one. If not correct, specify a targetFile.", verbose = verbose)
      targetFilePath <- targetFilePath[length(targetFilePath)]
    }
    targetFile <- targetFilePath
    if (!is.null(targetFile)) targetFilePath <- file.path(destinationPath, targetFile)
  }

  list(targetFilePath = targetFilePath, fun = fun)
}

#' @importFrom utils untar unzip
.whichExtractFn <- function(archive, args) {
  if (!(is.null(archive))) {
    ext <- tolower(fileExt(archive))
    if (!ext %in% knownArchiveExtensions)
      stop("preProcess can only deal with archives with following extensions:\n",
           paste(knownArchiveExtensions, collapse = ", "))
    if (ext == "zip") {
      fun <- unzip
      args <- c(args, list(junkpaths = TRUE))
    } else if (ext %in% c("tar", "tar.gz", "gz")) {
      fun <- untar
    } else if (ext == "rar") {
      fun <- "unrar"
    } else if (ext == "7z") {
      fun <- "7z"
    }
    out <- list(fun = fun, args = args)
  } else {
    out <- NULL
  }
  return(out)
}

#' @keywords internal
#' @importFrom utils capture.output
.callArchiveExtractFn <- function(fun, args, files, overwrite = TRUE,
                                  verbose = getOption("reproducible.verbose", 1), .tempPath) {
  argList <- list(files = files)
  if (missing(.tempPath)) {
    .tempPath <- tempdir2(rndstr(1, 6))
    on.exit({unlink(.tempPath, recursive = TRUE)},
            add = TRUE)
  }

  if (is.character(fun))
    if (!fun %in% knownSystemArchiveExtensions)
      fun <- eval(fun)

  if (is.character(fun)) {
    messagePrepInputs(paste0("The archive appears to be not a .zip. Trying a system call to ", fun), verbose = verbose)
    extractSystemCallPath <- .testForArchiveExtract()
    #tempDir <- file.path(args$exdir, "extractedFiles") %>%
    #  checkPath(create = TRUE)
    if (grepl(x = extractSystemCallPath, pattern = "7z")) {
      prependPath <- if (isWindows()) {
        paste0("\"", extractSystemCallPath, "\"")
      } else {
        extractSystemCallPath
      }
      # This spits out a message on non-Windows about arguments that are ignored
      suppressMessages({
        output <- system(paste0(prependPath, " e -aoa -o", .tempPath, " ", args[[1]]),
                         wait = TRUE,
                         ignore.stdout = FALSE,
                         ignore.stderr = FALSE,
                         invisible = TRUE,
                         show.output.on.console = FALSE, intern = TRUE)
      })
    } else {
      system(paste0("unrar x ", args[[1]], " ", .tempPath), wait = TRUE, ignore.stdout = TRUE)
    }
    # list of full paths of all extracted files!
    extractedFiles <- list.files(path = .tempPath, recursive = TRUE, include.dirs = TRUE)
    internalFolders <- extractedFiles[fileExt(extractedFiles) == ""]
    extractedFiles <- setdiff(x = extractedFiles, y = internalFolders)
    if (length(extractedFiles) == 0) {
      stop("preProcess could not extract the files from the archive ", basename(args[[1]]),".",
           "Please try to extract it manually to the destinationPath")
    } else {
      invisible(lapply(
        X = extractedFiles,
        FUN = function(fileToMove) {
          invisible(file.move(from = file.path(.tempPath, fileToMove),
                              to = file.path(args$exdir, basename(fileToMove))))
        }
      ))
      # unlink(file.path(args$exdir, "extractedFiles"), recursive = TRUE)
      extractedFiles <- basename(extractedFiles)
    }
  } else {
    # Try the direct, then indirect
    isUnzip <- if (identical(unzip, fun)) TRUE else ("overwrite" %in% names(formals(fun)))
    argList <- if (isUnzip) {
      c(argList, overwrite = overwrite)
    } else {
      c(argList)
    }
    opt <- options("warn")$warn
    on.exit(options(warn = opt), add = TRUE)
    options(warn = 1)
    tooBig <- FALSE
    worked <- FALSE
    if (isUnzip) {
      fattrs <- unzip(args[[1]], list = TRUE)
      ids <- which(fattrs[["Name"]] %in% argList$files)
      tooBig <- any(fattrs[ids, ]["Length"][[1]] >= 4294967295) ## files >= 4GB are truncated; see ?unzip
    }
    if (!tooBig) {
      mess <- capture.output({
        extractedFiles <- do.call(fun, c(args, argList))
      }, type = "message")
      worked <- if (isUnzip) {
        all(normPath(file.path(args$exdir, basename(argList[[1]]))) %in% normPath(extractedFiles))
      } else {
        isTRUE(extractedFiles == 0)
      }
    }
    if (!isTRUE(worked) | isTRUE(tooBig)) {
      messagePrepInputs("File unzipping using R does not appear to have worked.",
              " Trying a system call of unzip...", verbose = verbose)

      # tempDir <- file.path(args$exdir, "extractedFiles") %>%
      #   checkPath(create = TRUE)
      if (file.exists(args[[1]])) {
        pathToFile <-  normPath(args[[1]])
      } else {
        if (file.exists(file.path(args$exdir, args[[1]]))) {
          pathToFile <-  normPath(file.path(args$exdir, args[[1]]))
        } else {
          warning(mess)
          stop("prepInputs cannot find the file ", basename(args[[1]]),
               ". The file might have been moved during unzipping or is corrupted")
        }
      }
      system2("unzip",
              args = paste0(pathToFile," -d ", .tempPath),
              wait = TRUE,
              stdout = NULL)
      extractedFiles <- list.files(path = .tempPath,
                                   # list of full paths of all extracted files!
                                   recursive = TRUE,
                                   include.dirs = TRUE)
      invisible(lapply(
        X = extractedFiles,
        FUN = function(fileToMove) {
          invisible(file.move(from = file.path(.tempPath, fileToMove),
                              to = file.path(args$exdir, fileToMove)))
        }
      ))
      extractedFiles <- file.path(args$exdir, extractedFiles)
      unlink(file.path(args$exdir, "extractedFiles"), recursive = TRUE)
    }
    if (!isUnzip) {
      extractedFiles <- files
    }
  }
  return(extractedFiles)
}

#' @keywords internal
.checkSums <- function(filesToCheck, fileinfo, chksumsFilePath, quick,
                       verbose = getOption("reproducible.verbose", 1)) {
  if (missing(chksumsFilePath)) {
    chksumsFilePath <- file.path(dirname(filesToCheck), "CHECKSUMS.txt")
  }
  moduleName <- basename(dirname(dirname(chksumsFilePath)))
  modulePath <- dirname(dirname(dirname(chksumsFilePath)))
  checkSums <- Checksums(files = filesToCheck,
                         module = moduleName,
                         path = modulePath,
                         checksumFile = asPath(chksumsFilePath),
                         write = FALSE,
                         quickCheck = quick,
                         verbose = verbose
  )
  list(moduleName = moduleName, modulePath = modulePath, checkSums = checkSums)
}


.isArchive <- function(filename) {
  if (!is.null(filename)) {
    filename <- if (length(filename)) {
      isArchive <- fileExt(filename) %in% knownArchiveExtensions
      if (any(isArchive)) {
        filename[isArchive]
      } else {
        NULL
      }
    } else {
      NULL
    }
  }
  return(filename)
}

#' @keywords internal
.groupedMessage <- function(mess, omitPattern, verbose = getOption("reproducible.verbose", 1)) {
  mess <- grep(mess, pattern = omitPattern,
               invert = TRUE, value = TRUE)
  if (length(mess)) messagePrepInputs(paste(mess, collapse = "\n    "), verbose = verbose)
}

#' @keywords internal
#' @importFrom utils capture.output
#' @importFrom data.table rbindlist as.data.table setDT setDF
appendChecksumsTable <- function(checkSumFilePath, filesToChecksum,
                                 destinationPath = getOption("reproducible.destinationPath"),
                                 append = TRUE, verbose = getOption("reproducible.verbose", 1)) {
  if (append) {
    # a checksums file already existed, need to keep some of it
    cs <- suppressWarnings(try(read.table(checkSumFilePath, header = TRUE), silent = TRUE))
    if (is(cs, "try-error")) {
      # meant that it was an empty CHECKSUMS.txt file -- rebuild it
      append <- FALSE
    } else {
      setDT(cs)
      nonCurrentFiles <- cs[!file %in% filesToChecksum]
      # if (requireNamespace("dplyr")) {
      #   nonCurrentFiles1 <- cs %>%
      #     dplyr::filter(!file %in% filesToChecksum)
      #   # browser(expr = !identical(as.data.table(nonCurrentFiles1), nonCurrentFiles))
      #   stopifnot(identical(as.data.table(nonCurrentFiles1), nonCurrentFiles))
      # }
      setDF(cs)

    }
    messStart <- "Appending "
  } else {
    messStart <- "Writing "
  }
  messagePrepInputs(messStart, "checksums to CHECKSUMS.txt. If you see this messagePrepInputs repeatedly,\n",
          "  you can specify targetFile (and optionally alsoExtract) so it knows\n",
          "  what to look for.", verbose = verbose)
  csf <- if (append) tempfile(fileext = ".TXT") else checkSumFilePath
  capture.output(
    type = "message",
    currentFiles <- Checksums(path = destinationPath, write = TRUE, #write = !append || NROW(nonCurrentFiles) == 0,
                              files = file.path(destinationPath, filesToChecksum),
                              checksumFile = csf,
                              verbose = verbose)
  )
  if (append) { # a checksums file already existed, need to keep some of it
    currentFilesToRbind <- data.table::as.data.table(currentFiles)
    keepCols <- c("expectedFile", "checksum.x", "algorithm.x", "filesize.x")
    currentFilesToRbind <- currentFilesToRbind[, keepCols, with = FALSE]
    data.table::setnames(currentFilesToRbind, old = keepCols,
                         new = c("file", "checksum", "algorithm", "filesize"))
    currentFilesToRbind <- rbindlist(list(nonCurrentFiles, currentFilesToRbind), fill = TRUE)

    # Attempt to not change CHECKSUMS.txt file if nothing new occurred
    currentFilesToRbind <- unique(currentFilesToRbind)
    anyDuplicates <- duplicated(currentFilesToRbind)
    if (any(anyDuplicates)) {
      messagePrepInputs("The current targetFile is not the same as the expected targetFile in the ",
              "CHECKSUMS.txt; appending new entry in CHECKSUMS.txt. If this is not ",
              "desired, please check files for discrepancies", verbose = verbose)
    }

    # Sometimes a checksums file doesn't have filesize
    if (!is.null(cs$filesize)) {
      if (!is.character(cs$filesize))
        cs$filesize <- as.character(cs$filesize)
    }

    if (!identical(cs, as.data.frame(currentFilesToRbind)))
      writeChecksumsTable(as.data.frame(currentFilesToRbind), checkSumFilePath, dots = list())
  }
  return(currentFiles)
}

#' Check a neededFile for commonly needed auxiliary files
#'
#' Currently, this is only used for shapefiles.
#'
#' @param neededFiles A character string of file name(s) that will be checked. Specifically
#'        if the file extension is \code{.shp} it will output the names of files with
#'        these extensions also:
#'        c("shx", "dbf", "prj", "sbx", "sbn") files also.
#' @keywords internal
#' @rdname checkForAuxiliaryFiles
.checkForAuxiliaryFiles <- function(neededFiles) {
  if ("shp" %in% fileExt(neededFiles)) { # if user wants .shp file, needs other anciliary files
    # but not all
    shpfileBase <- gsub(".shp$", "", neededFiles[fileExt(neededFiles) %in% "shp"])
    reqdShpFiles <- paste0(shpfileBase, ".", c("shx", "dbf", "prj", "sbx", "sbn"))
    if (length(neededFiles) > 0) {
      if (identical(FALSE, (all(reqdShpFiles %in% neededFiles)))) {
        optionalShpFiles <- paste0(shpfileBase, ".", c("cpg", "shp.xml"))
        otherShpfiles <- c(reqdShpFiles, optionalShpFiles)
        neededFiles <- unique(c(neededFiles, otherShpfiles))
      }
    }

  }
  neededFiles
}

#' List files in either a \code{.zip} or or \code{.tar} file
#'
#' Makes the outputs from\code{.tar}\code{.zip} the same, which they aren't by default.
#'
#' @param archive A character string of a single file name to list files in.
#'
#' @return A character string of all files in the archive.
#'
#' @keywords internal
#' @rdname listFilesInArchive
.listFilesInArchive <- function(archive) {
  if (length(archive) > 0 && fileExt(archive[1]) %in% knownSystemArchiveExtensions) {
    extractSystemCallPath <- .testForArchiveExtract()
  }
  funWArgs <- .whichExtractFn(archive[1], NULL)
  filesInArchive <- NULL
  if (!is.null(funWArgs$fun)) {
    if (file.exists(archive[1])) {
      if (!fileExt(archive[1]) %in% knownSystemArchiveExtensions) {
        filesInArchive <- funWArgs$fun(archive[1], list = TRUE)
        if ("Name" %in% names(filesInArchive)) {
          # for zips, rm directories (length = 0)
          filesInArchive <- filesInArchive[filesInArchive$Length != 0, ]$Name
        } else {
          # untar
          filesInArchive
        }
      } else {
        if (grepl(x = extractSystemCallPath, pattern = "7z")) {
          extractSystemCall <- paste0("\"", extractSystemCallPath, "\"", " l ", archive[1])
          if (isWindows()) {
            filesOutput <- captureWarningsToAttr(
                             system(extractSystemCall, show.output.on.console = FALSE, intern = TRUE)
            )
            warn <- attr(filesOutput, "warning")
            attr(filesOutput, "warning") <- NULL
          } else {
            # On Linux/MacOS
            filesOutput <- captureWarningsToAttr(
              system(extractSystemCall, intern = TRUE, ignore.stderr = TRUE))
            warn <- attr(filesOutput, "warning")
            attr(filesOutput, "warning") <- NULL
          }

        } else {
          archiveExtractBinary <- .archiveExtractBinary()
          if (is.null(archiveExtractBinary))
            stop("unrar is not on this system; please install it")
          filesOutput <- system(paste0("unrar l ", archive[1]), intern = TRUE)
        }
        if (exists("warn", inherits = FALSE) && isTRUE(any(grepl("had status 2", warn))))
          stop(warn)
        if (isTRUE(any(grepl("(Can not open the file as archive)|(Errors: 1)", filesOutput))))
          stop("archive appears defective")
        filesInBetween <- grep(pattern = "----", filesOutput)
        filesLines <- filesOutput[(min(filesInBetween)+1):(max(filesInBetween)-1)]
        filesInArchive <- unlist(lapply(X = seq_along(filesLines), FUN = function(line){
          fullString <- unlist(strsplit(filesLines[[line]], split = " "))
          return(fullString[length(fullString)])
        })
        )
        if (length(filesInArchive)==0) {
          stop("preProcess could not find any files in the archive ", archive)
        }
      }
    }
  }
  return(filesInArchive)
}

.compareChecksumsAndFiles <- function(checkSums, files) {
  checkSumsDT <- data.table(checkSums)
  filesDT <- data.table(files = basename(files))
  isOKDT <- checkSumsDT[filesDT, on = c(expectedFile = "files")]
  isOKDT2 <- checkSumsDT[filesDT, on = c(actualFile = "files")]
  # fill in any OKs from "actualFile" intot he isOKDT
  isOKDT[compareNA(isOKDT2$result, "OK"), "result"] <- "OK"
  isOK <- compareNA(isOKDT$result, "OK")
  isOK
}

#' Tests if unrar or 7zip exist
#'
#' @return
#' unrar or 7zip path if exist, or NULL
#'
#' @author Tati Micheletti
#'
#' @keywords internal
#' @rdname archiveExtractBinary
#' @name archiveExtractBinary
.archiveExtractBinary <- function(verbose = getOption("reproducible.verbose", 1)) {
    possPrograms <- c("7z", "unrar") %>%
      lapply(., Sys.which) %>%
      unlist() %>%
      unique()
    extractSystemCallPath <- if (!all(possPrograms == "")) {
      possPrograms[nzchar(possPrograms)][1] # take first one if there are more than one
    } else {
      ""
    }
    if (!(isWindows())) {
      if (grepl("7z", extractSystemCallPath)) {
        SevenZrarExists <- system("apt -qq list p7zip-rar", intern = TRUE, ignore.stderr = TRUE) %>%
          grepl("installed", .)
        if (.isFALSE(SevenZrarExists))
          messagePrepInputs("To extract .rar files, you will need p7zip-rar, not just p7zip-full. Try: \n",
                  "--------------------------\n",
                  "apt install p7zip-rar\n",
                  "--------------------------\n", verbose = verbose
          )
      }
    }

    if (identical(extractSystemCallPath, "")) {
      if (isWindows()) {
        extractSystemCallPath <- Sys.which("7z.exe")
        if (extractSystemCallPath == "") {
          messagePrepInputs("prepInputs is looking for 'unrar' or '7z' in your system...", verbose = verbose)
          extractSystemCallPath <- list.files("C:/Program Files",
                                              pattern = "unrar.exe|7z.exe",
                                              recursive = TRUE,
                                              full.names = TRUE)
          if (extractSystemCallPath == "" || length(extractSystemCallPath) == 0) {
            extractSystemCallPath <- list.files(dirname(Sys.getenv("SystemRoot")),
                                                pattern = "unrar.exe|7z.exe",
                                                recursive = TRUE,
                                                full.names = TRUE)
            if (extractSystemCallPath == "" || length(extractSystemCallPath) == 0) {
              extractSystemCallPath <- NULL
              messagePrepInputs(missingUnrarMess, verbose = verbose)
            } else {
              messagePrepInputs("The extracting software was found in an unusual location: ",
                      extractSystemCallPath, ".",
                      "If you receive an error when extracting the archive, please install ",
                      "'7zip' or 'unrar' in 'Program Files' directory.", verbose = verbose)
            }
          }
          extractSystemCallPath <- extractSystemCallPath[1]
        }
      } else {
        messagePrepInputs(missingUnrarMess,
             "Try installing with, e.g.,: \n",
             "--------------------------\n",
             "apt install p7zip p7zip-rar p7zip-full -y\n",
             "yum install p7zip p7zip-plugins -y\n",
             "--------------------------", verbose = verbose
        )
      }
    }
    if (!exists("extractSystemCallPath", inherits = FALSE)) extractSystemCallPath <- NULL
    if (!nzchar(extractSystemCallPath)) extractSystemCallPath <- NULL

  return(extractSystemCallPath)
}

#' Returns unrar path and creates a shortcut as .unrarPath
#' Was not incorporated in previous function so it can be
#' used in the tests
#'
#' @return
#' unrar or 7zip path if exist, and assign it to .unrarPath
#' Stops and advise user to install it if unrar doesn't exist
#'
#' @author Tati Micheletti
#'
#' @keywords internal
#' @rdname testForArchiveExtract
#' @name testForArchiveExtract
.testForArchiveExtract <- function(){
  if (!is.null(.unrarPath)) {
    extractSystemCallPath  <- .unrarPath
  } else {
    # Find the path to unrar and assign to a package-stored object
    usrTg <- paste(sample(x = LETTERS, size = 15), collapse = "")
    # Cache for project-level persistence
    extractSystemCallPath <- Cache(.archiveExtractBinary, userTags = usrTg)
    utils::assignInMyNamespace(".unrarPath", extractSystemCallPath) # assign in namespace for pkg
  }
  if (is.null(extractSystemCallPath)) {
    clearCache(userTags = usrTg, ask = FALSE)
    stop(
      "prepInputs did not find '7-Zip' nor 'unrar' installed.",
      " Please install it before running prepInputs for a '.rar' archive"
    )
  }
  return(extractSystemCallPath)
}

#' The known path for unrar or 7z
#' @rdname unrarPath
#' @name unrarPath
.unrarPath <- NULL

missingUnrarMess <- "The archive is a 'rar' archive; your system does not have unrar or 7zip;\n"
proj6Warn <- "NOT UPDATED FOR PROJ"

knownInternalArchiveExtensions <- c("zip", "tar", "tar.gz", "gz")
knownSystemArchiveExtensions <- c("rar", "7z")
knownArchiveExtensions <- c(knownInternalArchiveExtensions, knownSystemArchiveExtensions)

