if (getRversion() >= "3.1.0") {
  utils::globalVariables(c("expectedFile", "objName", "V1"))
}

#' Download and optionally post process files
#'
#' This function can be used to prepare R objects from remote or local data
#' sources. The object of this function is to provide a reproducible version of
#' a series of commonly used steps for getting, loading, and processing data.
#' This function has two stages: Getting data (download, extracting from archives,
#' loading into R) and postProcessing (for \code{Spatial*} and \code{Raster*}
#' objects, this is crop, reproject, mask/intersect).
#' To trigger the first stage, provide \code{url} or \code{archive}.
#' To trigger the second stage, provide \code{studyArea} or \code{rasterToMatch}.
#' See examples.
#'
#' @note This function is still experimental: use with caution.
#'
#' @section Stage 1 - Getting data:
#'
#'   \enumerate{
#'     \item Download from the web via either \code{\link[googledrive]{drive_download}},
#'     \code{\link[utils]{download.file}}, or \code{\link{downloadFromWebDB}};
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
#'     \item Determine file name \code{\link{determineFilename}} via \code{postProcessedFilename};
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
#'   spatial simulation. See \code{\link{postProcess.spatialObjects}}.
#'
#'   \emph{Understanding various combinations of \code{rasterToMatch}
#'   and/or \code{studyArea}:}
#'   Please see \code{\link{postProcess.spatialObjects}}.
#'  }
#'
#' @param targetFile Character string giving the path to the eventual file
#'   (raster, shapefile, csv, etc.) after downloading and extracting from a zip
#'   or tar archive. This is the file \emph{before} it is passed to
#'   \code{postProcess}. Currently, the internal checksumming does not checksum
#'   the file after it is \code{postProcess}ed (e.g., cropped/reprojected/masked).
#'   Using \code{Cache} around \code{prepInputs} will do a sufficient job in these cases.
#'
#' @param archive Optional character string giving the path of an archive
#'   containing \code{targetFile}, or a vector giving a set of nested archives
#'   (e.g., \code{c("xxx.tar", "inner.zip")}). If there is/are (an) inner
#'   archive(s), but they are unknown, the function will try all until it finds
#'   the \code{targetFile}
#'
#' @param url Optional character string indicating the URL to download from.
#'   Normally, if used within a module, this url should be explicitly given as
#'   sourceURL for an \code{expectsInput}. In that case, it will use the
#'   module's checksums file to confirm that the download occurred correctly. If
#'   URL is used here, an ad hoc checksums will be created in the
#'   \code{destinationPath}. This will be used in subsequent calls to
#'   \code{prepInputs}, comparing the file on hand with the ad hoc
#'   \code{checksums.txt}.
#'
#' @param alsoExtract Optional character string naming files other than
#'   \code{targetFile} that must be extracted from the \code{archive}.
#'
#' @param destinationPath Character string of a directory in which to download
#'   and save the file that comes from \code{url} and is also where the function
#'   will look for \code{archive} or \code{targetFile}.
#'
#' @param fun Character string indicating the function to use to load
#'   \code{targetFile} into an \code{R} object.
#'
#' @param quick Logical. This is passed internally to \code{\link{checksums}}
#'   and \code{\link{downloadData}} (the quickCheck argument for both), and to
#'   \code{\link{Cache}} (the quick argument). This results in faster, though
#'   less robust checking of inputs. See the respective functions.
#'
#' @param purge When prepInputs is called from outside a module, it will write a
#'   \code{CHECKSUMS.txt} file. If there is an incorrect \code{CHECKSUMS.txt},
#'   this will purge it.
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
#' @param useCache Passed to Cache in various places. Default \code{FALSE}
#'
#' @author Eliot McIntire
#' @author Jean Marchal
#' @export
#' @importFrom data.table data.table
#' @importFrom digest digest
#' @importFrom methods is
#' @importFrom reproducible Cache compareNA asPath
#' @importFrom R.utils isAbsolutePath isFile
#' @importFrom utils methods
#' @importFrom googledrive drive_get drive_auth drive_download as_id
#' @rdname prepInputs
#' @seealso \code{\link{downloadFile}}, \code{\link{extractFromArchive}},
#'          \code{\link{downloadFile}}, \code{\link{postProcess}}.
#' @examples
#' # This function works within a module, however, currently,
#' #   \cde{sourceURL} is not yet working as desired. Use \code{url}.
#' \dontrun{
#' # Put chunks like this in your .inputObjects
#' if (!suppliedElsewhere("test", sim))
#'   sim$test <- Cache(prepInputs, "raster.tif", "downloadedArchive.zip",
#'                     destinationPath = dataPath(sim), studyArea = sim$studyArea,
#'                     rasterToMatch = sim$otherRasterTemplate, overwrite = TRUE)
#'
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
#'                      url = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip")
#' unlink(dPath, recursive = TRUE)
#'
#' # Once this is done, can be more precise in operational code:
#' #  specify targetFile, alsoExtract, and fun, wrap with Cache
#' ecozoneFilename <- file.path(dPath, "ecozones.shp")
#' ecozoneFiles <- c("ecozones.dbf", "ecozones.prj",
#'                   "ecozones.sbn", "ecozones.sbx", "ecozones.shp", "ecozones.shx")
#' shpEcozone <- prepInputs(targetFile = ecozoneFilename,
#'                     url = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip",
#'                     alsoExtract = ecozoneFiles,
#'                     fun = "shapefile", destinationPath = dPath)
#' unlink(dPath, recursive = TRUE)
#'
#' #' # Add a study area to Crop and Mask to
#' # Create a "study area"
#' StudyArea <- randomPolygon(x = sp::SpatialPoints(matrix(c(-110, 60), ncol=2)), 1e8)
#'
#' #  specify targetFile, alsoExtract, and fun, wrap with Cache
#' ecozoneFilename <- file.path(dPath, "ecozones.shp")
#' # Note, you don't need to "alsoExtract" the archive... if the archive is not there, but the
#' #   targetFile is there, it will not redownload the archive.
#' ecozoneFiles <- c("ecozones.dbf", "ecozones.prj",
#'                   "ecozones.sbn", "ecozones.sbx", "ecozones.shp", "ecozones.shx")
#' library(reproducible)
#' shpEcozoneSm <- Cache(prepInputs,
#'                          url = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip",
#'                          targetFile = reproducible::asPath(ecozoneFilename),
#'                          alsoExtract = reproducible::asPath(ecozoneFiles),
#'                          studyArea = StudyArea,
#'                          fun = "shapefile", destinationPath = dPath,
#'                          postProcessedFilename = "EcozoneFile.shp") # passed to determineFilename
#'
#' library(quickPlot)
#' dev();
#' Plot(shpEcozone)
#' Plot(shpEcozoneSm, addTo = "shpEcozone", gp = gpar(col = "red"))
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
#'                      destinationPath = asPath(dPath),
#'                      studyArea = StudyArea)
#'
#' Plot(LCC2005)
#'
#' # if wrapped with Cache, will be very fast second time
#' LCC2005 <- Cache(prepInputs, url = url,
#'                      targetFile = lcc2005Filename,
#'                      archive = asPath("LandCoverOfCanada2005_V1_4.zip"),
#'                      destinationPath = asPath(dPath),
#'                      studyArea = StudyArea)
#' }
#'
prepInputs <- function(targetFile, url = NULL, archive = NULL, alsoExtract = NULL,
                       destinationPath = ".", fun = NULL,
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
  if (!is.null(dots$writeCropped))  {
    message("writeCropped is being deprecated;",
            " use postProcessedFilename, used in determineFilename.")
    dots$postProcessedFilename <- dots$writeCropped
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

  if (!missing(targetFile)) {
    targetFile <- basename(targetFile)
    targetFilePath <- file.path(destinationPath, targetFile)
  } else {
    targetFile <- NULL
    targetFilePath <- NULL
  }

  if (!is.null(alsoExtract)) {
    alsoExtract <- basename(alsoExtract)
    alsoExtract <- file.path(destinationPath, alsoExtract)
  }

  checkSumFilePath <- file.path(destinationPath, "CHECKSUMS.txt")
  if (purge) unlink(checkSumFilePath)

  if (!dir.exists(destinationPath)) {
    if (isFile(destinationPath)) {
      stop("destinationPath must be a directory")
    }
    checkPath(destinationPath, create = TRUE)
  }
  message("Preparing: ", targetFile)

  emptyChecksums <- data.table(expectedFile = character(), result = character())
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

  # If quick, then use file.info as part of cache/memoise ... otherwise,
  #   pass a random real number to make a new memoise
  moduleName <- NULL
  modulePath <- NULL
  if (is.null(url)) { # the only way for this to be useful is if there is a SpaDES module
    # and url can be gotten during downloadData from module metadata
    fileinfo <- if (quick) {
      file.info(filesToCheck)
    } else {
      runif(1)
    }
    if (file.exists(checkSumFilePath)) {
      if (!grepl("data", basename(dirname(checkSumFilePath)))) {
        stop("You appear to be using prepInputs inside a module,",
             " but are not using the 'data/' subdirectory.\n",
             "Currently, this does not work.",
             " Please specify a url if you would like to do this, or use the 'data/' subdirectory.")
      }

      out <- .checkSumsMem(asPath(filesToCheck), fileinfo,
                           asPath(checkSumFilePath), quick = quick)
      moduleName <- out$moduleName
      modulePath <- out$modulePath

      checkSums <- out$checkSums
    } else {
      checkSums <- out <- emptyChecksums
    }

  } else {
    checkSums <- try(checksums(path = destinationPath, write = FALSE)#, checksumFile = checkSumFilePath)
                     , silent = TRUE)
    if (is(checkSums, "try-error")) {
      needChecksums <- 1
      checkSums <- emptyChecksums
    }
  }

  neededFiles <- c(targetFile, if (!is.null(alsoExtract)) basename(alsoExtract))

  # Stage 1 -- Download
  downloadFileResult <- downloadFile(archive, targetFile, neededFiles = neededFiles,
                                     destinationPath, quick, checkSums, url, needChecksums = needChecksums,
                                     overwrite = overwrite, moduleName = moduleName, modulePath = modulePath)
  needChecksums <- downloadFileResult$needChecksums
  neededFiles <- downloadFileResult$neededFiles
  if (is.null(archive)) archive <- downloadFileResult$archive

  filesToChecksum <- if (is.null(archive)) character() else basename(archive)
  on.exit({
    if (needChecksums > 0) {
      appendChecksumsTable(checkSumFilePath = checkSumFilePath, filesToChecksum = filesToChecksum,
                           destinationPath = destinationPath, append = needChecksums == 2)

      # if (needChecksums == 2) { # a checksums file already existed, need to keep some of it
      #   cs <- try(read.table(checkSumFilePath, header = TRUE), silent = TRUE)
      #   if (is(cs, "try-error")) { # meant that it was an empty CHECKSUMS.txt file -- rebuild it
      #     needChecksums <- 1
      #   } else {
      #     nonCurrentFiles <- cs %>%
      #       filter(!file %in% filesToChecksum)
      #   }
      # }
      # currentFiles <- checksums(path = destinationPath, write = TRUE, #checksumFile = checkSumFilePath,
      #                           files = file.path(destinationPath, filesToChecksum))
      # if (needChecksums == 2) { # a checksums file already existed, need to keep some of it
      #   currentFiles <- rbind(nonCurrentFiles, currentFiles)
      #   writeChecksumsTable(currentFiles, checkSumFilePath, dots = list())
      # }
    }
  })

  # Stage 1 - Extract from archive

  filesExtracted <- extractFromArchive(archive = archive, destinationPath = destinationPath,
                                         neededFiles = neededFiles,
                                         checkSums = checkSums, needChecksums = needChecksums)

  filesToChecksum <- unique(c(filesToChecksum, targetFile, alsoExtract,
                              basename(filesExtracted$filesExtracted)))
  needChecksums <- filesExtracted$needChecksums


  #targetFilePath might still be NULL, need destinationPath too
  targetParams <- .guessAtTargetAndFun(targetFilePath, destinationPath,
                                       filesExtracted$filesExtracted,
                                       fun) # passes through if all known
  targetFile <- basename(targetParams$targetFilePath)
  targetFilePath <- targetParams$targetFilePath
  fun <- targetParams$fun

  # Now that all files are downloaded and extracted from archive, deal with missing targetFilePath
  tryRasterFn <- if (endsWith(suffix = "raster", fun)) TRUE else FALSE

  # fun is a charcter string, convert to function
  if (grepl("::", fun)) {
    fun2 <- strsplit(fun, "::")[[1]]
    pkg <- fun2[1]
    fun <- fun2[2]
    fun <- getFromNamespace(fun, pkg)
  } else {
    fun <- get(fun)
  }

  # dots will contain too many things for some functions -- need to remove those that are known going
  #   into prepInputs
  argsToRemove <- unique(c(names(formals(prepInputs)),
                           names(formals(fixErrors)),
                           names(formals(writeRaster)),
                           names(formals(projectRaster)),
                           names(formals(determineFilename)),
                           names(formals(writeOutputs)),
                           unlist(lapply(methods("postProcess"), function(x) names(formals(x))))))
  args <- dots[!(names(dots) %in% argsToRemove)]
  if (length(args) == 0) args <- NULL

  # Stage 1 - load into R
  if (tryRasterFn) {
    # Don't cache the reading of a raster -- normal reading of raster on disk is fast b/c only reads metadata
    x <- do.call(fun, append(list(asPath(targetFilePath)), args))
  } else {
    x <- Cache(do.call, fun, append(list(asPath(targetFilePath)), args))
  }

  # postProcess
  out <-  Cache(postProcess, useCache = useCache, x,
                inputFilePath = targetFilePath, destinationPath = destinationPath,
                ...)
  return(out)
}


#' Do some minor error fixing
#'
#' These must be very common for this function to be useful. Currently, the only
#' meaningful method is on SpatialPolygons, and it runs \code{rgeos::gIsValid}. If
#' \code{FALSE}, then it runs a buffer of width 0.
#' @inheritParams prepInputs
#' @param x Any object that could be fixed for errors.
#'          See \code{\link{fixErrors.SpatialPolygons}}
#' @export
#' @keywords internal
#' @param ... None used currently
#' @param objectName Optional. This is only for messaging; if provided, then messages relayed
#'                   to user will mention this.
#' @param attemptErrorFixes Will attempt to fix known errors. Currently only some failures
#'        for SpatialPolygons* are attempted. Notably with \code{raster::buffer(..., width = 0)}.
#'        Default \code{TRUE}, though this may not be the right action for all cases.
#' @param useCache Logical, default \code{getOption("reproducible.useCache", FALSE)}, whether
#'                 Cache is used on the internal \code{raster::buffer} command.
#'  @examples
fixErrors <- function(x, objectName, attemptErrorFixes = TRUE,
                      useCache = getOption("reproducible.useCache", FALSE), ...) {
  UseMethod("fixErrors")
}

#' @export
#' @keywords internal
fixErrors.default <- function(x, objectName, attemptErrorFixes = TRUE,
                              useCache = getOption("reproducible.useCache", FALSE), ...) {
  x
}

#' Fix \code{rgeos::gIsValid} failures in \code{SpatialPolygons}
#'
#' This uses \code{raster::buffer(..., width = 0)} internally, which fixes some
#' failures to \code{rgeos::gIsValid}
#'
#' @export
#' @param x A \code{SpatialPolygons} object
#' @inheritParams fixErrors
fixErrors.SpatialPolygons <- function(x, objectName = NULL,
                                      attemptErrorFixes = TRUE,
                                      useCache = getOption("reproducible.useCache", FALSE), ...) {
  if (attemptErrorFixes) {
    if (is.null(objectName)) objectName = "SpatialPolygon"
    if (is(x, "SpatialPolygons")) {
      message("Checking for errors in ", objectName)
      if (suppressWarnings(any(!rgeos::gIsValid(x, byid = TRUE)))) {
        message("Found errors in ", objectName, ". Attempting to correct.")
        x1 <- try(Cache(raster::buffer, x, width = 0, dissolve = FALSE, useCache = useCache))
        if (is(x1, "try-error")) {
          message("There are errors with ", objectName,
                  ". Couldn't fix them with raster::buffer(..., width = 0)")
        } else {
          x <- x1
          message("  Some or all of the errors fixed.")
        }

      } else {
        message("  Found no errors.")
      }
    }
  }
  return(x)
}

#' Extract files from archive.
#'
#' Extract zip or tar archive files, possibly nested in other zip or tar
#' archives.
#'
#' @param archive Character string giving the path of the archive
#' containing the \code{file} to be extracted.
#'
#' @param destinationPath Character string giving the path where \code{neededFiles} will be
#' extracted. Defaults to the archive directory.
#'
#' @param neededFiles Character string giving the name of the file(s) to be extracted.
#'
#' @param extractedArchives Used internally to track archives that have been extracted from.
#' @param filesExtracted Used internally to track files that have been extracted.
#' @param checkSums A checksums file, e.g., created by checksums(..., write = TRUE)
#' @param needChecksums A numeric, with \code{0} indicating do not write a new checksums,
#'                      \code{1} write a new one,
#'                      \code{2} append new information to existing one.
#' @param ... Passed to \code{unzip} or \code{untar}, e.g., \code{overwrite}
#'
#' @return A character vector listing the paths of the extracted archives.
#'
#' @author Jean Marchal
#' @author Eliot McIntire
#' @importFrom reproducible Cache compareNA
#' @importFrom tools file_ext
#'
extractFromArchive <- function(archive, destinationPath = dirname(archive),
                               neededFiles, extractedArchives = NULL, checkSums, needChecksums,
                               filesExtracted = character()) {

  result <- if (!is.null(neededFiles)) {
    checkSums[checkSums$expectedFile %in% basename(neededFiles), ]$result
  } else {
    "NotOK"
  }
  extractedObjs <- list(filesExtraced = character())
  # needs to pass checkSums & have all neededFiles files
  if (!(all(compareNA(result, "OK")) && all(neededFiles %in% checkSums$expectedFile))) {
    if (!is.null(archive)) {
      args <- list(archive[1], exdir = destinationPath[1])

      funWArgs <- .whichExtractFn(archive[1], args)

      filesInArchive <- funWArgs$fun(archive[1], list = TRUE)

      if ("Name" %in% names(filesInArchive)) {
        filesInArchive <- filesInArchive[filesInArchive$Length != 0,]$Name # for zips, rm directories (length = 0)
      }

      # recheck, now that we have the whole file liast
      if (is.null(neededFiles)) {
        result <- checkSums[checkSums$expectedFile %in% basename(filesInArchive), ]$result
      }
      if (!(all(compareNA(result, "OK")) && all(neededFiles %in% checkSums$expectedFile)) ||
          NROW(result) == 0) { # don't extract if we already have all files and they are fine
        if (needChecksums == 0) needChecksums <- 2 # use binary addition -- 1 is new file, 2 is append

        if (length(archive) > 1) {
          filesExtracted <- c(filesExtracted,
                              .unzipOrUnTar(funWArgs$fun, funWArgs$args, files = basename(archive[2])))
          # recursion, removing one archive
          extractedObjs <- extractFromArchive(archive[-1], destinationPath = destinationPath,
                                              neededFiles = neededFiles, extractedArchives = extractedArchives,
                                              checkSums, needChecksums, filesExtracted = filesExtracted)
        } else if (any(neededFiles %in% basename(filesInArchive)) || is.null(neededFiles)) {
          extractingTheseFiles <- paste(basename(filesInArchive[basename(filesInArchive) %in% neededFiles]),
                                        collapse = ", ")
          if (!nzchar(extractingTheseFiles))
            extractingTheseFiles <- paste0("all files: ", paste(basename(filesInArchive), collapse = ", "))
          message("From:", basename(archive[1]), "  Extracting ", extractingTheseFiles)
          filesExtracted <- c(filesExtracted,
                              .unzipOrUnTar(funWArgs$fun, funWArgs$args,
                                            files = filesInArchive[basename(filesInArchive) %in% neededFiles]))
        } else {
          # don't have a 2nd archive, and don't have our neededFiles file
          isArchive <- grepl(file_ext(filesInArchive), pattern = "(zip|tar)", ignore.case = TRUE)

          if (any(isArchive)) {
            arch <- filesInArchive[isArchive]
            filesExtracted <- c(filesExtracted,
                                .unzipOrUnTar(funWArgs$fun, funWArgs$args, files = arch))

            # lapply(file.path(destinationPath, arch), function(archi)
            #   extractFromArchive(archi, destinationPath, neededFiles, extractedArchives))

            extractedArchives <- c(
              extractedArchives,
              unlist(
                lapply(file.path(destinationPath, arch), function(ap)
                  extractFromArchive(archive = ap, destinationPath = destinationPath,
                                     neededFiles = neededFiles, extractedArchives = extractedArchives,
                                     filesExtracted = filesExtracted,
                                     checkSums, needChecksums))
              )
            )
          }
        }
      } else {
        message("  Skipping extractFromArchive: all files already extracted.")
        filesExtracted <- checkSums[checkSums$expectedFile %in% basename(filesInArchive), ]$expectedFile
      }
    }
  } else {
    message("  Skipping extractFromArchive: targetFile (and any alsoExtract) already extracted.")
  }
  list(extractedArchives = c(extractedArchives, archive),
       filesExtracted = unique(c(filesExtracted, extractedObjs$filesExtracted)),
       needChecksums = needChecksums)
}

#' Try to pick a file to load
#'
#' @keywords internal
#' @rdname guessAtTarget
#' @name guessAtTarget
#' @importFrom tools file_ext
#' @inheritParams postProcess
#' @param filesExtracted A character vector of all files that have been extracted (e.g.,
#'                       from an archive)
#' @param destinationPath Full path of the directory where the target file should be
.guessAtTargetAndFun <- function(targetFilePath, destinationPath, filesExtracted, fun) {
  #if (is.null(targetFilePath)) {
  #filesExtracted <- dir(destinationPath)
  possibleFiles <- basename(unique(c(targetFilePath, filesExtracted)))
  isShapefile <- grepl("shp", file_ext(possibleFiles))
  isRaster <- file_ext(possibleFiles) %in% c("tif", "grd")
  if (is.null(fun)) { #i.e., the default
    fun <-if (any(isShapefile)) {
      "raster::shapefile"
    } else {
      "raster::raster"
    }
  }

  if (is.null(targetFilePath)) {
    message("  targetFile was not specified. ", if (any(isShapefile)) {
      c(" Trying raster::shapefile on ", possibleFiles[isShapefile], ".",
        " If that is not correct, please specify different targetFile",
        " and/or fun.")
    } else {
      c(" Trying ", fun,
        ". If that is not correct, please specify a targetFile",
        " and/or different fun. The current files in the targetFilePath's ",
        "directory are: \n",
        paste(possibleFiles, collapse = "\n"))
    })

    targetFilePath <- if (endsWith(suffix = "shapefile", fun )) {
      possibleFiles[isShapefile]
    } else {
      if (any(isRaster)) {
        possibleFiles[isRaster]
      } else {
        message("  Don't know which file to load. Please specify targetFile.")
      }

    }
    if (length(targetFilePath) > 1)  {
      message("  More than one possible files to load, ", paste(targetFilePath, collapse = ", "),
              " Picking the first one. If not correct, specify a targetFile.")
      targetFilePath <- targetFilePath[1]
    } else {
      message("  Trying ", targetFilePath, " with ", fun, ".")
    }
    targetFile <- targetFilePath
    targetFilePath <- file.path(destinationPath, targetFile)
  }

  list(targetFilePath = targetFilePath, fun = fun)
}

#' Generic function to post process objects
#'
#' There may be many methods developed. See e.g.,
#' \code{\link{postProcess.spatialObjects}}
#' @export
#' @keywords internal
#' @param x  An object of postProcessing. See individual methods.
#' @importClassesFrom quickPlot spatialObjects
#' @seealso \code{prepInputs}, \code{\link{postProcess.spatialObjects}}
#' @param targetFilePath Full path of the target file
#' @param ... Passed to internal functions. None implemented for the generic.
#' @inheritParams prepInputs
#'
postProcess <- function(x, ...) {
  UseMethod("postProcess")
}

#' @export
#' @keywords internal
postProcess.default <- function(x, ...) {
  x
}

#' Post processing for \code{spatialObjects}
#'
#' The method for spatialObjects (\code{Raster*} and \code{Spatial*}) will
#' crop, reproject, and mask, in that order.  This function is a wrapper for
#' \code{\link{cropInputs}}, \code{\link{maskInputs}} and
#' \code{\link{writeOutputs}}, with a decent amount of data manipulating
#' between these calls so that the crs match.
#'
#'
#' @section Post processing sequence:
#'
#'   If the \code{rasterToMatch} or \code{studyArea} are passed, then
#'   the following sequence will occur:
#'
#'   \enumerate{
#'     \item Fix errors. Currently only errors fixed are for \code{SpatialPolygons} using
#'            \code{buffer(..., width = 0)}.
#'     \item Crop using \code{\link{cropInputs}}
#'     \item Project using \code{\link{projectInputs}} \item Mask using \code{\link{maskInputs}}
#'     \item Determine file name \code{\link{determineFilename}} via \code{postProcessedFilename}
#'     \item Write that file name to disk, optionally \code{\link{writeOutputs}}
#'   }
#'
#'   NOTE: checksumming does not occur during the post-processing stage, as
#'   there are no file downloads. To achieve fast results, wrap
#'   \code{prepInputs} with \code{Cache}
#'
#'   NOTE: \code{sf} objects are still very experimental.
#'
#'  \subsection{Understanding various combinations of \code{rasterToMatch}
#'   and/or \code{studyArea}}{ Please see \code{\link{postProcess.spatialObjects}}
#'  }
#'
#' @inheritParams prepInputs
#'
#' @inheritParams cropInputs
#'
#' @param x A \code{Spatial*}, \code{sf} or \code{Raster*} object.
#'
#' @param postProcessedFilename Character string. If provided, then it is passed to
#'                 \code{determineFilename} and then \code{writeOutputs}.
#'
#' @param inputFilePath Character string giving the file path of the \emph{input} object,
#'                      if it has one. This is then used if \code{postProcessedFilename}
#'                      is \code{TRUE} to name the output file, where the resulting
#'                      post-processed filename will be
#'                      \code{.prefix(basename(inputFilePath), "Small")}.
#'                      Mostly used by \code{\link{prepInputs}},
#'                      where \code{postProcessedFilename} is missing.
#'
#' @param useSAcrs Logical. If \code{FALSE}, the default, then the desired projection
#'                 will be taken from \code{rasterToMatch} or none at all.
#'                 If \code{TRUE}, it will be taken from \code{studyArea}.
#'
#' @param ... Additonal arguments passed to \code{\link{cropInputs}},
#'            \code{\link{projectInputs}}, \code{\link{maskInputs}},
#'            \code{\link{determineFilename}}, and \code{\link{writeOutputs}}.
#'            These then pass \code{...} into other functions, like
#'            \code{\link[raster]{writeRaster}}, or \code{sf::st_write}.
#'            This might include potentially important arguments like \code{datatype},
#'            \code{format}. Also passed to \code{projectRaster},
#'            with likely important arguments such as \code{method = "bilinear"}.
#'
#' @export
#'
#' @section Passing \code{rasterToMatch} and/or \code{studyArea}:
#'
#' Depending on which of these were passed, different things will happen to the \code{targetFile}
#' located at \code{inputFilePath}.
#'
#' \subsection{If \code{targetFile} is a \code{Raster*} object:}{
#'   \tabular{lccc}{
#'                       \tab \code{rasterToMatch} \tab \code{studyArea} \tab             Both \cr
#'     \code{extent}     \tab Yes                  \tab   Yes        \tab \code{rasterToMatch} \cr
#'     \code{resolution} \tab Yes                  \tab   No         \tab \code{rasterToMatch} \cr
#'     \code{projection} \tab Yes                  \tab   No*        \tab \code{rasterToMatch}*\cr
#'     \code{alignment}  \tab Yes                  \tab   No         \tab \code{rasterToMatch} \cr
#'     \code{mask}       \tab No**                 \tab   Yes        \tab \code{studyArea}**   \cr
#'   }
#'   * Can be overridden with \code{useSAcrs}
#'   ** Will mask with \code{NA}s from \code{rasterToMatch} if \code{maskWithRTM}
#' }
#'
#' \subsection{If \code{targetFile} is a \code{Spatial*} object:}{
#'   \tabular{lccc}{
#'                       \tab \code{rasterToMatch} \tab \code{studyArea} \tab             Both \cr
#'     \code{extent}     \tab Yes                  \tab   Yes        \tab \code{rasterToMatch} \cr
#'     \code{resolution} \tab NA                   \tab   NA         \tab NA                   \cr
#'     \code{projection} \tab Yes                  \tab   No*        \tab \code{rasterToMatch}*\cr
#'     \code{alignment}  \tab NA                   \tab   NA         \tab NA                   \cr
#'     \code{mask}       \tab No                   \tab   Yes        \tab \code{studyArea}     \cr
#'   }
#'   * Can be overridden with \code{useSAcrs}
#' }
postProcess.spatialObjects <- function(x, inputFilePath = NULL,
                                       studyArea = NULL, rasterToMatch = NULL,
                                       overwrite = TRUE, useSAcrs = FALSE,
                                       useCache = getOption("reproducible.useCache", FALSE),
                                       postProcessedFilename = NULL,
                                       ...) {

  # Test if user supplied wrong type of file for "studyArea", "rasterToMatch"
  if (!is.null(studyArea) & !is(studyArea, "Spatial")) {
    stop("The 'studyArea' provided is not a Spatial* object.")
  }

  if (!is.null(rasterToMatch) & !is(rasterToMatch, "RasterLayer")) {
    stop("The 'rasterToMatch' provided is not a Raster* object.")
  }

  dots <- list(...)

  if (!is.null(dots$targetFilePath))  {
    message("targetFilePath is being deprecated; use inputFilePath.")
    inputFilePath <- dots$targetFilePath
    dots$targetFilePath <- NULL
  }

  if (!is.null(studyArea) || !is.null(rasterToMatch)) {

    # fix errors if methods available
    if (identical(useCache, FALSE)) {
      message("useCache is FALSE, skipping Cache during post-processing.")
    }
    skipCacheMess <- "useCache is FALSE, skipping Cache"
    skipCacheMess2 <- "No cacheRepo supplied"

    # cropInputs -- pass the extent and crs so Caching is faster than whole Raster
    if (!is.null(rasterToMatch)) {
      extRTM <- extent(rasterToMatch)
      crsRTM <- crs(rasterToMatch)
    } else {
      extRTM <- NULL
      crsRTM <- NULL
    }

    mess <- capture.output(type = "message",
                           x <- Cache(cropInputs, x, studyArea = studyArea,
                                      extentToMatch = extRTM,
                                      extentCRS = crsRTM,
                                      useCache = useCache, ...))

    .groupedMessage(mess, omitPattern = paste(skipCacheMess, skipCacheMess2, sep = "|"))

    # cropInputs may have returned NULL if they don't overlap
    if (!is.null(x)) {
      objectName <- if (is.null(inputFilePath)) NULL else basename(inputFilePath)
      mess <- capture.output(type = "message", # no Cache at the method level because may be just passed through if raster
                             x <- fixErrors(x, objectName = objectName,
                                            useCache = useCache, ...))
      .groupedMessage(mess, omitPattern = skipCacheMess)

      # projectInputs
      targetCRS <- getTargetCRS(useSAcrs, studyArea, rasterToMatch)

      mess <- capture.output(type = "message",
                             x <- Cache(projectInputs, x, targetCRS = targetCRS,
                                        rasterToMatch = rasterToMatch, useCache = useCache, ...))

      .groupedMessage(mess, omitPattern = paste(skipCacheMess, skipCacheMess2, sep = "|"))

      # maskInputs
      mess <- capture.output(type = "message",
                             x <- Cache(maskInputs, x, studyArea = studyArea,
                                        rasterToMatch = rasterToMatch, useCache = useCache, ...))

      .groupedMessage(mess, omitPattern = paste(skipCacheMess, skipCacheMess2, sep = "|"))

      # filename
      if (is.null(postProcessedFilename)) {
        postProcessedFilename <- TRUE
      }
      newFilename <- determineFilename(inputFilePath = inputFilePath,
                                       postProcessedFilename = postProcessedFilename,
                                       ...)
      if (!is.null(list(...)$filename)) stop("Can't pass filename; use postProcessedFilename")

      # writeOutputs
      x <- writeOutputs(x = x, filename = newFilename, overwrite = overwrite, ... )
    }
  }
  return(x)
}

#' Reproject, crop a \code{Spatial*} or \code{Raster*} object
#'
#' This function can be used to crop or reproject module inputs from raw data.
#'
#' @param x A \code{Spatial*}, \code{sf}, or \code{Raster*} object.
#'
#' @param studyArea Template \code{SpatialPolygons*} object used for masking, after cropping.
#'                  If not in same CRS, then it will be \code{spTransform}ed to
#'                  CRS of \code{x} before masking. Currently, this function will not reproject the
#'                  \code{x}. \code{\link{postProcess.spatialObjects}}
#'
#' @param rasterToMatch Template \code{Raster*} object used for cropping (so extent should be
#'                      the extent of desired outcome), reprojecting (including changing the
#'                      resolution and projection).
#'                      See details in \code{\link{postProcess.spatialObjects}}.
#' @param ... Passed to \code{projectRaster} and \code{Cache}
#' cropping.
#'
#' @author Eliot McIntire
#' @author Jean Marchal
#' @export
#' @importFrom methods is
#' @importFrom raster buffer crop crs extent projectRaster res crs<-
#' @importFrom rgeos gIsValid
#' @importFrom reproducible Cache
#' @importFrom sp SpatialPolygonsDataFrame spTransform CRS
#' @rdname cropInputs
cropInputs <- function(x, studyArea, rasterToMatch, ...) {
  UseMethod("cropInputs")
}

#' @export
#' @rdname cropInputs
cropInputs.default <- function(x, studyArea, rasterToMatch, ...) {
  x
}

#' @export
#' @rdname cropInputs
#' @importFrom raster projectExtent
#' @param extentToMatch Optional. Can pass an extent here and a \code{crs} to
#'                      \code{extentCRS} instead of \code{rasterToMatch}. These
#'                      will override \code{rasterToMatch}, with a warning if both
#'                      passed.
#' @param extentCRS     Optional. Can pass a \code{crs} here with an extent to
#'                      \code{extentTomatch} instead of \code{rasterToMatch}
cropInputs.spatialObjects <- function(x, studyArea, rasterToMatch = NULL, extentToMatch = NULL,
                                      extentCRS = NULL, ...) {


  if (!is.null(studyArea) ||
      !is.null(rasterToMatch) || !is.null(extentToMatch)) {
    rasterToMatch <- if (!is.null(extentToMatch)) {
      raster(extentToMatch, crs = extentCRS)
    }
    cropTo <-
      if (!is.null(rasterToMatch)) {
        rasterToMatch
      } else {
        studyArea
      }

    # have to project the extent to the x projection so crop will work -- this is temporary
    #   once cropped, then cropExtent should be rm

    cropExtent <- if (identical(crs(x), crs(cropTo))) {
      extent(cropTo)
    } else {
      if (!is.null(rasterToMatch)) {
        projectExtent(cropTo, crs(x))
      } else {
        if (is(studyArea, "Spatial")) {
          spTransform(x = cropTo, CRSobj = crs(x))
        } else {
          NULL
        }
      }
    }

    if (!is.null(cropExtent)) {
      # crop it
      if (!identical(cropExtent, extent(x))) {
        message("    cropping ...")
        x <- raster::crop(x = x, y = cropExtent)
        if (is.null(x)) {
          message("    polygons do not intersect.")
        }
      }
    }
  }
  return(x)
}

#' Project \code{Raster*} or {Spatial*} or \code{sf} objects
#'
#' A simple wrapper around the various different tools for these GIS types.
#'
#' @export
#' @param x A \code{Raster*}, \code{Spatial*} or \code{sf} object
#' @param targetCRS The CRS of x at the end  of this function (i.e., the goal)
#' @param ... Passed into \code{\link[raster]{projectRaster}},
#'            \code{\link[sp]{spTransform}} or \code{\link[sf]{st_transform}}
#'
#' @return
#' A file of the same type as starting, but with projection (and possibly other
#' characteristics, including resolution, origin, extent if changed.
projectInputs <- function(x, targetCRS, ...) {
  UseMethod("projectInputs")
}

#' @export
projectInputs.Raster <- function(x, targetCRS = NULL, rasterToMatch = NULL, ...) {

  if (!is.null(rasterToMatch)) {
    if (!is.null(targetCRS)) {
      if (!identical(crs(x), targetCRS) |
          !identical(res(x), res(rasterToMatch)) |
          !identical(extent(x), extent(rasterToMatch))) {
        message("    reprojecting ...")
        x <- projectRaster(from = x, to = rasterToMatch, ...)
      } else {
        message("    no reprojecting because target characteristics same as input Raster.")
      }
    } else {
      message("    no reprojecting because no rasterToMatch & useSAcrs is FALSE.")
    }
  } else {
    message("    no reprojecting because no rasterToMatch.")
  }
  x
}

#' @export
projectInputs.sf <- function(x, targetCRS, ...) {
  warning("sf class objects not fully implemented. Use with projectInputs.sf caution.")
  if (requireNamespace("sf")) {
    if (any(sf::st_is(x, c("POLYGON", "MULTIPOLYGON"))) && !any(isValid <- sf::st_is_valid(x))) {
      x[!isValid] <- sf::st_buffer(x[!isValid], dist = 0, ...)
    }

    x <- sf::st_transform(x = x, crs = sf::st_crs(targetCRS@projargs), ...)

  } else {
    stop("Please install sf package: https://github.com/r-spatial/sf")
  }
}

#' @export
projectInputs.Spatial <- function(x, targetCRS, ...) {
  if (!is.null(targetCRS)) {
    x <- spTransform(x = x, CRSobj = targetCRS)
  }
  x
}

#' Hierachically get crs from \code{Raster*}, \code{Spatial*}
#'
#' This is the function that follows the table of order of
#' preference for determining CRS. See \code{\link{postProcess.spatialObjects}}
#' @inheritParams postProcess.spatialObjects
#' @export
getTargetCRS <- function(useSAcrs, studyArea, rasterToMatch) {
  targetCRS <- if (useSAcrs) {
    crs(studyArea)
  } else if (!is.null(rasterToMatch)) {
    crs(rasterToMatch)
  } else {
    NULL # don't reproject a Raster if only has studyArea -- too lossy
  }
  targetCRS
}

#' Mask module inputs
#'
#' This function can be used to mask module inputs from raw data.
#'
#' @param x          A \code{Raster*} object
#'
#' @param studyArea  A \code{SpatialPolygons*} object
#' @param ... Arguments passed to methods
#'
#'
#' @author Eliot McIntire
#' @author Jean Marchal
#' @export
#' @inheritParams cropInputs
#' @importFrom reproducible Cache
#' @rdname maskInputs
#'
maskInputs <- function(x, studyArea, ...) {
  UseMethod("maskInputs")
}

#' @export
#' @param maskWithRTM Logical. If \code{TRUE}, then the default,
#' @rdname maskInputs
maskInputs.Raster <- function(x, studyArea, rasterToMatch, maskWithRTM = FALSE, ...) {

  message("    masking...")
  if (isTRUE(maskWithRTM)) {
    x[is.na(rasterToMatch)] <- NA
  } else {
    if (!is.null(studyArea)) {
      msg <- capture.output(type = "message",
                            x <- fastMask(x = x, y = studyArea))
      message(paste0("      ", paste(msg, collapse = "\n      ")))
    } else {
      message("studyArea not provided, skipping masking.")
    }
  }
  return(x)
}

#' @export
#' @rdname maskInputs
maskInputs.Spatial <- function(x, studyArea, ...) {

  if (!is.null(studyArea)) {
    message("    intersecting ...")
    studyArea <- raster::aggregate(studyArea, dissolve = TRUE)
    studyArea <- spTransform(studyArea, CRSobj = crs(x))
    suppressWarnings(studyArea <- fixErrors(studyArea, "studyArea"))
    x <- tryCatch(raster::intersect(x, studyArea), error = function(e) {
          warning("  Could not mask with studyArea, for unknown reasons.",
                  " Returning object without masking.")
          return(x)
        }
      )
    return(x)
  } else {
    message("studyArea not provided, skipping masking.")
    return(x)
  }
}

#' Determine filename, either automatically or manually
#'
#' If \code{postProcessedFilename} is \code{logical}, then the cropped/masked
#' raster will be written to disk with the original \code{targetFile} name, with
#' \code{"Small"} prefixed to the basename(\code{targetFilename}).
#' If a character string, it will be the path of the saved raster.
#' It will be tested whether it is an absolute or relative path and used as is
#' if absolute or prepended with \code{destinationPath} if relative.
#'
#' @inheritParams postProcess.spatialObjects
#'
#' @param postProcessedFilename Logical or character string (a file path). See details.
#'
#' @param inputFilePath Optional. Filename (with or without full path). Only used if
#'                       \code{postProcessedFilename} is \code{TRUE}, in which case,
#'                       this is used to help name the output.
#'
#' @param destinationPath Optional. If \code{postProcessedFilename} is a relative file path, then this
#'                        will be the directory of the resulting absolute file path.
#'
#' @include helpers.R
#'
#' @details
#'  If \code{postProcessedFilename} is \code{logical}, then the output
#'  filename will be \code{"Small"} prefixed to the basename(\code{inputFilePath}).
#'  If a character string, it
#'  will be the path returned. It will be tested whether it is an
#'  absolute or relative path and used as is if absolute or prepended with
#'  \code{destinationPath} if provided, and if \code{postProcessedFilename} is relative.
#'
determineFilename <- function(postProcessedFilename = TRUE, inputFilePath = NULL,
                              destinationPath = NULL, ...) {

  dots <- list(...)
  if (!is.null(dots$targetFilePath))  {
    message("targetFilePath is being deprecated from determineFilename:\n",
            "  use postProcessedFilename and inputFilePath.")
    if (is.null(inputFilePath)) {
      inputFilePath <- dots$targetFilePath
      dots$targetFilePath <- NULL
    }
  }

  if (!(is.logical(postProcessedFilename) || is.character(postProcessedFilename))) {
    stop("postProcessedFilename must be logical or character string")
  }

  newFilename <- if (!identical(postProcessedFilename, FALSE)) { # allow TRUE or path
    if (isTRUE(postProcessedFilename) ) {
      .prefix(inputFilePath, "Small")
    } else {
      if (isAbsolutePath(postProcessedFilename)) {
        postProcessedFilename
      } else {
        if (!is.null(destinationPath)) {
          file.path(destinationPath, basename(postProcessedFilename))
        } else {
          postProcessedFilename # accept relative
        }
      }
    }
  } else {
    NULL
  }
  newFilename
}

#' Write module inputs on disk
#'
#' Can be used to write prepared inputs on disk.
#'
#' @inheritParams postProcess
#' @param filename The filename to save the output object to disk (a \code{Raster*} or
#'                 \code{Spatial*} object)
#' @param overwrite Logical. Should file being written overwrite an existing file if it
#'                  exists.
#' @param ... Passed to \code{\link[raster]{writeRaster}}, such as \code{datatype},
#'            and \code{\link[raster]{shapefile}}
#'
#' @author Eliot McIntire
#' @author Jean Marchal
#' @export
#' @importFrom methods is
#' @importFrom raster shapefile writeRaster
#' @importFrom reproducible Cache
#' @rdname writeOutputs
#'
writeOutputs <- function(x, filename, overwrite, ...) {
  UseMethod("writeOutputs")
}

writeOutputs.Raster <- function(x, filename, overwrite = FALSE, ...) {
  if (!is.null(filename)) {
    xTmp <- writeRaster(x = x, filename = filename, overwrite = overwrite, ...)

    # This is a bug in writeRaster was spotted with crs of xTmp became
    # +proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs
    # should have stayed at
    # +proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0
    if (!identical(crs(xTmp), crs(x)))
      crs(xTmp) <- crs(x)

    x <- xTmp
  }
  x
}

writeOutputs.Spatial <- function(x, filename, overwrite = FALSE, ...) {
  if (!is.null(filename)) {
    shapefile(x = x, filename = filename, overwrite = overwrite)
  }
  x
}

writeOutputs.sf <- function(x, filename, overwrite = FALSE, ...) {
  if (!is.null(filename)) {
    if (requireNamespace("sf")) {
      x <- sf::st_write(obj = x, delete_dsn = TRUE, dsn = filename, delete_dsn = overwrite)
    } else {
      stop("Please install sf package: https://github.com/r-spatial/sf")
    }
  }
  x
}

writeOutputs.default <- function(x, filename, ...) {
  stop("Don't know how to write object of class ", class(x), " on disk.")
}


#' @importFrom utils untar unzip
.whichExtractFn <- function(archive, args) {
  ext <- tolower(file_ext(archive))
  if (ext == "zip") {
    fun <- unzip
    args <- c(args, list(junkpaths = TRUE))
  } else if (ext == "tar") {
    fun <- untar
  }
  return(list(fun = fun, args = args))
}

.unzipOrUnTar <- function(fun, args, files, overwrite = TRUE) {
  argList <- list(files = files)
  isUnzip <- ("overwrite" %in% names(formals(fun)))
  argList <- if (isUnzip) {
    c(argList, overwrite = overwrite)
  } else {
    c(argList)
  }
  extractedFiles <- do.call(fun, c(args, argList))
  if (!isUnzip) {
    extractedFiles <- files
  }
  extractedFiles
}

.checkSums <- function(filesToCheck, fileinfo, chksumsFilePath, quick) {
  if (missing(chksumsFilePath)) {
    chksumsFilePath <- file.path(dirname(filesToCheck), "CHECKSUMS.txt")
  }
  moduleName <- basename(dirname(dirname(chksumsFilePath)))
  modulePath <- dirname(dirname(dirname(chksumsFilePath)))
  checkSums <- checksums(files = filesToCheck,
                         module = moduleName,
                         path = modulePath,
                         checksumFile = asPath(chksumsFilePath),
                         write = FALSE,
                         quickCheck = quick
  )
  list(moduleName = moduleName, modulePath = modulePath, checkSums = checkSums)
}

.checkSumsMem <- memoise::memoise(.checkSums)

.isArchive <- function(filename) {
  archive = if (file_ext(filename) %in% c("zip", "tar")) {
    filename
  } else {
    NULL
  }

}

.groupedMessage <- function(mess, omitPattern) {
  mess <- grep(mess, pattern = omitPattern,
               invert = TRUE, value = TRUE)
  if (length(mess)) message(paste(mess, collapse = "\n    "))
}
