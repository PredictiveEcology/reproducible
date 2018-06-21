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
#'   spatial simulation. See \code{\link{postProcess.spatialObjects}}.
#'
#'   \emph{Understanding various combinations of \code{rasterToMatch}
#'   and/or \code{studyArea}:}
#'   Please see \code{\link{postProcess.spatialObjects}}.
#'  }
#'
#' @section \code{purge}:
#'
#' In options for control of purging the \code{CHECKSUMS.txt} file are:
#'
#'   \tabular{cl}{
#'     \code{0} \tab keep file \cr
#'     \code{1} \tab delete file \cr
#'     \code{2} \tab delete entry for \code{targetFile} \cr
#'     \code{4} \tab delete entry for \code{alsoExtract} \cr
#'     \code{3} \tab delete entry for \code{archive} \cr
#'     \code{5} \tab delete entry for \code{targetFile} & \code{alsoExtract} \cr
#'     \code{6} \tab delete entry for \code{targetFile}, \code{alsoExtract} & \code{archive} \cr
#'     \code{7} \tab delete entry that is failing (i.e., for the file downloaded by the \code{url})\cr
#'   }
#' will only remove entries in the \code{CHECKSUMS.txt} that are associated with
#'    \code{targetFile}, \code{alsoExtract} or \code{archive} When prepInputs is called, it will write or append to a (if
#'    already exists)
#'   \code{CHECKSUMS.txt} file. If the \code{CHECKSUMS.txt} is not correct, use
#'   this argument to remove it.

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
#'   \code{CHECKSUMS.txt}.
#'
#' @param alsoExtract Optional character string naming files other than
#'   \code{targetFile} that must be extracted from the \code{archive}. If
#'   \code{NULL}, the default, then it will extract all files. Other options:
#'   \code{"similiar"} will extract all files with the same filename without
#'   file extension as \code{targetFile}. \code{NA} will extract nothing other
#'   than \code{targetFile}. A character string of specific file names will cause
#'   only those to be extracted.
#'
#' @param destinationPath Character string of a directory in which to download
#'   and save the file that comes from \code{url} and is also where the function
#'   will look for \code{archive} or \code{targetFile}.
#'
#' @param fun Character string indicating the function to use to load
#'   \code{targetFile} into an \code{R} object.
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
#' @param useCache Passed to Cache in various places. Default \code{FALSE}
#'
#' @author Eliot McIntire
#' @author Jean Marchal
#' @export
#' @importFrom data.table data.table
#' @importFrom digest digest
#' @importFrom methods is
#' @importFrom R.utils isAbsolutePath isFile
#' @importFrom utils methods
#' @include checksums.R download.R postProcess.R
#' @rdname prepInputs
#' @seealso \code{\link{downloadFile}}, \code{\link{extractFromArchive}},
#'          \code{\link{downloadFile}}, \code{\link{postProcess}}.
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
#' library(sp)
#' library(raster)
#' coords <- structure(c(-122.98, -116.1, -99.2, -106, -122.98, 59.9, 65.73, 63.58, 54.79, 59.9),
#'                     .Dim = c(5L, 2L))
#' Sr1 <- Polygon(coords)
#' Srs1 <- Polygons(list(Sr1), "s1")
#' StudyArea <- SpatialPolygons(list(Srs1), 1L)
#' crs(StudyArea) <- "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
#'
#' #  specify targetFile, alsoExtract, and fun, wrap with Cache
#' ecozoneFilename <- file.path(dPath, "ecozones.shp")
#' # Note, you don't need to "alsoExtract" the archive... if the archive is not there, but the
#' #   targetFile is there, it will not redownload the archive.
#' ecozoneFiles <- c("ecozones.dbf", "ecozones.prj",
#'                   "ecozones.sbn", "ecozones.sbx", "ecozones.shp", "ecozones.shx")
#' shpEcozoneSm <- Cache(prepInputs,
#'                          url = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip",
#'                          targetFile = reproducible::asPath(ecozoneFilename),
#'                          alsoExtract = reproducible::asPath(ecozoneFiles),
#'                          studyArea = StudyArea,
#'                          fun = "shapefile", destinationPath = dPath,
#'                          filename2 = "EcozoneFile.shp") # passed to determineFilename
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
#'                      destinationPath = asPath(dPath),
#'                      studyArea = StudyArea)
#'
#' plot(LCC2005)
#'
#' # if wrapped with Cache, will be fast second time, very fast 3rd time (via memoised copy)
#' LCC2005 <- Cache(prepInputs, url = url,
#'                      targetFile = lcc2005Filename,
#'                      archive = asPath("LandCoverOfCanada2005_V1_4.zip"),
#'                      destinationPath = asPath(dPath),
#'                      studyArea = StudyArea)
#'
#' }
#'
prepInputs <- function(targetFile = NULL, url = NULL, archive = NULL, alsoExtract = NULL,
                       destinationPath = ".", fun = NULL,
                       quick = getOption("reproducible.quick"),
                       overwrite = FALSE, purge = FALSE,
                       useCache = getOption("reproducible.useCache", FALSE), ...) {

  # Download, Checksum, Extract from Archive
  message("Running preProcess")
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
    ...
  )

  # Load object to R
  ## dots will contain too many things for some functions
  ## -- need to remove those that are known going into prepInputs
  argsToRemove <- unique(c(names(formals(prepInputs)),
                           names(formals(fixErrors)),
                           names(formals(writeRaster)),
                           names(formals(projectRaster)),
                           names(formals(determineFilename)),
                           names(formals(writeOutputs)),
                           unlist(lapply(methods("postProcess"), function(x) names(formals(x))))))
  args <- out$dots[!(names(out$dots) %in% argsToRemove)]
  if (length(args) == 0) args <- NULL

  # Stage 1 - load into R
  message("Loading object into R from disk")
  if (out$tryRasterFn) {
    ## Don't cache the reading of a raster
    ## -- normal reading of raster on disk is fast b/c only reads metadata
    x <- do.call(out$fun, append(list(asPath(out$targetFilePath)), args))
  } else {
    x <- Cache(do.call, out$fun, append(list(asPath(out$targetFilePath)), args))
  }

  ## postProcess -- skip if no studyArea or rasterToMatch -- Caching could be slow otherwise
  if (!(all(is.null(out$dots$studyArea), is.null(out$dots$rasterToMatch)))) {
    message("Running postProcess")
    x <- Cache(do.call, postProcess, append(list(useCache = useCache, x = x, filename1 = out$targetFilePath,
                                                 destinationPath = out$destinationPath), out$dots))
  }

  return(x)
}

#' Extract files from archive
#'
#' Extract zip or tar archive files, possibly nested in other zip or tar archives.
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
#' @param checkSums A checksums file, e.g., created by Checksums(..., write = TRUE)
#' @param needChecksums A numeric, with \code{0} indicating do not write a new checksums,
#'                      \code{1} write a new one,
#'                      \code{2} append new information to existing one.
#' @param ... Passed to \code{unzip} or \code{untar}, e.g., \code{overwrite}
#'
#' @return A character vector listing the paths of the extracted archives.
#'
#' @author Jean Marchal & Eliot McIntire
#' @importFrom tools file_ext
#'
extractFromArchive <- function(archive, destinationPath = dirname(archive),
                               neededFiles, extractedArchives = NULL, checkSums,
                               needChecksums, filesExtracted = character()) {


  if (!is.null(archive)) {
    if (!(any(c("zip", "tar", "tar.gz", "gz") %in% file_ext(archive)))) {
      stop("Archives of type ", file_ext(archive), " are not currently supported. ",
           "Try extracting manually then placing extracted files in ", destinationPath)
    }
  }
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

      filesInArchive <- .listFilesInArchive(archive)
      # filesInArchive <- funWArgs$fun(archive[1], list = TRUE)
      #
      # if ("Name" %in% names(filesInArchive)) {
      #   # for zips, rm directories (length = 0)
      #   filesInArchive <- filesInArchive[filesInArchive$Length != 0, ]$Name
      # }

      # recheck, now that we have the whole file liast
      if (is.null(neededFiles)) {
        result <- checkSums[checkSums$expectedFile %in% basename(filesInArchive), ]$result
      }
      if (!(all(compareNA(result, "OK")) && all(neededFiles %in% checkSums$expectedFile)) ||
          NROW(result) == 0) {
        # don't extract if we already have all files and they are fine

        # use binary addition -- 1 is new file, 2 is append
        if (needChecksums == 0) needChecksums <- 2

        if (length(archive) > 1) {
          filesExtracted <- c(filesExtracted,
                              .unzipOrUnTar(funWArgs$fun, funWArgs$args,
                                            files = basename(archive[2])))
          # recursion, removing one archive
          extractedObjs <- extractFromArchive(archive[-1],
                                              destinationPath = destinationPath,
                                              neededFiles = neededFiles,
                                              extractedArchives = extractedArchives,
                                              checkSums,
                                              needChecksums,
                                              filesExtracted = filesExtracted)
        } else if (any(neededFiles %in% basename(filesInArchive)) || is.null(neededFiles)) {
          extractingTheseFiles <- paste(basename(filesInArchive[basename(filesInArchive) %in%
                                                                  neededFiles]),
                                        collapse = ", ")
          if (!nzchar(extractingTheseFiles))
            extractingTheseFiles <- paste0("all files: ", paste(basename(filesInArchive),
                                                                collapse = ", "))
          message("From:", basename(archive[1]), "  \nExtracting\n ",
                  paste(collapse = "\n ", extractingTheseFiles))
          filesExtracted <- c(filesExtracted,
                              .unzipOrUnTar(funWArgs$fun, funWArgs$args,
                                            files = filesInArchive[basename(filesInArchive) %in%
                                                                     neededFiles]))
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
                                     neededFiles = neededFiles,
                                     extractedArchives = extractedArchives,
                                     filesExtracted = filesExtracted,
                                     checkSums, needChecksums))
              )
            )
          }
        }
      } else {
        message("  Skipping extractFromArchive: all files already present")
        filesExtracted <- checkSums[checkSums$expectedFile %in%
                                      basename(filesInArchive), ]$expectedFile
      }
    }
  } else {
    message("  Skipping extractFromArchive: ", paste(neededFiles, collapse = ", "), " already present")
    filesExtracted <- setdiff(neededFiles, if (!is.null(archive)) basename(archive))
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
#' @keywords internal
.guessAtTargetAndFun <- function(targetFilePath, destinationPath, filesExtracted, fun) {
  #if (is.null(targetFilePath)) {
  #filesExtracted <- dir(destinationPath)
  possibleFiles <- basename(unique(c(targetFilePath, filesExtracted)))
  isShapefile <- grepl("shp", file_ext(possibleFiles))
  isRaster <- file_ext(possibleFiles) %in% c("tif", "grd")
  if (is.null(fun)) { #i.e., the default
    fun <- if (any(isShapefile)) {
      "raster::shapefile"
    } else {
      "raster::raster"
    }
  }

  if (is.null(targetFilePath)) {
    message("  targetFile was not specified. ", if (any(isShapefile)) {
      c(" Trying raster::shapefile on ", paste(possibleFiles[isShapefile], collapse = ", "), ".",
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
              ". Picking the first one. If not correct, specify a targetFile.")
      targetFilePath <- targetFilePath[1]
    } else {
      message("  Trying ", targetFilePath, " with ", fun, ".")
    }
    targetFile <- targetFilePath
    targetFilePath <- file.path(destinationPath, targetFile)
  }

  list(targetFilePath = targetFilePath, fun = fun)
}

#' @importFrom utils untar unzip
.whichExtractFn <- function(archive, args) {
  if (!(is.null(archive))) {
  ext <- tolower(file_ext(archive))
  if (ext == "zip") {
    fun <- unzip
    args <- c(args, list(junkpaths = TRUE))
  } else if (ext == "tar") {
    fun <- untar
  }
  out <- list(fun = fun, args = args)
  } else {
    out <- NULL
  }
  return(out)
}

#' @keywords internal
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

#' @keywords internal
.checkSums <- function(filesToCheck, fileinfo, chksumsFilePath, quick) {
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
                         quickCheck = quick
  )
  list(moduleName = moduleName, modulePath = modulePath, checkSums = checkSums)
}

#' @keywords internal
.checkSumsMem <- memoise::memoise(.checkSums)

#' @importFrom tools file_ext
.isArchive <- function(filename) {
  if (!is.null(filename)) {
    filename <- if (length(filename)) {
      isArchive <- file_ext(filename) %in% c("zip", "tar")
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
.groupedMessage <- function(mess, omitPattern) {
  mess <- grep(mess, pattern = omitPattern,
               invert = TRUE, value = TRUE)
  if (length(mess)) message(paste(mess, collapse = "\n    "))
}

#' @keywords internal
#' @importFrom utils capture.output
#' @importFrom data.table rbindlist as.data.table
appendChecksumsTable <- function(checkSumFilePath, filesToChecksum, destinationPath,
                                 append = TRUE) {
  if (append) {
    # a checksums file already existed, need to keep some of it
    cs <- try(read.table(checkSumFilePath, header = TRUE), silent = TRUE)
    if (is(cs, "try-error")) {
      # meant that it was an empty CHECKSUMS.txt file -- rebuild it
      append <- FALSE
    } else {
      nonCurrentFiles <- cs %>%
        filter(!file %in% filesToChecksum)
    }
    messStart <- "Appending "
  } else {
    messStart <- "Writing "
  }
  message(messStart, "checksums to CHECKSUMS.txt. If you see this message repeatedly,\n",
          "  you can specify targetFile (and optionally alsoExtract) so it knows\n",
          "  what to look for.")
  capture.output(
    type = "message",
    currentFiles <- Checksums(path = destinationPath, write = TRUE,
                              files = file.path(destinationPath, filesToChecksum))
  )
  if (append) { # a checksums file already existed, need to keep some of it
    currentFilesToRbind <- data.table::as.data.table(currentFiles)
    keepCols <- c("expectedFile", "checksum.x", "algorithm.x", "filesize.x")
    currentFilesToRbind <- currentFilesToRbind[, keepCols, with = FALSE]
    data.table::setnames(currentFilesToRbind, old = keepCols, new = c("file", "checksum", "algorithm", "filesize"))
    currentFilesToRbind <- rbindlist(list(nonCurrentFiles, currentFilesToRbind), fill = TRUE)
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
  neededFiles
}

#' List files in either a zip or tar
#'
#' Makes the outputs from tar or zip the same, which they aren't by default.
#'
#' @return
#' A character string of all files in the archive.
#'
#' @param archive A character string of a single file name to list files in.
#' @keywords internal
#' @rdname listFilesInArchive
.listFilesInArchive <- function(archive) {
  funWArgs <- .whichExtractFn(archive[1], NULL)
  if (!is.null(funWArgs$fun)) {
    filesInArchive <- funWArgs$fun(archive[1], list = TRUE)
    if ("Name" %in% names(filesInArchive)) {
      # for zips, rm directories (length = 0)
      filesInArchive <-
        filesInArchive[filesInArchive$Length != 0,]$Name
    }
  } else {
    filesInArchive <- NULL
  }
  return(filesInArchive)
}
