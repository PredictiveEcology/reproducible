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
#'   \code{"raster::raster"}.
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
#' @param useCache Passed to Cache in various places. Defaults to \code{getOption("reproducible.useCache")}
#'
#' @author Eliot McIntire, Jean Marchal, and Tati Micheletti
#' @export
#' @importFrom data.table data.table
#' @importFrom digest digest
#' @importFrom methods is
#' @importFrom rlang quo
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
#' # Using dlFun -- a custom download function -- passed to preProcess
#' test1 <- prepInputs(targetFile = "GADM_2.8_LUX_adm0.rds", # must specify currently
#'                     dlFun = "raster::getData", name = "GADM", country = "LUX", level = 0,
#'                     path = dPath)
#' }
#'
#'
prepInputs <- function(targetFile = NULL, url = NULL, archive = NULL, alsoExtract = NULL,
                       destinationPath = getOption("reproducible.destinationPath", "."),
                       fun = NULL,
                       quick = getOption("reproducible.quick"),
                       overwrite = getOption("reproducible.overwrite", FALSE),
                       purge = FALSE,
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
  fun <- .fnCleanup(out$fun, callingFun = "prepInputs")

  ## dots will contain too many things for some functions
  ## -- need to remove those that are known going into prepInputs
  args <- out$dots[!(names(out$dots) %in% .argsToRemove)]

  # Only accept the ones that are the formals of the function -- above removals may be redunant
  args <- args[(names(args) %in% fun$formalArgs)]
  if (length(args) == 0) args <- NULL

  # Stage 1 - load into R
  x <- if (is.null(out$object)) {
    message("Loading object into R")
    if (identical(out$fun, raster::raster) |
        identical(out$fun, raster::stack) |
        identical(out$fun, raster::brick)) {
      ## Don't cache the reading of a raster
      ## -- normal reading of raster on disk is fast b/c only reads metadata
      do.call(out$fun, append(list(asPath(out$targetFilePath)), args))
    } else {
      if (identical(out$fun, base::load)) {
        if (is.null(args$envir)) {
          message("  Running base::load, returning objects as a list. Pass envir = anEnvir ",
                  "if you would like it loaded to a specific environment")
          tmpEnv <- new.env()
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
        mess <- capture.output(type = "message", obj <- Cache(do.call, out$fun, append(list(asPath(out$targetFilePath)), args),
              useCache = useCache))
        mess <- grep("No cacheRepo supplied", mess, invert = TRUE, value = TRUE)
        if (length(mess) > 0)
          message(mess)
        obj
      }
    }
  } else {
    out$object
  }

  ## postProcess -- skip if no studyArea or rasterToMatch -- Caching could be slow otherwise
  if (!(all(is.null(out$dots$studyArea),
            is.null(out$dots$rasterToMatch),
            is.null(out$dots$targetCRS)))) {
    message("Running postProcess")

    # The do.call doesn't quote its arguments, so it doesn't work for "debugging"
    #  This rlang stuff is a way to pass through objects without evaluating them
    spatials <- sapply(out$dots, function(x) is(x, "Raster") || is(x, "Spatial") || is (x, "sf"))
    out$dots[spatials] <- lapply(out$dots[spatials], function(x) rlang::quo(x))
    x <- Cache(do.call, postProcess, append(list(x = rlang::quo(x), filename1 = out$targetFilePath,
                                                 overwrite = overwrite,
                                                 destinationPath = out$destinationPath,
                                                 useCache = useCache), # passed into postProcess
                                  out$dots),
               useCache = useCache # used here
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
#'
#' @return A character vector listing the paths of the extracted archives.
#'
#' @author Jean Marchal and Eliot McIntire
#' @importFrom tools file_ext
#'
extractFromArchive <- function(archive,
                               destinationPath = getOption("reproducible.destinationPath", dirname(archive)),
                               neededFiles = NULL, extractedArchives = NULL, checkSums = NULL,
                               needChecksums = 0, filesExtracted = character(),
                               checkSumFilePath = character(), quick = FALSE) {
  if (!is.null(archive)) {
    if (!(any(c("zip", "tar", "tar.gz", "gz", "rar") %in% file_ext(archive)))) {
      stop("Archives of type ", file_ext(archive), " are not currently supported. ",
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
                              .unzipOrUnTar(funWArgs$fun, funWArgs$args,
                                            files = basename(archive[2])))
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
            filesExtracted = filesExtracted
          )
        } else if (any(neededFiles %in% basename(filesInArchive)) || is.null(neededFiles)) {
          possibleFolders <- filesInArchive[file_ext(filesInArchive) == ""]
          if (length(possibleFolders)!=0){
            filesInArchive <- setdiff(filesInArchive, possibleFolders)
          }
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
          isArchive <- grepl(file_ext(filesInArchive), pattern = "(zip|tar|rar)", ignore.case = TRUE)

          if (any(isArchive)) {
            arch <- .basename(filesInArchive[isArchive])
            filesExtracted <- c(filesExtracted,
                                .unzipOrUnTar(funWArgs$fun, funWArgs$args, files = arch))

            prevExtract <- lapply(file.path(destinationPath, arch), function(ap)
              extractFromArchive(archive = ap, destinationPath = destinationPath,
                                 neededFiles = neededFiles,
                                 extractedArchives = extractedArchives,
                                 filesExtracted = filesExtracted,
                                 checkSums = checkSums,
                                 needChecksums = needChecksums,
                                 checkSumFilePath = checkSumFilePath,
                                 quick = quick))

            extractedArchives <- c(prevExtract[[1]]$extractedArchives, extractedArchives)
            filesExtracted <- unique(c(prevExtract[[1]]$filesExtracted, filesExtracted))
          }
        }
      } else {
        message("  Skipping extractFromArchive: all files already present")
        filesExtracted <- checkSums[checkSums$expectedFile %in%
                                      basename(filesInArchive), ]$expectedFile
      }
    }
  } else {
    if (!is.null(archive)) { # if archive is null, it means there was no archive passed
      message("  Skipping extractFromArchive: ", paste(neededFiles, collapse = ", "), " already present")
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
#' @importFrom tools file_ext
#' @importFrom utils unzip untar
#' @inheritParams postProcess
#' @param filesExtracted A character vector of all files that have been extracted (e.g.,
#'                       from an archive)
#' @param destinationPath Full path of the directory where the target file should be
#' @keywords internal
.guessAtTargetAndFun <- function(targetFilePath,
                                 destinationPath = getOption("reproducible.destinationPath", "."),
                                 filesExtracted, fun) {
  if (!is.null(fun) && !is.character(fun)) {
    stop("fun must be a character string, not the function")
  }
  possibleFiles <- unique(.basename(c(targetFilePath, filesExtracted)))
  fileExt <- file_ext(possibleFiles)
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
    message("  targetFile was not specified. ", if (any(isShapefile)) {
      c(" Trying ",fun," on ", paste(possibleFiles[isShapefile], collapse = ", "), ".",
          " If that is not correct, please specify a different targetFile",
          " and/or fun.")

    } else {
      c(" Trying ", fun,
        ". If that is not correct, please specify a targetFile",
        " and/or different fun. The current files in the targetFilePath's ",
        "directory are: \n",
        paste(possibleFiles, collapse = "\n"))
    })

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
        message("  Don't know which file to load. Please specify targetFile.")
      }
    }
    if (length(targetFilePath) > 1)  {
      message("  More than one possible files to load: ", paste(targetFilePath, collapse = ", "),
              ". Picking the last one. If not correct, specify a targetFile.")
      targetFilePath <- targetFilePath[length(targetFilePath)]
    } #else {
      #message("  Trying ", targetFilePath, " with ", fun, ".")
    #}
    targetFile <- targetFilePath
    if (!is.null(targetFile)) targetFilePath <- file.path(destinationPath, targetFile)
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
  } else if (ext == "rar") {
    fun <- "unrar"
  }
  out <- list(fun = fun, args = args)
  } else {
    out <- NULL
  }
  return(out)
}

#' @keywords internal
#' @importFrom utils capture.output
.unzipOrUnTar <- function(fun, args, files, overwrite = TRUE) {
  argList <- list(files = files)
  if (is.character(fun)) {
    message(paste0("The archive is a .rar file. preProcess will try a system call of 'unrar'."))
    hasUnrar <- .testForUnrar()
    tempDir <- file.path(args$exdir, "extractedFiles") %>%
      checkPath(create = TRUE)
    if (grepl(x = hasUnrar, pattern = "7z")) {
      system(
        paste0("\"", hasUnrar, "\"", " e -o",
               tempDir, " ",
               args[[1]]),
        wait = TRUE,
        invisible = TRUE,
        show.output.on.console = FALSE
      )
    } else {
      system(paste0("unrar x ",
                    args[[1]], " ",
                    tempDir),
             wait = TRUE, ignore.stdout = TRUE)
    }
    extractedFiles <-
      list.files(path = tempDir,
                 # list of full paths of all extracted files!
                 recursive = TRUE,
                 include.dirs = TRUE)
    internalFolders <- extractedFiles[file_ext(extractedFiles) == ""]
    extractedFiles <- setdiff(x = extractedFiles, y = internalFolders)
    if (length(extractedFiles)==0) {
      stop("preProcess could not extract the files from the archive ", basename(args[[1]]),".",
           "Please try to extract it manually to the destinationPath")
    } else {
      invisible(lapply(
        X = extractedFiles,
        FUN = function(fileToMove) {
          file.rename(from = file.path(tempDir, fileToMove),
                      to = file.path(dirname(tempDir), basename(fileToMove)))
        }
      ))
      unlink(file.path(args$exdir, "extractedFiles"), recursive = TRUE)
      extractedFiles <- basename(extractedFiles)
    }
  } else {
    isUnzip <- ("overwrite" %in% names(formals(fun)))
    argList <- if (isUnzip) {
      c(argList, overwrite = overwrite)
    } else {
      c(argList)
    }
    opt <- options("warn")$warn
    on.exit(options(warn = opt))
    options(warn = 1)
    mess <- capture.output(type = "message", extractedFiles <- do.call(fun, c(args, argList)))
    worked <- if (isUnzip) {
      all(normPath(file.path(args$exdir, basename(argList[[1]]))) %in% normPath(extractedFiles))
    } else {
      isTRUE(extractedFiles == 0)
    }
    if (!isTRUE(worked)) {
      message(
        paste0(
          "File unzipping does not appear to have worked.",
          " Trying a system call of unzip..."
        )
      )
      tempDir <- file.path(args$exdir, "extractedFiles") %>%
        checkPath(create = TRUE)
      if (file.exists(args[[1]])){
        pathToFile <-  normPath(args[[1]])
      } else {
        if (file.exists(file.path(args$exdir, args[[1]]))){
          pathToFile <-  normPath(file.path(args$exdir, args[[1]]))
        } else {
          warning(mess)
          stop("prepInputs cannot find the file ", basename(args[[1]]),
               ". The file might have been moved during unzipping or is corrupted")
        }
      }
      system2("unzip",
              args = paste0(pathToFile," -d ",tempDir),
              wait = TRUE,
              stdout = NULL)
      extractedFiles <-
        list.files(path = tempDir,
                   # list of full paths of all extracted files!
                   recursive = TRUE,
                   include.dirs = TRUE)
      invisible(lapply(
        X = extractedFiles,
        FUN = function(fileToMove) {
          file.rename(from = file.path(tempDir, fileToMove),
                      to = file.path(args$exdir, fileToMove))
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
      isArchive <- file_ext(filename) %in% c("zip", "tar", "rar")
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
appendChecksumsTable <- function(checkSumFilePath, filesToChecksum,
                                 destinationPath = getOption("reproducible.destinationPath"),
                                 append = TRUE) {
  if (append) {
    # a checksums file already existed, need to keep some of it
    cs <- suppressWarnings(try(read.table(checkSumFilePath, header = TRUE), silent = TRUE))
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
  csf <- if (append) tempfile(fileext = ".TXT") else checkSumFilePath
  capture.output(
    type = "message",
    currentFiles <- Checksums(path = destinationPath, write = TRUE, #write = !append || NROW(nonCurrentFiles) == 0,
                              files = file.path(destinationPath, filesToChecksum),
                              checksumFile = csf)
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
      message("The current targetFile is not the same as the expected targetFile in the ",
              "CHECKSUMS.txt; appending new entry in CHECKSUMS.txt. If this is not ",
              "desired, please check files for discrepancies")
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

#' List files in either a \code{.zip} or or \code{.tar} file
#'
#' Makes the outputs from\code{.tar}\code{.zip} the same, which they aren't by default.
#'
#' @param archive A character string of a single file name to list files in.
#'
#' @return A character string of all files in the archive.
#'
#' @importFrom testthat capture_warnings
#' @keywords internal
#' @rdname listFilesInArchive
.listFilesInArchive <- function(archive) {
  if (length(archive) > 0 && tools::file_ext(archive[1]) == "rar") {
    hasUnrar <- .testForUnrar()
  }
  funWArgs <- .whichExtractFn(archive[1], NULL)
  filesInArchive <- NULL
  if (!is.null(funWArgs$fun)) {
    if (file.exists(archive[1])) {
      if (!file_ext(archive[1]) == "rar") {
        filesInArchive <- funWArgs$fun(archive[1], list = TRUE)
        if ("Name" %in% names(filesInArchive)) {
          # for zips, rm directories (length = 0)
          filesInArchive <- filesInArchive[filesInArchive$Length != 0, ]$Name
        } else {
          # untar
          filesInArchive
        }
      } else {
        # On Windows
        if (grepl(x = hasUnrar, pattern = "7z")) {
          warn <- testthat::capture_warnings({
            filesOutput <- system(paste0("\"", hasUnrar, "\"", " l ", archive[1]),
                                  show.output.on.console = FALSE, intern = TRUE)
          })
        } else {
          # On Linux/MacOS
          filesOutput <- system(paste0("unrar l ", archive[1]), intern = TRUE)
        }
        if (exists("warn") && isTRUE(any(grepl("had status 2", warn))))
          stop(warn)
        if (isTRUE(any(grepl("Can not open the file as archive", filesOutput))))
          stop("rar is defective")
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
#' @rdname unrarExists
#' @name unrarExists
.unrarExists <- function() {
    hasUnrar <- ""
    hasUnrar <- Sys.which("unrar")
    if (hasUnrar == "") {
      hasUnrar <- Sys.which("7z.exe")
      if (hasUnrar == "") {
        message("prepInputs is looking for 'unrar' or '7z' in your system...")
        hasUnrar <- list.files("C:/Program Files",
                               pattern = "unrar.exe|7z.exe",
                               recursive = TRUE,
                               full.names = TRUE)
        if (hasUnrar == "" || length(hasUnrar) == 0) {
          hasUnrar <- list.files(dirname(Sys.getenv("SystemRoot")),
                                 pattern = "unrar.exe|7z.exe",
                                 recursive = TRUE,
                                 full.names = TRUE)
          if (hasUnrar == "" || length(hasUnrar) == 0) {
            hasUnrar <- NULL
          } else {
            warning("The extracting software was found in an unusual location: ", hasUnrar, ".",
                    "If you receive an error when extracting the archive, please install '7zip' or 'unrar'",
                    " in 'Program Files' folder")
          }
        }
        hasUnrar <- hasUnrar[1]
      }
    }
  return(hasUnrar)
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
#' @rdname testForUnrar
#' @name testForUnrar
.testForUnrar <- function(){
  if (!is.null(.unrarPath)) {
    hasUnrar  <- .unrarPath
  } else {
    # Find the path to unrar and assign to a package-stored object
    usrTg <- paste(sample(x = LETTERS, size = 15), collapse = "")
    hasUnrar <- Cache(.unrarExists, userTags = usrTg) # Cache for project-level persistence
    utils::assignInMyNamespace(".unrarPath", hasUnrar) # assign in namespace for package
  }
  if (is.null(hasUnrar)) {
    clearCache(userTags = usrTg, ask = FALSE)
    stop(
      "prepInputs did not find '7-Zip' nor 'unrar' installed.",
      " Please install it before running prepInputs for a '.rar' archive"
    )
  }
  return(hasUnrar)
}

#' The known path for unrar or 7z
#' @rdname unrarPath
#' @name unrarPath
.unrarPath <- NULL
