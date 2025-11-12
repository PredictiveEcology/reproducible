utils::globalVariables(c(
  "datatype", "expectedFile", "method", "needBuffer", "objName",
  "rasterToMatch", "studyArea", "targetCRS", "to", "touches", "useSAcrs", "V1"
))

#' Download and optionally post-process files
#'
#' \if{html}{\figure{lifecycle-maturing.svg}{options: alt="maturing"}}
#'
#' This function can be used to prepare R objects from remote or local data sources.
#' The object of this function is to provide a reproducible version of
#' a series of commonly used steps for getting, loading, and processing data.
#' This function has two stages: Getting data (download, extracting from archives,
#' loading into R) and post-processing (for `Spatial*` and `Raster*`
#' objects, this is crop, reproject, mask/intersect).
#' To trigger the first stage, provide `url` or `archive`.
#' To trigger the second stage, provide `studyArea` or `rasterToMatch`.
#' See examples.
#'
#' @note This function is still experimental: use with caution.
#'
#' @section Stage 1 - Getting data:
#'
#' See [preProcess()] for combinations of arguments.
#'
#'   \enumerate{
#'     \item Download from the web via either `googledrive::drive_download()`,
#'     [utils::download.file()];
#'     \item Extract from archive using [unzip()] or [untar()];
#'     \item Load into R using `terra::rast`,
#'     `sf::st_read`, or any other function passed in with `fun`;
#'     \item Checksumming of all files during this process. This is put into a
#'     \file{CHECKSUMS.txt} file in the `destinationPath`, appending if it is
#'     already there, overwriting the entries for same files if entries already exist.
#'  }
#'
#' @section Stage 2 - Post processing:
#'
#'   This will be triggered if either `rasterToMatch` or `studyArea`
#'   is supplied.
#'
#'   \enumerate{
#'     \item Fix errors. Currently only errors fixed are for `SpatialPolygons`
#'     using `buffer(..., width = 0)`;
#'     \item Crop using [cropTo()];
#'     \item Project using [projectTo()];
#'     \item Mask using [maskTo()];
#'     \item write the file to disk via [writeTo()].
#'    }
#'
#'   NOTE: checksumming does not occur during the post-processing stage, as
#'   there are no file downloads. To achieve fast results, wrap
#'   `prepInputs` with `Cache`.
#'
#'   NOTE: `sf` objects are still very experimental.
#'
#' \subsection{postProcessing of `Spat*`, `sf`, `Raster*` and `Spatial*` objects:}{
#'
#'   The following has been DEPRECATED because there are a sufficient number of
#'   ambiguities that this has been changed in favour of `from` and the `*to` family.
#'   See [postProcessTo()].
#'
#'   DEPRECATED: If `rasterToMatch` or `studyArea` are used, then this will
#'   trigger several subsequent functions, specifically the sequence,
#'   *Crop, reproject, mask*, which appears to be a common sequence while
#'   preparing spatial data from diverse sources.
#'   See [postProcess()] documentation section on
#'   *Backwards compatibility with `rasterToMatch` and/or `studyArea` arguments*
#'   to understand various combinations of `rasterToMatch` and/or `studyArea`.
#'  }
#'
#'
#' @section `fun`:
#'
#'  `fun` offers the ability to pass any custom function with which to load
#'  the file obtained by `preProcess` into the session. There are two cases that are
#'  dealt with: when the `preProcess` downloads a file (including via `dlFun`),
#'  `fun` must deal with a file; and, when `preProcess` creates an R object
#'  (e.g., raster::getData returns an object), `fun` must deal with an object.
#'
#'  `fun` can be supplied in three ways: a function, a character string
#'   (i.e., a function name as a string), or an expression.
#'   If a character string or function, is should have the package name e.g.,
#'   `"terra::rast"` or as an actual function, e.g., `base::readRDS`.
#'   In these cases, it will evaluate this function call while passing `targetFile`
#'   as the first argument. These will only work in the simplest of cases.
#'
#'   When more precision is required, the full call can be written and where the
#'   filename can be referred to as `targetFile` if the function
#'   is loading a file. If `preProcess` returns an object, `fun` should be set to
#'   `fun = NA`.
#'
#'   If there is a custom function call, is not in a package, `prepInputs` may not find it. In such
#'   cases, simply pass the function as a named argument (with same name as function) to `prepInputs`.
#'   See examples.
#'   NOTE: passing `fun = NA` will skip loading object into R. Note this will essentially
#'   replicate the functionality of simply calling `preProcess` directly.
#'
#' @section `purge`:
#'
#' In options for control of purging the `CHECKSUMS.txt` file are:
#'
#'   \describe{
#'     \item{`0`}{keep file}
#'     \item{`1`}{delete file in `destinationPath`, all records of downloads need to be rebuilt}
#'     \item{`2`}{delete entry with same `targetFile`}
#'     \item{`4`}{delete entry with same `alsoExtract`}
#'     \item{`3`}{delete entry with same `archive`}
#'     \item{`5`}{delete entry with same `targetFile` & `alsoExtract`}
#'     \item{`6`}{delete entry with same `targetFile`, `alsoExtract` & `archive`}
#'     \item{`7`}{delete entry that same `targetFile`, `alsoExtract` & `archive` & `url`}
#'   }
#' will only remove entries in the `CHECKSUMS.txt` that are associated with
#'   `targetFile`, `alsoExtract` or `archive` When `prepInputs` is called,
#'   it will write or append to a (if already exists) `CHECKSUMS.txt` file.
#'   If the `CHECKSUMS.txt` is not correct, use this argument to remove it.
#'
#' @param targetFile Character string giving the filename (without relative or
#'   absolute path) to the eventual file
#'   (raster, shapefile, csv, etc.) after downloading and extracting from a zip
#'   or tar archive. This is the file *before* it is passed to
#'   `postProcess`. The internal checksumming does not checksum
#'   the file after it is `postProcess`ed (e.g., cropped/reprojected/masked).
#'   Using `Cache` around `prepInputs` will do a sufficient job in these cases.
#'   See table in [preProcess()].
#'
#' @param archive Optional character string giving the path of an archive
#'   containing `targetFile`, or a vector giving a set of nested archives
#'   (e.g., `c("xxx.tar", "inner.zip", "inner.rar")`). If there is/are (an) inner
#'   archive(s), but they are unknown, the function will try all until it finds
#'   the `targetFile`. See table in [preProcess()]. If it is `NA`,
#'   then it will *not* attempt to see it as an archive, even if it has archive-like
#'   file extension (e.g., `.zip`). This may be useful when an R function
#'   is expecting an archive directly.
#'
#' @param url Optional character string indicating the URL to download from.
#'   If not specified, then no download will be attempted. If not entry
#'   exists in the `CHECKSUMS.txt` (in `destinationPath`), an entry
#'   will be created or appended to. This `CHECKSUMS.txt` entry will be used
#'   in subsequent calls to
#'   `prepInputs` or `preProcess`, comparing the file on hand with the ad hoc
#'   `CHECKSUMS.txt`. See table in [preProcess()].
#'
#' @param alsoExtract Optional character string naming files other than
#'   `targetFile` that must be extracted from the `archive`. If
#'   `NULL`, the default, then it will extract all files. Other options:
#'   `"similar"` will extract all files with the same filename without
#'   file extension as `targetFile`. `NA` will extract nothing other
#'   than `targetFile`. A character string of specific file names will cause
#'   only those to be extracted. See table in [preProcess()].
#'
#' @param destinationPath Character string of a directory in which to download
#'   and save the file that comes from `url` and is also where the function
#'   will look for `archive` or `targetFile`. NOTE (still experimental):
#'   To prevent repeated downloads in different locations, the user can also set
#'   `options("reproducible.inputPaths")` to one or more local file paths to
#'   search for the file before attempting to download. Default for that option is
#'   `NULL` meaning do not search locally.
#'
#' @param fun Optional. If specified, this will attempt to load whatever
#'   file was downloaded during `preProcess` via `dlFun`. This can be either a
#'   function (e.g., sf::st_read), character string (e.g., "base::load"),
#'   NA (for no loading, useful if `dlFun` already loaded the file) or
#'   if extra arguments are required
#'   in the function call, it must be a call naming
#'   `targetFile` (e.g., `sf::st_read(targetFile, quiet = TRUE)`)
#'   as the file path to the file to load. See details and examples below.
#'
#' @param quick Logical. This is passed internally to [Checksums()]
#'   (the quickCheck argument), and to
#'   [Cache()] (the quick argument). This results in faster, though
#'   less robust checking of inputs. See the respective functions.
#'
#' @param purge Logical or Integer. `0/FALSE` (default) keeps existing `CHECKSUMS.txt` file and
#'   `prepInputs` will write or append to it. `1/TRUE` will deleted the entire `CHECKSUMS.txt` file.
#'    Other options, see details.
#'
#' @param overwrite Logical. Should downloading and all the other actions occur
#'   even if they pass the checksums or the files are all there.
#'
#' @param ... Additional arguments passed to
#'   [postProcess()] and [reproducible::Cache()].
#'  Since `...` is passed to [postProcess()], these will
#'  `...` will also be passed into the inner
#'  functions, e.g., [cropInputs()]. Possibly useful other arguments include
#'  `dlFun` which is passed to `preProcess`. See details and examples.
#'
#' @param useCache Passed to `Cache` in various places.
#'   Defaults to `getOption("reproducible.useCache", 2L)` in `prepInputs`, and
#'   `getOption("reproducible.useCache", FALSE)` if calling any of the inner
#'   functions manually. For `prepInputs`, this mean it will use `Cache`
#'   only up to 2 nested levels, which includes `preProcess`. `postProcess` and
#'   its nested `*Input` functions (e.g., `cropInputs`, `projectInputs`,
#'   `maskInputs`) are no longer internally cached, as `terra` processing speeds
#'   mean internal caching is more time consuming. We recommend caching the full
#'   `prepInputs` call instead (e.g. `prepInputs(...) |> Cache()`).
#'
#' @param .tempPath Optional temporary path for internal file intermediate steps.
#'   Will be cleared `on.exit` from this function.
#'
#' @inheritParams Cache
#' @author Eliot McIntire, Jean Marchal, and Tati Micheletti
#' @export
#' @return
#' This is an omnibus function that will return an R object that will have resulted from
#' the running of [preProcess()] and [postProcess()] or [postProcessTo()]. Thus,
#' if it is a GIS object, it may have been cropped, reprojected, "fixed", masked, and
#' written to disk.
#'
#' @importFrom data.table data.table
#' @importFrom digest digest
#' @importFrom methods is
#' @importFrom utils methods modifyList
#' @include checksums.R download.R postProcess.R
#' @rdname prepInputs
#' @seealso [postProcessTo()], [downloadFile()], [extractFromArchive()],
#'          [postProcess()].
#' @examples
#' \donttest{
#' if (requireNamespace("terra", quietly = TRUE) &&
#'   requireNamespace("sf", quietly = TRUE)) {
#'   library(reproducible)
#'   # Make a dummy study area map -- user would supply this normally
#'   coords <- structure(c(-122.9, -116.1, -99.2, -106, -122.9, 59.9, 65.7, 63.6, 54.8, 59.9),
#'     .Dim = c(5L, 2L)
#'   )
#'   studyArea <- terra::vect(coords, "polygons")
#'   terra::crs(studyArea) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
#'   # Make dummy "large" map that must be cropped to the study area
#'   outerSA <- terra::buffer(studyArea, 50000)
#'   terra::crs(outerSA) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
#'   tf <- normPath(file.path(tempdir2(), "prepInputs2.shp"))
#'   terra::writeVector(outerSA, tf)
#'
#'   # run prepInputs -- load file, postProcess it to the studyArea
#'
#'   studyArea2 <- prepInputs(
#'     targetFile = tf, to = studyArea,
#'     fun = "terra::vect",
#'     destinationPath = tempdir2()
#'   ) |>
#'     suppressWarnings() # not relevant warning here
#'
#'   # clean up
#'   unlink("CHECKSUMS.txt")
#'
#'   ##########################################
#'   # Remote file using `url`
#'   ##########################################
#'   if (internetExists()) {
#'     data.table::setDTthreads(2)
#'     origDir <- getwd()
#'     # download a zip file from internet, unzip all files, load as shapefile, Cache the call
#'     # First time: don't know all files - prepInputs will guess, if download file is an archive,
#'     #   then extract all files, then if there is a .shp, it will load with sf::st_read
#'     dPath <- file.path(tempdir(), "ecozones")
#'     shpUrl <- "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip"
#'
#'     # Wrapped in a try because this particular url can be flaky
#'     shpEcozone <- try(prepInputs(
#'       destinationPath = dPath,
#'       url = shpUrl
#'     ))
#'     if (!is(shpEcozone, "try-error")) {
#'       # Robust to partial file deletions:
#'       unlink(dir(dPath, full.names = TRUE)[1:3])
#'       shpEcozone <- prepInputs(
#'         destinationPath = dPath,
#'         url = shpUrl
#'       )
#'       unlink(dPath, recursive = TRUE)
#'
#'       # Once this is done, can be more precise in operational code:
#'       #  specify targetFile, alsoExtract, and fun, wrap with Cache
#'       ecozoneFilename <- file.path(dPath, "ecozones.shp")
#'       ecozoneFiles <- c(
#'         "ecozones.dbf", "ecozones.prj",
#'         "ecozones.sbn", "ecozones.sbx", "ecozones.shp", "ecozones.shx"
#'       )
#'       shpEcozone <- prepInputs(
#'         targetFile = ecozoneFilename,
#'         url = shpUrl,
#'         fun = "terra::vect",
#'         alsoExtract = ecozoneFiles,
#'         destinationPath = dPath
#'       )
#'       unlink(dPath, recursive = TRUE)
#'
#'       # Add a study area to Crop and Mask to
#'       # Create a "study area"
#'       coords <- structure(c(-122.98, -116.1, -99.2, -106, -122.98, 59.9, 65.73, 63.58, 54.79, 59.9),
#'         .Dim = c(5L, 2L)
#'       )
#'       studyArea <- terra::vect(coords, "polygons")
#'       terra::crs(studyArea) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
#'
#'       #  specify targetFile, alsoExtract, and fun, wrap with Cache
#'       ecozoneFilename <- file.path(dPath, "ecozones.shp")
#'       # Note, you don't need to "alsoExtract" the archive... if the archive is not there, but the
#'       #   targetFile is there, it will not redownload the archive.
#'       ecozoneFiles <- c(
#'         "ecozones.dbf", "ecozones.prj",
#'         "ecozones.sbn", "ecozones.sbx", "ecozones.shp", "ecozones.shx"
#'       )
#'       shpEcozoneSm <- Cache(prepInputs,
#'         url = shpUrl,
#'         targetFile = reproducible::asPath(ecozoneFilename),
#'         alsoExtract = reproducible::asPath(ecozoneFiles),
#'         studyArea = studyArea,
#'         fun = "terra::vect",
#'         destinationPath = dPath,
#'         writeTo = "EcozoneFile.shp"
#'       ) # passed to determineFilename
#'
#'       terra::plot(shpEcozone[, 1])
#'       terra::plot(shpEcozoneSm[, 1], add = TRUE, col = "red")
#'       unlink(dPath)
#'     }
#'   }
#' }
#' }
#'
#' ## Using quoted dlFun and fun -- this is not intended to be run but used as a template
#' ## prepInputs(..., fun = customFun(x = targetFile), customFun = customFun)
#' ##   # or more complex
#' ##  test5 <- prepInputs(
#' ##   targetFile = targetFileLuxRDS,
#' ##   dlFun =
#' ##     getDataFn(name = "GADM", country = "LUX", level = 0) # preProcess keeps file from this!
#' ##   ,
#' ##   fun = {
#' ##     out <- readRDS(targetFile)
#' ##     sf::st_as_sf(out)}
#' ##  )
prepInputs <- function(targetFile = NULL, url = NULL, archive = NULL, alsoExtract = NULL,
                       destinationPath = getOption("reproducible.destinationPath", "."),
                       fun = NULL,
                       quick = getOption("reproducible.quick"),
                       overwrite = getOption("reproducible.overwrite", FALSE),
                       purge = FALSE,
                       useCache = getOption("reproducible.useCache", 2),
                       .tempPath,
                       verbose = getOption("reproducible.verbose", 1),
                       ...) {
  .callingEnv <- parent.frame()
  messagePreProcess("Running ", .messageFunctionFn("prepInputs"), verbose = verbose, verboseLevel = 0)
  .message$IndentUpdate()
  stStart <- Sys.time()
  if (missing(.tempPath)) {
    .tempPath <- tempdir2(rndstr(1, 6))
    on.exit(
      {
        unlink(.tempPath, recursive = TRUE)
      },
      add = TRUE
    )
  }
  funCaptured <- substitute(fun)
  prepInputsAssertions(environment())

  rpiut <- getOption("reproducible.prepInputsUrlTiles")
  runNormalPreProcess <- TRUE
  if (!(isNULLorNA(rpiut) || rpiut %in% FALSE) && (
    !is.null(list(...)$to) || !is.null(list(...)$cropTo) || !is.null(list(...)$maskTo)
  )) {
    if (!is.null(url) && isGoogleDriveDirectory(rpiut)) {
      message("Using prepInputsWithTiles because `to` is supplied and \n",
              "options(reproducible.prepInputsUrlTiles) is set to a Google Drive folder")
      x <- prepInputsWithTiles(url = url, destinationPath = destinationPath, purge = purge,
                               ...)
    }
    if (!identical(x, "NULL")) runNormalPreProcess <- FALSE

  }

  if (runNormalPreProcess) {

    ##################################################################
    # preProcess
    ##################################################################
    out <- preProcess(
      targetFile = targetFile,
      url = url,
      archive = archive,
      alsoExtract = alsoExtract,
      destinationPath = destinationPath,
      fun = funCaptured,
      quick = quick,
      overwrite = overwrite,
      purge = purge,
      useCache = useCache,
      .tempPath = .tempPath,
      verbose = verbose,
      .callingEnv = .callingEnv,
      ...
    )

    ##################################################################
    # Load object to R
    ##################################################################
    x <- process(out,
                 funCaptured = funCaptured,
                 useCache = useCache, verbose = verbose, .callingEnv = .callingEnv, ...
    )
  }

  ##################################################################
  # postProcess
  ##################################################################
  needPostProcess <- ...names() %in% c(
    "studyArea", "rasterToMatch", "targetCRS", "to", "cropTo",
    "maskTo", "projectTo", "fixErrorsIn", "useSAcrs", "writeTo"
  )
  if (any(needPostProcess)) {
    TopoErrors <- list() # eventually to update a Google ID #TODO
    x <- withCallingHandlers(
      postProcessTo(from = x, ..., destinationPath = destinationPath, overwrite = overwrite),
      message = function(m) {
        hasTopoExcError <- grepl("TopologyException: Input geom 0 is invalid", m$message)
        if (any(hasTopoExcError)) {
          TopoErrors <<- append(TopoErrors, list(m$message))
        }
      }
    )
  }
  .message$IndentRevert()
  stFinal <- reportTime(stStart, mess = paste0(.messageFunctionFn("prepInputs"), " done; took "), minSeconds = 10)
  # if (getOption("reproducible.savePrepInputsState", FALSE))
  #   savePrepInputsState(url, archive, out, stFinal, sysCalls = sys.calls())
  return(x)
}

#' Extract files from archive
#'
#' Extract zip or tar archive files, possibly nested in other zip or tar archives.
#'
#' @param archive Character string giving the path of the archive
#' containing the `file` to be extracted. This path must exist or be `NULL`
#'
#' @param destinationPath Character string giving the path where `neededFiles` will be
#' extracted. Defaults to the archive directory.
#'
#' @param neededFiles Character string giving the name of the file(s) to be extracted.
#'
#' @param extractedArchives Used internally to track archives that have been extracted from.
#' @param filesExtracted Used internally to track files that have been extracted.
#' @param checkSums A checksums file, e.g., created by Checksums(..., write = TRUE)
#' @param needChecksums A numeric, with `0` indicating do not write a new checksums,
#'                      `1` write a new one,
#'                      `2` append new information to existing one.
#' @param checkSumFilePath The full path to the checksum.txt file
#' @param quick Passed to `Checksums`
#' @param ... Passed to `unzip` or `untar`, e.g., `overwrite`
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
  if (!is.null(archive)) {
    if (!(any(c(knownArchiveExtensions) %in% fileExt(archive)))) {
      stop(
        "Archives of type ", fileExt(archive), " are not currently supported. ",
        "Try extracting manually then placing extracted files in ", destinationPath
      )
    }
  }
  if (!is.null(archive) && !is.null(neededFiles)) {
    neededFiles <- setdiff(neededFiles, archive)
  }
  # neededFiles <- setdiff(basename2(neededFiles), basename2(archive))
  if (length(neededFiles) == 0) neededFiles <- NULL
  result <- if (!is.null(neededFiles)) {
    checkSums[checkSums$expectedFile %in% makeRelative(neededFiles, destinationPath), ]$result
  } else {
    "NotOK"
  }
  extractedObjs <- list(filesExtracted = character())
  # needs to pass checkSums & have all neededFiles files
  neededFilesRel <- makeRelative(neededFiles, destinationPath)
  hasAllFiles <- if (NROW(checkSums)) {
    all(neededFilesRel %in% checkSums$expectedFile) # need basename2 for comparison with checkSums
  } else {
    FALSE
  }

  if (!(all(compareNA(result, "OK")) && hasAllFiles)) {
    if (!is.null(archive)) {
      if (!file.exists(archive[1])) {
        stop(
          "No archive exists with filename: ", archive[1],
          ". Please pass an archive name to a path that exists"
        )
      }
      args <- list(archive[1], exdir = destinationPath[1])
      funWArgs <- .whichExtractFn(archive[1], args)

      # need to deal with \\ vs. / and also needs to stay relative
      filesInArchive <- makeRelative(.listFilesInArchive(archive), destinationPath)
      if (is.null(neededFiles)) {
        neededFiles <- filesInArchive
      }

      neededFiles <- checkRelative(neededFiles, absolutePrefix = destinationPath, filesInArchive)
      neededFilesRel <- makeRelative(neededFiles, destinationPath) # neededFiles may have been changed
      neededFiles <- makeAbsolute(neededFiles, destinationPath)
      result <- if (NROW(checkSums)) {
        checkSums[checkSums$expectedFile %in% neededFilesRel, ]$result
      } else {
        logical(0)
      }
      # need to re-Checksums because
      checkSums <- .checkSumsUpdate(
        destinationPath = destinationPath,
        newFilesToCheck = neededFiles,
        checkSums = checkSums,
        checkSumFilePath = checkSumFilePath
      )

      # isOK will have "directories" so it will be longer than neededFiles
      isOK <- if (!is.null(checkSums)) {
        .compareChecksumsAndFilesAddDirs(checkSums, neededFiles, destinationPath)
      } else {
        FALSE
      }

      # recheck, now that we have the whole file list
      if (!(all(isOK)) || NROW(result) == 0) {
        # don't extract if we already have all files and they are fine

        # use binary addition -- 1 is new file, 2 is append
        if (needChecksums == 0) needChecksums <- 2
        filesInArchiveAbs <- makeAbsolute(filesInArchive, destinationPath)
        if (length(archive) > 1) {
          filesExtracted <- c(
            filesExtracted,
            .callArchiveExtractFn(funWArgs$fun, funWArgs$args,
                                  absolutePrefix = destinationPath, archive = archive,
                                  files = basename2(archive[2]), .tempPath = .tempPath
            )
          )
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
        } else if (any(neededFiles %in% filesInArchiveAbs) || is.null(neededFiles)) {
          possibleFolders <- dir.exists(filesInArchive)
          if (sum(possibleFolders)) {
            filesInArchive <- setdiff(filesInArchive, possibleFolders)
          }
          neededFilesRel <- if (is.null(neededFiles)) {
            NULL
          } else {
            if (!is.null(names(isOK))) {
              names(isOK)[!isOK]
            } else {
              makeRelative(neededFiles[!isOK], destinationPath)
            }
          }
          filesToExtractNow <- intersect(filesInArchive, neededFilesRel)
          dt <- data.table(files = filesToExtractNow)
          # extractingTheseFiles <- paste(filesToExtractNow, collapse = "\n")
          # extractingTheseFiles <- paste(basename2(filesInArchive[basename2(filesInArchive) %in%
          #                                                         neededFiles]), collapse = ", ")
          # if (!any(nzchar(filesToExtractNow)))
          #   extractingTheseFiles <- paste0("all files: ",
          #                                  paste(filesInArchive, collapse = "\n"))
          messagePreProcess("From:\n", archive[1], "  \n", "Extracting", verbose = verbose)
          messageDF(dt, indent = .message$PreProcessIndent, verbose = verbose, colour = getOption("reproducible.messageColourPrepInputs"))
          filesExtracted <- c(
            filesExtracted,
            .callArchiveExtractFn(funWArgs$fun,
                                  funWArgs$args,
                                  absolutePrefix = destinationPath,
                                  files = filesToExtractNow,
                                  archive = archive,
                                  .tempPath = .tempPath
            )
          )
        } else {
          # don't have a 2nd archive, and don't have our neededFiles file
          # isArchive <- grepl(fileExt(filesInArchive), pattern = "(zip|tar|rar)", ignore.case = TRUE)
          isArchive <- grepl(fileExt(filesInArchive),
                             pattern = paste0("(", paste(knownArchiveExtensions, collapse = "|"), ")"), ignore.case = TRUE
          )

          if (any(isArchive)) {
            arch <- makeRelative(filesInArchive[isArchive], destinationPath)
            filesExtracted <- c(
              filesExtracted,
              .callArchiveExtractFn(funWArgs$fun, funWArgs$args,
                                    files = arch,
                                    absolutePrefix = destinationPath,
                                    archive = archive,
                                    .tempPath = .tempPath
              )
            )
            filesExtracted <- unique(filesExtracted) # maybe unnecessary

            prevExtract <- lapply(makeAbsolute(arch, destinationPath), function(ap) {
              extractFromArchive(
                archive = ap, destinationPath = destinationPath,
                neededFiles = neededFiles,
                extractedArchives = extractedArchives,
                filesExtracted = filesExtracted,
                checkSums = checkSums,
                needChecksums = needChecksums,
                checkSumFilePath = checkSumFilePath,
                quick = quick,
                .tempPath = .tempPath
              )
            })

            extractedArchives <- c(prevExtract[[1]]$extractedArchives, extractedArchives)
            filesExtracted <- unique(c(prevExtract[[1]]$filesExtracted, filesExtracted))
          }
        }
      } else {
        messagePreProcess("Skipping extractFromArchive: all files already present", verbose = verbose)
        filesExtracted <- checkSums[checkSums$expectedFile %in%
                                      makeRelative(filesInArchive, destinationPath), ]$expectedFile
        filesExtracted <- makeAbsolute(filesInArchive, destinationPath)
      }
    }
  } else {
    if (!is.null(archive)) { # if archive is null, it means there was no archive passed
      messagePreProcess("Skipping extractFromArchive: all needed ",
                        "files now present",
                        verbose = verbose
      )
    }
    filesExtracted <- setdiff(neededFiles, if (!is.null(archive)) makeRelative(archive, destinationPath))
  }
  list(
    extractedArchives = c(extractedArchives, archive),
    neededFiles = neededFiles, # these may have been corrected for user supplying incorrect basename path
    filesExtracted = unique(c(filesExtracted, extractedObjs$filesExtracted)),
    needChecksums = needChecksums,
    checkSums = checkSums
  )
}

#' Try to pick a file to load
#'
#' @keywords internal
#' @rdname guessAtTarget
#' @name guessAtTarget
#' @importFrom utils unzip untar
#' @importFrom stats setNames
#' @inheritParams postProcess
#' @param filesExtracted A character vector of all files that have been extracted (e.g.,
#'                       from an archive)
#' @param destinationPath Full path of the directory where the target file should be
#' @keywords internal
.guessAtTargetAndFun <- function(targetFilePath,
                                 destinationPath = getOption("reproducible.destinationPath", "."),
                                 filesExtracted, fun = NULL, verbose = getOption("reproducible.verbose", 1)) {
  if (all(!is.na(targetFilePath))) {
    possibleFiles <- unique(c(targetFilePath, filesExtracted))
    whichPossFile <- possibleFiles %in% targetFilePath
    if (isTRUE(any(whichPossFile))) {
      possibleFiles <- possibleFiles[whichPossFile]
    }
    isShapefile <- FALSE
    isRaster <- FALSE
    isRDS <- FALSE
    fileExt <- fileExt(possibleFiles)
    feKnown <- .fileExtsKnown() # An object in helpers.R

    funPoss <- lapply(fileExt, function(fe) feKnown[startsWith(prefix = feKnown[[1]], fe), ])
    funPoss <- do.call(rbind, funPoss)

    if (NROW(funPoss) == 0 && length(possibleFiles)) {
      funPoss <- checkSFWebPage(funPoss, fileExt, feKnown, verbose)
    }

    if (length(funPoss)) {
      isShapefile <- fileExt %in% funPoss[funPoss[, "type"] == vectorType(), "extension"]
      isRaster <- fileExt %in% funPoss[funPoss[, "type"] == rasterType(), "extension"]
      isRDS <- fileExt %in% funPoss[funPoss[, "extension"] == .rdsFormat, "extension"]
      if (any(isShapefile)) {
        if (is.null(fun)) {
          if (requireNamespace("sf", quietly = TRUE)) {
            if (!isTRUE(grepl("st_read", fun))) {
              messagePreProcess(
                "Using sf::st_read on shapefile because sf package is available; to force old ",
                "behaviour with 'raster::shapefile' use fun = 'raster::shapefile' or ",
                "options('reproducible.shapefileRead' = 'raster::shapefile')"
              )
            }
          }
        }
      }
    }
    if (is.null(fun)) {
      fun <- unique(funPoss[, "fun"])
      if (length(fun) > 1) {
        if (sum(isRaster) > 0 && sum(isShapefile) > 0) {
          isRaster[isRaster] <- FALSE
          funPoss <- funPoss[funPoss$type == vectorType(), ]
          fun <- unique(funPoss[, "fun"])
          message("The archive has both a shapefile and a raster; selecting the shapefile. If this is incorrect, specify targetFile")
        } else {
          stop(
            "more than one file; can't guess at function to load with; ",
            "please supply 'fun' or 'targetFile' argument to reduce ambiguity"
          )
        }
      }
      if (length(fun) == 0) stop("Can't guess at which function to use to read in the object; please supply 'fun'")
    }
    if (is.null(targetFilePath) || length(targetFilePath) == 0) {
      secondPartOfMess <- if (any(isShapefile)) {
        c(
          " Trying ", fun, " on ", paste(possibleFiles[isShapefile], collapse = ", "), ".",
          " If that is not correct, please specify a different targetFile",
          " and/or fun."
        )
      } else if (is.null(fun)) {
        c(" Also, file extension does not unambiguously specify how it should be loaded. Please specify fun.")
      } else {
        c(
          " Trying ", fun, ".\n",
          " If that is not correct, please specify a targetFile",
          " and/or different fun. The current files in the destinationPath",
          " are: \n",
          paste(possibleFiles, collapse = "\n")
        )
      }
      messagePreProcess(c("targetFile was not specified.", secondPartOfMess), verbose = verbose)

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
          messagePreProcess("Don't know which file to load. Please specify targetFile.", verbose = verbose)
        }
      }
      if (length(targetFilePath) > 1) {
        messagePreProcess("More than one possible files to load:\n", verbose = verbose)
        if (length(targetFilePath) > 100) {
          filesForMess <- data.table(Extracted = targetFilePath)
          messageDF(filesForMess, indent = .message$PreProcessIndent, verbose = verbose)
        } else {
          filesForMess <- paste(targetFilePath, collapse = "\n")
          messagePreProcess(filesForMess)
        }
        messagePreProcess("Picking the last one. If not correct, specify a targetFile.", verbose = verbose)
        targetFilePath <- targetFilePath[length(targetFilePath)]
      }
    }
  }
  list(targetFilePath = targetFilePath, fun = fun)
}

#' @importFrom utils untar unzip
.whichExtractFn <- function(archive, args) {
  out <- NULL
  if (!(is.null(archive))) {
    if (!is.na(archive)) {
      ext <- tolower(fileExt(archive))
      if (!ext %in% knownArchiveExtensions) {
        stop(
          "preProcess can only deal with archives with following extensions:\n",
          paste(knownArchiveExtensions, collapse = ", ")
        )
      }
      canUseArchive <- .requireNamespace("archive")
      mustUseArchive <- !(ext %in% knownInternalArchiveExtensions)
      useArchive <- (mustUseArchive && canUseArchive || canUseArchive)
      if (mustUseArchive && canUseArchive %in% FALSE) {
        stop("Please install.packages('archive') to extract files from \n", archive)
      }
      if (useArchive && .requireNamespace("archive")) {
        fun <- archive::archive_extract
      } else { # base R or system call functions
        if (ext == "zip") {
          fun <- unzip
          args <- c(args, list(junkpaths = FALSE))
        } else if (ext %in% c("tar", "tar.gz", "gz")) {
          fun <- untar
        } else { # system only
          if (ext == "rar") {
            fun <- "unrar"
          } else if (ext == "7z") {
            fun <- "7z"
          }
        }
      }
      out <- list(fun = fun, args = args)
    }
  }
  return(out)
}

#' @keywords internal
#' @importFrom utils capture.output
.callArchiveExtractFn <- function(fun, args, files, overwrite = TRUE,
                                  absolutePrefix = getOption("reproducible.destinationPath", "."),
                                  archive = "",
                                  verbose = getOption("reproducible.verbose", 1), .tempPath) {
  argList <- list(files = files)
  argList$files <- makeRelative(argList$files, absolutePrefix)

  if (missing(.tempPath)) {
    .tempPath <- tempdir2(rndstr(1, 6))
    on.exit(
      {
        unlink(.tempPath, recursive = TRUE)
      },
      add = TRUE
    )
  }

  if (is.character(fun)) {
    if (!fun %in% knownSystemArchiveExtensions) {
      fun <- eval(fun)
    }
  }

  origExdir <- args$exdir
  if (!is.null(args$exdir)) {
    args$exdir <- .tempPath
  }

  if (is.character(fun)) {
    messagePreProcess(
      paste0("The archive appears to be not a .zip. Trying a system call to ", fun),
      verbose = verbose)
    extractSystemCallPath <- .testForArchiveExtract(archive)
    if (grepl(x = extractSystemCallPath, pattern = "7z")) {
      prependPath <- if (isWindows()) {
        paste0("\"", extractSystemCallPath, "\"")
      } else {
        extractSystemCallPath
      }

      # This spits out a message on non-Windows about arguments that are ignored
      suppressMessages({
        output <- system(paste0(prependPath, " x -aoa -o\"", .tempPath, "\" \"",
                                args[[1]], "\""),
                         wait = TRUE,
                         ignore.stdout = FALSE,
                         ignore.stderr = FALSE,
                         invisible = TRUE,
                         show.output.on.console = FALSE, intern = TRUE
        )
      })
    } else {
      system(paste0("unrar x ", args[[1]], " ", .tempPath), wait = TRUE, ignore.stdout = TRUE)
    }
    # list of full paths of all extracted files!
    # extractedFiles <- list.files(path = .tempPath, recursive = TRUE, include.dirs = TRUE)
    # internalFolders <- extractedFiles[fileExt(extractedFiles) == ""]
    # extractedFiles <- setdiff(x = extractedFiles, y = internalFolders)
  } else {
    # Try the direct, then indirect
    isUnzip <- if (identical(unzip, fun)) TRUE else ("overwrite" %in% names(formals(fun)))
    argList <- if (isUnzip) {
      c(argList, overwrite = overwrite)
    } else {
      c(argList)
    }

    # Start the extracting, starting with `archive`
    worked <- FALSE
    if (.requireNamespace("archive", stopOnFALSE = FALSE)) {
      #system.time(
        extractedFiles <- archive::archive_extract(args[[1]], args$exdir, argList$files)
      #  )
      listOfFilesExtracted <- extractedFiles <- list.files(
        path = .tempPath,
        # list of full paths of all extracted files!
        recursive = TRUE,
        all.files = TRUE,
        include.dirs = TRUE
      )

      worked <- all(extractedFiles %in% listOfFilesExtracted)
    }

    if (!worked) {
      if (exists("listOfFilesExtracted", inherits = FALSE))
        rm(listOfFilesExtracted)

      opt <- options("warn")$warn
      on.exit(options(warn = opt), add = TRUE)
      options(warn = 1)
      tooBig <- file.size(args[[1]]) > 5e9
      worked <- FALSE
      if (isUnzip && !tooBig) {
        fattrs <- unzip(args[[1]], list = TRUE)
        ids <- which(fattrs[["Name"]] %in% argList$files)
        tooBig <- any(fattrs[ids, ]["Length"][[1]] >= 4294967295) ## files >= 4GB are truncated; see ?unzip
      }

      if (!tooBig) {
        messagePreProcess("Extracting with R's unzip ... ")
        stExtract <- # system.time(
          mess <- capture.output(
          {
            extractedFiles <- do.call(fun, c(args, argList))
          },
          type = "message"
        )# )
        worked <- if (isUnzip) {
          all(normPath(file.path(args$exdir, argList[[1]])) %in% normPath(extractedFiles))
        } else {
          isTRUE(extractedFiles == 0)
        }
      }
      if (!isTRUE(worked) || isTRUE(tooBig)) {
        unz <- Sys.which("unzip")
        sZip <- Sys.which("7z")

        if (!isTRUE(tooBig)) {
          messagePreProcess("File unzipping using R does not appear to have worked.",
                            " Trying a system call of unzip...",
                            verbose = verbose
          )
        } else {
          messPart1 <- "R's unzip utility cannot handle a zip file this size.\n"
          if (nchar(sZip) > 0) {
            messagePreProcess(messPart1, verbose = verbose)
          } else {
            stop(
              paste(
                messPart1,
                # "Install 7zip and add it to your PATH (see https://www.7-zip.org/)."
                "Try installing the archive package then rerunning: \ninstall.packages('archive')"
              )
            )
          }
        }

        if (file.exists(args[[1]])) {
          pathToFile <- normPath(args[[1]])
        } else {
          if (file.exists(file.path(args$exdir, args[[1]]))) {
            pathToFile <- normPath(file.path(args$exdir, args[[1]]))
          } else {
            warning(mess)
            stop(
              "prepInputs cannot find the file ", basename2(args[[1]]), ".",
              " The file might have been moved during unzipping or is corrupted."
            )
          }
        }
        if (nchar(sZip) > 0) {
          messagePreProcess("Using '7zip'")
          op <- setwd(.tempPath)
          on.exit(
            {
              setwd(op)
            },
            add = TRUE
          )
          lstFiles <- system(paste0(sZip, " l ", pathToFile), intern = TRUE, wait = TRUE)
          startAndEnd <- grep("-----------", lstFiles)
          if (diff(startAndEnd) > 1) {
            lstFiles <- lstFiles[(startAndEnd[1] + 1):(startAndEnd[2] - 1)]
          }
          needListFiles <- FALSE
          if (length(files)) {
            filesAreInArch <- filenamesFromArchiveLst(lstFiles)
            if (all(files %in% filesAreInArch)) {
              if (all(filesAreInArch %in% files))
                needListFiles <- FALSE
              else
                needListFiles <- TRUE
            } else {
              stop("Some files are not in the archive (", pathToFile, "). Specifically:\n",
                   paste(files[!files %in% filesAreInArch], collapse = "\n"))
            }
          }

          # filesAreInArch <- unlist(lapply(files, function(x) any(grepl(x, lstFiles))))
          arg22 <- paste0(" x ", pathToFile)
          if (needListFiles) {
            arg22 <- paste(arg22, paste(files, collapse = " "))
          }
          system2(sZip,
                  args = arg22,
                  wait = TRUE,
                  stdout = NULL
          )
        } else if (nchar(unz) > 0) {
          messagePreProcess("Using 'unzip'")
          system2(unz,
                  args = paste0(pathToFile, " -d ", .tempPath),
                  wait = TRUE,
                  stdout = NULL
          )
        } else {
          if (nchar(unz) == 0) {
            stop(
              "unzip command cannot be found.",
              " Please try reinstalling Rtools if on Windows, and/or add unzip to system path",
              " (e.g., see 'https://cran.r-project.org/bin/windows/Rtools/'.)"
            )
          }
          stop(
            "There was no way to unzip all files; try manually. The file is located at: \n",
            pathToFile
          )
        }
      }
    }
    if (!isUnzip) {
      extractedFiles <- files
    }
  }

  if (!exists("listOfFilesExtracted", inherits = FALSE))
    listOfFilesExtracted <- list.files(
      path = .tempPath,
      # list of full paths of all extracted files!
      all.files = TRUE,
      recursive = TRUE,
      include.dirs = TRUE
    )

  mess <- paste0("       ... Done extracting ", length(listOfFilesExtracted), " files")
  if (exists("stExtract", inherits = FALSE))
    mess <- paste0(mess, "; took ", format(as.difftime(stExtract[3], units = "secs"), units = "auto"))
  messagePreProcess(mess)

  from <- makeAbsolute(listOfFilesExtracted, .tempPath)
  on.exit(
    {
      if (any(file.exists(from))) {
        suppressWarnings(try(unlink(from), silent = TRUE))
      }
    },
    add = TRUE
  )

  args$exdir <- origExdir
  to <- file.path(args$exdir, listOfFilesExtracted)

  suppressWarnings({
    out <- hardLinkOrCopy(from, to, verbose = 0)
  })

  if (!isTRUE(all(file.exists(to)))) {
    stop(paste("Could not move listOfFilesExtracted from", .tempPath, "to", args$exdir))
  }
  listOfFilesExtracted <- to
  # unlink(.tempPath, recursive = TRUE) # don't delete it if it was not created here --> on.exit does this

  if (length(listOfFilesExtracted) == 0) {
    stop(
      "preProcess could not extract the files from the archive ", args[[1]], ".\n",
      "Please try to extract it manually to the destinationPath"
    )
  }
  return(listOfFilesExtracted)
}

#' @keywords internal

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
#' @importFrom utils capture.output
#' @importFrom data.table rbindlist as.data.table setDT setDF
appendChecksumsTable <- function(checkSumFilePath, filesToChecksum,
                                 destinationPath = getOption("reproducible.destinationPath", "."),
                                 append = TRUE, verbose = getOption("reproducible.verbose", 1)) {
  csf <- tempfile(fileext = ".TXT")
  areAbs <- isAbsolutePath(filesToChecksum)
  if (any(!areAbs)) {
    filesToChecksum[!areAbs] <- file.path(destinationPath, filesToChecksum[!areAbs])
  }
  capture.output(type = "message", {
    currentFiles <- Checksums(
      path = destinationPath, write = TRUE,
      files = filesToChecksum,
      checksumFile = csf,
      verbose = verbose
    )
  })

  rip <- getOption("reproducible.inputPaths")
  checkSumFilePaths <- if (!is.null(rip)) {
    unique(c(checkSumFilePath, file.path(rip, basename(checkSumFilePath))))
  } else {
    checkSumFilePath
  }

  for (checkSumFilePath in checkSumFilePaths) {
    appendChecksumsTableWithCS(append, checkSumFilePath, destinationPath, filesToChecksum,
                               currentFiles = currentFiles, verbose = verbose)
  }
  return(currentFiles)
}


#' List files in either a `.zip` or or `.tar` file
#'
#' Makes the outputs from`.tar``.zip` the same, which they aren't by default.
#'
#' @param archive A character string of a single file name to list files in.
#'
#' @return A character string of all files in the archive.
#'
#' @keywords internal
#' @rdname listFilesInArchive
.listFilesInArchive <- function(archive) {
  needSystemCall <- (length(archive) > 0 && fileExt(archive[1]) %in% knownSystemArchiveExtensions)
  if (length(archive) > 0) {
    if (file.exists(archive[1])) {
      needSystemCall <- needSystemCall || file.size(archive[1]) > 2e9
    }
  }

  if (needSystemCall) {
    extractSystemCallPath <- .testForArchiveExtract(archive)
    funWArgs <- list(fun = extractSystemCallPath)
    } else {
    funWArgs <- .whichExtractFn(archive[1], NULL)
  }

  filesInArchive <- NULL
  if (!is.null(funWArgs$fun)) {
    if (file.exists(archive[1])) {
      if (!needSystemCall) {
        if (.requireNamespace("archive") && identical(archive::archive_extract, funWArgs$fun)) {
          filesInArchive <- archive::archive(archive[1])
        } else {
          filesInArchive <- funWArgs$fun(archive[1], list = TRUE)
        }
        nams <- names(filesInArchive)
        if ("Name" %in% nams) {
          # for zips, rm directories (length = 0)
          filesInArchive <- filesInArchive[filesInArchive$Length != 0, ]$Name
        } else if ("path" %in% nams) {
          # from archive::archive
          filesInArchive <- filesInArchive[filesInArchive$size != 0, ]$path
        } else {
          # untar & archive::archive
          filesInArchive
        }
      } else {
        if (grepl(x = extractSystemCallPath, pattern = "7z")) {
          extractSystemCall <- paste0("\"", extractSystemCallPath, "\"", " l \"", path.expand(archive[1]), "\"")
          if (isWindows()) {
              filesOutput <- captureWarningsToAttr(
                system(extractSystemCall, show.output.on.console = FALSE, intern = TRUE)
              )
              warn <- attr(filesOutput, "warning")
              attr(filesOutput, "warning") <- NULL
            } else {
              # On Linux/MacOS
              filesOutput <- captureWarningsToAttr(
                system(extractSystemCall, intern = TRUE, ignore.stderr = TRUE)
              )
              warn <- attr(filesOutput, "warning")
              attr(filesOutput, "warning") <- NULL
            }
          } else {
            archiveExtractBinary <- .archiveExtractBinary()
            if (is.null(archiveExtractBinary)) {
              stop("unrar is not on this system; please install it")
            }
            filesOutput <- system(paste0("unrar l ", archive[1]), intern = TRUE)
          }
          if (exists("warn", inherits = FALSE) && isTRUE(any(grepl("had status 2", warn)))) {
            stop(warn)
          }
          if (isTRUE(any(grepl("(Can not open the file as archive)|(Errors: 1)", filesOutput)))) {
            stop("archive appears defective")
          }
          # filesInBetween <- grep(pattern = "----", filesOutput)
          # filesLines <- filesOutput[(min(filesInBetween) + 1):(max(filesInBetween) - 1)]
          filesInArchive <- filenamesFromArchiveLst(filesOutput)
          # filenamesFromArchiveLst <- function(filesLines) {
          #   filesInArchive <- unlist(lapply(X = seq_along(filesLines), FUN = function(line) {
          #     first5trimmed <- unlist(strsplit(filesLines[[line]], split = " +"))[-(1:5)]
          #     if (length(first5trimmed) > 1)
          #       first5trimmed <- paste(first5trimmed, collapse = " ")
          #     # first5trimmed <- unlist(strsplit(filesLines[[line]], split = "  "))
          #     return(first5trimmed)
          #   }))
          # }
          if (length(filesInArchive) == 0) {
            stop("preProcess could not find any files in the archive ", archive)
        }
      }
    }
  }
  if (isTRUE(any(grepl("\\\\", filesInArchive))))
    filesInArchive <- gsub("\\\\", "/", filesInArchive)
  return(filesInArchive)
}

.compareChecksumsAndFilesAddDirs <- function(checkSums, files, destinationPath) {
  isOK <- NULL
  if (!is.null(files)) {
    checkSumsDT <- data.table(checkSums)
    if (NCOL(checkSumsDT) == 0) {
      checkSumsDT <- Copy(.emptyChecksumsResult)
    }
    dirs <- makeRelative(dirname(files), destinationPath) # basename2(unique(dirname(files)))
    dirs <- dirs[nzchar(dirs)]
    filesDT <- data.table(files = unique(makeRelative(c(files, dirs), destinationPath)))
    isOKDT <- checkSumsDT[filesDT, on = c(expectedFile = "files")]
    isOKDT2 <- checkSumsDT[filesDT, on = c(actualFile = "files"), nomatch = NA]
    # fill in any OKs from "actualFile" into the isOKDT
    isOKDT[compareNA(isOKDT2$result, "OK"), "result"] <- "OK"
    if (!all(compareNA(isOKDT$result, "OK"))) {
      isOKDT <- checksumsDirsOk(isOKDT)
    }

    isOK <- compareNA(isOKDT$result, "OK")
    names(isOK) <- makeRelative(filesDT$files, destinationPath)
  }
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
  possPrograms <- unique(unlist(lapply(c("7z", "unrar"), Sys.which)))
  extractSystemCallPath <- if (!all(possPrograms == "")) {
    possPrograms[nzchar(possPrograms)][1] # take first one if there are more than one
  } else {
    ""
  }
  if (!(isWindows() && !isMac())) { ## TODO: macOS ?? #266
    if (grepl("7z", extractSystemCallPath)) {
      SevenZrarExists <- system("apt -qq list p7zip-rar", intern = TRUE, ignore.stderr = TRUE)
      SevenZrarExists <- grepl(SevenZrarExists, pattern = "installed")
      if (isFALSE(SevenZrarExists)) {
        messagePreProcess("To extract .rar files, you will need p7zip-rar, not just p7zip-full. Try: \n",
                          "--------------------------\n",
                          "apt install p7zip-rar\n",
                          "--------------------------\n",
                          verbose = verbose
        )
      }
    }
  }

  if (identical(extractSystemCallPath, "")) {
    if (isWindows()) {
      extractSystemCallPath <- Sys.which("7z.exe")
      if (extractSystemCallPath == "") {
        messagePreProcess("prepInputs is looking for 'unrar' or '7z' in your system...", verbose = verbose)
        extractSystemCallPath <- list.files("C:/Program Files",
                                            pattern = "unrar.exe|7z.exe",
                                            recursive = TRUE,
                                            all.files = TRUE,
                                            full.names = TRUE
        )
        if (extractSystemCallPath == "" || length(extractSystemCallPath) == 0) {
          extractSystemCallPath <- list.files(dirname(Sys.getenv("SystemRoot")),
                                              pattern = "unrar.exe|7z.exe",
                                              recursive = TRUE,
                                              all.files = TRUE,
                                              full.names = TRUE
          )
          if (extractSystemCallPath == "" || length(extractSystemCallPath) == 0) {
            extractSystemCallPath <- NULL
            messagePreProcess(missingUnrarMess, verbose = verbose)
          } else {
            messagePreProcess("The extracting software was found in an unusual location: ",
                              extractSystemCallPath, ".",
                              "If you receive an error when extracting the archive, please install ",
                              "'7zip' or 'unrar' in 'Program Files' directory.",
                              verbose = verbose
            )
          }
        }
        extractSystemCallPath <- extractSystemCallPath[1]
      }
    } else {
      messagePreProcess(missingUnrarMess,
                        "Try installing with, e.g.,: \n",
                        "--------------------------\n",
                        "apt install p7zip p7zip-rar p7zip-full -y\n",
                        "yum install p7zip p7zip-plugins -y\n",
                        "--------------------------",
                        verbose = verbose
      )
    }
  }
  if (!exists("extractSystemCallPath", inherits = FALSE)) extractSystemCallPath <- NULL
  if (is.null(extractSystemCallPath) || !nzchar(extractSystemCallPath)) extractSystemCallPath <- NULL

  return(extractSystemCallPath)
}

#' Returns `unrar` path and creates a shortcut as `.systemArchivePath`.
#' Was not incorporated in previous function so it can be used in the tests.
#'
#' @return
#' path to `unrar` or `7-zip` if it exists, and assign it to `.systemArchivePath`.
#' Advise the user if not found..
#'
#' @author Tati Micheletti
#'
#' @keywords internal
#' @rdname testForArchiveExtract
#' @name testForArchiveExtract
.testForArchiveExtract <- function(archive = "") {
  sevenzName <- grep("7z", knownSystemArchiveExtensions, value = TRUE)

  extractSystemCallPath <- NULL
  if (tools::file_ext(archive) == sevenzName) {
    extractSystemCallPath <- Sys.which(sevenzName)
  }

  if (is.null(extractSystemCallPath)) {
    if (!is.null(.systemArchivePath)) {
      extractSystemCallPath <- .systemArchivePath
    } else {
      # Find the path to unrar and assign to a package-stored object
      usrTg <- paste(sample(x = LETTERS, size = 15), collapse = "")
      # Cache for project-level persistence
      extractSystemCallPath <- Cache(.archiveExtractBinary, userTags = usrTg)
      utils::assignInMyNamespace(".systemArchivePath", extractSystemCallPath) # assign in namespace for pkg
    }
    if (is.null(extractSystemCallPath)) {
      clearCache(userTags = usrTg, ask = FALSE)
      stop(
        "prepInputs did not find '7-Zip' nor 'unrar' installed.",
        " Please install it before running prepInputs for a '.rar' archive"
      )
    }
  }
  return(extractSystemCallPath)
}

#' The known path for unrar or 7z
#' @rdname unrarPath
#' @name unrarPath
.systemArchivePath <- NULL

missingUnrarMess <- "The archive is a 'rar' archive; your system does not have unrar or 7zip;\n"
proj6Warn <- "NOT UPDATED FOR PROJ"

sevenzName <- "7z"
knownArchivePkgExtensions <- c("zip", "tar", "tar.gz", "gz", "rar", sevenzName, "cab")
knownInternalArchiveExtensions <- c("zip", "tar", "tar.gz", "gz")
# knownSystemArchiveExtensions <- c("rar", sevenzName, "unrar")
knownSystemArchiveExtensions <- c("rar", sevenzName)
knownArchiveExtensions <- unique(c(knownInternalArchiveExtensions, knownSystemArchiveExtensions,
                            knownArchivePkgExtensions))


prepInputsAssertions <- function(env) {
  noisy <- nullOr(c("character", "logical"), c("alsoExtract"), env = env)
  noisy <- nullOr(c("character", "logical"), c("useCache", "archive"), env)
  noisy <- nullOr(c("numeric", "logical"), c("purge", "verbose"), env)
  noisy <- nullOr("character", c(
    "destinationPath", "targetFile", "url", # "archive",
    ".tempPath"
  ), env = env)
  noisy <- nullOr("logical", c("quick", "overwrite"), env = env)
}

nullOr <- function(clses, vals, env) {
  vapply(vals, function(val) {
    out <- is.null(env[[val]])
    if (out) { # pull out fast if NULL
      return(out)
    }
    out <- inherits(env[[val]], clses)
    if (isFALSE(out)) {
      stop(
        val, " must be of class", "(es)"[(length(clses) > 1)], " ",
        paste(clses, collapse = ", "),
        " or set to its default. It is currently ", class(env[[val]]), "."
      )
    }
    out
  }, logical(1))
}

is.nulls <- function(x) lapply(x, is.null)



#' @include messages.R
process <- function(out, funCaptured,
                    useCache = getOption("reproducible.useCache"),
                    verbose = getOption("reproducible.verbose"),
                    .callingEnv = parent.frame(),
                    ...) {
  theFun <- out$fun
  suppressWarnings({
    naFun <- all(is.na(theFun))
  })

  funChar <- if (is.character(out$funChar)) out$funChar else NULL

  out <- modifyList(out, list(...))

  argsFromPrepInputsFamily <- unique(c(
    .namesPostProcessFormals(), formalArgs(prepInputs), formalArgs(preProcess),
    "checkSums", "dots", "object"
  ))
  args <- NULL
  # keep the ones for theFun
  isAlreadyQuoted <- tryCatch(any(grepl("quote", theFun)), silent = TRUE,
                              error = function(e) FALSE)
  if (isAlreadyQuoted) {
    theFun <- eval(theFun, envir = out)
  }
  if (is.name(theFun))
    theFun <- eval(theFun, envir = .callingEnv)

  # need to differentiate sf::st_read from sf::st_read(targetFile, TRUE) -- both are calls, both length 3; both have pkgColon
  if (length(theFun) == 3 && isDollarSqBrPkgColon(theFun) && all(lengths(as.list(theFun)) == 1)) {
    theFun <- eval(theFun, envir = out)
  }
  if (naFun %in% FALSE && !is.call(theFun)) {
    formsForTheFun <- names(formals3(theFun))
    argsFromPrepInputsFamily <- setdiff(argsFromPrepInputsFamily, names(formals3(theFun)))
    argsPassingToTheFun <- out[!names(out) %in% argsFromPrepInputsFamily]
    args <- argsPassingToTheFun[names(argsPassingToTheFun) %in% formsForTheFun]
  }

  otherFiles <- out$checkSums[result == "OK"]
  .cacheExtra <- NULL
  if (NROW(otherFiles)) {
    .cacheExtra <- .robustDigest(sort(otherFiles$checksum.x))
  }
  out[["targetFile"]] <- out[["targetFilePath"]] # handle both

  if (!(naFun || is.null(theFun))) {

    x <- if (is.null(out$object)) {
      st <- Sys.time()
      messagePreProcess("Running `process` (i.e., loading file into R)", verbose = verbose, verboseLevel = 0)
      .message$IndentUpdate()
      if (!is.null(out$targetFilePath)) {
        if (!all(is.na(out$targetFilePath)))
          messagePreProcess("targetFile located at:\n", paste(out$targetFilePath, collapse = "\n"), verbose = verbose)
      }

      if (!isTRUE(is.na(out$targetFilePath)))
        messagePreProcess("Loading object into R", verbose = verbose)
      needRaster <- any(grepl("raster$|stack$|brick$", funCaptured)) ||
        any(grepl("raster$|stack$|brick$", funChar)) || any(grepl("raster", capture.output(show(theFun))))
      needTerra <- any(grepl("terra|rast$", funCaptured)) || any(grepl("terra|rast$", funChar)) ||
        any(grepl("terra", capture.output(show(theFun))))
      if (needRaster) {
        .requireNamespace("raster", stopOnFALSE = TRUE)
      }
      if ((needRaster || needTerra) && !is.call(theFun)) {
        ## Don't cache the reading of a raster
        ## -- normal reading of raster on disk is fast b/c only reads metadata
        outProcess <- try(do.call(theFun, append(list(asPath(out$targetFilePath)), args)))
        if (is(outProcess, "try-error")) browser()
      } else {
        if (identical(theFun, base::load)) {
          if (is.null(args$envir)) {
            messagePreProcess("Running base::load, returning objects as a list. Pass envir = anEnvir ",
                              "if you would like it loaded to a specific environment",
                              verbose = verbose
            )
            tmpEnv <- new.env(parent = emptyenv())
            returnAsList <- TRUE
          } else {
            tmpEnv <- args$envir
            args$envir <- NULL
            returnAsList <- FALSE
          }
          args2 <- append(list(file = out$targetFilePath, envir = tmpEnv), args)
          outProcess <- do.call(theFun, args2)
          if (returnAsList) {
            outProcess <- as.list(tmpEnv, all.names = TRUE)
          }
        } else {
          useCache2 <- useCache
          if (any(fileExt(out$targetFilePath) %in% c(.qsFormat, .rdsFormat)) &&
              !isTRUE(getOption("reproducible.useMemoise"))) {
            useCache2 <- FALSE
            messagePreProcess("targetFile is already a binary; skipping Cache while loading")
          }

          withCallingHandlers(
            # theFun could have been a call to get the function, e.g., fun = getOption("reproducible.shapefileRead")
            # so need to try 2x, just to figure out the function
            for (i in 1:2) {
              if (is.call(theFun)) { # an actual call, not just captured function name
                # put `targetFilePath` in the first position -- allows quoted call to use first arg
                out <- append(
                  append(
                    list(targetFilePath = out[["targetFilePath"]]),
                    out[-which(names(out) == "targetFilePath")]
                  ),
                  args
                )
                # out[["targetFile"]] <- out[["targetFilePath"]] # handle both
                if (is.null(funChar)) funChar <- paste0(substr(format(theFun), start = 1, stop = 40), "...")
                outProcess <- Cache(eval(theFun, envir = out, enclos = .callingEnv),
                                    useCache = useCache2, .cacheExtra = .cacheExtra,
                                    .functionName = funChar, omitArgs = "enclos"
                )
              } else {
                args2 <- append(list(asPath(out$targetFilePath)), args)
                outProcess <- Cache(do.call, theFun, args2,
                                    useCache = useCache2, .cacheExtra = .cacheExtra,
                                    .functionName = funChar
                )
              }
              # theFun could have been a call to get the function, e.g., fun = getOption("reproducible.shapefileRead")
              #  If this was the case, then the above will have just evaluated that
              if (identical(1L, length(outProcess))) {
                if (isTRUE(is.character(outProcess))) {
                  possTheFun <- eval(parse(text = outProcess), envir = out, enclos = .callingEnv)
                  if (isTRUE(is.function(possTheFun))) {
                    theFun <- possTheFun
                    next
                  }
                }
              }
              break
            },
            message = function(m) {
              m$message <- grep(paste0(.message$NoCachePathSupplied, "|useCache is FALSE"), m$message, invert = TRUE, value = TRUE)
              if (length(m$message)) {
                mm <- gsub("(.*)\n$", "\\1", m$message)
                message(mm) # this MUST NOT CREATE INDENTING -- using 'message' here
              }
              tryInvokeRestart("muffleMessage")
            }
          )
          # outProcess
        }
      }
      .message$IndentRevert()
      stNext <- reportTime(st, mess = "`process` done; took ", minSeconds = 10)
      outProcess

    } else {
      if (is.null(theFun) || !is.call(theFun) || naFun) {
        x <- out$object
      } else {
        # x <- out$object
        env1 <- new.env()
        list2env(out, envir = env1)
        eval(theFun, envir = env1)
      }
    }
  } else {
    x <- if ((is.null(theFun) || is.na(theFun)) && !is.null(out$object)) {
      out$object
    } else {
      messagePreProcess("No loading of object into R; fun = ", theFun, "; returning the targetFilePath: ",
                        out$targetFilePath, verbose = verbose)
      out$targetFilePath
    }
  }
  x
}

removeDirs <- function(paths) {
  out <- strsplit(paths, "\\\\|/")
  lens <- lengths(out)
  la <- unlist(lapply(unique(lens), function(len) {
    table(sapply(out[lens >= len], function(xx) paste(xx[seq(len)], collapse = "/")))
  }))
  dirs <- names(la[la > 1])
  paths <- paths[!paths %in% dirs]

}


filenamesFromArchiveLst <- function(filesOutput) {
  filesInBetween <- grep(pattern = "----", filesOutput)
  filesLines <- if (length(filesInBetween) == 0)
    filesOutput
  else
    filesOutput[(min(filesInBetween) + 1):(max(filesInBetween) - 1)]

  filesInArchive <- unlist(lapply(X = seq_along(filesLines), FUN = function(line) {
    tail(unlist(strsplit(filesLines[[line]], split = " +")), 1)
    # first5trimmed <- unlist(strsplit(filesLines[[line]], split = " +"))[-(1:5)]
    # if (length(first5trimmed) > 1)
    #   first5trimmed <- paste(first5trimmed, collapse = " ")
    # # first5trimmed <- unlist(strsplit(filesLines[[line]], split = "  "))
    # return(first5trimmed)
  }))
  if (isTRUE(any(grepl("\\\\", filesInArchive))))
    filesInArchive <- gsub("\\\\", "/", filesInArchive)

  filesInArchive <- removeDirs(filesInArchive)

  filesInArchive
}



reportTime <- function(stStart, mess, minSeconds) {
  stNow <- Sys.time()
  dt1sec <- difftime(stNow, stStart, units = "secs")
  dt1auto <- difftime(stNow, stStart)
  messagePreProcess(mess, format(dt1auto, units = "auto"), verbose = dt1sec > minSeconds)
  stNow
}


readCheckSumFilePath <- function(checkSumFilePath, destinationPath, filesToChecksum) {
  # a checksums file already existed, need to keep some of it
  cs <- suppressWarnings(try(read.table(checkSumFilePath, header = TRUE), silent = TRUE))
  if (is(cs, "try-error")) {
    # meant that it was an empty CHECKSUMS.txt file -- rebuild it
    cs <- NULL # append <- FALSE
  }
  cs
}

extractFileNOTtoChecksum <- function(cs, destinationPath, filesToChecksum) {
  setDT(cs)
  cs[!makeRelative(file, destinationPath) %in%
       makeRelative(filesToChecksum, destinationPath)]
  setDF(cs)
  cs
}



appendChecksumsTableWithCS <- function(append, checkSumFilePath, destinationPath,
                                       filesToChecksum, currentFiles, verbose) {
  if (append) {
    cs <- readCheckSumFilePath(checkSumFilePath, destinationPath, filesToChecksum)
    if (is.null(cs)) {
      append <- FALSE
    } else {
      # a checksums file already existed, need to keep some of it
      nonCurrentFiles <- extractFileNOTtoChecksum(cs, destinationPath, filesToChecksum)
    }
  }

  doWrite <- TRUE
  if (append) { # a checksums file already existed, need to keep some of it
    messStart <- "Appending "
    messagePreProcess(messStart, "checksums to CHECKSUMS.txt. If you see this message repeatedly, ",
                      "you can specify targetFile (and optionally alsoExtract) so it knows ",
                      "what to look for.", verbose = verbose)

    currentFilesToRbind <- currentFilesToChecksumsTable(currentFiles, nonCurrentFiles, verbose = verbose)

    # Sometimes a checksums file doesn't have filesize
    if (!is.null(cs$filesize)) {
      if (!is.character(cs$filesize)) {
        cs$filesize <- as.character(cs$filesize)
      }
    }
    if (identical(cs, as.data.frame(currentFilesToRbind))) {
      doWrite <- FALSE
    }
  } else {
    currentFilesToRbind <- currentFilesToChecksumsTable(currentFiles, verbose = verbose)
  }

  if (doWrite) {
    writeChecksumsTable(as.data.frame(currentFilesToRbind), checkSumFilePath, dots = list())
  }
}


currentFilesToChecksumsTable <- function(currentFiles, nonCurrentFiles = NULL, verbose) {
  currentFilesToRbind <- data.table::as.data.table(currentFiles)
  keepCols <- c("expectedFile", "checksum.x", "algorithm.x", "filesize.x")
  currentFilesToRbind <- currentFilesToRbind[, keepCols, with = FALSE]
  data.table::setnames(currentFilesToRbind,
                       old = keepCols,
                       new = c("file", "checksum", "algorithm", "filesize")
  )
  currentFilesToRbind <- rbindlist(list(nonCurrentFiles, currentFilesToRbind), fill = TRUE)

  # Attempt to not change CHECKSUMS.txt file if nothing new occurred
  currentFilesToRbind <- unique(currentFilesToRbind)
  anyDuplicates <- duplicated(currentFilesToRbind)
  if (any(anyDuplicates)) {
    messagePreProcess("The current targetFile is not the same as the expected targetFile in the ",
                      "CHECKSUMS.txt; appending new entry in CHECKSUMS.txt. If this is not ",
                      "desired, please check files for discrepancies",
                      verbose = verbose
    )
  }
  currentFilesToRbind
}


# savePrepInputsState <- function(url, archive, out, stFinal, sysCalls) {
#   if (is.null(url)) url <- ""
#   if (is.null(out$targetFilePath)) out$targetFilePath <- ""
#   if (is.null(out$destinationPath)) out$destinationPath <- ""
#   if (is.null(out$fun)) out$fun <- ""
#   if (is.null(archive)) archive <- ""
#   co <- paste0(capture.output(sysCalls[[length(sysCalls)]]), collapse = " ")
#   if (isTRUE(!any(grepl(" *<- *", co)))) {
#     co <- ""
#     Cached <- .grepSysCalls(sys.calls(), pattern = "Cache")
#     prepInputed <- .grepSysCalls(sys.calls(), pattern = "prepInputs")
#     if (length(Cached)) {
#       CachedPoss <- sysCalls[Cached]
#       if (identical(as.character(CachedPoss[[2]])[1], "prepInputs")) {
#         co <- paste0(capture.output(sysCalls[[Cached]]), collapse = " ")
#       } else {
#         co <- paste0(capture.output(sysCalls[tail(Cached, 1)]), collapse = " ")
#       }
#     }
#   }
#
#   objName <- strsplit(paste0(co, collapse = " "), split = " *<- *")[[1]][1]
#
#   keep <- setDT(list(objName = objName, url = url, archive = archive, targetFile = out$targetFilePath,
#                      destinationPath = out$destinationPath,
#                      fun = format(out$funChar), time = stFinal))
#   if (is.null(.pkgEnv[[._txtPrepInputsObjects]])) {
#     .pkgEnv[[._txtPrepInputsObjects]] <- keep
#   } else {
#     .pkgEnv[[._txtPrepInputsObjects]] <- tryCatch(rbindlist(list(.pkgEnv[[._txtPrepInputsObjects]], keep)), error = function(e) browser())
#   }
#   return(invisible())
# }


checkSFWebPage <- function(funPoss, fileExt, feKnown, verbose) {
  if (requireNamespace("rvest")) {
    sfURL <- "https://r-spatial.github.io/sf/articles/sf2.html#guessing-a-driver-for-output"
    tbls_ls <- try({rvest::read_html(sfURL) |>
        rvest::html_nodes("table")  |>
        rvest::html_table(fill = TRUE) } |>
          Cache(verbose = FALSE))
    if (!is(tbls_ls, "try-error")) {
      exts <- tbls_ls[[1]]$extension
      if (isTRUE(any(fileExt %in% exts))) {
        funPoss <- data.frame(fileExt, "sf::st_read", "sf::st_write", "sf") |>
          setNames(names(feKnown))
      }
    } else {
      messagePrepInputs("It looks like the sf article identifying which extensions",
                        "it can read has changed", "\nPlease contact reproducible",
                        "developers citing this message. Currently using:\n",
                        sfURL, verbose = verbose + 1)
    }

  } else {
    messagePrepInputs("`reproducible` does not know the file type passed.\n",
                      "Please run `install.packages('rvest')` to load other known ",
                      "file types that the `sf` package can load.",
                      verbose = verbose + 1)
  }
  funPoss
}
