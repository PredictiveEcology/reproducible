if (getRversion() >= "3.1.0") {
  utils::globalVariables(c("expectedFile", "objName", "V1",
                           "method", "rasterToMatch", "studyArea", "targetCRS",
                           "to", "useSAcrs", "datatype"))
}

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
#'     \item Download from the web via either [googledrive::drive_download()],
#'     [utils::download.file()];
#'     \item Extract from archive using [unzip()] or [untar()];
#'     \item Load into R using [raster::raster()],
#'     [raster::shapefile()], or any other function passed in with `fun`;
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
#'     \item Crop using [cropInputs()];
#'     \item Project using [projectInputs()];
#'     \item Mask using [maskInputs()];
#'     \item Determine file name [determineFilename()] via `filename2`;
#'     \item Optionally, write that file name to disk via [writeOutputs()].
#'    }
#'
#'   NOTE: checksumming does not occur during the post-processing stage, as
#'   there are no file downloads. To achieve fast results, wrap
#'   `prepInputs` with `Cache`.
#'
#'   NOTE: `sf` objects are still very experimental.
#'
#' \subsection{postProcessing of `Raster*` and `Spatial*` objects:}{
#'
#'   If `rasterToMatch` or `studyArea` are used, then this will
#'   trigger several subsequent functions, specifically the sequence,
#'   *Crop, reproject, mask*, which appears to be a common sequence in
#'   spatial simulation. See [postProcess.spatialClasses()].
#'
#'   *Understanding various combinations of `rasterToMatch`
#'   and/or `studyArea`:*
#'   Please see [postProcess.spatialClasses()].
#'  }
#'
#'
#' @section `fun`:
#'
#'  `fun` offers the ability to pass any custom function with which to load
#'  the object obtained by `preProcess` into the session. There are two cases that are
#'  dealt with: when the `preProcess` downloads a file (including via `dlFun`),
#'  `fun` must deal with a file; and, when `preProcess` creates an R object
#'  (e.g., raster::getData returns an object), `fun` must deal with an object.
#'
#'  `fun` can be supplied in three ways: a function, a character string
#'   (i.e., a function name as a string), of a quoted expression.
#'   If a character string or function, is should have the package name e.g.,
#'   `"raster::raster"` or as an actual function, e.g., `base::readRDS`.
#'   In these cases, it will evaluate this function call while passing `targetFile`
#'   as the first argument. These will only work in the simplest of cases.
#'
#'   When more precision is required, the full call can be written, surrounded by
#'   `quote`, and where the object can be referred to as `targetFile` if the function
#'   is loading a file or as `x` if it is loading the object that was returned by
#'   `preProcess`. If `preProcess` returns an object, this must be used by `fun`; if
#'   `preProcess` is only getting a file, then there will be no object, so `targetFile` is the
#'   only option.
#'
#'   If there is a custom function call, is not in a package, `prepInputs` may not find it. In such
#'   cases, simply pass the function as a named argument (with same name as function) to `prepInputs`.
#'   See examples.
#'   NOTE: passing `NA` will skip loading object into R. Note this will essentially
#'   replicate the functionality of simply calling `preProcess` directly.
#'
#' @section `purge`:
#'
#' In options for control of purging the `CHECKSUMS.txt` file are:
#'
#'   \describe{
#'     \item{`0`}{keep file}
#'     \item{`1`}{delete file}
#'     \item{`2`}{delete entry for `targetFile`}
#'     \item{`4`}{delete entry for `alsoExtract`}
#'     \item{`3`}{delete entry for `archive`}
#'     \item{`5`}{delete entry for `targetFile` & `alsoExtract`}
#'     \item{`6`}{delete entry for `targetFile`, `alsoExtract` & `archive`}
#'     \item{`7`}{delete entry that is failing (i.e., for the file downloaded by the `url`)}
#'   }
#' will only remove entries in the `CHECKSUMS.txt` that are associated with
#'   `targetFile`, `alsoExtract` or `archive` When `prepInputs` is called,
#'   it will write or append to a (if already exists) `CHECKSUMS.txt` file.
#'   If the `CHECKSUMS.txt` is not correct, use this argument to remove it.
#'
#' @param targetFile Character string giving the path to the eventual file
#'   (raster, shapefile, csv, etc.) after downloading and extracting from a zip
#'   or tar archive. This is the file *before* it is passed to
#'   `postProcess`. Currently, the internal checksumming does not checksum
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
#' @param fun Function, character string, or quoted call with which to load the
#'   `targetFile` or an object created by `dlFun`
#'   into an `R` object. See details and examples below.
#'
#' @param quick Logical. This is passed internally to [Checksums()]
#'   (the quickCheck argument), and to
#'   [Cache()] (the quick argument). This results in faster, though
#'   less robust checking of inputs. See the respective functions.
#'
#' @param purge Logical or Integer. `0/FALSE` (default) keeps existing
#'    `CHECKSUMS.txt` file and
#'    `prepInputs` will write or append to it. `1/TRUE` will deleted the entire
#'    `CHECKSUMS.txt` file. Other options, see details.
#'
#' @param overwrite Logical. Should downloading and all the other actions occur
#'   even if they pass the checksums or the files are all there.
#'
#' @param ... Additional arguments passed to `fun` (i.e,. user supplied),
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
#'   only up to 2 nested levels, which will generally including `postProcess` and
#'   the first level of `*Input` functions, e.g., `cropInputs`, `projectInputs`,
#'   `maskInputs`, but not `fixErrors`.
#'
#' @param .tempPath Optional temporary path for internal file intermediate steps.
#'   Will be cleared on.exit from this function.
#'
#' @inheritParams Cache
#' @author Eliot McIntire, Jean Marchal, and Tati Micheletti
#' @export
#' @return
#' This is an omnibus function that will return an R object that will have resulted from
#' the running of [preProcess()] and [postProcess()] or [postProcessTerra()]. Thus,
#' if it is a GIS object, it may have been cropped, reprojected, "fixed", masked, and
#' written to disk.
#'
#' @importFrom data.table data.table
#' @importFrom digest digest
#' @importFrom methods is
#' @importFrom rlang quo
#' @importFrom utils methods modifyList
#' @include checksums.R download.R postProcess.R
#' @rdname prepInputs
#' @seealso [postProcessTerra()], [downloadFile()], [extractFromArchive()],
#'          [postProcess()].
#' @examples
#' \donttest{
#' data.table::setDTthreads(2)
#' origDir <- getwd()
#' setwd(reproducible::tempdir2()) # use a temporary directory
#' # download a zip file from internet, unzip all files, load as shapefile, Cache the call
#' # First time: don't know all files - prepInputs will guess, if download file is an archive,
#' #   then extract all files, then if there is a .shp, it will load with raster::shapefile
#' dPath <- file.path(tempdir(), "ecozones")
#' shpUrl <- "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip"
#'
#' # Wrapped in a try because this particular url can be flaky
#' shpEcozone <- try(prepInputs(destinationPath = dPath,
#'                          url = shpUrl))
#' if (!is(shpEcozone, "try-error")) {
#'   # Robust to partial file deletions:
#'   unlink(dir(dPath, full.names = TRUE)[1:3])
#'   shpEcozone <- prepInputs(destinationPath = dPath,
#'                            url = shpUrl)
#'   unlink(dPath, recursive = TRUE)
#'
#'   # Once this is done, can be more precise in operational code:
#'   #  specify targetFile, alsoExtract, and fun, wrap with Cache
#'   ecozoneFilename <- file.path(dPath, "ecozones.shp")
#'   ecozoneFiles <- c("ecozones.dbf", "ecozones.prj",
#'                     "ecozones.sbn", "ecozones.sbx", "ecozones.shp", "ecozones.shx")
#'   shpEcozone <- prepInputs(targetFile = ecozoneFilename,
#'                            url = shpUrl,
#'                            alsoExtract = ecozoneFiles,
#'                            fun = "shapefile", destinationPath = dPath)
#'   unlink(dPath, recursive = TRUE)
#'
#'   # Add a study area to Crop and Mask to
#'   # Create a "study area"
#'   library(sp)
#'   library(raster)
#'   coords <- structure(c(-122.98, -116.1, -99.2, -106, -122.98, 59.9, 65.73, 63.58, 54.79, 59.9),
#'                       .Dim = c(5L, 2L))
#'   Sr1 <- Polygon(coords)
#'   Srs1 <- Polygons(list(Sr1), "s1")
#'   StudyArea <- SpatialPolygons(list(Srs1), 1L)
#'   crs(StudyArea) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
#'
#'   #  specify targetFile, alsoExtract, and fun, wrap with Cache
#'   ecozoneFilename <- file.path(dPath, "ecozones.shp")
#'   # Note, you don't need to "alsoExtract" the archive... if the archive is not there, but the
#'   #   targetFile is there, it will not redownload the archive.
#'   ecozoneFiles <- c("ecozones.dbf", "ecozones.prj",
#'                     "ecozones.sbn", "ecozones.sbx", "ecozones.shp", "ecozones.shx")
#'   shpEcozoneSm <- Cache(prepInputs,
#'                         url = shpUrl,
#'                         targetFile = reproducible::asPath(ecozoneFilename),
#'                         alsoExtract = reproducible::asPath(ecozoneFiles),
#'                         studyArea = StudyArea,
#'                         fun = "shapefile", destinationPath = dPath,
#'                         filename2 = "EcozoneFile.shp") # passed to determineFilename
#'
#'   plot(shpEcozone)
#'   plot(shpEcozoneSm, add = TRUE, col = "red")
#'   unlink(dPath)
#'
#'   # Big Raster, with crop and mask to Study Area - no reprojecting (lossy) of raster,
#'   #   but the StudyArea does get reprojected, need to use rasterToMatch
#'   dPath <- file.path(tempdir(), "LCC")
#'   lcc2005Filename <- file.path(dPath, "LCC2005_V1_4a.tif")
#'   url <- file.path("ftp://ftp.ccrs.nrcan.gc.ca/ad/NLCCLandCover",
#'                    "LandcoverCanada2005_250m/LandCoverOfCanada2005_V1_4.zip")
#'
#'   # messages received below may help for filling in more arguments in the subsequent call
#'   # This is in a `try` because the url can be flaky
#'   LCC2005 <- try(prepInputs(url = url,
#'                         destinationPath = asPath(dPath),
#'                         studyArea = StudyArea))
#'   if (!is(LCC2005, "try-error")) {
#'
#'     raster::plot(LCC2005)
#'
#'     # if wrapped with Cache, will be very fast second time (via memoised copy)
#'     LCC2005 <- Cache(prepInputs, url = url,
#'                      targetFile = lcc2005Filename,
#'                      archive = asPath("LandCoverOfCanada2005_V1_4.zip"),
#'                      destinationPath = asPath(dPath),
#'                      studyArea = StudyArea)
#'     # Using dlFun -- a custom download function -- passed to preProcess
#'     test1 <- prepInputs(targetFile = "GADM_2.8_LUX_adm0.rds", # must specify currently
#'                         dlFun = "raster::getData", name = "GADM", country = "LUX", level = 0,
#'                         path = dPath)
#'   }
#'   }
#'   setwd(origDir)
#' }
#'
#' ## Using quoted dlFun and fun -- this is not intended to be run but used as a template
#' ## prepInputs(..., fun = quote(customFun(x = targetFilePath)), customFun = customFun)
#' ##   # or more complex
#' ##  test5 <- prepInputs(
#' ##   targetFile = targetFileLuxRDS,
#' ##   dlFun = quote({
#' ##     getDataFn(name = "GADM", country = "LUX", level = 0) # preProcess keeps file from this!
#' ##   }),
#' ##   fun = quote({
#' ##     out <- readRDS(targetFilePath)
#' ##     out <- as(out, "SpatialPolygonsDataFrame")
#' ##     sf::st_as_sf(out)})
#' ##  )
#'
#'
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
  # Download, Checksum, Extract from Archive
  # browser(expr = exists("._prepInputs_1"))
  if (missing(.tempPath)) {
    .tempPath <- tempdir2(rndstr(1, 6))
    on.exit({
      unlink(.tempPath, recursive = TRUE)
    }, add = TRUE)
  }
  mess <- character(0)

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
  # If it is simple call, then we can extract stuff from the function call; otherwise all bets off
  theFun <- out$fun
  # fun <- if (is(theFun, "call") || is(theFun, "function") && is.null(out$object)) {
  #   fnNameInit <- deparse(substitute(out$fun))
  #   browser()
  #   argsPassingToTheFun <- out[!names(out) %in% c(formalArgs(prepInputs), "checkSums", "dots", "object")]
  #   argsPassingToTheFun
  #   match.call(theFun, as.call(append(list(theFun), argsPassingToTheFun)))
  #               #)
  # } else {
  #   NULL
  # }
  #
  suppressWarnings({
    naFun <- all(is.na(theFun))
  })

  funChar <- if (is.character(out$funChar)) out$funChar else NULL


  out <- modifyList(out, list(...))

  argsFromPrepInputsFamily <- unique(c(.namesPostProcessFormals(), formalArgs(prepInputs), formalArgs(preProcess),
                                "checkSums", "dots", "object"))
  # keep the ones for theFun
  formsForTheFun <- names(formals3(theFun))
  argsFromPrepInputsFamily <- setdiff(argsFromPrepInputsFamily, names(formals3(theFun)))
  argsPassingToTheFun <- out[!names(out) %in% argsFromPrepInputsFamily]
  # args <- argsPassingToTheFun[!names(argsPassingToTheFun) %in% "targetFilePath"] # will replace it without a named arg
  args <- argsPassingToTheFun[names(argsPassingToTheFun) %in% formsForTheFun]


  otherFiles <- out$checkSums[result == "OK"]
  .cacheExtra <- NULL
  if (NROW(otherFiles)) {
    .cacheExtra <- .robustDigest(otherFiles$checksum.x)
  }
  if (!(naFun || is.null(theFun))) {
    x <- if (is.null(out$object)) {

      messagePrepInputs("Loading object into R", verbose = verbose)
      if (identical(theFun, raster::raster) |
          identical(theFun, raster::stack) |
          identical(theFun, raster::brick) |
          identical(theFun, terra::rast)) {
        ## Don't cache the reading of a raster
        ## -- normal reading of raster on disk is fast b/c only reads metadata
        do.call(theFun, append(list(asPath(out$targetFilePath)), args))
      } else {
        if (identical(theFun, base::load)) {
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
          args2 <- append(list(file = out$targetFilePath, envir = tmpEnv), args)
          objs <- do.call(theFun, args2)
          if (returnAsList)
            as.list(tmpEnv, all.names = TRUE)
        } else {
          # browser(expr = exists("._prepInputs_3"))
          #err <- tryCatch(error = function(xx) xx,
          useCache2 <- useCache
          if (fileExt(out$targetFilePath) %in% c("qs", "rds") &&
              !isTRUE(getOption("reproducible.useMemoise"))) {
            useCache2 <- FALSE
            messagePrepInputs("targetFile is already a binary; skipping Cache while loading")
          }
          withCallingHandlers(
            if (is.call(theFun)) {
              # put `targetFilePath` in the first position -- allows quoted call to use first arg
              out <- append(append(list(targetFilePath = out[["targetFilePath"]]),
                                   out[-which(names(out) == "targetFilePath")]),
                            args)
              if (length(fun[["functionName"]]) == 1)
                out[[fun[["functionName"]]]] <- fun$FUN
              # obj <- Cache(eval, theFun, envir = out, useCache = useCache2, .cacheExtra = .cacheExtra,
              obj <- Cache(eval(theFun, envir = out), useCache = useCache2, .cacheExtra = .cacheExtra,
                           .functionName = funChar)
            } else {
              args2 <- append(list(asPath(out$targetFilePath)), args)
              obj <- Cache(do.call, theFun, args2, useCache = useCache2, .cacheExtra = .cacheExtra,
                           .functionName = funChar)
            }, message = function(m) {
              m$message <- grep("No cachePath supplied|useCache is FALSE", m$message, invert = TRUE, value = TRUE)
              if (length(m$message)) {
                mm <- gsub("(.*)\n$", "\\1", m$message)
                messagePrepInputs(mm)
              }
              tryInvokeRestart("muffleMessage")
            })
          obj
        }
      }
    } else {
      if (is.null(fun)) {
        out$object
      } else {
        x <- out$object
        env1 <- new.env()
        list2env(list(...), envir = env1)
        eval(theFun, envir = env1)
      }
    }
  } else {
    messagePrepInputs("No loading of object into R; fun = ", theFun, verbose = verbose)
    x <- out
  }

  if (requireNamespace("terra", quietly = TRUE) && getOption("reproducible.useTerra", FALSE)) {
    if (!(all(is.null(out$dots$studyArea),
              is.null(out$dots$rasterToMatch),
              is.null(out$dots$targetCRS))) || !(all(is.null(out$dots$to)))) {

      # This sequence puts all the objects that are needed for postProcessTerra into this environment
      #   so that we can avoid using do.call
      argsPostProcessTerra <- formalArgs(postProcessTerra)
      argsOldPostProcess <- c("rasterToMatch", "studyArea", "targetCRS", "useSAcrs", "filename2",
                              "overwrite")
      envHere <- environment()
      argsHere <- union(argsPostProcessTerra, argsOldPostProcess)
      argsHere <- setdiff(argsHere, "...")
      for (ar in argsHere) {
        # print(ar)
        if (!exists(ar, envir = envHere, inherits = FALSE)) {
          assign(ar, out$dots[[ar]], envHere)
        }
      }
      if (is.null(filename2)) {
        writeTo <- determineFilename(destinationPath = destinationPath, filename2 = writeTo, verbose = verbose)
      } else {
        filename2 <- determineFilename(destinationPath = destinationPath, filename2 = filename2, verbose = verbose)
      }
      # pass everything, including NULL where it was NULL. This means don't have to deal with
      #    rlang quo issues
      TopoErrors <- list() # eventually to update a Google ID #TODO
      x <- withCallingHandlers(
        postProcessTerra(from = x, to = to, rasterToMatch = rasterToMatch, studyArea = studyArea,
                              cropTo = cropTo, projectTo = projectTo, maskTo = maskTo, writeTo = writeTo,
                              method = method, targetCRS = targetCRS, useSAcrs = useSAcrs,
                              datatype = datatype,
                              filename2 = filename2,
                              overwrite = overwrite),
        message = function(m) {
          hasTopoExcError <- grepl("TopologyException: Input geom 0 is invalid", m$message)
          if (any(hasTopoExcError)) {
            TopoErrors <<- append(TopoErrors, list(m$message))
          }
        }
      )


    }
  } else {

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

      fullList <- modifyList(list(x = xquo, filename1 = out$targetFilePath,
                                  overwrite = overwrite,
                                  destinationPath = out$destinationPath,
                                  useCache = useCache,
                                  verbose = verbose), # passed into postProcess
                             out$dots)
      x <- Cache(
        do.call, postProcess, fullList,
        useCache = useCache, verbose = verbose # used here
      )
    }

  }


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
  # if (!is.null(fun) && !is.character(fun)) {
  #   stop("fun must be a character string, not the function")
  # }
  possibleFiles <- unique(.basename(c(targetFilePath, filesExtracted)))
  whichPossFile <- possibleFiles %in% basename2(targetFilePath)
  if (isTRUE(any(whichPossFile)))
    possibleFiles <- possibleFiles[whichPossFile]
  isShapefile <- FALSE
  isRaster <- FALSE
  isRDS <- FALSE
  fileExt <- fileExt(possibleFiles)
  feKnown <- .fileExtsKnown() # An object in helpers.R
  funPoss <- lapply(fileExt, function(fe) feKnown[startsWith(prefix = feKnown[[1]], fe), ])
  funPoss <- do.call(rbind, funPoss)
  if (length(funPoss)) {
    isShapefile <- fileExt %in% funPoss[funPoss[, "type"] == "shapefile", "extension"]
    isRaster <- fileExt %in% funPoss[funPoss[, "type"] == "Raster", "extension"]
    isRDS <- fileExt %in% funPoss[funPoss[, "extension"] == "rds", "extension"]
    if (any(isShapefile)) {
      if (is.null(fun))
        if (requireNamespace("sf", quietly = TRUE) ) {
          if (!isTRUE(grepl("st_read", fun)))
            messagePrepInputs("Using sf::st_read on shapefile because sf package is available; to force old ",
                              "behaviour with 'raster::shapefile' use fun = 'raster::shapefile' or ",
                              "options('reproducible.shapefileRead' = 'raster::shapefile')")
        }
    }
  }
  if (is.null(fun)) {
    fun <- unique(funPoss[, "fun"])
    if (length(fun) > 1) {
      if (sum(isRaster) > 0 && sum(isShapefile) > 0) {
        isRaster[isRaster] <- FALSE
        funPoss <- funPoss[funPoss$type == "shapefile", ]
        fun <- unique(funPoss[, "fun"])
        message("The archive has both a shapefile and a raster; selecting the shapefile. If this is incorrect, specify targetFile")
      } else
        stop("more than one file; can't guess at function to load with; ",
             "please supply 'fun' or 'targetFile' argument to reduce ambiguity")
    }
    if (length(fun) == 0) stop("Can't guess at which function to use to read in the object; please supply 'fun'")
  }
  if (is.null(targetFilePath)) {
    secondPartOfMess <- if (any(isShapefile)) {
      c(" Trying ",fun," on ", paste(possibleFiles[isShapefile], collapse = ", "), ".",
        " If that is not correct, please specify a different targetFile",
        " and/or fun.")
    } else if (is.null(fun)) {
      c(" Also, file extension does not unambiguously specify how it should be loaded. Please specify fun.")
    } else {
      c(" Trying ", fun, ".\n",
        " If that is not correct, please specify a targetFile",
        " and/or different fun. The current files in the targetFilePath's",
        " directory are: \n",
        paste(possibleFiles, collapse = ", "))
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
  out <- NULL
  if (!(is.null(archive))) {
    if (!is.na(archive)) {
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
    }
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
    if (grepl(x = extractSystemCallPath, pattern = "7z")) {
      prependPath <- if (isWindows()) {
        paste0("\"", extractSystemCallPath, "\"")
      } else {
        extractSystemCallPath
      }

      # This spits out a message on non-Windows about arguments that are ignored
      suppressMessages({
        output <- system(paste0(prependPath, " e -aoa -o\"", .tempPath, "\" \"", args[[1]], "\""),
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
          invisible(.file.move(from = file.path(.tempPath, fileToMove),
                              to = file.path(args$exdir, basename(fileToMove)),
                              overwrite = overwrite))
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
    tooBig <- file.size(args[[1]]) > 2e9
    worked <- FALSE
    if (isUnzip && !tooBig) {
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
      unz <- Sys.which("unzip")
      sZip <- Sys.which("7z")

      if (!isTRUE(tooBig)) {
        messagePrepInputs("File unzipping using R does not appear to have worked.",
                          " Trying a system call of unzip...", verbose = verbose)
      } else {
        messPart1 <- "R's unzip utility cannot handle a zip file this size.\n"
        if (nchar(sZip) > 0) {
          messagePrepInputs(messPart1, verbose = verbose)
        } else {
          messagePrepInputs(
            paste(messPart1,
                  "Install 7zip and add it to your PATH (see https://www.7-zip.org/)."),
            verbose = verbose
          )
        }
      }

      if (file.exists(args[[1]])) {
        pathToFile <-  normPath(args[[1]])
      } else {
        if (file.exists(file.path(args$exdir, args[[1]]))) {
          pathToFile <-  normPath(file.path(args$exdir, args[[1]]))
        } else {
          warning(mess)
          stop("prepInputs cannot find the file ", basename(args[[1]]), ".",
               " The file might have been moved during unzipping or is corrupted.")
        }
      }
      if (nchar(sZip) > 0) {
        messagePrepInputs("Using '7zip'")
        op <- setwd(.tempPath)
        on.exit({
          setwd(op)
        }, add = TRUE)
        lstFiles <- system(paste0(sZip, " l ", pathToFile), intern = TRUE, wait = TRUE)
        startAndEnd <- grep("-----------", lstFiles)
        if (diff(startAndEnd) > 1) {
          lstFiles <- lstFiles[(startAndEnd[1]+1) : (startAndEnd[2]-1)]
        }
        if (length(files)) {
          filesAreInArch <- unlist(lapply(files, function(x) any(grepl(x, lstFiles))))
          if (all(filesAreInArch))
            arg22 <- paste("e", pathToFile, paste(files, collapse = " "))
          else
            stop(paste(files, collapse = ", "), " not in ", basename2(pathToFile))
        } else {
          arg22 <- paste0(" e ", pathToFile)
        }
        system2(sZip,
                args = arg22,
                wait = TRUE,
                stdout = NULL)

      } else if (nchar(unz) > 0) {
        messagePrepInputs("Using 'unzip'")
        system2(unz,
                args = paste0(pathToFile," -d ", .tempPath),
                wait = TRUE,
                stdout = NULL)
      } else {
        if (nchar(unz) == 0) {
          stop("unzip command cannot be found.",
               " Please try reinstalling Rtools if on Windows, and/or add unzip to system path",
               " (e.g., see 'https://cran.r-project.org/bin/windows/Rtools/'.)")
        }
        stop("There was no way to unzip all files; try manually. The file is located at: \n",
             pathToFile)
      }
      extractedFiles <- list.files(path = .tempPath,
                                   # list of full paths of all extracted files!
                                   recursive = TRUE,
                                   include.dirs = TRUE)
      from <- file.path(.tempPath, extractedFiles)
      on.exit({
        if (any(file.exists(from)))
          suppressWarnings(try(unlink(from), silent = TRUE))
      }, add = TRUE)
      to <- file.path(args$exdir, extractedFiles)

      suppressWarnings({
        out <- try(file.link(from, to))
      })

      if (!isTRUE(all(out))) {
        out <- try(.file.move(from, to, overwrite))
      }

      if (!isTRUE(all(out))) {
        stop(paste("Could not move extractedfiles from", .tempPath, "to", args$exdir))
      }
      extractedFiles <- to
      unlink(.tempPath, recursive = TRUE)
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
      # if (requireNamespace("dplyr", quietly = TRUE)) {
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
  capture.output(type = "message", {
    currentFiles <- Checksums(path = destinationPath, write = TRUE, #write = !append || NROW(nonCurrentFiles) == 0,
                              files = file.path(destinationPath, filesToChecksum),
                              checksumFile = csf,
                              verbose = verbose)
  })
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
#'        if the file extension is `.shp` it will output the names of files with
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
  needSystemCall <- (length(archive) > 0 && fileExt(archive[1]) %in% knownSystemArchiveExtensions )
  if (length(archive) > 0)
    if (file.exists(archive[1]))
      needSystemCall <- needSystemCall || file.size(archive[1]) > 2e9

  if (needSystemCall) {
    extractSystemCallPath <- .testForArchiveExtract()
    funWArgs <- list(fun = extractSystemCallPath)
  } else {
    funWArgs <- .whichExtractFn(archive[1], NULL)
  }

  filesInArchive <- NULL
  if (!is.null(funWArgs$fun)) {
    if (file.exists(archive[1])) {
      if (!needSystemCall) {
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
        filesLines <- filesOutput[(min(filesInBetween) + 1):(max(filesInBetween) - 1)]
        filesInArchive <- unlist(lapply(X = seq_along(filesLines), FUN = function(line){
          fullString <- unlist(strsplit(filesLines[[line]], split = " "))
          return(fullString[length(fullString)])
        })
        )
        if (length(filesInArchive) == 0) {
          stop("preProcess could not find any files in the archive ", archive)
        }
      }
    }
  }
  return(filesInArchive)
}

.compareChecksumsAndFiles <- function(checkSums, files) {
  isOK <- NULL
  if (!is.null(files)) {
    checkSumsDT <- data.table(checkSums)
    filesDT <- data.table(files = .basename(files))
    isOKDT <- checkSumsDT[filesDT, on = c(expectedFile = "files")]
    isOKDT2 <- checkSumsDT[filesDT, on = c(actualFile = "files")]
    # fill in any OKs from "actualFile" intot he isOKDT
    isOKDT[compareNA(isOKDT2$result, "OK"), "result"] <- "OK"
    isOK <- compareNA(isOKDT$result, "OK")
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
    possPrograms <- c("7z", "unrar") %>%
      lapply(., Sys.which) %>%
      unlist() %>%
      unique()
    extractSystemCallPath <- if (!all(possPrograms == "")) {
      possPrograms[nzchar(possPrograms)][1] # take first one if there are more than one
    } else {
      ""
    }
    if (!(isWindows())) { ## TODO: macOS ?? #266
      if (grepl("7z", extractSystemCallPath)) {
        SevenZrarExists <- system("apt -qq list p7zip-rar", intern = TRUE, ignore.stderr = TRUE) %>%
          grepl("installed", .)
        if (isFALSE(SevenZrarExists))
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

