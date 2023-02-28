
#' A set of helpers for Cache
#'
#' These are internal only.
#'
#' @param FUN A function
#' @param ... passing the `...` from outer function, which will include potential
#'        arguments to the `FUN`
#' @param overrideCall A character string indicating a different (not "Cache") function
#'        name to search for. Mostly so that this works with deprecated "cache".
#' @note If the function cannot figure out a clean function name, it returns "internal"
#'
#' @author Eliot McIntire
#' @importFrom methods selectMethod showMethods
#' @importFrom utils head
#' @keywords internal
getFunctionName <- function(FUN, ..., overrideCall) { # nolint
  callIndex <- numeric()
  scalls <- sys.calls() # needed for nesting level
  functionName <- NULL # initiate this here, so we can know if it is found
  if (is(FUN, "list")) {
    functionName <- names(FUN)
    FUN <- FUN[[1]]
  }
  if (isS4(FUN)) {
    # Have to extract the correct dispatched method
    firstElems <- strsplit(showMethods(FUN, inherited = TRUE, printTo = FALSE), split = ", ")
    firstElems <- lapply(firstElems, function(x) {
      y <- strsplit(x, split = "=")
      unlist(lapply(y, function(z) z[1]))
    })
    firstElems <- firstElems[!unlist(lapply(firstElems, is.null))] # remove nulls
    firstElems <- firstElems[!unlist(lapply(firstElems, function(x) {
      any(grepl(x, pattern = "inherited")) # remove "nulls" inherited
    }))]
    firstElems <- firstElems[!unlist(lapply(firstElems, function(x) {
      any(grepl(x, pattern = "\\(inherited")) # remove "nulls" inherited
    }))]
    firstElems <- firstElems[!unlist(lapply(firstElems, function(x) {
      any(grepl(x, pattern = "^Function:"))  # remove "nulls" inherited
    }))]

    sigArgs <- lapply(unique(firstElems), function(x) {
      FUN@signature %in% x
    })
    signat <- unlist(sigArgs[unlist(lapply(sigArgs, function(y) any(y)))])

    matchedCall <- as.list(
      match.call(FUN, call(name = FUN@generic, list(...)))
    )

    matchedCall <- matchedCall[nzchar(names(matchedCall))]
    ff <- match(names(matchedCall), FUN@signature[signat])
    matchedCall <- matchedCall[ff[!is.na(ff)]]
    matchedCall <- lapply(matchedCall, eval)

    signatures <- rep("missing", (sum(signat))) # default is "missing"
    names(signatures) <- FUN@signature[signat]
    classMatchedCall <- sapply(matchedCall, class)

    # update "missing" with ones that aren't missing
    signatures[names(classMatchedCall)] <- classMatchedCall

    ## TO DO: need to get the method the dispatch correct
    methodUsed <- selectMethod(FUN, optional = TRUE, signature = signatures)
    functionName <- FUN@generic
    FUN <- methodUsed@.Data  # nolint
  } else {
    if (is.null(functionName)) {
      if (!missing(overrideCall)) {
        callIndices <- .grepSysCalls(scalls, pattern = paste0("^", overrideCall))
        functionCall <- scalls[callIndices]
      } else {
        callIndices <- .grepSysCalls(scalls,
                                     pattern = "^Cache|^SpaDES::Cache|^reproducible::Cache|^cloudCache")
        callIndicesDoCall <- .grepSysCalls(scalls, pattern = "^do.call")
        doCall1st2Elements <- lapply(scalls[callIndicesDoCall], function(x) x[1:2])
        callIndicesDoCall <- callIndicesDoCall[grep("Cache", doCall1st2Elements)]
        # The next line takes too long to grep if scalls has enormous objects
        # callIndices <- grep(scalls, pattern = "^Cache|^SpaDES::Cache|^reproducible::Cache")
        callIndices <- unique(sort(c(callIndices, callIndicesDoCall)))
        functionCall <- scalls[callIndices]
      }
      if (length(functionCall)) {
        # for() loop is a work around for R-devel that produces a different final call in the
        # sys.calls() stack which is NOT .Method ... and produces a Cache(FUN = FUN...)
        for (callIndex in head(rev(callIndices), 2)) {
          if (!missing(overrideCall)) {
            env <- sys.frames()[[callIndex]]
            matchedCall <- match.call(get(overrideCall, envir = env), scalls[[callIndex]])#parse(text = callIndex))
            forms <- tryCatch("FUN" %in% formalArgs(overrideCall), error = function(x) NULL)
            if (!is.null(forms)) {
              functionName <- matchedCall$FUN
            } else {
              functionName <- matchedCall[[2]]
            }
          } else {
            foundCall <- FALSE
            if (exists("callIndicesDoCall", inherits = FALSE))
              if (length(callIndicesDoCall) > 0) {
                if (callIndex %in% callIndicesDoCall) {
                  mcDoCall <- match.call(do.call, scalls[[callIndex]])
                  for (i in 1:2) {
                    fnLookup <- try(eval(mcDoCall$args, envir = sys.frames()[[callIndex - i]]), silent = TRUE)
                    if (!is(fnLookup, "try-error"))
                      break
                  }
                  functionName <- if (isTRUE("FUN" %in% names(fnLookup)))
                    fnLookup$FUN
                  else
                    fnLookup[[1]]

                  foundCall <- TRUE
                }
              }
            if (!foundCall) {
              matchedCall <- try(match.call(Cache, scalls[[callIndex]]), silent = TRUE)#parse(text = callIndex))
              functionName <- if (!is(matchedCall, "try-error"))
                matchedCall$FUN
              else
                ""
            }
          }
          functionName <- if (is(functionName, "name")) {
            deparse(functionName, width.cutoff = 300)
          } else {
            "FUN"
          }

          if (all(functionName != c("FUN")) && all(functionName != c("NULL"))) break
        }
      } else {
        functionName <- ""
      }
    }
    .FUN <- FUN  # nolint
  }

  if (is(FUN, "function")) {
    .FUN <- format(FUN)  # nolint
  } else {
    .FUN <- NULL # nolint
  }

  # if it can't deduce clean name (i.e., still has a "(" in it), return NA
  if (isTRUE(grepl(functionName, pattern = "\\(")))
    functionName <- NA_character_

  nestLevel <- length(grep(lapply(scalls, function(x) x[1:2]),
                           pattern = "^Cache"))/2

  return(list(functionName = functionName, .FUN = .FUN, nestLevel = nestLevel - 1))#, callIndex = callIndex))
}

#' @exportClass Path
#' @rdname Path-class
setClass("Path", slots = c(.Data = "character"), contains = "character", prototype = NA_character_)

#' Coerce a character string to a class "Path"
#'
#' Allows a user to specify that their character string is indeed a filepath.
#' Thus, methods that require only a filepath can be dispatched correctly.
#'
#' It is often difficult or impossible to know algorithmically whether a
#' character string corresponds to a valid filepath.
#' In the case where it is en existing file, `file.exists` can work.
#' But if it does not yet exist, e.g., for a `save`, it is difficult to know
#' whether it is a valid path before attempting to save to the path.
#'
#' This function can be used to remove any ambiguity about whether a character
#' string is a path. It is primarily useful for achieving repeatability with Caching.
#' Essentially, when Caching, arguments that are character strings should generally be
#' digested verbatim, i.e., it must be an exact copy for the Cache mechanism
#' to detect a candidate for recovery from the cache.
#' Paths, are different. While they are character strings, there are many ways to
#' write the same path. Examples of identical meaning, but different character strings are:
#' path expanding of `~` vs. not, double back slash vs. single forward slash,
#' relative path vs. absolute path.
#' All of these should be assessed for their actual file or directory location,
#' NOT their character string. By converting all character string that are actual
#' file or directory paths with this function, then `Cache` will correctly assess
#' the location, NOT the character string representation.
#'
#' @param obj A character string to convert to a `Path`.
#' @param nParentDirs A numeric indicating the number of parent directories starting
#'                    from basename(obj) = 0 to keep for the digest
#'
#' @export
#' @rdname Path-class
#' @return
#' A vector of class `Path`, which is similar to a character, but
#' has an attribute indicating how deep the Path should be
#' considered "digestible". In other words, most of the time, only some
#' component of an absolute path is relevant for evaluating its purpose in
#' a Cache situation. In general, this is usually equivalent to just the "relative" path
#'
#' @examples
#' tmpf <- tempfile(fileext = ".csv")
#' file.exists(tmpf)    ## FALSE
#' tmpfPath <- asPath(tmpf)
#' is(tmpf, "Path")     ## FALSE
#' is(tmpfPath, "Path") ## TRUE
#'
asPath <- function(obj, nParentDirs = 0) {
  UseMethod("asPath", obj)
}

#' @export
#' @importFrom methods is
#' @rdname Path-class
asPath.character <- function(obj, nParentDirs = 0) {  # nolint
  class(obj) <- c("Path", is(obj))
  attr(obj, "nParentDirs") <- nParentDirs
  return(obj)
}

#' @export
#' @rdname Path-class
asPath.null <- function(obj, nParentDirs = 0) {  # nolint
  return(NULL)
}

#' If using `as("string", "Path")`, there is no option to pass `nParentDirs`.
#' So, using `asPath` directly (e.g., `asPath("string", 0))`) is preferred.
#' @export
#' @importFrom methods new
#' @rdname Path-class
#' @name asPath
setAs(from = "character", to = "Path", function(from) {
  asPath(from, 0)
})

# Old one
.prepareFileBackedRaster2 <- function(obj, repoDir = NULL, overwrite = FALSE,
                                      drv = getOption("reproducible.drv", RSQLite::SQLite()),
                                      conn = getOption("reproducible.conn", NULL),
                                      ...) {
  isRasterLayer <- TRUE
  isBrick <- is(obj, "RasterBrick")
  isStack <- is(obj, "RasterStack")
  repoDir <- checkPath(repoDir, create = TRUE)
  isRepo <- CacheIsACache(cachePath = repoDir, drv = drv, conn = conn)

  ## check which files are backed
  numFiles <- sum(nchar(unique(Filenames(obj)))>0)
  allInOneFile <- allInOneFile(obj)
  # if (isStack && numFiles > 0) {
  #   innerFilenames <- unlist(lapply(obj@layers, filename))
  #   allInOneFile <- isTRUE(sum(nchar(innerFilenames)>0) == length(obj@layers))
  # }

  whichInMemory <- if (!isStack) {
    im <- inMemory(obj)
    if (isBrick) {
      if (isTRUE(im))
        im <- rep(im, raster::nlayers(obj))
    }
    im
  } else {
    sapply(obj@layers, inMemory)
  }
  whichHasValues <- if (!isStack) {
    hasValues(obj)
  } else {
    sapply(obj@layers, hasValues)
  }

  isFilebacked <- !(whichInMemory | !whichHasValues)
  # if (isStack && any(isFilebacked)) {
  #   if (allInOneFile) {
  #      isFilebacked <- rep(TRUE, numFiles)
  #   }
  # }

  ## create a storage vector of file names to be filled
  curFilename <- if (isBrick) {
    ""
  } else {
    rep("", length(isFilebacked))
  }

  if (any(!isFilebacked)) {
    fileExt <- if (!isStack) {
      raster::is.factor(obj)
    } else {
      sapply(obj@layers, raster::is.factor)
    }

    fileExt <- ifelse(fileExt, ".grd", ".tif")
    tempName <- basename(tempfile(pattern = "raster", fileext = fileExt, tmpdir = ""))
    curFilename[!isFilebacked] <- tempName[!isFilebacked]
  }
  if (any(isFilebacked)) {
    if (isTRUE(allInOneFile)) {
      curFiles <- normPath(Filenames(obj, allowMultiple = FALSE))
    } else {
      curFiles <- normPath(Filenames(obj, allowMultiple = FALSE)[isFilebacked])
    }
    curFilename[isFilebacked] <- curFiles
    # if (is(obj, "RasterLayer") || is(obj, "RasterBrick") ||
    #     (is(obj, "RasterStack") && numFiles == 1)) {
    #   curFilename <- normalizePath(Filenames(obj), winslash = "/", mustWork = FALSE)
    # } else  {
    #   curFilenames <- unlist(lapply(obj@layers, function(x)
    #     normalizePath(filename(x), winslash = "/", mustWork = FALSE)))
    #   curFilename[isFilebacked] <- curFilenames[isFilebacked]
    # }
  }

  ## check for files that should've been backed, but don't exist
  if (any(!file.exists(curFilename) & isFilebacked & isRasterLayer)) {
    badFileNames <- curFilename[!file.exists(curFilename) & isFilebacked]

    # File is in wrong folder, usually the result of a copy of cache between 2 machines
    splittedFilenames <- strsplit(badFileNames, split = basename(repoDir))
    trySaveFilename <- if (length(splittedFilenames) == 1) {
      normalizePath(
        file.path(repoDir, splittedFilenames[[1]][[length(splittedFilenames[[1]])]]),
        winslash = "/", mustWork = FALSE)
    } else {
      splittedFilenames2 <- lapply(splittedFilenames, function(x) {
        ifelse(length(x), x[length(x)], "")
      })

      normalizePath(file.path(repoDir, splittedFilenames2), winslash = "/", mustWork = FALSE)
    }
    if (any(!file.exists(trySaveFilename))) {
      stop("The following file-backed rasters are supposed to be on disk ",
           "but appear to have been deleted:\n",
           paste("    ", badFileNames, collapse = "\n"),
           ". The most likely reason is that two functions had the same output ",
           "and one of them was removed with clearCache(...). ",
           "The best solution to this is never have two functions create the same ",
           "file-backed raster.")
    } else {
      if (!isStack) {
        slot(slot(obj, "file"), "name") <- saveFilename <- curFilename <- trySaveFilename
      } else {
        curFilename[!file.exists(curFilename) & isFilebacked] <- trySaveFilename
        saveFilename <- curFilename
        for (i in seq_len(nlayers(obj))) {
          slot(slot(obj@layers[[i]], "file"), "name") <- saveFilename[i]
        }
      }
    }
  } else {
    saveFilename <- if (isRepo) {
      file.path(repoDir, "rasters", basename(curFilename))
    } else {
      file.path(repoDir, basename(curFilename))
    }
    saveFilename <- normPath(saveFilename)
  }

  sameFilenames <- saveFilename == curFilename
  if (any(sameFilenames)) {
    if (!overwrite) {
      saveFilename[sameFilenames] <- unlist(lapply(seq_along(curFilename[sameFilenames]),
                                                   function(x) {
                                                     if (file.exists(saveFilename[x])) {
                                                       nextNumericName(saveFilename[x])
                                                     } else {
                                                       saveFilename[x]
                                                     }
                                                   }))
    }
  }
  # filenames are not the same, check if backed, act accordingly

  if (any(saveFilename != curFilename)) {
    notSameButBacked <- saveFilename != curFilename & isFilebacked

    if (any(notSameButBacked)) {
      ## deal only with files that have been backed
      saveFilename2 <- saveFilename[notSameButBacked]
      curFilename2 <- curFilename[notSameButBacked]

      pathExists <- dir.exists(dirname(saveFilename2))
      if (any(!pathExists)) {
        dirname(saveFilename2) %>%
          unique() %>%
          sapply(., dir.create, recursive = TRUE)
      }

      saveFilename2 <- sapply(seq_along(curFilename2), function(x) {
        curFilenameBase <- filePathSansExt(curFilename2[x])
        curFilename <- dir(dirname(curFilename2[x]), pattern = paste0(basename(curFilenameBase), "\\."),
                           full.names = TRUE)
        exts <- fileExt(curFilename)
        saveFilenamesBase <- filePathSansExt(saveFilename2[x])
        saveFilenames <- paste0(saveFilenamesBase, ".", exts)


        # browser(expr = exists("._prepareFileBackedRaster_2"))
        # change filename if it already exists
        if (any(file.exists(saveFilenames))) {
          saveFilenames <- nextNumericName(saveFilenames)
        }
        out <- hardLinkOrCopy(from = curFilename, to = saveFilenames, overwrite = TRUE)
        # if (FALSE) {
        #   outFL <- suppressWarningsSpecific(file.link(to = saveFilenames,
        #                                               from = curFilename),
        #                                     falseWarnings = "already exists|Invalid cross-device")
        #   if (any(!outFL)) {
        #     copyFile(to = saveFilenames[!outFL],
        #              overwrite = TRUE,
        #              from = curFilename[!outFL], silent = TRUE)
        #
        #   }
        # }
        saveFilenames[match(fileExt(curFilename2[x]), fileExt(saveFilenames))]
      })

      # for a stack with independent Raster Layers (each with own file)
      obj <- updateFilenameSlots2(obj, curFilename2, saveFilename2, isStack)
      # if (length(curFilename2) > 1) {
      #   for (i in seq_along(curFilename2)) {
      #     slot(slot(slot(obj, "layers")[[i]], "file"), "name") <- saveFilename2[i]
      #   }
      # } else {
      #   if (!isStack) {
      #     slot(slot(obj, "file"), "name") <- saveFilename2
      #   } else {
      #     for (i in seq_len(nlayers(obj))) {
      #       whFilename <- match(basename(saveFilename2), basename(curFilename2))
      #       slot(slot(obj@layers[[i]], "file"), "name") <- saveFilename2[whFilename]
      #     }
      #   }
      # }

      ## update saveFilename
      saveFilename[notSameButBacked] <- saveFilename2[notSameButBacked]
    }
    if (any(!notSameButBacked)) {
      ## deal with files that haven't been backed
      checkPath(unique(dirname(saveFilename[!notSameButBacked])), create = TRUE)
      if (any(!whichInMemory[!notSameButBacked])) {
        if (!isStack) {
          obj <- writeRaster(obj, filename = saveFilename[!notSameButBacked], datatype = dataType(obj))
        } else {
          for (i in which(!notSameButBacked)) {
            obj@layers[[i]] <- writeRaster(obj@layers[[i]], filename = saveFilename[i], datatype = dataType(obj@layers[[i]]))
          }
        }
      }
    }
  }
  return(obj)
}

#' Copy a file using `robocopy` on Windows and `rsync` on Linux/macOS
#'
#' This is replacement for `file.copy`, but for one file at a time.
#' The additional feature is that it will use `robocopy` (on Windows) or
#' `rsync` on Linux or Mac, if they exist.
#' It will default back to `file.copy` if none of these exists.
#' If there is a possibility that the file already exists, then this function
#' should be very fast as it will do "update only", i.e., nothing.
#'
#' @param from The source file.
#'
#' @param to The new file.
#'
#' @param useRobocopy For Windows, this will use a system call to `robocopy`
#'        which appears to be much faster than the internal `file.copy` function.
#'        Uses `/MIR` flag. Default `TRUE`.
#'
#' @param overwrite Passed to `file.copy`
#'
#' @param delDestination Logical, whether the destination should have any files deleted,
#' if they don't exist in the source. This is `/purge` for robocopy and --delete for
#' rsync.
#'
#' @param create Passed to `checkPath`.
#'
#' @param silent Should a progress be printed.
#'
#' @author Eliot McIntire and Alex Chubaty
#' @rdname copyFile
#' @return
#' This function is called for its side effect, i.e., a file is copied `from` to `to`.
#'
#' @examples
#' tmpDirFrom <- file.path(tempdir(), "example_fileCopy_from")
#' tmpDirTo <- file.path(tempdir(), "example_fileCopy_to")
#' tmpFile1 <- tempfile("file1", tmpDirFrom, ".csv")
#' tmpFile2 <- tempfile("file2", tmpDirFrom, ".csv")
#' dir.create(tmpDirFrom, recursive = TRUE, showWarnings = FALSE)
#' f1 <- normalizePath(tmpFile1, mustWork = FALSE)
#' f2 <- normalizePath(tmpFile2, mustWork = FALSE)
#' t1 <- normalizePath(file.path(tmpDirTo, basename(tmpFile1)), mustWork = FALSE)
#' t2 <- normalizePath(file.path(tmpDirTo, basename(tmpFile2)), mustWork = FALSE)
#'
#' write.csv(data.frame(a = 1:10, b = runif(10), c = letters[1:10]), f1)
#' write.csv(data.frame(c = 11:20, d = runif(10), e = letters[11:20]), f2)
#' copyFile(c(f1, f2), c(t1, t2))
#' file.exists(t1) ## TRUE
#' file.exists(t2) ## TRUE
#' identical(read.csv(f1), read.csv(f2)) ## FALSE
#' identical(read.csv(f1), read.csv(t1)) ## TRUE
#' identical(read.csv(f2), read.csv(t2)) ## TRUE
#'
#' unlink(tmpDirFrom, recursive = TRUE)
#' unlink(tmpDirTo, recursive = TRUE)
#'
copySingleFile <- function(from = NULL, to = NULL, useRobocopy = TRUE,
                           overwrite = TRUE, delDestination = FALSE,
                           #copyRasterFile = TRUE, clearRepo = TRUE,
                           create = TRUE, silent = FALSE) {
  if (any(length(from) != 1, length(to) != 1)) stop("from and to must each be length 1")
  useFileCopy <- identical(dirname(from), dirname(to))

  lapply(unique(dirname(to)), checkPath, create = create)

  os <- tolower(Sys.info()[["sysname"]])
  .onLinux <- .Platform$OS.type == "unix" && unname(os) == "linux"
  if (!useFileCopy) {
    if (isWindows() && isTRUE(file.size(from) > 1e6)) {
      if (!isTRUE(unique(dir.exists(to)))) toDir <- dirname(to) # extract just the directory part
      robocopyBin <- tryCatch(Sys.which("robocopy"), warning = function(w) NA_character_)

      robocopy <-  if (silent) {
        paste0(robocopyBin, " /purge"[delDestination], " /ETA /XJ /XO /NDL /NFL /NJH /NJS \"",  # nolint
               normalizePath(dirname(from), mustWork = TRUE, winslash = "\\"), "\" \"",
               normalizePath(toDir, mustWork = FALSE, winslash = "\\"),  "\" ",
               basename(from))
      } else {
        paste0(robocopyBin, " /purge"[delDestination], " /ETA /XJ /XO \"", # nolint
               normalizePath(dirname(from), mustWork = TRUE, winslash = "\\"), "\" \"",
               normalizePath(toDir, mustWork = FALSE, winslash = "\\"), "\" ",
               basename(from))
      }

      useFileCopy <- if (useRobocopy && !is.na(robocopyBin)) {
        suppressWarnings(tryCatch(system(robocopy, intern = TRUE), error = function(x) TRUE))
      } else {
        TRUE
      }
      if (isTRUE(any(grepl("ERROR", useFileCopy)))) {
        useFileCopy <- TRUE
      }

      # means that the file didn't get copied because it is actually the same directory
      if (any(!nzchar(useFileCopy))) {
        useFileCopy <- TRUE
      }
    } else if ( (.onLinux) ) { # nolint
      if (!identical(basename(from), basename(to))) {
        # rsync can't handle file renaming on copy
        useFileCopy <- TRUE
      } else {
        if (!dir.exists(to)) toDir <- dirname(to) # extract just the directory part
        rsyncBin <- tryCatch(Sys.which("rsync"), warning = function(w) NA_character_)
        opts <- if (silent) " -a " else " -avP "
        # rsync command can't handle spaces file/dir names -- must protect them
        toDir <- gsub("\ ", "\\ ", toDir, fixed = TRUE)
        rsync <- paste0(rsyncBin, " ", opts, " --delete "[delDestination],
                        "'", normalizePath(from, mustWork = TRUE), "' ",
                        normalizePath(toDir, mustWork = FALSE), "/")

        useFileCopy <- tryCatch(system(rsync, intern = TRUE), error = function(x) TRUE)
      }
    } else {
      useFileCopy <- TRUE
    }
  }
  if (isTRUE(useFileCopy)) {
    lapply(unique(dirname(to)), checkPath, create = create)
    file.copy(from = from, to = to, overwrite = overwrite, recursive = FALSE)
  }

  return(invisible(to))
}

#' @export
#' @rdname copyFile
copyFile <- Vectorize(copySingleFile, vectorize.args = c("from", "to"))

#' @importFrom methods slotNames
#' @importFrom digest digest
#' @importFrom raster res crs extent
.digestRasterLayer <- function(object, length, algo, quick) {
  # metadata -- only a few items of the long list because one thing (I don't recall)
  #  doesn't cache consistently
  if (isTRUE(getOption("reproducible.useNewDigestAlgorithm") < 2)) {
    return(.digestRasterLayer2(object, length, algo, quick))
  }

  isRasterStack <- is(object, "RasterStack")
  if (!isRasterStack) {
    objList <- list(object)
  } else {
    objList <- object@layers
  }
  dig <- lapply(objList, function(object) {
    sn <- slotNames(object@data)
    sn <- sn[!(sn %in% c(#"min", "max", "haveminmax", "names", "isfactor",
      "dropped", "nlayers", "fromdisk", "inmemory"
      #"offset", "gain"
    ))]
    dataSlotsToDigest <- lapply(sn, function(s) slot(object@data, s))
    names(dataSlotsToDigest) <- sn
    theData <- .robustDigest(object@data@values)
    dataSlotsToDigest$values <- NULL
    if (isTRUE(getOption("reproducible.useNewDigestAlgorithm") > 0))
      dig <- .robustDigest(append(list(dim(object), res(object), crs(object),
                                       extent(object), theData), dataSlotsToDigest), length = length, quick = quick,
                           algo = algo) # don't include object@data -- these are volatile
    else {
      if (!requireNamespace("fastdigest", quietly = TRUE))
        stop(requireNamespaceMsg("fastdigest", "to use options('reproducible.useNewDigestAlgorithm' = FALSE"))
      dig <- fastdigest::fastdigest(append(list(dim(object), res(object), crs(object),
                                                extent(object)), dataSlotsToDigest)) # don't include object@data -- these are volatile
    }

    # Legend
    sn <- slotNames(object@legend)
    legendSlotsToDigest <- lapply(sn, function(s) slot(object@legend, s))
    if (isTRUE(getOption("reproducible.useNewDigestAlgorithm") > 0))
      dig2 <- .robustDigest(legendSlotsToDigest, length = length, quick = quick,
                            algo = algo) # don't include object@data -- these are volatile
    else {
      if (!requireNamespace("fastdigest", quietly = TRUE))
        stop(requireNamespaceMsg("fastdigest", "to use options('reproducible.useNewDigestAlgorithm' = FALSE"))
      dig2 <- fastdigest::fastdigest(legendSlotsToDigest) # don't include object@data -- these are volatile
    }
    dig <- c(dig, dig2)

    sn <- slotNames(object@file)
    sn <- sn[!(sn %in% c("name"))]
    fileSlotsToDigest <- lapply(sn, function(s) slot(object@file, s))
    if (isTRUE(getOption("reproducible.useNewDigestAlgorithm") > 0))
      digFile <- .robustDigest(fileSlotsToDigest, length = length, quick = quick,
                               algo = algo) # don't include object@file -- these are volatile
    else {
      if (!requireNamespace("fastdigest", quietly = TRUE))
        stop(requireNamespaceMsg("fastdigest", "to use options('reproducible.useNewDigestAlgorithm' = FALSE"))
      digFile <- fastdigest::fastdigest(fileSlotsToDigest) # don't include object@file -- these are volatile
    }

    dig <- c(dig, digFile)
  })

  fns <- Filenames(object, allowMultiple = FALSE)
  if (length(fns[nchar(fns) > 0])) {
    # if the Raster is on disk, has the first length characters;
    isGrd <- endsWith(basename(fns), suffix = ".grd")
    if (isTRUE(any(isGrd))) {
      fns[isGrd] <- sub(fns[isGrd], pattern = ".grd$", replacement = ".gri")
    }
    # # there is no good reason to use depth = 0, 1, or 2 or more -- but I think 2 is *more* reliable
    dig2 <- .robustDigest(asPath(fns, 2), length = length, quick = quick, algo = algo)
    dig <- c(dig, unname(dig2))
  }

  if (isTRUE(getOption("reproducible.useNewDigestAlgorithm") > 0))
    dig <- .robustDigest(unlist(dig), length = length, quick = quick, algo = algo)
  else {
    if (!requireNamespace("fastdigest", quietly = TRUE))
      stop(requireNamespaceMsg("fastdigest", "to use options('reproducible.useNewDigestAlgorithm' = FALSE"))
    dig <- fastdigest::fastdigest(dig)
  }
  dig
}

.digestRasterLayer2 <- function(object, length, algo, quick) {
  # metadata -- only a few items of the long list because one thing (I don't recall)
  #  doesn't cache consistently
  sn <- slotNames(object@data)
  sn <- sn[!(sn %in% c(#"min", "max", "haveminmax", "names", "isfactor",
    "dropped", "nlayers", "fromdisk", "inmemory"
    #"offset", "gain"
  ))]
  dataSlotsToDigest <- lapply(sn, function(s) slot(object@data, s))
  if (isTRUE(getOption("reproducible.useNewDigestAlgorithm")))
    dig <- .robustDigest(append(list(dim(object), res(object), crs(object),
                                     extent(object)), dataSlotsToDigest), length = length, quick = quick,
                         algo = algo) # don't include object@data -- these are volatile
  else {
    if (!requireNamespace("fastdigest", quietly = TRUE))
      stop(requireNamespaceMsg("fastdigest", "to use options('reproducible.useNewDigestAlgorithm' = FALSE"))
    dig <- fastdigest::fastdigest(append(list(dim(object), res(object), crs(object),
                                              extent(object)), dataSlotsToDigest)) # don't include object@data -- these are volatile
  }

  # Legend
  sn <- slotNames(object@legend)
  legendSlotsToDigest <- lapply(sn, function(s) slot(object@legend, s))
  if (isTRUE(getOption("reproducible.useNewDigestAlgorithm")))
    dig2 <- .robustDigest(legendSlotsToDigest, length = length, quick = quick,
                          algo = algo) # don't include object@data -- these are volatile
  else {
    if (!requireNamespace("fastdigest", quietly = TRUE))
      stop(requireNamespaceMsg("fastdigest", "to use options('reproducible.useNewDigestAlgorithm' = FALSE"))
    dig2 <- fastdigest::fastdigest(legendSlotsToDigest) # don't include object@data -- these are volatile
  }
  dig <- c(dig, dig2)

  sn <- slotNames(object@file)
  sn <- sn[!(sn %in% c("name"))]
  fileSlotsToDigest <- lapply(sn, function(s) slot(object@file, s))
  if (isTRUE(getOption("reproducible.useNewDigestAlgorithm")))
    digFile <- .robustDigest(fileSlotsToDigest, length = length, quick = quick,
                             algo = algo) # don't include object@file -- these are volatile
  else {
    if (!requireNamespace("fastdigest", quietly = TRUE))
      stop(requireNamespaceMsg("fastdigest", "to use options('reproducible.useNewDigestAlgorithm' = FALSE"))
    digFile <- fastdigest::fastdigest(fileSlotsToDigest) # don't include object@file -- these are volatile
  }

  dig <- c(dig, digFile)
  if (nzchar(object@file@name)) {
    # if the Raster is on disk, has the first length characters;
    filename <- if (isTRUE(endsWith(basename(object@file@name), suffix = ".grd"))) {
      sub(object@file@name, pattern = ".grd$", replacement = ".gri")
    } else {
      object@file@name
    }
    # there is no good reason to use depth = 0, 1, or 2 or more -- but I think 2 is *more* reliable
    dig2 <- .robustDigest(asPath(filename, 2), length = length, quick = quick, algo = algo)
    dig <- c(dig, unname(dig2))
  }

  if (isTRUE(getOption("reproducible.useNewDigestAlgorithm")))
    dig <- .robustDigest(unlist(dig), length = length, quick = quick, algo = algo)
  else {
    if (!requireNamespace("fastdigest", quietly = TRUE))
      stop(requireNamespaceMsg("fastdigest", "to use options('reproducible.useNewDigestAlgorithm' = FALSE"))
    dig <- fastdigest::fastdigest(dig)
  }
  dig
}


################################################################################
#' @details
#' `.sortDotsUnderscoreFirst`: This exists so Windows, Linux, and Mac machines can have
#' the same order after a sort. It will put dots and underscores first
#' (with the sort key based on their second character, see examples.
#' It also sorts lower case before upper case.
#'
#' @param obj  An arbitrary R object for which a `names` function
#'              returns a character vector.
#'
#' @return `.sortDotsUnderscoreFirst`: the same object as `obj`,
#'   but sorted with dots and underscores first,
#'   lower case before upper case.
#'
#' @author Eliot McIntire
#' @export
#' @rdname exportedMethods
#'
#' @examples
#' items <- c(A = "a", Z = "z", `.D` = ".d", `_C` = "_C")
#' .sortDotsUnderscoreFirst(items)
#'
#' # dots & underscore (using 2nd character), then all lower then all upper
#' items <- c(B = "Upper", b = "lower", A = "a", `.D` = ".d", `_C` = "_C")
#' .sortDotsUnderscoreFirst(items)
#'
#' # with a vector
#' .sortDotsUnderscoreFirst(c(".C", "_B", "A")) # _B is first
#'
.sortDotsUnderscoreFirst <- function(obj) {
  obj[.orderDotsUnderscoreFirst(obj)]
}

#' @export
#' @rdname exportedMethods
.orderDotsUnderscoreFirst <- function(obj) {
  if (!is.null(names(obj))) {
    namesObj <- names(obj)
  } else {
    namesObj <- obj
  }

  if (is.character(namesObj)) {
    namesObj <- gsub(namesObj, pattern = "\\.|_", replacement = "aa")
    allLower <- tolower(namesObj) == namesObj
    namesObj[allLower] <- paste0("abALLLOWER", namesObj[allLower])

    onesChanged <- startsWith(namesObj, prefix = "a")
    namesObj[!onesChanged] <- paste0("ZZZZZZZZZ", namesObj[!onesChanged])

    out <- order(namesObj)
  } else {
    out <- seq_along(obj)
  }
  out
}

################################################################################
#' Attach debug info to return for Cache
#'
#' Internal use only. Attaches an attribute to the output, usable for
#' debugging the Cache.
#'
#' @param obj  An arbitrary R object.
#' @param preDigest  A list of hashes.
#' @param ...  Dots passed from Cache
#'
#' @return The same object as `obj`, but with 2 attributes set.
#'
#' @author Eliot McIntire
#' @importFrom data.table setattr
#' @rdname debugCache
.debugCache <- function(obj, preDigest, ...) {
  setattr(obj, "debugCache1", list(...))
  setattr(obj, "debugCache2", preDigest)
  obj
}

# loadFromLocalRepoMem <- memoise::memoise(loadFromLocalRepo)

#' @keywords internal
.getOtherFnNamesAndTags <- function(scalls) {
  if (is.null(scalls)) {
    scalls <- sys.calls()
  }

  patt <- paste(.defaultOtherFunctionsOmit, collapse = ")|(")
  otherFns <- .grepSysCalls(
    scalls,
    pattern = patt)
  if (length(otherFns)) {
    otherFns <- unlist(lapply(scalls[-otherFns], function(x) {
      tryCatch(as.character(x[[1]]), error = function(y) "")
    }))
    otherFns <- otherFns[nzchar(otherFns)]
    otherFns <- otherFns[!startsWith(otherFns, prefix = ".")]
    otherFns <- paste0("otherFunctions:", otherFns)
  } else {
    otherFns <- character()
  }

  # Figure out if it is in SpaDES.core call, which could be either a .parseModule or .runModuleInputObjects call...
  #   , if yes, then extract the module
  doEventFrameNum <- .grepSysCalls(scalls, "\\.parseModule|\\.runModuleInputObjects")
  if (length(doEventFrameNum)) {
    module <- get0("m", envir = sys.frame(tail(doEventFrameNum, 1)))
    if (is.null(module)) { # this block should cover any other cases, though is likely unnecessary
      # This whole mechanism is predicated on the module name being called "m" in the above 2 functions
      moduleEnv <- whereInStack("m")
      module <- get0("m", envir = moduleEnv)
    }

    # module <- get("m", envir = sys.frame(doEventFrameNum[2])) # always 2
    otherFns <- c(paste0("module:", module), otherFns)
  }
  unique(otherFns)
}

#' @keywords internal
nextNumericName <- function(string) {
  theExt <- fileExt(string)
  saveFilenameSansExt <- filePathSansExt(string)
  finalNumericPattern <- "_[[:digit:]]+$"
  allSimilarFilesInDir <- dir(dirname(saveFilenameSansExt), pattern = basename(saveFilenameSansExt))
  allSimilarFilesInDirSansExt <- if (length(allSimilarFilesInDir) == 0) {
    unique(saveFilenameSansExt)
  } else {
    unique(filePathSansExt(allSimilarFilesInDir))
  }
  alreadyHasNumeric <- grepl(allSimilarFilesInDirSansExt, pattern = finalNumericPattern)
  if (isTRUE(any(alreadyHasNumeric))) {
    splits <- strsplit(allSimilarFilesInDirSansExt[alreadyHasNumeric], split = "_")
    highestNumber <- max(unlist(lapply(splits, function(split) as.numeric(tail(split,1)))),
                         na.rm = TRUE)
    preNumeric <- unique(unlist(lapply(splits, function(spl) paste(spl[-length(spl)], collapse = "_")))) #nolint
    ## keep rndstr in here (below), so that both streams keep same rnd number state
    out <- file.path(dirname(saveFilenameSansExt), paste0(preNumeric, "_", highestNumber + 1))
  } else {
    out <- paste0(saveFilenameSansExt, "_1")
  }

  paste0(out, ".", theExt)
}



#' @importFrom raster fromDisk
dealWithClass <- function(obj, cachePath, drv, conn, verbose = getOption("reproducible.verbose")) {
  # browser(expr = exists("._dealWithClass_1"))
  outputToSaveIsList <- is(obj, "list") # is.list is TRUE for anything, e.g., data.frame. We only want "list"
  outputToSaveIsEnv <- is(obj, "environment")

  # if (is(obj, "simList")) browser()
  if (isTRUE(outputToSaveIsEnv) || isTRUE(outputToSaveIsList)) {
    obj2 <- if (isTRUE(outputToSaveIsEnv))
      as.list(obj, all.names = FALSE) else obj
    out <- lapply(obj2, dealWithClass, cachePath = cachePath, drv = drv, conn = conn)
    innerTags <- lapply(obj, function(o) attr(o, "tags"))
    innerTags <- unique(unlist(innerTags))
    setattr(obj, "tags", innerTags)

    if (isTRUE(outputToSaveIsEnv)) {
      obj <- Copy(obj)
      obj2 <- list2envAttempts(out, obj)
      if (!is.null(obj2)) obj <- obj2
      # attempt <- try(list2env(out, obj), silent = TRUE)
      # if (is(attempt, "try-error")) {
      #   # this is simList
      #   attempt <- try(list2env(out, obj@.xData), silent = TRUE)
      #   if (is(attempt, "try-error")) {
      #     obj <- as.environment(out)
      #   }
      # }

    } else {
      obj <- out
    }
    # obj <- lapply(obj, dealWithClass, cachePath = cachePath, drv = drv, conn = conn)
  }


  rasters <- is(obj, "Raster")
  isFromDisk <- FALSE # Default --> this will be updated
  if (any(rasters)) {
    objOrig <- obj
    atts <- attributes(obj)
    obj <- .prepareFileBackedRaster(obj, repoDir = cachePath,
                                    overwrite = FALSE, drv = drv, conn = conn)
    isFromDisk <- fromDisk(obj)

    # have to reset all these attributes on the rasters as they were undone in prev steps
    atts$tags <- c(atts$tags, paste("fromDisk", sep = ":", isFromDisk))
    setattr(obj, "tags", atts$tags)
    .setSubAttrInList(obj, ".Cache", "newCache", atts$.Cache$newCache)
    setattr(obj, "call", atts$call)

    if (!identical(attr(obj, ".Cache")$newCache, atts$.Cache$newCache))
      stop("attributes are not correct 6")
    if (!identical(attr(obj, "call"), atts$call))
      stop("attributes are not correct 7")
    if (!identical(attr(obj, "tags"), atts$tags))
      stop("attributes are not correct 8")

    if (!is.null(atts[["function"]])) {
      setattr(obj, "function", atts[["function"]])
      if (!identical(attr(obj, "function"), atts[["function"]]))
        stop("There is an unknown error 04")
    }
    if (isFromDisk) {
      if (isTRUE(getOption("reproducible.useNewDigestAlgorithm") < 2)) {
        obj <- list(origRaster = objOrig, cacheRaster = obj)
      } else {
        obj <- list(origRaster = Filenames(objOrig), cacheRaster = obj)
      }
      setattr(obj, "tags",
              c(attributes(obj$cacheRaster)$tags,
              paste0("origRaster:", obj$origRaster),
              paste0("cacheRaster:", Filenames(obj))))
    }

  }

  if (any(inherits(obj, "SpatVector"), inherits(obj, "SpatRaster"))) {
    if (!requireNamespace("terra", quietly = TRUE) && getOption("reproducible.useTerra", FALSE))
      stop("Please install terra package")
    messageCache("...wrapping terra object for saving...", verboseLevel = 2, verbose = verbose)
    attrs <- attr(obj, ".Cache")

    # next is for terra objects --> terra::wrap is ridiculously slow for SpatVector objects; use
    #   custom version in reproducible where here
    if (inherits(obj, "SpatRaster")) {
      obj <- terra::wrap(obj)
    } else {
      obj <- wrapSpatVector(obj)
    }
    setattr(obj, ".Cache", attrs)

    messageCache("\b Done!", verboseLevel = 2, verbose = verbose)
  }
  obj
}

dealWithClassOnRecovery <- function(output, cachePath, cacheId,
                                    drv = getOption("reproducible.drv", RSQLite::SQLite()),
                                    conn = getOption("reproducible.conn", NULL)) {
  if (isTRUE(getOption("reproducible.useNewDigestAlgorithm") < 2)) {
    return(dealWithClassOnRecovery2(output, cachePath, cacheId,
                                    drv, conn))
  }

  isOutputList <- is(output, "list")
  isOutputEnv <- is(output, "environment")
  if (isOutputList || isOutputEnv) {
    anyNames <- names(output)
    isSpatVector <- if (is.null(anyNames)) FALSE else all(names(output) %in% spatVectorNamesForCache)
    if (isTRUE(isSpatVector)) {
      output <- unwrapSpatVector(output)
    } else {
      if (!"cacheRaster" %in% names(output)) { # recursive up until a list has cacheRaster name

        outList <- if (isOutputEnv) as.list(output) else output # the as.list doesn't get everything. But with a simList, this is OK; rest will stay

        outList <- lapply(outList, function(out) dealWithClassOnRecovery(out, cachePath, cacheId,
                                                                         drv, conn))

        if (isOutputEnv) { # don't overwrite everything, just the ones in the list part
          output2 <- list2envAttempts(outList, output)
          if (!is.null(output2)) output <- output2
          # attempt <- try(list2env(outList, output), silent = TRUE)
          # if (is(attempt, "try-error")) {
          #   attempt <- try(list2env(outList, output@.xData), silent = TRUE)
          #   if (is(attempt, "try-error"))
          #     output <- as.environment(outList)
          # }
        } else {
          output <- outList
        }
      } else {
        origFilenames <- if (is(output, "Raster")) {
          Filenames(output) # This is legacy piece which allows backwards compatible
        } else {
          output$origRaster
        }

        filesExist <- file.exists(origFilenames)
        cacheFilenames <- Filenames(output)
        filesExistInCache <- file.exists(cacheFilenames)
        if (any(!filesExistInCache)) {
          fileTails <- gsub("^.+(rasters.+)$", "\\1", cacheFilenames)
          correctFilenames <- file.path(cachePath, fileTails)
          filesExistInCache <- file.exists(correctFilenames)
          if (all(filesExistInCache)) {
            cacheFilenames <- correctFilenames
          } else {
            stop("File-backed raster files in the cache are corrupt for cacheId: ", cacheId)
          }

        }
        out <- hardLinkOrCopy(cacheFilenames[filesExistInCache],
                              origFilenames[filesExistInCache], overwrite = TRUE)

        newOutput <- updateFilenameSlots(output$cacheRaster,
                                         Filenames(output, allowMultiple = FALSE),
                                         newFilenames = grep("\\.gri$", origFilenames, value = TRUE, invert = TRUE))
        output <- newOutput
        .setSubAttrInList(output, ".Cache", "newCache", FALSE)

      }

    }
  }

  if (any(inherits(output, "PackedSpatVector"))) {
    if (!requireNamespace("terra", quietly = TRUE) && getOption("reproducible.useTerra", FALSE))
      stop("Please install terra package")
    output <- terra::vect(output)
  }
  if (any(inherits(output, "PackedSpatRaster"))) {
    if (!requireNamespace("terra", quietly = TRUE) && getOption("reproducible.useTerra", FALSE))
      stop("Please install terra package")
    output <- terra::rast(output)
  }
  if (any(inherits(output, "data.table"))) {
    output <- data.table::copy(output)
  }

  output
}

# This one is old, overly complicated; defunct
dealWithClassOnRecovery2 <- function(output, cachePath, cacheId,
                                     drv = getOption("reproducible.drv", RSQLite::SQLite()),
                                     conn = getOption("reproducible.conn", NULL)) {
  # This function is because the user doesn't want the path of the file-backed raster to
  #   be in the cachePath --> they want it in its original file location
  #   If it is in both, take the one in the original location; if it has been deleted
  #   from the original location, then grab it from cache and put it in original place
  if (is(output, "list")) {
    if (identical(names(output), c("origRaster", "cacheRaster"))) {
      origFilenames <- Filenames(output$origRaster)
      cacheFilenames <- Filenames(output$cacheRaster)
      origStillExist <- file.exists(origFilenames)
      origFilenamesNeed <- origFilenames[!origStillExist]
      cacheFilenamesNeed <- cacheFilenames[!origStillExist]
      origFilenamesNeedDig <- origFilenames[origStillExist]
      cacheFilenamesNeedDig <- cacheFilenames[origStillExist]
      if (any(origStillExist)) {
        cacheFilenamesDig <- unlist(.robustDigest(asPath(cacheFilenamesNeedDig)))
        origFilenamesDig <- unlist(.robustDigest(asPath(origFilenamesNeedDig)))
        whichUnchanged <- cacheFilenamesDig == origFilenamesDig
        if (any(whichUnchanged)) {
          origFilenamesNeedDig <- origFilenamesNeedDig[!whichUnchanged]
          cacheFilenamesNeedDig <- cacheFilenamesNeedDig[!whichUnchanged]
        }
        cacheFilenamesNeed <- c(cacheFilenamesNeed, cacheFilenamesNeedDig)
        origFilenamesNeed <- c(origFilenamesNeed, origFilenamesNeedDig)
      }
      dirnamesRasters <- unique(dirname(dirname(cacheFilenamesNeed)))
      if (length(dirnamesRasters))
        if (!isTRUE(all.equal(dirnamesRasters, cachePath))) { # if this is a moved cache, the filenames in the cache will be wrong
          cacheFilenamesNeed2 <- gsub(dirnamesRasters, cachePath, cacheFilenamesNeed)
          wrongFilenames <- file.exists(cacheFilenamesNeed2)
          if (any(wrongFilenames)) {
            output$cacheRaster <- updateFilenameSlots(output$cacheRaster, cacheFilenamesNeed[wrongFilenames],
                                                      newFilenames = cacheFilenamesNeed2[wrongFilenames])
            fs <- saveFileInCacheFolder(output, cachePath = cachePath, cacheId = cacheId)
            cacheFilenamesNeed[wrongFilenames] <- cacheFilenamesNeed2[wrongFilenames]
          }

        }
      copyFile(from = cacheFilenamesNeed, to = origFilenamesNeed, overwrite = TRUE)
      output <- output$origRaster
      .setSubAttrInList(output, ".Cache", "newCache", FALSE)
    }
  }
  output
}


list2envAttempts <- function(x, envir) {
  attempt <- try(list2env(x, envir), silent = TRUE)
  output <- NULL
  if (is(attempt, "try-error")) {
    attempt <- try(list2env(x, envir@.xData), silent = TRUE)
    if (is(attempt, "try-error"))
      output <- as.environment(x)
  }
  output
}

.loadedCacheResultMsg <- "loaded cached result from previous"

.loadedMemoisedResultMsg <- "loaded memoised result from previous"

.addingToMemoisedMsg <- "(and added a memoised copy)"

.loadedCacheMsg <- function(root, functionName) {
  paste0("     ", root," ", functionName, " call")
}




updateFilenameSlots2 <- function(obj, curFilenames, newFilenames, isStack = NULL) {
  whichNotGri <- grep("\\.gri$", curFilenames, invert = TRUE)
  curFilenamesNotGri <- curFilenames[whichNotGri]
  newFilenamesNotGri <- newFilenames[whichNotGri]
  if (length(curFilenamesNotGri) > 1 ) {
    for (i in seq_along(curFilenamesNotGri)) {
      slot(slot(slot(obj, "layers")[[i]], "file"), "name") <- newFilenamesNotGri[i]
    }
  } else {
    if (is.null(isStack)) isStack <- is(obj, "RasterStack")
    if (!isStack) {
      slot(slot(obj, "file"), "name") <- newFilenamesNotGri
    } else {
      for (i in seq_len(nlayers(obj))) {
        if (fromDisk(obj[[i]])) {
          whFilename <- match(withoutFinalNumeric(basename(newFilenamesNotGri)),
                              withoutFinalNumeric(basename(curFilenamesNotGri)))
          slot(slot(obj@layers[[i]], "file"), "name") <- newFilenamesNotGri[whFilename]
        }
      }
    }
  }
  obj
}

#' Copy the file-backing of a file-backed Raster* object
#'
#' Rasters are sometimes file-based, so the normal save and copy and assign
#' mechanisms in R don't work for saving, copying and assigning.
#' This function creates an explicit file copy of the file that is backing the raster,
#' and changes the pointer (i.e., `filename(object)`) so that it is pointing
#' to the new file.
#'
#' @param obj The raster object to save to the repository.
#'
#' @param repoDir Character denoting an existing directory in which an artifact will be saved.
#'
#' @param overwrite Logical. Should the raster be saved to disk, overwriting existing file.
#'
#' @param ... Not used
#'
#' @return A raster object and its newly located file backing.
#'         Note that if this is a legitimate Cache repository, the new location
#'         will be a subdirectory called \file{rasters/} of \file{repoDir/}.
#'         If this is not a repository, the new location will be within `repoDir`.
#'
#' @author Eliot McIntire
#' @importFrom digest digest
#' @importFrom methods is selectMethod slot slot<-
#' @importFrom raster dataType filename hasValues inMemory nlayers writeRaster
#' @inheritParams Cache
#' @rdname prepareFileBackedRaster
.prepareFileBackedRaster <- function(obj, repoDir = NULL, overwrite = FALSE,
                                     drv = getOption("reproducible.drv", RSQLite::SQLite()),
                                     conn = getOption("reproducible.conn", NULL),
                                     ...) {
  if (isTRUE(getOption("reproducible.useNewDigestAlgorithm") < 2)) {
    return(.prepareFileBackedRaster2(obj, repoDir = repoDir, overwrite = overwrite,
                                     drv = drv, conn = conn, ...))
  }
  fnsAll <- Filenames(obj)
  fnsShort <- Filenames(obj, FALSE)
  if (!all(nchar(fnsAll) == 0)) {
    repoDir <- checkPath(repoDir, create = TRUE)
    isRepo <- CacheIsACache(cachePath = repoDir, drv = drv, conn = conn)
    # thoseWithGRI <- endsWith(fnsAll, "gri")
    fns <- fnsAll
    FB <- nchar(fns) > 0
    ########################
    if (any(!file.exists(fns[FB]))) {
      FBshort <- nchar(fnsShort) > 0
      fnsOnly <- fnsShort[FBshort]
      badFileNames <- fnsOnly[!file.exists(fnsOnly)]

      trySaveFilename <- badFileNames
      if (any(grepl(basename(repoDir), badFileNames))) {
        # File is in wrong folder, usually the result of a copy of cache between 2 machines
        splittedFilenames <- strsplit(badFileNames, split = basename(repoDir))
        trySaveFilename <- if (length(splittedFilenames) == 1) {
          normalizePath(
            file.path(repoDir, splittedFilenames[[1]][[length(splittedFilenames[[1]])]]),
            winslash = "/", mustWork = FALSE)
        } else {
          splittedFilenames2 <- lapply(splittedFilenames, function(x) {
            ifelse(length(x), x[length(x)], "")
          })
          normalizePath(file.path(repoDir, splittedFilenames2), winslash = "/", mustWork = FALSE)
        }
      }
      if (any(!file.exists(trySaveFilename))) {
        stop("The following file-backed rasters are supposed to be on disk ",
             "but appear to have been deleted:\n",
             paste("    ", badFileNames, collapse = "\n"),
             ". The most likely reason is that two functions had the same output ",
             "and one of them was removed with clearCache(...). ",
             "The best solution to this is never have two functions create the same ",
             "file-backed raster.")
      } else {
        obj <- updateFilenameSlots(obj, curFilenames = fnsOnly, trySaveFilename)
        fnsAll <- fns <- Filenames(obj)
      }
    }
    #################


    saveFilename <- fns
    bn <- basename(fns)
    bnFB <- bn[FB]


    saveFilename[FB] <- if (isRepo) {
      file.path(repoDir, "rasters"[isRepo], bnFB)
    } else {
      file.path(repoDir, bnFB)
    }
    dirForNewFiles <- unique(dirname(saveFilename[FB]))
    checkPath(dirForNewFiles, create = TRUE)
    saveFilename <- normPath(saveFilename)
    saveFilenamePreNumeric <- saveFilename
    exist <- file.exists(saveFilename)
    if (any(exist)) {
      saveFilename[exist] <- unlist(lapply(saveFilename[exist], nextNumericName))
    }
    FBAll <- nchar(fnsAll) > 0

    out <- hardLinkOrCopy(from = fnsAll[FBAll], to = saveFilename[FBAll])

    # out <- Map(from = fnsAll[FBAll], to = saveFilename[FBAll],
    #     function(from, to) {
    #   linkTry <- suppressWarningsSpecific(file.link(from = from, to = to),
    #                                   falseWarnings = "already exists|Invalid cross-device")
    #   if (!linkTry)
    #     linkTry <- copyFile(from = from, to = to, overwrite = TRUE, silent = TRUE)
    #   linkTry
    # })

    # FBshort <- nchar(fnsShort) > 0
    saveFilenamesToUpdateSlot <- saveFilename[basename(saveFilenamePreNumeric) %in%
                                                basename(fnsShort)]
    obj <- updateFilenameSlots(obj, fnsShort, saveFilenamesToUpdateSlot)

  }
  obj
}

allInOneFile <- function(obj) {
  aiof <- TRUE
  if (is(obj, "RasterStack")) {
    aiof <- FALSE
    numFiles <- sum(nchar(unique(Filenames(obj)))>0)
    if (numFiles > 0) {
      innerFilenames <- unlist(lapply(obj@layers, filename))
      aiof <- isTRUE(sum(nchar(innerFilenames)>0) == length(obj@layers))
    }
  }
  aiof
}

withoutFinalNumeric <- function(string) {
  ext <- fileExt(string)
  string1 <- filePathSansExt(string)
  woNumeric <- gsub("^(.+)\\_[[:digit:]]+$", "\\1", string1)
  paste0(woNumeric, ".", ext)
}



wrapSpatVector <- function(obj) {
  geom1 <- terra::geom(obj)
  geom1 <- list(cols125 = matrix(as.integer(geom1[, c(1, 2, 5)]), ncol = 3),
                cols34 = matrix(as.integer(geom1[, c(3, 4)]), ncol = 2))
  geomtype1 <- terra::geomtype(obj)
  dat1 <- terra::values(obj)
  crs1 <- terra::crs(obj)
  obj <- list(geom1, geomtype1, dat1, crs1)
  names(obj) <- spatVectorNamesForCache
  obj
}

unwrapSpatVector <- function(obj) {
  obj$x <- cbind(obj$x$cols125[, 1:2, drop = FALSE], obj$x$cols34[, 1:2, drop = FALSE], obj$x$cols125[, 3, drop = FALSE])
  do.call(terra::vect, obj)
}
