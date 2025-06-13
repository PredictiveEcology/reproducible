utils::globalVariables(c(
  "actualFile", "algorithm", "checksum", "checksum.x", "checksum.y",
  "filesize", "filesize.x", "filesize.y",
  "i.algorithm", "i.checksum", "i.filesize", "result"
))

################################################################################
#' Calculate checksum
#'
#' Verify (and optionally write) checksums.
#' Checksums are computed using [.digest()], which is simply a
#' wrapper around `digest::digest`.
#'
#' @note In version 1.2.0 and earlier, two checksums per file were required
#' because of differences in the checksum hash values on Windows and Unix-like
#' platforms. Recent versions use a different (faster) algorithm and only require
#' one checksum value per file.
#' To update your \file{CHECKSUMS.txt} files using the new algorithm, see
#' <https://github.com/PredictiveEcology/SpaDES/issues/295#issuecomment-246513405>.
#'
#' @param path    Character string giving the directory path containing `CHECKSUMS.txt`
#'                file, or where it will be written if `checksumFile = TRUE`.
#'
#' @param write   Logical indicating whether to overwrite `CHECKSUMS.txt`.
#'                Default is `FALSE`, as users should not change this file.
#'                Module developers should write this file prior to distributing
#'                their module code, and update accordingly when the data change.
#'
#' @param quickCheck Logical. If `TRUE`, then this will only use file sizes,
#'                   rather than a digest::digest hash. This is generally faster,
#'                   but will be *much* less robust.
#'
#' @param checksumFile The filename of the checksums file to read or write to.
#'                     The default is \file{CHECKSUMS.txt} located at
#'                     `file.path(path, module, "data", checksumFile)`.
#'                     It is likely not a good idea to change this, and should
#'                     only be used in cases such as `Cache`, which can
#'                     evaluate if the `checksumFile` has changed.
#'
#' @param files An optional character string or vector of specific files to checksum.
#'              This may be very important if there are many files listed in a
#'              `CHECKSUMS.txt` file, but only a few are to be checksummed.
#'
#' @param ...     Passed to [digest::digest()] and [utils::write.table()].
#'                For `digest`, the notable argument is `algo`. For `write.table`,
#'                the notable argument is `append`.
#'
#' @inheritParams Cache
#' @return A `data.table` with columns: `result`, `expectedFile`,
#'         `actualFile`, `checksum.x`, `checksum.y`,
#'         `algorithm.x`, `algorithm.y`, `filesize.x`, `filesize.y`
#'         indicating the result of comparison between local file (`x`) and
#'         expectation based on the `CHECKSUMS.txt` file.
#'
#' @author Alex Chubaty
#' @export
#' @rdname Checksums
#'
#' @examples
#' \dontrun{
#' modulePath <- file.path(tempdir(), "myModulePath")
#' dir.create(modulePath, recursive = TRUE, showWarnings = FALSE)
#' moduleName <- "myModule"
#' cat("hi", file = file.path(modulePath, moduleName)) # put something there for this example
#'
#' ## verify checksums of all data files
#' Checksums(modulePath, files = moduleName)
#'
#' ## write new CHECKSUMS.txt file
#' Checksums(files = moduleName, modulePath, write = TRUE)
#' }
#'
setGeneric("Checksums", function(path, write, quickCheck = getOption("reproducible.quickCheck", FALSE),
                                 checksumFile = identifyCHECKSUMStxtFile(path),
                                 files = NULL, verbose = getOption("reproducible.verbose", 1),
                                 ...) {
  standardGeneric("Checksums")
})

#' @importFrom data.table setnames
#' @importFrom methods formalArgs
#' @importFrom stats na.omit
#' @importFrom utils read.table write.table
#' @rdname Checksums
setMethod(
  "Checksums",
  signature = c(
    path = "character", quickCheck = "ANY",
    write = "logical", files = "ANY"
  ),
  definition = function(path, write, quickCheck = getOption("reproducible.quickCheck", FALSE),
                        checksumFile,
                        files, verbose = getOption("reproducible.verbose", 1), ...) {
    defaultHashAlgo <- "xxhash64"
    defaultWriteHashAlgo <- "xxhash64"
    dots <- list(...)
    dotsWriteTable <- dots[names(dots) %in% formalArgs(write.table)]
    dots <- dots[names(dots) %in% formalArgs(digest::digest)]
    checkPath(path, create = write)

    if (!file.exists(checksumFile)) {
      writeChecksumsTable(.emptyChecksumsFileContent, checksumFile, dotsWriteTable)
    }

    if (is.null(files)) {
      files <- list.files(path, full.names = TRUE)
      files <- grep(files,
        pattern = makeRelative(checksumFile, path),
        value = TRUE, invert = TRUE
      )
      # files <- fs::path_norm(files)
    } else {
      isAbs <- isAbsolutePath(files)
      if (any(!isAbs)) {
        files[!isAbs] <- makeAbsolute(files[!isAbs], path)
      }
    }

    txt <- if (file.size(checksumFile) == 0) {
      .emptyChecksumsFileContent
    } else {
      read.table(checksumFile,
        header = TRUE,
        stringsAsFactors = FALSE
      )
    }
    txt <- as.data.table(lapply(txt, as.character))
    set(txt, NULL, "file", makeRelative(txt$file, path))
    if (is.null(txt$filesize)) txt$filesize <- rep("", NROW(txt))
    txtRead <- txt # keep a copy even if writing
    if (!(!write && file.info(checksumFile)$size > 0)) {
      txt <- data.frame(
        file = character(0), checksum = character(0),
        filesize = character(0), stringsAsFactors = FALSE
      )
    }

    if (is.null(dots$algo)) {
      if (NROW(files)) {
        if (write) {
          dots$algo <- defaultWriteHashAlgo
        } else {
          dots$algo <- defaultHashAlgo
        }
      } else {
        dots$algo <- character()
      }
    }

    stStart <- Sys.time()
    filesToCheck <- if (length(txt$file) & length(files)) {
      inTxt <- makeRelative(files, path) %in% makeRelative(txt$file, path)
      if (isTRUE(any(inTxt)))
        files <- files[inTxt]
      else {
        # might fail because it is listed in inputPaths; check there
        possPath <- getOption("reproducible.inputPaths")
        # can be length > 1
        if (!is.null(possPath)) {
          possPath <- normPath(possPath)
          if (!identical(possPath, path)) {
            for (pp in possPath) {
              inTxt <- makeRelative(files, path) %in%
                makeRelative(txt$file, pp)
              if (isTRUE(any(inTxt))) {
                files <- files[inTxt]
                break
              }
            }
          }
        }
      }
      files
    } else {
      files
    }

    if (length(filesToCheck) != length(files[!endsWith(files, "similar")])) {
      # Could be a case of user passing file path that is not with subdirectories; offer help
      justByBasename <- basename(txt$file) %in% basename(files)
      if (sum(justByBasename) == length(files)) {
        messagePreProcess(
          "Files found in CHECKSUMS.txt that match by basename; using these.\n",
          "  User should specify all files (e.g., targetFile, alsoExtract, archive)\n",
          "  with subfolders specified."
        )
        filesToCheck <- unique(c(filesToCheck, makeAbsolute(txt$file[justByBasename], path)))
      }
    }

    filesToCheck <- filesToCheck[file.exists(filesToCheck)] # remove non existing files
    # filesToCheck <- filesToCheck[!dir.exists(filesToCheck)] # remove directories # need to keep directories b/c e.g., gdb files need directories

    if (!is.null(txt$algorithm)) {
      if (!write) {
        dots$algo <- unique(txt[txt$file %in% makeRelative(filesToCheck, path), ][["algorithm"]])
        dots$algo <- dots$algo[!is.na(dots$algo)][1]
        # dots$algo <- na.omit(dots$algo)[1]
        if (is.na(dots$algo)) dots$algo <- defaultWriteHashAlgo
      }
    } else {
      if (NROW(txt)) {
        txt$algorithm <- defaultWriteHashAlgo
      } else {
        txt$algorithm <- character()
      }
      if (NROW(txtRead)) {
        txtRead$algorithm <- defaultWriteHashAlgo
      } else {
        txtRead$algorithm <- character()
      }
    }

    if (is.null(txt$filesize)) {
      quickCheck <- FALSE
      messagePreProcess("Not possible to use quickCheck;\n ",
        "    CHECKSUMS.txt file does not have filesizes",
        sep = "", verbose = verbose
      )
    }
    checksums <- rep(list(rep("", length(filesToCheck))), 2)
    dirs <- dir.exists(filesToCheck)
    filesToCheckWODirs <- filesToCheck[!dirs]
      if (quickCheck | write) {
        checksums[[2]][!dirs] <- do.call(.digest,
                                         args = append(
          list(file = filesToCheckWODirs, quickCheck = TRUE),
          dots
        )
      )
    }

    if (!quickCheck | write) {
      checksums[[1]][!dirs] <- do.call(.digest,
        args = append(
          list(file = filesToCheckWODirs, quickCheck = FALSE),
          dots
        )
      )
    }
    if (any(dirs)) {
      checksums[[1]][dirs] <- "dir"
      checksums[[2]][dirs] <- 0
    }

    verboseTmp <- difftime(Sys.time(), stStart) > 8
      messagePreProcess("Finished checking local files.", sep = "", verbose = verbose - 1 + verboseTmp)

    filesToCheckRel <- makeRelative(filesToCheck, path)
    out <- if (length(filesToCheck)) {
      data.table(
        file = filesToCheckRel, checksum = checksums[[1]],
        filesize = checksums[[2]], algorithm = dots$algo, stringsAsFactors = FALSE
      )
    } else {
      data.table(
        file = character(0), checksum = character(0), filesize = character(0),
        algorithm = character(0), stringsAsFactors = FALSE
      )
    }

    out1 <- data.table::copy(out)
    if (write) {
      writeChecksumsTable(out1, checksumFile, dotsWriteTable)
      txt <- txtRead
      txt <- txt[out, on = colnames(out)]
      # txt1Old <- dplyr::right_join(txt1Old, out)
    }
    txt1 <- data.table::copy(txt)

    out[, actualFile := file]
    if (write) {
      out <- txt[out, on = "file"]
    } else {
      out <- out[txt, on = "file"]
    }
    setnames(out, "file", "expectedFile")
    if (quickCheck) {
      out[, result := ifelse(filesize != i.filesize, "FAIL", "OK")]
    } else {
      out[, result := ifelse(checksum != i.checksum, "FAIL", "OK")]
    }
    data.table::setorderv(out, "result", order = -1L, na.last = TRUE)
    out <- out[, .SD[1, ], by = "expectedFile"]
    out <- checksumsDirsOk(out)

    results.df <- out[, list(
      "result" = result,
      "expectedFile" = expectedFile,
      "actualFile" = actualFile,
      "checksum.x" = i.checksum,
      "checksum.y" = checksum,
      "algorithm.x" = i.algorithm,
      "algorithm.y" = algorithm,
      "filesize.x" = i.filesize,
      "filesize.y" = filesize
    )]

    return(invisible(results.df))
  }
)

#' @rdname Checksums
setMethod(
  "Checksums",
  signature = c(
    path = "character", quickCheck = "ANY",
    write = "missing", files = "ANY"
  ),
  definition = function(path, quickCheck = getOption("reproducible.quickCheck", FALSE), checksumFile,
                        files, verbose, ...) {
    Checksums(path,
      write = FALSE, quickCheck = quickCheck, checksumFile = checksumFile,
      files = files, verbose = verbose, ...
    )
  }
)

#' @keywords internal
writeChecksumsTable <- function(out, checksumFile, dots) {
  out <- out[.orderDotsUnderscoreFirst(out$file), ] ## sort by filename alphabetically

  do.call(write.table,
    args = append(
      list(
        x = out, file = checksumFile, eol = "\n",
        col.names = !isTRUE(dots$append),
        row.names = FALSE
      ),
      dots
    )
  )
}

#' Calculate the hashes of multiple files
#'
#' Internal function. Wrapper for [digest::digest()] using `algo = xxhash64`.
#'
#' @param file  Character vector of file paths.
#' @param quickCheck Logical indicating whether to use a fast file size check as a heuristic
#'                   for determining changes to a file.
#' @param ...   Additional arguments to [digest::digest()].
#'
#' @return A character vector of hashes.
#'
#' @author Alex Chubaty
#' @importFrom digest digest
#' @keywords internal
#' @rdname digest
setGeneric(".digest", function(file, quickCheck, ...) {
  standardGeneric(".digest")
})

#' @rdname digest
setMethod(
  ".digest",
  signature = c(file = "character"),
  definition = function(file, quickCheck, algo = "xxhash64", ...) {
    if (quickCheck) {
      fs <- file.size(file)
      as.character(fs) # need as.character for empty case
    } else {
      as.character(
        unname(
          unlist(
            lapply(file, function(f) {
              digest::digest(object = f, file = TRUE, algo = algo, ...)
            })
          )
        )
      ) # need as.character for empty case # nolint
    }
  }
)


checksumsDirsOk <- function(out) {
  cscols <- "checksum.x|i.checksum"
  if (any(grepl(cscols, colnames(out)))) {
    cscol <- grep(cscols, colnames(out), value = TRUE)[1]
    dirsHave <- unique(dirname(out[!get(cscol) %in% "dir" & result == "OK"]$expectedFile))
    dirsHave <- grep("\\.", dirsHave, value = TRUE, invert = TRUE)
    if (length(dirsHave)) {
      out[get(cscol) %in% "dir" & expectedFile %in% dirsHave, result := "OK"]
    }
  }
  out
}
