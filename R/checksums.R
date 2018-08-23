if (getRversion() >= "3.1.0") {
  utils::globalVariables(c("checksum.x", "checksum.y", "filesize.x", "filesize.y", "result" ))
}


################################################################################
#' Calculate checksum
#'
#' Verify (and optionally write) checksums.
#' Checksums are computed using \code{\link{.digest}}, which is simply a
#' wrapper around \code{digest::digest}.
#'
#' @note In version 1.2.0 and earlier, two checksums per file were required
#' because of differences in the checksum hash values on Windows and Unix-like
#' platforms. Recent versions use a different (faster) algorithm and only require
#' one checksum value per file.
#' To update your \file{CHECKSUMS.txt} files using the new algorithm, see
#' \url{https://github.com/PredictiveEcology/SpaDES/issues/295#issuecomment-246513405}.
#'
#' @param path    Character string giving the directory path containing \code{CHECKSUMS.txt}
#'                file, or where it will be written if \code{checksumFile = TRUE}.
#'
#' @param write   Logical indicating whether to overwrite \code{CHECKSUMS.txt}.
#'                Default is \code{FALSE}, as users should not change this file.
#'                Module developers should write this file prior to distributing
#'                their module code, and update accordingly when the data change.
#'
#' @param quickCheck Logical. If \code{TRUE}, then this will only use file sizes,
#'                   rather than a digest::digest hash. This is generally faster,
#'                   but will be \emph{much} less robust.
#'
#' @param checksumFile The filename of the checksums file to read or write to.
#'                     The default is \file{CHECKSUMS.txt} located at
#'                     \code{file.path(path, module, "data", checksumFile)}.
#'                     It is likely not a good idea to change this, and should
#'                     only be used in cases such as \code{Cache}, which can
#'                     evaluate if the \code{checksumFile} has changed.
#'
#' @param files An optional character string or vector of specific files to checksum.
#'              This may be very important if there are many files listed in a
#'              \code{CHECKSUMS.txt} file, but only a few are to be checksummed.
#'
#' @param ...     Passed to \code{\link[digest]{digest}} and \code{\link[utils]{write.table}}.
#'                For \code{digest}, the notable argument is \code{algo}. For \code{write.table},
#'                the notable argument is \code{append}.
#'
#' @return A \code{data.table} with columns: \code{result}, \code{expectedFile},
#'         \code{actualFile}, \code{checksum.x}, \code{checksum.y},
#'         \code{algorithm.x}, \code{algorithm.y}, \code{filesize.x}, \code{filesize.y}
#'         indicating the result of comparison between local file (\code{x}) and
#'         expectation based on the \code{CHECKSUMS.txt} file.
#'
#' @importFrom dplyr arrange desc filter group_by left_join mutate rename row_number select
#' @export
#' @rdname Checksums
#'
#' @author Alex Chubaty
#'
#' @examples
#' \dontrun{
#' moduleName <- "my_module"
#' modulePath <- file.path("path", "to", "modules")
#'
#' ## verify checksums of all data files
#' Checksums(moduleName, modulePath)
#'
#' ## write new CHECKSUMS.txt file
#'
#' # 1. verify that all data files are present (and no extra files are present)
#' list.files(file.path(modulePath, moduleName, "data"))
#'
#' # 2. calculate file checksums and write to file (this will overwrite CHECKSUMS.txt)
#' Checksums(moduleName, modulePath, write = TRUE)
#' }
#'
setGeneric("Checksums", function(path, write, quickCheck = FALSE,
                                 checksumFile = file.path(path, "CHECKSUMS.txt"),
                                 files = NULL, ...) {
  standardGeneric("Checksums")
})

#' @rdname Checksums
#' @importFrom utils read.table write.table
#' @importFrom methods formalArgs
#' @importFrom crayon magenta
setMethod(
  "Checksums",
  signature = c(path = "character", quickCheck = "ANY",
                write = "logical", files = "ANY"),
  definition = function(path, write, quickCheck, checksumFile, files, ...) {
    defaultHashAlgo <- "xxhash64"
    defaultWriteHashAlgo <- "xxhash64"
    dots <- list(...)
    dotsWriteTable <- dots[names(dots) %in% formalArgs(write.table)]
    dots <- dots[names(dots) %in% formalArgs(digest::digest)]
    checkPath(path, create = write)

    # If it is a SpaDES module, then CHECKSUM.txt must be in the data folder
    checksumFile <- file.path(path, basename(checksumFile))

    if (!write) {
      stopifnot(file.exists(checksumFile))
    } else if (!file.exists(checksumFile)) {
      file.create(checksumFile)
    }

    if (is.null(files)) {
      files <- list.files(path, full.names = TRUE) %>%
        grep(basename(checksumFile), ., value = TRUE, invert = TRUE)
    }

    txt <- if (file.size(checksumFile) == 0 || !file.exists(checksumFile)) {
      .emptyChecksumsFileContent
    } else {
      read.table(checksumFile,
                 header = TRUE,
                 stringsAsFactors = FALSE)
    }
    #if (dim(txt)[1] == 0) { # if there are no rows
    txt <- dplyr::mutate_all(txt, as.character)
    #}
    if (is.null(txt$filesize)) txt$filesize <- rep("", NROW(txt))
    txtRead <- txt # keep a copy even if writing
    if (!(!write && file.info(checksumFile)$size > 0)) {
      txt <- data.frame(file = character(0), checksum = character(0),
                        filesize = character(0), stringsAsFactors = FALSE)
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

    message(crayon::magenta("Checking local files...", sep = ""))
    filesToCheck <-  if (length(txt$file) & length(files)) {
      files[basename(files) %in% txt$file]
    } else {
      files
    }
    filesToCheck <- filesToCheck[file.exists(filesToCheck)] # remove non existing files
    filesToCheck <- filesToCheck[!dir.exists(filesToCheck)] # remove directories

    if (!is.null(txt$algorithm)) {
      if (!write) {
        dots$algo <- unique(txt[txt$file %in% basename(filesToCheck),"algorithm"])
        dots$algo <- na.omit(dots$algo)[1]
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
      message(crayon::magenta("  Not possible to use quickCheck;\n ",
                              "    CHECKSUMS.txt file does not have filesizes", sep = ""))
    }
    checksums <- rep(list(rep("", length(filesToCheck))), 2)
    if (quickCheck | write) {
      checksums[[2]] <- do.call(.digest,
                                args = append(list(file = filesToCheck, quickCheck = TRUE),
                                              dots))
    }

    if (!quickCheck | write) {
      checksums[[1]] <- do.call(.digest,
                                args = append(list(file = filesToCheck, quickCheck = FALSE),
                                              dots))
    }
    message(crayon::magenta("Finished checking local files.", sep = ""))

    out <- if (length(filesToCheck)) {
      data.frame(file = basename(filesToCheck), checksum = checksums[[1]],
                 filesize = checksums[[2]], algorithm = dots$algo, stringsAsFactors = FALSE)
    } else {
      data.frame(file = character(0), checksum = character(0), filesize = character(0),
                 algorithm = character(0), stringsAsFactors = FALSE)
    }

    if (write) {
      writeChecksumsTable(out, checksumFile, dotsWriteTable)
      txt <- txtRead
      txt <- dplyr::right_join(txt, out)
      # wh <- match(txt$file, basename(filesToCheck))
      # wh <- na.omit(wh)
      # if (length(wh) > 0) {
      #   txt[wh,"checksum"] <- checksums[[1]]
      #   txt[wh,"filesize"] <- checksums[[2]]
      # }
      # txt <- txt[wh,]

    }
    results.df <- out %>%
      dplyr::mutate(actualFile = file) %>%
      {
        if (write) {
          dplyr::right_join(txt, ., by = "file")
        } else {
          dplyr::left_join(txt, ., by = "file")
        }
      } %>%
      dplyr::rename(expectedFile = file) %>%
      dplyr::group_by(expectedFile) %>%
      {
        if (quickCheck) {
          mutate(., result = ifelse(filesize.x != filesize.y, "FAIL", "OK"))
        } else {
          mutate(., result = ifelse(checksum.x != checksum.y, "FAIL", "OK"))
        }
      } %>%
      dplyr::arrange(desc(result)) %>%
      {
        #if (quickCheck) {
        select(
          .,
          "result",
          "expectedFile",
          "actualFile",
          "checksum.x",
          "checksum.y",
          "algorithm.x",
          "algorithm.y",
          "filesize.x",
          "filesize.y"
        )
        #} else {
        #  select(., "result", "expectedFile", "actualFile", "checksum.x", "checksum.y",
        #         "algorithm.x", "algorithm.y", "filesize.x", "filesize.y")
        #}
      } %>%
      dplyr::filter(row_number() == 1L)

      return(invisible(results.df))
    #}
  })

#' @rdname Checksums
setMethod(
  "Checksums",
  signature = c(path = "character", quickCheck = "ANY",
                write = "missing", files = "ANY"),
  definition = function(path, quickCheck, checksumFile, files, ...) {
    Checksums(path, write = FALSE, quickCheck = quickCheck, checksumFile = checksumFile,
              files = files, ...)
  })


writeChecksumsTable <- function(out, checksumFile, dots) {
  do.call(write.table,
          args = append(list(x = out, file = checksumFile, eol = "\n",
                             col.names = !isTRUE(dots$append),
                             row.names = FALSE),
                        dots))
}


#' Calculate the hashes of multiple files
#'
#' Internal function. Wrapper for \code{\link[digest]{digest}} using \code{xxhash64}.
#'
#' @param file  Character vector of file paths.
#' @param ...   Additional arguments to \code{digest::digest}.
#'
#' @return A character vector of hashes.
#'
#' @importFrom digest digest
#' @keywords internal
#' @rdname digest
#'
#' @author Alex Chubaty
#'
setGeneric(".digest", function(file, quickCheck, ...) {
  standardGeneric(".digest")
})

#' @rdname digest
setMethod(
  ".digest",
  signature = c(file = "character"),
  definition = function(file, quickCheck, algo = "xxhash64", ...) {
    if (quickCheck) {
      file.size(file) %>% as.character() # need as.character for empty case
    } else {
      lapply(file, function(f) {
        digest::digest(object = f, file = TRUE, algo = algo, ...)
      }) %>% unlist() %>% unname() %>% as.character() # need as.character for empty case # nolint
    }
  })
