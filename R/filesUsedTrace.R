#' List the files under given folders that an R script opens
#'
#' Runs `script` with `Rscript` under `strace` (Linux; `strace` must be installed)
#' and reports every existing regular file under any of `folders` that the run
#' (or any process it started) opened or looked at. Use it to stage a run on a
#' machine without internet: copy only the inputs and cache files it lists.
#' A trace from an earlier run can be given as `traceFile` instead of running
#' the script again.
#'
#' @param script Path to an R script, run as `Rscript script args`.
#' @param args Character vector of arguments passed to the script.
#' @param folders Character vector of folders. Only files under these are
#'   reported. Symbolic links in `folders` and in the traced paths are resolved,
#'   so a folder reached through a link and the folder itself are the same.
#' @param traceFile Path to an existing `strace` output file written with
#'   `strace -f -e trace=%file -o traceFile ...`. If given, `script` and `args`
#'   are not used and `strace` is not needed.
#'
#' @return A `data.frame` with one row per file, sorted by `path`: `path`
#'   (the resolved file path), `folder` (the element of `folders`, resolved,
#'   that the file is under) and `size` (bytes).
#'
#' @details
#' A trace line is kept when the call did not fail (no `= -1 E...` result);
#' every double-quoted absolute path on the line is a candidate. Relative paths,
#' paths that no longer exist and directories are dropped. Files opened through
#' a file descriptor opened earlier (e.g. `read()`) are not file operations and
#' need no separate handling: the `open` that preceded them is in the trace.
#'
#' @export
#' @examplesIf nzchar(Sys.which("strace"))
#' inputs <- file.path(tempdir(), "filesUsedTrace-inputs")
#' dir.create(inputs, showWarnings = FALSE)
#' writeLines("a", file.path(inputs, "used.txt"))
#' writeLines("b", file.path(inputs, "unused.txt"))
#' script <- tempfile(fileext = ".R")
#' writeLines(sprintf("readLines('%s')", file.path(inputs, "used.txt")), script)
#' filesUsedTrace(script, folders = inputs)
#' unlink(c(inputs, script), recursive = TRUE)
filesUsedTrace <- function(script, args = character(), folders, traceFile = NULL) {
  if (is.null(traceFile)) {
    if (!nzchar(Sys.which("strace")))
      stop("filesUsedTrace() needs 'strace' (Linux), which was not found on the PATH; ",
           "install it (e.g. 'apt install strace') or pass an existing trace as 'traceFile'.")
    traceFile <- tempfile("filesUsedTrace-", fileext = ".txt")
    on.exit(unlink(traceFile), add = TRUE)
    status <- system2("strace",
                      c("-f", "-e", "trace=%file", "-o", shQuote(traceFile),
                        "Rscript", shQuote(script), shQuote(args)))
    if (!identical(status, 0L))
      warning("Rscript exited with status ", status, "; the file list may be incomplete.")
  }
  folders <- normalizePath(folders, mustWork = TRUE)
  lines <- readLines(traceFile, warn = FALSE)
  lines <- lines[!grepl("= -1 E", lines, fixed = TRUE)]
  paths <- unlist(regmatches(lines, gregexpr('"/[^"]*"', lines)), use.names = FALSE)
  paths <- unique(substr(paths, 2L, nchar(paths) - 1L))
  paths <- unique(normalizePath(paths[file.exists(paths)]))
  paths <- paths[!dir.exists(paths)]
  prefixes <- paste0(folders, "/")
  folder <- vapply(paths, function(p) {
    hit <- which(startsWith(p, prefixes))
    if (length(hit)) folders[hit[1L]] else NA_character_
  }, character(1), USE.NAMES = FALSE)
  keep <- !is.na(folder)
  paths <- paths[keep]
  out <- data.frame(path = paths, folder = folder[keep],
                    size = file.size(paths), stringsAsFactors = FALSE)
  out <- out[order(out$path), , drop = FALSE]
  rownames(out) <- NULL
  out
}
