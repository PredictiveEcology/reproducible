#' Use 'RoboCopy' on Windows to intelligently copy files
#'
#' @param from source directory
#' @param to   destination directory
#' @param options RoboCopy options (default \code{c("xo", "e")}).
#' @param files files to copy
robocopy <- function(from, to, options = c("xo", "e"), files) {
  if (Sys.info()["sysname"] == "Windows") {
    system(paste("robocopy", normalizePath(from),
                 normalizePath(to), paste(paste0("/", options), collapse = " ")))
  } else {
    stop("RoboCopy is only available on Windows. On other operating systems, use 'rsync' if it's available instead.")
  }
}
