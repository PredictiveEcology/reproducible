#' The \code{reproducible} package
#'
#' @description
#' \if{html}{\figure{lifecycle-maturing.svg}{options: alt="maturing"}}
#'
#' This package aims at making
#' high-level, robust, machine and OS independent tools for making deeply
#' reproducible and reusable content in R. The core user functions are \code{Cache}
#' and \code{prepInputs}. Each of these is built around many core and edge cases
#' required to have deeply reproducible code.
#'
#' @section Main Tools:
#'
#' There are many elements within the reproducible package.
#' However, there are currently three main ones that are critical for reproducible research.
#' The key element for reproducible research is that the code must always return the same content
#' every time it is run, but it must be vastly faster the 2nd, 3rd, 4th etc, time it is run.
#' That way, the entire code sequence for a project of arbitrary size can be run
#' \emph{from the start} every time.
#'
#' \describe{
#'   \item{\code{Cache}:}{A robust wrapper for any function, including those with environments,
#'   disk-backed storage (currently on \code{Raster}) class), operating-system independent,
#'   whose first time called will execute the function, second time will compare the inputs to a
#'   database of entries, and recover the first result if inputs are identical.
#'   If \code{options("reproducible.useMemoise" = TRUE)}, the third time will be very fast as it
#'   will recover the answer from RAM.}
#'   \item{\code{prepInputs}:}{ Download, or load objects, and possibly post-process them.
#'   The main advantage to using this over more direct routes is that it will automatically build
#'   checksums tables, use \code{Cache} internally where helpful, and possibly run a variety of
#'   post-processing actions.
#'   This means this function can also itself be cached for even more speed.
#'   This allows all project data to be stored in custom cloud locations or in their original online
#'   data repositories, \code{without altering code} between the first, second, third, etc., times
#'   the code is run.}
#' }
#'
#' @section Package options:
#'
#' See \code{\link{reproducibleOptions}} for a complete description of package
#' \code{\link{options}} to configure behaviour.
#'
#' @import methods
"_PACKAGE"

