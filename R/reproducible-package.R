#' The \code{reproducible} package
#'
#' Built on top of \pkg{git2r} and \pkg{archivist}, this package aims at making
#' high-level, robust, machine and OS independent tools for making deeply
#' reproducible and reusable content in R. This extends beyond the package
#' management utilities of \pkg{packrat} and \pkg{checkpoint} by including
#' tools for caching, and accessing GitHub repositories.
#'
#' @section Main Tools:
#'
#' There are many elements within the reproducible package. However, there are currently
#' 3 main ones that are critical for reproducible research. The key element for reproducible
#' research is that the code must always return the same content every time it is run,
#' but it must be vastly faster the 2nd, 3rd, 4th etc, time it is run. That way, the entire
#' code sequence for a project of arbitrary size can be run \code{from the start} every time.
#'
#' \tabular{lcl}{
#'   \emph{Function} \tab \emph{Objective} \cr
#'   \code{Cache} \tab A robust wrapper for any function, including those
#'                     with environments, disk-backed storage (currently on \code{Raster})
#'                     class), operating-system independent, whose first time
#'                     called will execute the function, 2nd time will compare the inputs
#'                     to a an SQLite database of entries, and recover the first result
#'                     if inputs are identical. If \code{options("reproducible.useMemoise" = TRUE)},
#'                     the third time will be very fast as it will recover the answer from
#'                     RAM\cr
#'   \code{prepInputs} \tab A function to download, or load objects, and possible post process
#'                     them. The main advantage to using this over more direct routes is
#'                     that it will automatically build checksums tables, use \code{Cache}
#'                     internally where helpful, and possibly run a variety of post processing
#'                     actions. This means this function can also itself be cached for even
#'                     more speed. This allows all project data to be stored in custom cloud
#'                     locations or in their original online data repositories,
#'                     \code{without altering code} between the first, second, third etc. times
#'                     the code is run.\cr
#'   \code{Require} \tab A verion of \code{require} that incorporates elements of
#'                    \code{install.packages}, \code{devtools::install_github}, \code{packrat}.
#'                    It allows for users code to work for a new user on a new machine that
#'                    may or may not have all packages installed.\cr
#' }
#'
#' @section Package options:
#'
#' \code{reproducible} has the following \code{\link{options}} to configure behaviour.
#' See \code{\link{reproducibleOptions}}
#'
#' @import methods
#' @importFrom Rcpp evalCpp
#' @useDynLib reproducible
"_PACKAGE"
