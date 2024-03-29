% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/preProcess.R
\name{linkOrCopy}
\alias{linkOrCopy}
\title{Hardlink, symlink, or copy a file}
\usage{
linkOrCopy(
  from,
  to,
  symlink = TRUE,
  overwrite = TRUE,
  verbose = getOption("reproducible.verbose", 1)
)
}
\arguments{
\item{from, to}{Character vectors, containing file names or paths.
\code{to} can alternatively be the path to a single existing directory.}

\item{symlink}{Logical indicating whether to use symlink (instead of hardlink).
Default \code{FALSE}.}

\item{overwrite}{Logical. Should downloading and all the other actions occur
even if they pass the checksums or the files are all there.}

\item{verbose}{Numeric, -1 silent (where possible), 0 being very quiet,
1 showing more messaging, 2 being more messaging, etc.
Default is 1. Above 3 will output much more information about the internals of
Caching, which may help diagnose Caching challenges. Can set globally with an
option, e.g., \verb{options('reproducible.verbose' = 0) to reduce to minimal}}
}
\value{
This function is called for its side effects, which will be a \code{file.link} is that
is available or \code{file.copy} if not (e.g., the two directories are not on the
same physical disk).
}
\description{
Attempt first to make a hardlink. If that fails, try to make
a symlink (on non-windows systems and \code{symlink = TRUE}).
If that fails, copy the file.
}
\note{
Use caution with files-backed objects (e.g., rasters). See examples.
}
\examples{

tmpDir <- file.path(tempdir(), "symlink-test")
tmpDir <- normalizePath(tmpDir, winslash = "/", mustWork = FALSE)
dir.create(tmpDir)

f0 <- file.path(tmpDir, "file0.csv")
write.csv(iris, f0)

d1 <- file.path(tmpDir, "dir1")
dir.create(d1)
write.csv(iris, file.path(d1, "file1.csv"))

d2 <- file.path(tmpDir, "dir2")
dir.create(d2)
f2 <- file.path(tmpDir, "file2.csv")

## create link to a file
linkOrCopy(f0, f2)
file.exists(f2) ## TRUE
identical(read.table(f0), read.table(f2)) ## TRUE

## deleting the link shouldn't delete the original file
unlink(f0)
file.exists(f0) ## FALSE
file.exists(f2) ## TRUE

if (requireNamespace("terra", quietly = TRUE)) {
  ## using spatRasters and other file-backed objects
  f3a <- system.file("ex/test.grd", package = "terra")
  f3b <- system.file("ex/test.gri", package = "terra")
  r3a <- terra::rast(f3a)
  f4a <- file.path(tmpDir, "raster4.grd")
  f4b <- file.path(tmpDir, "raster4.gri")
  linkOrCopy(f3a, f4a) ## hardlink
  linkOrCopy(f3b, f4b) ## hardlink
  r4a <- terra::rast(f4a)

  isTRUE(all.equal(r3a, r4a)) # TRUE

  ## cleanup
  unlink(tmpDir, recursive = TRUE)
}
}
\seealso{
\code{\link[=file.link]{file.link()}}, \code{\link[=file.symlink]{file.symlink()}}, \code{\link[=file.copy]{file.copy()}}.
}
\author{
Alex Chubaty and Eliot McIntire
}
