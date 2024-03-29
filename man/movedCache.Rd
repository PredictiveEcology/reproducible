% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DBI.R
\name{movedCache}
\alias{movedCache}
\title{Deal with moved cache issues}
\usage{
movedCache(
  new,
  old,
  drv = getDrv(getOption("reproducible.drv", NULL)),
  conn = getOption("reproducible.conn", NULL),
  verbose = getOption("reproducible.verbose")
)
}
\arguments{
\item{new}{Either the path of the new \code{cachePath} where the cache was moved or copied to, or
the new DB Table Name}

\item{old}{Optional, if there is only one table in the \code{new} cache path.
Either the path of the previous \code{cachePath} where the cache was moved or copied from, or
the old DB Table Name}

\item{drv}{if using a database backend, drv must be an object that
inherits from DBIDriver e.g., from package RSQLite, e.g., SQLite}

\item{conn}{an optional DBIConnection object, as returned by dbConnect().}

\item{verbose}{Numeric, -1 silent (where possible), 0 being very quiet,
1 showing more messaging, 2 being more messaging, etc.
Default is 1. Above 3 will output much more information about the internals of
Caching, which may help diagnose Caching challenges. Can set globally with an
option, e.g., \verb{options('reproducible.verbose' = 0) to reduce to minimal}}
}
\value{
\code{movedCache} does not return anything; it is called for its side effects.
}
\description{
If a user manually copies a complete Cache folder (including the db file and rasters folder),
there are issues that must be addressed, depending on the Cache backend used.
If using DBI (e.g., RSQLite or Postgres), the db table must be renamed. Run
this function after a manual copy of a cache folder. See examples for one way to do that.
}
\details{
When the backend database for a \code{reproducinle} cache is an SQL database, the files
on disk cannot be copied manually to a new location because they contain internal
tables. Because \code{reproducible} gives the main table a name based on the \code{cachePath}
path, calls to \code{Cache} will attempt to call this internally if it detects a
name mismatch.
}
\examples{
data.table::setDTthreads(2)
tmpdir <- "tmpdir"
tmpCache <- "tmpCache"
tmpCacheDir <- normalizePath(file.path(tempdir(), tmpCache), mustWork = FALSE)
tmpdirPath <- normalizePath(file.path(tempdir(), tmpdir), mustWork = FALSE)
bb <- Cache(rnorm, 1, cachePath = tmpCacheDir)

# Copy all files from tmpCache to tmpdir
froms <- normalizePath(dir(tmpCacheDir, recursive = TRUE, full.names = TRUE),
  mustWork = FALSE
)
dir.create(file.path(tmpdirPath, "rasters"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(tmpdirPath, "cacheOutputs"), recursive = TRUE, showWarnings = FALSE)
file.copy(
  from = froms, overwrite = TRUE,
  to = gsub(tmpCache, tmpdir, froms)
)

# Can use 'movedCache' to update the database table, though will generally
#   happen automatically, with message indicating so
movedCache(new = tmpdirPath, old = tmpCacheDir)
bb <- Cache(rnorm, 1, cachePath = tmpdirPath) # should recover the previous call

}
