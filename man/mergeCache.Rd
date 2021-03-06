% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cache-tools.R
\name{mergeCache}
\alias{mergeCache}
\alias{mergeCache,ANY-method}
\title{Merge two cache repositories together}
\usage{
mergeCache(
  cacheTo,
  cacheFrom,
  drvTo = getOption("reproducible.drv", RSQLite::SQLite()),
  drvFrom = getOption("reproducible.drv", RSQLite::SQLite()),
  connTo = NULL,
  connFrom = NULL
)

\S4method{mergeCache}{ANY}(
  cacheTo,
  cacheFrom,
  drvTo = getOption("reproducible.drv", RSQLite::SQLite()),
  drvFrom = getOption("reproducible.drv", RSQLite::SQLite()),
  connTo = NULL,
  connFrom = NULL
)
}
\arguments{
\item{cacheTo}{The cache repository (character string of the file path)
that will become larger, i.e., merge into this}

\item{cacheFrom}{The cache repository (character string of the file path)
from which all objects will be taken and copied from}

\item{drvTo}{The database driver for the \code{cacheTo}.}

\item{drvFrom}{The database driver for the \code{cacheFrom}}

\item{connTo}{The connection for the \code{cacheTo}. If not provided, then
a new one will be made from \code{drvTo} and \code{cacheTo}}

\item{connFrom}{The database for the \code{cacheFrom}. If not provided, then
a new one will be made from \code{drvFrom} and \code{cacheFrom}}
}
\value{
The character string of the path of \code{cacheTo}, i.e., not the
objects themselves.
}
\description{
\if{html}{\figure{lifecycle-experimental.svg}{options: alt="experimental"}}
}
\details{
All the \code{cacheFrom} artifacts will be put into \code{cacheTo}
repository. All \code{userTags} will be copied verbatim, including
\code{accessed}, with 1 exception: \code{date} will be the
current \code{Sys.time()} at the time of merging. The
\code{createdDate} column will be similarly the current time
of merging.
}
