% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DBI.R
\name{createCache}
\alias{createCache}
\alias{loadFromCache}
\alias{rmFromCache}
\alias{CacheDBFile}
\alias{CacheStorageDir}
\alias{CacheStoredFile}
\alias{CacheDBTableName}
\alias{CacheIsACache}
\title{Functions to create and work with a cache}
\usage{
createCache(
  cachePath = getOption("reproducible.cachePath"),
  drv = getDrv(getOption("reproducible.drv", NULL)),
  conn = getOption("reproducible.conn", NULL),
  force = FALSE,
  verbose = getOption("reproducible.verbose")
)

loadFromCache(
  cachePath = getOption("reproducible.cachePath"),
  cacheId,
  preDigest,
  fullCacheTableForObj = NULL,
  format = getOption("reproducible.cacheSaveFormat", "rds"),
  .functionName = NULL,
  .dotsFromCache = NULL,
  drv = getDrv(getOption("reproducible.drv", NULL)),
  conn = getOption("reproducible.conn", NULL),
  verbose = getOption("reproducible.verbose")
)

rmFromCache(
  cachePath = getOption("reproducible.cachePath"),
  cacheId,
  drv = getDrv(getOption("reproducible.drv", NULL)),
  conn = getOption("reproducible.conn", NULL),
  format = getOption("reproducible.cacheSaveFormat", "rds")
)

CacheDBFile(
  cachePath = getOption("reproducible.cachePath"),
  drv = getDrv(getOption("reproducible.drv", NULL)),
  conn = getOption("reproducible.conn", NULL)
)

CacheStorageDir(cachePath = getOption("reproducible.cachePath"))

CacheStoredFile(
  cachePath = getOption("reproducible.cachePath"),
  cacheId,
  format = NULL,
  obj = NULL
)

CacheDBTableName(
  cachePath = getOption("reproducible.cachePath"),
  drv = getDrv(getOption("reproducible.drv", NULL))
)

CacheIsACache(
  cachePath = getOption("reproducible.cachePath"),
  create = FALSE,
  drv = getDrv(getOption("reproducible.drv", NULL)),
  conn = getOption("reproducible.conn", NULL)
)
}
\arguments{
\item{cachePath}{A path describing the directory in which to create
the database file(s)}

\item{drv}{A driver, passed to \code{dbConnect}}

\item{conn}{an optional DBIConnection object, as returned by dbConnect().}

\item{force}{Logical. Should it create a cache in the \code{cachePath},
even if it already exists, overwriting.}

\item{verbose}{Numeric, -1 silent (where possible), 0 being very quiet,
1 showing more messaging, 2 being more messaging, etc.
Default is 1. Above 3 will output much more information about the internals of
Caching, which may help diagnose Caching challenges. Can set globally with an
option, e.g., \verb{options('reproducible.verbose' = 0) to reduce to minimal}}

\item{cacheId}{The cacheId or otherwise digested hash value, as character string.}

\item{preDigest}{The list of \code{preDigest} that comes from \code{CacheDigest} of an object}

\item{fullCacheTableForObj}{The result of \code{showCache}, but subsetted for only
the \code{cacheId} being loaded or selected}

\item{format}{The text string representing the file extension used normally by
different save formats; currently only \code{"rds"} or \code{"qs"}. Defaults
to \code{getOption("reproducible.cacheSaveFormat", "rds")}}

\item{.functionName}{Optional. Used for messaging when this function is called from \code{Cache}}

\item{.dotsFromCache}{Optional. Used internally.}

\item{obj}{The optional object that is of interest; it may have an attribute "saveRawFile"
that would be important.}

\item{create}{Logical. Currently only affects non RQSLite default drivers. If this
is \code{TRUE} and there is no Cache database, the function will create one.}
}
\value{
\code{createCache} does not return a value; it is called for side effects.

\code{loadFromCache} returns the object from the cache that has the particular \code{cacheId}.

\code{rmFromCache} has no return value; it is called for its side effects.

\code{CacheDBFile} returns the name of the database file for a given Cache,
when \code{useDBI() == FALSE}, or \code{NULL} if \code{TRUE}.
\code{CacheDBFiles} (i.e,. plural) returns the name of all the database files for
a given Cache when \code{useDBI() == TRUE}, or \code{NULL} if \code{FALSE}

\code{CacheStorageDir} returns the name of the directory where cached objects are
stored.

\code{CacheStoredFile} returns the name of the file in which the cacheId object is stored.
This can be loaded to memory with e.g., \code{loadFile}.

\code{CacheDBTableName} returns the name of the table inside the SQL database, if that
is being used.

\code{CacheIsACache} returns a logical indicating whether the \code{cachePath} is currently
a \code{reproducible} cache database.
}
\description{
These are not intended for normal use.
}
\details{
\code{createCache} function will create a Cache folder structure and necessary files, based on
the particular \code{drv} or \code{conn} provided.

\code{loadFromCache} is a function to get a single object from the cache, given its \code{cacheId}.

\code{rmFromCache} removes one or more items from the cache, and updates the cache
database files.

\code{CacheStoredFile} returns the file path to the file with the specified hash value.

\code{CacheStoredFile} returns the file path to the file with the specified hash value.

\code{CacheIsACache} returns a logical of whether the specified \code{cachePath}
is actually a functioning cache.
}
\examples{
data.table::setDTthreads(2)
newCache <- tempdir2()
createCache(newCache)

out <- Cache(rnorm(1), cachePath = newCache)
cacheId <- gsub("cacheId:", "", attr(out, "tags"))
loadFromCache(newCache, cacheId = cacheId)

rmFromCache(newCache, cacheId = cacheId)

# clean up
unlink(newCache, recursive = TRUE)

data.table::setDTthreads(2)
newCache <- tempdir2()

# Given the drv and conn, creates the minimum infrastructure for a cache
createCache(newCache)

CacheDBFile(newCache) # identifies the database file
CacheStorageDir(newCache) # identifies the directory where cached objects are stored

out <- Cache(rnorm(1), cachePath = newCache)
cacheId <- gsub("cacheId:", "", attr(out, "tags"))
CacheStoredFile(newCache, cacheId = cacheId)

# The name of the table inside the SQL database
CacheDBTableName(newCache)

CacheIsACache(newCache) # returns TRUE

# clean up
unlink(newCache, recursive = TRUE)
}
