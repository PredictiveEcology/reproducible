% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DBI.R
\name{.isMemoised}
\alias{.isMemoised}
\title{Evaluate whether a cacheId is memoised}
\usage{
.isMemoised(cacheId, cachePath = getOption("reproducible.cachePath"))
}
\arguments{
\item{cacheId}{Character string. If passed, this will override the calculated hash
of the inputs, and return the result from this cacheId in the \code{cachePath}.
Setting this is equivalent to manually saving the output of this function, i.e.,
the object will be on disk, and will be recovered in subsequent
This may help in some particularly finicky situations
where Cache is not correctly detecting unchanged inputs. This will guarantee
the object will be identical each time; this may be useful in operational code.}

\item{cachePath}{A repository used for storing cached objects.
This is optional if \code{Cache} is used inside a SpaDES module.}
}
\value{
A logical, length 1 indicating whether the \code{cacheId} is memoised.
}
\description{
Intended for internal use. Exported so other packages can use this function.
}
