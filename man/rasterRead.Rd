% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{rasterRead}
\alias{rasterRead}
\title{A helper to \code{getOption("reproducible.rasterRead")}}
\usage{
rasterRead(...)
}
\arguments{
\item{...}{Passed to the function parsed and evaluated from
\code{getOption("reproducible.rasterRead")}}
}
\value{
A function, that will be the evaluated, parsed character
string, e.g., \code{eval(parse(text = "terra::rast"))}
}
\description{
A helper to \code{getOption("reproducible.rasterRead")}
}
