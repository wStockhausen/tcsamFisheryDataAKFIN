% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/akfin.AllConversions.R
\name{akfinConvert_GearCodeToType}
\alias{akfinConvert_GearCodeToType}
\title{Convert gear codes to gear types}
\usage{
akfinConvert_GearCodeToType(x)
}
\arguments{
\item{x}{: vector of gear codes to convert to gear types}
}
\value{
character vector with same length as x
}
\description{
Function to convert gear codes to gear types.
}
\details{
Uses conversions from \code{code} to \code{type}
defined by \code{\link[=akfinGet_GearConversions]{akfinGet_GearConversions()}}.
}
