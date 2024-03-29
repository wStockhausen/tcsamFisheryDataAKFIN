% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/akfin.AllConversions.R
\name{akfinConvert_GearDescToCode}
\alias{akfinConvert_GearDescToCode}
\title{Convert AKFIN gear descriptions to gear codes}
\usage{
akfinConvert_GearDescToCode(x)
}
\arguments{
\item{x}{: vector of gear descriptions to convert to gear codes}
}
\value{
character vector with same length as x
}
\description{
Function to convert gear descriptions to gear codes.
}
\details{
Uses conversions from \code{description} to \code{code}
defined by \code{\link[=akfinGet_GearConversions]{akfinGet_GearConversions()}}.
}
