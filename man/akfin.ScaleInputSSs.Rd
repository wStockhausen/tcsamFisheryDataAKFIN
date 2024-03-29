% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/akfin.ScaleInputSSs.R
\name{akfin.ScaleInputSSs}
\alias{akfin.ScaleInputSSs}
\title{Calculate the "input" sample sizes to TCSAM02 for AKFIN size composition data}
\usage{
akfin.ScaleInputSSs(dfrSS, .ss_scl, .ss_max = 200)
}
\arguments{
\item{dfrSS}{: dataframe with original sample sizes (see details)}

\item{.ss_scl}{: scale factor for sample sizes from \code{adfg.calcScaleFactorForInputSSs}}

\item{.ss_max}{: maximum input sample size allowed}
}
\value{
dataframe or tibble with the same column order as the input, but with
additional columns \code{ss_orig}, \code{ss_tot}, and \code{ss_scl}. Column \code{ss} contains the scaled
sample sizes, column \code{ss_scl} contains the scale used to convert the original
sample sizes, and column \code{ss_orig} contains the original sample sizes.
}
\description{
Function to calculate the "input" sample sizes to TCSAM02 for AKFIN size composition data.
}
\details{
The scale factor \code{ss_scl} and max input sample size \code{ss_max} are
used to determine input annual sample sizes for size composition data using the formula \cr
\code{inp_ss = min(max_ss,max_ss\*(ss_tot/ss_scl)\*(ss/ss_tot)} \cr
where \code{ss} is the original sample size (the total number of crab measured) by factor
combination and \code{ss_tot} is the total sample size by year.
}
\note{
\code{dfrSS} should have (at least) the columns
\itemize{
\item{year}
\item{ss}
}

\code{dfrSS} should include sample sizes for only a single fishery or survey.
}
