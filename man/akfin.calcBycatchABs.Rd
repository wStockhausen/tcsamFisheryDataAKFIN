% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/akfin.CalcFunctions.R
\name{akfin.calcBycatchABs}
\alias{akfin.calcBycatchABs}
\title{Calculate estimated total bycatch abundance and biomass of Tanner crab in groundfish fisheries}
\usage{
akfin.calcBycatchABs(
  maxYear = 2019,
  fnHIS = "GroundfishFisheries.HistoricalBycatchBiomass.TannerCrab.csv",
  fnCAS = "FromAKFIN.TannerCrab.BycatchEstimates.CAS.csv",
  fnCIA = "FromAKFIN.TannerCrab.BycatchEstimates.CIA.csv",
  verbosity = 0
)
}
\arguments{
\item{maxYear}{- max fishery year to extract}

\item{fnHIS}{- filename for bycatch estimates from "historical data" (pre-CAS)}

\item{fnCAS}{- filename for bycatch estimates from Catch Accounting System}

\item{fnCIA}{- filename for bycatch estimates from Catch-In_Areas database}

\item{verbosity}{- integer flag for diagnostic printout (0,1,2)}
}
\value{
tibble with columns
 * fishery
 * area
 * year
 * expFactor - expansion factor
 * sex - "male", "female", "undetermined", "hermaphrodite", or NA
 * variable - "count", "weight", "abundance", or "biomass"
 * value - value of associated variable
 * type - "observed" or "expanded"
 * units - "ones", "thousands", "kg" or "t"
}
\description{
Function to calculate estimated total catch abundance by expanding observed abundance.
}
\details{
Uses \code{sqldf::sqldf}. Units for 'weight' are kg, for 'abundance' are thousands, and for 'biomass' are t.
Historical (foreign, joint veture) data runs 1973-1990. CAS data is used for 1991-2008. CIA data is used for
2009+.
}