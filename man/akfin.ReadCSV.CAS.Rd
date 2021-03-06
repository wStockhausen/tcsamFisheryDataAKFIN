% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/akfin.ReadCSVs.R
\name{akfin.ReadCSV.CAS}
\alias{akfin.ReadCSV.CAS}
\title{Read a csv file from AKFIN with CAS (Catch Accounting System) data}
\usage{
akfin.ReadCSV.CAS(fn, bycatch_species_code = "BTCR", minYear = 1991)
}
\arguments{
\item{fn}{- filename for bycatch estimates from the Catch Accounting System database}

\item{bycatch_species_code}{- FMA species code (default = "BTCR", for bairdi Tanner crab)}

\item{minYear}{- min fishery year to extract}
}
\value{
tibble with columns "year","gear","area","target","num",and "wgt". 
Units for biomass (\code{wgt}) and number (\code{num}) are kg and one's (i.e., unscaled number of crab).
}
\description{
Function to read a csv file from AKFIN with CAS (Catch Accounting System) data.
}
\details{
Valid CAS data begins in 1991 and ends in 2009 (it overlaps with the CIA database a bit)
}
