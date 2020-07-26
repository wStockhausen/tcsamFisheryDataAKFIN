#'
#' @title Calculate bycatch tables for Tanner crab in groundfish fisheries
#'
#' @description Function to calculate bycatch tables for Tanner crab in groundfish fisheries.
#'
#' @param maxYear - max fishery year to extract
#' @param dirData - path to "current" AKFIN data directory
#' @param fnHisABs - filename for bycatch estimates (abundance, biomass) from "historical data" (pre-CAS)
#' @param fnCAS - filename for bycatch estimates (abundance, biomass) from Catch Accounting System
#' @param fnCIA - filename for bycatch estimates (abundance, biomass) from Catch-In_Areas database
#' @param fnHisZCs - filename for size composition estimates (numbers of crab) from "historical data" (pre-CAS)
#' @param fnNORPAC - filename for size data (numbers of crab) from a NORPAC length report file
#' @param verbosity - integer flag for diagnostic printout (0,1,2)
#'
#' @return list with the following tibbles: \cr
#' \itemize{
#' \item{dfrABs.YGAT - total bycatch abundance, biomass by YGAT}
#' \item{dfrSSs.YGAX - "raw" sample sizes for size compositions by YGAX}
#' \item{dfrZCs.onYGAXZ - "raw" size compositions by YGAXZ}
#' \item{dfrZCs.tnYGAXZ - total catch (if possible) size compositions by YGAXZ}
#' }
#'
#' @details Historical (foreign, joint veture) biomass data runs 1973-1990 (no abundance data 
#' is available during this time period). CAS data is used for 1991-2008. CIA data is used for
#' 2009+. Historical size composition data runs 1973-1990. NORPAC size composition data starts
#' (effectively) in 1986, but is used here starting in 1991. 
#'
#' @export
#'
akfin.calcCatchTables<-function(maxYear=2019,
                                dirData="~/Work/StockAssessments-Crab/Data/Fisheries/Fishery.AKFIN/Current",
                                fnHisABs="GroundfishFisheries.HistoricalABss.TannerCrab.csv",
                                fnCAS="FromAKFIN.TannerCrab.BycatchEstimates.CAS.csv",
                                fnCIA="FromAKFIN.TannerCrab.BycatchEstimates.CIA.csv",
                                fnHisZCs="GroundfishFisheries.HistoricalZCs.LongFormat.TannerCrab.csv",
                                fnNORPAC="FromAKFIN.TannerCrab.norpac_length_report.csv",
                                verbosity=0){
  #flags for printing diagnostic info
  verbose<-(verbosity>0);

  #--Calculate total bycatch by year, area, gear, and target
  dfrABs.YGAT<-akfin.calcBycatchABs(maxYear = maxYear,
                                    fnHIS = file.path(dirData,fnHisABs),
                                    fnCAS = file.path(dirData,fnCAS),
                                    fnCIA = file.path(dirData,fnCIA),
                                    verbosity=verbosity);
  
  #--Calculate "raw" size compositions
  dfrZCs.onYGAXZ<-akfin.calcUnscaledZCs(fnHisZCs=file.path(dirData,fnHisZCs),
                                        fnNORPAC=file.path(dirData,fnNORPAC));

  #--calculate observed sample sizes
  dfrSSs.YGAX<-wtsSizeComps::calcSampleSizes(dfrZCs.onYGAXZ,
                                             id.value="N",
                                             id.facs=c("year","gear","area","sex"));
  
  #--Calculate expanded size compositions
  dfrZCs.tnYGAXZ<-akfin.ScaleZCs(dfrABs.YGAT,dfrZCs.onYGAXZ);
  
  return(list(dfrABs.YGAT=dfrABs.YGAT,
              dfrSSs.YGAX=dfrSSs.YGAX,
              dfrZCs.onYGAXZ=dfrZCs.onYGAXZ,
              dfrZCs.tnYGAXZ=dfrZCs.tnYGAXZ));
}
