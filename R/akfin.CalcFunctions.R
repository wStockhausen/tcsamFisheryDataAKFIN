#'
#' @title Calculate estimated total bycatch abundance and biomass of Tanner crab in groundfish fisheries
#'
#' @description Function to calculate estimated total catch abundance by expanding observed abundance.
#'
#' @param maxYear - max fishery year to extract
#' @param fnHIS - filename for bycatch estimates from "historical data" (pre-CAS)
#' @param fnCAS - filename for bycatch estimates from Catch Accounting System
#' @param fnCIA - filename for bycatch estimates from Catch-In_Areas database
#' @param verbosity - integer flag for diagnostic printout (0,1,2)
#'
#' @return tibble with columns
#'  * fishery
#'  * area
#'  * year
#'  * expFactor - expansion factor
#'  * sex - "male", "female", "undetermined", "hermaphrodite", or NA
#'  * variable - "count", "weight", "abundance", or "biomass"
#'  * value - value of associated variable
#'  * type - "observed" or "expanded"
#'  * units - "ones", "thousands", "kg" or "t"
#'
#' @details Uses \code{sqldf::sqldf}. Units for 'weight' are kg, for 'abundance' are thousands, and for 'biomass' are t.
#' Historical (foreign, joint veture) data runs 1973-1990. CAS data is used for 1991-2008. CIA data is used for
#' 2009+.
#'
#' @import magrittr
#' @import dplyr
#' 
#' @importFrom sqldf sqldf
#' @importFrom wtsUtilities Sum
#'
#' @export
#'
akfin.calcBycatchABs<-function(maxYear=2019,
                                fnHIS="GroundfishFisheries.HistoricalBycatchBiomass.TannerCrab.csv",
                                fnCAS="FromAKFIN.TannerCrab.BycatchEstimates.CAS.csv",
                                fnCIA="FromAKFIN.TannerCrab.BycatchEstimates.CIA.csv",
                                verbosity=0){
  mnYrCAS = 1991;#--first year for reliable CAS data
  mnYrCIA = 2009;#--first year for reliable CIA data
  
  MILLION<-1000000;
  LBStoKG <- 0.45359237; #multiplication factor to get kg from lbs

  #flags for printing diagnostic info
  verbose<-(verbosity>0);
  
  #gear conversions
  dfr.gear<-akfin.GetGearConversions();
  #sex conversions
  dfr.sex<-akfin.GetSexConversions();

  Sum<-wtsUtilities::Sum;#sum function with na.rm=TRUE

  #--Calculate total bycatch abundance and biomass by year, area, gear, and target (if possible)
  #----combine historical, CAS, and CIA data
  #----final dataframe will have columns year, gear, area, target, num, wgt (kg)
  
  #----get historical data (biomass only, in millions lbs)
  tmp<-readr::read_csv(fnHIS);
  dfrHIS<-tmp %>% 
             dplyr::mutate(num=NA,wgt=`biomass (MLBS)`*(LBStoKG*MILLION)) %>%
             dplyr::select(!tidyselect::contains("biomass (MLBS)")) %>%
             subset(year<mnYrCAS);
  #------num values (absent) are in one's, wgt values in kg
  rm(tmp);
  
  #----process estimated bycatch numbers and weight by year, gear, and area from CAS/Blend database
  tmp<-akfin.ReadCSV.CAS(fnCAS);
  dfrCAS<-tmp %>% subset((mnYrCAS<=year)&(year<mnYrCIA));
  #------num values are in one's, wgt values in kg
  rm(tmp);
  
  #----process estimated bycatch numbers and weight by year, gear, area, and target from catch-in-areas (CIA) database
  tblCIA <-akfin.ReadCSV.CIA(fnCIA);
  #----aggregate by year, gear, nmfs stat area, target across ADFG stat areas
  query<-"select
            year, gear, `nmfs stat area`, target,
            sum(number) as num,
            sum(biomass) as wgt
          from tblCIA
          group by
            year,gear,`nmfs stat area`,target;";
  tmp1<-sqldf::sqldf(query);
  names(tmp1)[3]<-"area";
  dfrCIA<-tmp1 %>% subset((mnYrCIA<=year)&(year<=maxYear));
  #------num values are in one's, wgt values in kg
  rm(tmp1,tblCIA);
  
  #----combine HIS, CAS and CIA bycatch estimates
  dfr<-rbind(dfrHIS,dfrCAS,dfrCIA);
  rm(dfrHIS,dfrCAS,dfrCIA);
  names(dfr)[6]<-"wgt (kg)";
  return(dfr);
}
# dirData="~/Work/StockAssessments-Crab/Data/Fisheries/Fishery.AKFIN/Current";
# fnHisABs="GroundfishFisheries.HistoricalABs.TannerCrab.csv";
# fnCAS="FromAKFIN.TannerCrab.BycatchEstimates.CAS.csv";
# fnCIA="FromAKFIN.TannerCrab.BycatchEstimates.CIA.csv";
# tblABs<-akfin.calcBycatchABs(maxYear = 2019,
#                              fnHIS = file.path(dirData,fnHisABs),
#                              fnCAS = file.path(dirData,fnCAS),
#                              fnCIA = file.path(dirData,fnCIA));

#'
#' @title Calculate estimated total bycatch abundance and biomass of Tanner crab in groundfish fisheries
#'
#' @description Function to calculate estimated total catch abundance by expanding observed abundance.
#'
#' @param maxYear - max fishery year to extract
#' @param fnHisZCs - filename for size composition estimates (numbers of crab) from "historical data" (pre-CAS)
#' @param fnNORPAC - filename for size data (numbers of crab) from a NORPAC length report file
#' @param verbosity - integer flag for diagnostic printout (0,1,2)
#'
#' @return tibble with columns
#'
#' @details Uses \code{sqldf::sqldf}. Units for 'weight' are kg, for 'abundance' are thousands, and for 'biomass' are t.
#' Historical (foreign, joint veture) data runs 1973-1990. CAS data is used for 1991-2008. CIA data is used for
#' 2009+.
#'
#' @import magrittr
#' @import dplyr
#' 
#' @importFrom sqldf sqldf
#' @importFrom wtsSizeComps calcSizeComps
#'
#' @export
#'
akfin.calcUnscaledZCs<-function(maxYear=2019,
                                fnHisZCs=NULL,
                                fnNORPAC=NULL,
                                id.facs=c("year","gear","area","sex"),
                                cutpts=seq(25,185,5),
                                truncate.low=TRUE,
                                truncate.high=FALSE){
  #--Process historical ZCs
  dfrZCs.HIS<-NULL;
  if (!is.null(fnHisZCs)){
    tblHIS<-readr::read_csv(fnHisZCs);
    #--add gear, N columns, drop count column, keep only before 1991
    dfrZCs.HIS <- tblHIS %>% 
                dplyr::mutate(gear="undetermined",area="undetermined",N=count) %>% 
                dplyr::mutate(count=NULL) %>% 
                dplyr::relocate(gear,area,.after=year)  %>%
                subset(year<=1990);
  }

  #--Process NORPAC size compositions
  dfrZCs.NLR<-NULL;
  if (!is.null(fnNORPAC)){
    #--read recent NORPAC length report file
    tblNLR<-akfin.ReadCSV.NorpacLengthReport(fnNORPAC);
    
    #--keep after minYr-1, drop "unidentified" sex
    minYr = ifelse(!is.null(dfrZCs.HIS),1991,1986);
    dfrNLR <- tblNLR %>% 
                subset((year>=minYr)&(year<=maxYr)) %>% 
                subset((sex!="unidentified"));
    
    #--bin and aggregate
    dfrZCs.NLR<-wtsSizeComps::calcSizeComps(dfrNLR,
                                              id.size="size",
                                              id.value="N",
                                              id.facs=id.facs,
                                              cutpts=cutpts,
                                              truncate.low=truncate.low,
                                              truncate.high=truncate.high);
    cols<-names(dfrZCs.NLR);
    if (!("year" %in% cols))            dfrZCs.NLR$year              = "undetermined";
    if (!("gear" %in% cols))            dfrZCs.NLR$gear              = "undetermined";
    if (!("area" %in% cols))            dfrZCs.NLR$area              = "undetermined";
    if (!("sex" %in% cols))             dfrZCs.NLR$sex               = "undetermined";
    if (!("maturity" %in% cols))        dfrZCs.NLR$maturity          = "undetermined";
    if (!("shell condition" %in% cols)) dfrZCs.NLR$`shell condition` = "undetermined";
    dfrZCs.NLR <- dfrZCs.NLR[,c("year","gear","area","sex","maturity","shell condition","size","N")];#--drop ss
  }
  #--combine dataframes
  if (is.null(dfrZCs.HIS)) {dfrZCs=dfrZCs.NLR;} else
  if (is.null(dfrZCs.NLR)) {dfrZCs=dfrZCs.HIS;} else
  {dfrZCs<-rbind(dfrZCs.HIS,dfrZCs.NLR);}
  
  return(dfrZCs);
}
# fnHisZCs="GroundfishFisheries.HistoricalZCs.LongFormat.TannerCrab.csv";
# fnHisZCs=file.path(dirData,fnHisZCs);
# fnNORPAC="FromAKFIN.TannerCrab.norpac_length_report.csv";
# fnNORPAC=file.path(dirData,fnNORPAC);
# dfrZCs.onYGAXZ<-akfin.calcUnscaledZCs(fnHisZCs,fnNORPAC);

#'
#' @title Calculate estimated total bycatch abundance and biomass of Tanner crab in groundfish fisheries
#'
#' @description Function to calculate estimated total catch abundance by expanding observed abundance.
#'
#' @param dfrZCs - filename for size composition estimates (numbers of crab) from "historical data" (pre-CAS)
#' @param ss_scl - filename for size data (numbers of crab) from a NORPAC length report file
#' @param ss_max - integer flag for diagnostic printout (0,1,2)
#'
#' @return tibble with columns
#'
#' @details Uses \code{sqldf::sqldf}. Units for 'weight' are kg, for 'abundance' are thousands, and for 'biomass' are t.
#' Historical (foreign, joint veture) data runs 1973-1990. CAS data is used for 1991-2008. CIA data is used for
#' 2009+.
#'
#' @import magrittr
#' @import dplyr
#' 
#' @importFrom sqldf sqldf
#' @importFrom wtsSizeComps calcSampleSizes
#'
#' @export
#'
akfin.calcInputSSsForTCSAM<-function(dfrZCs,
                                     ss_scl,
                                     sss_max = 200){
  #--calculate and scale sample sizes
  dfrSSs<-wtsSizeComps::calcSampleSizes(dfrZCs,
                                        id.value="N",
                                        id.facs=c("year","gear","sex"));
  dfrISSs<-akfin.ScaleInputSSs(dfrSSs,ss_scl=ss_scl,ss_max=ss_max);
  return(dfrSSs);
}
# dfrSSs.onYGAX<-akfin.calcInputSSsForTCSAM(dfrZCs.onYGAXZ,
#                                           ss_scl=ss_scl,
#                                           ss_max=ss_max);

#' @title Calculate estimated total bycatch abundance and biomass of Tanner crab in groundfish fisheries
#'
#' @description Function to calculate estimated total catch abundance by expanding observed abundance.
#'
#' @param dfrABs.YGAT - dataframe with total abundance by YGAT
#' @param dfrZCs.onYGAXZ - dataframe with "raw" number size compositions by YGAX
#'
#' @return tibble with columns
#'
#' @details Uses \code{sqldf::sqldf}. Units for 'weight' are kg, for 'abundance' are thousands, and for 'biomass' are t.
#' Historical (foreign, joint veture) data runs 1973-1990. CAS data is used for 1991-2008. CIA data is used for
#' 2009+.
#'
#' @import magrittr
#' @import dplyr
#' 
#' @importFrom sqldf sqldf
#'
#' @export
#'
akfin.ScaleZCs<-function(dfrABs.YGAT,
                         dfrZCs.onYGAXZ){
  Sum<-wtsUtilities::Sum;
  #----calculate total observed numbers by year, gear, and area
  dfrONs.YGA<-dfrZCs.onYGAXZ %>%
                  dplyr::group_by(year,gear,area) %>%
                  dplyr::summarize(obsNum=wtsUtilities::Sum(N)) %>%
                  dplyr::ungroup();
  #----calculate total bycatch numbers by year, gear, and area
  dfrTNs.YGA<-dfrABs.YGAT %>%
                  subset(!is.na(num)) %>%
                  dplyr::group_by(year,gear,area) %>%
                  dplyr::summarize(totNum=wtsUtilities::Sum(num)) %>%
                  dplyr::ungroup();

  #----calculate YGA-specific multipliers to expand observed numbers to estimated total numbers
  ont<-dfrONs.YGA;
  ent<-dfrTNs.YGA;
  query<-"select
            ont.year,ont.gear,ont.area,
            1.0*ont.obsNum as obsNum,
            1.0*ent.totNum as estNum,
            1.0*(ent.totNum/ont.obsNum) as expnfac
          from
            ont, ent
          where
            ont.year=ent.year and
            ont.gear=ent.gear and
            ont.area=ent.area;";
  dfrXFs.YGA<-sqldf::sqldf(query);

  #----need to finish areas with catch but no size data
  dfrTNs.YG<-dfrTNs.YGA %>%
                  dplyr::group_by(year,gear) %>%
                  dplyr::summarize(estNumTot=wtsUtilities::Sum(totNum)) %>%
                  dplyr::ungroup();
  dfrTNs.YGp<-dfrXFs.YGA  %>%
                  dplyr::group_by(year,gear) %>%
                  dplyr::summarize(estNumTotP=Sum(estNum)) %>%
                  dplyr::ungroup();
  dfrTNs.YG<-cbind(dfrTNs.YG,estNumTotP=dfrTNs.YGp$estNumTotP);
  dfrTNs.YG<-dfrTNs.YG %>% dplyr::mutate(xfac=estNumTot/estNumTotP);
  # dfr.enYG<-reshape2::dcast(dfr.enYGA,year+gear~.,fun.aggregate=Sum,value.var="num");
  # names(dfr.enYG)[3]<-"estNumTot";
  # dfr.enYGp<-reshape2::dcast(dfr.xfYGA,year+gear~.,fun.aggregate=Sum,value.var="estNum");
  # names(dfr.enYGp)[3]<-"estNumTotP";
  # dfr.enYG<-cbind(dfr.enYG,estNumTotP=dfr.enYGp$estNumTotP);
  # dfr.enYG$xfac<-dfr.enYG$estNumTot/dfr.enYG$estNumTotP;

  ont<-dfrXFs.YGA;
  ent<-dfrTNs.YG;
  query<-"select
            ont.year,ont.gear,ont.area,
            1.0*ont.obsNum as obsNum,
            1.0*estNum as estNum,
            1.0*(ont.estNum*ent.xfac) as estNumP,
            1.0*(ont.expnfac*ent.xfac) as expnfac
          from
            ont, ent
          where
            ont.year=ent.year and
            ont.gear=ent.gear;";
  dfrXFs.YGA<-sqldf::sqldf(query);
  rm(ont,ent);
  
  #--expand observed size frequencies to estimated total bycatch numbers
  ont<-dfrZCs.onYGAXZ;
  emt<-dfrXFs.YGA;
  query<-"select
            ont.year,ont.gear,ont.area,
            ont.sex,ont.maturity,ont.`shell condition`,ont.size,
            emt.expnfac*ont.N as N
          from
            ont, emt
          where
            ont.year=emt.year and
            ont.gear=emt.gear and
            ont.area=emt.area
          order by
            ont.year,ont.gear,ont.sex,ont.area,ont.size;";
  dfrZCs.tnYGAXZ<-sqldf::sqldf(query);
  rm(ont,emt);
  
  #check on calculations
  dfrA1s.YG = dfrZCs.tnYGAXZ %>% 
                dplyr::group_by(year,gear) %>% dplyr::summarize(totNum1=Sum(N));
  dfrA2s.YG = dfrABs.YGAT %>% subset(year>=1991) %>%
                dplyr::group_by(year,gear) %>% dplyr::summarize(totNum2=Sum(num));
  idx<-abs((dfrA1s.YG$totNum1-dfrA2s.YG$totNum2)/(dfrA1s.YG$totNum1+dfrA2s.YG$totNum2))>0.0001;
  if (sum(idx)>0) {
    msg<-paste0("Problem with expansion of size comps!\n")
    warning(msg)
    dfr=cbind(dfrA1s.YG,dfrA2s.YG[,"totNum2"]);
    dfr$diff = abs((dfrA1s.YG$totNum1-dfrA2s.YG$totNum2)/(dfrA1s.YG$totNum1+dfrA2s.YG$totNum2))
    print(dfr);
  }
  
  #--combine ZCs that could not be expanded with those that were
  uY1s<-sort(unique(dfrZCs.onYGAXZ$year));
  uY2s<-sort(unique(dfrZCs.tnYGAXZ$year));
  uY1not2s<-uY1s[!(uY1s %in% uY2s)];
  dfrZCs<-rbind(dfrZCs.onYGAXZ %>% subset(year %in% uY1not2s),
                dfrZCs.tnYGAXZ);
  
  return(dfrZCs);
}

