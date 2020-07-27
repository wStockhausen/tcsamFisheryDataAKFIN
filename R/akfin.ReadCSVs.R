#'
#' @title Read a csv file from AKFIN with CIA (Catch-in-Areas) data
#'
#' @description Function to read a csv file from AKFIN with CIA (Catch-in-Areas) data.
#'
#' @param fn - filename for bycatch estimates from the Catch-in-Areas database
#' @param minYear - min fishery year to extract
#'
#' @return tibble with columns "year","target","gear code","adfg stat area","nmfs stat area",
#' "haul count","biomass","number","conf flag","vessel count", and "gear". Units for biomass
#' and number are kg and one's (i.e., unscaled number of crab).
#'
#' @details CIA data begins in 2009.
#'
#' @import magrittr
#' 
#' @importFrom readr read_csv
#'
#' @export
#'
akfin.ReadCSV.CIA<-function(fn,
                            minYear=2009){
  tmp <-readr::read_csv(fn);
  #------extract relevant columns
  tmp1<-tmp[,c(2,8,14,15,17,26,28,30,31,32)];
  names(tmp1)<-c("year","target","gear code","adfg stat area","nmfs stat area","haul count","biomass","number","conf flag","vessel count");
  tmp1$gear<-"";
  #------convert gear code to gear (type)
  dfr.gear<-akfin.GetGearConversions();
  for (rw in 1:nrow(dfr.gear)){
    idx<-tmp1$`gear code`==dfr.gear$code[rw];
    tmp1$gear[idx]<-dfr.gear$type[rw];
  }
  tblCIA<-tmp1 %>% subset(minYear<=year);
  return(tblCIA);
}
# dirData<-"~/Work/StockAssessments-Crab/Data/Fisheries/Fishery.AKFIN/Current";
# fn<-file.path(dirData,"FromAKFIN.TannerCrab.BycatchEstimates.CIA.csv");
# tbl<-akfin.ReadCSV.CIA(fn);

#'
#' @title Read a csv file from AKFIN with CAS (Catch Accounting System) data
#'
#' @description Function to read a csv file from AKFIN with CAS (Catch Accounting System) data.
#'
#' @param fn - filename for bycatch estimates from the Catch Accounting System database
#' @param bycatch_species_code - FMA species code (default = "BTCR", for bairdi Tanner crab)
#' @param minYear - min fishery year to extract
#'
#' @return tibble with columns "year","gear","area","target","num",and "wgt". 
#' Units for biomass (\code{wgt}) and number (\code{num}) are kg and one's (i.e., unscaled number of crab).
#'
#' @details Valid CAS data begins in 1991 and ends in 2009 (it overlaps with the CIA database a bit)
#'
#' @import magrittr
#' 
#' @importFrom readr read_csv
#'
#' @export
#'
akfin.ReadCSV.CAS<-function(fn,
                            bycatch_species_code="BTCR",
                            minYear=1991){
  tmp <-readr::read_csv(fn);
  #names(tmp);
  names(tmp)<-c("year","gear","area","species code","species name","num","wgt","load");
  tmp<-tmp[tmp$`species code`==bycatch_species_code,];#keep only Tanner crab
  tmp$year<-as.numeric(substr(tmp$year,1,4));
  tmp$gear<-tolower(tmp$gear);   #gear to lower case
  tmp$target<-"undetermined";
  cols.Byc<-c("year","gear","area","target","num","wgt");
  dfrCAS<-tmp[(minYear<=tmp$year),cols.Byc];
  #------num values are in one's, wgt values in kg
  return(dfrCAS);
}

#'
#' @title Read a csv file from AKFIN with NORPAC length report data
#'
#' @description Function to read a csv file from AKFIN with NORPAC length report data.
#'
#' @param fn - filename for size composition data from the NORPAC database
#'
#' @return tibble with columns "year","gear","area","sex","size",and "N". 
#' Units for N are counts (one's) of sampled crab..
#'
#' @details Valid NORPAC data begins in 1986, but is typically used starting in 1991, to the present.
#'
#' @importFrom readr read_csv
#'
#' @export
#'
akfin.ReadCSV.NorpacLengthReport<-function(fn){
  #--process input bycatch size frequencies to get observed numbers-at-size by year, gear, area, and sex
  tmp<-readr::read_csv(fn,guess_max=100000);
  #----need crab year, not year
  cols.NatZ<-c("Year","Haul Offload Date","Gear Description","NMFS Area","Sex","Length (cm)","Frequency");
  dfrp<-tmp[,cols.NatZ];
  names(dfrp)<-c("year","date","gear","area","sex","size","N");
  #------extract month from date column
  dfrp$month<-gsub("[^[:upper:]_]","",toupper(dfrp$date)); #Notes: '_' extends previous expression to entire "word"
  #------calculate crab year (cy = y-1*(month in [jan,feb,mar,apr,may,jun]))
  dfrp$crabyear<-dfrp$year-as.numeric(dfrp$month %in% c("JAN","FEB","MAR","APR","MAY","JUN"))
  
  #----rearrange columns
  dfrp<-dfrp[,c("crabyear","year","month","gear","area","sex","size","N")];
  
  #--convert gear codes to gear "types" (fixed, trawl)
  dfrp$gear<-akfin.ConvertGearDescToType(dfrp$gear);

  #----convert sex codes to sex (male, female)
  dfrp$sex<-akfin.ConvertSexCodes(dfrp$sex);

  #----keep only necessary columns
  dfrp<-dfrp[,c("crabyear","gear","area","sex","size","N")];
  
  #----rename columns
  names(dfrp)[1]<-"year";
  
  return(dfrp);
}

