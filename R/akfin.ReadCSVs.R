#'
#' @title Calculate estimated total bycatch abundance and biomass of Tanner crab in groundfish fisheries
#'
#' @description Function to calculate estimated total catch abundance by expanding observed abundance.
#'
#' @param fn - filename for bycatch estimates from Catch-In_Areas database
#' @param minYear - min fishery year to extract
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

#'
#' @title Calculate estimated total bycatch abundance and biomass of Tanner crab in groundfish fisheries
#'
#' @description Function to calculate estimated total catch abundance by expanding observed abundance.
#'
#' @param fn - filename for bycatch estimates from Catch Accounting System database
#' @param bycatch_species_code - code for bycatch species (default="BTCR")
#' @param minYear - min fishery year to extract
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
#' @title Calculate estimated total bycatch abundance and biomass of Tanner crab in groundfish fisheries
#'
#' @description Function to calculate estimated total catch abundance by expanding observed abundance.
#'
#' @param fn - filename for szie data from a NORPAC length report file
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

