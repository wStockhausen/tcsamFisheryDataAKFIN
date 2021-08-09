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
#' @details CIA data begins in 2009. Biomass is in kg.
#'
#' @import magrittr
#' 
#' @importFrom readr read_csv
#'
#' @export
#'
akfinRead_CIA<-function(fn,
                        minYear=2009){
  #--read file
  tmp = readr::read_csv(fn);
  #----extract relevant columns
#  tmp1 = tmp[,c(2,8,14,15,17,26,28,30,31,32)]; <-OLD WAY (number of columns changed)
  tmp1 = tmp[,c("Crab Year", 
                "Trip Target Name",
                "Agency Gear Code", 
                "ADFG Stat Area Code", 
                "Reporting Area Code", 
                "Haul Count", 
                "Estimated Crab Weight (kg)", 
                "Estimated Number")];
  names(tmp1) = c("year","target","gear code","adfg stat area","nmfs stat area","haul count","biomass","number");
  tmp1[["conf flag"]]   =1;#--AKFIN dropped column in 2021
  tmp1[["vessel count"]]=1;#--AKFIN dropped column in 2021
  
  #--convert gear codes to types
  tmp1$gear = akfinConvert_GearCodeToType(tmp1$`gear code`);
  
  #--add lat-lons based on ADFG stat areas
  dfrLLs = akfinConvert_ADFGStatAreasToLatLons(tmp1[["adfg stat area"]]);
  tmp1$lat = dfrLLs$lat;
  tmp1$lon = dfrLLs$lon;
  
  #--extract required years
  tblCIA = tmp1 %>% subset(minYear<=year);
  return(tblCIA);
}

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
#' @details Valid CAS data begins in 1991 and ends in 2009 (it overlaps with the CIA database a bit).
#' The "gear" reported here is represents gear "type" as used in the Conversion functions. 
#'
#' @import magrittr
#' 
#' @importFrom readr read_csv
#'
#' @export
#'
akfinRead_CAS<-function(fn,
                        bycatch_species_code="BTCR",
                        minYear=1991){
  #--read the file
  tmp <-readr::read_csv(fn);
  names(tmp)<-c("year","gear","area","species code","species name","num","wgt","load");
  
  #--extract info only on requested bycatch species
  tmp<-tmp[tmp$`species code`==bycatch_species_code,];
  
  #--extract year info
  tmp$year<-as.numeric(substr(tmp$year,1,4));
  
  #--extract gear and target info
  tmp$gear<-tolower(tmp$gear);   #gear to lower case
  tmp$target<-"undetermined";
  
  #--create final table
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
#' @return tibble with columns "year","gear","area","sex","size",and "N", 
#' "gear code","lat","lon",and "adfg stat area". "gear" here represents the
#' gear "type" from the Conversion functions whereas "gear code" represents the
#' gear "code" from those functions.
#' 
#' Units for N are counts (one's) of sampled crab.
#'
#' @details Valid NORPAC data begins in 1986, but is typically used starting in 1991, to the present.
#'
#' @importFrom readr read_csv
#' @importFrom wtsUtilities Sum
#' @importFrom wtsUtilities count
#' 
#' @import dplyr
#'
#' @export
#'
akfinRead_NorpacLengthReport<-function(fn){
  #--process input bycatch size frequencies to get observed numbers-at-size by year, gear, area, and sex
  tmp<-readr::read_csv(fn,guess_max=1000000);
  cols.NatZ<-c("Year","Haul Offload Date","Gear Description","NMFS Area","Sex","Length (cm)","Frequency","LatDD Start","LonDD Start","LatDD End","LonDD End");
  dfrp<-tmp[,cols.NatZ];
  names(dfrp)<-c("year","date","gear description","area","sex","size","N","lat_s","lon_s","lat_e","lon_e");
  
  #------extract lat, lon and convert to stat areas
  dfrp<-dfrp %>% rowwise() %>% 
                 mutate(lat=Sum(c(lat_s,lat_e)/count(c(lat_s,lat_e))),lon=Sum(c(lon_s,lon_e)/count(c(lon_s,lon_e)))) %>%
                 ungroup();
  stat_areas<-akfinConvert_LatLonsToADFGStatAreas(dfrp);
  dfrp$`adfg stat area`<-stat_areas;
  
  #----need crab year, not year
  #------extract month from date column
  dfrp$month<-gsub("[^[:upper:]_]","",toupper(dfrp$date)); #Notes: '_' extends previous expression to entire "word"
  #------calculate crab year (cy = y-1*(month in [jan,feb,mar,apr,may,jun]))
  dfrp$crabyear<-dfrp$year-as.numeric(dfrp$month %in% c("JAN","FEB","MAR","APR","MAY","JUN"))
  
  #--convert gear descriptions to gear "types" (fixed, trawl)
  dfrp$gear<-akfinConvert_GearDescToType(dfrp$`gear description`);
  #--convert gear descriptions to gear "codes" ("PTR","NPT", etc)
  dfrp$`gear code`<-akfinConvert_GearDescToCode(dfrp$`gear description`);

  #----convert sex codes to sex (male, female)
  dfrp$sex<-akfinConvert_SexCodes(dfrp$sex);

  #----keep only necessary columns
  dfrp<-dfrp[,c("crabyear","gear","area","sex","size","N","gear code","lat","lon","adfg stat area")];
  
  #----rename columns
  names(dfrp)[1]<-"year";
  
  #--set missing location info to NA's
  idx<-dfrp$lat==0;
  dfrp$lat[idx]<-NA;
  dfrp$lon[idx]<-NA;
  dfrp$`adfg stat area`[idx]<-NA;
  
  return(dfrp);
}

