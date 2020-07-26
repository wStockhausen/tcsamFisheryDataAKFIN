#'
#' @title Create dataframe describing gear conversions
#'
#' @description Function to create dataframe describing gear conversions.
#'
#' @return dataframe with columns
#'
#' @details None.
#'
#' @export
#'
akfin.GetGearConversions<-function(){
  #gear conversions
  dfr.gear<-rbind(data.frame(type="trawl",code="TRW",description="TRAWL",      stringsAsFactors=FALSE),
                  data.frame(type="trawl",code="PTR",description="PELAGIC",    stringsAsFactors=FALSE),
                  data.frame(type="trawl",code="NPT",description="NON PELAGIC",stringsAsFactors=FALSE),
                  data.frame(type="trawl",code="PTR",description="PAIR TRAWL", stringsAsFactors=FALSE),
                  data.frame(type="fixed",code="HAL",description="LONGLINER",  stringsAsFactors=FALSE),
                  data.frame(type="fixed",code="POT",description="POT OR TRAP",stringsAsFactors=FALSE),
                  data.frame(type="fixed",code="JIG",description="JIG",        stringsAsFactors=FALSE));
  return(dfr.gear);
}

#'
#' @title Convert gear descriptions to gear types
#'
#' @description Function to convert gear descriptions to gear types.
#' 
#' @param x - vector of gear descriptions to convert to gear types
#'
#' @return dataframe with columns
#'
#' @details None.
#'
#' @export
#'
akfin.ConvertGearDescToType<-function(x){
  dfr.gear<-akfin.GetGearConversions();
  #----convert gear description to gear "type"
  y<-vector(mode="character",length = length(x));
  for (rw in 1:nrow(dfr.gear)){
    idg<-x==dfr.gear$description[rw];
    y[idg]<-dfr.gear$type[rw];
  }
  return(y);
}

#'
#' @title Create dataframe describing gear conversions
#'
#' @description Function to create dataframe describing gear conversions.
#'
#' @return character vector with same length as x
#'
#' @details None.
#'
#' @export
#'
akfin.ConvertGearCodeToType<-function(x){
  dfr.gear<-akfin.GetGearConversions();
  #----convert gear code to gear "type"
  y<-vector(mode="character",length = length(x));
  for (rw in 1:nrow(dfr.gear)){
    idg<-x==dfr.gear$code[rw];
    y[idg]<-dfr.gear$type[rw];
  }
  return(y);
}

#'
#' @title Create dataframe describing sex conversions
#'
#' @description Function to create dataframe describing sex conversions.
#'
#' @return dataframe with columns
#'
#' @details None.
#'
#' @export
#'
akfin.GetSexConversions<-function(){
  #sex conversions
  dfr.sex<-rbind(data.frame(sex="male",  code="M",stringsAsFactors=FALSE),
                 data.frame(sex="female",code="F",stringsAsFactors=FALSE),
                 data.frame(sex="unidentified",code="U",stringsAsFactors=FALSE));
  return(dfr.sex);
}

#'
#' @title Convert sex codes to descriptions
#'
#' @description Function to convert sex codes to descriptions.
#' 
#' @param x - vector of sex codes to convert
#'
#' @return character vector with same length as x
#'
#' @details None.
#'
#' @export
#'
akfin.ConvertSexCodes<-function(x){
  dfr.sex<-akfin.GetSexConversions();
  #----convert gear code to gear "type"
  y<-vector(mode="character",length = length(x));
  for (rw in 1:nrow(dfr.sex)){
    idg   <-(x==dfr.sex$code[rw]);
    y[idg]<-dfr.sex$sex[rw];
  }
  return(y);
}

