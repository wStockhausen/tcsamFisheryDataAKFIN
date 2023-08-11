#'
#' @title Create dataframe describing AKFIN gear conversions
#'
#' @description Function to create dataframe describing gear conversions.\cr
#' 
#' "type" ("code") can be \cr
#'                "trawl" ("TRW", "PTR" or "NPT") or \cr
#'                "fixed" ("HAL","JIG","POT") \cr
#' 
#' "code" ("description") can be \cr
#'               "TRW" ("TRAWL"),\cr
#'               "PTR" ("PELAGIC" or "PAIR TRAWL"),\cr
#'               "NPT" ("NON PELAGIC"),\cr
#'               "HAL" ("LONGLINER"),\cr
#'               "POT" ("POT OR TRAP"),\cr
#'               "JIG" ("JIG")\cr
#'
#' @return dataframe with columns 'type', 'code', and 'description'
#'
#' @details None.
#'
#' @export
#'
akfinGet_GearConversions<-function(){
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
#' @title Convert AKFIN gear descriptions to gear types
#'
#' @description Function to convert gear descriptions to gear types.
#' 
#' @param x : vector of gear descriptions to convert to gear types
#'
#' @return character vector with same length as x
#'
#' @details Uses conversions from \code{description} to \code{type} 
#' defined by [akfinGet_GearConversions()].
#'
#' @export
#'
akfinConvert_GearDescToType<-function(x){
  dfr.gear<-akfinGet_GearConversions();
  #----convert gear description to gear "type"
  y<-vector(mode="character",length = length(x));
  for (rw in 1:nrow(dfr.gear)){
    idg<-x==dfr.gear$description[rw];
    y[idg]<-dfr.gear$type[rw];
  }
  return(y);
}

#'
#' @title Convert gear codes to gear types
#'
#' @description Function to convert gear codes to gear types.
#' 
#' @param x : vector of gear codes to convert to gear types
#'
#' @return character vector with same length as x
#'
#' @details Uses conversions from \code{code} to \code{type} 
#' defined by [akfinGet_GearConversions()].
#'
#' @export
#'
akfinConvert_GearCodeToType<-function(x){
  dfr.gear<-akfinGet_GearConversions();
  #----convert gear code to gear "type"
  y<-vector(mode="character",length = length(x));
  for (rw in 1:nrow(dfr.gear)){
    idg<-x==dfr.gear$code[rw];
    y[idg]<-dfr.gear$type[rw];
  }
  return(y);
}

#'
#' @title Convert AKFIN gear descriptions to gear codes
#'
#' @description Function to convert gear descriptions to gear codes.
#' 
#' @param x : vector of gear descriptions to convert to gear codes
#'
#' @return character vector with same length as x
#'
#' @details Uses conversions from \code{description} to \code{code} 
#' defined by [akfinGet_GearConversions()].
#'
#' @export
#'
akfinConvert_GearDescToCode<-function(x){
  dfr.gear<-akfinGet_GearConversions();
  #----convert gear description to gear "code"
  y<-vector(mode="character",length = length(x));
  for (rw in 1:nrow(dfr.gear)){
    idg<-x==dfr.gear$description[rw];
    y[idg]<-dfr.gear$code[rw];
  }
  return(y);
}

#'
#' @title Create dataframe describing sex conversions
#'
#' @description Function to create dataframe describing sex conversions.
#'
#' @return dataframe with columns 'sex' and 'code'.
#'
#' @details None.
#'
#' @export
#'
akfinGet_SexConversions<-function(){
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
#' @param x : vector of sex codes to convert
#'
#' @return character vector with same length as x
#'
#' @details Uses conversions from \code{code} to \code{sex} 
#' defined by [akfinGet_SexConversions()].
#'
#' @export
#'
akfinConvert_SexCodes<-function(x){
  dfr.sex<-akfinGet_SexConversions();
  #----convert gear code to gear "type"
  y<-vector(mode="character",length = length(x));
  for (rw in 1:nrow(dfr.sex)){
    idg   <-(x==dfr.sex$code[rw]);
    y[idg]<-dfr.sex$sex[rw];
  }
  return(y);
}

