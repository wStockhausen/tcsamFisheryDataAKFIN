#'
#' @title Convert ADFG stat areas to lat-lon coordinates
#'
#' @description Function to convert ADFG stat areas to lat-lon coordinates.
#'
#' @param stat_areas - character vector of ADFG stat areas
#'
#' @return a tibble with columns "lat", "lon" representing the center of each stat area.
#'
#' @details ADFG stat areas are coded AABBCC, where\cr 
#' lon = 100+AA W longitude \cr
#' and \cr
#' lat = BB+CC/60 N latitude \cr
#' in decimal degrees. 
#' 
#' AABBCC represents the lower right corner
#' of the stat area.
#'
#' @importFrom tcsamFisheryDataADFG adfgConvert_StatAreasToLatLons
#'
#' @export
#'
akfinConvert_ADFGStatAreasToLatLons<-function(stat_areas){
  tbl = tcsamFisheryDataADFG::adfgConvert_StatAreasToLatLons(stat_areas);
  return(tbl);
}

#'
#' @title Convert lat-lon coordinates to ADFG stat areas
#'
#' @description Function to convert lat-lon coordinates to ADFG stat areas.
#'
#' @param dfr - dataframe with lat, lon coordinates
#' @param latCol - name of column with latitudes
#' @param lonCol - name of column with longitudes
#'
#' @return a character vector with the ADFG stat area corresponding to each set
#' of lat-lon coordinates.
#'
#' @details ADFG stat areas are coded AABBCC, where\cr 
#' lon = 100+AA W longitude \cr
#' and \cr
#' lat = BB+CC/60 N latitude \cr
#' in decimal degrees. 
#' 
#' AABBCC represents the lower right corner
#' of the stat area.
#'
#' @importFrom tcsamFisheryDataADFG adfgConvert_LatLonsToStatAreas
#'
#' @export
#'
akfinConvert_LatLonsToADFGStatAreas<-function(dfr,latCol="lat",lonCol="lon"){
  stat_areas<-tcsamFisheryDataADFG::adfgConvert_LatLonsToStatAreas(dfr,latCol="lat",lonCol="lon");
  return(stat_areas);
}

