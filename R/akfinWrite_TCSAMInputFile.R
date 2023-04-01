#'
#' @title Write AKFIN data for crab bycatch to a TCSAM input file
#'
#' @description Function to write AKFIN data for crab bycatch to a TCSAM input file.
#'
#' @param fishery : TCSAM fishery name
#' @param fn : output file name (writes to stdout if NULL or empty)
#' @param lstAbd : NULL, or list as returned by [tcsamFunctions::inputList_AggregateCatchData()] with type "ABUNDANCE"
#' @param lstBio : NULL, or list as returned by [tcsamFunctions::inputList_AggregateCatchData()] with type "BIOMASS"
#' @param lstZCs : NULL, or list as returned by [tcsamFunctions::inputList_SizeCompsData()]
#'
#' @return nothing
#'
#' @details Calls [tcsamFunctions::writeInputFile_FleetData()] with \code{type}="FISHERY" (and lstIC, lstRC, lstDC, and lstEff set to NULL).
#'
#' @importFrom tcsamFunctions writeInputFile_FleetData
#' 
#' @export
#'
akfinWrite_TCSAMInputFile<-function(fishery=NULL,
                                    fn="Data.Fishery.AKFIN.inp",
                                    lstAbd=NULL,
                                    lstBio=NULL,
                                    lstZCs=NULL){
  if (is.null(fn)||(fn=="")){
    con = stdout();
  } else {
    if (!file.exists(fn)) {
      res<-file.create(fn);
      if (!res) stop(paste0("Could not create file '",fn,"'.\nAborting...\n"));
    }
    con<-file(fn,open="w");
    on.exit(close(con));
  }

  writeInputFile_FleetData(con=con,
                           fleet=fishery,
                           type="FISHERY",
                           closed=NULL,
                           lstRC=NULL,
                           lstTC=list(lstAbd=lstAbd,lstBio=lstBio,lstZCs=lstZCs),
                           lstDC=NULL,
                           lstIC=NULL,
                           lstEff=NULL);
}


