#'
#' @title Write AKFIN data for crab bycatch to a TCSAM input file
#'
#' @description Function to write AKFIN data for crab bycatch to a TCSAM input file.
#'
#' @param fishery - TCSAM fishery name
#' @param gear - gear type
#' @param fn - output file name (default writes to stdout)
#' @param closed - vector of years when fishery was closed
#' @param dfrTC_ABs - dataframe with total bycatch abundance/biomass data
#' @param dfrTC_ZCs - dataframe with total bycatch size composition data
#' @param dfrTC_SSs - dataframe with total bycatch sample size information
#' @param cutpts - vector with cutpts used for size comps
#' @param ss_factor - sample size scaling factor (from retained catch analysis)
#' @param ss_max - max for sample sizes (default=200)
#' @param likeTC - likelihood type for total bycatch ABs ("NORM2","NORMAL", or "LOGNORMAL")
#' @param cvTC - cv for total bycatch above min error (default=0.20)
#' @param minErrTCa - min error in total bycatch abundance (default=2000)
#' @param minErrTCb - min error in total bycatch biomass   (default=2000 kg)
#' @param unitsBiomass - units for output biomass ("THOUSANDS_MT" [default],"MILLIONS_LBS","KG")
#'
#' @return null
#'
#' @details None
#'
#' @export
#'
akfinWrite_TCSAMInputFile<-function(fishery=NULL,
                                    gear="all",
                                    fn="",
                                    closed=NULL,
                                    dfrTC_ABs=NULL,
                                    dfrTC_ZCs=NULL,
                                    dfrTC_SSs=NULL,
                                    dfrEffort=NULL,
                                    cutpts=NULL,
                                    ss_factor=1,
                                    ss_max=200,
                                    likeTC="NORM2",
                                    cvTC=0.20,
                                    minErrTCa=2000,
                                    minErrTCb=2000,
                                    unitsBiomass="THOUSANDS_MT"){
  #--SCALE CONSTANTS
  MILLIONS<-1000000;     #scale to convert to millions
  LBStoKG <- 0.45359237; #multiplicative factor to get kg from lbs
  #--determine scaling for output biomass (input in kg)
  if (unitsBiomass=="THOUSANDS_MT") {
    sclB <- 1.0/MILLIONS;
  } else if (unitsBiomass=="MILLIONS_LBS") {
    sclB <- 1.0/(MILLIONS*LBStoKG);
  } else if (unitsBiomass=="KG") {
    sclB <- 1.0;
  } else {
    msg<-paste0("\nERROR in adfgWrite_TCSAMInputFile.",
                "\ninput value for unitsBiomass ('",unitsBiomass,"') is invalid.",
                "\nValid values are 'THOUSANDS_MT','MILLIONS_LBS','KG'.\n");
    stop(msg);
  }

  con<-"";#--write to stdout
  if (fn!=""){
    if (!file.exists(fn)) {
      res<-file.create(fn);
      if (!res) stop(paste0("Could not create file '",fn,"'.\nAborting...\n"));
    }
    con<-file(fn,open="w");
  }

  #--function to make substitutions for "undetermined"
  subForTCSAM<-function(x,str){
    xp <- ifelse(tolower(x)=="undetermined",str,x);
    xp <- gsub(" ","_",xp,fixed=TRUE);
    return(xp);
    }

    cat("#--------------------------------------------------------------------\n",file=con);	
    cat("#TCSAM02 model file for",gear,"gear groundfish fisheries\n",file=con);
    cat("#--------------------------------------------------------------------\n",file=con);
    cat("FISHERY    #required keyword\n",file=con);
    cat(gfn,"     	  #fishery name\n",file=con,sep='');
    cat("FALSE    	#has index bycatch data?\n",file=con);
    cat("FALSE    	#has retained bycatch data?\n",file=con);
    cat("FALSE    	#has observed discard bycatch data\n",file=con);
    cat("TRUE   		#has observed total bycatch data\n",file=con);
    cat("FALSE    	#has effort data?\n",file=con);
    cat("#------------INDEX bycatch DATA------------	\n",file=con);
    cat("#---none\n",file=con);
    cat("#------------RETAINED bycatch DATA------------#\n",file=con);
    cat("#---none\n",file=con);
    cat("#------------DISCARD bycatch DATA------------#\n",file=con);
    cat("#---none\n",file=con);
    cat("#------------TOTAL bycatch DATA------------#\n",file=con);
    cat("CATCH_DATA  		#required keyword\n",file=con);
    cat("TRUE           #has aggregate bycatch abundance (numbers)\n",file=con);
    cat("TRUE           #has aggregate bycatch biomass (weight)\n",file=con);
    cat("TRUE           #has size frequency data\n",file=con);
    
    #--bycatch numbers
    dfrp<-reshape2::dcast(tmp1[idg,],year~.,fun.aggregate=Sum,value.var="totNum");
    names(dfrp)[2]<-"totNum";
    cat("#------------AGGREGATE bycatch ABUNDANCE (NUMBERS)------------#\n",file=con);
    cat("AGGREGATE_ABUNDANCE     #required keyword\n",file=con);
    cat("BY_TOTAL                #objective function fitting option\n",file=con);
    cat(likeTC,"                 #likelihood type\n",file=con);
    wgt<-1; if (likeRC=="NORM2") wgt<-20.0;;
    cat(wgt,"                    #likelihood weight\n",file=con);
    cat(nrow(dfrp),"             #number of years\n",file=con,sep='');
    cat("MILLIONS   		         # units, bycatch abundance\n",file=con);
    cat("1		#number of factor combinations\n",file=con);
    cat("ALL_SEX  ALL_MATURITY  ALL_SHELL\n",file=con);
    cat("#year    number    cv\n",file=con);
    for (rw in 1:nrow(dfrp)){
      val<-dfrp$totNum[rw];#--catch in numbers of crab
      cv<-max(cvTC,minErrTCa/val);#--effective cv
      cat(dfrp$year[rw],val,cv,"\n",sep="    ",file=con);
    }
    rm(dfrp);
    
    #--bycatch biomass
    idyg<-(dfr.enYGA$gear==gear)&(dfr.enYGA$year %in% years);
    dfrp<-reshape2::dcast(dfr.enYGA[idyg,],year~.,fun.aggregate=Sum,value.var="wgt");
    names(dfrp)[2]<-"wgt";
    cat("#------------AGGREGATE bycatch ABUNDANCE (BIOMASS)------------#\n",file=con);
    cat("AGGREGATE_BIOMASS       #required keyword\n",file=con);
    cat("BY_TOTAL                #objective function fitting option\n",file=con);
    cat(likeTC,"                 #likelihood type\n",file=con);
    wgt<-1; if (likeRC=="NORM2") wgt<-20.0;;
    cat(wgt,"                    #likelihood weight\n",file=con);
    cat(nrow(dfrp),"             #number of years\n",file=con,sep='');
    cat("THOUSANDS_MT   		     # units, bycatch biomass\n",file=con);
    cat("1		#number of factor combinations\n",file=con);
    cat("ALL_SEX  ALL_MATURITY  ALL_SHELL\n",file=con);
    cat("#year    biomass    cv\n",file=con);
    for (rw in 1:nrow(dfrp)){
      cat(dfrp$year[rw],,0.2,"\n",sep="    ",file=con);
      val<-dfrp$wgt[rw];#--catch in 1000's t of crab
      cv<-max(cvTC,minErrTCb/val);#--effective cv
      cat(dfrp$year[rw],val,cv,"\n",sep="    ",file=con);
    }
    rm(idyg,dfrp);
    
    #--bycatch size compositions
    bins<-(cutpts[2:length(cutpts)]+cutpts[1:(length(cutpts)-1)])/2;
    dfrp<-reshape2::dcast(tmp1[idg,],sex+year~bin,fun.aggregate=Sum,value.var="totNum");
    cat("#------------NUMBERS-AT-SIZE DATA-----------\n",file=con);
    cat("SIZE_FREQUENCY_DATA  		#required keyword\n",file=con);
    cat("BY_XE		#objective function fitting option\n",file=con);
    cat("MULTINOMIAL 		#likelihood type\n",file=con);
    cat("1.0     #likelihood weight\n",file=con);
    cat(length(unique(dfrp$year)),"	#number of years of data\n",file=con);
    cat("MILLIONS		#units\n",file=con);
    cat(length(cutpts),"	#number of size bin cutpoints\n",file=con);
    cat("#size bin cutpts (mm CW)\n",file=con);					
    cat(cutpts,"\n",file=con);				
    cat("#--------------\n",file=con);
    cat("2		#number of factor combinations\n",file=con);
    for (sex in c("male","female")){
      dfrpp<-dfrp[dfrp$sex==sex,];
      cat(toupper(sex),"    ALL_MATURITY  ALL_SHELL					#sample sizes are for ALL ",sex,"s\n",file=con,sep='');
      cat("#year	sample_size",bins,"\n",file=con);
      nc<-ncol(dfrpp);
      for (rw in 1:nrow(dfrpp)){
        year<-dfrpp$year[rw];
        ss<-ssYGX$ss[(ssYGX$year==year)&(ssYGX$sex==sex)];
        ss<-min(ss/ss_f)
        cat(year,ss,unlist(dfrpp[rw,3:nc]),"\n",sep=" ",file=con);
      }#rw
      rm(dfrpp,nc,year,ss);
    }#sex
    rm(dfrp,bins,cutpts,idg);
    
    close(con);
}