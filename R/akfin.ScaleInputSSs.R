#'
#' @title Calculate the "input" sample sizes to TCSAM02 for AKFIN size composition data
#'
#' @description Function to calculate the "input" sample sizes to TCSAM02 for AKFIN size composition data.
#'
#' @param dfrSS - dataframe with original sample sizes (see details)
#' @param ss_scl - scale factor for sample sizes from \code{adfg.calcScaleFactorForInputSSs}
#' @param ss_max - maximum input sample size allowed
#'
#' @return dataframe or tibble with the same column order as the input, but with
#' additional columns \code{ss_tot} and \code{ss_scl}. Column \code{ss} contains the scaled
#' sample sizes, column \code{ss_scl} contains the scale used to convert the original
#' sample sizes.
#'
#' @details The scale factor \code{ss_scl} and max input sample size \code{ss_max} are
#' used to determine input annual sample sizes for size composition data using the formula \cr
#'              \code{inp_ss = min(max_ss,max_ss*(ss_tot/scs_scl))*ss/ss_tot} \cr
#' where \code{ss} is the original sample size (the total number of crab measured) by factor
#' combination and \code{ss_tot} is the total sample size by year and sex.
#'
#' @note \code{dfrSS} should have (at least) the columns
#' \itemize{
#'   \item{year}
#'   \item{sex}
#'   \item{ss}
#' }
#'
#' @note  \code{dfrSS} should include sample sizes for only a single fishery or survey.
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr relocate
#' @importFrom dplyr summarize
#' @importFrom wtsUtilities Sum
#'
#' @export
#'
akfin.ScaleInputSSs<-function(dfrSS,
                              ss_scl,
                              ss_max=200){
  #--calculate scaled sample size for total sample size by year and sex
  dfrSSp<-dfrSS %>%
           dplyr::group_by(year) %>%
           dplyr::summarize(ss_tot=wtsUtilities::Sum(ss)) %>%
           dplyr::mutate(ss_scl=ss_max*(ss_tot/ss_scl)) %>%
           dplyr::group_by(year) %>%
           dplyr::mutate(ss_scl=min(ss_max,ss_scl));
  #--apportion scaled sample sizes by year, sex and other factors
  dfrSSs_inp<-dfrSS %>%
                 dplyr::inner_join(dfrSSp,by=c("year")) %>%
                 dplyr::mutate(ss=ss_scl*(ss/ss_tot)) %>%
                 dplyr::relocate(tidyselect::any_of(c(names(dfrSS),"ss_tot","ss_scl")));
  return(dfrSSs_inp);
}
