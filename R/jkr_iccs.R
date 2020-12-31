#' jkr_iccs() computes jackknifes for ICCS 2009 and ICCS 2016
#'
#' @param data data frame object which contains the rest of the arguments
#' @param nrep number of replicates
#' @param jkz vector containing the pseudo strata (75 in ICCS studies)
#' @param jki vector selecting which school goes into the replicate
#' @param wgt total survey weights
#'
#' @return the original data frame with the jacknnifes replicates added.
#'
#' @details a loop function to create jackknifes replicate weights (author: `merivera@uc.cl`)
#'
#' @examples
#' library(dplyr)
#' data_frame_with_jackknifes <- data_frame %>%
#'                               r4sda::jackknifes_iccs(
#'                               data = .,
#'                               nrep = 75,
#'                               jkz = 'JKZONEZ',
#'                               jki = 'JKREPS',
#'                               wgt = 'TOTWGTS')
#'
#' @export
jkr_iccs <-function(data, nrep, jkz, jki, wgt){

  # data = dataframe
  # nrep = number of repetions (numeric)
  # jz   = name of variables with zones (string)
  # ji   = name of variable with kreps (string)
  # wgt  = names of variable with weights (string)

  w_fstr <- matrix(ncol=nrep, nrow=dim(data)[1])
  colnames(w_fstr)<-paste0("jk", 1:nrep)

  for (j in 1:nrep){
    w_fstr[data[[jkz]]==j & data[[jki]]==0, j]<- data[[wgt]][data[[jkz]]==j & data[[jki]]==0]*0
    w_fstr[data[[jkz]]==j & data[[jki]]==1, j]<- data[[wgt]][data[[jkz]]==j & data[[jki]]==1]*2
    w_fstr[data[[jkz]]!=j , j]<- data[[wgt]][data[[jkz]]!=j]*1
  }

  data_with_replicates <- dplyr::bind_cols(data, as.data.frame(w_fstr))
  return(data_with_replicates)

}
