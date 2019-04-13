#' c_wmean() computes the cluster weighted mean for a given variable, and a given clustering variable
#'
#' @param x a numeric vector of a single variable
#' @param wi a numeric vector for within cluster weights
#'
#' @return a numeric vector
#' @export
#' 
#' @details simple wrapper for `ave(variable, group)` function couple with matrixStats::weightedMean(), to estimate cluster means weighted by within cluster weights
#'
#' @examples
#' library(dplyr)
#' data_frame %>%
#' mutate(cluster_mean =  c_wmean(x, id_j)) %>%
#' select(x, id_j) %>%
#' unique() %>%
#' print
c_wmean <- function(x,j){
ave(x,j,FUN=function(x) matrixStats::weightedMean(x, na.rm=T))
}
