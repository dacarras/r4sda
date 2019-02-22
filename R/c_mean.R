#' c_mean() computes the cluster mean for given variable, and a given clustering variable
#'
#' @param x a numeric vector of a single variable
#'
#' @return a numeric vector
#' @export
#' 
#' @details simple wrapper for `ave(variable, group)` function, to estimate cluster means
#'
#' @examples
#' library(dplyr)
#' data_frame %>%
#' mutate(cluster_mean =  c_mean(x, id_j)) %>%
#' select(x, id_j) %>%
#' unique() %>%
#' print
c_mean <- function(x,j){
ave(x,j,FUN=function(x) mean(x, na.rm=T))
}
