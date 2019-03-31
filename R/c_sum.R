#' c_sum() computes the cluster sum for a given variable, and a given clustering variable
#'
#' @param x a numeric vector of a single variable
#'
#' @return a numeric vector
#' @export
#' 
#' @details simple wrapper for `ave(variable, group)` function, to estimate cluster sum
#'
#' @examples
#' library(dplyr)
#' data_frame %>%
#' mutate(cluster_sum =  c_sum(dummy, id_j)) %>%
#' select(cluster_sum, id_j) %>%
#' unique() %>%
#' print
c_sum <- function(x,j){
ave(x,j,FUN=function(x) sum(x, na.rm=T))
}
