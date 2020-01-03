#' c_sd() computes the cluster standard deviation for given variable, and a given clustering variable
#'
#' @param x a numeric vector of a single variable
#'
#' @return a numeric vector
#' @export
#' 
#' @details simple wrapper for `ave(variable, group)` function, to estimate cluster standard deviations
#'
#' @examples
#' library(dplyr)
#' data_frame %>%
#' mutate(cluster_sd =  c_sd(x, id_j)) %>%
#' select(x, id_j) %>%
#' unique() %>%
#' print
c_sd <- function(x,j){
ave(x,j,FUN=function(x) sd(x, na.rm=T))
}
