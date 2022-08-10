#' c_sum() computes the cluster sum for a given variable, and a given clustering variable
#'
#' @param x a numeric vector of a single variable
#'
#' @return a numeric vector
#'
#' @details simple wrapper for `ave(variable, group)` function, to estimate cluster sum
#'
#' @examples
#' library(dplyr)
#' example_data <- data.frame(x = c(1,2,3,4,5,6), j = c(1,1,1,2,2,2))
#' example_data %>%
#' mutate(cluster_sum =  c_sum(x, j)) %>%
#' select(cluster_sum, j) %>%
#' unique() %>%
#' print
#' @export
c_sum <- function(x,j){
ave(x,j,FUN=function(x) sum(x, na.rm=T))
}
