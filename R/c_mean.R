#' c_mean() computes the cluster mean for given variable, and a given clustering variable
#'
#' @param x a numeric vector of a single variable
#' @param j a numeric vector that indexes values into groups
#'
#' @return a numeric vector
#'
#' @details simple wrapper for `ave(variable, group)` function, to estimate cluster means
#'
#' @examples
#' example_data <- data.frame(x = c(1,2,3,4,5,6), j = c(1,1,1,2,2,2))
#'library(dplyr)
#' example_data %>%
#' mutate(cluster_mean =  c_mean(x, j)) %>%
#' select(cluster_mean, j) %>%
#' unique() %>%
#' print
#' @export
c_mean <- function(x,j){
ave(x,j,FUN=function(x) mean(x, na.rm=T))
}
