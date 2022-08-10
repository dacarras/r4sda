#' c_sd() computes the cluster standard deviation for given variable, and a given clustering variable
#'
#' @param x a numeric vector of a single variable
#'
#' @return a numeric vector
#'
#' @details simple wrapper for `ave(variable, group)` function, to estimate cluster standard deviations
#'
#' @examples
#' example_data <- data.frame(x = c(1,2,3,4,5,6), j = c(1,1,1,2,2,2))
#' library(dplyr)
#' example_data %>%
#' mutate(cluster_sd =  c_sd(x, j)) %>%
#' select(x, j) %>%
#' unique() %>%
#' print
#' @export
c_sd <- function(x,j){
ave(x,j,FUN=function(x) sd(x, na.rm=T))
}
