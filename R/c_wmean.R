#' c_wmean() computes the cluster weighted mean for a given variable, and a given clustering variable
#'
#' @param x a numeric vector of a single variable
#' @param w a numeric vector for within cluster weights
#' @param j a numeric vector for clusters index
#'
#' @return a numeric vector
#'
#' @details simple wrapper for  ave(x,j), to estimate cluster means weighted by within cluster weights
#'
#' @examples
#' library(dplyr)
#' library(dplyr)
#' example_data <- data.frame(
#' x = c(1,2,3,4,5,6),
#' w = c(.4,.4,.4,.6,.6,.6),
#' id_j = c(1,1,1,2,2,2)
#' )
#' example_data %>%
#' mutate(cluster_mean =  c_wmean(x, w, id_j)) %>%
#' select(x, id_j, cluster_mean) %>%
#' unique() %>%
#' print
# pseudo function
#' @export
c_wmean <- function(x,w,j){

  weighted_values <- w*x
  numerator       <- ave(weighted_values,j,FUN=function(x) sum(x, na.rm=T))
  denominator     <- ave(w,j,FUN=function(x) sum(x, na.rm=T))

  data_frame <- data.frame(
    numerator = as.numeric(numerator),
    denominator = as.numeric(denominator)) %>%
    mutate(wm = numerator/denominator)

  return(as.numeric(data_frame$wm))
}

