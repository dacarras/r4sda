#' mean_score() computes the mean for a given set of variables in a row wise manner
#'
#' @param x,y,z are set of variables from a data frame (i.e. a selection of numeric columns)
#'
#' @return a numeric vector
#' @export
#'
#' @details simple wrapper to produce row wise means (default is na.rm=TRUE)
#'
#' @examples
#' library(dplyr)
#' data_frame %>%
#' mutate(score =  mean_score(x, z, y))
#'
#'
#'
mean_score = function(..., na.rm=TRUE){
rowMeans(cbind(...), na.rm=na.rm)
# source: https://stackoverflow.com/questions/33401788/dplyr-using-mutate-like-rowmeans
}
