#' na_score() computes a count of missings across a given set of variables in a row wise manner
#'
#' @param x,y,z are set of variables from a data frame (i.e. a selection of numeric columns)
#'
#' @return a numeric vector
#'
#' @details simple wrapper to produce rowwise sums (default is na.rm=TRUE)
#'
#' @examples
#' library(dplyr)
#' data_frame %>%
#' mutate(na_count =  na_score(x, z, y))
#'
#'
#' @export
na_score <- function(..., na.rm=TRUE){
rowSums(is.na(cbind(...)), na.rm=na.rm)
# source: https://stackoverflow.com/questions/33401788/dplyr-using-mutate-like-rowmeans
# by: https://stackoverflow.com/users/1191259/frank
}

