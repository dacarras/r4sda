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
#' mutate(cluster_mean =  c_wmean(x, w, id_j)) %>%
#' select(x, id_j, cluster_mean) %>%
#' unique() %>%
#' print
c_wmean <- function(x,w,j) {

  # required library
  require(dplyr)

  # generate vectores
  x = as.numeric(x)
  w = as.numeric(w)
  j = as.numeric(j)

  # generate temporal data
  clustered_data <- data.frame(
    variable = x,
    weight   = w,
    cluster  = j
  ) %>%
    group_by(cluster) %>%
    mutate(wm = matrixStats::weightedMean(variable,weight, na.rm=TRUE))

  # return
  return(as.numeric(clustered_data$wm))
}
