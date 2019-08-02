#' c_wmean() computes the cluster weighted mean for a given variable, and a given clustering variable
#'
#' @param data a data frame where the variables are contained
#' @param x a numeric vector of a single variable
#' @param w a numeric vector for within cluster weights
#' @param j a numeric vector for clusters index
#'
#' @return a numeric vector
#' @export
#'
#' @details simple wrapper for  with matrixStats::weightedMean(), to estimate cluster means weighted by within cluster weights
#'
#' @examples
#' library(dplyr)
#' data_frame %>%
#' mutate(cluster_mean =  c_wmean(., 'x', 'w', 'id_j')) %>%
#' select(x, id_j, cluster_mean) %>%
#' unique() %>%
#' print
# pseudo function
c_wmean <- function(data, x,w,j) {

  data_frame  <- data.frame(
    x = data[[rlang::quo_name(enquo(x))]],
    w = data[[rlang::quo_name(enquo(w))]],
    j = data[[rlang::quo_name(enquo(j))]]
  ) %>%
    group_by(j) %>%
    mutate(wm = matrixStats::weightedMean(x, w, na.rm=TRUE)) %>%
    ungroup()

  return(as.numeric(data_frame$wm))
}
