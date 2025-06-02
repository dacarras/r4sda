#' na_score() computes a count of missings across a given set of variables in a row wise manner
#'
#' @param x,y,z are set of variables from a data frame (i.e. a selection of numeric columns)
#'
#' @return a numeric vector
#'
#' @details simple wrapper to produce row wise sums (default is na.rm=TRUE)
#'
#' @examples
#' library(dplyr)
#' data_frame %>%
#' mutate(na_count   =  na_score(data = ., var_txt = c('x', 'z', 'y')))
#'
#'
#' @export
na_score <- function(data, var_txt){

library(dplyr)

na_count <- data %>%
            dplyr::select(all_of(var_txt)) %>%
            mutate(na_count = rowSums(is.na(.))) %>%
            dplyr::select(na_count) %>%
            dplyr::pull()

return(na_count)

}
