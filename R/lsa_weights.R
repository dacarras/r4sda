#' lsa_weights() computes normalized and effective sample size weights for pseudo maximum likelihood mixed models, with large scale assessment data
#'
#' @param data data frame object which contains the rest of the arguments
#' @param id_i a numeric vector of a single variable
#' @param id_j a numeric vector of a single variable
#' @param id_k a numeric vector of a single variable
#' @param wt observations final weight
#' @param wi observations weigth within the primary sampling unit (i.e. students eight within the school)
#' @param wj cluster weights (i.e. school weights)
#'
#' @return the original data frame with the new weights at the end, where `wa1` nad `wa2` are level 1 and level 2 normalized weights; `wb1` nad `wb2` are level 1 and level 2 effectives sample weights.
#'
#' @details a collection of dplyr::mutate sequences to create normalized and effective sample weights (see Rabe-Hesketh & Skrondal, 2006; Snijder & Bosker, 2012)
#'
#' @examples
#' library(dplyr)
#' data_frame_with_weights <- data_frame %>%
#'                            r4sda::lsa_weights(.,
#'                              id_i = 'id_i',
#'                              id_j = 'id_j',
#'                              id_k = 'id_k',
#'                              wt = 'wt',
#'                              wi = 'wi',
#'                              wj = 'wj' )
#' @export
lsa_weights <- function(data, id_i, id_j, id_k, wt, wi, wj){

# create data frame to produce weights
data_frame  <- data.frame(
            id_i = as.numeric(data[[id_i]]),
            id_j = as.numeric(data[[id_j]]),
            id_k = as.numeric(data[[id_k]]),
            wt = as.numeric(data[[wt]]),
            wi = as.numeric(data[[wi]]),
            wj = as.numeric(data[[wj]])
            )

require(dplyr)
data_weights  <- data_frame %>%
## method 2: normalized weights
            mutate(w   = wt)                             %>% # [01]
            mutate(w_sq  = wt^2)                         %>% # [02]
            mutate(sum_w = ave(w, id_j, FUN = sum))      %>% # [03]
            mutate(sum_wsq = ave(w_sq, id_j, FUN = sum)) %>% # [04]
            mutate(wa1 = (w*sum_w)/sum_wsq)              %>% # [05]
            mutate(wwa1   = wa1*wj)                      %>% # [06]
            mutate(awww1  = ave(wwa1, id_k))             %>% # [07] see notes
            mutate(wa2 = wj/awww1)                       %>% # [08]
## method 1: effective sample size
            group_by(id_j) %>% mutate(nj = n()) %>% ungroup() %>% # [09]
            mutate(wb1 = w*nj/sum_w)                          %>% # [10]
            mutate(wwb1 = wb1*wj)                             %>% # [11]
            mutate(awww2  = ave(wwb1, id_k))                  %>% # [12] see notes
            mutate(wb2 = wj/awww2)                            %>% # [13]
# keep only weights
            dplyr::select(wa1, wa2, wb1, wb2) %>%
            tibble::as_tibble()

# add to original data
data_output <- dplyr::bind_cols(data, data_weights)

# code steps

## method 2: normalized weights

# [01] a copy of the total weight
# [02] a square of the total weight
# [03] sum of total weights by schools
# [04] sum of square of total weights by schools
# [05] normalized within weights
# [06] normalized within weights by between raw weight
# [07] fixed mean of previous term (adapted for multiple country and single country datasets)
# [08] normalized between weights

## method 1: effective sample size

# [09] count of cases per cluster
# [10] effective sample size within weight
# [11] effective sample size within weight by between by raw weight
# [12] fixed mean of previous term (adapted for multiple country and single country datasets)
# [13] effective sample size between weight

# Sources:
#
# Rabe-Hesketh, S., & Skrondal, A. (2006).
#   Multilevel modelling of complex survey data.
#   Journal of the Royal Statistical Society.
#   Series A: Statistics in Society, 169(4), 805–827.
#   https://doi.org/10.1111/j.1467-985X.2006.00426.x

# Snijders, T. A. B., & Bosker, R. J. (2012).
#   Multilevel analysis: an introduction to
#   basic and advanced multilevel modeling (2nd ed.).
#   London: SAGE Publications Ltd.

#        Specially equations 14.19, page 232

# Notes: [07] and [02] are used to create the the w_j of the PISA 2009 report vol. IV, p. 143.
#        This has mean 1 over the whole data set.

return(data_output)
}
