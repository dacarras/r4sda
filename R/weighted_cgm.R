#' weighted_cgm() computes the grand mean centering, while including normalized survey weights
#'
#' @param data a data frame with variables id_j (cluster variable), and id_k (higher cluster, e.g., country)
#' @param variable variable to be center
#' @param name acronym for the generated variables
#'
#' @return a data frame, including the center variables
#'
#' @examples
#' library(dplyr)
#' data_model %>%
#' center_variables(data = ., variable = sex , name = sex) %>%
#' dplyr::glimpse()
#' @export
weighted_cgm <- function(data, variable, weight, name){

#------------------------------------------------
# function elements
#------------------------------------------------

data_model <- data
covariate  <- rlang::enquo(variable)
weights    <- rlang::enquo(weight)

#------------------------------------------------
# create centered variables
#------------------------------------------------

data_model <- data_model %>%
# raw score
mutate("{{name}}" := !!covariate) %>%  
# cluster means
# grand mean
mutate("{{name}}_g" := r4sda::c_wmean(!!covariate, !!weights, id_k)) %>%
# centered to the grand mean
mutate("{{name}}_m" := {{name}} - r4sda::c_wmean(!!covariate, !!weights, id_k))

#------------------------------------------------
# return
#------------------------------------------------
return(data_model)

}