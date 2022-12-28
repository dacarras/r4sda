#' center_variables() computes the centered variables for mixed models
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
center_variables <- function(data, variable, name){

#------------------------------------------------
# function elements
#------------------------------------------------

data_model <- data
covariate  <- rlang::enquo(variable)

#------------------------------------------------
# create centered variables
#------------------------------------------------

data_model <- data_model %>%
# raw score
mutate("{{name}}" := !!covariate) %>%  
# cluster means
mutate("{{name}}_c" := r4sda::c_mean(!!covariate, id_j)) %>%
# grand mean
mutate("{{name}}_g" := r4sda::c_mean(!!covariate, id_k)) %>%
# centered to the grand mean
mutate("{{name}}_m" := {{name}} - r4sda::c_mean(!!covariate, id_k)) %>%
# centered within cluster
mutate("{{name}}_w" := {{name}} - r4sda::c_mean(!!covariate, id_j)) %>%
# cluster means centered at the grand mean
mutate("{{name}}_b" := r4sda::c_mean(!!covariate, id_j) - r4sda::c_mean(!!covariate, id_k)) 

#------------------------------------------------
# return
#------------------------------------------------
return(data_model)

}