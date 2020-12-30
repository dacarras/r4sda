#' svy_freq() estimates proportion for observed values of a categorical variable, from a survey object.
#'
#' @param data a survey object, or survey data frame (i.e. TSL, JKN, BRR or else)
#' @param variable selected varible from the survey object
#'
#' @return a tibble with the estimated proportion, and their 95% confidence interval
#' @export
#'
#' @examples
#'
#'
#' data_svy %>%
#' svy_freq(
#'   data = .,
#'   variable = selected_var)
#'
svy_freq <- function(data, variable){

  require(dplyr)
  require(rlang)

  svy_data <- data
  var_name <- enquo(variable)
  col_name <- quo_name(enquo(variable))
  reg_name <- tibble::tibble(variable = !!col_name) %>%
    .$variable %>%
    as.character()

  list_of_values <- svy_data$variables %>%
    dplyr::select(one_of(reg_name)) %>%
    dplyr::distinct() %>%
    r4sda::remove_labels() %>%
    haven::zap_formats() %>%
    dplyr::arrange(across(one_of(reg_name))) %>%
    as.list() %>%
    unlist() %>%
    as.character() %>%
    as.numeric()

  single_value <- function(value){
    model_formula <- as.formula(paste0('~I(',reg_name,' %in% ',value,')'))
    survey::svyciprop(model_formula, svy_data, method="lo", df=degf(svy_data), level = .95) %>%
      c(attr(.,"ci")) %>%
      tibble::as_tibble() %>%
      mutate(term = c('est','ll','ul')) %>%
      tidyr::gather(key = 'response', value = 'estimates', -term) %>%
      tidyr::spread(key = 'term', value = 'estimates') %>%
      mutate(response = as.character(value))
  }

  table_freq <- list_of_values %>%
    purrr::map(single_value) %>%
    purrr::reduce(dplyr::bind_rows)

  return(table_freq)
# Note: based on IRTM approach:
# https://stackoverflow.com/questions/40461753/finding-proportions-for-categorical-data-in-a-survey
}
