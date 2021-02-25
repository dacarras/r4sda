#' remove_labels() from a data frame
#'
#' @param data data frame with labels
#'
#' @return the original data frame without any labels
#'
#' @examples
#' library(dplyr)
#' data_without_labels <- data_frame %>%
#'                        remove_labels()
#'
#'
#' @export
remove_labels <- function(data){
require(haven)
require(labelled)
require(sjlabelled)
data <- data %>%
          haven::zap_label() %>%
          labelled::remove_labels() %>%
          sjlabelled::remove_all_labels()
  return(data)
}
