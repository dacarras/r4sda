#' remove_labels() from a data frame
#'
#' @param data data frame with labels
#'
#' @return the original data frame without any labels
#' @export
#'
#' @examples
#' library(dplyr)
#' data_without_labels <- data_frame %>%
#'                        remove_labels()
#'

# pseudo function
remove_labels <- function(data){
  data <- data %>%
          haven::zap_label() %>%
          labelled::remove_labels() %>%
          sjlabelled::remove_all_labels()
  return(data)
}
