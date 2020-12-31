#' variable_label() gets the variable label from a labelled variable, from a data_frame
#'
#' @param d is a variable from a data_frame
#'
#' @return returns a string, which contains the variable label of variable, from a labelled data_frame
#'
#' @details is a wrapper to extract the label attribute from an object
#'
#' @examples
#' variable_label(data_frame$variable)
#'
#' @export
variable_label <- function(d){
ifelse(is.null(attr(d,'label')),
  '=== no variable label ===',
  attr(d,'label'))
}

