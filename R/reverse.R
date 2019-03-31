#' reverse() it reverse a score in a linear way, using the minimum and maximum values. Is often use onto likert type response variables, when the original responses are not coded with values that follow the direction of higher values are higher level of the attribute.
#'
#' @param x a numeric vector
#'
#' @return a reverse coded numeric vector
#' @export
#'
#' @examples
#' \donotrun{
#' items_data <- items_data %>%
#'               mutate(item = reverse(original))
#' dplyr::count(items_data, original, item)
#' }
reverse <- function(var){
max <- max(var, na.rm = TRUE)
min <- min(var, na.rm = TRUE)
return(max + min - var)
}
