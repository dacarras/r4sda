#' z_score() it standardized a continuous variable
#'
#' @param x a numeric vector
#'
#' @return an standardized variable, as a z score
#' @export
#'
#' @examples
#'
#' items_data <- items_data %>%
#'               mutate(zcore = zscore(original))
#'
#' @export
z_score <- function(x){
return(as.numeric(scale(x, center = TRUE, scale = TRUE)))
}
