#' z_score() it standardized a continous variable
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
#' skimr::skim(items_data$zcore)
#'
z_score <- function(x){
return(as.numeric(scale(x, center = TRUE, scale = TRUE)))
}
