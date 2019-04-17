#' zscore() it standardized a continous variable
#'
#' @param x a numeric vector
#'
#' @return an standardized variable, as a z score
#' @export
#'
#' @examples
#' \donotrun{
#' items_data <- items_data %>%
#'               mutate(zcore = zscore(original))
#' skimr::skim(items_data$zcore)
#' }
zscore <- function(x){
as.numeric(scale(x, center = TRUE, scale = TRUE))
}
