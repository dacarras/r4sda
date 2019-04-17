#' zscore() it standardized a continous measure
#'
#' @param x a numeric vector
#'
#' @return an standardized variable (in z score)
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

