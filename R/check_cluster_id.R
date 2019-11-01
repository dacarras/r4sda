#' z_score() it standardized a continous variable
#'
#' @param x a numeric vector
#'
#' @return an standardized variable, as a z score
#' @export
#'
#' @examples
#' \donotrun{
#'
#' data_example %>%
#' check_cluster_id(
#'   id_j = 'schools',
#'   id_k = 'country')
#' }
check_cluster_id <- function(data, cluster_1, cluster_2){
  # libraries we need
  require(dplyr)

  # create temp data out of the clustered variables
  data_clustered  <- data.frame(
    cluster_1 = as.numeric(data[[cluster_1]]),
    cluster_2 = as.numeric(data[[cluster_2]])
  )

  # check if id_j is unique (e.g. where schools id are id_j)
  counter_1 <- data_clustered %>%
    dplyr::select(cluster_1) %>%
    unique() %>%
    nrow()

  # check if id_k is unique (e.g. where country id is id_k)
  counter_2 <- nrow(dplyr::count(data_clustered, cluster_1, cluster_2))

  # extract cluster variable names
  cluster_1_name <- as.character(cluster_1)
  cluster_2_name <- as.character(cluster_2)

  # test
  return(dplyr::if_else(counter_1 == counter_2,
                        paste0(cluster_1_name,' id is unique between ', cluster_2_name, ' id'),
                        paste0(cluster_2_name,' id repeats between ', cluster_2_name, ' id')))
}
