#' check_cluster_id() checks if cluster id variable is unique a cross other nested variables. It was designed to assess if schools id are unique countries, and if strata id are unique across countries and regions.
#'
#' @param data a data frame with nested observations
#' @param cluster_1 the cluster id one's wants to check over a hierachical cluster (i.e. schools within countries)
#' @param cluster_2 the cluster id which contains the checked cluster (i.e. countries that contains schools)
#'
#' @return a logical message
#'
#' @examples
#'
#'
#' data_example %>%
#' check_cluster_id(
#'   cluster_1 = 'schools',
#'   cluster_2 = 'country')
#'
#' @export
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
                        paste0('cluster_1 = ',cluster_1_name,' is unique across cluster_2 = ', cluster_2_name),
                        paste0('cluster_1 = ',cluster_1_name,' is repeated between cluster_2 = ', cluster_2_name)))
}
