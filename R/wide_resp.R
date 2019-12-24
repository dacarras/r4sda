#' wide_resp() produces a frequency table in percentages where items are rows and columns are response values
#'
#' @param x a data frame, where rows = observations, and columns = variables
#'
#' @return data frame with a frequency table
#' @export
#'
#' @examples
#'
#'
#' items_data <- dplyr::select(data_frame, item_1:item_9)
#' wide_resp(items_data)
#'
wide_resp <- function(x){
  # remove warnings
  options(warn=-1)

  # load dplyr
  require(dplyr)

  # recode all missing
  x <- mutate_all(x, funs(replace(., is.na(.), -999)))


  # freq_table
  table_freq <- function(x){
    as.data.frame(table(x))
  }

  # create stacked table
  table <- lapply(x, table_freq) %>%
    dplyr::bind_rows(., .id = 'var') %>%
    rename(resp = x, n = Freq) %>%
    group_by(var) %>%
    mutate(per = n/sum(n)) %>%
    mutate(resp = as.character(resp)) %>%
    mutate(resp = if_else(resp=='-999','NA',resp)) %>%
    dplyr::select(var, resp, per)

  # wide variable table
  wide_resp <- tidyr::spread(table, resp, per)%>%
    rename(variable = var)

  return(wide_resp)
  options(warn=0)
}
