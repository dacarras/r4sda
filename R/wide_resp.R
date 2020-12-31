#' wide_resp() produces a frequency table in percentages where items are rows and columns are response values
#'
#' @param x a data frame, where rows = observations, and columns = variables
#'
#' @return data frame with a frequency table
#'
#' @examples
#'
#'
#' items_data <- dplyr::select(data_frame, item_1:item_9)
#' wide_resp(items_data)
#'
#' @export
wide_resp <- function(x){
  # remove warnings
  options(warn=-1)

  # load dplyr
  require(dplyr)

  # get table for histogram
  y <- as.data.frame(x)

  # recode all missing
  x <- mutate_all(x, funs(replace(., is.na(.), -999)))

  # order of table
  order_table <- data.frame(
    variable = as.character(names(x)),
    var_order = seq(1:length(names(x)))
  )

  # freq_table
  table_freq <- function(x){
    as.data.frame(table(x))
  }

  # histograms
  get_hist <- function(x){
    wide_table <- x %>%
      r4sda::remove_labels() %>%
      summarise_all(list(
        hist = ~skimr::inline_hist(.)
      ))
    hist_table <- data.frame(
      variable = as.character(names(x)),
      hist = as.character(tidyr::gather(wide_table)$value)
    )
    return(hist_table)
  }

  # create stacked table
  table <- lapply(x, table_freq) %>%
    dplyr::bind_rows(., .id = 'var') %>%
    rename(resp = x, n = Freq) %>%
    group_by(var) %>%
    mutate(per = n/sum(n)) %>%
    mutate(resp = as.character(resp)) %>%
    mutate(resp = case_when(
      nchar(resp)==1 ~ paste0(0,resp),
      TRUE ~ as.character(resp))) %>%
    mutate(resp = if_else(resp=='-999','NA',resp)) %>%
    arrange(resp) %>%
    dplyr::select(var, resp, per)

  # wide variable table
  wide_resp <- tidyr::spread(table, resp, per) %>%
    rename(variable = var) %>%
    left_join(., order_table, by = 'variable') %>%
    left_join(.,get_hist(y), by = 'variable') %>%
    arrange(var_order) %>%
    dplyr::select(-var_order) %>%
    mutate(hist = as.character(hist))

  return(wide_resp)
  options(warn=0)
}
