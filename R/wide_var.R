#' wide_var() produces a frequency table with percentages where responses are rows, and columnas are variables.
#'
#' @param x a data frame, where rows = observations, and columns = variables
#'
#' @return data frame with a frequency table
#'
#' @examples
#'
#' items_data <- dplyr::select(data_frame, item_1:item_9)
#' wide_var(items_data)
#'
#' @export
wide_var <- function(x){
# remove warnings
options(warn=-1)

# load dplyr
require(dplyr)

# remove all missing
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
          mutate(resp = na_if(resp, -999)) %>%
          mutate(resp = as.numeric(resp)) %>%
          dplyr::select(var, resp, per)

# wide variable table
wide_var <- tidyr::spread(table, var, per)
return(wide_var)
options(warn=0)
}
