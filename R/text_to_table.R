#' text_to_table() can take unstructure text, and turn into a long table. It was design to take Mplus variable section and produce a table where the second column is each of the included variables. However, this function could be use for other purposes.
#'
#' @param x a text input
#'
#' @return data frame where column one is the position of all variables, and the second column contain all the terms from the text input.
#' @export
#'
#' @examples
#'
#' text_input <- "
#' IDCNTRY COUNTRY IDSTUD IDCLASS IDSCHOOL
#' IS2G05 IS2G07 IS2G09 IS2G10A IS2G10B
#' id_i id_k id_s id_j
#' "
#'
#' text_to_table(text_input)
#'
text_to_table <- function(x){

  # require libraries
  require(stringi)
  require(dplyr)
  require(tidyr)

text_input <- x

# turn collection of text into a matrix
matrix <- stringi::stri_split_fixed(text_input, " ", simplify = TRUE)

# turn into a row
one_row <- as.data.frame(matrix)

library(dplyr)
variable_list <- tidyr::gather(one_row, position, variable) %>%
  filter(variable!="") %>%
  mutate(variable = stringr::str_replace_all(variable, "([\n\t])", "")) %>%
  mutate(position = seq(1:nrow(.)))

return(variable_list)
}


