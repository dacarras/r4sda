#' variables_table() produces a simple codebook, based on the output of a dplyr::glimpse()
#'
#' @param d is a data_frame
#'
#' @return returns a data_frame in tibble::tibble format.
#'
#' @details it relies on char_location, variable_label, stringr::str_sub, purr::map_dfr, dplyr::glimpse
#'
#' @examples
#' variable_label(data_frame$variable)
#'
#' @export
variables_table <- function(d){
  # required libraries
  require(dplyr)
  require(purrr)
  require(stringr)

  ## get main objects
  var_structure <- capture.output(dplyr::glimpse(d))
  text_lines    <- var_structure[-(1:2)]

  ## extract specific info
  variable_names <- names(purrr::map_dfr(d, variable_label))

  ## these needs to be specific
  variable_type  <- stringr::str_sub(text_lines,
    start=char_location(text_lines,'<')+1,
    end=char_location(text_lines,'>')-1)
  sample_values  <- as.character(
    stringr::str_sub(text_lines,
                     start=51,
                     end=120)
  )
  labels_values  <- as.character(purrr::map_dfr(d, variable_label)[1,])

  ## NOTE: start and end figures need to be determined beforehand. The current values
  #        were get by trial and error.

  ## create variable list table
  variable_list <- tibble::tibble(
      variable  = variable_names,
      type      = variable_type,
      values    = sample_values,
      labels    = labels_values,
      )

return(variable_list)
}
