#' local_path() returns the parent directory of the working directory.It aids the generation of relative directory to store all code in single folder, and then reproduce the calculations in a Mac OS machine or a Windows machine, or else.
#'
#' @param x a string with directory location (e.g, '/00_data/', '/01_syntax/', '/02_tables/')
#'
#' @return an absolute route or logical directory for the current machine, adding its root directory
#'
#' @examples
#'
#'
#' parent_directory <- local_path(getwd())
#' parent_directory #  this is the current working directory
#'                  #  it assumes the syntax folder, is within the parent directory
#' @export
local_path <- function(x){
  paste0(tools::file_path_as_absolute('..'),x)
}
