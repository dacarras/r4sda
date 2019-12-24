#' local_path() turns a relative directory into a absolute directory. It aids the generation of relative directory to store all code in single folder, and then reproduce the calculations in a Mac OS machine or a Windows machine, or else. Is simple wrapper for `tools::file_path_as_absolute(x)`, to which it adds a final `/`.
#'
#' @param x a string with directory location
#'
#' @return an absolute route or logical directory for the current machine
#' @export
#'
#' @examples
#'
#'
#' working_directory <- local_path(getwd())
#' working_directory # this is the current working directory
#'
local_path <- function(x){
paste0(tools::file_path_as_absolute(x),'/')
}
