#' char_location() gets the location of a character within a string
#'
#' @param string is a string or text line
#' @param char is character we want to locate
#'
#' @return returns a number, which is the location before the character is found within a string
#' @export
#'
#' @details is a simple wrapper of the function `stringr::str_locate`. Is used to generate codebook.
#'
#' @examples
#' char_location('this is text', 'x')
char_location <- function(string,char){
stringr::str_locate(string, char)[, 1]
}
