#' decimal() takes any numeric variable with decimal places, and returns its aproximation of decimal places to a given number.
#'
#' @param x a numeric vector of a single variable
#'
#' @return a string containing a numeric value, with a given number of decimals
#'
#' @details simple wrapper for `format(round(x, k), nsmall=k)` function, to format decimal numbers
#'
#' @examples
#'
#' number_with_decimals <- .5838
#' number_with_two_decimals <- decimal(number_with_decimals,2)
#' # result is .58
#' number_with_two_decimals
#' @export
decimal <- function(x, k){
  format(round(x, k), nsmall=k)

  # Ref: https://stackoverflow.com/questions/3443687/formatting-decimal-places-in-r
}
