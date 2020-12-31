#' value_labels() gets the value labels from a labelled variable, from a data_frame, is simple wrapper from haven::print_labels()
#'
#' @param d is a variable from a data_frame
#'
#' @return returns a tibble, which contains the value label of a variable, from a labelled data_frame
#'
#' @details is a wrapper to extract the label values from an object
#'
#' @examples
#' value_labels(data_frame$variable)
#'
#' @export
value_labels <- function (x, name = NULL)
{
    if (!haven::is.labelled(x)) {
        stop("x must be a labelled vector", call. = FALSE)
    }
    labels <- attr(x, "labels")
    if (length(labels) == 0) {
        return(invisible(x))
    }
    value <- if (is.double(labels))
        haven::format_tagged_na(labels)
    else unname(labels)
    lab_df <- tibble::tibble(
        value = value,
        label = names(labels))
    rownames(lab_df) <- NULL
    return(lab_df)
    invisible(x)
# source: based largely on haven::print_labels()
#         but returns a tibble object
}
