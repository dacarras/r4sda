% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/value_labels.R
\name{value_labels}
\alias{value_labels}
\title{value_labels() gets the value labels from a labelled variable, from a data_frame, is simple wrapper from haven::print_labels()}
\usage{
value_labels(x, name = NULL)
}
\arguments{
\item{d}{is a variable from a data_frame}
}
\value{
returns a tibble, which contains the value label of a variable, from a labelled data_frame
}
\description{
value_labels() gets the value labels from a labelled variable, from a data_frame, is simple wrapper from haven::print_labels()
}
\details{
is a wrapper to extract the label values from an object
}
\examples{
value_labels(data_frame$variable)

}
