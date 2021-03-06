% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/text_to_table.R
\name{text_to_table}
\alias{text_to_table}
\title{text_to_table() can take unstructure text, and turn into a long table. It was design to take Mplus variable section and produce a table where the second column is each of the included variables. However, this function could be use for other purposes.}
\usage{
text_to_table(x)
}
\arguments{
\item{x}{a text input}
}
\value{
data frame where column one is the position of all variables, and the second column contain all the terms from the text input.
}
\description{
text_to_table() can take unstructure text, and turn into a long table. It was design to take Mplus variable section and produce a table where the second column is each of the included variables. However, this function could be use for other purposes.
}
\examples{

text_input <- "
IDCNTRY COUNTRY IDSTUD IDCLASS IDSCHOOL
IS2G05 IS2G07 IS2G09 IS2G10A IS2G10B
id_i id_k id_s id_j
"

text_to_table(text_input)

}
