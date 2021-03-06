% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gen_chemformula.R
\name{gen_chemformula}
\alias{gen_chemformula}
\title{gen_chemformula}
\usage{
gen_chemformula(elements_df)
}
\arguments{
\item{elements_df}{a data.frame containing elements as variables and element
counts as values.}
}
\value{
a character string containing the corresponding chemical formula
}
\description{
gen_chemformula
}
\examples{
elements_df <- tibble::tibble(C = 17, H = 22, Br = 1, F = 13, O = NA, S = 0)
gen_chemformula(elements_df)
}
