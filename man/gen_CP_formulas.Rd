% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gen_CP_formulas.R
\name{gen_CP_formulas}
\alias{gen_CP_formulas}
\title{gen_CP_formulas}
\usage{
gen_CP_formulas(C_num, Cl_num)
}
\arguments{
\item{C_num}{an integer or range of integers representing the number of
carbons in a chlorinated paraffin formula}

\item{Cl_num}{an integer or range of integers representing the number of
chlorines in a chlorinated paraffin formula}
}
\value{
a vector of character strings
}
\description{
Produces a dataframe with the elemental composition of chlorinated paraffin
formulas with the specified number of carbons and chlorines.
}
\examples{
C_num <- 10:13
Cl_num <- 5:10
gen_CP_formulas(C_num, Cl_num)
}
