% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimate_conc.R
\name{estimate_conc}
\alias{estimate_conc}
\title{estimate_conc}
\usage{
estimate_conc(MSdata, model = "RF_model")
}
\arguments{
\item{MSdata}{a tibble containing SCCP congener data}

\item{model}{The computed model used to predict the mixture concentration ("RF_model" or "calib_model")}
}
\value{
an augmented tibble containing the predicted response factor and/or concentration
}
\description{
estimate_conc
}
