#' compute_RF_model
#'
#' @param MSdata a tibble containing the response factors and wt% Cl of a set of
#'   SCCP technical mixture standards
#' @param model a character string in c("linear", "quadratic", "exponential")
#'
#' @return
#' @export
compute_RF_model <- function(MSdata, model = "quadratic") {
  if (model == "linear") {
    regr <- lm(experimental_RF ~ experimental_perc_Cl,
               weights = I(1/experimental_perc_Cl^2),
               data = MSdata)
  } else if (model == "quadratic") {
    regr <- lm(experimental_RF ~ poly(experimental_perc_Cl, degree = 2),
               data = MSdata,
               weights = I(1/experimental_perc_Cl^2))
  } else if (model == "exponential") {
    start_a <- min(MSdata$experimental_RF)
    start_b <- 0.2   # estimated rate of increase
    regr <- nls(experimental_RF ~ I(a * exp(b * (experimental_perc_Cl - min(experimental_perc_Cl)))),
#    regr <- nls(experimental_RF ~ I(a * exp(b * experimental_perc_Cl)), -> fails
                            data = MSdata,
                weights = I(1/experimental_perc_Cl^2),
                start = list(a = start_a, b = start_b),
                trace = TRUE)
  } else if (!model %in% c("linear", "quadratic", "exponential")) {
    stop("The regression model must be \"linear\",  \"quadratic\", or \"exponential\"")
  }
  return(regr)
}
