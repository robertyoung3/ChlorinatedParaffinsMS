#' compute_calib_model_zero_intercept
#'
#' @param MSdata a tibble containing the response factors and wt% Cl of a set of
#'   SCCP technical mixture standards
#' @param model a character string indicating whether a "linear", "quadratic" or
#'   "exponential" model was used
#'
#' @return
#' @export
#
compute_calib_model_zero_intercept <- function(MSdata, model = "quadratic") {
  if (model == "linear") {
    regr <- lm(total_congener_response ~ concentration -1,
               data = MSdata)#,
#               weights = I(1/concentration^2))
  } else if (model == "quadratic") {
    regr <- lm(total_congener_response ~ concentration + I(concentration^2) - 1,
               data = MSdata,
               weights = I(1/concentration^2))
  } else if (model == "exponential") {
    start_a <- min(MSdata$total_congener_response) + 0.0001    # don't want zero
    start_b <- 0.2    # estimated rate of increase
    regr <- nls(total_congener_response ~ I(a * exp(b * concentration)),
                data = MSdata,
                start = list(a = start_a, b = start_b),
                trace = TRUE)
  } else if (!model %in% c("linear", "quadratic", "exponential")) {
    stop("The regression model must be \"linear\",  \"quadratic\", or \"exponential\"")
  }
  return(regr)
}
