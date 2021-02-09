#' compute_surrogate_calib
#'
#' @param MSdata a tibble containing the surrogate calibration standards
#' @param model a character string indicating whether a "linear", "quadratic" or
#'   "exponential" model was used
#'
#' @return
#' @export
compute_surrogate_calib <- function(MSdata, model = "quadratic") {
  if (model == "linear") {
    regr <- lm(rel_response ~ surrog_conc,
               data = MSdata,
               weights = I(1/surrog_conc^2))
  } else if (model == "quadratic") {
    regr <- lm(rel_response ~ surrog_conc + I(surrog_conc^2),
               data = MSdata,
               weights = I(1/surrog_conc^2))
  } else if (model == "exponential") {
    start_a <- min(MSdata$rel_response) + 0.0001    # don't want zero
    start_b <- 0.2    # estimated rate of increase
    regr <- nls(rel_response ~ I(a * exp(b * surrog_conc)),
                data = MSdata,
                start = list(a = start_a, b = start_b),
                trace = TRUE)
  } else if (!model %in% c("linear", "quadratic", "exponential")) {
    stop("The regression model must be \"linear\",  \"quadratic\", or \"exponential\"")
  }
  return(regr)
}
