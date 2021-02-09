#' estimate_surrogate_conc
#'
#' @param MSdata a tibble containing surrogate data
#'
#' @return an augmented tibble containing the predicted concentration
#' @export
estimate_surrogate_conc <- function(MSdata) {
  # quadratic model
  if (length(surrogate_calib_model$coefficients) == 3) {
    # y = ax^2 + bx + c; y = rel_response; x = concentration
    # ax^2 + bx + c - y = 0; new_c = c - y
    # x = [-b_x (+/-) sqrt(b_x^2 - 4*a_x2*new_c)] / 2*a_x2
    # (c_intercept - rel_response)
    c_intercept <- surrogate_calib_model$coefficients[[1]]
    b_x <- surrogate_calib_model$coefficients[[2]]
    a_x2 <- surrogate_calib_model$coefficients[[3]]
    MSdata <- MSdata %>%
      dplyr::mutate(soln_1 = (-b_x + sqrt(b_x^2 - (4 * a_x2 * (c_intercept - rel_response)))) / (2*a_x2),
                    soln_2 = (-b_x - sqrt(b_x^2 - (4 * a_x2 * (c_intercept - rel_response)))) / (2*a_x2),
                    estimated_conc_calib = pmax(soln_1, soln_2)) %>%
      dplyr::select(-soln_1, -soln_2)
  } else {
  # y = mx + b; y = rel_response; x = concentration
  # x = (y - b_x) / m_slope
  b_intercept <- surrogate_calib_model$coefficients[[1]]
  m_slope <- surrogate_calib_model$coefficients[[2]]
  MSdata <- MSdata %>%
    dplyr::mutate(estimated_conc_calib = (rel_response - b_intercept) / m_slope)
  }
  return(MSdata)
}
