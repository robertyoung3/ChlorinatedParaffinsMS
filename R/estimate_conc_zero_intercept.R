#' estimate_conc_zero_intercept
#'
#' @param MSdata a tibble containing SCCP congener data
#'
#' @return an augmented tibble containing the predicted concentration
#' @export
estimate_conc_zero_intercept <- function(MSdata) {
  if (length(calib_model_zero$coefficients) == 2) {
      # y = ax^2 + bx; y = total_congener_response; x = concentration
      # ax^2 + bx + c - y = 0; new_c = c - y; c = 0
      # x = [-b_x (+/-) sqrt(b_x^2 + 4*a_x2*y)] / 2*a_x2
      # (c_intercept - total_congener_response)
      b_x <- calib_model_zero$coefficients[[1]]
      a_x2 <- calib_model_zero$coefficients[[2]]
      MSdata <- MSdata %>%
        dplyr::mutate(soln_1 = (-b_x + sqrt(b_x^2 + (4 * a_x2 * total_congener_response))) / (2*a_x2),
                      soln_2 = (-b_x - sqrt(b_x^2 + (4 * a_x2 * total_congener_response))) / (2*a_x2),
                      estimated_conc_calib = pmax(soln_1, soln_2)) %>%
        dplyr::select(-soln_1, -soln_2)

  } else {
      # y = mx; y = total_congener_response; x = concentration
      # x = y / m_slope
      m_slope <- calib_model_zero$coefficients[[1]]
      MSdata <- MSdata %>%
        dplyr::mutate(estimated_conc_calib = total_congener_response / m_slope)
  }
  return(MSdata)
}
