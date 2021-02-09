#' estimate_conc
#'
#' @param MSdata a tibble containing SCCP congener data
#' @param model The computed model used to predict the mixture concentration ("RF_model" or "calib_model")
#'
#' @return an augmented tibble containing the predicted response factor and/or concentration
#' @export
estimate_conc <- function(MSdata, model = "RF_model") {
  if (model == "RF_model") {
    MSdata <- MSdata %>%
      dplyr::mutate(predicted_RF = predict(RF_model, newdata = .),
                    estimated_conc_RF = total_congener_response / predicted_RF)
    if (MSdata$predicted_RF < 0) {
      MSdata$estimated_conc_RF <- NA
    }
  } else if (model == "calib_model") {
    if (length(calib_model$coefficients) == 3) {
      # y = ax^2 + bx + c; y = total_congener_response; x = conc
      # ax^2 + bx + c - y = 0; new_c = c - y
      # x = [-b_x (+/-) sqrt(b_x^2 - 4*a_x2*new_c)] / 2*a_x2
      # (c_intercept - total_congener_response)
      c_intercept <- calib_model$coefficients[[1]]
      b_x <- calib_model$coefficients[[2]]
      a_x2 <- calib_model$coefficients[[3]]
      MSdata <- MSdata %>%
        dplyr::mutate(soln_1 = (-b_x + sqrt(b_x^2 - (4 * a_x2 * (c_intercept - total_congener_response)))) / (2*a_x2),
                      soln_2 = (-b_x - sqrt(b_x^2 - (4 * a_x2 * (c_intercept - total_congener_response)))) / (2*a_x2),
                      estimated_conc_calib = pmax(soln_1, soln_2)) #%>%
#        dplyr::select(-soln_1, -soln_2)

    } else {
      # y = mx + b; y = total_congener_response; x = conc
      # x = (y - b_intercept) / m_slope
      b_intercept <- calib_model$coefficients[[1]]
      m_slope <- calib_model$coefficients[[2]]
      MSdata <- MSdata %>%
        dplyr::mutate(estimated_conc_calib = (total_congener_response - b_intercept) / m_slope)
    }
  } else {
  stop("The calculation requires a response factor or calibration")
  }
  return(MSdata)
}
