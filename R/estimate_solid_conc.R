#' estimate_solid_conc
#'
#' @param MSdata a tibble containing SCCP congener data
#' @param model The computed model used to predict the mixture
#'   concentration ("RF_model" or "calib_model")
#'
#' @return an augmented tibble containing the solid concentration
#' @export
estimate_solid_conc <- function(MSdata, model = "RF_model") {
  if (model == "RF_model") {
    MSdata <- MSdata %>%
      dplyr::mutate(estimated_solid_conc_RF = adjusted_conc_RF / sample_mass)
  } else if (model == "calib_model") {
      MSdata <- MSdata %>%
        dplyr::mutate(estimated_solid_conc_calib = adjusted_conc_calib / sample_mass)
  } else {
    stop("The calculation requires a response factor or calibration model")
  }
  return(MSdata)
}
