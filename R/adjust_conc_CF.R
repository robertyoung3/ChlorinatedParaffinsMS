#' adjust_conc_CF
#'
#' @param MSdata a tibble containing SCCP congener data
#' @param model The computed model used to predict the mixture
#'   concentration ("RF_model" or "calib_model")
#'
#' @return an augmented tibble containing the solid concentration
#' @export
adjust_conc_CF <- function(MSdata, model = "RF_model") {
  if (model == "RF_model") {
    MSdata <- MSdata %>%
      dplyr::mutate(adjusted_conc_RF = estimated_conc_RF / CF)
  } else if (model == "calib_model") {
      MSdata <- MSdata %>%
        dplyr::mutate(adjusted_conc_calib = estimated_conc_calib / CF)
  } else {
    stop("The calculation requires a response factor or calibration model")
  }
  return(MSdata)
}
