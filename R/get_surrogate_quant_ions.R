#' get_surrogate_quant_ions
#'
#' @param MSdata a tibble containing the surrogate data
#'
#' @return a tibble containing the surrogate quantification ions
#' @export
get_surrogate_quant_ions <- function(MSdata) {
  MSdata %>%
    dplyr::rename(nominal_perc_Cl = num1,
                  concentration = num2) %>%
    dplyr::filter(peak_label == "T1")
}
