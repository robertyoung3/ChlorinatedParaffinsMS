#' compute_RF_tech_mix
#'
#' @param MSdata a tibble containing SCCP congener data
#'
#' @return an augmented tibble including computed RF data
#' @export
compute_RF_tech_mix <- function(MSdata) {
  MSdata %>%
    dplyr::mutate(experimental_RF = total_congener_response / concentration)
}
