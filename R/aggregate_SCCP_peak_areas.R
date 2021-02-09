#' aggregate_SCCP_peak_areas
#'
#' @param MSdata a tibble containing SCCP peak area data without normalizing for the internal standard
#'
#' @return a tibble containing the aggregated SCCPs
#' @export
aggregate_SCCP_peak_areas <- function(MSdata) {
  MSdata %>%
    dplyr::rename(nominal_perc_Cl = num1,
                  concentration = num2) %>%
    dplyr::group_by(batch, batch_order, filename, sample_id, nominal_perc_Cl, concentration) %>%
    dplyr::summarize(total_congener_response = sum(area, na.rm = TRUE),
                     istd_response = mean(istd_response),
                     #                     perc = sum(perc_congener_abund, na.rm = TRUE),
                     experimental_perc_Cl = sum(Cl_contrib, na.rm = TRUE)) %>%
    dplyr::ungroup()
}
