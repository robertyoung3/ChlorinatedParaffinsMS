#' aggregate_SCCP_congeners
#'
#' @param MSdata a tibble containing SCCP congener data, normalized by the internal standard
#'
#' @return a tibble containing the aggregated SCCPs
#' @export
aggregate_SCCP_congeners <- function(MSdata) {
  MSdata %>%
    dplyr::rename(nominal_perc_Cl = num1,
                  concentration = num2) %>%
    dplyr::group_by(batch, batch_order, filename, sample_id, nominal_perc_Cl, concentration) %>%
    dplyr::summarize(total_congener_response = sum(rel_congener_area, na.rm = TRUE),
                     congener_count = sum(!is.na(area )),
                     istd_response = mean(istd_response),
#                     perc = sum(perc_congener_abund, na.rm = TRUE),
                     experimental_perc_Cl = sum(Cl_contrib, na.rm = TRUE)) %>%
    dplyr::ungroup()
}
