#' compute_Cl_perc
#'
#' @param MSdata a tibble containing MSdata from TraceFinder for the SCCP
#'   congeners
#'
#' @return a tibble containing MSdata with chlorine data
#' @export
compute_Cl_perc <- function(MSdata) {
  MSdata <- MSdata %>%
    dplyr::left_join(SCCP_isotope_df,
                     by = c("compound", "unit_TF_mz" = "unit_M-Cl")) %>%
    dplyr::select(batch, batch_order, filename, sample_id, num1, num2, chem_formula, compound,
                  perc_Cl, frac_isotop_abund, rel_isotop_abund, neutral, `M-Cl`, dplyr::everything()) %>%
    dplyr::select(-unit_TF_mz, -TF_mz) %>%
    dplyr::rename(rel_peak_area = rel_response) %>%
    dplyr::mutate(rel_congener_area = rel_peak_area / frac_isotop_abund)
  total_SCCP_congeners <- MSdata %>%
    dplyr::filter(peak_label == "T1") %>%
    dplyr::select(batch, filename, rel_congener_area) %>%
    dplyr::group_by(batch, filename) %>%
    dplyr::summarize(total_congener_areas = sum(rel_congener_area, na.rm = TRUE))
  MSdata <- MSdata %>%
    dplyr::left_join(total_SCCP_congeners, by = c("batch", "filename")) %>%
    dplyr::mutate(perc_congener_abund = rel_congener_area / total_congener_areas,
                  Cl_contrib = perc_congener_abund * perc_Cl)
}
