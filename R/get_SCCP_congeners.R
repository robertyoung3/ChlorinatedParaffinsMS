#' get_SCCP_congeners
#'
#' @param MSdata a tibble containing MSdata from TraceFinder
#' @param inject_std a character string containing the injection standard name,
#'   as specified in the "compound" column. The default is the injection
#'   standard used during method development.
#' @param surrogate a character string containing the surrogate standard name,
#'   as specified in the "compound" column. The default is the surrogate
#'   standard used during method development.
#'
#' @return
#' @export
get_SCCP_congeners <- function(MSdata, inject_std = "13C-HCB", surrogate = "13C-PCN27") {
  MSdata %>%
    dplyr::filter(!compound %in% c(inject_std, surrogate)) %>%
    compute_Cl_perc() %>%
    compute_exper_isotop_abund() %>%
    dplyr::select(batch, batch_order, filename, sample_id, num1, num2, chem_formula, compound,
                  perc_Cl, frac_isotop_abund, rel_isotop_abund, neutral, `M-Cl`, type, peak_label,
                  area, exper_isotop_abund, diff_isotop_abund, dplyr::everything())
}
