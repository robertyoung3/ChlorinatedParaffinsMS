#' gen_CP_isotope_df
#'
#' @param isotope_list a list of matrices, indexed by chemical formula,
#'   containing isotopes with absolute and fractional relative abundances for
#'   each chemical formula
#' @param top_n_num the desired numer of isotopes for each chemical formula
#'
#' @return a dataframe containing CP formulas, ions, and isotopic abundances
#' @export
#'
#' @examples
#' gen_CP_isotope_df(subset_enhanced_SCCP_isotope_list, top_n_num = 4)
gen_CP_isotope_df <- function(isotope_list = SCCP_isotope_list, top_n_num = 3) {
  emass <- 0.00054858
  Hmass <- get_isotope_mass("1H")
  Dmass <- get_isotope_mass("2H")
  Cmass <- get_isotope_mass("12C")
  C13mass <- get_isotope_mass("13C")
  Cl35mass <- get_isotope_mass("35Cl")
  Cl37mass <- get_isotope_mass("37Cl")
  df <- isotope_list %>%
    purrr::map_dfr(~tibble::as_tibble(.), .id = "chem_formula") %>%
    dplyr::group_by(chem_formula) %>%
    dplyr::top_n(top_n_num, rel_isotop_abund) %>%
    dplyr::ungroup() %>%
    #initial value is uncharged
    dplyr::rename(neutral = `m/z`) %>%
    dplyr::mutate(`M-Cl` = neutral + emass - get_isotope_mass("35Cl"),
                  `unit_M-Cl` = round(`M-Cl`),
                  compound = stringr::str_remove(chem_formula, pattern = "H[0-9]+"),
                  perc_Cl = ((`35Cl` * Cl35mass + `37Cl` * Cl37mass) /
                             (`1H` * Hmass + `2H` * Dmass + `35Cl` * Cl35mass +
                             `37Cl` * Cl37mass + `12C` * Cmass + `13C` * C13mass))
                            * 100) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::matches("^[0-9]+")), ~as.integer(.)) %>%
    dplyr::select(chem_formula, compound, perc_Cl, neutral, rel_isotop_abund,
                  frac_isotop_abund, `unit_M-Cl`, dplyr::everything())
  return(df)
}

# future use:
# `M-` = neutral + emass
# `M-HCl` =  M - get_isotope_mass("35Cl") - get_isotope_mass("1H")
# `M-Cl-HCl` = M - 2* get_isotope_mass("35Cl") - get_isotope_mass("1H")
# `M+Cl` = M + get_isotope_mass("35Cl")
