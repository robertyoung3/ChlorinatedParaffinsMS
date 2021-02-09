#' get_enhanced_isotope_list
#'
#' @param chemformulas a character string or vector of characters strings
#'   representing chemical formulas
#'
#' @return a list of matrices, indexed by chemical formula, containing isotopes,
#'   relative isotopic abundances, and fractional relative isotopic abundances
#'   for each chemical formula
#' @export
#'
#' @examples
#' formula <- "C17H22BrF13"
#' get_enhanced_isotope_list(formula)
#'
#' subset_SCCP_formulas <- SCCP_formulas[1:3]
#' get_enhanced_isotope_list(subset_SCCP_formulas)
get_enhanced_isotope_list <- function(chemformulas) {
  revised_isotope_list <- get_isotope_list(chemformulas) %>%
    add_frac_abund()
  return(revised_isotope_list)
}
