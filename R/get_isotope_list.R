#' get_isotope_list
#'
#' @param chemformulas a character string or vector of characters strings
#'   representing chemical formulas
#'
#' @return a list of matrices from enviPat, indexed by chemical formula,
#'   containing isotopes and abundances for each chemical formula
#' @export
#'
#' @examples
#' formula <- "C17H22BrF13"
#' get_isotope_list(formula)
#'
#' subset_SCCP_formulas <- SCCP_formulas[1:3]
#' get_isotope_list(subset_SCCP_formulas)
get_isotope_list <- function(chemformulas) {
  isotope_list <- enviPat::isopattern(isotopes = isotopes,
                                      chemformulas,
                                      threshold = 0.1,
                                      # charge = -1 would add an electron
                                      charge = FALSE)
  return(isotope_list)
}
