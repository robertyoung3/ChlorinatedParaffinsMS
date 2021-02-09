#' add_frac_abund
#'
#' @param isotope_list a list from the enviPat package of matrices, indexed by
#'   chemical formula, containing isotopes, isotopic abundances, and atom
#'   numbers
#'
#' @return isotope list with fractional abundances
#' @export
#'
#' @examples
#' add_frac_abund(subset_SCCP_isotope_list)
add_frac_abund <- function(isotope_list) {
  compute_frac_isotop_abund <- function(isotope_matrix) {
    frac_isotop_abund <- isotope_matrix[, "abundance"] / sum(isotope_matrix[, "abundance"])
    isotope_matrix <- cbind(isotope_matrix, frac_isotop_abund)
    return(isotope_matrix)
  }
  rename_col <- function(isotope_matrix) {
    colnames(isotope_matrix)[colnames(isotope_matrix) == "abundance"] <- "rel_isotop_abund"
    return(isotope_matrix)
  }
  revised_isotope_list <- isotope_list %>%
    purrr::map(~compute_frac_isotop_abund(.)) %>%
    purrr::map(~rename_col(.))
  return(revised_isotope_list)
}
