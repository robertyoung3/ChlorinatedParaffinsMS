#' subset_enhanced_SCCP_isotope_list
#'
#' A dataset containing isotopic distributions for the chemical formulas of
#' short-chain chlorinated paraffins with carbon numbers ranging from 10 to 13
#' and chlorine numbers ranging from 5 to 10, created with enviPat::isopattern
#' and internal functions for use in processing SCCP data.
#'
#' @format A list of 3 matrices, indexed by SCCP chemical formula, each
#'   containing the following items: \describe{ \item{m/z}{accurate isotopic
#'   mass, in decimal unified atomic mass units (u)
#'   \item{rel_isotop_abund}{relative isotopic abundance, in decimal rather than
#'   percent notation} \item{frac_isotop_abund}{fractional relative isotopic
#'   abundance, in decimal rather than percent notation, computed as a fraction
#'   of all relative isotopic abundances} ... }
#' @source gen_enhanced_isotope_list package
#' @examples
#' data(subset_enhanced_SCCP_isotope_list)
"subset_enhanced_SCCP_isotope_list"
