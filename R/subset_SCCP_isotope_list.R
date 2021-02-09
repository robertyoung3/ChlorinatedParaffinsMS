#' subset_SCCP_isotope_list
#'
#' A dataset containing isotopic distributions for the chemical formulas of
#' short-chain chlorinated paraffins with carbon numbers ranging from 10 to 13
#' and chlorine numbers ranging from 5 to 10, created with enviPat::isopattern
#' and internal functions for use in processing SCCP data.
#'
#' @format A list of 3 matrices, indexed by SCCP chemical formula, each
#'   containing the following items: \describe{ \item{m/z}{accurate isotopic
#'   mass, in decimal unified atomic mass units (u)}
#'   \item{abundance}{relative isotopic abundance, in decimal rather than
#'   percent notation} ... }
#' @source gen_isotope_list package
#' @examples
#' data(subset_SCCP_isotope_list)
"subset_SCCP_isotope_list"
