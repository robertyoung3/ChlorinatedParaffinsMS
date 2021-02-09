#' get_isotope_mass
#'
#' Returns mass for specific elemental isotope (e.g., 1H or 35Cl)
#'
#' @param iso character string representing elemental isotope
#'
#' @return numeric representing the elemental isotope's mass
#' @export
#'
#' @examples
#' get_isotope_mass("35Cl")
get_isotope_mass <- function(iso) {
  temp <- isotopes %>%
    dplyr::filter(isotope == iso)
  return(temp$mass)
}
