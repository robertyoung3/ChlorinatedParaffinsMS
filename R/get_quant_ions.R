#' get_surrogate_ions
#'
#' @param MSdata a tibble containing MSdata from TraceFinder
#'
#' @return a tibble containing only the quantitation ions, which are labeled as "peak_label" = 'T1'
#' @export
get_quant_ions <- function(MSdata) {
  MSdata %>%
    dplyr::filter(peak_label == "T1")
}
