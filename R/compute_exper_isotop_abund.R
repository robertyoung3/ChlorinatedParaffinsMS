#' compute_exper_isotop_abund
#'
#' @param MSdata a tibble containing MSdata from TraceFinder
#'
#' @return an augmented tibble with relative isotopic abundance for each ion
#' @export
compute_exper_isotop_abund <- function(MSdata) {
  MSdata %>%
    dplyr::group_by(batch, filename, num1, num2, compound) %>%
    dplyr::mutate(exper_isotop_abund = 100* area /area[peak_label == "T1"],
                  diff_isotop_abund = rel_isotop_abund - exper_isotop_abund) %>%
    dplyr::ungroup()
}


